[<AutoOpen>]
module Scratch.Transpiler.Core
open System
open System.Collections.Generic
open System.Reflection
open System.Text.RegularExpressions
open FSharp.Quotations
open FSharp.Reflection
open Scratch
open Scratch.Ast
open Scratch.IR
open Scratch.MemoryModel
open Scratch.MemoryModel.TypeSpec
open Scratch.Primitives
open Scratch.Primitives.Field
open Scratch.Reflection
open Scratch.Reflection.Expr
open Scratch.Reflection.Transformers
open Scratch.Transpiler.Environments
open Scratch.Transpiler.TypeSpec
module A = Scratch.Ast.Expression
module A = Scratch.Ast.Expressions
module A = Scratch.SValue
module E = FSharp.Quotations.Patterns
module E = FSharp.Quotations.ExprShape
module E = FSharp.Quotations.DerivedPatterns
module Tagged = Scratch.Transformers.Tagged
module Tags = Scratch.Transformers.Tags
module IsTag = Scratch.Transformers.IsTag
module Exp = Exp.Op
module Top = Top.Listener
module Types = ExpTypes
type private E = Quotations.Expr
type private T = FSharp.Reflection.FSharpType
type private InliningOptions = Scratch.Transformers.InliningOptions
/// `Quotations.Var`
type private QVar = Quotations.Var


[<AutoOpen>]
module Keywords =
    let export _ = ()

let raiseIfTypeSizeMismatch source expected actual =
    let expected = List.length expected
    let actual = List.length actual
    if expected <> actual then TypeSizeMismatch(actual = actual, expected = expected) |> raiseError source

let lookupOrRaiseProcedure source id (Get FEnvironment env) =
    match Map.tryFind id env.vars with
    | ValueSome(ProcedureSpec p) -> p
    | _ -> ProcedureNotFound id |> raiseError source

let lookupOrRaiseList source id (Get FEnvironment env) =
    match Map.tryFind id env.vars with
    | ValueSome(ListSpec l) -> l
    | _ -> ListNotFound id |> raiseError source

let lookupOrRaiseVar source id (Get FEnvironment env) =
    match Map.tryFind id env.vars with
    | ValueSome(VarSpec v) -> v
    | _ -> StorageVarNotFound id |> raiseError source

let vTypeDefaultValue = function
    | Any -> A.sEmptyString
    | Typed t -> SType.parameterDefaultValue t

let memberName config = function
    | TupleField i -> config.tupleFieldName i
    | RecordField f -> config.propertyName (MemberId.resolveProperty f)
    | UnionCaseField f -> config.unionCaseFieldName f

let rec memberPath config = function
    | UnderlyingValue -> ""
    | UnionCaseTagField _ -> config.namespaceSeparator + config.unionCaseTagName
    | MemberPath(m, p) -> config.namespaceSeparator + memberName config m + memberPath config p
    | UnionCaseUnifiedField(p1, p2, ps) ->
        let paths = memberPath config p1, memberPath config p2, List.map (memberPath config) ps
        config.unionCaseUnifiedFieldName paths

let withMemberPath config varName path = varName + memberPath config path

let private memberType config = function
    | [] -> Types.empty
    | [UnderlyingTypeSpec(vType = t; path = m)] ->
        ExpType.ofMemberType <| MemberType.make (memberPath config m) t

    | t ->
        t
        |> List.map (fun (UnderlyingTypeSpec(vType = t; path = m)) ->
            MemberType.make (memberPath config m) t
        )
        |> UnboxedType

// TODO: thread safe
let internal underlyingMemberType (Get FEnvironment env & Get FConfig config) t =
    let mutable r = Unchecked.defaultof<_>
    if env.rare.memberTypeCache.TryGetValue(t, &r) then r else

    let m =
        match underlyingType t with
        | None -> None
        | Some t -> Some <| memberType config t

    env.rare.memberTypeCache.Add(t, m)
    m

let internal (|UnderlyingMemberType|_|) env t = underlyingMemberType env t

let private underlyingMemberTypeOrRaise env source t =
    match underlyingMemberType env t with
    | None -> InvalidExpressionType t |> raiseError source
    | Some t -> t

module private Var =
    let newVarFromQVar env source (v: QVar) =
        let t = underlyingMemberTypeOrRaise env source v.Type
        Var.newStorage v.Name v.IsMutable t

    let procedureType env source ps (ExprType bodyT) =
        let ts = ps |> List.map (fun p -> Var.varType p.parameterIRVar)
        let t = underlyingMemberTypeOrRaise env source bodyT
        { parameterTypes = ts; resultType = t }

    let newProcFromQVar env source (v: QVar) (_, ps) body =
        Var.newVar v.Name (procedureType env source ps body)

    let newProcFromMethodInfo (Get FConfig config & env) source m (_, ps) body =
        Var.newVar (config.methodName m) (procedureType env source ps body)

    let newSimpleFromQVar (v: QVar) = Var.newSimple v.Name
    let newSimpleFromFieldInfo (Get FConfig config) f =
        Var.newSimple (config.fieldName f)

    let newSimpleFromPropertyInfo (Get FConfig config) p =
        Var.newSimple (config.propertyName p)


let localName env varName =
    let config = get FConfig env
    let name = (get FBlockEnvironment env).proc.procedureVar |> Var.name
    sprintf "%s%s%s" name config.namespaceSeparator varName

let rec isConstantLike = function
    | E.DefaultValue _ -> true
    | E.FieldGet(None, f) -> f.IsInitOnly || f.IsLiteral
    | E.PropertyGet(None, p, []) -> not p.CanWrite && T.IsModule p.DeclaringType

    | E.NewTuple es as e -> e.Type.IsValueType && List.forall isConstantLike es
    | E.NewRecord(t, es) -> t.IsValueType && List.forall isConstantLike es
    | E.NewUnionCase(u, es) ->
        let t = u.DeclaringType
        match unionRepresentation t with
        // repr = void
        | Some(UnitLikeUnion _)
        // repr = constant(tag)
        | Some(EnumLikeUnion _) -> true
        // repr = class: collectable | struct: value...
        | Some(RecordLikeUnion _) -> t.IsValueType && List.forall isConstantLike es
        | Some(OptionLikeUnion(noneCase = noneCase)) ->
            // repr = struct: constant(tag), value...
            if t.IsValueType then List.forall isConstantLike es

            // repr = class: (noneCase: constant(nil(0)) | someCase: collectable)
            else u.Tag = noneCase.Tag

        // repr = class: collectable | struct: (constant(tag), value...)
        | Some(ComplexUnion _) -> u.DeclaringType.IsValueType && List.forall isConstantLike es
        | None -> false

    //| E.TupleGet(e1, _) -> isConstantLike e1
    //| E.FieldGet(Some e1, f)
    //| E.ProperyGet(Some e1, p, [])
    | E.Value _
    | E.ValueWithName _ -> true
    | E.Var v -> not v.IsMutable

    | _ -> false

let transformConstantPropagation = function
    | E.Let(v, e1, e2) when not v.IsMutable && isConstantLike e1 -> e2.Substitute(fun v' -> if v = v' then Some e1 else None) |> Some
    | _ -> None

let topLevelPreTransformers = [
    transformPipeLeft
    transformPipeRight
    transformSingleLetVar
]
let expressionPreTransformers = [
    transformPipeLeft
    transformPipeRight
    transformConstantPropagation
    transformPurePropagation
    transformUnusedLet
    transformInlining
]
let expressionPreTransform (Get FConfig config) e =
    e
    |> transform 100 expressionPreTransformers
    |> (fun x -> config.plugin.preTransform config x |> Option.defaultValue x)
    |> transform 100 expressionPreTransformers

let topLevelPreTransform _ e = transform 100 topLevelPreTransformers e

let underlyingScratchTypeSizeOrRaise e = function
    | UnderlyingType x -> List.length x
    | t -> InvalidExpressionType t |> raiseError e

// ((string * string) * (int * int) * (string * string)).(1) -> [S; S; N; N; S; S].(2..3)
let rec measureTupleSpan e offset t = function
    | [] -> offset, underlyingScratchTypeSizeOrRaise e t
    | i::is ->

    let ts = T.GetTupleElements t
    let offset' = Array.sumBy (underlyingScratchTypeSizeOrRaise e) ts.[0..i-1]
    measureTupleSpan e (offset + offset') ts.[i] is

let measureFieldIndexes e = function
    | [] -> []
    | (p: PropertyInfo::_) as ps ->

    let rec recordFieldChain = function
        | MemberPath(RecordField p, path) -> p::recordFieldChain path
        | UnderlyingValue

        // TODO: ???
        | UnionCaseTagField _
        | UnionCaseUnifiedField _
        | MemberPath(TupleField _, _)
        | MemberPath(UnionCaseField _, _) -> []

    // struct Vec = { x: double; y: double }
    // struct Rect = { center: Vec; extend: Vec }
    // struct Plater = { name: string; rect: Rect; velocity: Vec; ... }
    //
    // ps = [Player::rect; Rect::center]
    match p.DeclaringType with
    | UnderlyingType us ->
        // us = [
        //     S, [Player::name], P
        //     N, [Player::rect; Rect::center; Vec::x], P
        //     N, [Player::rect; Rect::center; Vec::y], P
        //     N, [Player::rect; Rect::extend; Vec::x], P
        //     N, [Player::rect; Rect::extend; Vec::y], P
        //     N, [Player::velocity; Vec::x], P
        //     N, [Player::velocity; Vec::y], P
        //     ...
        // ]
        us
        |> Seq.indexed
        |> Seq.filter (fun (_, UnderlyingTypeSpec(_, m, _)) ->
            let path = recordFieldChain m
            Seq.forall2 (=) (List.map MemberId.propertyId ps) path
        )
        |> Seq.map fst
        |> Seq.toList
        // result = [1; 2]

    | t -> InvalidExpressionType t |> raiseError e

let stringLiteralRegex = Regex @"^[^\u0000\r\n]*$"

let (|MemoryItem1Get|_|) = (|SpecificUnionCaseFieldGet|_|) <@ function Scratch.MemoryModel.Memory x -> x @>
let (|IfFSharpCall|_|) = E.(|SpecificCall|_|) <@@ Scratch.Operators.``#if-fsharp`` @@> 
let (|IfElseFSharpCall|_|) = E.(|SpecificCall|_|) <@@ Scratch.Operators.``#if-else-fsharp`` @@>

let isMutableStructRecordField (p: PropertyInfo) = p.CanWrite && T.IsRecord p.DeclaringType && isValueType p.DeclaringType
let rec (|StructMutableRecordPropertyGetChain|) = function
    | E.PropertyGet(Some(StructMutableRecordPropertyGetChain (home, ps)), p, []) when isMutableStructRecordField p -> home, ps @ [p]
    | e -> e, []

let (|MutableStructHome|_|) senv = function
    | E.PropertyGet(None, p, []) when p.CanWrite && T.IsModule p.DeclaringType -> propertyId p |> Some
    | E.Var v when v.IsMutable -> VarId v |> Some
    | E.FieldGet(Some(SpriteOrProcedureThis senv ()), f) when not f.IsInitOnly && not f.IsLiteral -> fieldId f |> Some
    | _ -> None

let (|ValueOnlyUnionType|_|) t =
    if not <| isValueType t then None else

    match valueLayout t, unionRepresentation t with
    | Some us, Some r when us |> List.exists (function UnderlyingTypeSpec(kind = Kind.Collectable) -> true | _ -> false) |> not -> Some struct(us, r)
    | _ -> None

let (|ValueOnlyUnion|_|) (u: UnionCaseInfo) =
    (|ValueOnlyUnionType|_|) u.DeclaringType

let isUnionField t (p: PropertyInfo) =
    if T.IsUnion(t, allowAccessToPrivateRepresentation = true) |> not then false else
    T.GetUnionCases(t, allowAccessToPrivateRepresentation = true)
    |> Array.exists (fun case ->
        case.GetFields()
        |> Array.exists (fun field -> field.PropertyType = p.PropertyType && field.Name = p.Name)
    )

let isRecordField t (p: PropertyInfo) =
    if T.IsRecord(t, allowAccessToPrivateRepresentation = true) |> not then false else
    T.GetRecordFields(t, allowAccessToPrivateRepresentation = true)
    |> Array.exists (fun field -> field.Name = p.Name)

let (|RecordOrUnionPropertyGet|_|) = function
    | E.PropertyGet(Some(ExprType t as this), p, []) when isUnionField t p || isRecordField t p -> Some struct(this, p)
    | _ -> None

let unitArgs = [Expr.Value(())]

let rec transpileExpression senv e =
    match (get FConfig senv.e).plugin.expression.Invoke senv e with
    | Ok x -> x
    | Error pluginError ->

    let source = SourceCode.ofExpr e
    match e with
    | E.Unit() -> Exp.empty (getLoc e)

    // <@ c = "_" || c = "a" @> =
    // <@ if c = "_" then true else c = "a" @> =>
    // acc: ``, expr: `c = "_" || c = "a"`
    //
    // <@ c = "_" || isLetter c @> =
    // <@ if c = "_" then true else isLetter c @> =>
    // acc: `_ = false; if c = "_" then _ <- true else isLetter(c); _ <- isLetter.result`, expr: `_`
    //
    // <@ isLetter c || c = "_" @> =
    // <@ if isLetter c then true else c = "_" @> =>
    // acc: `isLetter(c)`, expr: `isLetter.result || c = "_"`
    //
    // <@ isLetter c || isDigit c @> =
    // <@ if isLetter c then true else isDigit c @> =>
    // acc: `_ = false; isLetter(c); if isLetter.result then _ <- true else isDigit(c); _ <- isDigit.result`, expr: `_`
    | E.OrElse(e1, e2) -> Exp.``|`` (getLoc e) (transpileExpression senv e1) (transpileExpression senv e2)
    | E.AndAlso(e1, e2) -> Exp.``&`` (getLoc e) (transpileExpression senv e1) (transpileExpression senv e2)

    // <@ if $test then $ifTrue else $ifFalse @> ->
    // acc: ```
    // ...$testAcc
    // if $test then
    //     ...$ifTrueAcc
    //     temp <- $ifTrue
    // else
    //     ...$ifFalseAcc
    //     temp <- $ifFalse
    // ```
    // expr: `temp`
    | E.IfThenElse(testE, ifTrueE, ifFalseE) ->
        transpileIfExpression senv testE ifTrueE ifFalseE source

    // <@ $e1; $e2 @>
    | E.Sequential(e1, e2) ->
        Exp.seq (getLoc e) (transpileExpression senv e1) (transpileExpression senv e2)

    // <@ let $v = $e1 in $e2 @> ->
    // acc: ```
    // ...$e1Acc
    // v <- $e1
    // ...$e2Acc
    // ```
    // expr: $e2
    | E.Let(v, e1, e2) -> transpileLetExpression senv v e1 e2 source

    // <@ $v <- $e1 @> ->
    // acc: `...$e1Acc; v <- $e1`, expr: ``
    | E.VarSet(v, e1) -> transpileVarOrPropertySetExpression senv (VarId v) (VarNotFound v) e1 source
    | E.PropertySet(None, p, [], e1) when T.IsModule p.DeclaringType -> transpileVarOrPropertySetExpression senv (propertyId p) (PropertyNotFound p) e1 source
    | E.FieldSet(Some(SpriteOrProcedureThis senv ()), f, e1) -> transpileVarOrPropertySetExpression senv (fieldId f) (FieldNotFound f) e1 source

    // <@ $player.rect.center <- $point @> ->
    // acc: `..$pointAcc; player.rect.center <- $point`, expr: ``
    | E.PropertySet(Some(StructMutableRecordPropertyGetChain (home, ps)), p, [], value) when isMutableStructRecordField p ->
        transpileStructPropertySetExpression senv home ps p value source

    | E.Int32 x -> Exp.number (getLoc e) (double x)
    | E.Double x -> Exp.number (getLoc e) x
    | E.String x ->
        if stringLiteralRegex.IsMatch x then Exp.string (getLoc e) x else
        raiseError source StringLiteralCanNotContainNewLine

    | E.Bool x -> Exp.bool (getLoc e) x

    // <@ SValue ... @> | <@ N ... @> | ...
    | E.NewUnionCase(u, [e]) when isPrimitiveType u.DeclaringType -> transpileExpression senv e

    // <@ UnitLike @>
    | E.NewUnionCase(u, []) when underlyingType u.DeclaringType = Some [] -> Exp.empty (getLoc e)

    // <@ ValueOnlyUnion(%es...) @>
    | E.NewUnionCase(ValueOnlyUnion(us, repr) & c, es) -> transpileValueOnlyNewUnionCase senv us repr c es e

    // <@ ValueOnlyUnion? @>
    | E.UnionCaseTest(value, ValueOnlyUnion(_, repr) & c) ->
        let l = getLoc e

        let value = transpileExpression senv value
        match repr with
        
        // `true`
        | UnitLikeUnion _
        | RecordLikeUnion _ -> Exp.bool l true

        // `%matchValue.tag = $(c.Name)`
        | EnumLikeUnion _
        | OptionLikeUnion _
        | ComplexUnion _ ->
            let tag = Exp.tupleGet l value 0
            Exp.``=`` l tag (Exp.string l c.Name)

    // <@ zeroSizedRecord.field @>, <@ zeroSizedUnion.Item1 @>
    | RecordOrUnionPropertyGet(ExprType(UnderlyingType []), _) -> Exp.empty (getLoc e)

    // <@ ValueOnlyUnion.Item1 @>
    | E.PropertyGet(Some(ExprType(ValueOnlyUnionType(us, _)) & this), p, []) ->
        let l = getLoc e
        let this = transpileExpression senv this
        Exp.letScope l (get FConfig senv.e).localName this <| fun temp ->

        let p = MemberId.propertyId p
        us
        |> Seq.indexed
        |> Seq.choose (fun (i, t) ->
            match t with
            | UnderlyingTypeSpec(path = MemberPath(UnionCaseField(UnionCaseFieldInfo(field = p')), _)) when p = p' ->
                Some <| Exp.tupleGet l temp i

            | _ -> None
        )
        |> Seq.toList
        |> Exp.newTuple l

    // <@ struct(..., ...) @>
    | E.NewTuple es when not e.Type.IsClass -> es |> List.map (transpileExpression senv) |> Exp.newTuple (getLoc e)

    // let struct(struct(x1, _), _) = ...
    | TupleChain(ExprType tupleT as tupleE, indexes) when not tupleT.IsClass ->
        let l = getLoc e
        let tuple = transpileExpression senv tupleE
        Exp.letScope l (get FConfig senv.e).localName tuple <| fun temp ->

        let offset, count = measureTupleSpan source 0 tupleT indexes
        let getTuples = List.init count <| fun i -> Exp.tupleGet l temp (i + offset)
        Exp.newTuple l getTuples

    // <@ v @> | <@ SomeFSharpModule.someProperty @>
    | PropertyOrVarOrFieldGet senv k -> transpilePropertyOrVarGetExpression senv k source

    // <@ let (Memory m) = e1 @>
    | MemoryItem1Get(_, e1)  -> transpileExpression senv e1

    // <@ ``#if-fsharp`` ... @> -> ``
    | IfFSharpCall _ -> Exp.empty (getLoc e)

    // <@ ``#if-else-fsharp`` ... (fun _ -> $body) @> -> `$body`
    | IfElseFSharpCall(_, _, _::E.Lambda(_, body)::_) ->
        transpileExpression senv body

    // `let g () = f (f 1 2 + f 3 4) 5` ->
    // `
    // f 1 2
    // g._ = f.result
    // f 3 4
    // g._.1 = f.result
    // g._.2 = (g._ + g._.1)
    // g.result = f g._.2 5
    // `
    // `self.Method()` | `localFunction()`
    | E.Call((None | Some(SpriteOrProcedureThis senv ())), m, xs) ->
        transpileCallExpression senv m xs source

    | NonVirtualInstanceCall(this, m, args) ->
        transpileCallExpression senv m (this::args) source

    | ExprPatterns.Applications(PropertyOrVarOrFieldGet senv k & f, xxs) ->
        let p = lookupOrRaiseProcedure source k senv.e
        transpileApplicationsExpression senv p (List.concat xxs) (SourceCode.ofExpr f)

    | _ ->

    match pluginError with
    | Some e -> raise <| TranspileException e
    | None -> InvalidExpressionForm |> raiseError source

and transpileValueOnlyNewUnionCase senv us repr c es e =
    match repr with
    | UnitLikeUnion _ -> Exp.empty (getLoc e)
    | EnumLikeUnion _ -> Exp.string (getLoc e) c.Name
    | RecordLikeUnion _ -> Exp.newTuple (getLoc e) (List.map (transpileExpression senv) es)
    | OptionLikeUnion(noneCase, someCase) ->
        let es = List.map (transpileExpression senv) es
        let l = getLoc e
        if noneCase.Tag = c.Tag then
            let uninitValue = Exp.lit l (get FConfig senv.e).unionCaseUninitField
            let fs =
                us
                |> List.tail
                |> List.map (fun (UnderlyingTypeSpec(vType = t)) ->
                    Exp.reinterpret l uninitValue <| ExpType.ofVType t
                )

            Exp.newTuple l (Exp.string l noneCase.Name::fs)
        else
            Exp.newTuple l (Exp.string l someCase.Name::es)

    | ComplexUnion _ -> transpileComplexNewUnionCase senv c us es e

and transpileComplexNewUnionCase _senv _c _us _es _e =
    // TODO:
    raise <| NotImplementedException()

    //(*
    //[<Struct>]
    //type VU =
    //    | A
    //    | B of b: string
    //    | C of c1: Point voption * c2: struct(string * bool)
        
        
    //undelyingType VU = [
    //    [VU.tag]
    //    ([VU.B.b; value] | [VU.C.c1; voption.tag])
    //    [VU.C.c1; voption.ValueSome.item1; Point.x; value]
    //    [VU.C.c1; voption.ValueSome.item1; Point.y; value]
    //    [VU.C.c2; tuple.1; Point.y; value]
    //    [VU.C.c2; tuple.2; Point.y; value]
    //]
    //<@ A @> => `["A"; _; _; _; _]`
    //<@ B a @> => `["B"; a; _; _; _]`
    //<@ C(a, b) @> => `["C"; a.tag; a.item1.x; a.item1.y; b.1; b.2]`
    //*)
    //let l = getLoc e
    //let caseId = MemberId.unionCaseId c
    //let valueMap = 
    //    List.zip (Array.toList <| c.GetFields()) es
    //    |> Seq.indexed
    //    |> Seq.collect (fun (i, (f, e)) ->
    //        let caseFieldMember = UnionCaseField <| UnionCaseFieldInfo(caseId, i, MemberId.propertyId f)
    //        let fieldPaths =
    //            valueLayout e.Type
    //            |> Option.get
    //            |> List.map (fun (UnderlyingTypeSpec(path = p)) -> MemberPath(caseFieldMember, p))

    //        let es = transpileExpression senv e
    //        List.zip fieldPaths es
    //    )
    //    |> Map.ofSeq

    //let unusedValue = (get FConfig senv.e).unionCaseUninitField
    //us
    //|> List.map (fun (UnderlyingTypeSpec(path = p)) ->
    //    match p with

    //    // ケースのタグ
    //    | UnionCaseTagField d when d = MemberId.typeId c.DeclaringType -> A.eString l c.Name

    //    | MemberPath _ ->

    //        match Map.tryFind p valueMap with
    //        | ValueSome e -> e

    //        // 未使用
    //        | _ -> Exp.lit l unusedValue

    //    | UnionCaseUnifiedField(p1, p2, ps) ->
    //        match Map.tryFind p1 valueMap with
    //        | ValueSome e -> e
    //        | _ ->

    //        match Map.tryFind p2 valueMap with
    //        | ValueSome e -> e
    //        | _ ->

    //        match ps |> List.tryPick (fun p -> FSharp.Collections.Map.tryFind p valueMap) with
    //        | Some e -> e

    //        // 未使用
    //        | _ -> Exp.lit l unusedValue

    //    | _ -> failwith "internl error"
    //)
    //|> Exp.newTuple l

and transpileLetExpression senv v e1 e2 source =
    let e1 = transpileExpression senv e1
    let var = Var.newVarFromQVar senv.e source v
    let spec = { var = var; export = NoExport; persistence = NoPersistent }
    local FE senv (addVarSpec v (VarSpec spec)) <| fun senv ->

    let e2 = transpileExpression senv e2
    Exp.let' (SourceCode.tag source) var e1 e2

// `let f<'a> (x: 'a): 'a = x` のような型引数を含む `f` と
// 式 `f<Point> p` が定義されているとき
//
// 実際には `f` の `'a` を `Point` で置き換えた `let f@Point: Point -> Point` が生成され、
// 式は `f@Point p` のように変更される
//
// コードを共有するために以下のルールを当てはめる
// 型引数 `'a` を `X` に置き換えるとき
//     - `X` が `IWord` に代入できる型かつ値型なら、`V` に置き換える
//     - `X` が `IWord` に代入できる型かつ値型でないなら、`IWord` に置き換える
//     - `X` が `Collectable` なら `obj` に置き換える
//     - その他の場合 `X` に置き換える
//     - 以上のルールで置き換えたとき `'a` の型制約に違反するなら、`X` に置き換える
// 
// 例:
//     `N` → `V`
//     `B` → `V`
//     `int ref` → `obj`
//     `struct(N * S)` → `struct(N * S)`
//
// コードの共有が行われると呼び出し境界で型情報が抜け落ちてしまうので、
// 以下のように、呼び出しの周りに再解釈を強制する式を挿入する必要がある
// ```
// let f<'a> (x: 'a): 'a = x
// f<int> 10 + 20
// ``` =>
// ```
// let f@V (x: any): any = x
// (f@V (10 :> any) :?> n) + 20
// ```
and transpileCallExpression senv (SharingGenericMethod m as nonSharingMethod) xs source =
    let procSpec = lookupOrRaiseProcedure source (methodId m) senv.e

    // unit を受け取る関数と型関数の呼び出し規約の違いを吸収
    // 例: `let f<'a>() = ()` と `let f<'a> = ()`
    let xs =
        match xs, Var.trivia(procSpec.procedureVar).parameterTypes with
        | [], [_] -> unitArgs
        | _ -> xs

    let call = transpileApplicationsExpression senv procSpec xs source
    if not nonSharingMethod.IsGenericMethod then call else

    let nonSharingResultType =
        let rt = nonSharingMethod.ReturnType
        if rt = typeof<Void> then Types.empty
        else underlyingMemberTypeOrRaise senv.e source nonSharingMethod.ReturnType

    if ExpType.isAssignable nonSharingResultType (Exp.varType call) then call
    else Exp.reinterpret (SourceCode.tag source) call nonSharingResultType

and transpileApplicationsExpression senv procSpec args l =
    let args = List.map (transpileExpression senv) args
    Exp.call (SourceCode.tag l) procSpec.procedureVar args

and transpilePrimitiveExpression senv x =
    let e = transpileExpression senv x
    match Exp.varType e with
    | UnboxedType [_] -> e
    | UnboxedType t -> TypeSizeMismatch(actual = List.length t, expected = 1) |> raiseError (SourceCode.ofExpr x)

and transpileVarOrPropertySetExpression senv k error e1 source =
    match lookupSpec k senv.e with
    | ValueSome(VarSpec v) ->
        let x1 = transpileExpression senv e1
        Exp.varSet (SourceCode.tag source) v.var x1

    | _ -> error |> raiseError source

and transpileStructPropertySetExpression senv home ps p value source =
    match home with

    // let temp = value
    // home.(3) <- temp.(0)
    // home.(4) <- temp.(1)
    // ...
    | MutableStructHome senv homeId ->
        let l = SourceCode.tag source
        let home = lookupOrRaiseVar (SourceCode.ofExpr home) homeId senv.e
        let indexMap = measureFieldIndexes source (ps @ [p])
        let value = transpileExpression senv value
        Exp.letScope l (get FConfig senv.e).localName value <| fun temp ->
        indexMap
        |> List.mapi (fun i fieldIndex -> Exp.tupleSet l home.var fieldIndex (Exp.tupleGet l temp i))
        |> Exp.concat l

    | _ -> RequireMutableStructHome home |> raiseError source

and transpileIfExpression senv testE ifTrueE ifFalseE source =
    Exp.if' (SourceCode.tag source)
        (transpileExpression senv testE)
        (transpileExpression senv ifTrueE)
        (transpileExpression senv ifFalseE)

and transpilePropertyOrVarGetExpression senv k source =
    let s = lookupSpec k senv.e
    if OutputLevel.Debug <= (get FConfig senv.e).outputLevel then
        printfn "  PropertyOrVarGet %A -> %A -> %A" (SourceCode.buildText source) k s

    let l = SourceCode.tag source
    match s with

    // TODO:
    //| ValueSome(ListSpec s) -> Exp.string l s.listName

    | ValueSome(VarSpec v) -> Exp.var l v.var
    | _ -> RequireListOrVar |> raiseError source

let transpileBlock senv e = transpileExpression senv e

let transpileProcedureParameter env source v =
    {
        parameterIRVar = Var.newVarFromQVar env source v
        parameterVar = v
    }

let transpileProcedureParameters senv vvs source =
    let vs = Seq.collect id vvs |> Seq.toList
    let procedureThis, vs =
        match vs with
        | v: QVar::vs when typeof<Sprite>.IsAssignableFrom v.Type -> Some v, vs
        | _ -> None, vs

    let ps = List.map (transpileProcedureParameter senv.e source) vs
    procedureThis, ps

let transpileProcedureSpec export inlining var (procedureThis, parameters) body =
    let (ExprType bodyType) = body
    let atomicity =
        match bodyType with
        | GeneratorType _ -> NoAtomic
        | _ -> Atomic
    {
        procedureVar = var

        procedureThis = procedureThis
        parameters = parameters
        atomicity = atomicity

        isExport = export
        inlining = inlining
    }

let private printProcedure spec source =
    let varName = sprintf "%A -> " spec.procedureVar
    let paramNames = spec.parameters |> Seq.map (fun { parameterIRVar = v } -> sprintf "%A" v) |> String.concat " "
    let location = SourceCode.location source |> locationText |> Option.defaultValue ""
    printfn "%s %s %s" varName paramNames location

// `let v = (fun x1 x2 ... xn -> ...) in ...`
let transpileProcedure senv spec body =
    if OutputLevel.Debug <= (get FConfig senv.e).outputLevel then printProcedure spec (SourceCode.ofExpr body)
    let bodyTag = getLocation body |> Tagged.empty

    let senv = senv |> map FE (addParameters spec.parameters)
    let senv = { s = senv.s; e = BlockEnvironments.make({ proc = spec }, senv.e) }
    let body = transpileBlock senv body

    let procTag = bodyTag |> addExport spec.isExport
    let procTag = procTag |> addInliningOptions spec.inlining
    let ps =
        spec.parameters
        |> List.map (fun p -> { source = bodyTag; value = p.parameterIRVar })

    let proc = ProcDef.make procTag spec.procedureVar ps spec.atomicity body
    do addScript senv <| Top.Procedure proc

let transpileWhenGreenFlag senv (ExprType et as e) =
    let var = Var.newProc (get FConfig senv.e).lambdaName [] Types.empty
    let proc = {
        procedureVar = var

        procedureThis = None
        parameters = []
        atomicity = NoAtomic

        isExport = NoExport
        inlining = None
    }
    let l = getLocation e
    let e =
        if et <> typeof<unit> then
            E.Sequential(e, E.Value(()) |> withLocation l) |> withLocation l
        else e

    let e =
        let senv = { s = senv.s; e = BlockEnvironments.make({ proc = proc }, senv.e) }
        transpileBlock senv e

    let script = Top.whenGreenFlag (Tagged.empty l) e
    do addScript senv script

let private defineVariableOfQVar senv source export (var: QVar) init =
    let t = underlyingMemberTypeOrRaise senv.e source var.Type
    let spec = declareVariable export NoPersistent var.IsMutable var.Name t
    implementVariable senv source spec init
    spec

let private printVariableInit source varName =
    let location = SourceCode.location source |> locationText |> Option.defaultValue ""
    printfn "%s -> %s" varName location

let transpileWhenGreenFlagVariableInit senv varName init =
    if OutputLevel.Debug <= (get FConfig senv.e).outputLevel then printVariableInit (SourceCode.ofExpr init) varName

    let procedureVar = Var.newProc "" [] Types.empty
    let proc = {
        procedureVar = procedureVar

        procedureThis = None
        parameters = []
        atomicity = NoAtomic

        isExport = NoExport
        inlining = None
    }
    let s' = senv.s
    let e = BlockEnvironments.make({ proc = proc }, senv.e)
    let s = ref <| ExpressionStates.make s'.contents
    let senv = { s = s; e = e }
    let x1 = transpileExpression senv init

    s'.contents <- s.contents.Tail
    x1, s.contents

let (|DefineListCall|_|) = E.(|SpecificCall|_|) <@@ SListOperations.defineList @@>
let transpileListInit _list = function
    | DefineListCall _ -> ()
    | e -> InvalidExpressionForm |> raiseError (SourceCode.ofExpr e)

let clearItemAcc senv = modifyRef FItemState senv.s <| fun state -> { state with itemAcc = [] }
let pushItemAcc senv item = modifyRef FItemState senv.s <| fun state -> { state with itemAcc = item::state.itemAcc }

let transpileAccAsWhenGreenFlag senv =
    let rec sequentials e1 = function
        | [] -> e1
        | (e, l)::es -> E.Sequential(e1, sequentials e es) |> withLocation l

    match List.rev (get FItemState senv.s.contents).itemAcc with
    | [] -> ()
    | (e1, _)::es ->
        do transpileWhenGreenFlag senv <| sequentials e1 es
        clearItemAcc senv

let private transpileProcedureSpecFromVar senv source export inlining var vvs body =
    let procedureThis, parameters = transpileProcedureParameters senv vvs source
    let var = Var.newProcFromQVar senv.e source var (procedureThis, parameters) body
    transpileProcedureSpec export inlining var (procedureThis, parameters) body

let (|ExportCall|_|) = E.(|SpecificCall|_|) <@ export @>
let (|DefineSpriteCall|_|) = E.(|SpecificCall|_|) <@ SpriteOperations.defineSprite @>
let rec transpileItems senv e =
    match (get FConfig senv.e).plugin.item.Invoke senv e with
    | Ok() -> ()

    // TODO: handle plugin error
    | Error _error ->

    match e with

    // <@ () @>
    | E.Unit()

    | DefineSpriteCall _

    // [<ReflectedDefinition>] let $f ...$vs = $e1;; ... <@ export f @>
    | ExportCall(_, _, [Lambdas(_, E.Call(None, (E.MethodWithReflectedDefinition _), _))]) -> transpileAccAsWhenGreenFlag senv

    // <@ let $v ...$vs = $e1 in $e2 @>
    | E.Let(v, (E.Lambda _ as lambda), e2) ->
        match expressionPreTransform senv.e <| lambdaFlatten lambda with
        | Lambdas(vvs, body) ->
            do transpileAccAsWhenGreenFlag senv
            let proc = transpileProcedureSpecFromVar senv (SourceCode.ofExpr lambda) Export None v vvs body
            do transpileProcedure senv proc body
            local FE senv (addVarSpec v (ProcedureSpec proc)) <| fun senv ->
            do clearItemAcc senv
            transpileItems senv e2
        | _ -> failwith "internal error"

    // <@ let rec $v ...$vs = $e1 and ... in $e2 @>
    | LetRecursiveLambdas(recLambdas, e2) ->
        do transpileAccAsWhenGreenFlag senv
        let env =
            recLambdas
            |> List.fold (fun env (v, _, _, lambda) ->
                match expressionPreTransform senv.e lambda with
                | Lambdas(vvs, body) ->
                    let proc = transpileProcedureSpecFromVar senv (SourceCode.ofExpr lambda) Export None v vvs body
                    let env = addVarSpec v (ProcedureSpec proc) env
                    env
                | _ -> failwith "internal error"
            ) senv.e
        let senv = wiz FE env senv
        do
            List.iter (fun (v, _, body, _) ->
                match lookupVar v env with
                | ValueSome(ProcedureSpec proc) -> transpileProcedure senv proc body
                | _ -> failwith "internal error"
            ) recLambdas

        do clearItemAcc senv
        transpileItems senv e2

    // <@ let $v = $e1 in $e2 @>
    | E.Let(v, e1, e2) ->
        do transpileAccAsWhenGreenFlag senv
        let source = SourceCode.ofExpr e1
        let e1 = expressionPreTransform senv.e e1

        match e1.Type with
        | GenericType(d, _) when d = typedefof<SList<_>> ->
            let var = Var.newSimpleFromQVar v
            let list = registerList senv source Export None var NoPersistent IArray.empty
            do transpileListInit list e1
            let senv = senv |> map FE (addVarSpec v (ListSpec list))
            do clearItemAcc senv
            transpileItems senv e2

        | UnderlyingType _ ->
            let init, _ = transpileWhenGreenFlagVariableInit senv v.Name e1
            let spec = defineVariableOfQVar senv source Export v init
            let senv = senv |> map FE (addVarSpec v (VarSpec spec))
            do clearItemAcc senv
            transpileItems senv e2

        | _ -> InvalidVarType v |> raiseError (SourceCode.ofExpr e1)

    // <@ %defineSprite ...; %e2 @>
    | E.Sequential(DefineSpriteCall _, e2) -> transpileItems senv e2

    // <@ $e1; ...; $e @>
    | E.Sequential(e1, e2) ->
        let e1 = expressionPreTransform senv.e e1
        pushItemAcc senv (e1, getLocation e)
        transpileItems senv e2

    | e ->
        let e = expressionPreTransform senv.e e
        pushItemAcc senv (e, getLocation e)
        transpileAccAsWhenGreenFlag senv

let typeDatas (_: 'D The when 'D :> _ DataAttribute) (t: Type) =
    t.GetCustomAttributes<'D>(``inherit`` = true)
    |> Seq.map (fun c -> c.GetData())

let fillCostumeData data =
    let { PartialData.costumeName = costumeName } = data
    {
        costumeName = costumeName |> Option.defaultValue ""
        baseLayerMD5 = data.baseLayerMD5 |> Option.defaultValue ""
        baseLayerID = data.baseLayerID |> Option.defaultValue nan
        rotationCenterX = data.rotationCenterX |> Option.defaultValue 0.
        rotationCenterY = data.rotationCenterY |> Option.defaultValue 0.

        textLayerMD5 = data.textLayerMD5
        textLayerID = data.textLayerID
        bitmapResolution = data.bitmapResolution
    }

let updateSpriteDataByClassAttributes thisType data =
    let costumes = typeDatas the<CostumeAttribute> thisType |> Seq.map fillCostumeData |> Seq.toList
    let sounds = typeDatas the<SoundAttribute> thisType |> Seq.toList
    let currentCostumeIndex =
        typeDatas the<CurrentCostumeIndexAttribute> thisType
        |> Seq.tryHead
        |> Option.map (double >> Some)
        |> Option.defaultValue data.currentCostumeIndex

    let extension = data.ObjectDataExtension
    let extension =
        typeDatas the<SpriteAttribute> thisType
        |> Seq.tryHead
        |> Option.map (fun e -> { e with scratchX = extension.scratchX; scratchY = extension.scratchY; spriteInfo = extension.spriteInfo })
        |> Option.defaultValue extension

    let scratchX, scratchY =
        typeDatas the<PositionAttribute> thisType
        |> Seq.tryHead
        |> Option.defaultValue (extension.scratchX, extension.scratchY)

    let spriteInfo = typeDatas the<AnnotationAttribute> thisType |> Map.ofSeq
    let extension =
        { extension with
            scratchX = scratchX
            scratchY = scratchY
            spriteInfo = spriteInfo
        }

    { data with
        costumes = costumes
        sounds = sounds
        currentCostumeIndex = currentCostumeIndex
        ObjectDataExtension = extension
    }

let debugPrintMember (Get FItemEnvironment env) source (m: MemberInfo) print =
    SourceCode.location source
    |> locationText
    |> Option.defaultValue ""
    |> print (String.replicate env.depth "  ") m.DeclaringType.FullName m.Name

let (|SpriteSpecThis|_|) sprite proc e =
    match proc, sprite, e with
    | _, { this = this' }, E.Var this when this = this' -> Some()
    | Some { procedureThis = Some this' }, _, E.Var this when this = this' -> Some()
    | _ -> None

let exportOfMember (m: MemberInfo) = if m.GetCustomAttributes<ExportAttribute>(``inherit`` = true) |> Seq.isEmpty then NoExport else Export
let inliningOfMethod (m: MethodBase) =
    let f = m.MethodImplementationFlags
    if f &&& MethodImplAttributes.NoInlining <> enum 0 then Some InliningOptions.NoInlining
    elif f &&& MethodImplAttributes.AggressiveInlining <> enum 0 then Some InliningOptions.AggressiveInlining
    else None

let private addGenericInstanceOrRaise senv source (genericMethod: MethodInfo) =
    assert genericMethod.IsGenericMethod

    let addSpec senv mdid spec = modifyRef FExternalItemState senv.s <| fun s -> { s with externalMethodDefinitions = Map.add mdid spec s.externalMethodDefinitions }

    let d = genericMethod.GetGenericMethodDefinition()
    let id = MemberId.methodId d

    match Map.tryFind id (get FExternalItemState senv.s.contents).externalMethodDefinitions with
    | ValueSome { instantiatedMethodCount = count } when (get FConfig senv.e).maxGenericInstantiateCount <= count ->
        raiseError source <| MethodGenericInstanceTooMany(d, count)

    | ValueSome spec -> addSpec senv id { spec with instantiatedMethodCount = spec.instantiatedMethodCount + 1 }
    | ValueNone -> addSpec senv id { instantiatedMethodCount = 1 }

let rec transpileSpriteExternalMemberSpecs senv sprite proc e =
    let externalItems = (get FConfig senv.e).plugin.externalItems
    Expr.fold (fun () e ->
        match externalItems.Invoke senv e with
        | Error(Some e) -> raise <| TranspileException e
        | Error None -> ()
        | Ok() -> ()

        match e with

        // <@ self.Method1(...) @> | <@ Type.Call(...) @> | <@ obj.Method1(...) @>
        | E.Call((None | Some(SpriteSpecThis sprite proc ())), SharingGenericMethod(E.MethodWithReflectedDefinition lambda as m), _)
        | NonVirtualInstanceCall(_, SharingGenericMethod(E.MethodWithReflectedDefinition lambda as m), _)
            when lookupMethod m (get FExternalItemState senv.s.contents).externalEnv |> VOption.isNone ->
            let source = SourceCode.ofExpr e
            if m.IsGenericMethod then addGenericInstanceOrRaise senv source m

            let lambdaSource = SourceCode.ofExpr lambda
            let lambda = lambdaFlatten lambda
            let vvs, body =
                match lambda with
                | Lambdas(vvs, body) -> vvs, body

                | body -> [], body

            transpileSpriteMethodSpecs senv sprite vvs body lambdaSource source m

        | E.FieldSet(Some(SpriteSpecThis sprite proc ()), f, init) when lookupField f (get FExternalItemState senv.s.contents).externalEnv |> VOption.isNone ->
            transpileSpriteFieldSpec senv sprite f (Some init)

        | E.FieldGet(Some(SpriteSpecThis sprite proc ()), f) when lookupField f (get FExternalItemState senv.s.contents).externalEnv |> VOption.isNone ->
            transpileSpriteFieldSpec senv sprite f None

        | _ -> ()

    ) () e

and transpileSpriteMethodSpecs senv sprite vvs body lambdaSource callSource m =
    let config = get FConfig senv.e
    let { externalEnv = env } = get FExternalItemState senv.s.contents
    if OutputLevel.Debug <= config.outputLevel then
        debugPrintMember senv.e callSource m <| printfn "%s%s.%s(...) %s"

    let body = if not m.IsStatic then removeClassInitCheck sprite body else body
    let body = expressionPreTransform senv.e body
    let parameters = transpileProcedureParameters senv vvs lambdaSource
    let var = Var.newProcFromMethodInfo senv.e lambdaSource m parameters body
    let spec = transpileProcedureSpec (exportOfMember m) (inliningOfMethod m) var parameters body
    let env = addMethodSpec m (ProcedureSpec spec) env
    modifyRef FExternalItemState senv.s <| fun s -> { s with externalEnv = env }

    do
        let senv = senv |> map FE (map FItemEnvironment <| fun e -> { e with depth = e.depth + 1 })
        transpileSpriteExternalMemberSpecs senv sprite (Some spec) body

    modifyRef FExternalItemState senv.s <| fun s -> { s with externalAcc = ExternalProcedure(spec, body)::s.externalAcc }

and transpileSpriteFieldSpec senv spec f init =
    let config = get FConfig senv.e
    if OutputLevel.Debug <= config.outputLevel then
        debugPrintMember senv.e (SourceCode.ofExpr <| Option.defaultValue (E.Value(())) init) f <| printfn "%s%s.%s %s"

    let source = match init with None -> SourceCode.empty | Some e -> SourceCode.ofExpr e

    match f.FieldType with
    | GenericType(d, [UnderlyingType _]) when d = typedefof<SList<_>> ->
        let var = Var.newSimpleFromFieldInfo senv.e f
        let spec = registerList senv source Export None var NoPersistent IArray.empty
        modifyRef FExternalItemState senv.s <| fun s -> { s with externalEnv = addFieldSpec f (ListSpec spec) s.externalEnv }

    | UnderlyingType _ ->
        let t = underlyingMemberTypeOrRaise senv.e source f.FieldType
        let spec = declareVariable Export NoPersistent (not f.IsInitOnly) (config.fieldName f) t
        modifyRef FExternalItemState senv.s <| fun s -> { s with externalEnv = addFieldSpec f (VarSpec spec) s.externalEnv }

        match init with
        | Some init -> pushExternalAcc senv <| ExternalVariable(spec, init)

        // ???
        | _ -> ()

    | _ ->
        let init = match init with Some init -> SourceCode.ofExpr init | _ -> spec.spriteSource
        InvalidFieldType f |> raiseError init

let transpileSpriteMembers senv spec =
    let states' = senv.s.contents
    let states =
        let senv = {
            e = ItemEnvironments.make({ depth = 0; bySprite = Some spec }, senv.e)
            s = ref <| ExternalItemStates.make {
                externalEnv = Environments.make(get FEnvironment senv.e, senv.e)
                externalMethodDefinitions = Map.empty
                externalAcc = []
                externalState = get FState states'
            }
        }
        transpileSpriteExternalMemberSpecs senv spec None spec.items
        senv.s.contents

    let { externalEnv = Get FEnvironment env; externalAcc = acc; externalState = state } = get FExternalItemState states
    let env = { env with rare = { env.rare with sprite = Some spec } }

    let states =
        let senv = { s = ref <| States.make(state); e = Environments.make(env, senv.e) }
        for spec in List.rev acc do
            match spec with
            | ExternalProcedure(proc, body) -> transpileProcedure senv proc body
            | ExternalList(list, init) -> transpileListInit list init
            | ExternalVariable(var, init) ->
                let source = SourceCode.ofExpr init
                let init, _ = transpileWhenGreenFlagVariableInit senv (Var.name var.var) init
                implementVariable senv source var init

            | ExternalSprite _ -> failwith "internal error"

        senv.s.contents

    senv.s := states' |> wiz FState (get FState states)
    env

let visibleInSprite = function
    | ProcedureSpec _ -> false
    | ListSpec _
    | SpriteSpec _
    | VarSpec _ -> true

let transpileSprite senv spec = run senv <| fun envs stageStates ->
    let Get FStageDataExtension stageExtension & Get FState stageState = stageStates

    let thisType = spec.this.Type
    let data = SpriteData.defaultValue
    let data = updateSpriteDataByClassAttributes thisType data
    let data = { data with objName = spec.spriteUniqueName }
    let extensionData = data.ObjectDataExtension
    let data = EntityData.mapExtension ignore data
    let state = {
        spriteNamespace = stageState.spriteNamespace
        data = data
    }
    let env, states =
        let env = get FEnvironment envs
        let env = { env with vars = env.vars |> Map.filter (fun _ v -> visibleInSprite v) }
        let senv = {
            e = Environments.make(env, envs)
            s = ref <| States.make state
        }
        let env = transpileSpriteMembers senv spec
        env, senv.s.contents

    let Get FItemState itemState & Get FState { data = data } =
        let senv = {
            e = Environments.make(env, envs)
            s = ref <| ItemStates.make({ itemAcc = [] }, states)
        }
        transpileItems senv spec.items
        senv.s.contents

    match itemState.itemAcc with
    | [] -> ()
    | _ -> failwith "internal error"

    let data =
        { data with
            variables = List.rev data.variables
            lists = List.rev data.lists
            scripts = List.rev data.scripts
        }

    let data = EntityData.mapExtension (fun () -> extensionData) data
    let stageExtension = { stageExtension with StageDataExtension.children = Choice2Of3 data::stageExtension.children }
    (), stageStates |> wiz FStageDataExtension stageExtension

let (|SpriteSpecThis'|_|) (Get FItemEnvironment { bySprite = sprite }) e =
    match sprite with
    | None -> None
    | Some sprite -> (|SpriteSpecThis|_|) sprite None e

let isVisited id senv = lookupSpec id senv.s.contents |> VOption.isSome

type private TranspileExternalItemSpecsFrame<'SE> =
    | FoldTailRec of 'SE * Expr
    | FoldTailRec_2 of 'SE * Expr
    | FoldTailRec_3 of 'SE * Expr list

    | TranspileExternalItemSpecs of 'SE * Expr
    | TranspileExternalItemSpecs' of 'SE * Expr
    | TranspileExternalItemListSpecs of 'SE * Expr list

    | TranspileExternalSpriteSpecs of 'SE * ConstructorInfo * SourceCode
    | TranspileExternalSpriteSpecs_2 of 'SE * SpriteSpec

    | TranspileExternalMethodSpecs of 'SE * SourceCode * lambda: Expr * MethodInfo
    | TranspileExternalMethodSpecs_2 of 'SE * ProcedureSpec * body: Expr

    | TranspileExternalPropertySpecs of 'SE * SourceCode * init: Expr * PropertyInfo
    | TranspileExternalPropertySpecs_2 of 'SE * ListSpec * init: Expr
    | TranspileExternalPropertySpecs_3 of 'SE * VariableSpec * init: Expr

module private TranspileExternalItemSpecs =
    let ( ** ) x (stack: _ Stack) = stack.Push x; stack

    let transpileExternalItemSpecs'(senv, e) stack =
        let plugin = (get FConfig senv.e).plugin.externalItems
        match plugin.Invoke senv e with
        | Error(Some e) -> raise <| TranspileException e
        | Error None -> ()
        | Ok() -> ()

        let source = SourceCode.ofExpr e
        match e with
        | DefineListCall _ -> stack
        | DefineSpriteCall(None, _, [e1]) ->
            match e1 with
            | E.Lambda(v, E.NewObject(c, [E.Var v'])) when v = v' ->
                let (GenericTypeDefinition spriteType) = c.DeclaringType
                if not <| isVisited (typeId spriteType) senv
                then TranspileExternalSpriteSpecs(senv, c, source)**stack
                else stack

            | _ -> raiseError source InvalidSpriteImportForm

        | E.Call((None | Some(SpriteSpecThis' senv.e ())), SharingGenericMethod(E.MethodWithReflectedDefinition lambda as m), _)
        | NonVirtualInstanceCall(_, SharingGenericMethod(E.MethodWithReflectedDefinition lambda as m), _)

            // スプライトのみから参照されたグローバルなメソッドはステージに登録する必要はないが、
            // スプライトのみから参照されたグローバルなメソッドから参照された変数はステージに登録する必要がある
            //
            // ```
            // let out v = SList.push v output
            //
            // [<Sealed>]
            // type Sprite1(options) =
            //     inherit Sprite(options)
            //     do self.WhenGreenFlag { out "Sprite1" }
            // ```
            // =>
            // ```
            // let output = []
            //
            // sprite Sprite1 =
            //     let out(v) = append:List:of: v output
            //     whenGreenFlag \{
            //         call out ("Sprite1")
            //     }
            // ```
            when not <| isVisited (methodId m) senv ->
            TranspileExternalMethodSpecs(senv, source, lambda, m)**stack

        | E.PropertySet(None, (E.PropertyGetterWithReflectedDefinition init as p), [], _)
        | E.PropertyGet(None, (E.PropertyGetterWithReflectedDefinition init as p), [])
            when not <| isVisited (propertyId p) senv ->

            let init = expressionPreTransform senv.e init
            TranspileExternalPropertySpecs(senv, source, init, p)**stack

        | _ -> stack

    let transpileExternalMethodSpecs(senv, source, lambda, m: MethodInfo) stack =
        if m.IsGenericMethod then addGenericInstanceOrRaise senv source m

        let config = get FConfig senv.e
        if OutputLevel.Debug <= config.outputLevel then debugPrintMember senv.e source m <| printfn "%s%s.%s(...) %s"
        let lambdaSource = SourceCode.ofExpr lambda

        let lambda = lambdaFlatten lambda
        let lambda =
            if not m.IsStatic then
                match (get FItemEnvironment senv.e).bySprite with
                | Some sprite -> removeClassInitCheck sprite lambda
                | _ -> lambda
            else
                lambda

        let lambda = expressionPreTransform senv.e lambda
        let vvs, body =
            match lambda with
            | Lambdas(vvs, body) -> vvs, body

            // 型関数はラムダでない ( 例: `let x<'a> = 10` → <@ 10 @>)
            | lambda -> [[QVar("unitVar", typeof<unit>)]], lambda

        let parameters = transpileProcedureParameters senv vvs source
        let var = Var.newProcFromMethodInfo senv.e lambdaSource m parameters body
        let spec = transpileProcedureSpec (exportOfMember m) (inliningOfMethod m) var parameters body
        addExternalSpec senv (methodId m) (ProcedureSpec spec)

        let senv' = senv |> map FE (map FItemEnvironment (fun e -> { e with depth = e.depth + 1 }))
        TranspileExternalItemSpecs(senv', body)**TranspileExternalMethodSpecs_2(senv, spec, body)**stack

    let transpileExternalMethodSpecs_2(senv, spec, body) stack =
        pushExternalAcc senv <| ExternalProcedure(spec, body)
        stack

    let transpileExternalPropertySpecs(senv, source, init, p: PropertyInfo) stack =
        let config = get FConfig senv.e

        if OutputLevel.Debug <= config.outputLevel then debugPrintMember senv.e source p <| printfn "%s%s.%s %s"

        let source = SourceCode.ofExpr init
        match p.PropertyType with
        | GenericType(d, [UnderlyingType _]) when d = typedefof<SList<_>> ->
            let var = Var.newSimpleFromPropertyInfo senv.e p
            let spec = registerList senv source (exportOfMember p) None var NoPersistent IArray.empty
            addExternalSpec senv (propertyId p) (ListSpec spec)

            let senv' = senv |> map FE (map FItemEnvironment (fun e -> { e with depth = e.depth + 1 }))
            TranspileExternalItemSpecs(senv', init)**TranspileExternalPropertySpecs_2(senv, spec, init)**stack

        | UnderlyingType _ ->
            let t = underlyingMemberTypeOrRaise senv.e source p.PropertyType
            let isMutable = T.IsModule p.DeclaringType && p.CanWrite
            let spec = declareVariable (exportOfMember p) NoPersistent isMutable (config.propertyName p) t
            addExternalSpec senv (propertyId p) (VarSpec spec)
            let senv' = senv |> map FE (map FItemEnvironment (fun e -> { e with depth = e.depth + 1 }))
            TranspileExternalItemSpecs(senv', init)**TranspileExternalPropertySpecs_3(senv, spec, init)**stack

        | _ -> InvalidPropertyType p |> raiseError source

    let transpileExternalPropertySpecs_2(senv, spec, init) stack =
        pushExternalAcc senv <| ExternalList(spec, init)
        stack

    let transpileExternalPropertySpecs_3(senv, spec, init) stack =
        pushExternalAcc senv <| ExternalVariable(spec, init)
        stack

    let transpileExternalSpriteSpecs(senv, c, source) stack =
        let config = get FConfig senv.e
        if OutputLevel.Debug <= config.outputLevel then debugPrintMember senv.e source c <| fun a b c _ -> printfn "%s%s.%s" a b c

        match c with
        | Class.ClassWithReflectedDefinition r ->
            match r with
            | Ok(classDef, classExpr) ->
                let classSource = SourceCode.ofExpr classExpr
                match classDef with

                // <@ type Sprite1(options) ... = inherit Sprite(options) ... @>
                | { constructorParameters = [options]; baseConstructorArguments = [E.Var options'] } when options = options' ->
                    let thisType = classDef.this.Type
                    let varName = config.spriteName thisType
                    let uniqueName = declareSprite senv varName
                    let tops = classDef.tops
                    let tops = tops.beforeInit @ tops.afterInit
                    let tops = List.foldBack (fun top tops -> E.Sequential(top, tops)) tops (E.Value(()))
                    let spec = {
                        spriteUniqueName = uniqueName
                        spriteSource = classSource
                        this = classDef.this
                        selfField = classDef.selfField
                        items = tops
                    }
                    addExternalSpec senv (typeId thisType) (SpriteSpec spec)
                    let senv' = senv |> map FE (map FItemEnvironment (fun e -> { e with depth = e.depth + 1; bySprite = Some spec }))
                    TranspileExternalItemListSpecs(senv', classDef.tops.beforeInit)**
                    TranspileExternalItemListSpecs(senv', classDef.tops.afterInit)**
                    TranspileExternalSpriteSpecs_2(senv, spec)**
                    stack

                | _ -> InvalidSpriteDefinitionForm |> raiseError classSource

            | Error error ->
                InvalidClassDefinition error |> raiseError source

    let transpileExternalSpriteSpecs_2(senv, spec) stack =
        pushExternalAcc senv <| ExternalSprite spec
        stack

    let dispatch stack = function
        | TranspileExternalItemSpecs(senv, e) -> FoldTailRec(senv, e)**stack
        | TranspileExternalItemListSpecs(senv, es) ->
            match es with
            | [] -> stack
            | e::es -> TranspileExternalItemSpecs(senv, e)**TranspileExternalItemListSpecs(senv, es)**stack

        | FoldTailRec(senv, e) -> TranspileExternalItemSpecs'(senv, e)**FoldTailRec_2(senv, e)**stack
        | FoldTailRec_2(senv, e) ->
            match e with
            | E.ShapeVar _ -> stack
            | E.ShapeLambda(_, e) -> FoldTailRec(senv, e)**stack
            | E.ShapeCombination(_, es) -> FoldTailRec_3(senv, es)**stack

        | FoldTailRec_3(senv, es) ->
            match es with
            | [] -> stack
            | e::es -> FoldTailRec(senv, e)**FoldTailRec_3(senv, es)**stack

        | TranspileExternalItemSpecs'(senv, e) -> transpileExternalItemSpecs'(senv, e) stack
        | TranspileExternalMethodSpecs(senv, source, lambda, m) -> transpileExternalMethodSpecs(senv, source, lambda, m) stack
        | TranspileExternalMethodSpecs_2(senv, p, body) -> transpileExternalMethodSpecs_2(senv, p, body) stack
        | TranspileExternalPropertySpecs(senv, source, init, p) -> transpileExternalPropertySpecs(senv, source, init, p) stack
        | TranspileExternalPropertySpecs_2(senv, l, init) -> transpileExternalPropertySpecs_2(senv, l, init) stack
        | TranspileExternalPropertySpecs_3(senv, v, init) -> transpileExternalPropertySpecs_3(senv, v, init) stack
        | TranspileExternalSpriteSpecs(senv, c, source) -> transpileExternalSpriteSpecs(senv, c, source) stack
        | TranspileExternalSpriteSpecs_2(senv, s) -> transpileExternalSpriteSpecs_2(senv, s) stack

let transpileExternalItemSpecs senv e =
    let stack = Stack()
    stack.Push <| TranspileExternalItemSpecs(senv, e)
    while stack.Count <> 0 do
        stack.Pop() |> TranspileExternalItemSpecs.dispatch stack |> ignore

let transpileExternalItems senv e =
    let states = ref <| ExternalItemStates.make {
        externalEnv = senv.e
        externalMethodDefinitions = Map.empty
        externalState = get FState senv.s.contents
        externalAcc = []
    }
    let senv' = { e = ItemEnvironments.make({ depth = 0; bySprite = None }, senv.e); s = states }
    do transpileExternalItemSpecs senv' e

    let { externalEnv = env; externalState = state; externalAcc = acc } = get FExternalItemState senv'.s.contents
    let senv'' = { s = ref (SpriteStates.make(get FStageDataExtension senv.s.contents, state)); e = env }
    for x in List.rev acc do
        match x with
        
        // スプライト内のプロシージャは、ステージからは見えない
        | ExternalProcedure({ procedureThis = Some _ }, _) -> ()

        | ExternalProcedure({ procedureThis = None } as proc, body) -> transpileProcedure senv'' proc body
        | ExternalVariable(var, body) ->
            let init, _ = transpileWhenGreenFlagVariableInit senv'' (Var.name var.var) body
            implementVariable senv'' (SourceCode.ofExpr body) var init

        | ExternalList(list, body) -> transpileListInit list body
        | ExternalSprite sprite -> transpileSprite senv'' sprite

    senv.s := senv''.s.contents
    env

let convertExprToIR initialData config e =
    let envs = { Environments.Environment = newEmptyEnv(); Config = config }
    let extensionData = initialData.ObjectDataExtension
    let state = emptyState (EntityData.mapExtension ignore initialData)
    let e = topLevelPreTransform config e
    let states = ref <| SpriteStates.make(extensionData, state)
    let env = transpileExternalItems { e = envs; s = states } e
    let Get FStageDataExtension extensionData & states = states.contents
    let states = ref <| ItemStates.make({ itemAcc = [] }, states)
    let senv = { e = env; s = states }
    do transpileItems senv e
    let Get FItemState { itemAcc = itemAcc } & Get FState { data = data } = states.contents
    match itemAcc with
    | [] -> ()
    | _ -> failwith "internal error"

    let data =
        { data with
            variables = List.rev data.variables
            lists = List.rev data.lists
            scripts = List.rev data.scripts
        }

    EntityData.mapExtension (fun () -> extensionData) data

let transpileEntity initialData config e =
    let data = convertExprToIR initialData config e
    let data = IRToStageData.convertWith (fun _ -> config.ir) data
    let data = postTransform config data
    let data = fixBlocks data
    StageData.applyScratch3ExecutionOrderTrick data
