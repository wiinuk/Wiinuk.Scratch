module Scratch.Reflection.Expr
open FSharp.Quotations
open FSharp.Reflection
open System.Reflection
open System
open System.Runtime.CompilerServices
open System.Text.RegularExpressions
module E = FSharp.Quotations.ExprShape
module E = FSharp.Quotations.Patterns
module E = FSharp.Quotations.DerivedPatterns
type private E = FSharp.Quotations.Expr
type private V = FSharp.Reflection.FSharpValue
type private T = FSharp.Reflection.FSharpType


let rec tryPick (|Pick|_|) = function
    | Pick x -> Some x
    | E.ShapeVar _ -> None
    | E.ShapeLambda(_, e) -> tryPick (|Pick|_|) e
    | E.ShapeCombination(_, es) -> List.tryPick (tryPick (|Pick|_|)) es

module internal ToLinqExpression =
    open System.Linq.Expressions
    type internal L = System.Linq.Expressions.Expression
    type internal K = System.Linq.Expressions.ExpressionType

    [<Struct>]
    [<NoComparison; NoEquality>]
    type ConvertEnv = {
        epath: struct(string * int) list
        vars: Map<Var, ParameterExpression>
    }
    module ConvertEnv =
        let errorf env e format =
            let cont m =
                let path = env.epath |> Seq.rev |> Seq.map (fun struct(n, i) -> $"%s{n}.[{i}]") |> String.concat "."
                failwithf $"{path}: %s{m}; {e}"

            Printf.ksprintf cont format

        let child p n env = { env with epath = struct(p, n)::env.epath }

        let lookup env v e =
            let mutable result = Unchecked.defaultof<_>
            if env.vars.TryGetValue(v, &result) then
                result :> L
            else
                errorf env e $"free var: {v}"

        let bind (v, v') env = { env with vars = Map.add v v' env.vars }
    open ConvertEnv

    let rec toExpression env e =
        match e with
        | E.Value(x, t)
        | E.ValueWithName(x, t, _) -> L.Constant(x, t) :> L
        | E.Var v -> lookup env v e
        | E.VarSet(v, e1) -> L.Assign(lookup env v e, toExpression (child "VarSet" 0 env) e1) :> L
        | E.Let x -> letToExpression env x
        | E.UnionCaseTest x -> unionCaseTestToExpression env x e
        | E.PropertyGet x -> propertyGetToExpression env x
        | E.PropertySet x -> propertySetToExpression env x
        | E.OrElse(e1, e2) -> L.OrElse(toExpression (child "OrElse" 0 env) e1, toExpression (child "OrElse" 1 env) e2) :> L
        | E.AndAlso(e1, e2) -> L.AndAlso(toExpression (child "AndAlso" 0 env) e1, toExpression (child "AndAlso" 1 env) e2) :> L
        | E.IfThenElse x -> ifThenElseToExpression env x
        | E.Call x -> callToExpression env x e
        | E.TupleGet x -> tupleGetToExpression env x e
        | E.NewTuple x -> newTupleToExpression env x e
        | E.NewRecord x -> newRecordToExpression env x
        | E.NewUnionCase x -> newUnionCaseToExpression env x
        | E.Sequential x -> sequentialToExpression env x
        | E.DefaultValue t -> L.Default t :> L
        | E.FieldGet x -> fieldGetToExpression env x
        | E.FieldSet x -> fieldSetToExpression env x
        | E.NewArray(t, es) -> L.NewArrayInit(t, es |> Seq.mapi (fun i x -> toExpression (child "NewArray" i env) x)) :> L
        | E.NewObject(c, es) -> L.New(c, es |> Seq.mapi (fun i x -> toExpression (child "NewObject" i env) x)) :> L
        | E.Coerce(e1, t) -> L.Convert(toExpression (child "Coerce" 0 env) e1, t) :> L

        //| E.AddressOf _
        //| E.AddressSet _
        //| E.Application _
        //| E.Coerce _
        //| E.ForIntegerRangeLoop _
        //| E.Lambda _
        //| E.LetRecursive _
        //| E.NewDelegate _
        //| E.QuoteRaw _
        //| E.QuoteTyped _
        //| E.TryFinally _
        //| E.TryWith _
        //| E.TypeTest _
        //| E.WhileLoop _
        //| E.WithValue _
        | _ -> errorf env e "not implemented expression"

    // <@ var x; (x = %e1, %e2); @>
    and letToExpression env (v, e1, e2) =
        let n = "Let"
        let v' = L.Parameter(v.Type, v.Name)
        let e1 = L.Assign(v', toExpression (child n 0 env) e1) :> L
        let env = bind (v, v') env
        let e2 = toExpression (child n 1 env) e2
        L.Block([v'], [e1; e2]) :> L

    // <@ %getTag %e = %caseTag @>
    and unionCaseTestToExpression env (e1, case) e =
        let e1 = toExpression (child "UnionCaseTest" 0 env) e1
        let getTag =
            match V.PreComputeUnionTagMemberInfo(case.DeclaringType, allowAccessToPrivateRepresentation = true) with
            | :? PropertyInfo as p -> L.Property(e1, p) :> L
            | :? MethodInfo as m -> L.Call(m, e1) :> L
            | m -> errorf env e $"unknown UnionTagMember {m}"

        L.Equal(getTag, L.Constant case.Tag) :> L

    and propertyGetToExpression env (this, p, es) =
        propertyAccess env "PropertyGet" (this, p, es) |> fst

    and propertySetToExpression env (this, p, es, value) =
        let n = "PropertySet"
        let access, advance = propertyAccess env n (this, p, es)
        let i = advance + List.length es
        let value = toExpression (child n i env) value
        L.Assign(access, value) :> L

    and propertyAccess env n (this, p, es) =
        let this = this |> Option.map (toExpression (child n 0 env))
        let advance = if Option.isNone this then 0 else 1
        let this = this |> Option.toObj
        let access =
            match es with
            | [] -> L.Property(this, p) :> L
            | _ ->
                let es = es |> Seq.mapi (fun i x -> toExpression (child n (i + advance) env) x)
                L.Property(this, p, es) :> L

        access, advance

    and callToExpression env (this, m, es) e =
        match e with
        | Binary <@@ (=) @@> env K.Equal e
        | Binary <@ (<>) @> env K.NotEqual e
        | Binary <@ (<) @> env K.LessThan e
        | Binary <@ (<=) @> env K.LessThanOrEqual e
        | Binary <@ (>) @> env K.GreaterThan e
        | Binary <@ (>=) @> env K.GreaterThanOrEqual e

        | Binary <@ (+) @> env K.Add e
        | Binary <@ (-) @> env K.Subtract e
        | Binary <@ (*) @> env K.Multiply e
        | Binary <@ (/) @> env K.Divide e
        | Binary <@ (%) @> env K.Modulo e
        | Unary <@@ (~-) @@> env K.Negate e
        | Unary <@ (~+) @> env K.UnaryPlus e

        | Binary <@ Checked.(+) @> env K.AddChecked e
        | Binary <@ Checked.(-) @> env K.SubtractChecked e
        | Binary <@ Checked.(*) @> env K.MultiplyChecked e
        | Unary <@ Checked.(~-) @> env K.NegateChecked e

        | Binary <@ (&&&) @> env K.And e
        | Binary <@ (|||) @> env K.Or e
        | Binary <@ (^^^) @> env K.ExclusiveOr e
        | Binary <@ (<<<) @> env K.LeftShift e
        | Binary <@ (>>>) @> env K.RightShift e

        | Unary <@ not @> env K.Not e
        | Unary <@ (~~~) @> env K.OnesComplement e -> e
        | _ ->

        let n = "Call"
        let this = this |> Option.map (toExpression (child n 0 env))
        let advance = if Option.isNone this then 0 else 1
        let es = es |> Seq.mapi (fun i x -> toExpression (child n (i + advance) env) x)
        match this with
        | None -> L.Call(m, es) :> L
        | Some this -> L.Call(this, m, es) :> L

    and (|Binary|_|) template env kind = function

        // bool byte sbyte int16 uint16 int32 uint32 int64 uint64 nativeptr unativeptr char double single
        | E.SpecificCall template (_, t1::ts, [e1; e2]) when t1.IsPrimitive && List.forall ((=) t1) ts ->
            let name = "Call"
            let e1 = toExpression (child name 0 env) e1
            let e2 = toExpression (child name 1 env) e2
            L.MakeBinary(kind, e1, e2) :> L |> Some

        | _ -> None

    and (|Unary|_|) template env kind = function

        // bool byte sbyte int16 uint16 int32 uint32 int64 uint64 nativeptr unativeptr char double single
        | E.SpecificCall template (_, t1::ts, [e1]) when t1.IsPrimitive && List.forall ((=) t1) ts ->
            let name = "Call"
            let e1 = toExpression (child name 0 env) e1
            L.MakeUnary(kind, e1, null) :> L |> Some

        | _ -> None

    and ifThenElseToExpression env (test, ifTrue, ifFalse) =
        let n = "IfThenElse"
        L.Condition(toExpression (child n 0 env) test, toExpression (child n 1 env) ifTrue, toExpression (child n 2 env) ifFalse) :> L

    and newRecordToExpression env (t, es) =
        let c = V.PreComputeRecordConstructorInfo(t, allowAccessToPrivateRepresentation = true)
        let es = es |> Seq.mapi (fun i x -> toExpression (child "NewRecord" i env) x)
        L.New(c, es) :> L

    // <@ new %T(%e0, %e1, ... new %T(..., %eN)) @>
    and newTupleToExpression env es e =
        let n = "NewTuple"
        let rec newTuple tupleT advance es =
            match V.PreComputeTupleConstructorInfo tupleT with
            | c, None ->
                let es = es |> Seq.mapi (fun i x -> toExpression (child n (i + advance) env) x)
                L.New(c, es)

            | c, Some restT ->
                let chunkSize = 7
                let es, rests = List.splitAt chunkSize es
                let es = es |> Seq.mapi (fun i x -> toExpression (child n (i + advance) env) x)
                let rest = newTuple restT (advance + chunkSize) rests
                L.New(c, Seq.append es [rest])

        newTuple e.Type 0 es :> L

    // <@ %e1.Rest.Item2 @>
    and tupleGetToExpression env (e1, index) e =
        let rec tupleGet e1 tupleT index =
            match V.PreComputeTuplePropertyInfo(tupleT, index) with
            | p, None -> L.Property(e1, p)
            | p, Some(restT, restIndex) -> tupleGet (L.Property(e1, p) :> L) restT restIndex

        let e1 = toExpression (child "TupleGet" 0 env) e1
        tupleGet e1 e.Type index :> L

    and newUnionCaseToExpression env (c, es) =
        let newUnionM = V.PreComputeUnionConstructorInfo(c, allowAccessToPrivateRepresentation = true)
        let es = es |> Seq.mapi (fun i x -> toExpression (child "NewUnionCase" i env) x)
        L.Call(newUnionM, es) :> L

    and sequentialToExpression env (e1, e2) =
        let n = "Sequential"
        L.Block(toExpression (child n 0 env) e1, toExpression (child n 1 env) e2) :> L

    and fieldGetToExpression env (this, f) =
        let this = this |> Option.map (toExpression (child "FieldGet" 0 env))
        L.Field(Option.toObj this, f) :> L

    and fieldSetToExpression env (this, f, value) =
        let n = "FieldSet"
        let this = this |> Option.map (toExpression (child n 0 env))
        let advance = if Option.isNone this then 0 else 1
        let value = value |> toExpression (child n advance env)
        L.Assign(L.Field(Option.toObj this, f), value) :> L

    let toFuncExpression (e: Expr<'T -> 'R>) =
        let env = { epath = []; vars = Map.empty }
        match e with
        | E.Lambda(v, body) ->
            let v' = L.Parameter(v.Type, v.Name)
            let body' = toExpression (bind (v, v') env) body
            L.Lambda<Func<'T,'R>>(body', v')

        | e -> errorf env e "requires Lambda expression: e.g. <@ fun x -> x @>"

let toLinqLambdaExpression e = ToLinqExpression.toFuncExpression e

let showLocation = function
    | None -> "???(?,?)"
    | Some x -> $"%s{x.path}({x.position1.line},{x.position1.column})"

let private exprLocationTable = System.Runtime.CompilerServices.ConditionalWeakTable()

let rebuild = function
    | E.ShapeVar x -> E.Var x
    | E.ShapeLambda(x, e) -> E.Lambda(x, e)
    | E.ShapeCombination(shape, es) -> E.RebuildShapeCombination(shape, es)

let withLocationTyped ca e =
    let mutable r = None
    if exprLocationTable.TryGetValue(e :> Expr, &r) then
        let rebuildTyped (e: 'T Expr) =
            let e' = E.Cast<'T> e
            if LanguagePrimitives.PhysicalEquality e e' then 
                rebuild e |> E.Cast<'T>
            else e'

        let e' = rebuildTyped e
        assert (LanguagePrimitives.PhysicalEquality e e' |> not)
        exprLocationTable.Add(e', ca)
        e'
    else
        exprLocationTable.Add(e, ca)
        e

let withLocation ca e =
    let mutable r = None
    if exprLocationTable.TryGetValue(e, &r) then
        let e' = rebuild e
        assert (LanguagePrimitives.PhysicalEquality e e' |> not)
        exprLocationTable.Add(e', ca)
        e'
    else
        exprLocationTable.Add(e, ca)
        e

let private (|DebugRange|_|) = function
    | E.NewTuple(E.String "DebugRange"::E.NewTuple(E.String path::E.Int32 l1::E.Int32 c1::E.Int32 l2::E.Int32 c2::_)::_) ->
        Some {
            path = path
            position1 =
                {
                line = l1
                column = c1
                }
            position2 =
                {
                line = l2
                column = c2
                }
        }
    | _ -> None

let private getLocationRaw (e: Expr) = List.tryPick (|DebugRange|_|) e.CustomAttributes
let private getLocationRawDelegate = System.Runtime.CompilerServices.ConditionalWeakTable.CreateValueCallback getLocationRaw
let getLocation e = exprLocationTable.GetValue(e, getLocationRawDelegate)

let fold folder state e =
    let rec foldTailRec state = function
        | [] -> state
        | Choice1Of2 e::callStack ->
            let state = folder state e
            match e with
            | E.ShapeVar _ -> foldTailRec state callStack
            | E.ShapeLambda(_, e) -> foldTailRec state (Choice1Of2 e::callStack)
            | E.ShapeCombination(_, es) -> foldTailRec state (Choice2Of2 es::callStack)

        | Choice2Of2 es::callStack ->
            match es with
            | [] -> foldTailRec state callStack
            | e::es -> foldTailRec state (Choice1Of2 e::Choice2Of2 es::callStack)

    foldTailRec state [Choice1Of2 e]

let maxCost: Expr -> _ =
    let minCost = LiteralLike
    let knownMaxCost = HasSideEffect

    let pureBy f ts = if ts |> List.forall f then Pure else HasSideEffect
    let smallType t = match trySizeOf t with Some size when size <= sizeof<nativeint> * 4 -> true | _ -> false

    /// boolean | int8 | uint8 | int16 | uint16 | int32 | uint32 | int64 | uint64 | nativeint | unativeint | char | double | single
    let isPrimitive (t: Type) = t.IsPrimitive
    /// isPrimitive t | bigint | decimal
    let isPrimitiveOrNumeric t = isPrimitive t || typeof<bigint> = t || typeof<decimal> = t
    /// isPrimitive t | bigint | decimal | string | unit
    let isBasic t = isPrimitiveOrNumeric t || typeof<string> = t || typeof<unit> = t

    let operatorsCost = function
        | E.SpecificCall <@ Operators.(@) @> _ -> Pure

        | E.SpecificCall <@ Operators.(/) @> (_, ts, _)
        | E.SpecificCall <@ Operators.(%) @> (_, ts, _) -> ts |> pureBy (fun t -> t = typeof<double> || t = typeof<float>)
        | E.SpecificCall <@ Operators.(+) @> (_, ts, _) -> ts |> pureBy (fun t -> isPrimitiveOrNumeric t || t = typeof<string>)

        | E.SpecificCall <@ Operators.( ** ) @> (_, ts, _)
        | E.SpecificCall <@ Operators.(*) @> (_, ts, _)
        | E.SpecificCall <@ Operators.(-) @> (_, ts, _)
        | E.SpecificCall <@ Operators.(~-) @> (_, ts, _) -> ts |> pureBy isPrimitiveOrNumeric

        | E.SpecificCall <@ Operators.(~+) @> (_, ts, _) -> if ts |> List.forall isPrimitiveOrNumeric then LiteralLike else HasSideEffect

        | E.SpecificCall <@ Operators.(=) @> (_, ts, _)
        | E.SpecificCall <@ Operators.(<>) @> (_, ts, _)
        | E.SpecificCall <@ Operators.(>) @> (_, ts, _)
        | E.SpecificCall <@ Operators.(>=) @> (_, ts, _)
        | E.SpecificCall <@ Operators.(<) @> (_, ts, _)
        | E.SpecificCall <@ Operators.(<=) @> (_, ts, _) -> ts |> pureBy isBasic

        | E.SpecificCall <@ Operators.(>>) @> _
        | E.SpecificCall <@ Operators.(<<) @> _
        | E.SpecificCall <@ Operators.(|>) @> _
        | E.SpecificCall <@ Operators.(||>) @> _
        | E.SpecificCall <@ Operators.(|||>) @> _
        | E.SpecificCall <@ Operators.(<|) @> _
        | E.SpecificCall <@ Operators.(<||) @> _
        | E.SpecificCall <@ Operators.(<|||) @> _
        | E.SpecificCall <@ Operators.(..) @> _
        | E.SpecificCall <@ Operators.(.. ..) @> _ -> Pure

        | E.SpecificCall <@ Operators.(^^^) @> (_, ts, _)
        | E.SpecificCall <@ Operators.(|||) @> (_, ts, _)
        | E.SpecificCall <@ Operators.(&&&) @> (_, ts, _)
        | E.SpecificCall <@ Operators.(~~~) @> (_, ts, _)
        | E.SpecificCall <@ Operators.(>>>) @> (_, ts, _)
        | E.SpecificCall <@ Operators.(<<<) @> (_, ts, _) -> ts |> pureBy isPrimitiveOrNumeric

        | E.SpecificCall <@ Operators.abs @> (_, ts, _)
        | E.SpecificCall <@ Operators.acos @> (_, ts, _)
        | E.SpecificCall <@ Operators.asin @> (_, ts, _)
        | E.SpecificCall <@ Operators.atan @> (_, ts, _)
        | E.SpecificCall <@ Operators.atan2 @> (_, ts, _)
        | E.SpecificCall <@ Operators.ceil @> (_, ts, _)
        | E.SpecificCall <@ Operators.cos @> (_, ts, _)
        | E.SpecificCall <@ Operators.cosh @> (_, ts, _)
        | E.SpecificCall <@ Operators.exp @> (_, ts, _)
        | E.SpecificCall <@ Operators.floor @> (_, ts, _)
        | E.SpecificCall <@ Operators.log @> (_, ts, _)
        | E.SpecificCall <@ Operators.log10 @> (_, ts, _)
        | E.SpecificCall <@ Operators.pown @> (_, ts, _)
        | E.SpecificCall <@ Operators.round @> (_, ts, _)
        | E.SpecificCall <@ Operators.sign @> (_, ts, _)
        | E.SpecificCall <@ Operators.sin @> (_, ts, _)
        | E.SpecificCall <@ Operators.sinh @> (_, ts, _)
        | E.SpecificCall <@ Operators.sqrt @> (_, ts, _)
        | E.SpecificCall <@ Operators.tan @> (_, ts, _)
        | E.SpecificCall <@ Operators.tanh @> (_, ts, _)
        | E.SpecificCall <@ Operators.truncate @> (_, ts, _) -> ts |> pureBy isPrimitiveOrNumeric

        // Implementation is using boxing, but it is replaced by direct conversion by the compiler
        | E.SpecificCall <@ Operators.enum: _ -> CompilationRepresentationFlags @> _ -> Pure

        | E.SpecificCall <@ Operators.byte @> (_, ts, _)
        | E.SpecificCall <@ Operators.char @> (_, ts, _)
        | E.SpecificCall <@ Operators.float @> (_, ts, _)
        | E.SpecificCall <@ Operators.float32 @> (_, ts, _)
        | E.SpecificCall <@ Operators.int @> (_, ts, _)
        | E.SpecificCall <@ Operators.int16 @> (_, ts, _)
        | E.SpecificCall <@ Operators.int32 @> (_, ts, _)
        | E.SpecificCall <@ Operators.int64 @> (_, ts, _)
        | E.SpecificCall <@ Operators.nativeint @> (_, ts, _)
        | E.SpecificCall <@ Operators.sbyte @> (_, ts, _)
        | E.SpecificCall <@ Operators.uint16 @> (_, ts, _)
        | E.SpecificCall <@ Operators.uint32 @> (_, ts, _)
        | E.SpecificCall <@ Operators.uint64 @> (_, ts, _)
        | E.SpecificCall <@ Operators.unativeint @> (_, ts, _) ->
            // throws (lazy byte 999I)
            ts |> pureBy (fun t -> t.IsPrimitive)

        | E.SpecificCall <@ Operators.max @> (_, ts, _)
        | E.SpecificCall <@ Operators.min @> (_, ts, _)
        | E.SpecificCall <@ Operators.hash @> (_, ts, _)
        | E.SpecificCall <@ Operators.limitedHash @> (_, ts, _)
        | E.SpecificCall <@ Operators.compare @> (_, ts, _) -> ts |> pureBy isBasic

        | E.SpecificCall <@ Operators.sizeof<_> @> _ -> LiteralLike

        // LiteralLike?
        | E.SpecificCall <@ Operators.id @> _
        | E.SpecificCall <@ Operators.ignore @> _

        | E.SpecificCall <@ Operators.not @> _
        | E.SpecificCall <@ Operators.isNull @> _
        | E.SpecificCall <@ Operators.fst @> _
        | E.SpecificCall <@ Operators.snd @> _
        | E.SpecificCall <@ Operators.defaultArg @> _
        | E.SpecificCall <@ Operators.defaultValueArg @> _
        | E.SpecificCall <@ Operators.box @> _
        | E.SpecificCall <@ Operators.tryUnbox @> _
        | E.SpecificCall <@ Operators.typedefof<_> @> _
        | E.SpecificCall <@ Operators.typeof<_> @> _ -> Pure
        | _ -> HasSideEffect

    let extraTopLevelOperatorsCost = function
        | E.SpecificCall <@ ExtraTopLevelOperators.double @> (_, ts, _)
        | E.SpecificCall <@ ExtraTopLevelOperators.single @> (_, ts, _)
        | E.SpecificCall <@ ExtraTopLevelOperators.int8 @> (_, ts, _)
        | E.SpecificCall <@ ExtraTopLevelOperators.uint8 @> (_, ts, _) -> ts |> pureBy (fun t -> t.IsPrimitive)
        | _ -> HasSideEffect

    let nonStructuralComparisonCost = function
        | E.SpecificCall <@ NonStructuralComparison.compare: int -> _ @> (_, ts, _)
        | E.SpecificCall <@ NonStructuralComparison.hash @> (_, ts, _)
        | E.SpecificCall <@ NonStructuralComparison.max: int -> _ @> (_, ts, _)
        | E.SpecificCall <@ NonStructuralComparison.min: int -> _ @> (_, ts, _)

        | E.SpecificCall <@ NonStructuralComparison.(=): int -> _ @> (_, ts, _)
        | E.SpecificCall <@ NonStructuralComparison.(<>): int -> _ @> (_, ts, _)
        | E.SpecificCall <@ NonStructuralComparison.(>): int -> _ @> (_, ts, _)
        | E.SpecificCall <@ NonStructuralComparison.(>=): int -> _ @> (_, ts, _)
        | E.SpecificCall <@ NonStructuralComparison.(<): int -> _ @> (_, ts, _)
        | E.SpecificCall <@ NonStructuralComparison.(<=): int -> _ @> (_, ts, _) -> ts |> pureBy isBasic
        | _ -> HasSideEffect

    let uncheckedCost = function
        | E.SpecificCall <@ Unchecked.defaultof<_> @> (_, [t], _) -> if smallType t then LiteralLike else Pure

        | E.SpecificCall <@ Unchecked.compare @> (_, ts, _)
        | E.SpecificCall <@ Unchecked.equals @> (_, ts, _)
        | E.SpecificCall <@ Unchecked.hash @> (_, ts, _) -> ts |> pureBy isBasic

        | _ -> HasSideEffect

    let maxCostTable = ConditionalWeakTable()
    let mutable createMaxCost = null

    let rec maxCostMemoize e = maxCostTable.GetValue(e, createMaxCost)
    and maxCost = function
        | E.ShapeVar v -> if v.IsMutable then HasSideEffect else LiteralLike
        | E.ShapeLambda _ -> Pure
        | E.Application _ -> HasSideEffect
        | E.Call xs as e -> maxCostCall e xs
        | E.Coerce(e, _) -> maxCostMemoize e |> max LiteralLike
        | E.DefaultValue t -> if smallType t then LiteralLike else Pure
        | E.FieldGet(_, f) -> if f.IsInitOnly || f.IsLiteral then LiteralLike else HasSideEffect

        | E.FieldSet _
        | E.ForIntegerRangeLoop _ -> HasSideEffect

        | E.IfThenElse(e1, e2, e3) -> maxCostMemoize e1 |> maxCost' e2 |> maxCost' e3 |> max Pure
        | E.Let(v, e1, e2) -> if v.IsMutable then HasSideEffect else maxCostMemoize e1 |> maxCost' e2

        | E.LetRecursive(lets, e2) ->
            if lets |> Seq.exists (fun (v, _) -> v.IsMutable) then HasSideEffect else

            lets
            |> Seq.map snd
            |> maxCostExprs
            |> maxCost' e2

        | E.NewArray _ -> HasSideEffect
        | E.NewDelegate _ -> Pure
        | E.NewObject _ -> HasSideEffect
        | E.NewRecord(t, es) ->
            let hasMutableField = T.GetRecordFields t |> Seq.exists (fun p -> p.CanWrite)
            if hasMutableField then HasSideEffect else
            let newCost = if t.IsValueType && smallType t then LiteralLike else Pure

            es
            |> maxCostExprs
            |> max newCost

        | E.NewTuple es as e ->
            let t = e.Type
            let newCost = if t.IsValueType && smallType t then LiteralLike else Pure
            es
            |> maxCostExprs
            |> max newCost

        | E.NewUnionCase(_, []) -> LiteralLike
        | E.NewUnionCase(c, es) ->
            let t = c.DeclaringType
            let newCost = if t.IsValueType && smallType t then LiteralLike else Pure
            es
            |> maxCostExprs
            |> max newCost

        // module ... = let x = ...
        | E.PropertyGet(None, (DeclaringType(ModuleType _) as p), []) when not p.CanWrite -> LiteralLike

        // let { x = x } = point
        // let (Some x) = opt
        | E.PropertyGet(Some e1, (DeclaringType(RecordType _ | UnionType _) as p), []) when not p.CanWrite -> maxCostMemoize e1

        | E.PropertyGet _
        | E.PropertySet _ -> HasSideEffect
        | E.QuoteRaw _
        | E.QuoteTyped _ -> Pure

        | E.Sequential _ -> HasSideEffect
        | E.TryFinally _
        | E.TryWith _ -> HasSideEffect
        | E.TupleGet(e1, _) -> maxCostMemoize e1

        | E.TypeTest(e1, _)
        | E.UnionCaseTest(e1, _) -> maxCostMemoize e1 |> max Pure

        | E.Value _
        | E.ValueWithName _ -> LiteralLike
        | E.Var v -> if v.IsMutable then HasSideEffect else LiteralLike
        | E.VarSet _
        | E.WhileLoop _ -> HasSideEffect
        | E.WithValue(_, _, e) -> maxCostMemoize e

        | _ -> Unknown

    and maxCost' e cost =
        if knownMaxCost <= cost then cost else
        max cost (maxCostMemoize e)

    and maxCostExprs (es: _ seq) =
        use it = es.GetEnumerator()
        let rec aux cost =
            if knownMaxCost <= cost then cost else
            if it.MoveNext() then
                aux (max cost (maxCostMemoize it.Current))
            else
                cost
        aux minCost

    and maxCostCall e = function
        | None, m, args ->
            let argsCost = maxCostExprs args
            if HasSideEffect <= argsCost then argsCost else

            let operationCost =
                match m.DeclaringType.Name with
                | "Operators" -> operatorsCost e
                | "ExtraTopLevelOperators" -> extraTopLevelOperatorsCost e
                | "NonStructuralComparison" -> nonStructuralComparisonCost e
                | "Unchecked" -> uncheckedCost e

                // TODO:
                | _ -> HasSideEffect

            max argsCost operationCost

        | _ -> HasSideEffect

    createMaxCost <- ConditionalWeakTable.CreateValueCallback maxCost
    maxCostMemoize

let varCount =
    let rec vars = function
        | E.ShapeCombination(_, es) -> Seq.collect vars es
        | E.ShapeLambda(x, e) -> Seq.append [x] (vars e)
        | E.ShapeVar x -> Seq.singleton x

    let table = ConditionalWeakTable()
    let varCounts e = vars e |> Seq.countBy id |> Map.ofSeq
    let varCounts = ConditionalWeakTable.CreateValueCallback varCounts
    fun var (e: Expr) ->
        let mutable result = -1
        if table.GetValue(e, varCounts).TryGetValue(var, &result) then
            result
        else
            0

let size e = fold (fun size _ -> size + 1) 0 e

let tryFindRecursiveCall m e =
    let m = genericTypeGenericMethodDefinition m

    let mutable visitedMethods = []
    let rec visitMethod m' e =
        let m' = genericTypeGenericMethodDefinition m'
        if m = m' then Some(m', e) else

        if List.contains m' visitedMethods then None else
        visitedMethods <- m'::visitedMethods

        match m' with
        | E.MethodWithReflectedDefinition body -> aux body
        | _ -> None

    and aux e =
        e
        |> tryPick (function
            | E.Call(_, m', _) as e -> visitMethod m' e
            | E.PropertyGet(_, p, _) as e -> visitMethod p.GetMethod e
            | E.PropertySet(_, p, _, _) as e -> visitMethod p.SetMethod e

            // コンストラクタはインライン展開しない
            //| E.NewObject(c, _) -> 

            | _ -> None
        )

    aux e

let private operatorCompleteNameMap = Map [
    "op_Nil", "([])"
    "op_Cons", "(::)"
    "op_Addition", "(+)"
    "op_Subtraction", "(-)"
    "op_Multiply", "(*)"
    "op_Division", "(/)"
    "op_Append", "(@)"
    "op_Concatenate", "(^)"
    "op_Modulus", "(%)"
    "op_BitwiseAnd", "(&&&)"
    "op_BitwiseOr", "(|||)"
    "op_ExclusiveOr", "(^^^)"
    "op_LeftShift", "(<<<)"
    "op_LogicalNot", "(~~~)"
    "op_RightShift", "(>>>)"
    "op_UnaryPlus", "(~+)"
    "op_UnaryNegation", "(~-)"
    "op_Equality", "(=)"
    "op_LessThanOrEqual", "(<=)"
    "op_GreaterThanOrEqual", "(>=)"
    "op_LessThan", "(<)"
    "op_GreaterThan", "(>)"
    "op_Dynamic", "(?)"
    "op_DynamicAssignment", "(?<-)"
    "op_PipeRight", "(|>)"
    "op_PipeLeft", "(<|)"
    "op_Dereference", "(!)"
    "op_ComposeRight", "(>>)"
    "op_ComposeLeft", "(<<)"
    "op_Quotation", "(<@ @>)"
    "op_QuotationUntyped", "(<@@ @@>)"
    "op_AdditionAssignment", "(+=)"
    "op_SubtractionAssignment", "(-=)"
    "op_MultiplyAssignment", "(*=)"
    "op_DivisionAssignment", "(/=)"
    "op_Range", "(..)"
    "op_RangeStep", "(.. ..)"
]
let private operatorPartialNameMap = Map [
    "Greater", ">"
    "Less", "<"
    "Plus", "+"
    "Minus", "-"
    "Multiply", "*"
    "Divide", "/"
    "Equals", "="
    "Twiddle", "~"
    "Percent", "%"
    "Dot", "."
    "Amp", "&"
    "Bar", "|"
    "At", "@"
    "Hat", "^"
    "Bang", "!"
    "Qmark", "?"
    "LParen", "("
    "Comma", ","
    "RParen", ")"
    "LBrack", "["
    "RBrack", "]"
]
let private operatorOtherNameRegex =
    operatorPartialNameMap
    |> Seq.map (fun kv -> kv.Key)
    |> String.concat "|"
    |> sprintf "^op_(%s)*$"
    |> Regex

let parseOperatorName = function
    | "op_" -> ValueSome ""
    | n when n.StartsWith "op_" ->
        match Map.tryFind n operatorCompleteNameMap with
        | ValueSome _ as op -> op
        | ValueNone ->
            let m = operatorOtherNameRegex.Match n
            if m.Success then
                seq {
                    "("
                    for c in m.Groups.[1].Captures do
                        Map.find c.Value operatorPartialNameMap
                    ")"
                }
                |> System.String.Concat
                |> ValueSome
            else
                ValueNone
    | _ -> ValueNone
