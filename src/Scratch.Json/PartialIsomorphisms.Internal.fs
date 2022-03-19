[<AutoOpen>]
module internal Scratch.Json.PartialIsomorphisms.Internal
open System
open System.Reflection
open System.Linq.Expressions
open FSharp.Quotations
open FSharp.Reflection
open Scratch.Primitives
open Scratch.Reflection
open Scratch.Reflection.Expr
open Scratch.Reflection.Transformers
open Scratch.Json.PartialIsomorphisms.Iso
type T = Reflection.FSharpType
type E = Quotations.Expr
module E = Quotations.Patterns
module E = Quotations.ExprShape
module E = Quotations.DerivedPatterns
module X = TypedExpr
module U = TypedUnionCaseInfo
module C = TypedUnionCaseInfo
module T = TypedExpression

[<AutoOpen>]
module Internal =
    type E = System.Linq.Expressions.Expression
    type T = Scratch.Reflection.TypedExpression


type ExprIso<'T1,'T2> = ExprIso of Expr<'T1 -> Result<'T2, IsoError>> * Expr<'T2 -> Result<'T1, IsoError>>
type IsoRExpression<'T1,'T2> = IsoRExpression of Expression<FuncIOV<'T1,'T2, bool>> * Expression<FuncIOV<'T2,'T1, bool>>

let private transformers = [
    transformLambdaApply
    transformConstantPropagation
    transformPurePropagation
    transformUnusedLet
]
let compileToIso (ExprIso(e1, e2)) =
    let transformDeepOnceTyped t (e: 'T Expr) =
        transformDeepOnce t e
        |> Option.defaultValue (upcast e)
        |> E.Cast<'T>

    let transformTyped ts (e: 'T Expr) =
        transform 100 ts e
        |> E.Cast<'T>

    let compile e =
        let e =
            transformDeepOnceTyped transformUnionCasePropertyGetCorece e
            |> transformTyped transformers
            |> toLinqLambdaExpression
        e.Compile()

    { OfFunc.f1 = compile e1; f2 = compile e2 }


let isoFromUnionError<'U,'T1> caseName (x: 'U): Result<'T1, IsoError> = Error <| IsoError.ofMessage $"isoFromUnion({caseName}) %A{x}"

let expressionFromUnion0 e =
    let case = U.create0 e
    let forward =
        let x = TypedVar<_> "tuple"
        X.Lambda(x, <@ Ok %(X.NewUnionCase(case, HUnit)) @>)

    let reverse =
        let u = TypedVar<_> "union"
        let union = X.Var u
        let test = X.UnionCaseTest(union, case)
        let caseName = X.Value(U.name case)
        X.Lambda(u,
            <@
            if %test then Ok HUnit
            else isoFromUnionError %caseName %union
            @>
        )
    ExprIso(forward, reverse)

let expressionFromUnion1 e =
    let case = U.create1 e
    let fields = U.fields case

    let forward =
        let x = TypedVar<_> "tuple"
        X.Lambda(x, <@ Ok %(X.NewUnionCase(case, <@ (%(X.Var x)).head @> ^^ HUnit)) @>)

    let reverse =
        let u = TypedVar<_> "union"
        let union = X.Var u
        let test = X.UnionCaseTest(union, case)
        let fieldGet = X.InstancePropertyGet(union, fields.head)
        let caseName = X.Value(U.name case)
        X.Lambda(u, <@
            if %test then Ok { head = %fieldGet; tail = HUnit }
            else isoFromUnionError %caseName %union
        @>)

    ExprIso(forward, reverse)

let expressionFromUnion2 e =
    let case = U.create2 e
    let fields = U.fields case

    let forward =
        let x = TypedVar<_> "tuple"
        X.Lambda(x, <@ Ok %(X.NewUnionCase(case, <@ (%(X.Var x)).head @> ^^ <@ (%(X.Var x)).tail.head @> ^^ HUnit)) @>)

    let reverse =
        let u = TypedVar<_> "union"
        let union = X.Var u
        let test = X.UnionCaseTest(union, case)
        let fieldGet1 = X.InstancePropertyGet(union, fields.head)
        let fieldGet2 = X.InstancePropertyGet(union, fields.tail.head)
        let caseName = X.Value(U.name case)
        X.Lambda(u, <@
            if %test then Ok { head = %fieldGet1; tail = { head = %fieldGet2; tail = HUnit } }
            else isoFromUnionError %caseName %union
        @>)

    ExprIso(forward, reverse)

let expressionFromUnion3 e =
    let case = U.create3 e
    let fields = U.fields case

    let forward =
        let x = TypedVar<_> "tuple"
        let tuple = X.Var x
        let es = <@ (%tuple).head @> ^^ <@ (%tuple).tail.head @> ^^ <@ (%tuple).tail.tail.head @> ^^ HUnit
        X.Lambda(x, <@ Ok %(X.NewUnionCase(case, es)) @>)

    let reverse =
        let u = TypedVar<_> "union"
        let union = X.Var u
        let test = X.UnionCaseTest(union, case)
        let fieldGet1 = X.InstancePropertyGet(union, fields.head)
        let fieldGet2 = X.InstancePropertyGet(union, fields.tail.head)
        let fieldGet3 = X.InstancePropertyGet(union, fields.tail.tail.head)
        let caseName = X.Value(U.name case)
        X.Lambda(u, <@
            if %test then Ok (%fieldGet1 ^^ %fieldGet2 ^^ %fieldGet3 ^^ HUnit)
            else isoFromUnionError %caseName %union
        @>)

    ExprIso(forward, reverse)

let expressionFromUnion4 e =
    let case = U.create4 e
    let fields = U.fields case

    let forward =
        let x = TypedVar<_> "tuple"
        let tuple = X.Var x
        let es =
            <@ (%tuple).head @> ^^
            <@ (%tuple).tail.head @> ^^
            <@ (%tuple).tail.tail.head @> ^^
            <@ (%tuple).tail.tail.tail.head @> ^^
            HUnit
        X.Lambda(x, <@ Ok %(X.NewUnionCase(case, es)) @>)

    let reverse =
        let u = TypedVar<_> "union"
        let union = X.Var u
        let test = X.UnionCaseTest(union, case)
        let fieldGet1 = X.InstancePropertyGet(union, fields.head)
        let fieldGet2 = X.InstancePropertyGet(union, fields.tail.head)
        let fieldGet3 = X.InstancePropertyGet(union, fields.tail.tail.head)
        let fieldGet4 = X.InstancePropertyGet(union, fields.tail.tail.tail.head)
        let caseName = X.Value(U.name case)
        X.Lambda(u, <@
            if %test then Ok (%fieldGet1 ^^ %fieldGet2 ^^ %fieldGet3 ^^ %fieldGet4 ^^ HUnit)
            else isoFromUnionError %caseName %union
        @>)
    ExprIso(forward, reverse)

let expressionFromUnion5 e =
    let case = U.create5 e
    let fields = U.fields case

    let forward =
        let x = TypedVar<_> "tuple"
        let tuple = X.Var x
        let es =
            <@ (%tuple).head @> ^^
            <@ (%tuple).tail.head @> ^^
            <@ (%tuple).tail.tail.head @> ^^
            <@ (%tuple).tail.tail.tail.head @> ^^
            <@ (%tuple).tail.tail.tail.tail.head @> ^^
            HUnit
        X.Lambda(x, <@ Ok %(X.NewUnionCase(case, es)) @>)

    let reverse =
        let u = TypedVar<_> "union"
        let union = X.Var u
        let test = X.UnionCaseTest(union, case)
        let fieldGet1 = X.InstancePropertyGet(union, fields.head)
        let fieldGet2 = X.InstancePropertyGet(union, fields.tail.head)
        let fieldGet3 = X.InstancePropertyGet(union, fields.tail.tail.head)
        let fieldGet4 = X.InstancePropertyGet(union, fields.tail.tail.tail.head)
        let fieldGet5 = X.InstancePropertyGet(union, fields.tail.tail.tail.tail.head)
        let caseName = X.Value(U.name case)
        X.Lambda(u, <@
            if %test then Ok (%fieldGet1 ^^ %fieldGet2 ^^ %fieldGet3 ^^ %fieldGet4 ^^ %fieldGet5 ^^ HUnit)
            else isoFromUnionError %caseName %union
        @>)
    ExprIso(forward, reverse)

let expressionFromRecord (recordToHList: Expr<'R -> 'T> when 'T :> HList): ExprIso<'T,'R> =
    let error() = failwithf "e.g. <@ fun r -> r.x^^x.y^^HUnit @>"
    if not <| T.IsRecord(typeof<'R>, allowAccessToPrivateRepresentation = true) then error() else

    match recordToHList with

    // <@ fun r -> %makeTuple @>
    | E.Lambda(recordV, makeTuple) ->
        let (|ConsOpCall|_|) = E.(|SpecificCall|_|) <@ (^^) @>
        let recordV = X.CastVar<'R> recordV
        let makeTuple = E.Cast<'T> makeTuple

        let rec checkMakeTuple fields = function

            // <@ HUnit @>
            | E.NewUnionCase(case, _) when case.DeclaringType = typeof<HUnit> ->
                match fields with
                | [] -> ()
                | fields -> failwith $"unused fields: %A{fields}"

            // <@ r.f::%rest @>
            | ConsOpCall(_, _, [E.PropertyGet(Some(E.Var r), p, _); rest]) when r = recordV.Raw ->
                match fields with
                | (f: PropertyInfo)::fields when p.Name = f.Name -> checkMakeTuple fields rest
                | _ -> failwith $"invalid field order. expected: %A{fields}"

            | _ -> error()

        let fields = T.GetRecordFields(typeof<'R>, allowAccessToPrivateRepresentation = true) |> Array.toList
        checkMakeTuple fields makeTuple

        // <@ fun (t: 'T) ->
        //     { f1 = t.head }
        // @>
        // <@ fun (t: 'T) ->
        //     let t2 = t.tail
        //     let t3 = t2.tail
        //     let t4 = t3.tail
        //     { f1 = t.head; f2 = t2.head; f3 = t3.head; f4 = t4.head }
        // @>
        let t = TypedVar<'T>("tuple")
        let tuple = X.Var t

        let makeNewRecord vars =
            let getHeads = vars |> List.map (fun (v: Expr) -> E.PropertyGet(v, v.Type.GetProperty "head"))
            E.NewRecord(typeof<'R>, getHeads) |> E.Cast<'R>

        let rec makeTailLetsAndNewRecord n (listVar, listVarsRev) (listType: Type) =
            let tailP = listType.GetProperty "tail"
            let tailT = tailP.PropertyType
            if tailT = typeof<HUnit> then
                let newRecord = List.rev (listVar::listVarsRev) |> makeNewRecord
                <@ (Ok %newRecord): Result<'R, IsoError> @>
            else
                let tN = Var($"t{n}", tailT)
                E.Let(tN, E.PropertyGet(listVar, tailP), makeTailLetsAndNewRecord (n + 1) (E.Var tN, listVar::listVarsRev) tailT) |> E.Cast<Result<'R, IsoError>>

        let body = makeTailLetsAndNewRecord 2 (tuple, []) typeof<'T>
        let forward = X.Lambda(t, body)
        let reverse = X.Lambda(recordV, <@ Ok %makeTuple @>)
        ExprIso(forward, reverse)

    | _ -> error()

let tagMember union (case: #ITypedUnionCaseInfo<'Union>) =
    let case = C.raw case
    let union = T.rawOf the<'Union> union
    match FSharpValue.PreComputeUnionTagMemberInfo(case.DeclaringType, allowAccessToPrivateRepresentation = true) with

    // <@ union.Tag @>
    | :? PropertyInfo as p -> T.fromRawUnchecked<int>(E.Property(union, p))

    // <@ UnionType.GetTag(union) @>
    | :? MethodInfo as m -> T.fromRawUnchecked<int>(E.Call(m, union))

    | m -> failwith $"unknown UnionTagMember %A{m}"

let tryGetCaseSubClass case =
    let unionType = C.declaringType case
    match C.raw(case).GetFields() with
    | null
    | [||] -> None
    | fs ->

    let caseType = fs[0].DeclaringType
    if unionType.IsClass && caseType.BaseType = unionType then
        Some caseType
    else
        None

let memberName (p: #MemberInfo) = p.Name

/// 'Tuple = ('T1 * 'T2 * ... * 'TN);
/// 'VList = [# 'T1 * 'T2 * ... * 'TN #]
let isoExpressionFromUnionNCore case: IsoRExpression<'VList,'Union> when 'VList :> HList =
    let fs = C.raw(case).GetFields()

    let makeCaseM = FSharpValue.PreComputeUnionConstructorInfo(C.raw case, allowAccessToPrivateRepresentation = true)

    let vheadName = Member.findField <@ fun x -> x.vhead @> |> memberName
    let vtailName = Member.findField <@ fun x -> x.vtail @> |> memberName
    let forward =
        // new FuncIOV(fun (value: 'VList byref) (result: 'Union byref): bool ->
        //     result <-
        //         %makeCase(
        //             value.vhead,
        //             value.vtail.vhead,
        //             ...
        //         )
        //     true
        // )
        let value = T.Parameter<'VList>("value", isByRef = true)
        let result = T.Parameter<'Union>("result", isByRef = true)

        let rec readValues home = function
            | 0 -> []
            | count ->

            // %home.vhead
            let read = E.Field(home, vheadName) :> E

            let home = E.Field(home, vtailName) :> E
            read::readValues home (count - 1)

        let makeCase = T.FromRaw<'Union>(E.Call(makeCaseM, readValues (T.Raw value) fs.Length))

        let body =
            T.Sequential(
                T.Assign(result, makeCase),
                T.Constant true
            )
        T.Lambda(value^^result^^HUnit, body)

    let reverse =
        // FuncIOV(fun (value: 'Union byref) (result: 'VList byref): bool ->
        //     if %getTag value = %tag then
        //         result.vhead <- value.field1
        //         result.vtail.head <- value.field2
        //         ...
        //         true
        //     else
        //         false
        // )
        let value = T.Parameter<'Union>("value", isByRef = true)
        let result = T.Parameter<'VList>("result", isByRef = true)

        let assigns value =
            let value = T.rawOf the<'Union> value
            let rec assigns home = function
                | [] -> []
                | (field: PropertyInfo)::fs ->

                // %home.vhead <- %value.%field
                let assign = T.FromRaw<unit>(E.Assign(E.Field(home, vheadName), E.Property(value, field)))

                let home = E.Field(home, vtailName) :> E
                assign::assigns home fs

            fs
            |> Array.toList
            |> assigns (T.Raw result)
            |> T.Sequential

        let castAndAssigns =
            match tryGetCaseSubClass case with
            | None -> assigns value
            | Some caseType ->

            let case = T.FromRaw<'Union>(E.Parameter(caseType, "case"))
            T.Block(
                [case],
                [
                    T.Assign(case, T.FromRaw<_>(E.TypeAs(T.Raw value, caseType)))
                    assigns case
                ]
            )

        let body =
            T.Condition(
                T.Equal(tagMember value case, T.Constant(C.tag case)),
                T.Sequential(
                    castAndAssigns,
                    T.Constant true
                ),
                T.Constant false
            )
        T.Lambda(value^^result^^HUnit, body)

    IsoRExpression(forward, reverse)

let isoExpressionFromUnionN (e: Expr<'Tuple -> 'Union>) =
    isoExpressionFromUnionNCore (C.createAny e)

let compileToIsoR (IsoRExpression(forward, reverse)) =
    { OfFuncIOV.forward = forward.Compile(); reverse = reverse.Compile() }

let verifyFieldMultiSetEq actual expected =
    let expecteds = expected |> Set.ofSeq

    let duplicateds, unuseds =
        actual
        |> Seq.fold (fun (duplicateds, expected) actual ->
            if Set.contains actual expected then
                duplicateds, Set.remove actual expected
            else
                Set.add actual duplicateds, expected
        ) (Set.empty, expecteds)

    match not (Set.isEmpty duplicateds), not (Set.isEmpty unuseds) with
    | (true as hasDup), hasUnused
    | hasDup, (true as hasUnused) ->
        let unuseds = if hasUnused then [$"""unused fields: {String.concat ", " unuseds}"""] else []
        let duplicateds = if hasDup then [$"""duplicated fields: {String.concat ", " duplicateds}"""] else []
        failwith (String.concat ", " (unuseds @ duplicateds))

    | _ -> ()

let veritySeqEq actual expected =
    let actualTypes = actual |> Seq.map (fun (t: Type) -> t.FullName)
    let expectedTypes = expected |> Seq.map (fun (t: Type) -> t.FullName)
    if Seq.compareWith compare actualTypes expectedTypes = 0 then () else

    expected
    |> Seq.map (fun t -> t.FullName)
    |> String.concat "; "
    |> failwithf "invalid field order. expected type: ( %s )"

let rec mvlistTypes (GenericTypeDefinition td as t) =
    if t = typeof<hunit> then []
    elif td = typedefof<mvcons<_,_>> then
        let ts = t.GetGenericArguments()
        ts[0]::mvlistTypes ts[1]
    else [t]

let fields e =
    e
    |> Expr.fold (fun acc e ->
        match e with
        | E.PropertyGet(_, p, _)
        | E.PropertySet(_, p, _, _) -> p::acc
        | _ -> acc
    ) []
    |> List.rev

/// 'HList = #['T1 * 'T2 * ... * 'TN]
let isoExpressionFromRecord (e: Expr<'Record -> 'VList> when 'VList :> MVList): IsoRExpression<'VList,'Record> =
    let fieldsInDefinitionOrder = FSharpType.GetRecordFields(typeof<'Record>, allowAccessToPrivateRepresentation = true)
    let fieldsInExprOrder =
        fields e
        |> Seq.choose (fun p ->
            match Array.tryFindIndex ((=) p) fieldsInDefinitionOrder with
            | Some definitionIndex -> Some(p, definitionIndex)
            | _ -> None
        )
        |> Seq.cache

    let vlistItemTypes = mvlistTypes typeof<'VList>

    verifyFieldMultiSetEq
        (fieldsInExprOrder |> Seq.map (fst >> memberName))
        (fieldsInDefinitionOrder |> Seq.map memberName)

    veritySeqEq
        (fieldsInExprOrder |> Seq.map (fun (p, _) -> p.PropertyType))
        vlistItemTypes

    let vheadName = Member.findField <@ fun t -> t.vhead @> |> memberName
    let vtailName = Member.findField <@ fun t -> t.vtail @> |> memberName
    let newRecord = FSharpValue.PreComputeRecordConstructorInfo(typeof<'Record>, allowAccessToPrivateRepresentation = true)
    let forward =
        // new FuncIOV(fun (value: 'VList inref) (result: 'Record outref): bool ->
        //     result <- new 'Record(
        //         value.vhead,
        //         value.vtail.vhead,
        //         ...
        //     )
        //     true
        // )

        // // e.g.
        // type Record1 = { f1: int; f2: string; f3: string }
        // e = <@ fun r -> r.f1^^r.f3^^r.f2^^HUnit @>
        // new FuncIOV(fun (value: mvcons<int, mvcons<string, mvcons<string, hunit>>> inref) (result: Record1 outref): bool ->
        //     result <- new Record1(
        //         f1 = value.vhead,
        //         f2 = value.vtail.vtail.vhead,
        //         f3 = value.vtail.vhead
        //     )
        //     true
        // )
        let value = T.Parameter<'VList>("value", isByRef = true)
        let result = T.Parameter<'Record>("result", isByRef = true)

        let rec readFields home = function
            | [] -> []
            | definitionIndex::fieldsInExprOrder ->
                let read = E.Field(home, vheadName) :> E
                let home = E.Field(home, vtailName) :> E
                (read, definitionIndex)::readFields home fieldsInExprOrder

        let readFields = fieldsInExprOrder |> Seq.map snd |> Seq.toList |> readFields (T.Raw value)
        let readFields = readFields |> Seq.sortBy snd |> Seq.map fst

        let body =
            T.Sequential(
                T.Assign(result, T.FromRaw<'Record>(E.New(newRecord, readFields))),
                T.Constant true
            )
        T.Lambda(value^^result^^HUnit, body)

    let reverse =
        // new FuncIOV(fun (value: 'Record inref) (result: 'VList outref): bool ->
        //     result.vhead <- value.fieldX
        //     result.vtail.vhead <- value.fieldY
        //     ...
        //     true
        // )

        // // e.g.
        // type Record1 = { f1: int; f2: string; f3: string }
        // e = <@ fun r -> r.f1^^r.f3^^r.f2^^HUnit @>
        // new FuncIOV(fun (value: Record1 inref) (result: mvcons<int, mvcons<string, mvcons<string, hunit>>> outref): bool ->
        //     result.vhead <- value.f1
        //     result.vtail.vhead <- value.f3
        //     result.vtail.vtail.vhead <- value.f2
        //     true
        // )

        let value = T.Parameter<'Record>("value", isByRef = true)
        let result = T.Parameter<'VList>("result", isByRef = true)
        let rec assigns home = function
            | [] -> []
            | (f: PropertyInfo)::fieldsInExprOrder ->
                let assign = T.FromRaw<unit>(E.Assign(E.Field(home, vheadName), E.Property(T.Raw value, f)) :> E)
                let home = E.Field(home, vtailName) :> E
                assign::assigns home fieldsInExprOrder

        let assigns =
            fieldsInExprOrder
            |> Seq.map fst
            |> Seq.toList
            |> assigns (T.Raw result)
            |> T.Sequential

        let body = T.Sequential(assigns, T.Constant true)
        T.Lambda(value^^result^^HUnit, body)

    IsoRExpression(forward, reverse)
