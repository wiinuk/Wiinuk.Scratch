[<AutoOpen>]
module Scratch.Reflection.Transformers.PredefinedTransformers
open FSharp.Quotations
open Scratch.Reflection
open Scratch.Reflection.Expr
open Scratch.Reflection.Transformers.PredefinedTransformer.Helpers
open Scratch.Reflection.Transformers.Transformer
open System.Reflection
module E = FSharp.Quotations.Patterns
module E = FSharp.Quotations.DerivedPatterns
type private E = FSharp.Quotations.Expr


let transformPipeLeft = function
    // <@ m $v1 ... <| $v @> =
    // <@ (let $x1 = $v1 in ... in (fun x -> m $x1 ... x)) <| $v @> ->
    // <@ m $v1 ... $v @>
    | E.SpecificCall <@@ (<|) @@> (_, _, [Lets(lets, E.Lambda(lastVar, (CallOrNewUnionCaseOrNewObject(m, args) as mcall))); last]) ->
        pipeToCall (getLocation mcall) lets m args lastVar last

    // <@ f $v1 ... <| $v @> ->
    // <@ f $v1 ... $v @>
    | E.SpecificCall <@@ (<|) @@> (_, _, [SimpleApplicationsWithLocation(lambda, args); last]) as call ->
        simpleApplicationsWithLocation lambda (args @ [last, getLocation call]) |> Some

    | _ -> None

let transformPipeRight = function

    // <@ $xn |> m $v1 ... @> =
    // <@ $xn |> (let $x1 = $v1 in ... in (fun xn -> m $x1 ... xn)) @> ->
    // <@ m $v1 ... $xn @>
    | E.SpecificCall <@@ (|>) @@> (_, _, [last; Lets(lets, E.Lambda(lastVar, (CallOrNewUnionCaseOrNewObject(m, args) as mcall)))]) as call ->
        match maxCost last with

        // <@ 20 |> add 10 @> ->
        // <@ add 10 20 @>
        | LiteralLike
        | Pure -> pipeToCall (getLocation mcall) lets m args lastVar last

        // <@ (printfn "20"; 20) |> add (printfn "10"; 10) @> ->
        // <@
        //     let pipeRightLast = (printfn "20"; 20)
        //     add (printfn "10"; 10) pipeRightLast
        // @>
        | HasSideEffect
        | Unknown ->
            let v = Var("pipeRightLast", last.Type)
            let tempVar =
                E.Var v
                |> withLocation (getLocation last)

            pipeToCall (getLocation mcall) lets m args lastVar tempVar
            |> Option.map (fun callm ->
                E.Let(v, last, callm)
                |> withLocation (getLocation call)
            )

    // <@ $xn |> f $v1 ... @> ->
    // <@ f $v1 ... $xn @>
    | E.SpecificCall <@@ (|>) @@> (_, _, [last; SimpleApplicationsWithLocation(lambda, args)]) as call ->
        match maxCost last with
        | LiteralLike
        | Pure -> simpleApplicationsWithLocation lambda (args @ [last, getLocation call]) |> Some

        | HasSideEffect
        | Unknown ->
            let v = Var("pipeRightLast", last.Type)
            let tempVar =
                E.Var v
                |> withLocation (getLocation last)

            let apply = simpleApplicationsWithLocation lambda (args @ [tempVar, getLocation call])
            E.Let(v, last, apply)
            |> withLocation (getLocation call)
            |> Some

    | _ -> None

let transformSingleLetVar = function
    | E.Let(x, e1, E.Var x') when x = x' -> Some e1
    | _ -> None

let transformConstantPropagation = function
    | E.Let(v, e1, e2) when not v.IsMutable && maxCost e1 <= LiteralLike ->
        e2.Substitute(fun v' -> if v = v' then Some e1 else None) |> Some

    | _ -> None

let transformLambdaApply = function
    | E.Lambda(x, E.Application(E.Lambda(v, body), (E.Var x' as xe))) as e when x = x' ->

        // TODO: hold location
        E.Lambda(x, body.Substitute(fun v' -> if v = v' then Some xe else None))

        |> withLocation (getLocation e)
        |> Some
    | _ -> None

let transformUnionCasePropertyGetCorece = function
    | E.IfThenElse(E.UnionCaseTest(E.Var var as varE, case) as testE, ifTrue, ifFalse) ->
        if var.IsMutable || not case.DeclaringType.IsClass then None else

        let fields = case.GetFields()
        if fields.Length = 0 then None else

        let caseType = fields.[0].DeclaringType
        if caseType.BaseType <> case.DeclaringType then None else

        let caseField =
            ifTrue
            |> tryPick (function
                | E.PropertyGet(Some(E.Var var'), p, []) when var = var' && p.DeclaringType = caseType -> Some()
                | _ -> None
            )

        if caseField |> Option.isNone then None else

        let case = Var("_case", caseType)
        let caseE = E.Var case

        let ifTrue =
            ifTrue
            |> transformDeepOnce (function
                | E.PropertyGet(Some(E.Var var'), p, []) when var = var' && p.DeclaringType = caseType -> Some(E.PropertyGet(caseE, p, []))
                | _ -> None
            )

        match ifTrue with
        | None -> None
        | Some ifTrue ->

        Some <| E.IfThenElse(testE, E.Let(case, E.Coerce(varE, caseType), ifTrue), ifFalse)

    | _ -> None

let transformUnusedLet = function
    | E.Let(x, e1, body) when varCount x body = 0 && maxCost e1 <= Pure ->
        // <@ let x = 10 + y in z @> => <@ z @>

        Some body

    | _ -> None

let transformPurePropagation = function
    | E.Let(x, e1, body) as e when varCount x body <= 1 && maxCost e1 <= Pure ->
        // <@ let x = 10 + y in x - z @> => <@ let x = 10 + y in 10 + y - z @>

        E.Let(x, e1,

            // TODO: location
            body.Substitute(fun v -> if x = v then Some e1 else None)

            |> withLocation (getLocation body)
        )
        |> withLocation (getLocation e)
        |> Some

    | _ -> None

let transformInlining = function
    | UniversalCall(this, m, args) ->
        let defaultInliningSize = 0
        let flags = m.MethodImplementationFlags
        if flags.HasFlag MethodImplAttributes.NoInlining then None
        elif flags.HasFlag MethodImplAttributes.AggressiveInlining then tryInlining ValueNone this m args
        else tryInlining (ValueSome defaultInliningSize) this m args

    | _ -> None

let transformers = [
    transformPipeLeft
    transformPipeRight
    transformSingleLetVar
    transformConstantPropagation
    transformPurePropagation
    transformLambdaApply
    transformUnusedLet
    transformUnionCasePropertyGetCorece
    transformInlining
]
