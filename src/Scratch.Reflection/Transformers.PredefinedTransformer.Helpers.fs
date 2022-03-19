module internal Scratch.Reflection.Transformers.PredefinedTransformer.Helpers
open FSharp.Quotations
open Scratch.Reflection
open Scratch.Reflection.Expr
open Scratch.Reflection.Transformers.Transformer
open System.Reflection
module E = FSharp.Quotations.Patterns
module E = FSharp.Quotations.DerivedPatterns
module VOption = ValueOption
type private E = FSharp.Quotations.Expr


let pipeToCall location lets m args lastVar last =
    let rec takeLets lets args lastVar last =
        match lets, args with

        // <@ @>, <@ $x ... @>
        | [], [E.Var lastVar'] -> if lastVar = lastVar' then Some [last] else None

        // <@ ... @>, <@ @>
        | _, [] -> None

        // <@ let $x = $v in ... @>, <@ $x ... @>
        | (letVar, letValue)::lets, E.Var argVar::args when letVar = argVar ->
            takeLets lets args lastVar last
            |> Option.map (fun args -> letValue::args)

        // <@ ... @>, <@ $x ... @>
        | lets, arg::args ->
            takeLets lets args lastVar last
            |> Option.map (fun args -> arg::args)

    takeLets lets args lastVar last
    |> Option.map (fun args ->
        match m with
        | Choice1Of3 m -> E.Call(m, args)
        | Choice2Of3 u -> E.NewUnionCase(u, args)
        | Choice3Of3 c -> E.NewObject(c, args)
        |> withLocation location
    )

let rec (|SimpleApplicationsWithLocation|) = function
    | E.Application(SimpleApplicationsWithLocation(f, xs), x) as e -> f, xs @ [x, getLocation e]
    | e -> e, []

let rec simpleApplicationsWithLocation f = function
    | [] -> f
    | (x, l)::xs -> simpleApplicationsWithLocation (E.Application(f, x) |> withLocation l) xs

[<return: Struct>]
let (|CallOrNewUnionCaseOrNewObject|_|) = function
    | E.Call(None, m, args) -> ValueSome struct(Choice1Of3 m, args)
    | E.NewUnionCase(u, args) -> ValueSome(Choice2Of3 u, args)
    | E.NewObject(c, args) -> ValueSome(Choice3Of3 c, args)
    | _ -> ValueNone

let has m e = tryFindRecursiveCall m e |> Option.isSome

let inlining this args vvs body lambda =
    let args = match this with None -> args | Some x -> x::args
    let vars = List.concat vvs

    let rec aux = function
        | [], [] -> body
        | var::vars, arg::args ->
            let body = aux (vars, args)
            E.Let(var, arg, body) |> withLocation (getLocation arg)

        // inlining <@ (fun a b -> a + b) (p 10) @> => <@ (fun b -> let a = p 10 in a + b) @>
        | var::vars, ([] as args) ->
            let body = aux (vars, args)
            E.Lambda(var, body) |> withLocation (getLocation lambda)

        // inlining <@ (fun x -> f x) (p 10) (p 11) @> => <@ let x = p 10 in f x (p 11) @>
        | ([] as vars), arg::args ->
            let body = aux (vars, args)
            E.Application(body, arg)

    aux (vars, args)

let tryInlining maxSize this m args =
    match m with
    | E.MethodWithReflectedDefinition(Lambdas(vvs, body) as lambda) ->
        match maxSize with
        | ValueSome maxSize when size body <= maxSize && not (has m body) -> inlining this args vvs body lambda |> Some
        | ValueNone when not (has m body) -> inlining this args vvs body lambda |> Some
        | _ -> None
    | _ -> None
