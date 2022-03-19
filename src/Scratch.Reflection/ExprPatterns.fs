[<AutoOpen>]
module Scratch.Reflection.ExprPatterns
open FSharp.Quotations
open FSharp.Reflection
open Scratch.Primitives
open Scratch.Reflection.Expr
module E = FSharp.Quotations.Patterns
module E = FSharp.Quotations.DerivedPatterns
type private T = FSharp.Reflection.FSharpType


let (|ExprType|) (e: #Expr) = e.Type

let private qOneOrMoreRLinear q inp =
    let rec queryAcc rvs e = 
        match q e with 
        | ValueSome struct(v, body) -> queryAcc (v::rvs) body 
        | _ -> 

        match rvs with
        | [] -> ValueNone
        | _ -> ValueSome struct(List.rev rvs, e) 

    queryAcc [] inp

let private (|TupledLambda|) lam =
    let rec stripSuccessiveProjLets p n expr =
        match expr with 
        | E.Let(v1, E.TupleGet(E.Var pA, m), rest)
            when p = pA && m = n ->
            let struct(restvs, b) = stripSuccessiveProjLets p (n + 1) rest
            v1::restvs, b

        | _ -> [], expr

    match lam with 
    | E.Lambda(v, body) ->
        if v.Type = typeof<unit> || v.Type.IsValueType then ValueSome struct([v], body) else

        match stripSuccessiveProjLets v 0 body with 
        | [], b -> ValueSome([v], b)
        | letvs, b -> ValueSome(letvs, b)

    | _ -> ValueNone

[<return: Struct>]
let (|Lambdas|_|) (input: Expr) = qOneOrMoreRLinear (|TupledLambda|) input

let private qOneOrMoreLLinear q inp =
    let rec queryAcc e rvs =
        match q e with
        | ValueSome struct(body, v) -> queryAcc body (v::rvs) 
        | _ -> 
            match rvs with
            | [] -> ValueNone
            | _ -> ValueSome struct(e, rvs)
    queryAcc inp []

let private (|TupledApplication|) e =
    match e with
    | E.Application(f, x) ->
        match x with
        | E.NewTuple xs when not x.Type.IsValueType -> ValueSome struct(f, xs)
        | x -> ValueSome struct(f, [x])
    | _ -> ValueNone

[<return: Struct>]
let (|Applications|_|) (input: Expr) = qOneOrMoreLLinear (|TupledApplication|) input

[<return: Struct>]
let rec private (|TupleChainRev|_|): _ -> _ voption = function
    | E.TupleGet(tuple, i) -> ValueSome struct(tuple, i, [])
    | E.Let(v, TupleChainRev(tuple, i, is), E.TupleGet(E.Var v', n')) when v = v' -> ValueSome(tuple, n', i::is)
    | _ -> ValueNone

[<return: Struct>]
let (|TupleChain|_|) = function
    | TupleChainRev(tuple, i, is) -> ValueSome struct(tuple, List.rev(i::is))
    | _ -> ValueNone

[<return: Struct>]
let rec (|Sequentials|_|): _ -> struct(_ * _) voption = function
    | E.Sequential(e1, Sequentials(es, e)) as seq -> ValueSome((e1, getLocation seq)::es, e)
    | e -> ValueSome([], e)

/// `<@ let $x1 = $v1 in let $x2 = $v2 in ... in $e @>`
let rec (|Lets|) = function
    | E.Let(x, v, Lets(xvs, e)) -> (x, v)::xvs, e
    | e -> [], e

/// `[$xs...; $x]`
[<return: Struct>]
let rec (|HeadAndLast|_|): _ -> _ voption = function
    | [x] -> ValueSome struct([], x)
    | x::HeadAndLast(xs, last) -> ValueSome struct(x::xs, last)
    | _ -> ValueNone

[<return: Struct>]
let (|LetRecursiveLambdas|_|) = function
    | E.LetRecursive(ves, e) ->
        ves
        |> VOption.mapSeq (function
            | v, (Lambdas(vvs, body) as lambda) -> ValueSome struct(v, vvs, body, lambda)
            | _ -> ValueNone
        )
        |> VOption.map (fun xs -> struct (Seq.toList xs, e))
    | _ -> ValueNone

let inline private getCaseOrRaise templateParameter =
    let c =
        templateParameter
        |> tryPick (function
            | E.NewUnionCase(c, _) -> Some c
            | _ -> None
        )
        |> Option.defaultWith (fun _ ->
            invalidArg (nameof templateParameter) "e.g. <@ Some @>"
        )
    struct(c.Tag, let (GenericTypeDefinition t) = c.DeclaringType in t)

[<return: Struct>]
let (|SpecificNewUnionCase|_|) templateParameter =
    let struct(tag, t) = getCaseOrRaise templateParameter

    function
    | E.NewUnionCase(c', args) when tag = c'.Tag && let (GenericTypeDefinition t') = c'.DeclaringType in t = t' ->
       ValueSome struct(c'.DeclaringType.GetGenericArguments() |> Array.toList, args)
    | _ -> ValueNone

[<return: Struct>]
let (|SpecificUnionCaseTest|_|) templateParameter =
    let struct(tag, t) = getCaseOrRaise templateParameter

    function
    | E.UnionCaseTest(e, c') when tag = c'.Tag && let (GenericTypeDefinition t') = c'.DeclaringType in t = t' ->
        ValueSome struct(c'.DeclaringType.GetGenericArguments() |> Array.toList, e)
    | _ -> ValueNone

[<return: Struct>]
let (|SpecificUnionCaseFieldGet|_|) templateParameter =
    let struct(GenericTypeDefinition t, name) =
        templateParameter
        |> tryPick (function
            | E.PropertyGet(Some u, p, []) when T.IsUnion(u.Type, true) -> Some struct(u.Type, p.Name)
            | _ -> None
        )
        |> Option.defaultWith (fun _->
            invalidArg (nameof templateParameter) "e.g. <@ function Some x -> Some x | _ -> None @>"
        )

    function
    | E.PropertyGet(Some u, p, []) when p.Name = name && let (GenericTypeDefinition t') = u.Type in t = t' ->
        ValueSome struct(u.Type.GetGenericArguments() |> Array.toList, u)
    | _ -> ValueNone

let (|SpecificPropertyGet|_|) templateParameter =
    let p' = Member.findProperty templateParameter
    function
    | E.PropertyGet(this, p, indexer) when p = p' -> Some(this, indexer)
    | _ -> None

let (|SpecificPropertySet|_|) templateParameter =
    let p' = Member.findProperty templateParameter
    function
    | E.PropertySet(this, p, indexer, value) when p = p' -> Some(this, indexer, value)
    | _ -> None

// <@ f.m(a, b, c) @> => <@ $Call(f, m, [a;b;c]) @>
// <@ f.m a (b, c) d @> => <@ (fun a (b, c) d -> $Call(f, m, [a;b;c;d]))) a (b, c) d @>
// <@ (p x).m a (b, c) d @> => <@ let objectArg = (p x) in (fun a (b, c) d -> $Call(objectArg, m, [a;b;c;d]))) a (b, c) d @>
[<return: Struct>]
let (|UniversalCall|_|) = function
    | E.Call(this, m, args) -> ValueSome struct(this, m, args)
    | E.Applications(lambdas, args) ->
        let rec varEq = function
            | [], [] -> true
            | v::vs, (E.Var v')::args when v = v' -> varEq (vs, args)
            | _ -> false

        match lambdas with
        | E.Let(objectArg, objectWithSideEffect, Lambdas(vvs, E.Call(Some(E.Var objectArg'), m, callArgs))) when 
            objectArg = objectArg' && varEq (List.concat vvs, callArgs) ->
            ValueSome(Some objectWithSideEffect, m, List.concat args)

        | Lambdas(vvs, E.Call(this, m, callArgs)) when varEq (List.concat vvs, callArgs) ->
            ValueSome(this, m, List.concat args)

        | _ -> ValueNone
    | _ -> ValueNone
