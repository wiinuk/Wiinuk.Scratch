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
        | [] -> None
        | _ -> Some(List.rev rvs, e) 

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

let (|Lambdas|_|) (input: Expr) = qOneOrMoreRLinear (|TupledLambda|) input

let private qOneOrMoreLLinear q inp =
    let rec queryAcc e rvs =
        match q e with
        | ValueSome struct(body, v) -> queryAcc body (v::rvs) 
        | _ -> 
            match rvs with
            | [] -> None
            | _ -> Some(e, rvs)
    queryAcc inp []

let private (|TupledApplication|) e =
    match e with
    | E.Application(f, x) ->
        match x with
        | E.NewTuple xs when not x.Type.IsValueType -> ValueSome struct(f, xs)
        | x -> ValueSome struct(f, [x])
    | _ -> ValueNone

let (|Applications|_|) (input: Expr) = qOneOrMoreLLinear (|TupledApplication|) input

let (|TupleChain|_|) e =
    let rec (|TupleChain|_|) = function
        | E.TupleGet(tuple, i) -> Some(tuple, i, [])
        | E.Let(v, TupleChain(tuple, i, is), E.TupleGet(E.Var v', n')) when v = v' -> Some(tuple, n', i::is)
        | _ -> None

    match e with
    | TupleChain(tuple, i, is) -> Some(tuple, List.rev(i::is))
    | _ -> None

let rec (|Sequentials|_|) = function
    | E.Sequential(e1, Sequentials(es, e)) as seq -> Some((e1, getLocation seq)::es, e)
    | e -> Some([], e)

/// `<@ let $x1 = $v1 in let $x2 = $v2 in ... in $e @>`
let rec (|Lets|) = function
    | E.Let(x, v, Lets(xvs, e)) -> (x, v)::xvs, e
    | e -> [], e

/// `[$xs...; $x]`
let rec (|HeadAndLast|_|) = function
    | [x] -> Some([], x)
    | x::HeadAndLast(xs, last) -> Some(x::xs, last)
    | _ -> None

let (|LetRecursiveLambdas|_|) = function
    | E.LetRecursive(ves, e) ->
        ves
        |> Option.mapSeq (function
            | v, (Lambdas(vvs, body) as lambda) -> Some(v, vvs, body, lambda)
            | _ -> None
        )
        |> Option.map (fun xs -> Seq.toList xs, e)
    | _ -> None

let inline private getCaseOrRaise templateParameter =
    let c =
        templateParameter
        |> tryPick (function
            | E.NewUnionCase(c, _) -> Some c
            | _ -> None
        )
        |> Option.defaultWith (fun _ ->
            invalidArg "templateParameter" "e.g. <@ Some @>"
        )
    c.Tag, let (GenericTypeDefinition t) = c.DeclaringType in t

let (|SpecificNewUnionCase|_|) templateParameter =
    let tag, t = getCaseOrRaise templateParameter

    function
    | E.NewUnionCase(c', args) when tag = c'.Tag && let (GenericTypeDefinition t') = c'.DeclaringType in t = t' ->
       Some(c'.DeclaringType.GetGenericArguments() |> Array.toList, args)
    | _ -> None

let (|SpecificUnionCaseTest|_|) templateParameter =
    let tag, t = getCaseOrRaise templateParameter

    function
    | E.UnionCaseTest(e, c') when tag = c'.Tag && let (GenericTypeDefinition t') = c'.DeclaringType in t = t' ->
        Some(c'.DeclaringType.GetGenericArguments() |> Array.toList, e)
    | _ -> None

let (|SpecificUnionCaseFieldGet|_|) templateParameter =
    let (GenericTypeDefinition t), name =
        templateParameter
        |> tryPick (function
            | E.PropertyGet(Some u, p, []) when T.IsUnion(u.Type, true) -> Some(u.Type, p.Name)
            | _ -> None
        )
        |> Option.defaultWith (fun _->
            invalidArg "templateParameter" "e.g. <@ function Some x -> Some x | _ -> None @>"
        )

    function
    | E.PropertyGet(Some u, p, []) when p.Name = name && let (GenericTypeDefinition t') = u.Type in t = t' ->
        Some(u.Type.GetGenericArguments() |> Array.toList, u)
    | _ -> None

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
let (|UniversalCall|_|) = function
    | E.Call(this, m, args) -> Some(this, m, args)
    | E.Applications(lambdas, args) ->
        let rec varEq = function
            | [], [] -> true
            | v::vs, (E.Var v')::args when v = v' -> varEq (vs, args)
            | _ -> false

        match lambdas with
        | E.Let(objectArg, objectWithSideEffect, Lambdas(vvs, E.Call(Some(E.Var objectArg'), m, callArgs))) when 
            objectArg = objectArg' && varEq (List.concat vvs, callArgs) ->
            Some(Some objectWithSideEffect, m, List.concat args)

        | Lambdas(vvs, E.Call(this, m, callArgs)) when varEq (List.concat vvs, callArgs) ->
            Some(this, m, List.concat args)

        | _ -> None
    | _ -> None
