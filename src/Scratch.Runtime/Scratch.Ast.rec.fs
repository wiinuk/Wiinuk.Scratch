namespace rec Scratch.Ast
open Scratch
open Scratch.Ast
open Scratch.Primitives


[<AutoOpen>]
module private Rec =
    let mapExpression f = function
        | Literal(s, x) -> Literal(f s, x)
        | Complex x -> Complex(mapComplex f x)
        | Block x -> Block(mapBlock f x)
    let mapComplex f (ComplexExpression(x, op, xs)) = ComplexExpression(f x, op, List.map (mapExpression f) xs)
    let mapBlock f (BlockExpression(x, ss)) = BlockExpression(f x, List.map (mapComplex f) ss)

    let foldExpression f s = function
        | Literal _ -> s
        | Complex x -> foldComplex f s x
        | Block x -> foldBlock f s x

    let foldComplex (f: OptimizedClosures.FSharpFunc<_,_,_>) s (ComplexExpression(operands = xs) as e) =
        let s = f.Invoke(s, e)
        List.fold (foldExpression f) s xs
   
    let foldBlock f s (BlockExpression(body = xs)) =
        List.fold (foldComplex f) s xs

    let tryPickExpression f = function
        | Literal _ -> None
        | Complex x -> tryPickComplex f x
        | Block x -> tryPickBlock f x

    let tryPickComplex f (ComplexExpression(operands = xs) as e) =
        match f e with
        | Some _ as r -> r
        | _ -> List.tryPick (tryPickExpression f) xs

    let tryPickBlock f (BlockExpression(body = xs)) = List.tryPick (tryPickComplex f) xs

module Expression =
    let map mapping e = mapExpression mapping e
    let fold folder state e = foldExpression (OptimizedClosures.FSharpFunc<_,_,_>.Adapt folder) state e
    let tryPick chooser e = tryPickExpression chooser e
    let rec mapComplexExpression mapping = function
        | Complex x -> Complex(ComplexExpression.mapComplexExpression mapping x)
        | Block x -> Block(BlockExpression.mapComplexExpression mapping x)
        | Literal _ as x -> x

    let eString p x = Literal(p, SString x)
    let eNumber p x = Literal(p, SNumber x)
    let eBool p x = Literal(p, SBool x)
    let state = function
        | Literal(state = x)
        | Complex(ComplexExpression(state = x))
        | Block(BlockExpression(state = x)) -> x

    let flow mergeState rider state e =
        let mergeState = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mergeState
        let rider = OptimizedClosures.FSharpFunc<_,_,_>.Adapt rider

        let rec expression s e =
            match rider.Invoke(s, e) with
            | Error _ as r -> r
            | Ok s ->

            match e with
            | Literal _ -> Ok s
            | Block x -> block s x
            | Complex x -> complex s x

        and block s (BlockExpression(body = body)) =
            let rec aux s = function
                | [] -> Ok s
                | e::es ->

                match complex s e with
                | Error _ as r -> r
                | Ok s -> aux s es

            aux s body

        and complex s (ComplexExpression(operator = operator; operands = operands) as e) = result {
            match operator, operands with
            | (O.doForeverIf | O.doWhile), [test; Block ifTrue] ->
                let! s = expression s test
                let! s1 = block s ifTrue
                //let! s1 = expression s1 test
                return! mergeState.Invoke(s, s1)

            | O.doIf, [test; Block ifTrue] ->
                let! s = expression s test
                let! s1 = block s ifTrue
                return! mergeState.Invoke(s, s1)

            | O.doIfElse, [test; Block ifTrue; Block ifFalse] ->
                let! s = expression s test
                let! s1 = block s ifTrue
                let! s2 = block s ifFalse
                return! mergeState.Invoke(s1, s2)

            | O.doRepeat, [count; Block body] ->
                let! s = expression s count
                let! s1 = block s body
                return! mergeState.Invoke(s, s1)

            | O.doReturn, [] -> return s

            | O.doUntil, [test; Block ifFalse] ->
                let! s = expression s test
                let! s1 = block s ifFalse
                //let! s1 = expression s1 test
                return! mergeState.Invoke(s, s1)
 
            | O.doWaitUntil, [test] -> return! expression s test

            | O.doForever, [Block body] -> return! block s body
            | O.stopAll, []
            | O.stopScripts, [_] -> return s
            | O.deleteClone, [] -> return s
            | _ ->

            let rec aux s = function
                | [] -> rider.Invoke(s, Complex e)
                | e::es ->

                match expression s e with
                | Error _ as r -> r
                | Ok s -> aux s es

            return! aux s operands
        }
        expression state e

    let hasVariableAccess varName e =
        e
        |> tryPick (fun c -> if ComplexExpression.isVariableAccess varName c then Some() else None)
        |> Option.isSome

    let variableCount varName e =
        e
        |> fold (fun s c -> s + if ComplexExpression.isVariableAccess varName c then 1 else 0) 0

[<AutoOpen>]
module ExpressionPatterns =
    [<return: Struct>]
    let (|EString|_|) = function Literal(p, SString x) -> ValueSome struct(p, x) | _ -> ValueNone
    [<return: Struct>]
    let (|EBool|_|) = function Literal(p, SBool x) -> ValueSome(p, x) | _ -> ValueNone

module ComplexExpression =
    let map mapping e = mapComplex mapping e
    let fold folder state e = foldComplex (OptimizedClosures.FSharpFunc<_,_,_>.Adapt folder) state e
    let tryPick chooser e = tryPickComplex chooser e
    let mapComplexExpression mapping (ComplexExpression(state, operator, operands)) =
        let operands = List.map (Expression.mapComplexExpression mapping) operands
        mapping (ComplexExpression(state, operator, operands))

    let state (ComplexExpression(state = x)) = x
    let withState state (ComplexExpression(_, operator, operands)) = ComplexExpression(state, operator, operands)

    let isVariableRead varName (ComplexExpression(operator = operator; operands = operands)) =
        match operator, operands with
        | O.readVariable, [Literal(_, SString n)]
        | O.``changeVar:by:``, [Literal(_, SString n); _]
        | O.``showVariable:``, [Literal(_, SString n)]
        | O.``hideVariable:``, [Literal(_, SString n)] when n = varName -> true
        | _ -> false

    let internal isVariableAccess varName (ComplexExpression(operator = operator; operands = operands)) =
        match operator, operands with
        | O.readVariable, [Literal(_, SString n)]
        | O.``setVar:to:``, [Literal(_, SString n); _]
        | O.``changeVar:by:``, [Literal(_, SString n); _]
        | O.``showVariable:``, [Literal(_, SString n)]
        | O.``hideVariable:``, [Literal(_, SString n)] when n = varName -> true
        | _ -> false

    let variableCount varName e = e |> fold (fun n c -> n + if isVariableAccess varName c then 1 else 0) 0

module BlockExpression =
    let map mapping e = mapBlock mapping e
    let fold folder state e = foldBlock (OptimizedClosures.FSharpFunc<_,_,_>.Adapt folder) state e
    let tryPick chooser e = tryPickBlock chooser e
    let mapComplexExpression mapping (BlockExpression(state, es)) =
        BlockExpression(state, List.map (ComplexExpression.mapComplexExpression mapping) es)

    let state (BlockExpression(state = x)) = x
    let withState state (BlockExpression(_, body)) = BlockExpression(state, body)
    let body (BlockExpression(body = x)) = x
