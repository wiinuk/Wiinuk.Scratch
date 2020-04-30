[<AutoOpen>]
module Scratch.Reflection.Transformers.Transformer
open Scratch.Reflection.Expr
module E = FSharp.Quotations.Patterns
module E = FSharp.Quotations.ExprShape
type private E = FSharp.Quotations.Expr


let merged ts = fun expr -> List.fold (fun e f -> Option.defaultValue expr e |> f |> Option.orElse e) None ts

let rec transformDeepOnce t expr =
    let expr' = t expr
    let expr = Option.defaultValue expr expr'
    match expr with
    | E.ShapeVar _ -> expr'
    | E.ShapeLambda(v, body) ->
        match transformDeepOnce t body with
        | None -> expr'
        | Some e -> E.Lambda(v, e) |> withLocation (getLocation expr) |> Some

    | E.ShapeCombination(_, []) -> expr'
    | E.ShapeCombination(x, es) ->
        match transformDeepOnceList t es with
        | None -> expr'
        | Some es -> E.RebuildShapeCombination(x, es) |> withLocation (getLocation expr) |> Some

and private transformDeepOnceList t = function
    | [] -> None
    | e::es ->

    match transformDeepOnce t e, transformDeepOnceList t es with
    | None, None -> None
    | None, Some es -> Some(e::es)
    | Some e, None -> Some(e::es)
    | Some e, Some es -> Some(e::es)

let rec private transformDeepRepeat maxCount t e =
    if maxCount <= 0 then e else
    match transformDeepOnce t e with
    | None -> e
    | Some e -> transformDeepRepeat (maxCount - 1) t e

let transform maxCount ts e = transformDeepRepeat maxCount (merged ts) e
