module Scratch.Transformers.Transformer
module R = TransformResult


let inline noop x _ = R.noModify x

let private mergeArray (ts: _ array) e env =
    let mutable e = e
    let mutable tag = ModifyTag.None
    let mutable i = 0
    while i < ts.Length do
        let e' = (ts[i]: OptimizedClosures.FSharpFunc<_,_,_>).Invoke(e, env)
        tag <- tag ||| e'.tag
        e <- e'.value
        i <- i + 1
    { tag = tag; value = e }

let merged = function
    | [] -> noop
    | [t] -> t
    | [t1; t2] -> R.compose t1 t2
    | ts ->

        ts
        |> Seq.map OptimizedClosures.FSharpFunc<_,_,_>.Adapt
        |> Seq.toArray
        |> mergeArray

let transformRepeat maxIterationCount t e env =
    let rec aux count t e env =
        if count <= 0 then e else
        let e' = t e.value env
        if R.isNoModify e' then e
        else aux (count - 1) t e' env

    aux maxIterationCount t (R.noModify e) env
    |> R.withIsSkip false
