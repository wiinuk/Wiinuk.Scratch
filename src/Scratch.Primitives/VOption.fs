module Scratch.Primitives.VOption

let inline map ([<InlineIfLambda>] f) = function ValueSome x -> ValueSome(f x) | _ -> ValueNone
let inline bind ([<InlineIfLambda>] f) = function ValueSome x -> f x | _ -> ValueNone
let inline box x = match x with ValueSome x -> Some x | _ -> None
let inline unbox x = match x with Some x -> ValueSome x | _ -> ValueNone
let inline mapSeq ([<InlineIfLambda>] mapping) (xs: #seq<_>) =
    use e = xs.GetEnumerator()
    let mutable xs = null
    let mutable hasNone = false
    while
        begin
            if e.MoveNext() then
                match mapping e.Current with
                | ValueNone ->
                    xs <- null
                    hasNone <- true
                    false

                | ValueSome x ->
                    if isNull xs then xs <- ResizeArray()
                    xs.Add x
                    true
            else
                false
        end
        do ()

    match xs, hasNone with
    | null, true -> ValueNone
    | null, _ -> ValueSome([] :> _ seq)
    | _ -> ValueSome(xs :> _ seq)
