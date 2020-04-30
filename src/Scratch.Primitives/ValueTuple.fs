module Scratch.Primitives.ValueTuple

let inline fst struct(x, _) = x
let inline snd struct(_, x) = x
