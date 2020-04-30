module Scratch.Primitives.VOption

let inline isSome x = match x with ValueSome _ -> true | _ -> false
let inline isNone x = match x with ValueNone -> true | _ -> false
let inline map f = function ValueSome x -> ValueSome(f x) | ValueNone -> ValueNone
let inline defaultValue x = function ValueSome x -> x | ValueNone -> x
let inline box x = match x with ValueSome x -> Some x | ValueNone -> None
let inline unbox x = match x with Some x -> ValueSome x | None -> ValueNone
