module Scratch.Primitives.VOption

let inline isSome x = match x with ValueSome _ -> true | _ -> false
let inline isNone x = match x with ValueNone -> true | _ -> false
let inline map f = function ValueSome x -> ValueSome(f x) | _ -> ValueNone
let inline bind f = function ValueSome x -> f x | _ -> ValueNone
let inline defaultValue x = function ValueSome x -> x | _ -> x
let inline box x = match x with ValueSome x -> Some x | _ -> None
let inline unbox x = match x with Some x -> ValueSome x | _ -> ValueNone
