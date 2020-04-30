namespace Scratch.Primitives

[<Struct>]
type LowerName = private LowerName of string
module LowerName =
    let ofString (x: string) = LowerName <| x.ToLowerInvariant()
    let toString (LowerName x) = x
