[<AutoOpen>]
module Scratch.Primitives.MathOperators
[<ReflectedDefinition>]
let inline clamp (lo, hi) x = max lo (min hi x)
