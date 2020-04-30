namespace Scratch.Primitives

[<Struct>]
type The<'T> = | The

[<AutoOpen>]
module TheOperators =
    [<RequiresExplicitTypeArguments>]
    let the<'T> : 'T The = The
