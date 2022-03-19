module Scratch.Json.IdentityBuilder

type IdentityBuilder = | IdentityBuilder with
    member inline _.Return x = x
    member inline _.ReturnFrom x = x
    member inline _.Bind(x, [<InlineIfLambda>] f) = f x
    member inline _.TryFinally(x, [<InlineIfLambda>] f) = try x() finally f()
    member inline _.Delay([<InlineIfLambda>] f: _ -> _) = f
    member inline _.Run([<InlineIfLambda>] f) = f()
    member inline _.Zero() = ()
    member inline _.Using(x, [<InlineIfLambda>] f) = use x = x in f x
    member inline _.Combine((), [<InlineIfLambda>] f) = f()

let identity = IdentityBuilder
