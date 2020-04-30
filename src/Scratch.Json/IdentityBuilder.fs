module Scratch.Json.IdentityBuilder

type IdentityBuilder = | IdentityBuilder with
    member inline _.Return x = x
    member inline _.ReturnFrom x = x
    member inline _.Bind(x, f) = f x
    member inline _.TryFinally(x, f) = try x() finally f()
    member inline _.Delay f = f
    member inline _.Run f = f()
    member inline _.Zero() = ()
    member inline _.Using(x, f) = use x = x in f x
    member inline _.Combine((), f) = f()

let identity = IdentityBuilder
