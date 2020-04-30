/// Provides a locked version of the `|*WithReflectedDefinition|_|`.
module FSharp.Quotations.DerivedPatterns

let private gate = obj()

/// locked version
let (|MethodWithReflectedDefinition|_|) e =
    lock gate <| fun _ -> global.Microsoft.FSharp.Quotations.DerivedPatterns.(|MethodWithReflectedDefinition|_|) e

/// locked version
let (|PropertyGetterWithReflectedDefinition|_|) e =
    lock gate <| fun _ -> global.Microsoft.FSharp.Quotations.DerivedPatterns.(|PropertyGetterWithReflectedDefinition|_|) e

/// locked version
let (|PropertySetterWithReflectedDefinition|_|) e =
    lock gate <| fun _ -> global.Microsoft.FSharp.Quotations.DerivedPatterns.(|PropertySetterWithReflectedDefinition|_|) e
