module Scratch.IR.Pretty.Tests

open Scratch.IR
open Xunit


[<Fact>]
let shadowingNameTest() =
    let pretty = Pretty.prettyExpWith <| fun c -> { c with indent = ""; newLine = " "; maxWidth = 999 }

    let e =
        Exp.letScope () "a" (Exp.number () 10.) <| fun a ->
        Exp.letScope () "a" (Exp.number () 20.) <| fun a' ->
        Exp.Op.``+`` () a a' 

    pretty e =? "let a = 10 in let a@1 = 20 in a + a@1"
