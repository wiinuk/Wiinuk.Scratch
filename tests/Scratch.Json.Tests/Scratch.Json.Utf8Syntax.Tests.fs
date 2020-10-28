module Scratch.Json.Utf8Syntax.Tests
open Scratch.Primitives
open Scratch.Json
open Scratch.Json.PartialIsomorphisms
open Scratch.Json.Utf8
open Scratch.Json.Utf8.Syntax
open Scratch.Json.Tests
open Utf8Json
open Xunit


let parse s x =
    try deserializeString (Syntax.box s) x |> Ok
    with :? JsonParsingException as e -> Error e

let print s x =
    try serializeString (Syntax.box s) x |> Ok
    with e -> Error e

let roundtrip syntax = serializeString (Syntax.box syntax) >> deserializeString (Syntax.box syntax)
let roundtripEqualsProperty syntax x = roundtrip syntax x =? x

[<Fact>]
let arrayTest() =
    parse (jEmptyArray |> jArray) "[]" =? Ok HUnit
    parse (jNumber ** jEmptyArray |> jArray) "[0]" =? Ok(0. ^^ HUnit)
    parse (jNumber ** jString ** jEmptyArray |> jArray) "[10, \"A\"]" =? Ok(10. ^^ "A" ^^ HUnit)

    let syntax = jObject (("x", jArray (jNumber ** jNumber ** jEmptyArray)) @@ jEmptyObject)
    parse syntax """{ "x": [48, 34] }""" =? Ok((48.^^34.^^HUnit)^^HUnit)

    roundtripEqualsProperty (jArray (jNumber ** jEmptyArray)) (10. ^^ HUnit)

    let s = jArray (jTupleToList jNumber)
    parse (s <|> s) """[10, 20]""" =? Ok([10.; 20.]^^HUnit)


[<Fact>]
let objectTest() =
    let point2D =
        ("x", jNumber) @@
        ("y", jNumber) @@?
        ("magnitude", jNumber, -1.) @@!
        jEmptyObject
        |> jObject

    parse point2D """{}""" |> Result.mapError ignore =? Error()
    parse point2D """{"x": 10}""" =? Ok(10. ^^ None ^^ -1. ^^ HUnit)
    parse point2D """{"x": 10, "y": -20}""" =? Ok(10. ^^ Some -20. ^^ -1. ^^ HUnit)
    parse point2D """{"x": 10, "y": -20, "magnitude": 22.36 }""" =? Ok(10.^^Some -20.^^22.36^^HUnit)
    parse point2D """{"x": 10, "y": -20, "magnitude": 22.36, "z": 10 }""" |> Result.mapError ignore =? Error()
    parse point2D """{"x": 10, "magnitude": 10 }""" =? Ok(10.^^None^^10.^^HUnit)
    parse point2D """{"magnitude": -1, "y": -20000, "x": 1000, "magnitude": 22.36, "y": -20, "x": 10}"""
        =? Ok(10.^^Some -20.^^22.36^^HUnit)

[<Fact>]
let choiceTest() =
    let sbool =
        (jStringLiteral "TRUE" |>> Iso.singleton true) <|>
        (jStringLiteral "FALSE" |>> Iso.singleton false)

    parse sbool "\"TRUE\"" =? Ok true
    parse sbool "\"FALSE\"" =? Ok false
    print sbool true =? Ok "\"TRUE\""
    print sbool false =? Ok "\"FALSE\""

[<Fact>]
let numberRoundtripEqualsTest() = qcheck <| roundtripEqualsProperty jNumber

[<Fact>]
let stringRoundtripEqualsTest() = qcheck <| roundtripEqualsProperty jString

[<Fact>]
let jsonTest() =
    parse json "[null, true, false]" =? Ok(Json.jarray [Json.jnull; Json.jtrue; Json.jfalse])

[<Fact>]
let stringEscapeTest() =
    let s = jString
    print s "\u0019" =? Ok "\"\\u0019\""
