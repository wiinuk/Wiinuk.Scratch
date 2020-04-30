module Scratch.Json.Parser.Tests
open FsCheck
open Xunit
open Scratch.Primitives
open Scratch.Json
open Scratch.Json.Utf8
open Scratch.Json.Parser.Test.Helpers
module Map = OMap

let print = Syntax.serializeString Syntax.json
let parse = Syntax.deserializeString Syntax.json >> unLocation


let jNumber = JNumber >> json
let jString = JString >> json
let jArray = JArray >> json
let jObject = Map.ofSeq >> JObject >> json


let numberRoundTripProperty (NormalFloat x) =
    let s = sprintf "%.17g" x
    parse s =? json(JNumber x)

[<Fact>]
let numberRoundTripTest() = quickcheck numberRoundTripProperty

let jsonRoundTripProperty j =
    let j' = j |> print |> parse
    unLocation j' =? unLocation j

[<Fact>]
let jsonRoundTripPropertyTest() = quickcheck jsonRoundTripProperty

[<Fact>]
let printerTest() =
    jString "\n*" |> print =? "\"\\n*\""
    jString null |> print =? "null"
    jNumber nan |> print =? "NaN" // out of spec
    jsonRoundTripProperty <| jNumber infinity
    jsonRoundTripProperty <| jNumber -infinity

[<Fact>]
let parserTest() =
    parse "null" =? json JNull
    parse "\r\n\t null  " =? json JNull
    parse "null _" =? json JNull // out of spec
    throws (lazy parse "_ null")

    parse "true" =? json JTrue
    parse "false" =? json JFalse

    parse "0" =? jNumber 0.
    parse "10" =? jNumber 10.
    parse "01" =? jNumber 1. // spec violation
    parse "-0" =? jNumber -0.
    parse "-10" =? jNumber -10.
    parse "-01" =? jNumber -1. // spec violation

    parse "12.34" =? jNumber 12.34
    parse "0.0120" =? jNumber 0.012
    parse "0.00" =? jNumber 0.

    parse "1." =? jNumber 1. // spec violation
    throws (lazy parse ".1")
    parse "01.1" =? jNumber 1.1 // spec violation

    parse "0e12" =? jNumber 0e12
    parse "5e0" =? jNumber 5e0
    parse "5e+06" =? jNumber 5e+06
    parse "-12.34E-56" =? jNumber -12.34E-56

    parse "18e307" =? jNumber infinity
    parse "1000000e303" =? jNumber 9.2188684372274053e18 // out of spec ( infinity? )
    parse "180000000e300" =? jNumber infinity
    parse "17976931348623159e292" =? jNumber infinity

    parse "8.9255000000000008e-18" =? jNumber 8.9255000000000008e-18
    // parse "89255e-22" =? jNumber 89255e-22 // linux, x86

    parse """ "" """ =? jString ""
    parse """ "abc" """ =? jString "abc"
    parse """ "🍰" """ =? jString "🍰"
    parse """ "\\\"\b\n\r\t\u01aF" """ =? jString "\\\"\b\n\r\t\u01aF"
    parse """ "\u004134" """ =? jString "A34"
    parse "\"\n\"" =? jString "\n" // spec violation
    throws (lazy parse """ "\X" """)
    throws (lazy parse """ "\" """)
    throws (lazy parse """ "\u" """)
    throws (lazy parse """ "\u00" """)

    parse "[]" =? jArray []
    parse "[null]" =? jArray [json JNull]
    parse "[null, 20]" =? jArray [json JNull; jNumber 20.]

    parse "{}" =? jObject []
    parse """{ "name": "alice", "age": 80 }"""
    =? jObject [
        "name", jString "alice"
        "age", jNumber 80.
    ]
    throws (lazy parse """{"x"}""")
    throws (lazy parse """{"x":}""")
    throws (lazy parse """{"x":0,}""")
    throws (lazy parse """{,}""")
    throws (lazy parse """{:}""")

    parse "{\r\n    \"name\": \"bob\",\r\n    \"age\": 30,\r\n    \"books\": [\"A\", \"B\"]\r\n}"
    =? jObject [
        "name", jString "bob"
        "age", jNumber 30.
        "books", jArray [
            jString "A"
            jString "B"
        ]
    ]
