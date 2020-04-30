module Scratch.Json.Tests
open Scratch
open Scratch.Ast
open Scratch.Json.Utf8
open Scratch.Serialization.Sb2.Syntax
open Scratch.Primitives
open Scratch.Json.Test.Helpers
open FsCheck
open Scratch.Runtime.Test
open Xunit

module A = Scratch.Ast.Expression


let print s = Syntax.serializeString <| Syntax.box s
let parse s = Syntax.deserializeString <| Syntax.box s
let empty = None

let qcheck test = qcheckWith (fun c -> { c with Arbitrary = typeof<Arbs> :: c.Arbitrary }) test

let roundTrip syntax x = parse syntax (print syntax x)
let roundTripTest syntax mapping = qcheck <| fun x ->
    let x = mapping (fun () -> None) x
    roundTrip syntax x =? x

let spriteData<'a> = spriteData HasDefault.option
let scriptData<'a> = scriptData HasDefault.option
let script<'a> = script HasDefault.option
let statement<'a> = statement HasDefault.option
let stageData<'a> = stageData HasDefault.option
let complexExpression<'a> = complexExpression HasDefault.option

let [<Fact>] spriteDataRoundTripTest() = roundTripTest spriteData SpriteData.map
let [<Fact>] scriptDataRoundTripTest() = roundTripTest scriptData ScriptData.map
let [<Fact>] scriptRoundTripTest() = roundTripTest script Script.map
let [<Fact>] statementRoundTripTest() = roundTripTest statement ComplexExpression.map
let [<Fact>] soundDataRoundTripTest() = roundTripTest soundData (fun _ x -> x)
let [<Fact>] watcherDataRoundTripTest() = roundTripTest watcherData (fun _ x -> x)

let [<Fact>] singleStatementsAsExpressionTest() =
    let json = """
        {
          "objName": "Stage",
          "scripts": [
            [0, 0, [
                ["computeFunction:of:", "sin", 0]
            ]]
          ],
          "currentCostumeIndex": 0,
          "tempoBPM": 60,
          "videoAlpha": 0.5
        }
    """
    json
    |> parse stageData =? {
        StageData.defaultValue with
            scripts = [
                {
                    x = 0.; y = 0.; script = Expression(ComplexExpression(empty, O.``computeFunction:of:``, [A.eString empty "sin"; A.eNumber empty 0.]))
                }
            ]
    }

let [<Fact>] singleExpressionStatementsAsExpressionTest() =
    {
        x = 0.
        y = 0.
        script = Statements(BlockExpression(empty, [
            ComplexExpression(empty, O.``+``, [A.eNumber empty 0.; A.eNumber empty 0.])
        ]))
    }
    |> roundTrip scriptData =? {
        x = 0.
        y = 0.
        script = Expression(ComplexExpression(empty, O.``+``, [A.eNumber empty 0.; A.eNumber empty 0.]))
    }

let [<Fact>] nullAsEmptyBlockTest() =
    """["doForever", null]"""
    |> parse complexExpression =? ComplexExpression(empty, O.doForever, [Block(BlockExpression(empty, []))])

let stringLiteralRoundTripProperty = function
    | null -> ()
    | x ->

    let s = Syntax.jStringLiteral x
    let json = print s HUnit
    let (HUnit) = parse s json
    ()

let [<Fact>] stringLiteralRoundTripTest() =
    stringLiteralRoundTripProperty "a"
    stringLiteralRoundTripProperty "\n"
    stringLiteralRoundTripProperty "\u12EF"
    stringLiteralRoundTripProperty "\""
    stringLiteralRoundTripProperty "\\"

let [<Fact>] stringLiteralRoundTripPropertyTest() =
    qcheck stringLiteralRoundTripProperty
