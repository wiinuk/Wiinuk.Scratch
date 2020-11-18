module Scratch.Transpiler.Extensions.Tests
open Scratch
open Scratch.Ast
open Scratch.Transpiler
open Xunit
open Scratch.Test.AssertionWithDiff.Operators


module Translate =
    [<Block(ExtensionId = "translate")>]
    let getTranslate (_WORDS: string) (_LANGUAGE: string) = _WORDS

module E = Ast.Expression
module E = Ast.Expressions

[<Fact>]
let getTranslateTest() =
    <@
    let output = defineList []
    SList.push output (Translate.getTranslate "Hello" "sr")
    @>
    |> transpileStageWith id
    |> StageData.map ignore
    =? {
        StageData.defaultValue with
            lists = [
                { ListData.make () "output" [] with width = 0.; height = 0. }
            ]
            scripts = [
                let translate_getTranslate s word language =
                    ComplexExpression(s, Symbol.Extension, [E.eString s "translate_getTranslate"; word; language])

                E.whenGreenFlag () [
                    E.``deleteLine:ofList:`` () "output" (E.eString () "all")
                    translate_getTranslate () (E.eString () "Hello") (E.eString () "sr")
                    |> Complex
                    |> E.``append:toList:`` () "output"
                ]
            ]
    }
