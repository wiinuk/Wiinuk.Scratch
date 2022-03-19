module Scratch.Transpiler.Extensions.Tests
open Scratch
open Scratch.Ast
open Scratch.Transpiler
open Xunit
open Scratch.Test.AssertionWithDiff.Operators

module Translate =
    open Scratch.IR
    open Scratch.IR.Source.Operators
    open Scratch.AstDefinitions
    open Scratch.Reflection
    open FSharp.Quotations.DerivedPatterns

    let getTranslate (_word: string) (_language: string) = _word

    let private getTranslateSign = {
        extensionId = "translate_getTranslate"
        kind = Kind.Expression
        resultType = ExpType.ofVType <| Typed SType.S
        operands = [
            {
                operandType = OperandType.Expression TsType.gString
                literalOperandType = LiteralOperandTypeInfo.ForceInherit
            }
            {
                operandType = OperandType.Expression TsType.gString
                literalOperandType = LiteralOperandTypeInfo.ForceInherit // TODO:
            }
        ]
        control = Control.Unknown
        cost = HasSideEffect
    }
    let private transpilerPlugin =
        { Plugin.empty with
            expression = { new ExpressionPluginProcess() with
                override _.Invoke tranapiler e =
                    match e with
                    | SpecificCall <@ getTranslate @> (_, [], [word; language]) ->
                        let word = transpileExpression tranapiler word
                        let language = transpileExpression tranapiler language

                        let source = SourceCode.ofExpr e |> SourceCode.tag
                        Ok <| ExtOp(getTranslateSign, [word; language]) @+ source

                    | _ -> Error None
            }
        }

    let withTranspilerConfig c = { c with plugin = Plugin.merge c.plugin transpilerPlugin }

module E = Ast.Expression
module E = Ast.Expressions

[<Fact>]
let getTranslateTest() =
    <@
        let output = defineList []
        SList.push output (Translate.getTranslate "Hello" "sr")
    @>
    |> transpileStageWith Translate.withTranspilerConfig
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

module Translate2 =
    [<Block(ExtensionId = "translate")>]
    let getTranslate (_WORDS: string) (_LANGUAGE: string) = _WORDS

[<Fact>]
let instantGetTranslateTest() =
    <@
        let output = defineList []
        SList.push output (Translate2.getTranslate "Hello" "sr")
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
