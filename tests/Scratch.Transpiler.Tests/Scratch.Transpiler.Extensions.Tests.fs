module Scratch.Transpiler.Extensions.Tests
open Scratch
open Scratch.Ast
open Scratch.Transpiler
open Xunit


module Translate =
    open Scratch.IR
    open Scratch.IR.Source.Operators

    let getTranslate (_word: string) (_language: string) = _word

    let private transpilerPlugin =
        { Plugin.empty with
            expression = { new ExpressionPluginProcess() with
                override _.Invoke tranapiler e =
                    match e with
                    | Quotations.DerivedPatterns.SpecificCall <@ getTranslate @> (_, [], [word; language]) ->
                        let word = transpileExpression tranapiler word
                        let language = transpileExpression tranapiler language

                        let source = SourceCode.ofExpr e |> SourceCode.tag
                        Op(Symbol.Extension, [
                            Lit(SString "translate_getTranslate") @+ source
                            word
                            language
                        ]) @+ source
                        |> Ok 
                    | _ -> Error None
            }
        }

    let withTranspilerConfig c = { c with plugin = Plugin.merge c.plugin transpilerPlugin }

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
                ListData.make () "output" []
            ]
            scripts = [
                let translate_getTranslate s word language =
                    ComplexExpression(s, Symbol.Extension, [Expression.eString s "translate_getTranslate"; word; language])

                Expressions.whenGreenFlag () [
                    translate_getTranslate () (Expression.eString () "Hello") (Expression.eString () "sr")
                    |> Complex
                    |> Expressions.``append:toList:`` () "output"
                ]
            ]
    }
