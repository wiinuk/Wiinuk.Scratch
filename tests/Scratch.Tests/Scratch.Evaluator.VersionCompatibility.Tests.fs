module Scratch.Evaluator.VersionCompatibility.Tests

open Scratch
open Scratch.Ast
open Scratch.Evaluator
open Scratch.Executor
open Scratch.Executor.Executor
open Scratch.Primitives
open Xunit

module A = Expression
module A = Expressions

let evaluate' withEvaluateConfig withExecuteConfig data =
    let getOutput { Evaluator.stage = stage } =
        match Map.tryFind "output" stage.lists with
        | ValueNone -> []
        | ValueSome output ->
            output
            |> Seq.map SValue.toString
            |> Seq.toList

    let getOutput' =
        ExecutionState.stage
        >> EntityState.list "output"
        >> Option.defaultValue []
        >> List.map SValue.toString

    let output1 =
        data
        |> evaluateStage withEvaluateConfig
        |> getOutput

    let output2 =
        let image = data |> Executor.Compiler.compileToImage
        let state = image |> Executor.Executor.runImageWith withExecuteConfig
        getOutput' state

    output2 =? output1
    output1

let evaluateSb2 data =
    evaluate'
        (fun c -> { c with version = RuntimeVersion.Sb2 })
        (ExecutionConfig.withVersion RuntimeVersion.sb2)
        data

let evaluateSb3 data =
    evaluate'
        (fun c -> { c with version = RuntimeVersion.Sb3 })
        (ExecutionConfig.withVersion RuntimeVersion.sb3)
        data

[<Fact>]
let deleteListRandomTest() =
    let data line =
        { StageData.defaultValue with
            lists = [
                ListData.make () "output" []
                ListData.make () "list" []
            ]
            scripts = [
                A.whenGreenFlag () [
                    A.``append:toList:`` () "list" (A.eNumber () 10.)
                    A.``append:toList:`` () "list" (A.eNumber () 10.)
                    A.``deleteLine:ofList:`` () "list" (A.eString () line)
                    A.``append:toList:`` () "output" (A.``lineCountOfList:`` () "list")
                ]
            ]
        }

    evaluateSb2 (data "any") =? ["2"]
    evaluateSb2 (data "random") =? ["2"]
    evaluateSb3 (data "any") =? ["1"]
    evaluateSb3 (data "random") =? ["1"]

[<Fact>]
let letterOfInfinityTest() =
    let infinity = A.``/`` () (A.eNumber () 1.) (A.eNumber () 0.)
    /// `-infinity`
    let mInfinity = A.``/`` () (A.eNumber () -1.) (A.eNumber () 0.)
    let out e = A.``append:toList:`` () "output" e
    let letterOf i x = A.``letter:of:`` () i (A.eString () x)

    let data =
        { StageData.defaultValue with
            lists = [
                ListData.make () "output" []
            ]
            scripts = [
                A.whenGreenFlag () [
                    out (letterOf infinity "world")
                    out (letterOf mInfinity "world")
                    out (letterOf (A.eString () "Infinity") "world")
                ]
            ]
        }

    evaluateSb2 data =? ["w";"w";"w"]
    evaluateSb3 data =? ["";"";""]
