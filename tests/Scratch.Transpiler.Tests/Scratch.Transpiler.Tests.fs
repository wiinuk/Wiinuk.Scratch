module Scratch.Transpiler.Tests
open FSharp.Quotations
open Scratch
open Scratch.Primitives
open Scratch.Ast
open Scratch.Evaluator
open Scratch.Executor
open Scratch.Transpiler
module A = Scratch.Ast.Expressions
module P = Scratch.Ast.ParameterDefinition
module E = Quotations.Patterns
module E = Quotations.DerivedPatterns

let evaluate = evaluateStage
let startAsStdSpriteWith' withTranspileConfig withEvaluateConfig withExecuteConfig e =
    let getOutput { Evaluator.stage = stage } =
        match Map.tryFind "output" stage.lists with
        | ValueNone -> []
        | ValueSome output ->
            output
            |> Seq.map SValue.toString
            |> Seq.toList

    let data =
        e
        |> transpileStageWith withTranspileConfig

    let output1 =
        data
        |> evaluate (fun c -> withEvaluateConfig { c with showState = showLoc })
        |> getOutput

    let output2 =
        let image =
            data
            |> Executor.Compiler.compileToImage

        let state =
            image
            |> Executor.Executor.runImageWith withExecuteConfig

        let stage = state.mainStage.stageState
        stage.entityImage.listVariables
        |> IArray.toSeqCopiable
        |> Seq.tryFindIndex (fun v -> "output" = v.listName)
        |> function
            | None -> []
            | Some index ->
                stage.listVariables[index]
                |> Seq.map Value.toString
                |> Seq.toList

    output2 =? output1
    output1

let startAsStdSpriteWith withTranspileConfig withEvaluateConfig e = startAsStdSpriteWith' withTranspileConfig withEvaluateConfig id e
let startAsStdSprite e = startAsStdSpriteWith id id e
