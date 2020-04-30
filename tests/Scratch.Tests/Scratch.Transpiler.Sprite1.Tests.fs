module Scratch.Transpiler.Tests
open Xunit
open Scratch
open Scratch.Ast
open Scratch.Evaluator
open Scratch.Executor


[<Fact>]
let evaluateTest() =
    async {
        let! sprite = "./Sprite1.sprite2" |> readSprite2Project
        let state = evaluateSprite id sprite
        state.objects.[0].lists.["stdout"] |> Seq.map SValue.toString |> Seq.toList =? ["test";"success"]

        let state =
            let s = StageData.defaultValue
            { s with
                ObjectDataExtension =
                { s.ObjectDataExtension with
                    children = [Choice2Of3 sprite]
                }
            }
            |> Executor.Compiler.compileToImage |> Executor.runImage

        state
            |> ExecutionState.sprites "Sprite1"
            |> List.exactlyOne
            |> EntityState.list "stdout"
            |> Option.defaultValue []
            |> Seq.map SValue.toString
            |> Seq.toList
            =? ["test";"success"]
    }
    |> Async.StartAsTask
