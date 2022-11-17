module Scratch.Transpiler.Cloud.Tests
open Xunit
open Scratch
open Scratch.Transpiler.Tests
open Scratch.Evaluator
open Scratch.Executor

let newIncrementCloud() =
    let mutable map = Map.empty
    Cloud.poly { new ICloud with
        override _.Set(n, v) = map <- Map.add n v map
        override _.Get n =
            let v =
                match Map.tryFind n map with
                | ValueSome x -> SValue.toNumber x + 1. |> SNumber
                | _ -> SValue.sZero
            map <- Map.add n v map
            v
    }

[<ReflectedDefinition; Persistent>]
let mutable ``☁ x`` = 10.

[<Fact>]
let cloudVariableTest() =
    <@
        let output = defineList []
        whenGreenFlag {
            ``☁ x`` <- 20.
            do! repeatAsync 3 {
                SList.push output ``☁ x``
            }
        }
        startMainLoop()
    @>
    |> startAsStdSpriteWith' id
        (fun c -> EvaluateConfig.withCloud (newIncrementCloud()) c)
        (fun c -> ExecutionConfig.withCloud (newIncrementCloud()) c)

    =? ["21"; "22"; "23"]
