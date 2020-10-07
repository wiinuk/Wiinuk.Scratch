module Scratch.Transpiler.Cloud.Tests
open Xunit
open Scratch
open Scratch.Transpiler.Tests
open Scratch.Evaluator

let withIncrementCloud c =
    let mutable map = Map.empty
    let incrementCloud = Cloud.poly { new ICloud with
        override _.Set(n, v) = map <- Map.add n v map
        override _.Get n =
            let v =
                match Map.tryFind n map with
                | ValueSome x -> SValue.toNumber x + 1. |> SNumber
                | _ -> SValue.sZero
            map <- Map.add n v map
            v
    }
    EvaluateConfig.withCloud incrementCloud c

[<ReflectedDefinition; Persistent>]
let mutable ``☁ x`` = 10

[<Fact>]
let cloudVariableTest() =
    <@
        let output = defineList []
        whenGreenFlag {
            ``☁ x`` <- 0
            do! repeatAsync 3 {
                SList.push output ``☁ x``
            }
        }
    @>
    |> startAsStdSpriteWith id withIncrementCloud
    =? ["0"; "1"; "2"]
