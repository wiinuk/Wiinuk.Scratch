module Scratch.Evaluator.EvaluateState
open Scratch
open Scratch.Ast
open Scratch.Evaluator
open Scratch.Primitives
open Scratch.Threading


let private addOriginalSprites state =
    for sprite in IArray.toSeq state.originalSprites do
        state.objects.Add sprite
        StageView.added &state.view sprite

let private ofStageDataCore config data =
    let clouds =
        data.variables
        |> List.fold (fun xs v ->
            match v.isPersistent with
            | Persistent -> Set.add v.name xs
            | NoPersistent -> xs
        ) Set.empty

    let stage = ObjectState.ofEntityData (fun _ -> ValueNone) data
    let scheduler = Scheduler.make config.schedulerConfig
    let originalSprites =
        data
        |> StageData.sprites
        |> Seq.map (ObjectState.ofEntityData (SpriteState.ofSpriteData >> ValueSome))
        |> IArray.ofSeq

    let originalSpriteMap =
        originalSprites
        |> IArray.toSeq
        |> Seq.map (fun s -> s.shared.objectName, s)
        |> Map.ofSeq

    let objects = ResizeArray()
    let state = {
        EvaluateState.data = data
        config = config

        clouds = clouds
        scheduler = scheduler
        stage = stage
        sharedState = {
            answer = ""
            timerEpoch = ref <| Scheduler.now scheduler
            tempo = 60.
            currentAskVersion = Version 0
            nextAskVersion = Version 0
            showInputBox = false
        }
        objects = objects
        originalSprites = originalSprites
        originalSpriteMap = originalSpriteMap

        input = config.initialInput
        view = config.initialView
        cloud = config.initialCloud
    }
    state

let ofStageData config data =
    let data = StageData.unapplyScratch3ExecutionOrderTrick data
    Validators.validateStageData data
    let state = ofStageDataCore config data
    addOriginalSprites state
    state
