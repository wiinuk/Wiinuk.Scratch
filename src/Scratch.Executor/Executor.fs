module Scratch.Executor.Executor
open System.Collections.Generic
open Scratch
open Scratch.Ast
open Scratch.Primitives
open Scratch.Threading
open Scratch.Executor.Executions


let toNameToIndexMap name xs =
    xs
    |> IArray.toSeq
    |> IArray.fold (fun struct(map, index) x ->
        struct(Map.add (name x) index map, index + 1)
    ) struct(Map.empty, 0)
    |> ValueTuple.fst

let makeEntityState image = {
    isClone = false
    entityImage = image
    costumeNameToIndex = image.costumes |> toNameToIndexMap (fun x -> x.costumeName)
    soundNameToIndex = image.sounds |> toNameToIndexMap (fun x -> x.soundName)

    scalarVariables =
        image.scalarVariables
        |> IArray.mapToArray (fun v -> v.value |> Value.ofSValue)

    listVariables =
        image.listVariables
        |> IArray.mapToArray (fun v ->
            v.contents'
            |> IArray.toSeqCopiable
            |> Seq.map Value.ofSValue
            |> ResizeArray
        )

    drawingData = {
        currentCostumeIndex =
            match image.currentCostumeIndex with
            | None -> 0
            | Some i -> int i

        filters = {
            whirl = 0.
            fisheye = 0.
            brightness = 0.
            pixelate = 0.
            mosaic = 0.
            color = 0.
            ghost = 0.
            others = Map.empty
        }
        volume = 100.
    }
    spriteDrawingData = 
        match image.spriteImage with
        | None -> SpriteDrawingData.initialData()
        | Some image ->
            { SpriteDrawingData.initialData() with
                visible = match image.visible with Visible -> true | Hidden -> false
                x = image.scratchX
                y = image.scratchY
                direction = image.direction
                scale = image.scale
                rotationStyle = image.rotationStyle
            }
    sayVersion = Version 0
}

type ExecutionState<'a,'Custom>
    when 'Custom :> ICustomization<'a>
    and 'Custom : struct = {
    mutable env: ExecutionEnvironment<'a,'Custom>
    mainStage: StageState<'a>
}

let addInputListeners state =
    let mutable env = state.env
    let stage = state.mainStage

    let input: #IInput byref = &env.custom
    let l = Func.ofFun <| fun i ->
        let env = env
        let self =
            match i with
            | ValueSome index -> stage.instances.[index]
            | ValueNone -> stage.stageState

        scheduleProcedureThreads &env stage self self.entityImage.whenClicked
        HUnit

    input.AddMouseClicked(&l)

let addOriginalSprites state =
    let { image = image; instances = instances; originalSpriteMap = originalSpriteMap } = state.mainStage
    for spriteImage in IArray.toSeq image.spriteImages do
        let sprite = makeEntityState spriteImage
        instances.Add sprite
        StageView.added &state.env.custom sprite

        // old sprites is replaced
        originalSpriteMap.Add(spriteImage.entityName, sprite)

let registerWhenGreenFlags state =
    registerWhenGreenFlags &state.env state.mainStage

[<Struct; RequireQualifiedAccess; NoEquality; NoComparison>]
type AggregateCustom<'a,'Random,'Observer,'Input,'View,'Cloud,'Version>
    when
        'Random :> IRandom and
        'Random : struct and
        'Observer :> IExecutionObserver and
        'Observer : struct and
        'Input :> IInput and
        'Input : struct and
        'View :> IStageView<'a EntityState> and
        'View : struct and
        'Cloud :> ICloud and
        'Cloud : struct and
        'Version :> IRuntimeVersion and
        'Version : struct
    = private {
    mutable random: 'Random
    mutable observer: 'Observer
    mutable input: 'Input
    mutable view: 'View
    mutable cloud: 'Cloud
    mutable version: 'Version
}
with
    interface ICustomization<'a>

    interface IRandom with
        member this.Next(low, high) = this.random.Next(low, high)
        member this.NextDouble() = this.random.NextDouble()

    interface IInput with
        member this.AddMouseClicked(listener) = this.input.AddMouseClicked(&listener)
        member this.GetMousePosition() = this.input.GetMousePosition()
        member this.IsAnyKeyDown() = this.input.IsAnyKeyDown()
        member this.IsAnyMouseButtonPressed() = this.input.IsAnyMouseButtonPressed()
        member this.IsKeyDown(code) = this.input.IsKeyDown(code)

    interface IStageView<'a EntityState> with
        member this.Added(sprite) = this.view.Added(sprite)
        member this.ClearCanvas() = this.view.ClearCanvas()
        member this.Cloned(prototypeSprite, clonedSprite, insertedIndex) = this.view.Cloned(prototypeSprite, clonedSprite, insertedIndex)
        member this.HideStageQuestion() = this.view.HideStageQuestion()
        member this.Moved(sprite, oldIndex, newIndex) = this.view.Moved(sprite, oldIndex, newIndex)
        member this.PenDown(sprite, isDown) = this.view.PenDown(sprite, isDown)
        member this.PenSize(sprite, size) = this.view.PenSize(sprite, size)
        member this.PlaySound(sprite, index) = this.view.PlaySound(sprite, index)
        member this.QueryTouchingColor(selfSprite, argb) = this.view.QueryTouchingColor(selfSprite, argb)
        member this.Removed(cloneSprite, index) = this.view.Removed(cloneSprite, index)
        member this.SetPenArgb(sprite, argb) = this.view.SetPenArgb(sprite, argb)
        member this.ShowInputBox(initialValue, onSubmit) = this.view.ShowInputBox(initialValue, &onSubmit)
        member this.ShowStageQuestion(question) = this.view.ShowStageQuestion(question)
        member this.StampToCanvas(entity) = this.view.StampToCanvas(entity)
        member this.VariableMonitorChanged(selfSprite, variableName, visibility) = this.view.VariableMonitorChanged(selfSprite, variableName, visibility)

    interface ICloud with
        member this.Set(n, v) = this.cloud.Set(n, v)
        member this.Get n = this.cloud.Get n

    interface IRuntimeVersion with
        member this.LetterOf(env, target, nth) = this.version.LetterOf(&env, target, nth)
        member this.DeleteListLine(env, line, list) = this.version.DeleteListLine(&env, line, list)

    interface IExecutionObserver with
        member this.OnEnterExecute(state) = this.observer.OnEnterExecute(&state)
        member this.OnLeaveExecute(state, result) = this.observer.OnLeaveExecute(&state, &result)
        member this.OnPreExecute(state) = this.observer.OnPreExecute(&state)

let makeEmptyStageState withConfig scheduler image =
    let config = withConfig <| ExecutionConfig.makeDefaultConfig 0xCAFEBABE
    let stageState = makeEntityState image.stageImage
    let instances = ResizeArray(capacity = IArray.length image.spriteImages)
    let originalSpriteMap = Dictionary(capacity = IArray.length image.spriteImages)

    let stage = {
        image = image
        stageState = stageState
        originalSpriteMap = originalSpriteMap
        instances = instances

        showState = config.showLocation

        timerEpoch = Scheduler.now scheduler
        answer = ""
        tempo = 60.
        currentAskVersion = Version 0
        nextAskVersion = Version 0
        showInputBox = false

        userId = config.userId
        userName = config.userName
    }
    {
        env = {
            scheduler = scheduler
            custom = {
                AggregateCustom.random = config.random
                AggregateCustom.observer = config.observer
                AggregateCustom.input = config.input
                AggregateCustom.view = config.view
                AggregateCustom.cloud = config.cloud
                AggregateCustom.version = config.version
            }
        }
        mainStage = stage
    }

let makeStageState withConfig scheduler image =
    let state = makeEmptyStageState withConfig scheduler image
    addInputListeners state
    addOriginalSprites state
    registerWhenGreenFlags state
    state

let runImageWith withConfig image =
    let scheduler = Scheduler.make SchedulerConfig.defaultConfig
    let state = makeStageState withConfig scheduler image
    Scheduler.run scheduler
    state

let runImage image = runImageWith id image
