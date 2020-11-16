module Scratch.Executor.Executions
open System
open System.Runtime.CompilerServices
open Scratch
open Scratch.Primitives
open Scratch.Threading
open Scratch.Executor
open Scratch.Executor.Blocks

#nowarn "0069" // Interface implementations in augmentations are now deprecated.


type IRuntimeVersion =
    abstract DeleteListLine: env: ExecutionTask<'a,'Custom> byref * line: Value * list: Value ResizeArray -> unit
        when 'Custom :> ICustomization<'a>
        and 'Custom : struct

    abstract LetterOf: env: ExecutionTask<'a,'Custom> byref * target: string * nth: double -> string
        when 'Custom :> ICustomization<'a>
        and 'Custom : struct

and ICustomization<'a> =
    inherit IRandom
    inherit IExecutionObserver
    inherit IInput
    inherit IStageView<'a EntityState>
    inherit IRuntimeVersion
    inherit ICloud

and [<Struct>] ExecutionEnvironment<'a,'Custom>
    when 'Custom :> ICustomization<'a>
    and 'Custom : struct
    = {
    scheduler: Scheduler<unit, ExecutionTask<'a,'Custom>>
    mutable custom: 'Custom
}
and [<Struct>] ExecutionTask<'a,'Custom>
    when 'Custom :> ICustomization<'a>
    and 'Custom : struct
    = {
    mutable environment: ExecutionEnvironment<'a,'Custom>
    mutable executionState: 'a ThreadState
}
with
    interface task

module Expressions =
    open System.IO
    open Scratch.Ast
    open Scratch.Executor.Diagnostics

    let printStackMessage w (state: _ inref) =
        let showState = state.stage.showState
        let image = state.stage.image

        Printer.printFlame w showState image &state.currentFlame
        fprintfn w ""
        for i in state.flames.next-1..-1..Index.zero do
            Printer.printFlame w showState image &(state.flames.items.Address(i))
            fprintfn w ""

    let stackMessage (state: _ byref) =
        let b =
            use w = new StringWriter()
            printStackMessage (upcast w) &state
            w.GetStringBuilder()
        b.ToString()

    let executeFiberInstruction (state: _ byref) (fiber: 'F byref when 'F :> fiber<_,_,_>) (result: _ outref) =
        let mutable continueExecution = false
        while
            match Fiber.next &fiber &state with
            | Return() -> continueExecution <- true; false
            | Yield ThreadYield when state.currentFlame.isAtomicContext -> true
            | Yield _ as r ->

                // continue
                state.position <- state.position - 1
                result <- r
                continueExecution <- false
                false

            do ()
        continueExecution

    let executeCall (state: _ byref) (i: _ inref) =
        let calleeIndex = operandToIndex<Procedure> &i
        let data = &state.data
        let callerFlame = &state.currentFlame
        let callee = &state.stage.image.procedures.Address(calleeIndex)
        Stack.push &state.flames callerFlame
        state.currentFlame <- {
            returnAddress = state.position
            procedure = calleeIndex
            argumentsOrigin = data.next - callee.parameterCount
            localsOrigin = data.next
            isAtomicContext = callerFlame.isAtomicContext || callee.isAtomic
        }
        Stack.extendNoinit &data callee.localCount
        state.position <- callee.startAddress

    let executeReturn (state: _ byref) (result: _ outref) (next: _ outref) =

        // 実行終わり
        if state.flames.next.index = 0 then
            result <- Return()
            next <- false
        else
            let calleeFlame = state.currentFlame
            let callee = &state.stage.image.procedures.Address(calleeFlame.procedure)
            state.currentFlame <- Stack.pop &state.flames
            Stack.truncate &state.data (callee.parameterCount + callee.localCount)

            // データスタックに積まれた何個かの戻り値を、呼び出し元が使えるようにコピーする必要がある
            // しかし今のバージョンではすべてのプロシージャの戻り値の数は0なので、コピー操作は必要ない

            state.position <- calleeFlame.returnAddress

    let selfList (state: _ byref) (i: _ inref) =
        let variable = operandToIndex<Value ResizeArray> &i
        state.self.listVariables.Address(variable)

    let stageList (state: _ byref) (i: _ inref) =
        let variable = operandToIndex<Value ResizeArray> &i
        state.stage.stageState.listVariables.Address(variable)

    let executeCloudVariable (cloud: _ byref) (state: _ byref) (i: _ inref) =
        let name = state.stage.image.stringLiterals.Address(operandToIndex<string> &i)
        Stack.push &state.data (Value.ofSValue <| Cloud.get &cloud name)

    let executeSetCloudVariable (cloud: _ byref) (state: _ byref) (i: _ inref) =
        let name = state.stage.image.stringLiterals.Address(operandToIndex<string> &i)
        Cloud.set &cloud name (Value.toSValue <| Stack.pop &state.data)

    let executeSpriteListCount (state: _ byref) (i: _ inref) =
        let list = selfList &state &i
        Stack.push &state.data (Value.Number(double list.Count))

    let executeListLine (random: _ byref) (state: _ byref) list =
        let nth = Stack.pop &state.data
        let x = getLine &random nth list
        Stack.push &state.data x

    let executeSpriteListLine (random: _ byref) (state: _ byref) (i: _ inref) =
        executeListLine &random &state (selfList &state &i)

    let executeStageListLine (random: _ byref) (state: _ byref) (i: _ inref) =
        executeListLine &random &state (stageList &state &i)

    let executeDeleteListLine (task: _ byref) list =
        let nth = Stack.pop &task.executionState.data
        task.environment.custom.DeleteListLine(&task, nth, list)

    let executeDeleteSpriteListLine (env: _ byref) (i: _ inref) =
        executeDeleteListLine &env (selfList &env.executionState &i)

    let executeDeleteStageListLine (env: _ byref) (i: _ inref)=
        executeDeleteListLine &env (stageList &env.executionState &i)

    let executeAppendSpriteList (state: _ byref) (i: _ inref) =
        let list = selfList &state &i
        let value = Stack.pop &state.data
        list.Add value

    let executeSetListLine (random: _ byref) (state: _ byref) (list: _ ResizeArray) =
        let value = Stack.pop &state.data
        let nth = Stack.pop &state.data
        match listIndex &random nth list.Count with
        | ValueNone -> ()
        | ValueSome index -> list.[index] <- value

    let executeSetSpriteListLine (random: _ byref) (state: _ byref) (i: _ inref) =
        executeSetListLine &random &state (selfList &state &i)

    let executeSetStageListLine (random: _ byref) (state: _ byref) (i: _ inref) =
        executeSetListLine &random &state (stageList &state &i)

    let executeListContents (state: _ byref) list =
        Stack.push &state.data (Value.String(contentsOfList list))

    let executeSpriteListContents (state: _ byref) (i: _ inref) =
        executeListContents &state (selfList &state &i)

    let executeStageListContents (state: _ byref) (i: _ inref) =
        executeListContents &state (stageList &state &i)

    let executeListContains (state: _ byref) (list: _ ResizeArray) =
        let target = Stack.pop &state.data

        let mutable e = list.GetEnumerator()
        let mutable hasValue = false
        while
            if e.MoveNext() then
                if Value.equals e.Current target then
                    hasValue <- true
                    false
                else
                    true
            else
                false
            do ()
        Stack.push &state.data (Value.Bool hasValue)

    let executeSpriteListContains (state: _ byref) (i: _ inref) = executeListContains &state (selfList &state &i)
    let executeStageListContains (state: _ byref) (i: _ inref) = executeListContains &state (stageList &state &i)

    let executeInsertAtList (random: _ byref) (state: _ byref) (list: _ ResizeArray) =
        let target = Stack.pop &state.data
        let value = Stack.pop &state.data

        match listIndex &random target (list.Count + 1) with
        | ValueSome i -> list.Insert(i, value)
        | _ -> ()

        //if state.blockState.config.useRangeCheck then
        //    cfailwithf state location "out of range: insert:at:ofList:, '%s' '%A'" listName index

    let executeInsertAtSpriteList (random: _ byref) (state: _ byref) (i: _ inref) = executeInsertAtList &random &state (selfList &state &i)
    let executeInsertAtStageList (random: _ byref) (state: _ byref) (i: _ inref) = executeInsertAtList &random &state (stageList &state &i)

    let executeLetterOf (state: _ byref) =
        let data = &state.executionState.data
        let x2 = Stack.pop &data
        let x1 = Stack.pop &data
        Stack.push &data (Value.String(state.environment.custom.LetterOf(&state, Value.toString x2, Value.toNumber x1)))

    let executeRandomFromTo (random: _ byref) (state: _ byref) =
        let x2 = Stack.pop &state.data |> Value.toNumberZeroIfNaN
        let x1 = Stack.pop &state.data |> Value.toNumberZeroIfNaN
        let x1, x2 = if x2 < x1 then x2, x1 else x1, x2
        let r =
            if x1 = x2 then x1
            elif x1 % 1. = 0. && x2 % 1. = 0.
            then floor (Random.nextDouble &random * (x2 - x1 + 1.)) + x1
            else Random.nextDouble &random * (x2 - x1) + x1

        Stack.push &state.data (Value.Number r)

    let executeTimeAndDate scheduler (state: _ byref) =
        let kind = Stack.pop &state.data
        let now = Scheduler.now scheduler
        let value = timeAndDate now kind
        Stack.push &state.data (Value.Number(double value))

    let executeForward (state: _ byref) =
        let steps = Stack.pop &state.data |> Value.toNumberZeroIfNaN
        let data = &state.self.spriteDrawingData
        let d = (90. - data.direction) * Math.PI / 180.
        moveTo state.self (data.x + steps * cos d, data.y + steps * sin d)

    let executeDistanceTo (input: _ byref) (state: _ byref) =
        let target = Stack.pop &state.data
        Stack.push &state.data (Value.Number(distanceTo &input state.stage state.self target))

    let executePointTowards (input: _ byref) (state: _ byref) =
        let target = Stack.pop &state.data
        pointTowards &input state.stage state.self target

    let executeKeyPressed (input: _ byref) (state: _ byref) =
        let data = &state.data
        let key = Stack.pop &data
        let keyPressed =
            if key.StringOrDefault = "any" then
                Input.isAnyKeyDown &input
            else
                input.IsKeyDown(getKeyCode key)
        Stack.push &data (Value.Bool keyPressed)

    let executeGoto (state: _ byref) =
        let y = Stack.pop &state.data |> Value.toNumberZeroIfNaN
        let x = Stack.pop &state.data |> Value.toNumberZeroIfNaN
        moveTo state.self (x, y)

    let executeGotoSpriteOrMouse (input: _ byref) (random: _ byref) (state: _ byref) =
        let target = Stack.pop &state.data
        gotoObject &input &random state.stage state.self target

    let executeSayOrThink (state: _ byref) (i: _ inref) =
        let kind = if i.operand1 <> 0L then Say else Think
        let message = Stack.pop &state.data |> Value.toString
        let (Version v) = say state.self message kind
        Stack.push &state.data (Value.Number(double v))

    let executeClearSayOrThink (state: _ byref) =
        let version: SayPhantom Version = Stack.pop(&state.data).NumberOrDefault |> int32 |> Version
        clearSay state.self version

    let executeSetRotationStyle (state: _ byref) =
        let style = Stack.pop &state.data
        setRotationStyle state.self style

    let executeFilter (state: _ byref) =
        let name = Stack.pop &state.data
        Stack.push &state.data (Value.Number(getFilter state.self (Value.toString name)))

    let executeSetFilter (state: _ byref) =
        let value = Stack.pop &state.data
        let name = Stack.pop &state.data
        setFilter state.self (Value.toString name) (Value.toNumber value)

    let executeFilterWith (state: _ byref) (i: _ inref) =
        let filter = enum<Filter>(int32 i.operand1)
        Stack.push &state.data (Value.Number(getFilterWith state.self filter))

    let executeSetFilterWith (state: _ byref) (i: _ inref) =
        let filter = enum<Filter>(int32 i.operand1)
        let value = Stack.pop &state.data
        setFilterWith state.self filter (Value.toNumber value)

    let executeComeToFront (view: _ byref) (state: _ byref) =
        let instances = state.stage.instances
        let self = state.self

        // TODO: optimize
        let index = instances.IndexOf self
        instances.RemoveAt index
        instances.Add self

        StageView.moved &view self index (instances.Count - 1)

    let executeGoBackByLayers (view: _ byref) (state: _ byref) =
        let layerCount = Stack.pop &state.data |> Value.toNumber
        let instances = state.stage.instances
        let self = state.self

        // TODO: optimize
        let index = instances.IndexOf self
        let index' = max 0 (index - int layerCount)
        instances.RemoveAt index
        instances.Insert(index', self)

        StageView.moved &view self index index'

    let executePlaySound (view: _ byref) (state: _ byref) =
        let sound = Stack.pop &state.data
        match findSound (IArray.length state.self.entityImage.sounds) state.self.soundNameToIndex sound with
        | ValueNone -> ()
        | ValueSome index -> StageView.playSound &view state.self index

    let valueToArgb color =
        let c = uint32 (Value.toNumber color)
        let a = (c >>> 24) &&& 0xffu
        let r = (c >>> 16) &&& 0xffu
        let g = (c >>> 8) &&& 0xffu
        let b = (c >>> 0) &&& 0xffu
        let a = if a = 0u then 0xFFu else a
        struct(byte a, byte r, byte g, byte b)

    let executeSetPenColor (view: _ byref) (state: _ byref) =
        let argb = Stack.pop &state.data |> valueToArgb
        state.self.spriteDrawingData.penArgb <- argb
        StageView.setPenArgb &view state.self argb

    let executeChangePenHue (view: _ byref) (state: _ byref) (i: _ inref) =
        let hue = Stack.pop &state.data |> Value.toNumber
        let oldValueRatio = double (int32 i.operand1)

        let struct(a, h, _, l) = Color.rgb2hsl state.self.spriteDrawingData.penArgb
        let argb = Color.hsl2rgb struct(a, (h * oldValueRatio) + (hue * 360. / 200.), 100., l)
        state.self.spriteDrawingData.penArgb <- argb
        StageView.setPenArgb &view state.self argb

    let executeChangePenShade (view: _ byref) (state: _ byref) (i: _ inref) =
        let lightness = Stack.pop &state.data |> Value.toNumber
        let oldValueRatio = double (int32 i.operand1)

        let struct(a, h, _, l) = Color.rgb2hsl state.self.spriteDrawingData.penArgb
        let lightness = (l * oldValueRatio + lightness) % 200.

        // 0..<200
        let lightness = if lightness < 0. then lightness + 200. else lightness

        let argb = Color.hsl2rgb struct(a, h, 100., lightness)
        state.self.spriteDrawingData.penArgb <- argb
        StageView.setPenArgb &view state.self argb

    let executeChangePenSize (view: _ byref) (state: _ byref) (i: _ inref) =
        let extend = Stack.pop &state.data |> Value.toNumber
        let oldValueRatio = double (int32 i.operand1)

        let penSize = &state.self.spriteDrawingData.penSize
        let size = max 1. (oldValueRatio * penSize + extend)
        penSize <- size
        StageView.penSize &view state.self size

    let executeTouchingColor (view: _ byref) (state: _ byref) =
        let argb = Stack.pop &state.data |> valueToArgb
        let isTouching = StageView.queryTouchingColor &view state.self argb
        Stack.push &state.data (Value.Bool isTouching)

    let executeShowVariable (view: _ byref) (state: _ byref) (i: _ inref) =
        let isShow = Stack.pop &state.data |> Value.toBool
        let variableName = state.stage.image.stringLiterals.Address(operandToIndex<string> &i)
        StageView.variableMonitorChanged &view state.self variableName (if isShow then Visible else Hidden)

let scheduleProcedureThread (env: _ inref) stage self procedure =

    // TODO: configuration
    let initialDataStackLength = 1024
    let initialCallStackLength = 256

    let p = &stage.image.procedures.Address(procedure)
    let initialState = {
        stage = stage
        self = self
        position = p.startAddress
        data = Stack.create initialDataStackLength
        flames = Stack.create initialCallStackLength
        childThreadIds = null
        glideSecs = GlideSecsFiber.make env.scheduler
        ask = AskFiber.make &env.custom
        currentFlame = {
            returnAddress = Index.zero
            procedure = procedure
            argumentsOrigin = Index.zero
            localsOrigin = Index.zero
            isAtomicContext = false
        }
    }
    let task = {
        executionState = initialState
        environment = env
    }
    Scheduler.registerFiber "" &task env.scheduler

open Expressions

let scheduleProcedureThreads (env: _ inref) stage self procedures =
    for index in IArray.toSeqCopiable procedures do
        ignore <| scheduleProcedureThread &env stage self index

let registerEntiryWhenGreenFlags (env: _ inref) stage self =
    for index in self.entityImage.whenGreenFlags do
        ignore <| scheduleProcedureThread &env stage self index

let registerWhenGreenFlags (env: _ inref) stage =
    registerEntiryWhenGreenFlags &env stage stage.stageState
    for sprite in stage.instances do
        registerEntiryWhenGreenFlags &env stage sprite

let broadcastEntity (env: _ inref) stage self broadcastIndex =
    match IndexMap.tryFind broadcastIndex self.entityImage.broadcastListeners with
    | ValueNone -> ()
    | ValueSome listeners ->

    for index in listeners do
        ignore <| scheduleProcedureThread &env stage self index

let executeBroadcast (env: _ inref) stage (i: _ inref) =
    let broadcastIndex = operandToIndex<Broadcast> &i
    broadcastEntity &env stage stage.stageState broadcastIndex
    for sprite in stage.instances do
        broadcastEntity &env stage sprite broadcastIndex

let broadcastAndWaitObject (env: _ inref) stage self broadcastIndex (result: _ ResizeArray) =
    match IndexMap.tryFind broadcastIndex self.entityImage.broadcastListeners with
    | ValueNone -> ()
    | ValueSome listeners ->
        for index in listeners do
            let t = scheduleProcedureThread &env stage self index
            result.Add t.id

let executeBroadcastAndWait (task: _ byref) (i: _ inref) (result: _ outref) =
    let childThreadIds =
        match task.executionState.childThreadIds with
        | null ->
            let xs = ResizeArray()
            task.executionState.childThreadIds <- xs
            xs
        | xs -> xs

    if childThreadIds.Count = 0 then
        // start child threads

        let stage = task.executionState.stage
        let index = operandToIndex<Broadcast> &i
        broadcastAndWaitObject &task.environment stage stage.stageState index childThreadIds
        for self in stage.instances do
            broadcastAndWaitObject &task.environment stage self index childThreadIds

        if childThreadIds.Count = 0 then
            // nowait
            true
        else
            // wait loop
            task.executionState.position <- task.executionState.position - 1
            result <- Yield ThreadYieldForce
            false
    else
        // waiting
        let state = &task.executionState
        let threads = task.environment.scheduler.threads
        if threads |> Colony.exists (fun t -> childThreadIds.Contains t.id) then
            // wait loop
            state.position <- state.position - 1
            result <- Yield ThreadYieldForce
            false
        else
            // end wait
            childThreadIds.Clear()
            true

let executeCreateCloneOfSprite (task: _ byref) parent =
    let state = &task.executionState
    let stage = state.stage
    let c = cloneSprite parent
    let instances = stage.instances
    let index = instances.IndexOf parent
    instances.Insert(index, c)
    StageView.cloned &task.environment.custom parent c index

    for listenerIndex in parent.entityImage.whenCloned do
        scheduleProcedureThread &task.environment stage c listenerIndex |> ignore

let executeCreateCloneOf (task: _ byref) =
    let state = &task.executionState
    let name = Stack.pop &state.data |> Value.toString

    let parent =
        match name with
        | "_myself_" -> ValueSome state.self
        | _ -> findEntity state.stage name

    match parent with
    | ValueNone
    | ValueSome { entityImage = { spriteImage = None } } -> ()
    | ValueSome parent -> executeCreateCloneOfSprite &task parent

let executeDeleteClone (task: _ byref) (result: _ outref) =
    let state = &task.executionState
    if state.self.isClone then
        let self = state.self
        let instances = state.stage.instances
        let index = instances.IndexOf self
        instances.RemoveAt index

        task.environment.scheduler.threads
        |> Colony.ignoreWith (fun thread -> LanguagePrimitives.PhysicalEquality self thread.runningTask.executionState.self)

        StageView.removed &task.environment.custom self index
        result <- Yield Abort
        false
    else
        true

let executeStopScripts (thread: _ inref) scheduler (state: _ byref) (result: _ outref) =
    let kind = Stack.pop &state.data |> Value.toString
    match kind with
    | "all" ->
        printStackMessage stderr &state

        // disable all threads
        scheduler.threads
        |> Colony.ignoreWith (fun _ -> true)

        result <- Yield Abort
        false

    | "this script" ->
        result <- Yield Abort
        false

    | "other scripts in sprite"
    | "other scripts in stage" ->
        let self = state.self
        let { ThreadInfo.id = threadId } = thread
        scheduler.threads
        |> Colony.ignoreWith (fun t -> self = t.runningTask.executionState.self && t.id <> threadId)
        true

    | _ -> true


[<MethodImpl(MethodImplOptions.NoInlining)>]
let private raiseNotImplementedCode (i: _ inref) = raise <| NotImplementedException(sprintf "%A" i.code)

[<NoGcAllocation(AllocatableTypes = [|typeof<exn>|])>]
let execute (task: _ byref) (threadInfo: _ inref) =
    let state = &task.executionState
    let data = &state.data
    let is = state.stage.image.instructions
    let p = &state.position

    task.environment.custom.OnEnterExecute(&state)

    //                          currentFlame.localsOrigin
    //                          |
    //                          *
    // ..., arguments...,       locals...,      stack...
    //      <- parameterCount -><- localCount -><- maxStack                       ->
    //
    //                          flames.[0].localsOrigin                     currentFlame.localsOrigin
    //                          |                                           |
    //                          *                                           *
    // ..., arguments...,       locals...,      stack...arguments...,       locals...,       stack...
    //      <- parameterCount -><- localCount -><- maxStack                       ->
    //                                                   <- parameterCount -><- localCount -><- maxStack      ->
    //
    //                          currentFlame.localsOrigin
    //                          |
    //                          *
    // ..., arguments...,       locals...,      stack...
    //      <- parameterCount -><- localCount -><- maxStack            ->
    let mutable next = true
    let mutable result = Return()
    while next do
        task.environment.custom.OnPreExecute &state

        let i = is.Address(p)
        p <- p + 1

        match i.code with
        | Code.Nop -> ()
        | Code.Number -> Stack.push &data (Value.Number(BitConverter.Int64BitsToDouble i.operand1))

        | Code.String ->
            let stringIndex = operandToIndex<string> &i
            let value = state.stage.image.stringLiterals.Address stringIndex
            Stack.push &data (Value.String value)

        | Code.Bool -> Stack.push &data (Value.Bool(i.operand1 <> 0L))

        | Code.Call -> executeCall &state &i
        | Code.Return -> executeReturn &state &result &next
        | Code.Yield ->
            if state.currentFlame.isAtomicContext then
                ()
            else
                result <- Yield ThreadYield
                next <- false

        | Code.YieldForce ->
            result <- Yield ThreadYieldForce
            next <- false

        | Code.YieldSleep ->
            let seconds = Stack.pop &data |> Value.toNumberZeroIfNaN
            result <- Yield(Sleep(TimeSpan.FromSeconds seconds))
            next <- false

        | Code.Forward -> executeForward &state
        | Code.DistanceTo -> executeDistanceTo &task.environment.custom &state
        | Code.PointTowards -> executePointTowards &task.environment.custom &state
        | Code.Goto -> executeGoto &state
        | Code.GotoSpriteOrMouse -> executeGotoSpriteOrMouse &task.environment.custom &task.environment.custom &state
        | Code.NextCostume -> showNextCostume state.self
        | Code.SayOrThink -> executeSayOrThink &state &i
        | Code.ClearSayOrThink -> executeClearSayOrThink &state
        | Code.Filter -> executeFilter &state
        | Code.SetFilter -> executeSetFilter &state
        | Code.FilterWith -> executeFilterWith &state &i
        | Code.SetFilterWith -> executeSetFilterWith &state &i
        | Code.SetFilterBrightness ->
            let value = Stack.pop &state.data
            state.self.drawingData.filters.brightness <- Value.toNumber value

        | Code.SetFilterColor ->
            let value = Value.toNumber (Stack.pop &state.data)
            let v = value % 200.
            let v = if v < 0. then v + 200. else v
            state.self.drawingData.filters.color <- clamp (0., 200.) v

        | Code.SetFilterGhost ->
            let value = Value.toNumber (Stack.pop &state.data)
            state.self.drawingData.filters.ghost <- clamp (0., 100.) value

        | Code.ResetAllFilter ->
            let f = &state.self.drawingData.filters
            f.brightness <- 0.
            f.pixelate <- 0.
            f.fisheye <- 0.
            f.mosaic <- 0.
            f.whirl <- 0.
            f.color <- 0.
            f.ghost <- 0.
            f.others <- Map.empty

        | Code.Show -> state.self.spriteDrawingData.visible <- true
        | Code.Hide -> state.self.spriteDrawingData.visible <- false
        | Code.ComeToFront -> executeComeToFront &task.environment.custom &state
        | Code.GoBackByLayers -> executeGoBackByLayers &task.environment.custom &state
        | Code.PlaySound -> executePlaySound &task.environment.custom &state
        | Code.PlaySoundWith ->
            let index = int32 i.operand1
            task.environment.custom.PlaySound(state.self, index)

        | Code.ClearPenTrails -> task.environment.custom.ClearCanvas()
        | Code.SetPenDown ->
            let isDown = i.operand1 <> 0L
            state.self.spriteDrawingData.penDown <- isDown

        | Code.SetPenColor -> executeSetPenColor &task.environment.custom &state
        | Code.ChangePenHue -> executeChangePenHue &task.environment.custom &state &i
        | Code.ChangePenShade -> executeChangePenShade &task.environment.custom &state &i
        | Code.ChangePenSize -> executeChangePenSize &task.environment.custom &state &i
        | Code.StampCostume -> task.environment.custom.StampToCanvas state.self
        | Code.GlideSecs -> next <- executeFiberInstruction &state (let mutable f = state.glideSecs in &f) &result

        | Code.CreateCloneOf -> executeCreateCloneOf &task
        | Code.CreateCloneOfMyself -> executeCreateCloneOfSprite &task state.self
        | Code.DeleteClone -> next <- executeDeleteClone &task &result
        | Code.Ask -> next <- executeFiberInstruction &state (let mutable f = state.ask in &f) &result

        | Code.SpriteVariable ->
            let variable = operandToIndex<Value> &i
            let v = state.self.scalarVariables.Address(variable)
            Stack.push &data v
        | Code.StageVariable ->
            let variable = operandToIndex<Value> &i
            let v = state.stage.stageState.scalarVariables.Address(variable)
            Stack.push &data v
        | Code.CloudVariable -> executeCloudVariable &task.environment.custom &state &i

        | Code.SetSpriteVariable ->
            let variable = operandToIndex<Value> &i
            let v = Stack.pop &data
            state.self.scalarVariables.Address(variable) <- v
        | Code.SetStageVariable ->
            let variable = operandToIndex<Value> &i
            let v = Stack.pop &data
            state.stage.stageState.scalarVariables.Address(variable) <- v
        | Code.SetCloudVariable -> executeSetCloudVariable &task.environment.custom &state &i

        | Code.SpriteListCount -> executeSpriteListCount &state &i
        | Code.StageListCount ->
            let variable = operandToIndex<Value ResizeArray> &i
            let list = state.stage.stageState.listVariables.Address(variable)
            Stack.push &data (Value.Number(double list.Count))

        | Code.SpriteListLine -> executeSpriteListLine &task.environment.custom &state &i
        | Code.StageListLine -> executeStageListLine &task.environment.custom &state &i

        | Code.DeleteSpriteListLine -> executeDeleteSpriteListLine &task &i
        | Code.DeleteStageListLine -> executeDeleteStageListLine &task &i

        | Code.AppendSpriteList -> executeAppendSpriteList &state &i
        | Code.AppendStageList ->
            let variable = operandToIndex<Value ResizeArray> &i
            let value = Stack.pop &data
            let list = state.stage.stageState.listVariables.Address(variable)
            list.Add value

        | Code.SetSpriteListLine -> executeSetSpriteListLine &task.environment.custom &state &i
        | Code.SetStageListLine -> executeSetStageListLine &task.environment.custom &state &i

        | Code.SpriteListContents -> executeSpriteListContents &state &i
        | Code.StageListContents -> executeStageListContents &state &i

        | Code.SpriteListContains -> executeSpriteListContains &state &i
        | Code.StageListContains -> executeStageListContains &state &i

        | Code.InsertAtSpriteList -> executeInsertAtSpriteList &task.environment.custom &state &i
        | Code.InsertAtStageList -> executeInsertAtStageList &task.environment.custom &state &i

        | Code.Broadcast -> executeBroadcast &task.environment state.stage &i
        | Code.BroadcastAndWait -> next <- executeBroadcastAndWait &task &i &result
        | Code.StopScripts -> next <- executeStopScripts &threadInfo task.environment.scheduler &state &result

        | Code.BrunchIfFalsy ->
            let test = Stack.pop &data
            if not (Value.toBool test) then
                let offset = int32 i.operand1
                state.position <- state.position + offset

        | Code.Brunch ->
            let offset = int32 i.operand1
            state.position <- state.position + offset

        | Code.SetLocal ->
            let localIndex = operandToIndex<Value> &i
            let v = Stack.pop &data
            Stack.address &data (state.currentFlame.localsOrigin .+. localIndex) <- v

        | Code.Local ->
            let localIndex = operandToIndex<Value> &i
            let v = Stack.address &data (state.currentFlame.localsOrigin .+. localIndex)
            Stack.push &data v

        | Code.Argument ->
            let argumentIndex = operandToIndex<Value> &i
            let v = Stack.address &data (state.currentFlame.argumentsOrigin .+. argumentIndex)
            Stack.push &data v

        | Code.Pop -> ignore <| Stack.pop &data
        | Code.Dup ->
            let v = Stack.pop &data
            Stack.push &data v
            Stack.push &data v

        | Code.Add ->
            let b = Stack.pop &data
            let a = Stack.pop &data
            Stack.push &data (Value.Number(Value.toNumberZeroIfNaN a + Value.toNumberZeroIfNaN b))

        | Code.Sub ->
            let b = Stack.pop &data
            let a = Stack.pop &data
            Stack.push &data (Value.Number(Value.toNumberZeroIfNaN a - Value.toNumberZeroIfNaN b))

        | Code.Mul ->
            let b = Stack.pop &data
            let a = Stack.pop &data
            Stack.push &data (Value.Number(Value.toNumberZeroIfNaN a * Value.toNumberZeroIfNaN b))

        | Code.Div ->
            let b = Stack.pop &data
            let a = Stack.pop &data
            Stack.push &data (Value.Number(Value.toNumberZeroIfNaN a / Value.toNumberZeroIfNaN b))

        | Code.Mod ->
            let b = Stack.pop &data
            let a = Stack.pop &data
            let a = Value.toNumberZeroIfNaN a
            let b = Value.toNumberZeroIfNaN b
            let r = a % b
            let r = if r / b < 0. then r + b else r
            Stack.push &data (Value.Number r)

        | Code.Concat ->
            let b = Stack.pop &data
            let a = Stack.pop &data
            Stack.push &data (Value.String(Value.toString a + Value.toString b))

        | Code.Equal ->
            let b = Stack.pop &data
            let a = Stack.pop &data
            Stack.push &data (Value.Bool(Value.equals a b))

        | Code.EqualNumberLiteral ->
            let n = BitConverter.Int64BitsToDouble i.operand1
            let v = Stack.pop &data
            Stack.push &data (Value.Bool(Value.equalsToNumber v n))

        | Code.LessThan ->
            let b = Stack.pop &data
            let a = Stack.pop &data
            Stack.push &data (Value.Bool(Value.compare a b < 0))

        | Code.GreaterThan ->
            let b = Stack.pop &data
            let a = Stack.pop &data
            Stack.push &data (Value.Bool(Value.compare a b > 0))

        | Code.Not ->
            let x = Stack.pop &data
            Stack.push &data (Value.Bool(not (Value.toBool x)))

        | Code.LetterOf -> executeLetterOf &task
        | Code.RandomFromTo -> executeRandomFromTo &task.environment.custom &state
        | Code.Abs ->
            let v = Value.toNumberZeroIfNaN (Stack.pop &data)
            Stack.push &data (Value.Number(abs v))

        | Code.Sqrt ->
            let v = Value.toNumberZeroIfNaN (Stack.pop &data)
            Stack.push &data (Value.Number(sqrt v))

        | Code.StringLength ->
            let v = Value.toString(Stack.pop &data)
            Stack.push &data (Value.Number(double v.Length))

        | Code.Round ->
            let x = Value.toNumberZeroIfNaN (Stack.pop &data)
            Stack.push &data (Value.Number(Math.Round(x, MidpointRounding.AwayFromZero)))

        | Code.Floor ->
            let x = Value.toNumberZeroIfNaN (Stack.pop &data)
            Stack.push &data (Value.Number(floor x))

        | Code.Ceil ->
            let x = Value.toNumberZeroIfNaN (Stack.pop &data)
            Stack.push &data (Value.Number(ceil x))

        | Code.Cos ->
            let x = Value.toNumberZeroIfNaN (Stack.pop &data)
            let x = cos (x * Math.PI / 180.)
            Stack.push &data (Value.Number x)

        | Code.Sin ->
            let x = Value.toNumberZeroIfNaN (Stack.pop &data)
            let x = sin (x * Math.PI / 180.)
            Stack.push &data (Value.Number x)

        | Code.Tan ->
            let x = Value.toNumberZeroIfNaN (Stack.pop &data)
            let x = tan (x * Math.PI / 180.)
            Stack.push &data (Value.Number x)

        | Code.Asin ->
            let x = Value.toNumberZeroIfNaN (Stack.pop &data)
            let x = asin x * 180. / Math.PI
            Stack.push &data (Value.Number x)

        | Code.Acos ->
            let x = Value.toNumberZeroIfNaN (Stack.pop &data)
            let x = acos x * 180. / Math.PI
            Stack.push &data (Value.Number x)

        | Code.Atan ->
            let x = Value.toNumberZeroIfNaN (Stack.pop &data)
            let x = atan x * 180. / Math.PI
            Stack.push &data (Value.Number x)

        | Code.LogE ->
            let x = Value.toNumberZeroIfNaN (Stack.pop &data)
            let x = log x
            Stack.push &data (Value.Number x)

        | Code.Log10 ->
            let x = Value.toNumberZeroIfNaN (Stack.pop &data)
            let x = log10 x
            Stack.push &data (Value.Number x)

        | Code.ExpE ->
            let x = Value.toNumberZeroIfNaN (Stack.pop &data)
            let x = exp x
            Stack.push &data (Value.Number x)

        | Code.Exp10 ->
            let x = Value.toNumberZeroIfNaN (Stack.pop &data)
            let x = exp (x * log 10.)
            Stack.push &data (Value.Number x)

        | Code.CostumeName -> Stack.push &data (Value.String(getCostumeName state.self))
        | Code.StageCostumeName -> Stack.push &data (Value.String(getCostumeName state.stage.stageState))
        | Code.CostumeIndex -> Stack.push &data (Value.Number(double(state.self.drawingData.currentCostumeIndex + 1)))
        | Code.StageCostumeIndex -> Stack.push &data (Value.Number(double(state.stage.stageState.drawingData.currentCostumeIndex + 1)))
        | Code.SetCostume ->
            let costume = Stack.pop &data
            setCostume state.self costume

        | Code.ChangeCostumeWith ->
            let addIndex = int32 i.operand1
            changeCostumeOfCurrentIndex state.self addIndex

        | Code.SetCostumeWith -> state.self.drawingData.currentCostumeIndex <- int32 i.operand1

        | Code.X -> Stack.push &data (Value.Number(state.self.spriteDrawingData.x))
        | Code.Y -> Stack.push &data (Value.Number(state.self.spriteDrawingData.y))
        | Code.Direction -> Stack.push &data (Value.Number(state.self.spriteDrawingData.direction))
        | Code.SetDirection ->
            let degrees = Value.toNumberZeroIfNaN(Stack.pop &data)
            let d = degrees % 360.
            let d = if d > 180. then d - 360. else d
            let d = if d <= -180. then d + 360. else d
            state.self.spriteDrawingData.direction <- d

        // TODO:
        //match state.self.sprite with
        //| ValueSome sprite -> SNumber(sprite.spriteDrawingData.scale * 100.)
        //| ValueNone _ -> SNumber 100.
        | Code.Scale -> Stack.push &data (Value.Number(state.self.spriteDrawingData.scale * 100.))

        | Code.SetScale ->
            let size = Stack.pop &data |> Value.toNumberZeroIfNaN
            let scale = max 0. (size * 0.01)
            state.self.spriteDrawingData.scale <- scale

        | Code.Volume -> Stack.push &data (Value.Number(state.self.drawingData.volume))
        | Code.SetVolume ->
            let value = Stack.pop &data |> Value.toNumber
            state.self.drawingData.volume <- min 1. (max 0. (value * 0.01))

        | Code.Tempo -> Stack.push &data (Value.Number state.stage.tempo)
        | Code.SetTempo ->
            let value = Stack.pop &data |> Value.toNumber
            state.stage.tempo <- value

        | Code.SetRotationStyle -> executeSetRotationStyle &state


        | Code.Answer -> Stack.push &data (Value.String state.stage.answer)
        | Code.UserId -> Stack.push &data (Value.Number (double state.stage.userId))
        | Code.UserName -> Stack.push &data (Value.String state.stage.userName)

        | Code.MouseX ->
            let struct(x, _) = Input.getMousePosition &task.environment.custom
            Stack.push &data (Value.Number x)
        | Code.MouseY ->
            let struct(x, _) = task.environment.custom.GetMousePosition()
            Stack.push &data (Value.Number x)

        | Code.Timer ->
            let now = Scheduler.now task.environment.scheduler
            let span = now - state.stage.timerEpoch
            Stack.push &data (Value.Number span.TotalSeconds)
        | Code.TimerReset ->
            state.stage.timerEpoch <- Scheduler.now task.environment.scheduler

        | Code.Timestamp ->
            let span = Scheduler.now task.environment.scheduler - timeStampEpoch
            Stack.push &data (Value.Number span.TotalDays)

        | Code.TimeAndDate -> executeTimeAndDate task.environment.scheduler &state
        | Code.AnyMousePressed -> Stack.push &data (Value.Bool(task.environment.custom.IsAnyMouseButtonPressed()))
        | Code.KeyPressed -> executeKeyPressed &task.environment.custom &state
        | Code.AnyKeyPressed -> Stack.push &data (Value.Bool(task.environment.custom.IsAnyKeyDown()))
        | Code.KeyPressedWithKeyCode ->
            let keyCode = enum<KeyCode> (int32 i.operand1)
            Stack.push &data (Value.Bool(task.environment.custom.IsKeyDown keyCode))

        | Code.TouchingColor -> executeTouchingColor &task.environment.custom &state

        | Code.ShowVariable -> executeShowVariable &task.environment.custom &state &i

        //| _ -> raiseNotImplementedCode &i
#if DEBUG
    match result with
    | Return _ ->
        if not (Stack.isEmpty &state.flames) then
            failwithf "error %A" state.flames
        if not (Stack.isEmpty &state.data) then
            failwithf "error %A" state.data
    | _ -> ()
#endif
    task.environment.custom.OnLeaveExecute(&state, &result)
    result

type ExecutionTask<'a,'Custom>
    when 'Custom :> ICustomization<'a>
    and 'Custom : struct
with
    interface task with
        member t.Next struct(e, _) = execute &t &e
