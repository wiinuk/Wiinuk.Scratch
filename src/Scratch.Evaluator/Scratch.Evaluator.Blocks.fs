module Scratch.Evaluator.Blocks
open System
open Scratch
open Scratch.Primitives
open Scratch.SValue
open Scratch.Ast
open Scratch.Threading
open Scratch.Evaluator
open Scratch.Evaluator.Expressions
open Scratch.Evaluator.ObjectState
open FiberBuilderDelay


let screenWidth = 480.
let screenHeight = 360.

let listenerName config self (ListenerDefinition(state = location; name = listenerName; arguments = listenerArgs)) =
    $"%s{config.showState location}%s{self.shared.objectName}.%s{Symbol.name listenerName}{listenerArgs}"

let checkArgs (state: _ inref) location (ProcedureDefinition(name = procName; parameters = ps)) args =
    let el = List.length ps
    let al = List.length args
    if el <> al then
        cfailwithf state location $"call %s{procName}; diffarence parameter length; expected: {el}, actual: {al}"

    let state = state
    List.zip ps args
    |> List.fold (fun xs (ParameterDefinition(name = n), v) ->
        if Map.containsKey n xs then
            cfailwithf state location $"duplicate parameter name '%s{n}'"
        Map.add n v xs
    ) Map.empty

let setFilter state location name value =
    let filters = &state.self.drawingData.filters
    match name with
    | "whirl" -> filters.whirl <- value
    | "fisheye" -> filters.fisheye <- value
    | "brightness" -> filters.brightness <- value
    | "pixelate" -> filters.pixelate <- value
    | "mosaic" -> filters.mosaic <- value
    | "color" ->
        let v = value % 200.
        let v = if v < 0. then v + 200. else v
        filters.color <- clamp (0., 200.) v

    | "ghost" ->
        filters.ghost <- clamp (0., 100.) value

    | _ when state.blockState.config.useFilterNameCheck ->
            cfailwithf state location $"unknown filter name: {name}"
    | _ ->
        let v = clamp (0., 100.) value
        filters.others <- Map.add name v filters.others

let changeFilter state location name addValue =
    let value =
        let filters = &state.self.drawingData.filters
        match name with
        | "whirl" -> filters.whirl
        | "fisheye" -> filters.fisheye
        | "brightness" -> filters.brightness
        | "pixelate" -> filters.pixelate
        | "mosaic" -> filters.mosaic
        | "color" -> filters.color
        | "ghost" -> filters.ghost
        | _ when state.blockState.config.useFilterNameCheck ->
            cfailwithf state location $"unknown filter name: {name}"
        | _ ->
            Map.tryFind name filters.others
            |> VOption.defaultValue 0.

    setFilter state location name (value + addValue)

let resetFilter self =
    let filters = &self.drawingData.filters
    filters.brightness <- 0.
    filters.pixelate <- 0.
    filters.fisheye <- 0.
    filters.mosaic <- 0.
    filters.whirl <- 0.
    filters.color <- 0.
    filters.ghost <- 0.
    filters.others <- Map.empty


let inline evaluateExpression state e = evaluateExpression &state e
let inline private getValueOrRaise state location name = getValueOrRaise &state location name
let inline private getListOrRaise state location name = getListOrRaise &state location name

let forward sprite steps =
    let data = &sprite.spriteDrawingData
    let d = (90. - data.direction) * Math.PI / 180.
    moveTo sprite (data.x + steps * cos d, data.y + steps * sin d)

let pointTowardsFromPoint sprite (x, y) =
    let data = &sprite.spriteDrawingData
    let dx = x - data.x
    let dy = y - data.y
    data.direction <- if (dx = 0. && dy = 0.) then 90. else atan2 dx dy * 180. / Math.PI

let pointTowards state sprite = function
    | SString "_mouse_" ->
        let struct(x, y) = Input.getMousePosition &state.input
        pointTowardsFromPoint sprite (x, y)

    | target ->

    let name = toString target
    match ObjectState.findObject name state with
    | ValueNone
    | ValueSome { sprite = ValueNone } -> ()
    | ValueSome { sprite = ValueSome target } ->

    pointTowardsFromPoint sprite (target.spriteDrawingData.x, target.spriteDrawingData.y)

let gotoObject state sprite = function
    | SString "_mouse_" ->
        let struct(x, y) = Input.getMousePosition &state.input
        moveTo sprite (x, y)

    | SString "_random_" ->
        let random = state.config.randomNextDouble
        let x = Math.Round(screenWidth * random() - screenWidth * 0.5, MidpointRounding.AwayFromZero)
        let y = Math.Round(screenHeight * random() - screenHeight * 0.5, MidpointRounding.AwayFromZero)
        moveTo sprite (x, y)

    | target ->
        match ObjectState.findObject (toString target) state with
        | ValueNone
        | ValueSome { sprite = ValueNone } -> ()
        | ValueSome { sprite = ValueSome target } ->

        moveTo sprite (target.spriteDrawingData.x, target.spriteDrawingData.y)

let say sprite message kind =
    sprite.spriteDrawingData.sayText <- message
    sprite.spriteDrawingData.sayKind <- kind
    let Version v as sayVersion = sprite.sayVersion
    sprite.sayVersion <- Version(v + 1)
    sayVersion

let clearSay sprite version =
    if sprite.sayVersion = version then
        sprite.spriteDrawingData.sayText <- ""

let sayDuration sprite message seconds kind = fiber {
    let version = say sprite message kind
    yield Sleep(TimeSpan.FromSeconds seconds)
    clearSay sprite version
}

let listIndex stage index length =
    match index with
    | SNumber index when double (int index) = index ->
        let i = int index
        if i > 0 && i <= length then ValueSome(i - 1) else
        ValueNone

    | SString("random" | "any") -> ValueSome(int (stage.config.randomNextDouble() * double length))
    | SString "last" -> if length = 0 then ValueNone else ValueSome(length - 1)
    | _ ->
        let i = int (toNumber index)
        if i > 0 && i <= length then ValueSome(i - 1) else
        ValueNone

let listMaxLength state =
    match state.blockState.config.listMaxLength with
    | Some l -> l
    | None ->

    match state.blockState.config.version with
    | RuntimeVersion.Sb2 -> Int32.MaxValue
    | RuntimeVersion.Sb3 -> 200000

let clampList state location listName (list: _ ResizeArray) =
    let maxLength = listMaxLength state
    if maxLength < list.Count then
        list.RemoveAt(list.Count - 1)
        if state.blockState.config.useLengthCheck then
            cfailwithf state location $"list overflow: '%s{listName}', max length: {maxLength}"

let insertInList state location listName (list: _ ResizeArray) index value =
    match listIndex state.blockState index (list.Count + 1) with
    | ValueSome i ->
        list.Insert(i, value)
        clampList state location listName list
    | _ ->

    if state.blockState.config.useRangeCheck then
        cfailwithf state location $"out of range: insert:at:ofList:, '%s{listName}' '{index}'"

let onSubmit stage value =
    let Version version as promptVersion = stage.currentAskVersion
    if promptVersion < stage.nextAskVersion then
        stage.answer <- value
        stage.currentAskVersion <- Version(version + 1)
        if stage.currentAskVersion >= stage.nextAskVersion then
            stage.showInputBox <- false
    HUnit

let ask state question =
    let stage = state.blockState
    if question <> "" then
        match state.self.sprite with
        | ValueSome sprite when sprite.spriteDrawingData.visible ->
            say sprite question Say |> ignore
            StageView.hideStageQuestion &stage.view
        | _ ->
            stage.view.ShowStageQuestion question
    else
        stage.view.HideStageQuestion()

    stage.sharedState.showInputBox <- true
    let mutable f = Func.ofFun(onSubmit stage.sharedState)
    stage.view.ShowInputBox("", &f)

let findSoundFromNth self nth =
    let l = IArray.length self.sounds
    if l <> 0 && not (Double.IsNaN nth) then
        let i = Math.Round(nth - 1., MidpointRounding.AwayFromZero) % 1.
        let i = if i < 0. then i + 1. else i
        ValueSome(int i)
    else
        ValueNone

let findSound self = function
    | SBool _ -> ValueNone
    | SNumber index -> findSoundFromNth self index
    | SString name as sound ->

    match Map.tryFind name self.soundNameToIndex with
    | ValueSome _ as r -> r
    | _ -> findSoundFromNth self (toNumber sound)

let setRotationStyle sprite style =
    let style =
        match style with
        | SString "left-right" -> RotationStyle.LeftRight
        | SString "don't rotate" -> RotationStyle.None
        | _ -> RotationStyle.Normal

    sprite.spriteDrawingData.rotationStyle <- style

let comeToFront stage self =
    let { objects = objects } = stage
    let index = objects.IndexOf self
    objects.RemoveAt index
    objects.Add self
    StageView.moved &stage.view self index (objects.Count - 1)

let goBackByLayers stage self layerCount =
    let { objects = objects } = stage
    let index = objects.IndexOf self
    let index' = max 0 (index - int layerCount)
    objects.RemoveAt index
    objects.Insert(index', self)
    StageView.moved &stage.view self index index'

let setPenColor stage self sprite color =
    let c = uint32 color
    let a = (c >>> 24) &&& 0xffu
    let r = (c >>> 16) &&& 0xffu
    let g = (c >>> 8) &&& 0xffu
    let b = (c >>> 0) &&& 0xffu
    let a = if a = 0u then 0xFFu else a
    let argb = struct(byte a, byte r, byte g, byte b)
    sprite.spriteDrawingData.penArgb <- argb
    StageView.setPenArgb &stage.view self argb

let changePenHueBy stage self sprite oldValueRatio hue =
    let struct(a, h, _, l) = Color.rgb2hsl sprite.spriteDrawingData.penArgb
    let argb = Color.hsl2rgb struct(a, (h * oldValueRatio) + (hue * 360. / 200.), 100., l)
    sprite.spriteDrawingData.penArgb <- argb
    StageView.setPenArgb &stage.view self argb

let changePenShadeBy stage self sprite oldValueRatio lightness =
    let struct(a, h, _, l) = Color.rgb2hsl sprite.spriteDrawingData.penArgb
    let lightness = (l * oldValueRatio + lightness) % 200.
    
    // 0..<200
    let lightness = if lightness < 0. then lightness + 200. else lightness

    let argb = Color.hsl2rgb struct(a, h, 100., lightness)
    sprite.spriteDrawingData.penArgb <- argb
    StageView.setPenArgb &stage.view self argb

let changePenSizeBy stage self sprite oldValueRatio extend =
    let penSize = &sprite.spriteDrawingData.penSize
    let size = max 1. (oldValueRatio * penSize + extend)
    penSize <- size
    StageView.penSize &stage.view self size

let deleteLintAtNth state location name nth (xs: _ ResizeArray) =
    let i = int (toNumber nth) - 1
    if 0 <= i && i < xs.Count then
        xs.RemoveAt i
    else
        if state.blockState.config.useRangeCheck then
            cfailwithf state location $"out of range: deleteLine:ofList:, '%s{name}' '{nth}'"

let deleteLine state location name nth =
    let xs = getListOrRaise state location name
    match nth with
    | SString "all" -> xs.Clear()
    | SString "last" ->
        if xs.Count <> 0 then
            xs.RemoveAt(xs.Count - 1)
    | nth ->
        match state.blockState.config.version with
        | RuntimeVersion.Sb3 ->
            match nth with
            | SString("any" | "random") ->
                if xs.Count <> 0 then
                    xs.RemoveAt(int (state.blockState.config.randomNextDouble() * double xs.Count))

            | _ -> deleteLintAtNth state location name nth xs

        | RuntimeVersion.Sb2 ->
            deleteLintAtNth state location name nth xs

let glideSecs scheduler sprite seconds x2 y2 = fiber {
    let t1 = Scheduler.now scheduler
    let duration = seconds
    let x1 = sprite.spriteDrawingData.x
    let y1 = sprite.spriteDrawingData.y
    let xDelta = x2 - x1
    let yDelta = y2 - y1

    while
        begin
            let t = Scheduler.now scheduler - t1
            let normalizedTime = min 1. (t.TotalSeconds / duration)
            moveTo sprite (x1 + normalizedTime * xDelta, y1 + normalizedTime * yDelta)
            normalizedTime < 1.
        end
        do yield ThreadYieldForce

}

let doAsk state question = fiber {
    let stage = state.blockState.sharedState
    let Version v as askVersion = stage.nextAskVersion
    stage.nextAskVersion <- Version(v + 1)
    while stage.currentAskVersion < askVersion do
        yield ThreadYieldForce

    ask state question

    while stage.currentAskVersion = askVersion do
        yield ThreadYieldForce
}
let stopScripts state location kind = fiberPoly {
    match kind with
    | "all" -> raise <| StopException(StopAll, stackMessage state location "")
    | "this script" -> yield Abort
    | "other scripts in sprite"
    | "other scripts in stage" ->
        let! struct({ ThreadInfo.id = currentThreadId }, _) = Fiber.environment
        let { self = self; blockState = { scheduler = { threads = threads } } } = state

        threads
        |> Colony.ignoreWith (fun t -> self = t.self && t.id <> currentThreadId)

    | _ -> ()
}
let deleteClone state = fiberPoly {
    let { self = self; blockState = { objects = objects; scheduler = { threads = threads } } } = state

    if self.isClone then
        let index = objects.IndexOf self
        objects.RemoveAt index
        threads |> Colony.ignoreWith (fun thread -> LanguagePrimitives.PhysicalEquality self thread.self)
        StageView.removed &state.blockState.view self index
        yield Abort
}
let rec evaluateBlock state (BlockExpression(_, ss)) = fiber {
    for s in ss do
        do! evaluateStatement state s
    }

and evaluateProcedureCall state location (ProcedureDefinition(isAtomic = isAtomic; body = body) as proc) args = fiberPoly {
    let state = {
        state with
            args = args
            isAtomic = match isAtomic with Atomic -> true | NoAtomic -> false
            callStack = StackFlame(location, Procedure proc, args, state.self)::state.callStack
        }
    match isAtomic with
    | Atomic ->
        let (AsAtomic g) = Atomic.doAtomic (let f = evaluateBlock state body in &f)
        do! g
    | NoAtomic ->
        do! evaluateBlock state body
    }

and evaluateCall state location name args =
    let proc = getProcedureOrRaise &state location name
    let args = evaluateExpressionsOrRaise &state args
    let args = checkArgs &state location proc args

    evaluateProcedureCall state location proc args

and evaluateListener callerLocation (state: _ inref) (ListenerDefinition(body = body) as listener) args self =
    let state = {
        blockState = state.blockState
        args = args
        isAtomic = false
        self = self
        callStack = StackFlame(callerLocation, Listener listener, args, self)::state.callStack
    }
    let action = evaluateBlock state body

    let name = listenerName state.blockState.config self listener
    let task = action :> ThreadTask<_>
    Scheduler.registerFiberWithSelf self name DateTime.MinValue &task state.blockState.scheduler

and evaluateCreateCloneOf state location name =
    let name = Expressions.evaluateExpression &state name |> toString

    let parent =
        match name with
        | "_myself_" -> ValueSome state.self
        | _ -> findObject name state.blockState

    match parent with
    | ValueNone
    | ValueSome { sprite = ValueNone } -> ()
    | ValueSome({ sprite = ValueSome sprite } as parent) ->

    let c = cloneSprite sprite parent
    let objects = state.blockState.objects
    let index = objects.IndexOf parent
    objects.Insert(index, c)
    StageView.cloned &state.blockState.view parent c index

    let args = Map ["name", SString name]
    for listener in sprite.whenCloned do
        evaluateListener location &state listener args c |> ignore

and evaluateExpressionsOrRaise (state: _ inref) xs =
    let state = state
    xs
    |> List.map (function
        | Block(BlockExpression(state = location)) -> cfailwithf state location "unexpected block"
        | e -> Expressions.evaluateExpression &state e
    )
and evaluateDoRepeat state count body = fiberPoly {
    let count = evaluateExpression state count |> toNumberZeroIfNaN
    let count = Math.Round(count, MidpointRounding.AwayFromZero)
    match state.isAtomic with
    | true ->
        for _ in 1. .. 1. .. count do
            do! evaluateBlock state body
    | _ ->
        for _ in 1. .. 1. .. count do
            do! evaluateBlock state body
            yield ThreadYield
    }
and evaluateDoUntil state test ifFalse = fiberPoly {
    match state.isAtomic with
    | true ->
        while evaluateExpression state test |> toBool |> not do
            do! evaluateBlock state ifFalse
    | _ ->
        while evaluateExpression state test |> toBool |> not do
            do! evaluateBlock state ifFalse
            yield ThreadYield
    }
and evaluateDoForever state body = fiberPoly {
    match state.isAtomic with
    | true ->
        while true do
            do! evaluateBlock state body
    | _ ->
        while true do
            do! evaluateBlock state body
            yield ThreadYield
    }
and evaluateStatement state (ComplexExpression(location, op, args)) = fiberPoly {
    match op, args, state.self.sprite with
    | O.call, Literal(_, SString name)::callArgs, _ ->
        do! evaluateCall state location name callArgs

// type KnownStatement<E, M, S> =

    | O.``forward:``, [steps], ValueSome sprite ->
        let steps = evaluateExpression state steps |> toNumberZeroIfNaN
        forward sprite steps

    | O.``turnRight:``, [x1], ValueSome sprite ->
        let x1 = evaluateExpression state x1 |> toNumberZeroIfNaN
        setDirection sprite (sprite.spriteDrawingData.direction + x1)

    | O.``turnLeft:``, [x1], ValueSome sprite ->
        let x1 = evaluateExpression state x1 |> toNumberZeroIfNaN
        setDirection sprite (sprite.spriteDrawingData.direction - x1)

    | O.``heading:``, [degrees], ValueSome sprite ->
        let degrees = evaluateExpression state degrees |> toNumberZeroIfNaN
        setDirection sprite degrees

    | O.``pointTowards:``, [target], ValueSome sprite ->
        let target = evaluateExpression state target
        pointTowards state.blockState sprite target

    | O.``gotoX:y:``, [e1; e2], ValueSome sprite ->
        let x = evaluateExpression state e1 |> toNumberZeroIfNaN
        let y = evaluateExpression state e2 |> toNumberZeroIfNaN
        moveTo sprite (x, y)

    | O.``gotoSpriteOrMouse:``, [target], ValueSome sprite ->
        let target = evaluateExpression state target
        gotoObject state.blockState sprite target

    | O.``changeXposBy:``, [moveX], ValueSome sprite ->
        let moveX = evaluateExpression state moveX |> toNumberZeroIfNaN
        moveTo sprite (sprite.spriteDrawingData.x + moveX, sprite.spriteDrawingData.y)

    | O.``xpos:``, [positionX], ValueSome sprite ->
        let positionX = evaluateExpression state positionX |> toNumberZeroIfNaN
        moveTo sprite (positionX, sprite.spriteDrawingData.y)

    | O.``changeYposBy:``, [moveY], ValueSome sprite ->
        let moveY = evaluateExpression state moveY |> toNumberZeroIfNaN
        moveTo sprite (sprite.spriteDrawingData.x, sprite.spriteDrawingData.y + moveY)

    | O.``ypos:``, [positionY], ValueSome sprite ->
        let positionY = evaluateExpression state positionY |> toNumberZeroIfNaN
        moveTo sprite (sprite.spriteDrawingData.x, positionY)

//     | Case0<"bounceOffEdge">

    | O.setRotationStyle, [style], ValueSome sprite ->
        let style = evaluateExpression state style
        setRotationStyle sprite style

    | O.``lookLike:``, [costume], _ ->
        let costume = evaluateExpression state costume
        setCostume state.self costume

    | O.nextCostume, _, _ -> showNextCostume state.self

//     | Case1<"showBackground:", E>
//     | Case1<"startScene", E>
//     | Case0<"nextBackground">
//     | Case0<"nextScene">
//     | Case1<"startSceneAndWait", E>

    | O.``say:duration:elapsed:from:``, [message; seconds], ValueSome sprite ->
        let message = evaluateExpression state message |> toString
        let seconds = evaluateExpression state seconds |> toNumberZeroIfNaN
        do! sayDuration sprite message seconds Say

    | O.``say:``, [message], ValueSome sprite ->
        let message = evaluateExpression state message |> toString
        say sprite message Say |> ignore

    | O.``think:duration:elapsed:from:``, [message; seconds], ValueSome sprite ->
        let message = evaluateExpression state message |> toString
        let seconds = evaluateExpression state seconds |> toNumberZeroIfNaN
        do! sayDuration sprite message seconds Think

    | O.``think:``, [message], ValueSome sprite ->
        let message = evaluateExpression state message |> toString
        say sprite message Think |> ignore

    | O.``changeGraphicEffect:by:``, [name; addValue], _ ->
        let name = evaluateExpression state name |> toString
        let addValue = evaluateExpression state addValue |> toNumber
        changeFilter state location name addValue

    | O.``setGraphicEffect:to:``, [name; value], _ ->
        let name = evaluateExpression state name |> toString
        let value = evaluateExpression state value |> toNumber
        setFilter state location name value

    | O.filterReset, _, _ ->
        resetFilter state.self

    | O.``setSizeTo:``, [e1], ValueSome sprite ->
        let size = evaluateExpression state e1 |> toNumberZeroIfNaN
        let scale = max 0. (size / 100.)
        sprite.spriteDrawingData.scale <- scale

    | O.``changeSizeBy:``, [e1], ValueSome sprite ->
        let size = evaluateExpression state e1 |> toNumberZeroIfNaN
        let scale = max 0. (sprite.spriteDrawingData.scale + (size / 100.))
        sprite.spriteDrawingData.scale <- scale

    | O.show, _, _ -> setVisibility state.self true
    | O.hide, _, _ -> setVisibility state.self false
    | O.comeToFront, _, ValueSome _ -> comeToFront state.blockState state.self
    | O.``goBackByLayers:``, [layerCount], ValueSome _ ->
        let layerCount = evaluateExpression state layerCount |> toNumber
        goBackByLayers state.blockState state.self layerCount

    | O.``playSound:``, [sound], _ ->
        let sound = evaluateExpression state sound
        match findSound state.self.shared sound with
        | ValueNone -> ()
        | ValueSome index -> state.blockState.view.PlaySound(state.self, index)

//     | Case1<"doPlaySoundAndWait", E>
//     | Case0<"stopAllSounds">

//     | Case2<"playDrum", E, E>
//     | Case1<"rest:elapsed:from:", E>
//     | Case2<"noteOn:duration:elapsed:from:", E, E>
//     | Case1<"instrument:", E>

    | O.``changeVolumeBy:``, [addValue], _ ->
        let addValue = evaluateExpression state addValue |> toNumber
        let volume = &state.self.drawingData.volume
        volume <- min 1. (max 0. (volume + addValue / 100.))

    | O.``setVolumeTo:``, [value], _ ->
        let value = evaluateExpression state value |> toNumber
        state.self.drawingData.volume <- min 1. (max 0. (value / 100.))

    | O.``changeTempoBy:``, [addValue], _ ->
        let addValue = evaluateExpression state addValue |> toNumber
        let tempo = &state.blockState.sharedState.tempo
        tempo <- tempo + addValue

    | O.``setTempoTo:``, [value], _ ->
        let value = evaluateExpression state value |> toNumber
        state.blockState.sharedState.tempo <- value

    | O.clearPenTrails, _, _ -> state.blockState.view.ClearCanvas()

    | O.putPenDown, _, ValueSome sprite ->
        sprite.spriteDrawingData.penDown <- true
        state.blockState.view.PenDown(state.self, true)

    | O.putPenUp, _, ValueSome sprite ->
        sprite.spriteDrawingData.penDown <- false
        state.blockState.view.PenDown(state.self, false)

    | O.``penColor:``, [color], ValueSome sprite ->
        let color = evaluateExpression state color |> toNumber
        setPenColor state.blockState state.self sprite color

    | O.``setPenHueTo:``, [hue], ValueSome sprite ->
        let hue = evaluateExpression state hue |> toNumber
        changePenHueBy state.blockState state.self sprite 0. hue

    | O.``changePenHueBy:``, [hue], ValueSome sprite ->
        let hue = evaluateExpression state hue |> toNumber
        changePenHueBy state.blockState state.self sprite 1. hue

    | O.``setPenShadeTo:``, [lightness], ValueSome sprite ->
        let lightness = evaluateExpression state lightness |> toNumber
        changePenShadeBy state.blockState state.self sprite 0. lightness

    | O.``changePenShadeBy:``, [lightness], ValueSome sprite ->
        let lightness = evaluateExpression state lightness |> toNumber
        changePenShadeBy state.blockState state.self sprite 1. lightness

    | O.``penSize:``, [size], ValueSome sprite ->
        let size = evaluateExpression state size |> toNumber
        changePenSizeBy state.blockState state.self sprite 0. size

    | O.``changePenSizeBy:``, [extend], ValueSome sprite ->
        let extend = evaluateExpression state extend |> toNumber
        changePenSizeBy state.blockState state.self sprite 1. extend

    | O.stampCostume, _, _ -> state.blockState.view.StampToCanvas state.self

    | O.``setVar:to:``, [Literal(_, SString name); e], _ ->
        let v = evaluateExpression state e
        if Set.contains name state.blockState.clouds then setCloudValue &state location name v
        else getValueOrRaise state location name := v

    | O.``changeVar:by:``, [Literal(_, SString name); e], _ ->
        let x = evaluateExpression state e

        if Set.contains name state.blockState.clouds then
            let v = getCloudValue &state name
            setCloudValue &state location name (SNumber(toNumber v + toNumber x))
        else
            let v = getValueOrRaise state location name
            v := SNumber(toNumber v.contents + toNumber x)

    | O.``append:toList:``, [e; Literal(_, SString name)], _ ->
        let v = getListOrRaise state location name
        let x = evaluateExpression state e
        v.Add x
        clampList state location name v

    | O.``deleteLine:ofList:``, [nth; Literal(_, SString name)], _ ->
        let nth = evaluateExpression state nth
        deleteLine state location name nth

    | O.``insert:at:ofList:``, [value; target; Literal(_, SString listName)], _ ->
        let value = evaluateExpression state value
        let target = evaluateExpression state target
        let list = getListOrRaise state location listName
        insertInList state location listName list target value

    | O.``setLine:ofList:to:``, [nth; Literal(_, SString name); value], _ ->
        let v = getListOrRaise state location name
        let nth = evaluateExpression state nth
        let value = evaluateExpression state value
        match listIndex state.blockState nth v.Count with
        | ValueSome index -> v.[index] <- value
        | _ ->

        if state.blockState.config.useRangeCheck then
            cfailwithf state location $"out of range: setLine:ofList:to:, '%s{name}' '{nth}'"

    | O.``showVariable:``, [Literal(_, SString name)], _
    | O.``showList:``, [Literal(_, SString name)], _ ->

        // TODO:
        state.blockState.view.VariableMonitorChanged(state.self, name, Hidden)

    | O.``hideVariable:``, [Literal(_, SString name)], _
    | O.``hideList:``, [Literal(_, SString name)], _ ->

        // TODO:
        state.blockState.view.VariableMonitorChanged(state.self, name, Visible)

    | O.``broadcast:``, [Literal(_, SString name)], _ -> broadcast location state name
    | O.doBroadcastAndWait, [Literal(_, SString name)], _ -> do! broadcastAndWait location state name

//     | Case2<"doForeverIf", E, M>

    | O.doIf, [test; Block ifTrue], _ ->
        let test = evaluateExpression state test |> toBool
        if test then do! evaluateBlock state ifTrue

    | O.doIfElse, [test; Block ifTrue; Block ifFalse], _ ->
        let test = evaluateExpression state test |> toBool
        if test then do! evaluateBlock state ifTrue
        else do! evaluateBlock state ifFalse

    | O.doRepeat, [count; Block body], _ -> do! evaluateDoRepeat state count body
//     | Case0<"doReturn">
    | O.doUntil, [test; Block ifFalse], _ -> do! evaluateDoUntil state test ifFalse
//     | Case2<"doWhile", E, M>

    | O.doWaitUntil, [test], _ ->
        while evaluateExpression state test |> toBool |> not do
            yield ThreadYieldForce

    | O.``glideSecs:toX:y:elapsed:from:``, [seconds; x2; y2], ValueSome sprite ->
        let seconds = evaluateExpression state seconds |> toNumber
        let x2 = evaluateExpression state x2 |> toNumber
        let y2 = evaluateExpression state y2 |> toNumber
        do! glideSecs state.blockState.scheduler sprite seconds x2 y2

    | O.``wait:elapsed:from:``, [seconds], _ ->
        let seconds = evaluateExpression state seconds |> toNumberZeroIfNaN
        yield Sleep(TimeSpan.FromSeconds seconds)

//     | Case1<"warpSpeed", M>

    | O.createCloneOf, [name], _ -> evaluateCreateCloneOf state location name
    | O.doAsk, [question], _ ->
        let question = evaluateExpression state question |> toString
        do! doAsk state question

    | O.timerReset, _, _ ->
        state.blockState.sharedState.timerEpoch := Scheduler.now state.blockState.scheduler

    | O.doForever, [Block body], _ -> do! evaluateDoForever state body

//     | Case0<"stopAll">

    | O.stopScripts, [Literal(_, SString kind)], _ -> do! stopScripts state location kind
    | O.deleteClone, _, _ -> do! deleteClone state

    | opxs -> cfailwithf state location $"not implemented {opxs}"
    }

and private broadcastObject location (state: _ inref) name self =
    let name = LowerName.toString name
    match Map.tryFind name self.shared.whenIReceive with
    | ValueNone -> ()
    | ValueSome listeners ->
        let args = Map ["name", SString name]
        for listener in listeners do
            evaluateListener location &state listener args self |> ignore

and broadcast location state name =
    let name = LowerName.ofString name
    broadcastObject location &state name state.blockState.stage
    for self in state.blockState.objects do
        broadcastObject location &state name self

and private broadcastAndWaitObject location (state: _ inref) name self runningThreadIds =
    let name = LowerName.toString name
    match Map.tryFind name self.shared.whenIReceive with
    | ValueNone -> runningThreadIds
    | ValueSome listeners ->
        let args = Map ["name", SString name]
        let mutable runningThreadIds = runningThreadIds
        for listener in listeners do
            let t = evaluateListener location &state listener args self 
            runningThreadIds <- Set.add t.id runningThreadIds
        runningThreadIds

and broadcastAndWait location state name =
    let name = LowerName.ofString name
    let mutable runningThreadIds = Set.empty
    runningThreadIds <- broadcastAndWaitObject location &state name state.blockState.stage runningThreadIds
    for self in state.blockState.objects do
        runningThreadIds <- broadcastAndWaitObject location &state name self runningThreadIds

    let threads = state.blockState.scheduler.threads
    let runningThreadIds = runningThreadIds
    fiber {
        while threads |> Colony.exists (fun t -> Set.contains t.id runningThreadIds) do
            yield ThreadYieldForce
    }
