module Scratch.Executor.Blocks
open System
open System.Text
open Scratch
open Scratch.Primitives
open Scratch.Ast
open Scratch.AstDefinitions
open Scratch.Executor


let screenWidth = 480.
let screenHeight = 360.
let timeStampEpoch = DateTime(2000, 1, 1)

let timeAndDate (now: DateTime) kind =
    if Value.isString kind then
        match kind.StringOrDefault with
        | "year" -> now.Year
        | "month" -> now.Month
        | "date" -> now.Day
        | "day of week" -> int now.DayOfWeek + 1
        | "hour" -> now.Hour
        | "minute" -> now.Minute
        | "second" -> now.Second
        | _ -> 0
    else 0

let deleteLineNoString nth (xs: _ ResizeArray) =
    let i = int (Value.toNumber nth) - 1
    if 0 <= i && i < xs.Count then
        xs.RemoveAt i
    //else
    //    if state.blockState.config.useRangeCheck then
    //        cfailwithf state location "out of range: deleteLine:ofList:, '%s' '%A'" name nth

let deleteLineSb2 nth xs =
    match Value.stringOrDefault nth with
    | null -> deleteLineNoString nth xs
    | "all" -> xs.Clear()
    | "last" ->
        if xs.Count <> 0 then
            xs.RemoveAt(xs.Count - 1)
    | _ -> deleteLineNoString nth xs

let deleteLineSb3 (random: _ byref) nth xs =
    match Value.stringOrDefault nth with
    | null -> deleteLineNoString nth xs
    | "all" -> xs.Clear()
    | "last" ->
        if xs.Count <> 0 then
            xs.RemoveAt(xs.Count - 1)
    | "any"
    | "random" ->
        if xs.Count <> 0 then
            xs.RemoveAt(int (Random.nextDouble &random * double xs.Count))

    | _ -> deleteLineNoString nth xs

let toNumberListIndex index length =
    let i = int (Value.toNumber index)
    if i > 0 && i <= length then ValueSome(i - 1) else
    ValueNone

let listIndex (random: _ byref) index length =
    if Value.isNumber index then
        let n = index.NumberOrDefault
        if double (int n) = n then
            let i = int n
            if i > 0 && i <= length then ValueSome(i - 1) else
            ValueNone
        else
            toNumberListIndex index length

    elif index.IsString then
        match index.StringOrDefault with
        | "random"
        | "any" -> ValueSome(int (Random.nextDouble &random * double length))
        | "last" -> ValueSome(length - 1)
        | _ -> toNumberListIndex index length
    else
        toNumberListIndex index length

let getLine (random: _ byref) nth (list: _ ResizeArray) =
    let n =
        if Value.isString nth then
            match nth.StringOrDefault with
            | "random"
            | "any" -> double (Random.nextRange &random (1, list.Count + 1))
            | "last" -> double list.Count
            | _ -> Value.toNumber nth
        else
            Value.toNumber nth
    let n =
        if Double.IsNaN n || Double.IsInfinity n then 0
        else int n

    if 1 <= n && n <= list.Count then list.[n - 1]
    else Value.EmptyString

let isSingle = Predicate(fun x -> Value.isString x && x.StringOrDefault.Length = 1)
let contentsOfList (list: _ ResizeArray) =
    match list.Count with
    | 0 -> ""
    | 1 -> Value.toString list.[0]
    | _ ->

    let isSingle = list.TrueForAll isSingle
    let separator = if isSingle then "" else " "

    let b = StringBuilder(capacity = 1024)
    let mutable e = list.GetEnumerator()
    e.MoveNext() |> ignore
    b.Append(Value.toString e.Current) |> ignore
    while e.MoveNext() do
        b
            .Append(separator)
            .Append(Value.toString e.Current) |> ignore
    b.ToString()

let getCostumeName self =
    let costumes = self.entityImage.costumes
    let index = self.drawingData.currentCostumeIndex
    if 0 <= index && index < IArray.length costumes then (IArray.item index costumes).costumeName
    else ""

let letterOfSb2 s n =
    let n =
        if Double.IsInfinity n then 1
        elif Double.IsNaN n then 0
        else int n

    if 1 <= n && n <= String.length s then string s.[n - 1] else
    ""

let letterOfSb3 s n =
    let n =
        if Double.IsNaN n then 0
        else int n

    if 1 <= n && n <= String.length s then string s.[n - 1] else
    ""

let moveTo sprite (x, y) =
    let sprite = &sprite.spriteDrawingData

    let ox = sprite.x
    let oy = sprite.y
    if ox = x && oy = y (* && not sprite.isPenDown *) then () else

    sprite.x <- x
    sprite.y <- y
    //if sprite.isPenDown && not sprite.isDragging then
    //    _movePen sprite (ox, oy) (x, y)

    //if sprite.saying then
    //    updateBubble sprite

let findEntity stage name =
    let mutable r = Unchecked.defaultof<_>
    if stage.originalSpriteMap.TryGetValue(name, &r) then ValueSome r else

    match name with
    | "_stage_" | _ when name = stage.stageState.entityImage.entityName -> ValueSome stage.stageState
    | _ -> ValueNone

let distance (x1, y1) (x2, y2) =
    sqrt ((x2 - x1) * (x2 - x1) + (y2 - y2) * (y2 - y1))

let distanceToPoint sprite (x, y) =
    distance (sprite.spriteDrawingData.x, sprite.spriteDrawingData.y) (x, y)

let distanceTo (input: _ byref) stage self (target: Value) =
    if target.StringOrDefault = "_mouse_" then
        let struct(x, y) = Input.getMousePosition &input
        distanceToPoint self (x, y)
    else

    match findEntity stage (Value.toString target) with
    | ValueNone
    | ValueSome { entityImage = { spriteImage = None }} -> 10000.
    | ValueSome sprite -> distanceToPoint sprite (sprite.spriteDrawingData.x, sprite.spriteDrawingData.y)

let getKeyCode v =
    if Value.isNumber v then
        enum (KeyCode.getKeyCodeFromNumber v.NumberOrDefault)

    elif v.IsTrue then enum (int 'T')
    elif v.IsFalse then enum (int 'F')
    else

    match v.StringOrDefault with
    | "space" -> KeyCode.Space
    | "left arrow" -> KeyCode.Left
    | "up arrow" -> KeyCode.Up
    | "right arrow" -> KeyCode.Right
    | "down arrow" -> KeyCode.Down
    | "" -> KeyCode.None
    | key ->

    match key.[0] with
    | ' ' -> KeyCode.Space
    | key -> enum (int (Char.ToUpperInvariant key))

let pointTowardsFromPoint sprite (x, y) =
    let data = &sprite.spriteDrawingData
    let dx = x - data.x
    let dy = y - data.y
    data.direction <- if (dx = 0. && dy = 0.) then 90. else atan2 dx dy * 180. / Math.PI

let pointTowards (input: _ byref) stage self target =
    if Value.stringOrDefault target = "_mouse_" then
        let struct(x, y) = Input.getMousePosition &input
        pointTowardsFromPoint self (x, y)
    else

    let name = Value.toString target
    match findEntity stage name with
    | ValueNone
    | ValueSome { entityImage = { spriteImage = None } } -> ()
    | ValueSome target -> pointTowardsFromPoint target (target.spriteDrawingData.x, target.spriteDrawingData.y)

let gotoObject (input: _ byref) (random: _ byref) stage self target =
    match Value.stringOrDefault target with
    | "_mouse_" ->
        let struct(x, y) = Input.getMousePosition &input
        moveTo self (x, y)

    | "_random_" ->
        let x = Math.Round(screenWidth * Random.nextDouble &random - screenWidth * 0.5, MidpointRounding.AwayFromZero)
        let y = Math.Round(screenHeight * Random.nextDouble &random - screenHeight * 0.5, MidpointRounding.AwayFromZero)
        moveTo self (x, y)

    | _ ->

    match findEntity stage (Value.toString target) with
    | ValueNone
    | ValueSome { entityImage = { spriteImage = None } } -> ()
    | ValueSome target -> moveTo target (target.spriteDrawingData.x, target.spriteDrawingData.y)

let setRotationStyle self style =
    let style =
        match Value.stringOrDefault style with
        | "left-right" -> RotationStyle.LeftRight
        | "don't rotate" -> RotationStyle.None
        | _ -> RotationStyle.Normal

    self.spriteDrawingData.rotationStyle <- style


type ICostumeIndexEnvironment =
    abstract IsSprite: bool
    abstract CostumeNameToIndex: name: string -> int voption
    abstract CostumeCount: int

[<AutoOpen>]
module CostumeIndexEnvironmentOperations =
    let isSprite (x: 'T byref when 'T :> ICostumeIndexEnvironment and 'T : struct) = x.IsSprite
    let costumeCount (x: 'T byref when 'T :> ICostumeIndexEnvironment and 'T : struct) = x.CostumeCount
    let costumeNameToIndex (x: 'T byref when 'T :> ICostumeIndexEnvironment and 'T : struct) name = x.CostumeNameToIndex name

module CostumeIndexEnvironment =
    [<Struct; RequireQualifiedAccess>]
    type OfEntityState<'a> = { entity: EntityState<'a> } with
        interface ICostumeIndexEnvironment with
            member x.IsSprite = x.entity.entityImage.spriteImage |> Option.isSome
            member x.CostumeNameToIndex name = Map.tryFind name x.entity.costumeNameToIndex
            member x.CostumeCount = x.entity.entityImage.costumes |> IArray.length
    let ofEntityState entity = { OfEntityState.entity = entity }

    [<Struct; RequireQualifiedAccess>]
    type Poly<'T> when 'T :> ICostumeIndexEnvironment = private { mutable env: 'T } with
        interface ICostumeIndexEnvironment with
            member x.IsSprite = x.env.IsSprite
            member x.CostumeNameToIndex name = x.env.CostumeNameToIndex name
            member x.CostumeCount = x.env.CostumeCount
    let poly env = { Poly.env = env }

[<Struct>]
type IndexOrigin = CurrentIndex | FirstIndex

let costumeIndex (c: _ byref) costume =
    if Double.IsNaN costume then struct(CurrentIndex, 0) else
    let i = (int costume - 1) % costumeCount &c
    let i = if i < 0 then i + costumeCount &c else i
    struct(FirstIndex, i)

let costumeIndexFromValue (c: _ byref) costume =
    if Value.isNumber costume then
        let costume = costume.NumberOrDefault
        costumeIndex &c costume
    else

    match Value.stringOrDefault costume with
    | null -> struct(CurrentIndex, 0)
    | costume ->

    match costumeNameToIndex &c costume with
    | ValueSome index -> struct(FirstIndex, index)
    | _ ->

    //let isSprite = Option.isSome self.entityImage.spriteImage
    match costume, isSprite &c with
    | "next costume", true
    | "next backdrop", false -> struct(CurrentIndex, 1)
    | "previous costume", true
    | "previous backdrop", false -> struct(CurrentIndex, -1)
    | _ ->

    match SValue.tryParseSNumber costume with
    | ValueSome costume -> costumeIndex &c costume
    | _ -> struct(CurrentIndex, 0)

let changeCostumeOfCurrentIndex self addIndex =
    let index = &self.drawingData.currentCostumeIndex
    let length = IArray.length self.entityImage.costumes
    index <- (index + length + addIndex) % length

let changeCostumeIndex self = function
    | struct(FirstIndex, i) -> self.drawingData.currentCostumeIndex <- i
    | struct(CurrentIndex, v) -> changeCostumeOfCurrentIndex self v

let showNextCostume self = changeCostumeIndex self struct(CurrentIndex, 1)
let showPreviousCostume self = changeCostumeIndex self struct(CurrentIndex, -1)

let setCostumeFromIndex self costume =
    let i = costumeIndex (let mutable x = CostumeIndexEnvironment.ofEntityState self in &x) costume
    changeCostumeIndex self i

let setCostume self costume =
    let i = costumeIndexFromValue (let mutable x = CostumeIndexEnvironment.ofEntityState self in &x) costume
    changeCostumeIndex self i

let say self message kind =
    self.spriteDrawingData.sayText <- message
    self.spriteDrawingData.sayKind <- kind
    let Version v as sayVersion = self.sayVersion
    self.sayVersion <- Version(v + 1)
    sayVersion

let clearSay self version =
    if self.sayVersion = version then
        self.spriteDrawingData.sayText <- ""

let getFilter self name =
    let filters = self.drawingData.filters
    match name with
    | "whirl" -> filters.whirl
    | "fisheye" -> filters.fisheye
    | "brightness" -> filters.brightness
    | "pixelate" -> filters.pixelate
    | "mosaic" -> filters.mosaic
    | "color" -> filters.color
    | "ghost" -> filters.ghost
    //| _ when state.blockState.config.useFilterNameCheck ->
    //    cfailwithf state location "unknown filter name: %A" name
    | _ ->
        Map.tryFind name filters.others
        |> VOption.defaultValue 0.

let getFilterWith self filter =
    let filters = self.drawingData.filters
    match filter with
    | Filter.Whirl -> filters.whirl
    | Filter.Fisheye -> filters.fisheye
    | Filter.Brightness -> filters.brightness
    | Filter.Pixelate -> filters.pixelate
    | Filter.Mosaic -> filters.mosaic
    | Filter.Color -> filters.color
    | Filter.Ghost -> filters.ghost
    //| _ when state.blockState.config.useFilterNameCheck ->
    //    cfailwithf state location "unknown filter name: %A" name
    | _ -> 0.

let setFilter self name value =
    let filters = &self.drawingData.filters
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
    //| _ when state.blockState.config.useFilterNameCheck ->
    //        cfailwithf state location "unknown filter name: %A" name
    | _ ->
        let v = clamp (0., 100.) value
        filters.others <- Map.add name v filters.others

let setFilterWith self filter value =
    let filters = &self.drawingData.filters
    match filter with
    | Filter.Whirl -> filters.whirl <- value
    | Filter.Fisheye -> filters.fisheye <- value
    | Filter.Brightness -> filters.brightness <- value
    | Filter.Pixelate -> filters.pixelate <- value
    | Filter.Mosaic -> filters.mosaic <- value
    | Filter.Color ->
        let v = value % 200.
        let v = if v < 0. then v + 200. else v
        filters.color <- clamp (0., 200.) v

    | Filter.Ghost ->
        filters.ghost <- clamp (0., 100.) value
    //| _ when state.blockState.config.useFilterNameCheck ->
    //        cfailwithf state location "unknown filter name: %A" name
    | _ -> ()

let findSoundFromNth soundCount nth =
    let l = soundCount
    if l <> 0 && not (Double.IsNaN nth) then
        let i = Math.Round(nth - 1., MidpointRounding.AwayFromZero) % 1.
        let i = if i < 0. then i + 1. else i
        ValueSome(int i)
    else
        ValueNone

let findSound soundCount soundNameToIndex sound =
    if Value.isNumber sound then
        let index = sound.NumberOrDefault
        findSoundFromNth soundCount index
    elif Value.isBool sound then
        ValueNone
    else
        let name = sound.StringOrDefault
        match Map.tryFind name soundNameToIndex with
        | ValueSome _ as r -> r
        | _ -> findSoundFromNth soundCount (Value.toNumber sound)

let cloneSprite parent = {

    // instance state
    isClone = true
    scalarVariables = Array.copy parent.scalarVariables
    listVariables = parent.listVariables |> Array.map ResizeArray
    drawingData = parent.drawingData
    spriteDrawingData = parent.spriteDrawingData
    sayVersion = Version 0

    // entity metadata
    entityImage = parent.entityImage
    costumeNameToIndex = parent.costumeNameToIndex
    soundNameToIndex = parent.soundNameToIndex

    //s2.costumes = s1.costumes;
    //s2.currentCostumeIndex = s1.currentCostumeIndex;
    //s2.soundRefs = s1.soundRefs;
    //s2.sounds = s1.sounds;
    //s2.listeners = s1.listeners;
    //s2.fns = s1.fns;
    //s2.scripts = s1.scripts;

    //const fs1 = s1.filters
    //const fs2 = s2.filters
    //fs2.color = fs1.color
    //fs2.fisheye = fs1.fisheye
    //fs2.whirl = fs1.whirl
    //fs2.pixelate = fs1.pixelate
    //fs2.mosaic = fs1.mosaic
    //fs2.brightness = fs1.brightness
    //fs2.ghost = fs1.ghost

    //s2.direction = s1.direction;
    //s2.indexInLibrary = s1.indexInLibrary;
    //s2.isDraggable = s1.isDraggable;
    //s2.rotationStyle = s1.rotationStyle;
    //s2.scale = s1.scale;
    //s2.volume = s1.volume;
    //s2.visible = s1.visible;
    //s2.penColor = s1.penColor;

    //// Pen color in RGB mode?
    //s2.penRGBA = s1.penRGBA;
    //// Pen color in RGBA
    //s2.penRed = s1.penRed;
    //s2.penGreen = s1.penGreen;
    //s2.penBlue = s1.penBlue;
    //s2.penAlpha = s1.penAlpha;
    ////Pen color in HSL
    //s2.penHue = s1.penHue;
    //s2.penSaturation = s1.penSaturation;
    //s2.penLightness = s1.penLightness;

    //s2.penSize = s1.penSize;
    //s2.isPenDown = s1.isPenDown;
}


[<Struct; RequireQualifiedAccess; NoEquality; NoComparison>]
type OnSubmit<'a> = private { stage: StageState<'a> } with
    interface IFunc<string, HUnit> with
        member f.Invoke value =
            let stage = f.stage
            let Version version as promptVersion = stage.currentAskVersion
            if promptVersion < stage.nextAskVersion then
                stage.answer <- value
                stage.currentAskVersion <- Version(version + 1)
                if stage.currentAskVersion >= stage.nextAskVersion then
                    stage.showInputBox <- false
            HUnit

let onSubmit stage = { OnSubmit.stage = stage }

let ask (view: _ byref) stage self question =
    if question <> "" then
        match self.entityImage.spriteImage with
        | Some _ when self.spriteDrawingData.visible ->
            say self question Say |> ignore
            StageView.hideStageQuestion &view
        | _ ->
            view.ShowStageQuestion question
    else
        view.HideStageQuestion()

    stage.showInputBox <- true
    let mutable f = onSubmit stage
    view.ShowInputBox("", &f)

[<RequiresExplicitTypeArguments>]
let operandToIndex<'a> (i: _ inref): 'a Index = { index = int32 i.operand1 }

[<Struct>]
type GlideSecs = {
    duration: double
    t1: DateTime
    x1: double
    y1: double
    xDelta: double
    yDelta: double
}
