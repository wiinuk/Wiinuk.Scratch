module Scratch.Evaluator.Expressions
open System
open Scratch
open Scratch.Primitives
open Scratch.Evaluator
open Scratch.Ast
open Scratch.Threading


let letterOf (state: _ inref) location s n =
    let n =
        match state.blockState.config.version with
        | RuntimeVersion.Sb3 -> n
        | RuntimeVersion.Sb2 -> if Double.IsInfinity n then 1. else n

    let n =
        if Double.IsNaN n then 0
        else int n

    if 1 <= n && n <= String.length s then SString(string s[n - 1])
    elif state.blockState.config.useRangeCheck then
        cfailwithf state location $"out of range: letter:of: {s} {n}"
    else
        sEmptyString

let getCostumeName self =
    let costumes = self.shared.costumes
    let index = self.drawingData.currentCostumeIndex
    if 0 <= index && index < IArray.length costumes then (IArray.item index costumes).costumeName
    else ""

let contentsOfList (list: _ ResizeArray) =
    let isSingle = list.Exists(fun x -> match x with SString x -> x.Length <> 1 | _ -> false) |> not
    list
    |> Seq.map SValue.toString
    |> String.concat (if isSingle then "" else " ")

let timeStampEpoch = DateTime(2000, 1, 1)

let distance (x1, y1) (x2, y2) =
    sqrt ((x2 - x1) * (x2 - x1) + (y2 - y2) * (y2 - y1))

let distanceToPoint sprite (x, y) =
    distance (sprite.spriteDrawingData.x, sprite.spriteDrawingData.y) (x, y)

let distanceTo (state: _ inref) sprite = function
    | SString "_mouse_" ->
        let struct(x, y) = Input.getMousePosition &state.blockState.input
        distanceToPoint sprite (x, y)

    | target ->

    match ObjectState.findObject (SValue.toString target) state.blockState with
    | ValueNone
    | ValueSome { sprite = ValueNone } -> 10000.
    | ValueSome { sprite = ValueSome thing } ->

    distanceToPoint sprite (thing.spriteDrawingData.x, thing.spriteDrawingData.y)

let timeAndDate (state: _ inref) = function
    | SString format ->
        let now = Scheduler.now state.blockState.scheduler
        match format with
        | "year" -> now.Year
        | "month" -> now.Month
        | "date" -> now.Day
        | "day of week" -> int now.DayOfWeek + 1
        | "hour" -> now.Hour
        | "minute" -> now.Minute
        | "second" -> now.Second
        | _ -> 0
    | _ -> 0

let computeMathFunction f x =
    match f with
    | "abs" -> abs x
    | "floor" -> floor x
    | "sqrt" -> sqrt x
    | "ceiling" -> ceil x
    | "cos" -> cos (x * Math.PI / 180.)
    | "sin" -> sin (x * Math.PI / 180.)
    | "tan" -> tan (x * Math.PI / 180.)
    | "asin" -> asin x * 180. / Math.PI
    | "acos" -> acos x * 180. / Math.PI
    | "atan" -> atan x * 180. / Math.PI
    | "ln" -> log x
    | "log" -> log10 x
    | "e ^" -> exp x
    | "10 ^" -> exp (x * log 10.)
    | _ -> 0.

let rec evaluateExpression (state: _ inref) = function
    | Literal(value = x) -> x
    | Complex x -> evaluateComplexExpression &state x
    | Block(BlockExpression(location, _)) -> cfailwithf state location "unexpected block"

and evaluateBinaryNumberExpression (state: _ inref) op e1 e2 =
    let x1 = toNumberZeroIfNaN (evaluateExpression &state e1)
    let x2 = toNumberZeroIfNaN (evaluateExpression &state e2)
    SNumber(op x1 x2)

and evaluateComplexExpression (state: _ inref) (ComplexExpression(location, op, ops)) =
    match op, ops with
    | O.getParam, [Literal(_, SString name); Literal(_, SString("r" | "b"))] ->
        match Map.tryFind name state.args with
        | ValueNone ->
            if state.blockState.config.useVariableDefinitionCheck then
                cfailwithf state location $"parameter '{name}' not found"
            else
                // TODO:
                sEmptyString
        | ValueSome x -> x

    | O.costumeName, _ -> SString(getCostumeName state.self)
    | O.sceneName, _ -> SString(getCostumeName state.blockState.stage)

    | O.readVariable, [Literal(_, SString name)] ->
        if Set.contains name state.blockState.clouds then
            getCloudValue &state name
        else
            (getValueOrRaise &state location name).contents

    | O.``contentsOfList:``, [Literal(_, SString name)] -> SString(contentsOfList (getListOrRaise &state location name))

    | O.``getLine:ofList:``, [nth; Literal(_, SString name)] ->
        let xs = getListOrRaise &state location name
        let n =
            match evaluateExpression &state nth with
            | SString("random" | "any") -> double (state.blockState.config.randomNext struct(1, xs.Count + 1))
            | SString "last" -> double xs.Count
            | nth -> SValue.toNumber nth
        let n =
            if Double.IsNaN n || Double.IsInfinity n then 0
            else int n

        if 1 <= n && n <= xs.Count then xs[n - 1]
        elif state.blockState.config.useRangeCheck then
            cfailwithf state location $"out of range: {Symbol.name op} '{name}' '{n}'"
        else
            sEmptyString

    | O.``concatenate:with:``, [e1; e2] ->
        let x1 = evaluateExpression &state e1
        let x2 = evaluateExpression &state e2
        SString(SValue.toString x1 + SValue.toString x2)

    | O.``letter:of:``, [e1; e2] ->
        let x1 = evaluateExpression &state e1
        let x2 = evaluateExpression &state e2
        letterOf &state location (SValue.toString x2) (SValue.toNumber x1)

    | O.answer, _ -> SString state.blockState.sharedState.answer

//     | Case2<"getAttribute:of:", E, E>

    | O.getUserId, _ -> SNumber(double state.blockState.config.userId)
    | O.getUserName, _ -> SString state.blockState.config.userName

// type KnownNumberExpression<E> =
    | O.xpos, [] ->
       match state.self.sprite with
       | ValueSome sprite -> SNumber sprite.spriteDrawingData.x
       | ValueNone -> cfailwithf state location $"sprite only: {Symbol.name op}"

    | O.ypos, [] ->
        match state.self.sprite with
        | ValueSome sprite -> SNumber sprite.spriteDrawingData.y
        | ValueNone -> cfailwithf state location $"sprite only: {Symbol.name op}"

    | O.heading, _ ->
        match state.self.sprite with
        | ValueSome sprite -> SNumber sprite.spriteDrawingData.direction
        | ValueNone -> cfailwithf state location $"sprite only: {Symbol.name op}"

    | O.costumeIndex, _ ->
        SNumber(double (state.self.drawingData.currentCostumeIndex + 1))

    | O.backgroundIndex, _ ->
        SNumber(double (state.blockState.stage.drawingData.currentCostumeIndex + 1))

    | O.scale, _ ->
        match state.self.sprite with
        | ValueSome sprite -> SNumber(sprite.spriteDrawingData.scale * 100.)
        | ValueNone _ -> SNumber 100.

    | O.volume, _ -> SNumber(state.self.drawingData.volume * 100.)

    | O.``lineCountOfList:``, [Literal(_, SString name)] ->
        let xs = getListOrRaise &state location name
        SNumber(double xs.Count)

    | O.``+``, [e1; e2] -> evaluateBinaryNumberExpression &state (+) e1 e2
    | O.``-``, [e1; e2] -> evaluateBinaryNumberExpression &state (-) e1 e2
    | O.``*``, [e1; e2] -> evaluateBinaryNumberExpression &state (*) e1 e2
    | O.``/``, [e1; e2] -> evaluateBinaryNumberExpression &state (/) e1 e2
    | O.``randomFrom:to:``, [e1; e2] ->
        let x1 = evaluateExpression &state e1 |> toNumberZeroIfNaN
        let x2 = evaluateExpression &state e2 |> toNumberZeroIfNaN
        let x1, x2 = if x2 < x1 then x2, x1 else x1, x2
        let r =
            if x1 = x2 then x1
            elif x1 % 1. = 0. && x2 % 1. = 0.
            then floor (state.blockState.config.randomNextDouble() * (x2 - x1 + 1.)) + x1
            else state.blockState.config.randomNextDouble() * (x2 - x1) + x1
            
        SNumber r

    | O.abs, [e] ->
        let x = evaluateExpression &state e
        let x = toNumberZeroIfNaN x
        SNumber(abs x)

    | O.sqrt, [e] ->
        let x = evaluateExpression &state e
        let x = toNumberZeroIfNaN x
        SNumber(sqrt x)

    | O.``stringLength:``, [e] ->
        let x = evaluateExpression &state e
        SNumber(SValue.toString x |> String.length |> double)

    | (O.``%`` | O.``\\``), [e1; e2] ->
        let x1 = evaluateExpression &state e1
        let x2 = evaluateExpression &state e2
        let x1 = toNumberZeroIfNaN x1
        let x2 = toNumberZeroIfNaN x2
        let r = x1 % x2
        let r = if r / x2 < 0. then r + x2 else r
        SNumber r

    | O.rounded, [e1] ->
        let x1 = evaluateExpression &state e1 |> toNumberZeroIfNaN
        SNumber(Math.Round(x1, MidpointRounding.AwayFromZero))

    | O.``computeFunction:of:``, [Literal(_, SString f); e1] ->
        let x = evaluateExpression &state e1 |> toNumberZeroIfNaN
        SNumber(computeMathFunction f x)

    | O.mouseX, _ ->
        let struct(x, _) = Input.getMousePosition &state.blockState.input
        SNumber x

    | O.mouseY, _ ->
        let struct(_, y) = state.blockState.input.GetMousePosition()
        SNumber y

    | O.timer, [] ->
        let now = Scheduler.now state.blockState.scheduler
        let span = now - state.blockState.sharedState.timerEpoch.contents
        SNumber span.TotalSeconds

    | O.``distanceTo:``, [target] ->
        let target = evaluateExpression &state target
        match state.self.sprite with
        | ValueSome sprite -> SNumber(distanceTo &state sprite target)
        | ValueNone -> cfailwithf state location $"sprite only: {Symbol.name op}"

    | O.timestamp, [] ->
        let span = Scheduler.now state.blockState.scheduler - timeStampEpoch
        SNumber span.TotalDays

    | O.timeAndDate, [kind] ->
        let kind = evaluateExpression &state kind
        SNumber(double (timeAndDate &state kind))

    | O.``list:contains:``, [name; value] ->
        let name = evaluateExpression &state name |> SValue.toString
        let value = evaluateExpression &state value

        let list = getListOrRaise &state location name
        SBool(list.Exists(fun x -> SValue.equalsSValue value x))

    | O.``<``, [e1; e2] ->
        let x1 = evaluateExpression &state e1
        let x2 = evaluateExpression &state e2
        SValue.compareSValue x1 x2 < 0 |> SBool

    | O.``>``, [e1; e2] ->
        let x1 = evaluateExpression &state e1
        let x2 = evaluateExpression &state e2
        SValue.compareSValue x1 x2 > 0 |> SBool

    | O.``=``, [e1; e2] ->
        let x1 = evaluateExpression &state e1
        let x2 = evaluateExpression &state e2
        SValue.compareSValue x1 x2 = 0 |> SBool

    | O.``&``, [e1; e2] ->
        if evaluateExpression &state e1 |> SValue.toBool
        then evaluateExpression &state e2 |> SValue.toBool |> SBool
        else sFalse

    | O.``|``, [e1; e2] ->
        if evaluateExpression &state e1 |> SValue.toBool
        then sTrue
        else evaluateExpression &state e2 |> SValue.toBool |> SBool

    | O.not, [e] ->
        evaluateExpression &state e |> SValue.toBool |> not |> SBool

    | O.mousePressed, _ -> SBool(state.blockState.input.IsAnyMouseButtonPressed())

//     | Case1<"touching:", E>
//     | Case1<"touchingColor:", E>
//     | Case2<"color:sees:", E, E>

    | O.``keyPressed:``, [key] ->
        let key = evaluateExpression &state key
        match key with
        | SString "any" -> SBool(state.blockState.input.IsAnyKeyDown())
        | _ -> SBool(state.blockState.input.IsKeyDown(KeyCode.getKeyCode key))

    | opxs -> cfailwithf state location $"not implemented: %A{opxs}"
