namespace Scratch.Executor


// TODO: byte enum
[<RequireQualifiedAccess>]
type Code =
    | Nop

    // literal
    | Number
    | String
    | Bool

    // procedure control
    | Call
    | Return
    | Yield
    | YieldForce
    | YieldSleep

    // control
    | Brunch
    | BrunchIfFalsy

    // storage
    | SetLocal
    | Local
    | Argument
    | Pop
    | Dup

    // special control
    | Broadcast
    | BroadcastAndWait
    | StopScripts
    | CreateCloneOf
    | CreateCloneOfMyself
    | DeleteClone

    // basic operation
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Concat
    | Equal
    | EqualNumberLiteral
    | LessThan
    | GreaterThan
    | Not
    | LetterOf
    | RandomFromTo
    | Abs
    | Sqrt
    | StringLength
    | Round
    | Floor
    | Ceil
    | Cos
    | Sin
    | Tan
    | Asin
    | Acos
    | Atan
    | LogE
    | Log10
    | ExpE
    | Exp10

    // object variable operation
    | SpriteVariable
    | StageVariable
    | CloudVariable
    | SetSpriteVariable
    | SetStageVariable
    | SetCloudVariable

    // object list operation
    | SpriteListCount
    | StageListCount
    | SpriteListLine
    | StageListLine
    | DeleteSpriteListLine
    | DeleteStageListLine
    | AppendSpriteList
    | AppendStageList
    | SetSpriteListLine
    | SetStageListLine
    | StageListContents
    | SpriteListContents
    | StageListContains
    | SpriteListContains
    | InsertAtStageList
    | InsertAtSpriteList

    // object operation
    | Forward
    | DistanceTo
    | PointTowards
    | Goto
    | GotoSpriteOrMouse
    | NextCostume
    | SayOrThink
    | ClearSayOrThink
    | Filter
    | SetFilter
    | FilterWith
    | SetFilterWith
    | SetFilterBrightness
    | SetFilterColor
    | SetFilterGhost
    | ResetAllFilter
    | Show
    | Hide
    | ComeToFront
    | GoBackByLayers
    | PlaySound
    | PlaySoundWith
    | ClearPenTrails
    | SetPenDown
    | SetPenColor
    | ChangePenHue
    | ChangePenShade
    | ChangePenSize
    | StampCostume
    | GlideSecs
    | Ask

    // object field
    | CostumeName
    | StageCostumeName
    | CostumeIndex
    | StageCostumeIndex
    | SetCostume
    | ChangeCostumeWith
    | SetCostumeWith
    | X
    | Y
    | Direction
    | SetDirection
    | Scale
    | SetScale
    | Volume
    | SetVolume
    | SetRotationStyle
    | Tempo
    | SetTempo

    // shared field
    | Answer
    | UserId
    | UserName

    // input
    | MouseX
    | MouseY
    | Timer
    | TimerReset
    | Timestamp
    | TimeAndDate
    | AnyMousePressed
    | KeyPressed
    | AnyKeyPressed
    | KeyPressedWithKeyCode

    | TouchingColor

    | ShowVariable

[<RequireQualifiedAccess>]
type FlowControl =
    | Next
    | Call
    | Return
    | Yield
    | Brunch
    | ConditionalBrunch

[<RequireQualifiedAccess>]
type OperandTypeCode =
    | Void
    | Double
    | Bool
    | StringIndex
    | Int32
    | Offset
    | KeyCode
    | Filter
    | ProcedureIndex
    | StageVariableIndex
    | SpriteVariableIndex
    | StageListIndex
    | SpriteListIndex
    | LocalIndex
    | ArgumentIndex
    | BroadcastIndex

[<RequireQualifiedAccess>]
type PopBehaviour =
    | Pop0
    | Pop1
    | Pop2
    | Pop3
    | Var

[<RequireQualifiedAccess>]
type PushBehaviour =
    | Push0
    | Push1
    | Push2

type CodeMetadata = {

    /// e.g. ["setVar:to:"]
    internalName: string list

    flowControl: FlowControl
    operandTypeCode: OperandTypeCode

    popBehaviour: PopBehaviour
    pushBehaviour: PushBehaviour
}
module CodeMetadata =
    type private T = OperandTypeCode
    type private F = FlowControl
    type private Pop = PopBehaviour
    type private Push = PushBehaviour

    let private name0 = []
    let private name1 x = [x]

    /// Next, Void, 1 -> 1
    let private basicUnary name = {
        internalName = name1 name
        flowControl = F.Next
        operandTypeCode = T.Void
        popBehaviour = Pop.Pop1
        pushBehaviour = Push.Push1
    }
    /// Next, Void, 2 -> 1
    let private basicBinary name = {
        internalName = name1 name
        flowControl = F.Next
        operandTypeCode = T.Void
        popBehaviour = Pop.Pop2
        pushBehaviour = Push.Push1
    }
    /// Next, Void, 0 -> 1
    let private basicPropertyGet' name = {
        internalName = name
        flowControl = F.Next
        operandTypeCode = T.Void
        popBehaviour = Pop.Pop0
        pushBehaviour = Push.Push1
    }
    /// Next, Void, 0 -> 1
    let private basicPropertyGet name = basicPropertyGet' (name1 name)
    /// Next, Void, 1 -> 0
    let private basicPropertySet' name = {
        internalName = name
        flowControl = F.Next
        operandTypeCode = T.Void
        popBehaviour = Pop.Pop1
        pushBehaviour = Push.Push0
    }
    /// Next, Void, 1 -> 0
    let private basicPropertySet name = basicPropertySet' (name1 name)
    /// Next, Void, pop -> 0
    let private basicActionN pop name = {
        internalName = name1 name
        flowControl = F.Next
        operandTypeCode = T.Void
        popBehaviour = pop
        pushBehaviour = Push.Push0
    }
    /// Next, Void, 0 -> 0
    let private basicAction0 name = basicActionN Pop.Pop0 name
    /// Next, Void, 1 -> 0
    let private basicAction1 name = basicActionN Pop.Pop1 name
    /// Next, Void, 2 -> 0
    let private basicAction2 name = basicActionN Pop.Pop1 name

    let private readVariable operandTypeCode = {
        internalName = name1 "readVariable"
        flowControl = F.Next
        operandTypeCode = operandTypeCode
        popBehaviour = Pop.Pop0
        pushBehaviour = Push.Push1
    }
    let private setVariable operandTypeCode = {
        internalName = name1 "setVar:to:"
        flowControl = F.Next
        operandTypeCode = operandTypeCode
        popBehaviour = Pop.Pop1
        pushBehaviour = Push.Push0
    }

    let metadata = function
        | Code.Nop ->
            {
                internalName = name0
                flowControl = F.Next
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push0
            }
        | Code.Number ->
            {
                internalName = name0
                flowControl = F.Next
                operandTypeCode = T.Double
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push1
            }
        | Code.String ->
            {
                internalName = name0
                flowControl = F.Next
                operandTypeCode = T.StringIndex
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push1
            }
        | Code.Bool ->
            {
                internalName = name0
                flowControl = F.Next
                operandTypeCode = T.Bool
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push1
            }
        | Code.Call ->
            {
                internalName = name1 "call"
                flowControl = F.Call
                operandTypeCode = T.ProcedureIndex
                popBehaviour = Pop.Var
                pushBehaviour = Push.Push0
            }
        | Code.Return ->
            {
                internalName = name0
                flowControl = F.Return
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push0
            }
        | Code.Yield ->
            {
                internalName = name0
                flowControl = F.Yield
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push0
            }
        | Code.YieldForce ->
            {
                internalName = name0
                flowControl = F.Yield
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push0
            }
        | Code.YieldSleep ->
            {
                internalName = name0
                flowControl = F.Yield
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push0
            }
        | Code.Brunch ->
            {
                internalName = name0
                flowControl = F.Brunch
                operandTypeCode = T.Offset
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push0
            }
        | Code.BrunchIfFalsy ->
            {
                internalName = name0
                flowControl = F.ConditionalBrunch
                operandTypeCode = T.Offset
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push0
            }
        | Code.SetLocal ->
            {
                internalName = name0
                flowControl = F.Next
                operandTypeCode = T.LocalIndex
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push0
            }
        | Code.Local ->
            {
                internalName = name0
                flowControl = F.Next
                operandTypeCode  = T.LocalIndex
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push1
            }
        | Code.Argument ->
            {
                internalName = name1 "getParam"
                flowControl = F.Next
                operandTypeCode = T.ArgumentIndex
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push1
            }
        | Code.Pop ->
            {
                internalName = name0
                flowControl = F.Next
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push0
            }
        | Code.Dup ->
            {
                internalName = name0
                flowControl = F.Next
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push2
            }
        | Code.Broadcast ->
            {
                internalName = name1 "broadcast"
                flowControl = F.Next
                operandTypeCode = T.BroadcastIndex
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push0
            }
        | Code.BroadcastAndWait ->
            {
                internalName = name1 "doBroadcastAndWait"
                flowControl = F.Yield
                operandTypeCode = T.BroadcastIndex
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push0
            }
        | Code.StopScripts ->
            {
                internalName = ["stopScripts"]
                flowControl = F.Yield
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push0
            }
        | Code.DeleteClone ->
            {
                internalName = ["deleteClone"]
                flowControl = F.Return
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push0
            }
        | Code.Add -> basicBinary "+"
        | Code.Sub -> basicBinary "-"
        | Code.Mul -> basicBinary "*"
        | Code.Div -> basicBinary "/"
        | Code.Mod ->
            {
                internalName = ["%"; "\\\\"]
                flowControl = F.Next
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop2
                pushBehaviour = Push.Push1
            }
        | Code.Concat -> basicBinary "concatenate:with:"
        | Code.Equal -> basicBinary "="
        | Code.EqualNumberLiteral ->
            {
                internalName = name1 "="
                flowControl = F.Next
                operandTypeCode = T.Double
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push1
            }
        | Code.LessThan -> basicBinary "<"
        | Code.GreaterThan -> basicBinary ">"
        | Code.Not -> basicUnary "not"
        | Code.LetterOf -> basicBinary "letter:of:"
        | Code.RandomFromTo -> basicBinary "randomFrom:to:"

        | Code.Abs -> basicUnary "abs"
        | Code.Sqrt -> basicUnary "sqrt"
        | Code.StringLength -> basicUnary "stringLength:"
        | Code.Round -> basicUnary "rounded"
        | Code.Floor -> basicUnary "computeFunction:of:"
        | Code.Ceil -> basicUnary "computeFunction:of:"
        | Code.Cos -> basicUnary "computeFunction:of:"
        | Code.Sin -> basicUnary "computeFunction:of:"
        | Code.Tan -> basicUnary "computeFunction:of:"
        | Code.Asin -> basicUnary "computeFunction:of:"
        | Code.Acos -> basicUnary "computeFunction:of:"
        | Code.Atan -> basicUnary "computeFunction:of:"
        | Code.LogE -> basicUnary "computeFunction:of:"
        | Code.Log10 -> basicUnary "computeFunction:of:"
        | Code.ExpE -> basicUnary "computeFunction:of:"
        | Code.Exp10 -> basicUnary "computeFunction:of:"

        | Code.SpriteVariable -> readVariable T.SpriteVariableIndex
        | Code.StageVariable -> readVariable T.StageVariableIndex
        | Code.CloudVariable -> readVariable T.StringIndex
        | Code.SetSpriteVariable -> setVariable T.SpriteVariableIndex
        | Code.SetStageVariable -> setVariable T.StageVariableIndex
        | Code.SetCloudVariable -> setVariable T.StringIndex

        | Code.SpriteListCount ->
            {
                internalName = name1 "lineCountOfList:"
                flowControl = F.Next
                operandTypeCode = T.SpriteListIndex
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push1
            }
        | Code.StageListCount ->
            {
                internalName = name1 "lineCountOfList:"
                flowControl = F.Next
                operandTypeCode = T.StageListIndex
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push1
            }
        | Code.SpriteListLine ->
            {
                internalName = name1 "getLine:ofList:"
                flowControl = F.Next
                operandTypeCode = T.SpriteListIndex
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push1
            }
        | Code.StageListLine ->
            {
                internalName = name1 "getLine:ofList:"
                flowControl = F.Next
                operandTypeCode = T.StageListIndex
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push1
            }
        | Code.DeleteSpriteListLine ->
            {
                internalName = name1 "deleteLine:ofList:"
                flowControl = F.Next
                operandTypeCode = T.SpriteListIndex
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push0
            }
        | Code.DeleteStageListLine ->
            {
                internalName = name1 "deleteLine:ofList:"
                flowControl = F.Next
                operandTypeCode = T.StageListIndex
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push0
            }
        | Code.AppendSpriteList ->
            {
                internalName = name1 "append:toList:"
                flowControl = F.Next
                operandTypeCode = T.SpriteListIndex
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push0
            }
        | Code.AppendStageList ->
            {
                internalName = name1 "append:toList:"
                flowControl = F.Next
                operandTypeCode = T.StageListIndex
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push0
            }
        | Code.SetSpriteListLine ->
            {
                internalName = name1 "setLine:ofList:to:"
                flowControl = F.Next
                operandTypeCode = T.SpriteListIndex
                popBehaviour = Pop.Pop2
                pushBehaviour = Push.Push0
            }
        | Code.SetStageListLine ->
            {
                internalName = name1 "setLine:ofList:to:"
                flowControl = F.Next
                operandTypeCode = T.StageListIndex
                popBehaviour = Pop.Pop2
                pushBehaviour = Push.Push0
            }
        | Code.StageListContents ->
            {
                internalName = name1 "contentsOfList:"
                flowControl = F.Next
                operandTypeCode = T.StageListIndex
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push1
            }
        | Code.SpriteListContents ->
            {
                internalName = name1 "contentsOfList:"
                flowControl = F.Next
                operandTypeCode = T.SpriteListIndex
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push1
            }
        | Code.StageListContains ->
            {
                internalName = name1 "list:contains:"
                flowControl = F.Next
                operandTypeCode = T.StageListIndex
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push1
            }
        | Code.SpriteListContains ->
            {
                internalName = name1 "list:contains:"
                flowControl = F.Next
                operandTypeCode = T.SpriteListIndex
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push1
            }
        | Code.InsertAtStageList ->
            {
                internalName = name1 "insert:at:ofList:"
                flowControl = F.Next
                operandTypeCode = T.StageListIndex
                popBehaviour = Pop.Pop2
                pushBehaviour = Push.Push0
            }
        | Code.InsertAtSpriteList ->
            {
                internalName = name1 "insert:at:ofList:"
                flowControl = F.Next
                operandTypeCode = T.SpriteListIndex
                popBehaviour = Pop.Pop2
                pushBehaviour = Push.Push0
            }

        | Code.Forward -> basicAction1 "forward:"
        | Code.DistanceTo -> basicUnary "distanceTo:"
        | Code.PointTowards -> basicAction1 "pointTowards:"
        | Code.Goto -> basicAction2 "gotoX:y:"
        | Code.GotoSpriteOrMouse -> basicAction1 "gotoSpriteOrMouse:"
        | Code.NextCostume -> basicAction0 "nextCostume"
        | Code.SayOrThink ->
            {
                internalName = ["say:duration:elapsed:from:"; "say:"; "think:duration:elapsed:from:"; "think:"]
                flowControl = F.Next
                operandTypeCode = T.Bool // isSay
                popBehaviour = Pop.Pop1 // message
                pushBehaviour = Push.Push1 // version
            }
        | Code.ClearSayOrThink ->
            {
                internalName = ["say:duration:elapsed:from:"; "think:duration:elapsed:from:"]
                flowControl = F.Next
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push0
            }
        | Code.Filter ->
            {
                internalName = ["changeGraphicEffect:by:"]
                flowControl = F.Next
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop1 // filterName
                pushBehaviour = Push.Push1 // filterValue
            }
        | Code.SetFilter ->
            {
                internalName = ["changeGraphicEffect:by:"; "setGraphicEffect:to:"]
                flowControl = F.Next
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop2 // filterName, value
                pushBehaviour = Push.Push0
            }
        | Code.FilterWith ->
            {
                internalName = ["changeGraphicEffect:by:"]
                flowControl = F.Next
                operandTypeCode = T.Filter
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push1
            }
        | Code.SetFilterWith ->
            {
                internalName = ["changeGraphicEffect:by:"; "setGraphicEffect:to:"]
                flowControl = F.Next
                operandTypeCode = T.Filter
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push0
            }
        | Code.SetFilterBrightness
        | Code.SetFilterColor
        | Code.SetFilterGhost -> basicPropertySet' ["changeGraphicEffect:by:"; "setGraphicEffect:to:"]
        | Code.ResetAllFilter -> basicAction0 "filterReset"
        | Code.Show -> basicAction0 "show"
        | Code.Hide -> basicAction0 "hide"
        | Code.ComeToFront -> basicAction0 "comeToFront"
        | Code.GoBackByLayers -> basicAction1 "goBackByLayers:"
        | Code.PlaySound -> basicAction1 "playSound:"
        | Code.PlaySoundWith ->
            {
                internalName = ["playSound:"]
                flowControl = F.Next
                operandTypeCode = T.Int32
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push0
            }

        | Code.ClearPenTrails -> basicAction0 "clearPenTrails"
        | Code.SetPenDown ->
            {
                internalName = ["putPenDown"]
                flowControl = F.Next
                operandTypeCode = T.Bool
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push0
            }
        | Code.SetPenColor -> basicPropertySet "penColor:"
        | Code.ChangePenHue ->
            {
                internalName = ["setPenHueTo:"; "changePenHueBy:"]
                flowControl = F.Next
                operandTypeCode = T.Int32 // old value ratio
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push0
            }
        | Code.ChangePenShade ->
            {
                internalName = ["setPenShadeTo:"; "changePenShadeBy:"]
                flowControl = F.Next
                operandTypeCode = T.Int32 // old value ratio
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push0
            }
        | Code.ChangePenSize ->
            {
                internalName = ["penSize:"; "changePenSizeBy:"]
                flowControl = F.Next
                operandTypeCode = T.Int32 // old value ratio
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push0
            }
        | Code.StampCostume -> basicAction0 "stampCostume"
        | Code.GlideSecs ->
            {
                internalName = ["glideSecs:toX:y:elapsed:from:"]
                flowControl = F.Yield
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop3
                pushBehaviour = Push.Push0
            }
        | Code.CreateCloneOf ->
            {
                internalName = ["createCloneOf"]
                flowControl = F.Next
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push0
            }
        | Code.CreateCloneOfMyself ->
            {
                internalName = ["createCloneOf"]
                flowControl = F.Next
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push0
            }
        | Code.Ask ->
            {
                internalName = ["doAsk"]
                flowControl = F.Yield
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push0
            }

        | Code.StageCostumeName -> basicPropertyGet "costumeName"
        | Code.CostumeName -> basicPropertyGet "sceneName"
        | Code.StageCostumeIndex -> basicPropertyGet "backgroundIndex"
        | Code.CostumeIndex -> basicPropertyGet "costumeIndex"
        | Code.SetCostume -> basicPropertySet "lookLike:"
        | Code.ChangeCostumeWith ->
            {
                internalName = ["lookLike:"]
                flowControl = F.Next
                operandTypeCode = T.Int32
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push0
            }
        | Code.SetCostumeWith ->
            {
                internalName = ["lookLike:"]
                flowControl = F.Next
                operandTypeCode = T.Int32
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push0
            }
        | Code.Answer -> basicPropertyGet "answer"
        | Code.UserId -> basicPropertyGet "getUserId"
        | Code.UserName -> basicPropertyGet "getUserName"
        | Code.X -> basicPropertyGet "xpos"
        | Code.Y -> basicPropertyGet "ypos"
        | Code.Direction -> basicPropertyGet "heading"
        | Code.SetDirection -> basicPropertySet "heading:"
        | Code.Scale -> basicPropertyGet "scale"
        | Code.SetScale -> basicPropertySet' ["setSizeTo:"; "changeSizeBy:"]
        | Code.Volume -> basicPropertyGet "volume"
        | Code.SetVolume -> basicPropertySet' ["changeVolumeBy:"; "setVolumeTo:"]
        | Code.SetRotationStyle -> basicPropertySet "setRotationStyle"
        | Code.Tempo -> basicPropertyGet "changeTempoBy:"
        | Code.SetTempo -> basicPropertySet' ["changeTempoBy:"; "setTempoTo:"]

        | Code.MouseX -> basicPropertyGet "mouseX"
        | Code.MouseY -> basicPropertyGet "mouseY"
        | Code.Timer -> basicPropertyGet "timer"
        | Code.TimerReset -> basicAction0 "timerReset"
        | Code.Timestamp -> basicPropertyGet "timestamp"
        | Code.TimeAndDate -> basicUnary "timeAndDate"

        | Code.AnyMousePressed -> basicPropertyGet "mousePressed"
        | Code.KeyPressed -> basicUnary "keyPressed:"
        | Code.AnyKeyPressed -> basicPropertyGet "keyPressed:"
        | Code.KeyPressedWithKeyCode ->
            {
                internalName = ["keyPressed:"]
                flowControl = F.Next
                operandTypeCode = T.KeyCode
                popBehaviour = Pop.Pop0
                pushBehaviour = Push.Push1
            }

        | Code.TouchingColor ->
            {
                internalName = ["touchingColor:"]
                flowControl = F.Next
                operandTypeCode = T.Void
                popBehaviour = Pop.Pop1
                pushBehaviour = Push.Push1
            }

        | Code.ShowVariable ->
            {
                internalName = ["showVariable:"; "showList:"; "hideVariable:"; "hideList:"]
                flowControl = F.Next
                operandTypeCode = T.StringIndex
                popBehaviour = Pop.Pop1 // bool: isShow
                pushBehaviour = Push.Push0
            }
