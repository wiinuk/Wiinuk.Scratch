module Scratch.Executor.Compiler
open System
open Scratch
open Scratch.Primitives
open Scratch.Ast
open Scratch.Executor
open Scratch.Executor.CodeBuilder
open Scratch.Executor.Blocks


type ProcedureId = ProcedureId of procedureName: string * selfName: string

type CompilerConfig = {
    convertStringLiteralsToNumbersIfPossible: bool
}
module CompilerConfig =
    let defaultConfig = {
        convertStringLiteralsToNumbersIfPossible = true
    }

type VariableHome = Stage | Sprite
type VariableStorage =
    | LocalStorage of VariableHome * Value Index
    | CloudStorage

type EmitEnvironment<'Ex,'a> = {
    config: CompilerConfig

    builder: CodeBuilder
    procedures: Procedure ResizeArray
    instructionIndexToLocation: Map<Instruction index, 'a> ref

    stage: StageData<'a>
    entity: EntityData<'Ex,'a>
    sprite: SpriteDataExtension option

    variableNameToStorage: Map<string, VariableStorage>
    parameterNameToIndex: Map<string, Value index>
    procedureNameToIndex: Map<ProcedureId, Procedure index> Lazy
    broadcastNameToIndex: Map<LowerName, Broadcast index> Lazy
    costumeNameToIndex: Map<string, int> Lazy
    soundNameToIndex: Map<string, int> Lazy
}
let registerLocationAtNextBuilderIndex env location =
    let index = CodeBuilder.offset env.builder
    env.instructionIndexToLocation := Map.add index location !env.instructionIndexToLocation

[<Struct>]
type EmitEnvironmentWithLocation<'Ex,'a> = {
    withoutLocation: EmitEnvironment<'Ex,'a>
    location: 'a
}
let withLocation location env = { withoutLocation = env; location = location }
let emit' def e code operand =
    registerLocationAtNextBuilderIndex e.withoutLocation e.location
    emit def e.withoutLocation.builder code operand

let emit e code = emit' DVoid e code ()
let defineLabel name e = defineLabel name e.withoutLocation.builder
let markLabel e label = markLabel e.withoutLocation.builder label

let resolveScalarVariable env name =
    Map.tryFind name env.variableNameToStorage

let resolveListVariable env name =
    match env.entity.lists |> List.tryFindIndex (fun v -> v.listName = name) with
    | Some i ->
        let home = if env.entity.objName = env.stage.objName then Stage else Sprite
        Some(home, Index.create<Value ResizeArray> i)

    | None ->

    match env.stage.lists |> List.tryFindIndex (fun v -> v.listName = name) with
    | Some i -> Some(Stage, Index.create<Value ResizeArray> i)

    // TODO: undeclared variable to stage variable
    | None -> None

let (|NumberLikeLiteral|_|) x =
    let mutable result = 0.
    if Value.tryToNumberWithoutNaN (Value.ofSValue x) &result then
        Some result
    else
        None

let filterCode = function
    | Literal(_, SString filterName) ->
        match filterName with
        | "whirl" -> Some(Filter.Whirl, None)
        | "fisheye" -> Some(Filter.Fisheye, None)
        | "brightness" -> Some(Filter.Brightness, Some Code.SetFilterBrightness)
        | "pixelate" -> Some(Filter.Pixelate, None)
        | "mosaic" -> Some(Filter.Mosaic, None)
        | "color" -> Some(Filter.Color, Some Code.SetFilterColor)
        | "ghost" -> Some(Filter.Ghost, Some Code.SetFilterGhost)
        | _ -> None
    | _ -> None

let emitLiteral env = function
    | SNumber x -> emit' DDouble env Code.Number x
    | SString x ->
        if env.withoutLocation.config.convertStringLiteralsToNumbersIfPossible then
            match SValue.tryParseSNumber x with
            | ValueSome n -> emit' DDouble env Code.Number n
            | _ -> emit' DString env Code.String x
        else
            emit' DString env Code.String x

    | SBool x -> emit' DBool env Code.Bool x
    
let rec emitExpression env = function
    | Literal(location, x) -> emitLiteral (withLocation location env) x
    | Complex x -> emitComplexExpression env x
    | Block _ -> failwithf "unexpeted block" // emitBlock env x

and emitBinaryExpression env e1 e2 code =
    emitExpression env.withoutLocation e1
    emitExpression env.withoutLocation e2
    emit env code

and emitComplexExpression env (ComplexExpression(location, operator, operands)) =
    let b = withLocation location env

    match operator, operands with
    | O.getParam, [Literal(_, SString name); Literal(_, SString("r" | "b"))] ->
        match Map.tryFind name env.parameterNameToIndex with
        | ValueSome index -> emit' DArgument b Code.Argument index

        // TODO:
        | ValueNone -> emit' DString b Code.String ""

    | O.costumeName, _ -> emit b Code.CostumeName
    | O.sceneName, _ -> emit b Code.StageCostumeName

    | O.readVariable, [Literal(_, SString name)] ->
        match resolveScalarVariable env name with
        | ValueSome(LocalStorage(Stage, i)) -> emit' DScalar b Code.StageVariable i
        | ValueSome(LocalStorage(Sprite, i)) -> emit' DScalar b Code.SpriteVariable i
        | ValueSome CloudStorage -> emit' DString b Code.CloudVariable name
        | ValueNone -> emit' DString b Code.String ""

    | O.``contentsOfList:``, [Literal(_, SString name)] ->
        match resolveListVariable env name with
        | Some(Stage, i) -> emit' DList b Code.StageListContents i
        | Some(Sprite, i) -> emit' DList b Code.SpriteListContents i
        | None -> emit' DString b Code.String ""

    | O.``getLine:ofList:``, [nth; Literal(_, SString name)] ->
        emitExpression env nth
        match resolveListVariable env name with
        | Some(Stage, i) -> emit' DList b Code.StageListLine i
        | Some(Sprite, i) -> emit' DList b Code.SpriteListLine i
        | None -> emit b Code.Pop

    | O.``list:contains:``, [Literal(_, SString name); value] ->
        emitExpression env value
        match resolveListVariable env name with
        | Some(Stage, i) -> emit' DList b Code.StageListContains i
        | Some(Sprite, i) -> emit' DList b Code.SpriteListContains i
        | None -> emit b Code.Pop

    | O.``concatenate:with:``, [e1; e2] -> emitBinaryExpression b e1 e2 Code.Concat

    | O.``letter:of:``, [e1; e2] ->
        emitExpression env e1
        emitExpression env e2
        emit b Code.LetterOf

    | O.answer, _ -> emit b Code.Answer


//     | Case2<"getAttribute:of:", E, E>

    | O.getUserId, _ -> emit b Code.UserId
    | O.getUserName, _ -> emit b Code.UserName

// type KnownNumberExpression<E> =
    | O.xpos, [] -> emit b Code.X
    | O.ypos, [] -> emit b Code.Y
    | O.heading, _ -> emit b Code.Direction
    | O.costumeIndex, _ -> emit b Code.CostumeIndex
    | O.backgroundIndex, _ -> emit b Code.StageCostumeIndex
    | O.scale, _ -> emit b Code.Scale
    | O.volume, _ -> emit b Code.Volume

    | O.``lineCountOfList:``, [Literal(_, SString name)] ->
        match resolveListVariable env name with
        | Some(Stage, i) -> emit' DList b Code.StageListCount i
        | Some(Sprite, i) -> emit' DList b Code.SpriteListCount i

        // TODO:
        | None -> emit' DDouble b Code.Number 0.

    | O.``+``, [e1; e2] -> emitBinaryExpression b e1 e2 Code.Add
    | O.``-``, [e1; e2] -> emitBinaryExpression b e1 e2 Code.Sub
    | O.``*``, [e1; e2] -> emitBinaryExpression b e1 e2 Code.Mul
    | O.``/``, [e1; e2] -> emitBinaryExpression b e1 e2 Code.Div
    
    | O.``randomFrom:to:``, [e1; e2] ->
        emitExpression env e1
        emitExpression env e2
        emit b Code.RandomFromTo

    | O.abs, [e] ->
        emitExpression env e
        emit b Code.Abs

    | O.sqrt, [e] ->
        emitExpression env e
        emit b Code.Sqrt

    | O.``stringLength:``, [e] ->
        emitExpression env e
        emit b Code.StringLength

    | (O.``%`` | O.``\\``), [e1; e2] ->
        emitExpression env e1
        emitExpression env e2
        emit b Code.Mod

    | O.rounded, [e1] ->
        emitExpression env e1
        emit b Code.Round

    | O.``computeFunction:of:``, [Literal(_, SString f); e1] ->
        emitExpression env e1
        
        let code =
            match f with
            | "abs" -> Some Code.Abs
            | "floor" -> Some Code.Floor
            | "sqrt" -> Some Code.Sqrt
            | "ceiling" -> Some Code.Ceil
            | "cos" -> Some Code.Cos
            | "sin" -> Some Code.Sin
            | "tan" -> Some Code.Tan
            | "asin" -> Some Code.Asin
            | "acos" -> Some Code.Acos
            | "atan" -> Some Code.Atan
            | "ln" -> Some Code.LogE
            | "log" -> Some Code.Log10
            | "e ^" -> Some Code.ExpE
            | "10 ^" -> Some Code.Exp10
            | _ -> None

        match code with
        | Some code -> emit b code
        | None ->
            emit b Code.Pop
            emit' DDouble b Code.Number 0.

    | O.mouseX, _ -> emit b Code.MouseX
    | O.mouseY, _ -> emit b Code.MouseY
    | O.timer, [] -> emit b Code.Timer
    | O.timestamp, [] -> emit b Code.Timestamp
    | O.timeAndDate, [kind] ->
        emitExpression env kind
        emit b Code.TimeAndDate

    | O.``distanceTo:``, [target] ->
        emitExpression env target
        emit b Code.DistanceTo

    | O.``<``, [e1; e2] ->
        emitExpression env e1
        emitExpression env e2
        emit b Code.LessThan

    | O.``>``, [e1; e2] ->
        emitExpression env e1
        emitExpression env e2
        emit b Code.GreaterThan

    | O.``=``, [e1; e2] ->
        match e1, e2 with
        | Literal(_, NumberLikeLiteral n1), _ ->
            emitExpression env e2
            emit' DDouble b Code.EqualNumberLiteral n1
        | _, Literal(_, NumberLikeLiteral n2) ->
            emitExpression env e1
            emit' DDouble b Code.EqualNumberLiteral n2
        | _ ->
            emitBinaryExpression b e1 e2 Code.Equal

    // %e1                      // ...xs, value
    // Dup                      // ...xs, value, value
    // BrunchIfFalsy endAndExpr // ...xs, value
    //     Pop                  // ...xs
    //     %e2                  // ...xs, value
    // endAndExpr:              // ...xs, value
    | O.``&``, [e1; e2] ->
        let endAndExpr = defineLabel "endAndExpr" b

        emitExpression env e1
        emit b Code.Dup
        emit' DLabel b Code.BrunchIfFalsy endAndExpr
        begin
            emit b Code.Pop
            emitExpression env e2
        end
        markLabel b endAndExpr

    // %e1                      // ...xs, value
    // Dup                      // ...xs, value, value
    // BrunchIfFalsy ifFalse    // ...xs, value
    //     Brunch endOrExpr     // ...xs, value
    // ifFalse:
    //     Pop                  // ...xs
    //     %e2                  // ...xs, value
    // endOrExpr:               // ...xs, value
    | O.``|``, [e1; e2] ->
        let ifFalse = defineLabel "beginIfFalse" b
        let endOrExpr = defineLabel "endOrExpr" b

        emitExpression env e1
        emit b Code.Dup
        emit' DLabel b Code.BrunchIfFalsy ifFalse
        begin
            emit' DLabel b Code.Brunch endOrExpr
        end
        markLabel b ifFalse
        begin
            emit b Code.Pop
            emitExpression env e2
        end
        markLabel b endOrExpr

    | O.not, [e] ->
        emitExpression env e
        emit b Code.Not

    | O.mousePressed, _ -> emit b Code.AnyMousePressed

//     | Case1<"touching:", E>
    | O.``touchingColor:``, [color] ->
        emitExpression env color
        emit b Code.TouchingColor
//     | Case2<"color:sees:", E, E>

    | O.``keyPressed:``, [key] ->
        match key with
        | Literal(_, SString "any") -> emit b Code.AnyKeyPressed
        | Literal(_, key) ->
            let key = Value.ofSValue key
            let keyCode = Blocks.getKeyCode key
            emit' DKeyCode b Code.KeyPressedWithKeyCode keyCode

        | _ ->
            emitExpression env key
            emit b Code.KeyPressed

    | opxs -> failwithf "not implemented: %A" opxs

let rec emitStatement env (ComplexExpression(location, operator, operands)) =
    let b = withLocation location env

    match operator, operands with
    | O.call, Literal(_, SString name)::es ->
        for e in es do emitExpression env e
        let index = Map.find (ProcedureId(selfName = env.entity.objName, procedureName = name)) env.procedureNameToIndex.Value
        emit' DProcedure b Code.Call index

    | O.``forward:``, [steps] ->
        emitExpression env steps
        emit b Code.Forward

    | O.``turnRight:``, [x1] ->
        emitExpression env x1       // ...xs, value
        emit b Code.Direction       // ...xs, value, direction
        emit b Code.Add             // ...xs, (value+direction)
        emit b Code.SetDirection    // ...xs

    | O.``turnLeft:``, [x1] ->
        emitExpression env x1       // ...xs, value
        emit b Code.Direction       // ...xs, value, direction
        emit b Code.Sub             // ...xs, (value-direction)
        emit b Code.SetDirection    // ...xs

    | O.``heading:``, [degrees] ->
        emitExpression env degrees  // ...xs, degrees
        emit b Code.SetDirection    // ...xs

    | O.``pointTowards:``, [target] ->
        emitExpression env target
        emit b Code.PointTowards

    | O.``gotoX:y:``, [e1; e2] ->
        emitExpression env e1
        emitExpression env e2
        emit b Code.Goto

    | O.``gotoSpriteOrMouse:``, [target] ->
        emitExpression env target
        emit b Code.GotoSpriteOrMouse

    // TODO: optimize
    | O.``changeXposBy:``, [moveX] ->
        emit b Code.X               // ..., x
        emitExpression env moveX    // ..., x, moveX
        emit b Code.Add             // ..., (x+moveX)
        emit b Code.Y               // ..., (x+moveX), y
        emit b Code.Goto            // ...

    // TODO: optimize
    | O.``xpos:``, [positionX] ->
        emitExpression env positionX    // ..., newX
        emit b Code.Y                   // ..., newX, y
        emit b Code.Goto                // ...

    // TODO: optimize
    | O.``changeYposBy:``, [moveY] ->
        emit b Code.X               // ..., x
        emit b Code.Y               // ..., x, y
        emitExpression env moveY    // ..., x, y, moveY
        emit b Code.Add             // ..., x, (y+moveY)
        emit b Code.Goto            // ...

    // TODO: optimize
    | O.``ypos:``, [positionY] ->
        emit b Code.X                   // ..., x
        emitExpression env positionY    // ..., x, newY
        emit b Code.Goto                // ...

//     | Case0<"bounceOffEdge">

    | O.setRotationStyle, [style] ->
        emitExpression env style
        emit b Code.SetRotationStyle

    | O.``lookLike:``, [costume] ->
        match costume with
        | Literal(_, costume) ->
            let mutable e = CostumeIndexEnvironment.poly {
                new ICostumeIndexEnvironment with
                    member _.CostumeCount = env.entity.costumes |> List.length
                    member _.CostumeNameToIndex name = env.costumeNameToIndex.Value |> Map.tryFind name
                    member _.IsSprite = env.sprite |> Option.isSome
            }
            match costumeIndexFromValue &e (Value.ofSValue costume) with
            | struct(CurrentIndex, 0) -> ()
            | struct(CurrentIndex, n) -> emit' DInt32 b Code.ChangeCostumeWith n
            | struct(FirstIndex, index) -> emit' DInt32 b Code.SetCostumeWith index

        | _ ->
            emitExpression env costume
            emit b Code.SetCostume

    | O.nextCostume, _ ->
        emit b Code.NextCostume

//     | Case1<"showBackground:", E>
//     | Case1<"startScene", E>
//     | Case0<"nextBackground">
//     | Case0<"nextScene">
//     | Case1<"startSceneAndWait", E>

    | O.``say:duration:elapsed:from:``, [message; seconds] ->
        emitExpression env message          // ..., message
        emit' DBool b Code.SayOrThink true  // ..., sayVersion
        emitExpression env seconds          // ..., sayVersion, seconds
        emit b Code.YieldSleep              // ..., sayVersion
        emit b Code.ClearSayOrThink         // ...

    | O.``say:``, [message] ->
        emitExpression env message          // ..., message
        emit' DBool b Code.SayOrThink true  // ..., sayVersion
        emit b Code.Pop                     // ...

    | O.``think:duration:elapsed:from:``, [message; seconds] ->
        emitExpression env message          // ..., message
        emit' DBool b Code.SayOrThink false // ..., version
        emitExpression env seconds          // ..., version, seconds
        emit b Code.YieldSleep              // ..., version
        emit b Code.ClearSayOrThink         // ...

    | O.``think:``, [message] ->
        emitExpression env message          // ..., message
        emit' DBool b Code.SayOrThink false // ..., version
        emit b Code.Pop                     // ...

    | O.``changeGraphicEffect:by:``, [name; addValue] ->
        match filterCode name with
        | Some(filter, Some set) ->
            emit' DFilter b Code.FilterWith filter // ..., value
            emitExpression env addValue         // ..., value, addValue
            emit b Code.Add                     // ..., value+addValue
            emit b set                          // ...

        | Some(filter, None) ->
            emit' DFilter b Code.FilterWith filter     // ..., value
            emitExpression env addValue             // ..., value, addValue
            emit b Code.Add                         // ..., value+addValue
            emit' DFilter b Code.SetFilterWith filter  // ...

        | None ->
            emitExpression env name     // ..., name
            emit b Code.Dup             // ..., name, name
            emit b Code.Filter          // ..., name, value
            emitExpression env addValue // ..., name, value, addValue
            emit b Code.Add             // ..., name, (value+addValue)
            emit b Code.SetFilter       // ...

    | O.``setGraphicEffect:to:``, [name; value] ->
        match filterCode name with
        | Some(_, Some set) ->
            emitExpression env value    // ..., newValue
            emit b set                  // ...

        | Some(filter, None) ->
            emitExpression env value                // ..., newValue
            emit' DFilter b Code.SetFilterWith filter  // ...

        | None ->
            emitExpression env name     // ..., name
            emitExpression env value    // ..., name, newValue
            emit b Code.SetFilter       // ...

    | O.filterReset, _ ->
        emit b Code.ResetAllFilter

    | O.``setSizeTo:``, [e1] ->
        emitExpression env e1
        emit b Code.SetScale

        //let size = evaluateExpression state e1 |> toNumberZeroIfNaN
        //let scale = max 0. (size / 100.)
        //sprite.spriteDrawingData.scale <- scale

    | O.``changeSizeBy:``, [e1] ->
        emit b Code.Scale       // ..., scale
        emitExpression env e1   // ..., scale, addValue
        emit b Code.Add         // ..., scale+addValue
        emit b Code.SetScale    // ...

    | O.show, _ -> emit b Code.Show
    | O.hide, _ -> emit b Code.Hide
    | O.comeToFront, _ -> emit b Code.ComeToFront
    | O.``goBackByLayers:``, [layerCount] ->
        emitExpression env layerCount
        emit b Code.GoBackByLayers

    | O.``playSound:``, [sound] ->
        match sound with
        | Literal(_, sound') ->
            match findSound (List.length env.entity.sounds) env.soundNameToIndex.Value (Value.ofSValue sound') with
            | ValueSome index -> emit' DInt32 b Code.PlaySoundWith index
            | ValueNone ->
                emitExpression env sound
                emit b Code.PlaySound
        | _ ->
            emitExpression env sound
            emit b Code.PlaySound
//     | Case1<"doPlaySoundAndWait", E>
//     | Case0<"stopAllSounds">

//     | Case2<"playDrum", E, E>
//     | Case1<"rest:elapsed:from:", E>
//     | Case2<"noteOn:duration:elapsed:from:", E, E>
//     | Case1<"instrument:", E>

    | O.``changeVolumeBy:``, [addValue] ->
        emit b Code.Volume          // ..., volume
        emitExpression env addValue // ..., volume, addValue
        emit b Code.Add             // ..., volume+addValue
        emit b Code.SetVolume       // ...

    | O.``setVolumeTo:``, [value] ->
        emitExpression env value
        emit b Code.SetVolume

    | O.``changeTempoBy:``, [addValue] ->
        emit b Code.Tempo
        emitExpression env addValue
        emit b Code.Add
        emit b Code.SetTempo

    | O.``setTempoTo:``, [value] ->
        emitExpression env value
        emit b Code.SetTempo

    | O.clearPenTrails, _ -> emit b Code.ClearPenTrails
    | O.putPenDown, _ -> emit' DBool b Code.SetPenDown true
    | O.putPenUp, _ -> emit' DBool b Code.SetPenDown false
    | O.``penColor:``, [color] ->
        emitExpression env color
        emit b Code.SetPenColor

    | O.``setPenHueTo:``, [hue] ->
        emitExpression env hue
        emit' DInt32 b Code.ChangePenHue 0

    | O.``changePenHueBy:``, [hue] ->
        emitExpression env hue
        emit' DInt32 b Code.ChangePenHue 1

    | O.``setPenShadeTo:``, [lightness] ->
        emitExpression env lightness
        emit' DInt32 b Code.ChangePenShade 0

    | O.``changePenShadeBy:``, [lightness] ->
        emitExpression env lightness
        emit' DInt32 b Code.ChangePenShade 1

    | O.``penSize:``, [size] ->
        emitExpression env size
        emit' DInt32 b Code.ChangePenSize 0

    | O.``changePenSizeBy:``, [extend] ->
        emitExpression env extend
        emit' DInt32 b Code.ChangePenSize 1

    | O.stampCostume, _ -> emit b Code.StampCostume


    | O.``setVar:to:``, [Literal(_, SString name); e] ->
        emitExpression env e
        match resolveScalarVariable env name with
        | ValueSome(LocalStorage(Stage, i)) -> emit' DScalar b Code.SetStageVariable i
        | ValueSome(LocalStorage(Sprite, i)) -> emit' DScalar b Code.SetSpriteVariable i
        | ValueSome CloudStorage -> emit' DString b Code.SetCloudVariable name
        | ValueNone -> emit b Code.Pop

    | O.``changeVar:by:``, [Literal(_, SString name); e] ->
        let emitLocalChange operandDef get set env b i e =
            emit' operandDef b get i    // ..., value
            emitExpression env e        // ..., value, addValue
            emit b Code.Add             // ..., value+addValue
            emit' operandDef b set i    // ...

        match resolveScalarVariable env name with
        | ValueSome(LocalStorage(Stage, i)) -> emitLocalChange DScalar Code.StageVariable Code.SetStageVariable env b i e
        | ValueSome(LocalStorage(Sprite, i)) -> emitLocalChange DScalar Code.SpriteVariable Code.SetSpriteVariable env b i e
        | ValueSome CloudStorage -> emitLocalChange DString Code.CloudVariable Code.SetCloudVariable env b name e
        | ValueNone ->
            emitExpression env e
            emit b Code.Pop

    | O.``append:toList:``, [value; Literal(_, SString name)] ->
        emitExpression env value
        match resolveListVariable env name with
        | Some(Stage, i) -> emit' DList b Code.AppendStageList i
        | Some(Sprite, i) -> emit' DList b Code.AppendSpriteList i
        | None -> emit b Code.Pop

    | O.``deleteLine:ofList:``, [nth; Literal(_, SString name)] ->
        emitExpression env nth
        match resolveListVariable env name with
        | Some(Stage, i) -> emit' DList b Code.DeleteStageListLine i
        | Some(Sprite, i) -> emit' DList b Code.DeleteSpriteListLine i
        | None -> emit b Code.Pop

    | O.``insert:at:ofList:``, [value; target; Literal(_, SString listName)] ->
        emitExpression env value
        emitExpression env target
        match resolveListVariable env listName with
        | Some(Stage, i) -> emit' DList b Code.InsertAtStageList i
        | Some(Sprite, i) -> emit' DList b Code.InsertAtSpriteList i
        | None ->
            emit b Code.Pop
            emit b Code.Pop

    | O.``setLine:ofList:to:``, [nth; Literal(_, SString name); value] ->
        emitExpression env nth
        emitExpression env value

        match resolveListVariable env name with
        | Some(Stage, i) -> emit' DList b Code.SetStageListLine i
        | Some(Sprite, i) -> emit' DList b Code.SetSpriteListLine i
        | None ->
            emit b Code.Pop
            emit b Code.Pop

    | O.``showVariable:``, [Literal(_, SString name)]
    | O.``showList:``, [Literal(_, SString name)] ->
        emit' DBool b Code.Bool true
        emit' DString b Code.ShowVariable name

    | O.``hideVariable:``, [Literal(_, SString name)]
    | O.``hideList:``, [Literal(_, SString name)] ->
        emit' DBool b Code.Bool false
        emit' DString b Code.ShowVariable name

    | O.``broadcast:``, [Literal(_, SString name)] ->
        let index = env.broadcastNameToIndex.Value.[LowerName.ofString name]
        emit' DBroadcast b Code.Broadcast index

    | O.doBroadcastAndWait, [Literal(_, SString name)] ->
        let index = env.broadcastNameToIndex.Value.[LowerName.ofString name]
        emit' DBroadcast b Code.BroadcastAndWait index

//     | Case2<"doForeverIf", E, M>

    // ```
    // %test
    // BrunchIfFalsy endIf
    //    %ifTrue
    // endIf:
    // ```
    | O.doIf, [test; Block ifTrue] ->
        emitExpression env test
        let label = defineLabel "endIf" b
        emit' DLabel b Code.BrunchIfFalsy label
        do emitBlock env ifTrue
        markLabel b label

    // ```
    // %test
    // BrunchIfFalsy beginElse
    //     %ifTrue
    //     Brunch endIf
    // beginElse:
    //     %ifFalse
    // endIf:
    | O.doIfElse, [test; Block ifTrue; Block ifFalse] ->
        emitExpression env test
        let beginElse = defineLabel "beginElse" b
        let endIf = defineLabel "endIf" b
        emit' DLabel b Code.BrunchIfFalsy beginElse
        do
            emitBlock env ifTrue
            emit' DLabel b Code.Brunch endIf
        do
            markLabel b beginElse
            emitBlock env ifFalse
        markLabel b endIf

    // `doRepeat (%count) { %body }` =>
    // ```
    // %count                       // ..., count
    // Round                        // ..., count
    // Dup                          // ..., count, count
    // Number 0                     // ..., count, count, 0
    // LessThan                     // ..., count, (count < 0)
    // Not                          // ..., count, (0 <= count)
    // BrunchIfFalsy endRepeat      // ..., count
    // beginRepeat:
    //     Dup                      // ..., count, count
    //     BrunchIfFalsy endRepeat  // ..., count
    //     %body                    // ..., count
    //     Yield                    // ..., count
    //     Number 1                 // ..., count, 1
    //     Sub                      // ..., count-1
    //     Brunch beginRepeat       // ..., count-1
    // endRepeat:
    // Pop                          // ...,
    // ```
    | O.doRepeat, [count; Block body] ->
        emitExpression env count

        let beginRepeat = defineLabel "beginRepeat" b
        let endRepeat = defineLabel "endRepeat" b

        emit b Code.Round
        emit b Code.Dup
        emit' DDouble b Code.Number 0.
        emit b Code.LessThan
        emit b Code.Not
        emit' DLabel b Code.BrunchIfFalsy endRepeat

        markLabel b beginRepeat
        begin
            emit b Code.Dup
            emit' DLabel b Code.BrunchIfFalsy endRepeat
            emitBlock env body
            emit b Code.Yield
            emit' DDouble b Code.Number 1.
            emit b Code.Sub
            emit' DLabel b Code.Brunch beginRepeat
        end
        markLabel b endRepeat
        emit b Code.Pop
//     | Case0<"doReturn">

    // beginLoop:               // ...
    // %test                    // ..., value
    // Not                      // ..., not(value)
    // BrunchIfFalsy endLoop    // ...
    //     %ifFalse             // ...
    //     Yield                // ...
    //     Brunch beginLoop     // ...
    // endLoop:
    | O.doUntil, [test; Block ifFalse] ->
        let beginLoop = defineLabel "beginLoop" b
        let endLoop = defineLabel "endLoop" b
        markLabel b beginLoop
        emitExpression env test
        emit b Code.Not
        emit' DLabel b Code.BrunchIfFalsy endLoop
        begin
            emitBlock env ifFalse
            emit b Code.Yield
            emit' DLabel b Code.Brunch beginLoop
        end
        markLabel b endLoop

//     | Case2<"doWhile", E, M>

    // beginLoop:               // ...
    // %test                    // ..., value
    // Not                      // ..., not(value)
    // BrunchIfFalsy endLoop:   // ...
    //     YieldForce           // ...
    //     Brunch beginLoop     // ...
    // endLoop:                 // ...
    | O.doWaitUntil, [test] ->
        let beginLoop = defineLabel "beginLoop" b
        let endLoop = defineLabel "endLoop" b
        markLabel b beginLoop
        emitExpression env test
        emit b Code.Not
        emit' DLabel b Code.BrunchIfFalsy endLoop
        begin
            emit b Code.YieldForce
            emit' DLabel b Code.Brunch beginLoop
        end
        markLabel b endLoop

    | O.``glideSecs:toX:y:elapsed:from:``, [seconds; x2; y2] ->
        emitExpression env seconds
        emitExpression env x2
        emitExpression env y2
        emit b Code.GlideSecs

    | O.``wait:elapsed:from:``, [seconds] ->
        emitExpression env seconds
        emit b Code.YieldSleep

//     | Case1<"warpSpeed", M>

    | O.createCloneOf, [name] ->
        match name with
        | Literal(_, SString "_myself_") -> emit b Code.CreateCloneOfMyself
        | _ ->
            emitExpression env name
            emit b Code.CreateCloneOf

    | O.doAsk, [question] ->
        emitExpression env question
        emit b Code.Ask

    | O.timerReset, _ -> emit b Code.TimerReset

    // beginLoop:
    //     %body
    //     Yield
    //     Brunch beginLoop
    | O.doForever, [Block body] ->
        let beginLoop = defineLabel "beginLoop" b
        markLabel b beginLoop
        emitBlock env body
        emit b Code.Yield
        emit' DLabel b Code.Brunch beginLoop

//     | Case0<"stopAll">

    | O.stopScripts, [Literal(_, SString kind)] ->
        emit' DString b Code.String kind
        emit b Code.StopScripts

    | O.deleteClone, _ -> emit b Code.DeleteClone

    | opxs -> failwithf $"not implemented {opxs}"

and emitBlock env (BlockExpression(_, list)) =
    for e in list do emitStatement env e

let collectStorageSpecs initialVariableEnv home entity =
    entity.variables
    |> Seq.mapi (fun i v ->
        let storage =
            match v.isPersistent with
            | Persistent -> CloudStorage
            | NoPersistent -> LocalStorage(home, Index.create<Value> i)
        v.name, storage
    )
    |> Seq.fold (fun map (k, v) -> Map.add k v map) initialVariableEnv

let collectStageProcedureIndexes stage =
    let collectEntityProcedures s entity =
        let objName = entity.objName
        entity.scripts
        |> List.fold (fun (struct(map, index) as s) x ->
            match x.script with
            | Expression _
            | Statements _ -> s
            | Listener _ -> struct(map, Index.(+) (index, 1))
            | Procedure(ProcedureDefinition(name = procName)) ->
                struct(Map.add (ProcedureId(selfName = objName, procedureName = procName)) index map, index + 1)
        ) s

    let s = struct(Map.empty, Index.zero)
    let s = collectEntityProcedures s stage
    let s =
        stage.ObjectDataExtension.children
        |> List.fold (fun s c ->
            match c with
            | Choice2Of3 x -> collectEntityProcedures s x
            | _ -> s
        ) s
    let struct(map, _) = s
    map

let collectBroadcasts stage =
    let rec collectExpression = function
        | Literal _ -> List.toSeq []
        | Block x -> collectBlock x
        | Complex x -> collectComplex x

    and collectComplex (ComplexExpression(operator = op; operands = args)) =
        match op, args with
        | O.``broadcast:``, [Literal(_, SString name)] -> List.toSeq [name]
        | O.doBroadcastAndWait, [Literal(_, SString name)] -> List.toSeq [name]
        | _ -> Seq.collect collectExpression args

    and collectBlock (BlockExpression(body = body)) =
        Seq.collect collectComplex body

    let collectEntiry entity =
        entity.scripts
        |> Seq.collect (fun s ->
            match s.script with
            | Expression _
            | Statements _ -> List.toSeq []
            | Listener(ListenerDefinition(name = O.whenIReceive; arguments = Literal(_, SString name)::_; body = body)) ->
                Seq.append [name] (collectBlock body)

            | Listener(ListenerDefinition(body = body))
            | Procedure(ProcedureDefinition(body = body)) -> collectBlock body
        )

    let stages = collectEntiry stage
    let sprites =
        stage
        |> StageData.sprites
        |> Seq.collect collectEntiry

    Seq.append stages sprites
    |> Seq.map LowerName.ofString
    |> Set.ofSeq
    |> Seq.map (fun n -> { broadcastName = n })
    |> IArray.ofSeq

let emitProcedure env location name parameters atomicity body =
    let startAddress = CodeBuilder.offset env.builder

    let parameterNameToIndex =
        parameters
        |> Seq.mapi (fun i (ParameterDefinition(_, name, _)) -> name, Index.create<Value> i)
        |> Map.ofSeq
    let env = { env with parameterNameToIndex = parameterNameToIndex }

    emitBlock env body
    emit (withLocation location env) Code.Return

    let endAddress = CodeBuilder.offset env.builder

    let index = env.procedures.NextIndex()
    env.procedures.Add {
        name = name
        startAddress = startAddress
        codeLength = endAddress .-. startAddress
        parameterCount = List.length parameters
        localCount = CodeBuilder.localCount env.builder

        parameters =
            parameters
            |> Seq.map (fun (ParameterDefinition(name = name)) -> { parameterName = name })
            |> IArray.ofSeq

        isAtomic = match atomicity with Atomic -> true | NoAtomic -> false
    }
    CodeBuilder.resetLocals env.builder
    index

let entityToImage env =
    let env = {
        env with
            EmitEnvironment.costumeNameToIndex =
                lazy
                    env.entity.sounds
                    |> Seq.mapi (fun i s -> s.soundName, i)
                    |> Map.ofSeq

            soundNameToIndex =
                lazy
                    env.entity.costumes
                    |> Seq.mapi (fun i s -> s.costumeName, i)
                    |> Map.ofSeq
    }
    let whenGreenFlags = ResizeArray()
    let whenCloneds = ResizeArray()
    let whenClickeds = ResizeArray()
    let mutable broadcastListeners = Map.empty
    let definedProcedures = ResizeArray()

    let registerBroadcastListener broadcastName procedureIndex =
        let broadcastName = LowerName.ofString broadcastName
        let broadcastIndex = env.broadcastNameToIndex.Value.[broadcastName]
        match Map.tryFind broadcastIndex broadcastListeners with
        | ValueNone ->
            let listeners = ResizeArray()
            listeners.Add procedureIndex
            broadcastListeners <- Map.add broadcastIndex listeners broadcastListeners

        | ValueSome listeners ->
            listeners.Add procedureIndex

    let emitListener (ListenerDefinition(location, eventName, args, body) as l) =
        let procedureIndex = emitProcedure env location (Symbol.name eventName) [] NoAtomic body
        match eventName, args with
        | O.whenIReceive, Literal(_, SString broadcastName)::_ ->
            registerBroadcastListener broadcastName procedureIndex

        | O.whenGreenFlag, [] -> whenGreenFlags.Add procedureIndex
        | O.whenCloned, [] -> whenCloneds.Add procedureIndex
        | O.whenClicked, [] -> whenClickeds.Add procedureIndex
        | _ ->
            // TODO:
            failwithf $"unregistered listener: {l}"

    for script in env.entity.scripts do
        match script.script with
        | Expression _
        | Statements _ -> ()
        | Procedure(ProcedureDefinition(location, name, ps, atomicity, body)) ->
            definedProcedures.Add <| emitProcedure env location name ps atomicity body
        | Listener x -> emitListener x

    {
        broadcastListeners = broadcastListeners |> Map.map (fun _ xs -> IArray.toSeqCopiable (IArray.ofICollection xs)) |> IndexMap.ofMap
        whenGreenFlags = IArray.toSeqCopiable (IArray.ofICollection whenGreenFlags)
        whenCloned = IArray.toSeqCopiable (IArray.ofICollection whenCloneds)
        whenClicked = IArray.toSeqCopiable (IArray.ofICollection whenClickeds)

        scalarVariables = env.entity.variables |> IArray.ofSeq
        listVariables = env.entity.lists |> IArray.ofSeq

        definedProcedures = IArray.ofICollection definedProcedures
        entityName = env.entity.objName
        costumes = env.entity.costumes |> IArray.ofSeq
        sounds = env.entity.sounds |> IArray.ofSeq
        currentCostumeIndex = env.entity.currentCostumeIndex
        spriteImage = env.sprite
    }

let compileToImageWith withConfig stage =
    let config = withConfig CompilerConfig.defaultConfig
    let stage = StageData.unapplyScratch3ExecutionOrderTrick stage
    let broadcasts = collectBroadcasts stage
    let broadcastNameToIndex =
        lazy
            broadcasts
            |> IArray.toSeqCopiable
            |> Seq.mapi (fun i x -> x.broadcastName, Index.create<Broadcast> i)
            |> Map.ofSeq

    let procedureNameToIndex = lazy collectStageProcedureIndexes stage
    let variableNameToStorage = collectStorageSpecs Map.empty Stage stage

    let env = {
        config = config

        builder = CodeBuilder.create()
        procedures = ResizeArray()
        instructionIndexToLocation = ref Map.empty

        variableNameToStorage = variableNameToStorage
        parameterNameToIndex = Map.empty
        procedureNameToIndex = procedureNameToIndex
        broadcastNameToIndex = broadcastNameToIndex
        costumeNameToIndex = lazy Map.empty
        soundNameToIndex = lazy Map.empty

        entity = stage
        stage = stage
        sprite = None
    }

    let stageImage = entityToImage env
    let spriteImages = ResizeArray()
    for sprite in StageData.sprites stage do
        let env = {
            config = env.config

            builder = env.builder
            procedures = env.procedures
            instructionIndexToLocation = env.instructionIndexToLocation

            variableNameToStorage = collectStorageSpecs env.variableNameToStorage Sprite sprite
            parameterNameToIndex = env.parameterNameToIndex
            procedureNameToIndex = env.procedureNameToIndex
            broadcastNameToIndex = env.broadcastNameToIndex
            costumeNameToIndex = env.costumeNameToIndex
            soundNameToIndex = env.soundNameToIndex
            entity = sprite
            stage = stage
            sprite = Some sprite.ObjectDataExtension
        }
        spriteImages.Add <| entityToImage env

    let instructions, stringLiterals = CodeBuilder.build env.builder
    {
        instructions = instructions
        broadcasts = broadcasts
        stringLiterals = stringLiterals
        procedures = IArray.ofICollection env.procedures
        stageImage = stageImage
        spriteImages = IArray.ofICollection spriteImages
        locationMap = IndexMap.ofMap !env.instructionIndexToLocation
    }

let compileToImage stage = compileToImageWith id stage
