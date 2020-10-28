module Scratch.AstDefinitions
open Scratch.Ast
open Scratch.Reflection
open System.Text.RegularExpressions


type PrimitiveTypes<'t> = {
    tNumber: 't
    tString: 't
    tBoolean: 't
    tUnit: 't
}
module TsType =
    let gUnknown = TsType.Named("unknown", [])
    let gNumber = TsType.Named("number", [])
    let gString = TsType.Named("string", [])
    let gBoolean = TsType.Named("boolean", [])
    let gColor = TsType.Named("color", [])
    let gUnit = TsType.Named("()", [])
    let types = {
        tNumber = gNumber
        tString = gString
        tBoolean = gBoolean
        tUnit = gUnit
    }

open TsType

[<Struct>]
type OperandInfo = {
    operandType: TsType OperandType
    literalOperandType: TsType option
    forceLiteralType: bool
}

[<AutoOpen>]
module private Privates =
    type G = TsType
    type O<'a> = 'a OperandType

    let op t = { operandType = t; forceLiteralType = false; literalOperandType = None }
    let opE t = op <| O.Expression t
    let opS ss = op <| O.StringLiterals (Set.ofSeq ss)

    let gStringL x = G.StringSs [x]
    let g0 t = [], t
    let g1 v1 f =
        let v1 = TypeVar v1
        [v1], f (G.GVar 0)
        
    let e0 t = [], t
    let e1 t1 t = [opE t1], t
    let e2 t1 t2 t = [opE t1; opE t2], t
    let e3 t1 t2 t3 t = [opE t1; opE t2; opE t3], t

    let s0 = [], gUnit
    let s1 t1 = e1 t1 gUnit
    let s2 t1 t2 = e2 t1 t2 gUnit
    let s3 t1 t2 t3 = e3 t1 t2 t3 gUnit

    let g0s0 = [], s0
    let g0s1 t1 = g0 <| s1 t1
    let g0s2 t1 t2 = g0 <| s2 t1 t2

    let g0e0 t = g0 <| e0 t
    let g0e1 t1 t = g0 <| e1 t1 t
    let g0e2 t1 t2 t = g0 <| e2 t1 t2 t

    let v1 t = [op O.Variable], t
    let ebs t = [opE t; op O.Block], gUnit

    let (.|.) t1 t2 = G.Or(t1, t2)

    let tValue = G.Or(G.Or(gString, gNumber), gBoolean)
    let attributeNames = [
        "x position"
        "y position"
        "direction"
        "costume #"
        "costume name"
        "size"
        "volume"
        "background #"
        "backdrop #"
        "backdrop name"
    ]
    let mathFunctionNames = [
        "abs"
        "floor"
        "sqrt"
        "ceiling"
        "cos"
        "sin"
        "tan"
        "asin"
        "acos"
        "atan"
        "ln"
        "log"
        "e ^"
        "10 ^"
    ]
    let timeAndDateFormats = [
        "year"
        "month"
        "date"
        "day of week"
        "hour"
        "minute"
        "second"
    ]
    let filterNames = [
        "color"
        "fisheye"
        "whirl"
        "pixelate"
        "mosaic"
        "brightness"
        "ghost"
    ]
    let tRgb = gNumber
    let tCostume = G.Or(G.Or(gNumber, gString), G.StringSs [
        "next costume"
        "next backdrop"
        "previous costume"
        "previous backdrop"
    ])
    let opColor = { opE tRgb with forceLiteralType = true; literalOperandType = Some gColor }

[<Struct; RequireQualifiedAccess>]
type Kind = Statement | Expression

[<Struct; RequireQualifiedAccess>]
type Control =
    | Unknown
    | Next
    | Call
    | Return
    | Stop
    | DeleteClone
    | NormalYield
    | ForceYield

[<NoEquality; NoComparison>]
type OperatorInfo = {
    kind: Kind
    typeVariables: TypeVar list
    operands: OperandInfo list
    resultType: TsType
    operatorCost: Cost
    control: Control

    isFooter: bool
}

let private expressionTable xs =
    xs
    |> List.map (fun (name, cost, (typeVars, (operands, resultType))) ->
        name, {
            typeVariables = typeVars
            kind = Kind.Expression
            operands = operands
            resultType = resultType
            operatorCost = cost
            control = Control.Next
            isFooter = false
        }
    )

//type AttributeName =
//    | "x position"
//    | "y position"
//    | "direction"
//    | "costume #"
//    | "costume name"
//    | "size"
//    | "volume"
//    | "background #"
//    | "backdrop #"
//    | "backdrop name"
//type MathFunctionName =
//    | "abs"
//    | "floor"
//    | "sqrt"
//    | "ceiling"
//    | "cos"
//    | "sin"
//    | "tan"
//    | "asin"
//    | "acos"
//    | "atan"
//    | "ln"
//    | "log"
//    | "e ^"
//    | "10 ^"
//type TimeAndDateFormat =
//    | 'year'
//    | 'month'
//    | 'date'
//    | 'day of week'
//    | 'hour'
//    | 'minute'
//    | 'second'
// type KnownCallExpression<E, V> =
//     | Case1<"getParam", E, "r">
//     | Case0<"costumeName">
//     | Case0<"sceneName">
//     | Case1<"readVariable", V>
//     | Case1<"contentsOfList:", E>
//     | Case2<"getLine:ofList:", E, E>
//     | Case2<"concatenate:with:", E, E>
//     | Case2<"letter:of:", E, E>
//     | Case0<"answer">
//     | Case2<"getAttribute:of:", E, E>
//     | Case0<"getUserId">
//     | Case0<"getUserName">
let private knownCallExpressions() = expressionTable [
    O.getParam, LiteralLike, g0 ([op O.ParameterName; opS ["r"]], tValue)
    O.costumeName, Unknown, g0e0 gString
    O.sceneName, Unknown, g0e0 gString
    O.readVariable, HasSideEffect, g0 (v1 tValue)
    O.``contentsOfList:``, Unknown, g1 "T" (fun t -> [op <| O.ListVariableExpression t], gString)
    O.``getLine:ofList:``, Unknown, g1 "T" (fun t -> [opE (gNumber .|. G.StringSs ["random"; "any"; "last"]); op <| O.ListVariableExpression t], (t .|. gStringL ""))
    O.``concatenate:with:``, Pure, g0e2 gString gString gString
    O.``letter:of:``, Pure, g0e2 gNumber gString gString 
    O.``answer``, Unknown, g0e0 gString
    O.``getAttribute:of:``, Unknown, g0 ([opS attributeNames; opE gString], tValue)
    O.getUserId, Unknown, g0e0 gNumber
    O.getUserName, Unknown, g0e0 gString
]

// type KnownNumberExpression<E> =
//     | Case0<"xpos">
//     | Case0<"ypos">
//     | Case0<"heading">
//     | Case0<"costumeIndex">
//     | Case0<"backgroundIndex">
//     | Case0<"scale">
//     | Case0<"volume">
//     | Case0<"tempo">
//     | Case1<"lineCountOfList:", E>
//     | Case2<"+", E, E>
//     | Case2<"-", E, E>
//     | Case2<"*", E, E>
//     | Case2<"/", E, E>
//     | Case2<"randomFrom:to:", E, E>
//     | Case1<"abs", E>
//     | Case1<"sqrt", E>
//     | Case1<"stringLength:", E>
//     | Case2<"%", E, E>
//     | Case2<"\\\\", E, E>
//     | Case1<"rounded", E>
//     | Case2<"computeFunction:of:", E, E>
//     | Case0<"mouseX">
//     | Case0<"mouseY">
//     | Case0<"timer">
//     | Case1<"distanceTo:", E>
//     | Case0<"timestamp">
//     | Case1<"timeAndDate", E>
let private knownNumberExpressions() = expressionTable [
    O.xpos, Unknown, g0e0 gNumber
    O.ypos, Unknown, g0e0 gNumber
    O.heading, Unknown, g0e0 gNumber
    O.costumeIndex, Unknown, g0e0 gNumber
    O.backgroundIndex, Unknown, g0e0 gNumber
    O.scale, Unknown, g0e0 gNumber
    O.volume, Unknown, g0e0 gNumber
    O.tempo, Unknown, g0e0 gNumber
    O.``lineCountOfList:``, Unknown, g1 "T" (fun t -> [op <| O.ListVariableExpression t], gNumber)
    O.``+``, Pure, g0e2 gNumber gNumber gNumber
    O.``-``, Pure, g0e2 gNumber gNumber gNumber
    O.``*``, Pure, g0e2 gNumber gNumber gNumber
    O.``/``, Pure, g0e2 gNumber gNumber gNumber
    O.``randomFrom:to:``, HasSideEffect, g0e2 gNumber gNumber gNumber
    O.abs, Pure, g0e1 gNumber gNumber
    O.sqrt, Pure, g0e1 gNumber gNumber
    O.``stringLength:``, Pure, g0e1 gString gNumber
    O.``%``, Pure, g0e2 gNumber gNumber gNumber
    O.``\\``, Pure, g0e2 gNumber gNumber gNumber
    O.rounded, Pure, g0e1 gNumber gNumber
    O.``computeFunction:of:``, Pure, g0 ([opS mathFunctionNames; opE gNumber], gNumber)
    O.mouseX, Unknown, g0e0 gNumber
    O.mouseY, Unknown, g0e0 gNumber
    O.timer, Unknown, g0e0 gNumber
    O.``distanceTo:``, Unknown, g0e1 (gString .|. gStringL "_mouse_") gNumber
    O.timestamp, Unknown, g0e0 gNumber
    O.timeAndDate, Unknown, g0 ([opS timeAndDateFormats], gNumber)
]

// type KnownBooleanExpression<E> =
//     | Case2<"list:contains:", E, E>
//     | Case2<"<", E, E>
//     | Case2<">", E, E>
//     | Case2<"=", E, E>
//     | Case2<"&", E, E>
//     | Case2<"|", E, E>
//     | Case1<"not", E>
//     | Case0<"mousePressed">
//     | Case1<"touching:", E>
//     | Case1<"touchingColor:", E>
//     | Case2<"color:sees:", E, E>
//     | Case1<"keyPressed:", E>
let private knownBooleanExpressions() = expressionTable [
    O.``list:contains:``, Unknown, g1 "T" (fun t -> [op <| O.ListVariableExpression t; opE t], gBoolean)
    O.``<``, Pure, g1 "T" (fun t -> e2 t t gBoolean)
    O.``>``, Pure, g1 "T" (fun t -> e2 t t gBoolean)
    O.``=``, Pure, g1 "T" (fun t -> e2 t t gBoolean)
    O.``&``, Pure, g0e2 gBoolean gBoolean gBoolean
    O.``|``, Pure, g0e2 gBoolean gBoolean gBoolean
    O.not, Pure, g0e1 gBoolean gBoolean
    O.mousePressed, Unknown, g0e0 gBoolean
    O.``touching:``, Unknown, g0e1 (gString .|. G.StringSs ["_mouse_"; "_edge_"]) gBoolean
    O.``touchingColor:``, Unknown, g0 ([opColor], gBoolean)
    O.``color:sees:``, Unknown, g0 ([opColor; opColor], gBoolean)
    O.``keyPressed:``, Unknown, g0e1 (gString .|. gNumber) gBoolean
]


let private statementTable isFooter xs =
    xs
    |> List.map (fun (name, control, (typeVars, (operands, resultType))) ->
        name, {
            typeVariables = typeVars
            kind = Kind.Statement
            operands = operands
            resultType = resultType
            operatorCost = Cost.Unknown
            control = control
            isFooter = isFooter
        }
    )
// type KnownCallStatement<E> = 
//     | Case2<"call", "phosphorus: debug" | E, ReadonlyArray<E>>

// type KnownStatement<E, M, S> =
//     | Case1<"forward:", E>
//     | Case1<"turnRight:", E>
//     | Case1<"turnLeft:", E>
//     | Case1<"heading:", E>
//     | Case1<"pointTowards:", E>
//     | Case2<"gotoX:y:", E, E>
//     | Case1<"gotoSpriteOrMouse:", E>
//     | Case1<"changeXposBy:", E>
//     | Case1<"xpos:", E>
//     | Case1<"changeYposBy:", E>
//     | Case1<"ypos:", E>
//     | Case0<"bounceOffEdge">
//     | Case1<"setRotationStyle", Rotation>
//     | Case1<"lookLike:", E>
//     | Case0<"nextCostume">
//     | Case1<"showBackground:", E>
//     | Case1<"startScene", E>
//     | Case0<"nextBackground">
//     | Case0<"nextScene">
//     | Case1<"startSceneAndWait", E>
//     | Case2<"say:duration:elapsed:from:", E, E>
//     | Case1<"say:", E>
//     | Case2<"think:duration:elapsed:from:", E, E>
//     | Case1<"think:", E>
//     | Case2<"changeGraphicEffect:by:", E, E>
//     | Case2<"setGraphicEffect:to:", E, E>
//     | Case0<"filterReset">
//     | Case1<"changeSizeBy:", E>
//     | Case1<"setSizeTo:", E>
//     | Case0<"show">
//     | Case0<"hide">
//     | Case0<"comeToFront">
//     | Case1<"goBackByLayers:", E>
//     | Case1<"playSound:", E>
//     | Case1<"doPlaySoundAndWait", E>
//     | Case0<"stopAllSounds">
//     | Case2<"playDrum", E, E>
//     | Case1<"rest:elapsed:from:", E>
//     | Case2<"noteOn:duration:elapsed:from:", E, E>
//     | Case1<"instrument:", E>
//     | Case1<"changeVolumeBy:", E>
//     | Case1<"setVolumeTo:", E>
//     | Case1<"changeTempoBy:", E>
//     | Case1<"setTempoTo:", E>
//     | Case0<"clearPenTrails">
//     | Case0<"putPenDown">
//     | Case0<"putPenUp">
//     | Case1<"penColor:", E>
//     | Case1<"setPenHueTo:", E>
//     | Case1<"changePenHueBy:", E>
//     | Case1<"setPenShadeTo:", E>
//     | Case1<"changePenShadeBy:", E>
//     | Case1<"penSize:", E>
//     | Case1<"changePenSizeBy:", E>
//     | Case0<"stampCostume">
//     | Case2<"setVar:to:", E, E>
//     | Case2<"changeVar:by:", E, E>
//     | Case2<"append:toList:", E, E>
//     | Case2<"deleteLine:ofList:", E, E>
//     | Case3<"insert:at:ofList:", E, E, E>
//     | Case3<"setLine:ofList:to:", E, E, E>
//     | Case1<"showVariable:", S>
//     | Case1<"hideVariable:", S>
//     | Case1<"showList:", S>
//     | Case1<"hideList:", S>
//     | Case1<"broadcast:", E>
//     | Case1<"doBroadcastAndWait", E>
//     | Case2<"doForeverIf", E, M>
//     | Case2<"doIf", E, M>
//     | Case3<"doIfElse", E, M, M>
//     | Case2<"doRepeat", E, M>
//     | Case0<"doReturn">
//     | Case2<"doUntil", E, M>
//     | Case2<"doWhile", E, M>
//     | Case1<"doWaitUntil", E>
//     | Case3<"glideSecs:toX:y:elapsed:from:", E, E, E>
//     | Case1<"stopScripts", "other scripts in sprite" | "other scripts in stage">
//     | Case1<"wait:elapsed:from:", E>
//     | Case1<"warpSpeed", M>
//     | Case1<"createCloneOf", E>
//     | Case1<"doAsk", E>
//     | Case0<"timerReset">
let private knownStatements() = statementTable false [
    O.call, Control.Call, g0 ([op O.ProcedureName; op O.VariadicExpressions], gUnit)
    O.``forward:``, Control.Next, g0s1 gNumber
    O.``turnRight:``, Control.Next, g0s1 gNumber
    O.``turnLeft:``, Control.Next, g0s1 gNumber
    O.``heading:``, Control.Next, g0s1 gNumber
    O.``pointTowards:``, Control.Next, g0s1 (gString .|. gStringL "_mouse_")
    O.``gotoX:y:``, Control.Next, g0s2 gNumber gNumber
    O.``gotoSpriteOrMouse:``, Control.Next, g0s1 (gString .|. G.StringSs ["_mouse_"; "_random_"])
    O.``changeXposBy:``, Control.Next, g0s1 gNumber
    O.``xpos:``, Control.Next, g0s1 gNumber
    O.``changeYposBy:``, Control.Next, g0s1 gNumber
    O.``ypos:``, Control.Next, g0s1 gNumber
    O.``bounceOffEdge``, Control.Next, g0s0
    O.``setRotationStyle``, Control.Next, g0 ([opS ["left-right"; "don't rotate"; "normal"]], gUnit)
    O.``lookLike:``, Control.Next, g0s1 tCostume
    O.``nextCostume``, Control.Next, g0s0
    O.``showBackground:``, Control.Next, g0s1 tCostume
    O.startScene, Control.Next, g0s1 tCostume
    O.nextBackground, Control.Next, g0s0
    O.nextScene, Control.Next, g0s0
    O.startSceneAndWait, Control.Unknown, g0s1 tCostume
    O.``say:duration:elapsed:from:``, Control.ForceYield, g0s2 gString gNumber
    O.``say:``, Control.Next, g0s1 gString
    O.``think:duration:elapsed:from:``, Control.ForceYield, g0s2 gString gNumber
    O.``think:``, Control.Next, g0s1 gString
    O.``changeGraphicEffect:by:``, Control.Next, g0 ([opS filterNames; opE gNumber], gUnit)
    O.``setGraphicEffect:to:``, Control.Next, g0 ([opS filterNames; opE gNumber], gUnit)
    O.filterReset, Control.Next, g0s0
    O.``changeSizeBy:``, Control.Next, g0s1 gNumber
    O.``setSizeTo:``, Control.Next, g0s1 gNumber
    O.show, Control.Next, g0s0
    O.hide, Control.Next, g0s0
    O.comeToFront, Control.Next, g0s0
    O.``goBackByLayers:``, Control.Next, g0s1 gNumber
    O.``playSound:``, Control.Next, g0s1 gString
    O.doPlaySoundAndWait, Control.ForceYield, g0s1 gString
    O.stopAllSounds, Control.Next, g0s0
    O.playDrum, Control.Next, g0s2 gNumber gNumber
    O.``rest:elapsed:from:``, Control.Unknown, g0s1 gNumber
    O.``noteOn:duration:elapsed:from:``, Control.Unknown, g0s2 gNumber gNumber
    O.``instrument:``, Control.Unknown, g0s1 gNumber
    O.``changeVolumeBy:``, Control.Next, g0s1 gNumber
    O.``setVolumeTo:``, Control.Next, g0s1 gNumber
    O.``changeTempoBy:``, Control.Next, g0s1 gNumber
    O.``setTempoTo:``, Control.Next, g0s1 gNumber
    O.clearPenTrails, Control.Next, g0s0
    O.putPenDown, Control.Next, g0s0
    O.putPenUp, Control.Next, g0s0
    O.``penColor:``, Control.Next, g0 ([opColor], gUnit)
    O.``setPenHueTo:``, Control.Next, g0s1 gNumber
    O.``changePenHueBy:``, Control.Next, g0s1 gNumber
    O.``setPenShadeTo:``, Control.Next, g0s1 gNumber
    O.``changePenShadeBy:``, Control.Next, g0s1 gNumber
    O.``penSize:``, Control.Next, g0s1 gNumber
    O.``changePenSizeBy:``, Control.Next, g0s1 gNumber
    O.stampCostume, Control.Next, g0s0
    O.``setVar:to:``, Control.Next, g0s2 gString gUnknown
    O.``changeVar:by:``, Control.Next, g0 ([op O.Variable; opE gNumber], gUnit)
    O.``append:toList:``, Control.Next, g1 "T" (fun t -> [opE t; op <| O.ListVariableExpression t], gUnit)
    O.``deleteLine:ofList:``, Control.Next, g1 "T" (fun t -> [opE (gNumber .|. G.StringSs ["all"; "random"; "any"; "last"]); op <| O.ListVariableExpression t], gUnit)
    O.``insert:at:ofList:``, Control.Next, g1 "T" (fun t -> [opE t; opE (gNumber .|. G.StringSs ["random"; "last"]); op <| O.ListVariableExpression t], gUnit)
    O.``setLine:ofList:to:``, Control.Next, g1 "T" (fun t -> [opE (gNumber .|. G.StringSs ["random"; "last"]); op <| O.ListVariableExpression t; opE t], gUnit)
    O.``showVariable:``, Control.Next, g0 ([op O.Variable], gUnit)
    O.``hideVariable:``, Control.Next, g0 ([op O.Variable], gUnit)
    O.``showList:``, Control.Next, g0 ([op O.Variable], gUnit)
    O.``hideList:``, Control.Next, g0 ([op O.Variable], gUnit)
    O.``broadcast:``, Control.Next, ([], ([{ opE gString with forceLiteralType = true }], gUnit))
    O.doBroadcastAndWait, Control.NormalYield, g0s1 gString
    O.doForeverIf, Control.Unknown, g0 (ebs gBoolean)
    O.doIf, Control.Next, g0 (ebs gBoolean)
    O.doIfElse, Control.Next, g0 ([opE gBoolean; op OperandType.Block; op OperandType.Block], gUnit)
    O.doRepeat, Control.NormalYield, g0 (ebs gNumber)
    O.doReturn, Control.Return, g0s0
    O.doUntil, Control.NormalYield, g0 (ebs gBoolean)
    O.doWhile, Control.Unknown, g0 (ebs gBoolean)
    O.doWaitUntil, Control.ForceYield, g0s1 gBoolean
    O.``glideSecs:toX:y:elapsed:from:``, Control.NormalYield, g0 (s3 gNumber gNumber gNumber)
    O.stopScripts, Control.Stop, g0 ([opS ["other scripts in sprite"; "other scripts in stage"]], gUnit)
    O.``wait:elapsed:from:``, Control.NormalYield, g0s1 gNumber
    O.warpSpeed, Control.Unknown, g0 ([op OperandType.Block], gUnit)
    O.createCloneOf, Control.Next, g0s1 (gString .|. gStringL "_myself_")
    O.doAsk, Control.ForceYield, g0s1 gString
    O.timerReset, Control.Next, g0s0
]
// type KnownFooterStatement<M> =
//     | Case1<"doForever", M>
//     | Case0<"stopAll">
//     | Case1<"stopScripts", "all" | "this script">
//     | Case0<"deleteClone">
let private knownFooterStatements() = statementTable true [
    O.doForever, Control.NormalYield, g0 ([op O.Block], gUnit)
    O.stopAll, Control.Stop, g0s0
    O.stopScripts, Control.Stop, g0 ([opS ["all"; "this script"]], gUnit)
    O.deleteClone, Control.DeleteClone, g0s0
]

let knownAllOperatorMap =
    knownCallExpressions() @
    knownNumberExpressions() @
    knownBooleanExpressions() @
    knownStatements() @
    knownFooterStatements()
    |> Map.ofSeq

let (|KnownOperatorInfo|) name = Map.tryFind name knownAllOperatorMap

// type KnownListenerHeader<S, B> =
//     | Case0<"whenClicked">
//     | Case0<"whenGreenFlag">
//     | Case0<"whenCloned">
//     | Case1<"whenIReceive", S>
//     | Case1<"whenKeyPressed", "any" | S>
//     | Case4<"whenSceneStarts", S, null, null, B>
let knownListenerHeaders = [
    O.whenClicked, []
    O.whenGreenFlag, []
    O.whenCloned, []
    O.whenIReceive, [ListenerHeaderType.EventName]
    O.whenKeyPressed, [ListenerHeaderType.AnyOrKeyName]
    O.whenSceneStarts, [ListenerHeaderType.String; ListenerHeaderType.Null; ListenerHeaderType.Null; ListenerHeaderType.Bool]
]

/// `@"%" => @"\%"`
/// `@"\" => @"\\"`
let escapeProcedureName n = Regex.Replace(n, @"[%\\]", MatchEvaluator(fun m -> @"\" + m.Value))

let mangleProcedureName baseName parameterTypes =
    let sigName =
        parameterTypes
        |> Seq.map (SType.scratchParameterTypeName >> (+) " %")
        |> String.concat ""

    escapeProcedureName baseName + sigName

let demangleProcedureName mangledName =
    let rec aux cs' ts' = function
        | '\\'::c::cs -> aux (c::cs') ts' cs

        | '%'::c::cs
        | ' '::'%'::c::cs ->
            let t =
                match c with
                | 'n' -> Ok SType.N
                | 's' -> Ok SType.S
                | 'b' -> Ok SType.B
                | t -> Error t
            aux cs' (t::ts') cs

        | [] -> List.rev cs', List.rev ts'
        | c::cs -> aux (c::cs') ts' cs

    let cs, ts = aux [] [] (Seq.toList (mangledName: string))
    System.String(List.toArray cs), ts

let minColorCode = -0x1000000
let maxColorCode = 0xFFFFFF

let isColorCode n = 
    double minColorCode <= n && n <= double maxColorCode &&
    truncate n = n
