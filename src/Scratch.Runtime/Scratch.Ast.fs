namespace Scratch.Ast
open Scratch
open Scratch.Primitives
open System
type number = double
type boolean = bool
type 'a ReadonlyArray = 'a list
type 'a nullable = 'a option
type 'a optional = 'a option

type Symbol =
    /// ""
    | Empty = 0uy

    | ``%`` = 1uy
    | ``&`` = 2uy
    | ``*`` = 3uy
    | ``+`` = 4uy
    | ``-`` = 5uy
    | ``/`` = 6uy
    | ``<`` = 7uy
    | ``=`` = 8uy
    | ``>`` = 9uy
    | ``\\`` = 10uy
    | abs = 11uy
    | answer = 12uy
    | ``append:toList:`` = 13uy
    | backgroundIndex = 14uy
    | bounceOffEdge = 15uy
    | ``broadcast:`` = 16uy
    | call = 17uy
    | ``changeGraphicEffect:by:`` = 18uy
    | ``changePenHueBy:`` = 19uy
    | ``changePenShadeBy:`` = 20uy
    | ``changePenSizeBy:`` = 21uy
    | ``changeSizeBy:`` = 22uy
    | ``changeTempoBy:`` = 23uy
    | ``changeVar:by:`` = 24uy
    | ``changeVolumeBy:`` = 25uy
    | ``changeXposBy:`` = 26uy
    | ``changeYposBy:`` = 27uy
    | clearPenTrails = 28uy
    | ``color:sees:`` = 29uy
    | comeToFront = 30uy
    | ``computeFunction:of:`` = 31uy
    | ``concatenate:with:`` = 32uy
    | ``contentsOfList:`` = 33uy
    | costumeIndex = 34uy
    | costumeName = 35uy
    | createCloneOf = 36uy
    | deleteClone = 37uy
    | ``deleteLine:ofList:`` = 38uy
    | ``distanceTo:`` = 39uy
    | doAsk = 40uy
    | doBroadcastAndWait = 41uy
    | doForever = 42uy
    | doForeverIf = 43uy
    | doIf = 44uy
    | doIfElse = 45uy
    | doPlaySoundAndWait = 46uy
    | doRepeat = 47uy
    | doReturn = 48uy
    | doUntil = 49uy
    | doWaitUntil = 50uy
    | doWhile = 51uy
    | filterReset = 52uy
    | ``forward:`` = 53uy
    | ``getAttribute:of:`` = 54uy
    | ``getLine:ofList:`` = 55uy
    | getParam = 56uy
    | getUserId = 57uy
    | getUserName = 58uy
    | ``glideSecs:toX:y:elapsed:from:`` = 59uy
    | ``goBackByLayers:`` = 60uy
    | ``gotoSpriteOrMouse:`` = 61uy
    | ``gotoX:y:`` = 62uy
    | heading = 63uy
    | ``heading:`` = 64uy
    | hide = 65uy
    | ``hideList:`` = 66uy
    | ``hideVariable:`` = 67uy
    | ``insert:at:ofList:`` = 68uy
    | ``instrument:`` = 69uy
    | ``keyPressed:`` = 70uy
    | ``letter:of:`` = 71uy
    | ``lineCountOfList:`` = 72uy
    | ``list:contains:`` = 73uy
    | ``lookLike:`` = 74uy
    | mousePressed = 75uy
    | mouseX = 76uy
    | mouseY = 77uy
    | nextBackground = 78uy
    | nextCostume = 79uy
    | nextScene = 80uy
    | not = 81uy
    | ``noteOn:duration:elapsed:from:`` = 82uy
    | ``penColor:`` = 83uy
    | ``penSize:`` = 84uy
    | playDrum = 85uy
    | ``playSound:`` = 86uy
    | ``pointTowards:`` = 87uy
    | putPenDown = 88uy
    | putPenUp = 89uy
    | ``randomFrom:to:`` = 90uy
    | readVariable = 91uy
    | ``rest:elapsed:from:`` = 92uy
    | rounded = 93uy
    | ``say:`` = 94uy
    | ``say:duration:elapsed:from:`` = 95uy
    | scale = 96uy
    | sceneName = 97uy
    | ``setGraphicEffect:to:`` = 98uy
    | ``setLine:ofList:to:`` = 99uy
    | ``setPenHueTo:`` = 100uy
    | ``setPenShadeTo:`` = 101uy
    | setRotationStyle = 102uy
    | ``setSizeTo:`` = 103uy
    | ``setTempoTo:`` = 104uy
    | ``setVar:to:`` = 105uy
    | ``setVolumeTo:`` = 106uy
    | show = 107uy
    | ``showBackground:`` = 108uy
    | ``showList:`` = 109uy
    | ``showVariable:`` = 110uy
    | sqrt = 111uy
    | stampCostume = 112uy
    | startScene = 113uy
    | startSceneAndWait = 114uy
    | stopAll = 115uy
    | stopAllSounds = 116uy
    | stopScripts = 117uy
    | ``stringLength:`` = 118uy
    | tempo = 119uy
    | ``think:`` = 120uy
    | ``think:duration:elapsed:from:`` = 121uy
    | timeAndDate = 122uy
    | timer = 123uy
    | timerReset = 124uy
    | timestamp = 125uy
    | ``touching:`` = 126uy
    | ``touchingColor:`` = 127uy
    | ``turnLeft:`` = 128uy
    | ``turnRight:`` = 129uy
    | volume = 130uy
    | ``wait:elapsed:from:`` = 131uy
    | warpSpeed = 132uy
    | xpos = 133uy
    | ``xpos:`` = 134uy
    | ypos = 135uy
    | ``ypos:`` = 136uy
    | ``|`` = 137uy

    | whenClicked = 138uy
    | whenGreenFlag = 139uy
    | whenCloned = 140uy
    | whenIReceive = 141uy
    | whenKeyPressed = 142uy
    | whenSceneStarts = 143uy

    | Extension = 144uy

    | KNOWN_MAX = 144uy

type O = Symbol

type Expression<'a> =
    | Literal of state: 'a * value: SValue
    | Complex of 'a ComplexExpression
    | Block of 'a BlockExpression

and [<Struct>] ComplexExpression<'a> = ComplexExpression of state: 'a * operator: Symbol * operands: 'a Expression list
and [<Struct>] BlockExpression<'a> = BlockExpression of state: 'a * body: 'a ComplexExpression list

type ListenerDefinition<'a> = ListenerDefinition of state: 'a * name: Symbol * arguments: 'a Expression list * body: 'a BlockExpression
type ParameterDefinition<'a> = ParameterDefinition of state: 'a * name: string * defaultValue: SValue

[<Struct>]
type Atomicity =
    | Atomic
    | NoAtomic

type ProcedureDefinition<'a> =
    | ProcedureDefinition of
        state: 'a *
        name: string *
        parameters: 'a ParameterDefinition list *
        isAtomic: Atomicity *
        body: 'a BlockExpression

type Script<'a> =
    | Listener of 'a ListenerDefinition
    | Procedure of 'a ProcedureDefinition
    | Statements of 'a BlockExpression
    | Expression of 'a ComplexExpression

[<Struct; NoEquality; NoComparison>]
type TypeVar = TypeVar of string

[<RequireQualifiedAccess>]
type TsType =
    | Named of name: string * TsType list
    | StringSs of string list
    | Or of TsType * TsType

    /// `'a`
    | GVar of index: int

/// `<%vars> : %t`
[<Struct>]
[<NoComparison; NoEquality>]
type TypeScheme = TypeScheme of vars: TypeVar list * t: TsType

[<RequireQualifiedAccess>]
type OperandType<'t> =
    | Expression of 't
    | Variable
    | ListVariableExpression of itemType: 't
    | ParameterName
    | ProcedureNameAndExpressions
    | ExtensionNameAndExpressions
    | StringLiterals of string Set
    | Block

[<RequireQualifiedAccess>]
type ListenerHeaderType =
    | EventName

    /// "any" | S
    | AnyOrKeyName

    | String
    | Null
    | Bool

[<Struct>]
type ScriptData<'S,'a> = {
    x: number
    y: number
    script: 'S
}
type ScriptData<'a> = ScriptData<'a Script,'a>

module PartialData =
    type PartialCostumeData = {
        costumeName: string option
        baseLayerMD5: string option
        baseLayerID: double option
        rotationCenterX: double option
        rotationCenterY: double option

        textLayerMD5: string option
        textLayerID: double option
        bitmapResolution: double option
    }

type CostumeData = {
    baseLayerMD5: string
    baseLayerID: number
    textLayerMD5: string option
    textLayerID: number option

    bitmapResolution: number option
    costumeName: string
    rotationCenterX: number
    rotationCenterY: number
}

[<Struct>]
type SoundRate = R11025 | R22050 | R48000
module SoundRate =
    let toNumber = function
        | R11025 -> 11025.
        | R22050 -> 22050.
        | R48000 -> 48000.

[<Struct>]
type SoundFormat =
    /// ""
    | EmptyFormat
    /// "adpcm"
    | Adpcm

module SoundFormat =
    let toString = function
        | EmptyFormat -> ""
        | Adpcm -> "adpcm"

type SoundData = {
    md5: string
    soundID: number
    soundName: string
    sampleCount: number option
    rate: SoundRate option
    format: SoundFormat option
}

[<Struct>]
type Persistence =
    | Persistent
    | NoPersistent

[<Struct>]
type Visibility =
    | Visible
    | Hidden

[<Struct>]
type ListVariableData<'a> = {
    state: 'a
    isPersistent: Persistence
    listName: string
    contents': SValue ImmutableArray

    x: double
    y: double
    width: double
    height: double
    visible: Visibility
}

type VariableData<'a> = {
    state: 'a
    isPersistent: Persistence
    name: string
    value: SValue
}

[<Struct; RequireQualifiedAccess>]
type RotationStyle =
    /// "none"
    | None
    /// "normal"
    | Normal
    /// "leftRight"
    | LeftRight
    
// type SimpleWatcherCommand =
//     | "getVar:"
//     | "sensor:"
//     | "sensorPressed"
//     | "senseVideoMotion"
//     | "answer"
//     | "backgroundIndex"
//     | "costumeIndex"
//     | "sceneName"
//     | "soundLevel"
//     | "tempo"
//     | "timer"
//     | "volume"
//     | "xpos"
//     | "ypos"

// type WatcherCommand =
//     | SimpleWatcherCommand
//     | "timeAndDate"
//     | "heading"
//     | "scale"
type WatcherCommand = string

// interface EntityData {
//     readonly cmd?: WatcherCommand
//     readonly listName?: string
// }
// interface WatcherData extends EntityData {
//     readonly cmd: WatcherCommand

//     readonly param: string | null
//     readonly target: string

//     readonly color?: number

//     readonly isDiscrete?: boolean | null
//     readonly label?: string
//     readonly mode?: 1 | 3
//     readonly sliderMax?: number | null
//     readonly sliderMin?: number | null
//     readonly visible?: boolean | null
//     readonly x?: number | null
//     readonly y?: number | null
// }
type WatcherData = {
    cmd: WatcherCommand

    param: string option
    target: string

    color: number option

    /// isDiscrete?: booelean | null
    isDiscrete: boolean nullable optional
    label: string option
    /// 1 | 3
    mode: number option
    sliderMax: number nullable optional
    sliderMin: number nullable optional
    visible: Visibility nullable optional
    x: number nullable optional
    y: number nullable optional
}

type EntityData<'Extension,'Script,'Variable,'List,'a> = {
    objName: string
    scripts: ReadonlyArray<ScriptData<'Script,'a>>
    costumes: ReadonlyArray<CostumeData>
    sounds: ReadonlyArray<SoundData>
    variables: ReadonlyArray<'Variable>
    lists: ReadonlyArray<'List>
    currentCostumeIndex: number

    ObjectDataExtension: 'Extension
}
type EntityData<'Extension,'a> = EntityData<'Extension,'a Script,'a VariableData,'a ListVariableData,'a>

type SpriteDataExtension = {
    direction: number
    indexInLibrary: number
    isDraggable: boolean
    rotationStyle: RotationStyle
    scale: number
    scratchX: number
    scratchY: number
    /// {}
    spriteInfo: Map<string, SValue>
    visible: Visibility
}
type SpriteData<'Script,'Variable,'List,'a> = EntityData<SpriteDataExtension,'Script,'Variable,'List,'a>
type SpriteData<'a> = SpriteData<'a Script,'a VariableData,'a ListVariableData,'a>

type StageDataChild<'Script,'Variable,'List,'a> = Choice<WatcherData, SpriteData<'Script,'Variable,'List,'a>, ListVariableData<'a>>
type StageDataChild<'a> = StageDataChild<'a Script,'a VariableData,'a ListVariableData,'a>

type StageDataExtension<'Script,'Variable,'List,'a> = {
    children: ReadonlyArray<StageDataChild<'Script,'Variable,'List,'a>>

    penLayerMD5: string nullable
    penLayerID: number option
    tempoBPM: number
    videoAlpha: number option
    info: Map<string, SValue>
}
type StageDataExtension<'a> = StageDataExtension<'a Script,'a VariableData,'a ListVariableData,'a>
type StageData<'Script,'Variable,'List,'a> = EntityData<StageDataExtension<'Script,'Variable,'List,'a>,'Script,'Variable,'List,'a>
type StageData<'a> = StageData<'a Script,'a VariableData,'a ListVariableData,'a>

[<Struct; RequireQualifiedAccess>]
type SType =
    | S
    | N
    | B

module SType =
    let scratchParameterTypeName = function
        | SType.S -> "s"
        | SType.N -> "n"
        | SType.B -> "b"
    
    let parameterDefaultValue = function
        | SType.N -> SNumber 1.
        | SType.B -> SBool false
        | SType.S -> SString ""

[<Struct>]
type VType =
    | Any
    | Typed of SType
