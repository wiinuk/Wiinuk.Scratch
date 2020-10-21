module Scratch.Serialization.Sb3.Ast
open Scratch
open Scratch.Primitives
open Scratch.Json.PartialIsomorphisms.Iso


/// p?: 'a
type 'a optional = 'a option
/// 'a | null
type 'a nullable = 'a option

// e.g. ["varName", 330], ["varName", "a", true]
type VariableData = VariableData of name: string * initial: SValue * isCloud: bool

// e.g. ["listName", []], ["listName", ["a"], true]
type ListData = ListData of name: string * initial: SValue ImmutableArray * isCloud: bool
[<Struct>]
type BroadcastData = BroadcastData of name: string

[<Struct>]
type Position = Position of x: double * y: double

[<Struct>]
type Id<'Phantom> = Id of string
let makeId = {
    forward = fun x -> Ok(Id x)
    reverse = fun (Id x) -> Ok x
}
[<Sealed; AbstractClass>]
type BlockPhantom = class end
type BlockId = BlockPhantom Id
[<Sealed; AbstractClass>]
type BroadcastPhantom = class end
type BroadcastId = BroadcastPhantom Id
[<Sealed; AbstractClass>]
type VariableOrListPhantom = class end
type VariableOrListId = VariableOrListPhantom Id
[<Sealed; AbstractClass>]
type InputPhantom = class end
type InputId = InputPhantom Id
[<Sealed; AbstractClass>]
type FieldPhantom = class end
type FieldId = FieldPhantom Id
[<Sealed; AbstractClass>]
type CommentPhantom = class end
type CommentId = CommentPhantom Id

/// "default" | "list" | "large" | "slider"
type MonitorMode = string
type Monitor = {
    // e.g. "*`2fy7Nt#CXpF2Y~=O#5-.variable-"
    id: string
    mode: MonitorMode
    // e.g. "data_variable", "data_listcontents"
    opcode: string
    ``params``: OMap<string, SValue>
    // spriteName: string | null
    spriteName: string option
    value: SValue
    width: double option
    height: double option
    x: double
    y: double
    visible: bool
    min: double option
    max: double option
}

(*
/**
 * @template T
 * @typedef {[T, string|null]} StringOrNullInput
 */
/**
 * @template T
 * @typedef {[T, string|null, string|null]} StringOrNullStringOrNullInput
 */
/**
 * @template T
 * @typedef {[T, Value]} ValueInput
 */
/**
 * @template T
 * @typedef {[T, Value, string]} ValueStringInput
 */
/**
 * @template T
 * @typedef {[T, Value, string, number, number]} ValueStringNumberNumberInput
 */
/**
 * @typedef { StringOrNullInput<typeof INPUT_SAME_BLOCK_SHADOW> | StringOrNullInput<typeof INPUT_BLOCK_NO_SHADOW> } StringOrNullInputs
 * @typedef { StringOrNullStringOrNullInput<typeof INPUT_DIFF_BLOCK_SHADOW> } StringOrNullStringOrNullInputs
 * @typedef { ValueInput<typeof MATH_NUM_PRIMITIVE> | ValueInput<typeof POSITIVE_NUM_PRIMITIVE> | ValueInput<typeof WHOLE_NUM_PRIMITIVE> | ValueInput<typeof INTEGER_NUM_PRIMITIVE> | ValueInput<typeof ANGLE_NUM_PRIMITIVE> | ValueInput<typeof COLOR_PICKER_PRIMITIVE> | ValueInput<typeof TEXT_PRIMITIVE> } ValueInputs
 * @typedef { ValueStringInput<typeof BROADCAST_PRIMITIVE> | ValueStringInput<typeof VAR_PRIMITIVE> | ValueStringInput<typeof LIST_PRIMITIVE> } ValueStringInputs
 * @typedef { ValueStringNumberNumberInput<typeof VAR_PRIMITIVE> | ValueStringNumberNumberInput<typeof LIST_PRIMITIVE> } ValueStringNumberNumberInputs
 * 
 * @typedef { StringOrNullInputs | ValueInputs | ValueStringInputs | StringOrNullStringOrNullInputs | ValueStringNumberNumberInputs } InputDescriptor
 * @typedef {InputDescriptor} CompressedInputData
 */

const INPUT_SAME_BLOCK_SHADOW = 1; // unobscured shadow
const INPUT_BLOCK_NO_SHADOW = 2; // no shadow
const INPUT_DIFF_BLOCK_SHADOW = 3; // obscured shadow

// math_number
const MATH_NUM_PRIMITIVE = 4; // there's no reason these constants can't collide
// math_positive_number
const POSITIVE_NUM_PRIMITIVE = 5; // with the above, but removing duplication for clarity
// math_whole_number
const WHOLE_NUM_PRIMITIVE = 6;
// math_integer
const INTEGER_NUM_PRIMITIVE = 7;
// math_angle
const ANGLE_NUM_PRIMITIVE = 8;
// colour_picker
const COLOR_PICKER_PRIMITIVE = 9;
// text
const TEXT_PRIMITIVE = 10;
// event_broadcast_menu
const BROADCAST_PRIMITIVE = 11;
// data_variable
const VAR_PRIMITIVE = 12;
// data_listcontents
const LIST_PRIMITIVE = 13;
*)
type SimpleBlock =
    // null
    | EmptyBlock

    // string
    | BlockReference of id: BlockId

    // [1, id: string]
    | SameBlockShadow of SimpleBlock
    // [2, id: string]
    | BlockNoShadow of SimpleBlock
    // [3, id: string, shadowId: string]
    | DiffBlockShadow of block: SimpleBlock * shadow: SimpleBlock

    // [4, value: number | "all"]
    | MathNumber of value: SValue
    // [5, number]
    | MathPositiveNumber of value: SValue
    // [6, number]
    | MathWholeNumber of value: SValue
    // [7, number]
    | MathInteger of value: SValue
    // [8, number]
    | MathAngle of value: SValue
    // [9, string]
    // e.g. "#0006EF"
    | ColourPicker of colorHexString: SValue
    // [10, string]
    | Text of value: SValue

    // [11, string, string]
    | EventBroadcastMenu of name: string * id: BroadcastId
    // [12, string, string]
    // [12, string, string, number, number]
    | DataVariable of name: string * id: VariableOrListId * topLevelPosition: Position option
    // [13, string, string]
    // [13, string, string, number, number]
    | DataListContents of name: string * id: VariableOrListId * topLevelPosition: Position option

type Input = SimpleBlock
// e.g. ["varName", "Mj|0yZ(tpU!jW(1CeF4T-varName-"] | ["varName"]
//      ["broadcastName", "broadcastMsgId-broadcastName"]
type Field = {
    value: SValue
    name: string nullable optional
}

type Mutation = {
    // e.g. "mutation"
    tagName: string optional
    // children?: []
    children: HUnit optional
    // e.g. "procName %n %s"
    proccode: string optional
    // e.g. "[]", "[\"input0\",\"input1\"]"
    argumentids: string optional

    // e.g. "[]", "[1]", "[1,\"\"]", "[1,1]"
    argumentdefaults: string optional
    // e.g. "[]", "[\"paramName\"]"
    argumentnames: string optional
    warp: bool optional
    hasnext: bool optional
}
type ComplexBlock<'Input> = {
    // e.g. "event_whenflagclicked"
    opcode: string nullable
    // e.g. "U~8x:mJQPp#9?rgJLZHL"
    next: BlockId nullable
    // e.g. "#D|Ew|:fKLDuUoJ+y4Wc"
    parent: BlockId nullable optional
    // e.g. { "...": ... }
    inputs: OMap<InputId, 'Input>
    // e.g. { "...": ... }
    fields: OMap<FieldId, Field>
    shadow: bool
    topLevel: bool
    x: double optional
    y: double optional
    comment: CommentId optional
    mutation: Mutation optional
}
type CompressedBlock =
    | Complex of Input ComplexBlock
    | Simple of SimpleBlock

type Comment = {
    blockId: BlockId nullable
    x: double nullable
    y: double nullable
    width: double
    height: double
    minimized: bool
    text: string
}
type Costume = {
    // e.g. "b00b100000ea1000abee90000a7903ff"
    assetId: string
    name: string
    // e.g. 2
    bitmapResolution: double
    // e.g. "b00b100000ea1000abee90000a7903ff.png"
    md5ext: string
    // e.g. "png"
    dataFormat: string
    rotationCenterX: double
    rotationCenterY: double
}
type Sound = {
    // e.g. "b00b100000ea1000abee90000a7903ff"
    assetId: string
    name: string
    // e.g. "wav"
    dataFormat: string
    // e.g. ""
    format: string
    // e.g. 44100
    rate: double
    sampleCount: double
    // e.g. "b00b100000ea1000abee90000a7903ff.wav"
    md5ext: string
}
type RotationStyle =
    /// "all around"
    | AllAround
    /// "left-right"
    | LeftRight
    /// "don't rotate"
    | DontRotate

[<RequireQualifiedAccess>]
type VideoState =
    /// "off"
    | Off
    /// "on"
    | On
    /// "on-flipped"
    | OnFlipped

type Target<'Block> = {
    isStage: bool
    name: string
    // e.g. { "Mj|0yZ(tpU!jW(1CeF4T-name-": ... }
    variables: OMap<VariableOrListId, VariableData>
    // e.g. { "Mj|0yZ(tpU!jW(1CeF4T-listName-list": ... }
    lists: OMap<VariableOrListId, ListData>
    // e.g. { "broadcastMsgId-broadcastName": ... }
    broadcasts: OMap<BroadcastId, BroadcastData>
    // e.g. { "`9bzdi@C-d[BPZq98Kdj": ... }
    blocks: OMap<BlockId,'Block>
    // e.g. { "bQ)`RwYK7VJc28VZF|H8": ... }
    comments: OMap<CommentId, Comment>
    // e.g. 0
    currentCostume: double
    costumes: Costume list
    sounds: Sound list

    // e.g. 100
    volume: double optional
    layerOrder: double optional
    tempo: double optional
    videoTransparency: double optional
    videoState: VideoState optional
    textToSpeechLanguage: string nullable optional
    visible: bool optional
    x: double optional
    y: double optional
    size: double optional
    direction: double optional
    draggable: bool optional
    // e.g. "all around"
    rotationStyle: RotationStyle optional
    //targetPaneOrder: double optional
}

type Meta = {
    // e.g. "3.0.0"
    semver: string
    // e.g. "0.2.0-prerelease.20190116202234"
    vm: string
    // e.g. "Mozilla/5.0 ...", null
    agent: string nullable
}
type Project = {
    targets: CompressedBlock Target list
    monitors: Monitor list
    // e.g. "pen"
    extensions: string list
    meta: Meta
}
