namespace Scratch.Executor
open System
open System.Collections.Generic
open Scratch
open Scratch.Primitives
open Scratch.Ast
open Scratch.Executor


[<Struct>]
type Filter =
    | Whirl = 1
    | Fisheye = 2
    | Brightness = 3
    | Pixelate = 4
    | Mosaic = 5
    | Color = 6
    | Ghost = 7

[<Struct>]
type Instruction = {
    code: Code
    operand1: int64
}

[<Struct>]
type Stack<'T> = {
    mutable next: 'T index
    mutable items: 'T[]
}

[<Struct>]
type Parameter = {
    parameterName: string
}

[<Struct>]
type Procedure = {
    /// absolute address
    startAddress: Instruction index
    codeLength: int
    parameterCount: int
    localCount: int
    isAtomic: bool

    // debug info
    name: string
    parameters: Parameter iarray
}
[<Struct>]
type Flame = {
    returnAddress: Instruction index
    argumentsOrigin: Value index
    localsOrigin: Value index
    procedure: Procedure index
    isAtomicContext: bool
}

[<Struct>]
type Broadcast = {
    broadcastName: LowerName
}
type VariableImage<'T> = {
    variableName: string
    initialValue: 'T
}
type CostumeImage = {
    costumeName: string
}
type SpriteImage = {
    visible: Visibility
}

type EntityImage<'a> = {
    broadcastListeners: IndexMap<Broadcast, Procedure index iarray_seq>
    whenGreenFlags: Procedure index iarray_seq
    whenCloned: Procedure index iarray_seq
    whenClicked: Procedure index iarray_seq
    scalarVariables: 'a VariableData iarray
    listVariables: 'a ListVariableData iarray
    costumes: CostumeData iarray
    sounds: SoundData iarray
    currentCostumeIndex: double option
    spriteImage: SpriteDataExtension option

    // debug info
    entityName: string
    definedProcedures: Procedure index iarray
}
type Image<'a> = {
    broadcasts: Broadcast iarray
    instructions: Instruction iarray
    stringLiterals: string iarray
    procedures: Procedure iarray
    locationMap: IndexMap<Instruction,'a>

    stageImage: 'a EntityImage
    spriteImages: 'a EntityImage iarray
}

[<ReferenceEquality; NoComparison>]
type EntityState<'a> = {
    
    // instance state
    isClone: bool
    scalarVariables: Value[]
    listVariables: Value ResizeArray[]
    mutable drawingData: EntityDrawingData
    mutable spriteDrawingData: SpriteDrawingData
    mutable sayVersion: SayVersion

    // entity metadata
    entityImage: 'a EntityImage
    costumeNameToIndex: Map<string, int>
    soundNameToIndex: Map<string, int>
}

type StageState<'a> = {
    showState: 'a -> string
    stageState: 'a EntityState

    originalSpriteMap: Dictionary<string,'a EntityState>
    instances: 'a EntityState ResizeArray

    mutable timerEpoch: DateTime
    mutable answer: string
    mutable tempo: double
    mutable currentAskVersion: AskVersion
    mutable nextAskVersion: AskVersion
    mutable showInputBox: bool
    userId: int
    userName: string

    image: 'a Image
}

type IRandom =
    abstract NextDouble: unit -> double
    abstract Next: low: int * high: int -> int
    
module Random =
    [<Struct>]
    type SystemRandom = SystemRandom of Random with
        interface IRandom with
            member x.NextDouble() =
                let (SystemRandom x) = x
                x.NextDouble()
            member x.Next(l, h) =
                let (SystemRandom x) = x
                x.Next(l, h)

    let system seed = SystemRandom(Random seed)

    let nextDouble (random: 'T byref when 'T :> IRandom and 'T : struct) = random.NextDouble()
    let nextRange (random: 'T byref when 'T :> IRandom and 'T : struct) (low, high) = random.Next(low, high)

module Stack =
    let create length = { items = Array.zeroCreate length; next = Index.zero }

    let address (stack: _ byref) index =
        &stack.items.Address(index)

    let push (stack: _ byref) value =
        let next = stack.next
        stack.items.Address(next) <- value
        stack.next <- next + 1

    let pop (stack: _ byref) =
        let next = stack.next - 1
        stack.next <- next
        stack.items.Address(next)

    let isEmpty (stack: _ byref) =
        Index.isZero stack.next

    let extendNoinit (stack: _ byref) count =
        stack.next <- stack.next + count

    let truncate (stack: _ byref) count =
        stack.next <- stack.next - count
