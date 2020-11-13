namespace Scratch.Evaluator
open System
open Scratch
open Scratch.Primitives
open Scratch.Ast
open Scratch.Threading


[<NoComparison>]
type SpriteState<'a> = {
    mutable spriteDrawingData: SpriteDrawingData

    mutable sayVersion: SayVersion
    whenCloned: 'a ListenerDefinition list
}
module SpriteState =
    let ofSpriteData data =
        let whenCloned =
            data.scripts
            |> Seq.choose (function
                | { script = Listener(ListenerDefinition(name = O.whenCloned) as x) } -> Some x 
                | _ -> None
            )
            |> Seq.toList

        let ex = data.ObjectDataExtension
        {
            whenCloned = whenCloned

            spriteDrawingData = {
            SpriteDrawingData.initialData() with
                visible = match ex.visible with Visible -> true | Hidden -> false
                x = ex.scratchX
                y = ex.scratchY
                direction = ex.direction
                scale = ex.scale
                rotationStyle = ex.rotationStyle
            }
            sayVersion = Version 0
        }

/// クローンしても変化しないデータ
type ObjectStateShared<'a> = {
    objectName: string
    procs: Map<string, 'a ProcedureDefinition>

    whenGreenFlag: 'a ListenerDefinition list
    whenIReceive: Map<string, 'a ListenerDefinition list>

    costumes: CostumeData ImmutableArray
    costumeNameToIndex: Map<string, int>

    sounds: SoundData ImmutableArray
    soundNameToIndex: Map<string, int>
}

[<NoComparison; ReferenceEquality>]
type ObjectState<'a> = {
    mutable drawingData: EntityDrawingData

    isClone: bool
    values: Map<string, SValue ref>
    lists: Map<string, SValue ResizeArray>
    sprite: 'a SpriteState voption

    shared: 'a ObjectStateShared
}

[<RequireQualifiedAccess>]
type RuntimeVersion =
    | Sb2
    | Sb3

[<NoComparison; NoEquality>]
type EvaluateConfig<'a,'I,'O,'C> = {
    version: RuntimeVersion
    showState: 'a -> string
    randomNext: struct(int * int) -> int
    randomNextDouble: unit -> double
    useRangeCheck: bool
    useFilterNameCheck: bool
    useVariableDefinitionCheck: bool
    useLengthCheck: bool
    useCloudValueCheck: bool
    listMaxLength: int option
    schedulerConfig: SchedulerConfig

    userId: int64
    userName: string

    initialInput: 'I
    initialView: 'O
    initialCloud: 'C
}

module EvaluateConfig =
    let makeDefault() =
        let r = Random 0xCAFEBABE
        {
            version = RuntimeVersion.Sb2
            showState = sprintf "%A"
            randomNext = fun struct(lo, hi) -> r.Next(lo, hi)
            randomNextDouble = r.NextDouble
            useRangeCheck = false
            useFilterNameCheck = false
            useVariableDefinitionCheck = false
            useLengthCheck = false
            useCloudValueCheck = false
            listMaxLength = None

            schedulerConfig = {
                startTime = Some <| DateTime(2000, 1, 1)
                flame = Limited <| TimeSpan.FromSeconds(1. / 30.)
                deterministic = DeterministicTime {
                    processSpan = TimeSpan.FromTicks 1L
                    flameSpan = TimeSpan.FromSeconds (1. / 30.)
                }
            }
            userId = 0L
            userName = ""
            initialView = StageView.ignore
            initialInput = Input.nil
            initialCloud = Cloud.offline()
        }

    let withView view config = {
        version = config.version
        showState = config.showState
        randomNext = config.randomNext
        randomNextDouble = config.randomNextDouble
        useRangeCheck = config.useRangeCheck
        useVariableDefinitionCheck = config.useVariableDefinitionCheck
        useFilterNameCheck = config.useFilterNameCheck
        useLengthCheck = config.useLengthCheck
        useCloudValueCheck = config.useCloudValueCheck
        listMaxLength = config.listMaxLength
        schedulerConfig = config.schedulerConfig
        userId = config.userId
        userName = config.userName
        initialView = view
        initialInput = config.initialInput
        initialCloud = config.initialCloud
    }
    let withInput input config = {
        version = config.version
        showState = config.showState
        randomNext = config.randomNext
        randomNextDouble = config.randomNextDouble
        useRangeCheck = config.useRangeCheck
        useFilterNameCheck = config.useFilterNameCheck
        useVariableDefinitionCheck = config.useVariableDefinitionCheck
        useLengthCheck = config.useLengthCheck
        useCloudValueCheck = config.useCloudValueCheck
        listMaxLength = config.listMaxLength
        schedulerConfig = config.schedulerConfig
        userId = config.userId
        userName = config.userName
        initialView = config.initialView
        initialInput = input
        initialCloud = config.initialCloud
    }
    let withCloud cloud config = {
        version = config.version
        showState = config.showState
        randomNext = config.randomNext
        randomNextDouble = config.randomNextDouble
        useRangeCheck = config.useRangeCheck
        useFilterNameCheck = config.useFilterNameCheck
        useVariableDefinitionCheck = config.useVariableDefinitionCheck
        useLengthCheck = config.useLengthCheck
        useCloudValueCheck = config.useCloudValueCheck
        listMaxLength = config.listMaxLength
        schedulerConfig = config.schedulerConfig
        userId = config.userId
        userName = config.userName
        initialView = config.initialView
        initialInput = config.initialInput
        initialCloud = cloud
    }

type SharedState = {
    mutable answer: string
    mutable tempo: double

    mutable currentAskVersion: AskVersion
    mutable nextAskVersion: AskVersion
    mutable showInputBox: bool
    timerEpoch: DateTime ref
}

[<NoComparison; NoEquality>]
type EvaluateState<'a,'I,'O,'C> = {
    data: StageData<'a>
    config: EvaluateConfig<'a,'I,'O,'C>

    clouds: string Set
    scheduler: 'a ObjectState Scheduler
    stage: 'a ObjectState
    sharedState: SharedState
    objects: 'a ObjectState ResizeArray
    originalSprites: 'a ObjectState ImmutableArray
    originalSpriteMap: Map<string, 'a ObjectState>

    // NOTE:
    // サイズが不明な値型のコピーを回避するのと、値型フィールドの内部状態を変更することを許可するため、`mutable` にする
    mutable input: 'I
    mutable view: 'O
    mutable cloud: 'C
}

[<Struct>]
[<NoComparison; NoEquality>]
type StackFlame<'a> = StackFlame of callerLocation: 'a * script: 'a Script * arguments: Map<string, SValue> * self: 'a ObjectState

[<Struct>]
[<NoComparison; NoEquality>]
type BlockState<'a,'I,'O,'C> = {
    blockState: EvaluateState<'a,'I,'O,'C>
    args: Map<string, SValue>
    isAtomic: bool
    self: 'a ObjectState

    callStack: 'a StackFlame list
}

[<AutoOpen>]
module internal Internals =
    let sEmptyString = SString ""
    let sTrue = SBool true
    let sFalse = SBool false

    let toNumberZeroIfNaN x =
        let x = SValue.toNumber x
        if Double.IsNaN x then 0. else x

    let stackMessage state location e =
        state.callStack
        |> List.map (fun (StackFlame(location, script, arguments, self)) ->
            let name =
                match script with
                | Procedure(ProcedureDefinition(name = n)) -> n
                | Listener(ListenerDefinition(name = n))
                | Expression(ComplexExpression(operator = n)) -> Symbol.name n
                | Statements(BlockExpression _) -> "<%block>"

            let arguments = arguments |> Map.toSeq |> Seq.map (fun (k, v) -> $"%s{k}={v}") |> String.concat ","

            $"\r\n%s{state.blockState.config.showState location}: %s{self.shared.objectName}.%s{name} {{{arguments}}}"
        )
        |> String.concat ""
        |> sprintf "%s: %s%s" (state.blockState.config.showState location) e

    let cfailwithf state location format = Printf.kprintf (stackMessage state location >> failwithf "%s") format

    let inline tryFind k map = Map.tryFind k map

    let getListOrRaise (state: _ inref) location name =
        match tryFind name state.self.lists with
        | ValueSome xs -> xs
        | ValueNone ->

        match tryFind name state.blockState.stage.lists with
        | ValueSome xs -> xs
        | ValueNone ->

        if state.blockState.config.useVariableDefinitionCheck then
            cfailwithf state location $"list '%s{name}' not found"
        else
            // TODO:
            ResizeArray()

    let getValueOrRaise (state: _ inref) location name =
        match tryFind name state.self.values with
        | ValueSome v -> v
        | ValueNone ->

        match tryFind name state.blockState.stage.values with
        | ValueSome xs -> xs
        | ValueNone ->

        if state.blockState.config.useVariableDefinitionCheck then
            cfailwithf state location $"variable '%s{name}' not found"
        else
            // TODO:
            ref (SString "")

    let getProcedureOrRaise (state: _ inref) location name =
        match Map.tryFind name state.self.shared.procs with
        | ValueNone -> cfailwithf state location $"procedure '%s{name}' not found"
        | ValueSome proc -> proc

    let setCloudValue (state: _ inref) location name v =
        if not <| SValue.isCloudValue v then
            if state.blockState.config.useCloudValueCheck then
                cfailwithf state location $"invalid cloud value: serVar:to:, '%s{name}' '{v}'"
        else
            Cloud.set &state.blockState.cloud name v

    let getCloudValue (state: _ inref) name =
        Cloud.get &state.blockState.cloud name
