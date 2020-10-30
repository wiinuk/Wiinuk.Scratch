namespace Scratch.Serialization.Sb3
open Scratch
open Scratch.Primitives
open Scratch.Serialization.Sb3.Ast
open Scratch.Serialization.Sb3.OpCodeSpecs
open System.Text.RegularExpressions


module Target =
    let defaultStage = {
        isStage = true
        name = "Stage"
        variables = OMap.empty
        lists = OMap.empty
        broadcasts = OMap.empty
        blocks = OMap.empty
        comments = OMap.empty
        currentCostume = -1.
        costumes = []
        sounds = []
        volume = Some 100.
        layerOrder = Some 0.
        tempo = Some 60.
        videoTransparency = Some 50.
        videoState = Some VideoState.On
        textToSpeechLanguage = Some None

        visible = None
        x = None
        y = None
        size = None
        direction = None
        draggable = None
        rotationStyle = None
    }

module Mutation =
    let defaultValue = {
        tagName = None
        children = None
        proccode = None
        argumentids = None
        argumentdefaults = None
        argumentnames = None
        warp = None
        hasnext = None
    }

module Meta =
    let defaultValue = {
        semver = "3.0.0"
        vm = "0.2.0"
        agent = Some "none"
    }

module VariableType =
    let namingText = function
        | VariableType.Scalar -> ""
        | VariableType.List -> "list"
        | VariableType.BroadcastMessage -> "broadcast_msg"

module Id =
    open System
    open System.Security.Cryptography
    open System.Threading


    let create (_: 'Phantom The) x: 'Phantom Id = Id x
    let toString (Id x) = x

    let escape input = Regex.Replace(input, @"[<>&'""]", evaluator = fun m ->
        match m.Value with
        | "<" -> "lt"
        | ">" -> "gt"
        | "&" -> "amp"
        | "'" -> "apos"
        | "\"" -> "quot"
        | x -> x
    )
    let naming phantom targetId name type' = create phantom <| sprintf "%s-%s-%s" targetId (escape name) type'
    let createVariableId (topLevel, targetId) (globalVariableNameToId, name, type') =
        if topLevel then
            let freshName = naming the<VariableOrListPhantom> targetId name <| VariableType.namingText type'
            struct(freshName, Map.add struct(name, type') freshName globalVariableNameToId)
        else
            match globalVariableNameToId |> Map.tryFind (name, type')  with
            | ValueSome name -> name, globalVariableNameToId
            | _ -> naming the<VariableOrListPhantom> targetId name (VariableType.namingText type'), globalVariableNameToId

    let private uniqueIdChars = "!#%()*+,-./:;=?@[]^_`{|}~ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    let private uniqueIdLength = 20
    let private makeRandom() =
        use random0 = new RNGCryptoServiceProvider()
        let buffer = Array.zeroCreate sizeof<int>
        random0.GetBytes buffer
        Random <| BitConverter.ToInt32(buffer, 0)

    let private uniqueIdState = new ThreadLocal<_>(fun _ -> struct(Array.zeroCreate uniqueIdLength, makeRandom()))
    let newUniqueId() =
        let struct(builder, random) = uniqueIdState.Value
        for i in 0..builder.Length-1 do
            let j = int (random.NextDouble() * double uniqueIdChars.Length)
            builder.[i] <- uniqueIdChars.[j]

        String builder

    let createBroadcastIdCore (name: string) =
        let name = name.ToLowerInvariant()
        escape name |> sprintf "broadcastMsgId-%s" |> create the<BroadcastPhantom>

    let createBroadcastId = function
        | "" -> ValueNone
        | n -> createBroadcastIdCore n |> ValueSome

module Color =
    /// 0x0099EE ⇒ "#0099ee"
    let numberToColorCode n =
        let x = if n < 0. then n + double 0xFFFFFF + 1. else n
        sprintf "#%06x" <| int x

module OpCodes =
    let [<Literal>] text = "text"
    let [<Literal>] boolean = "boolean"
    let [<Literal>] note = "note"
    let [<Literal>] substack = "substack"
    let [<Literal>] data_variable = "data_variable"
    let [<Literal>] data_listcontents = "data_listcontents"
    let [<Literal>] math_number = "math_number"
    let [<Literal>] math_whole_number = "math_whole_number"
    let [<Literal>] math_positive_number = "math_positive_number"
    let [<Literal>] math_integer = "math_integer"
    let [<Literal>] math_angle = "math_angle"
    let [<Literal>] colour_picker = "colour_picker"
    let [<Literal>] event_broadcast_menu = "event_broadcast_menu"
    let [<Literal>] sensing_of_object_menu = "sensing_of_object_menu"

    let [<Literal>] ``music.menu.DRUM`` = "music.menu.DRUM"
    let [<Literal>] ``music.menu.INSTRUMENT`` = "music.menu.INSTRUMENT"
    let [<Literal>] ``videoSensing.menu.ATTRIBUTE`` = "videoSensing.menu.ATTRIBUTE"
    let [<Literal>] ``videoSensing.menu.SUBJECT`` = "videoSensing.menu.SUBJECT"
    let [<Literal>] ``videoSensing.menu.VIDEO_STATE`` = "videoSensing.menu.VIDEO_STATE"

module Project =
    open Scratch.Ast
    open System.Collections.Generic
    module Op = OpCodes

    let defaultValue = {
        targets = [Target.defaultStage]
        monitors = []
        extensions = []
        meta = Meta.defaultValue
    }

    let collectBroadcastsAtEntity uniqueId map entity =
        entity.scripts
        |> List.fold (fun map s ->
            let map =
                match s.script with
                | Listener(ListenerDefinition(name = O.whenIReceive; arguments = Literal(_, SString name)::_)) ->
                    OMap.add (Id.createBroadcastId name |> VOption.defaultValue uniqueId) (BroadcastData <| name.ToLowerInvariant()) map
                | _ -> map

            s.script
            |> Script.fold (fun map e ->
                match e with
                | ComplexExpression(operator = O.``broadcast:`` | O.doBroadcastAndWait; operands = name::_) ->
                    let name =
                        match name with
                        | Literal(value = SString name) -> name
                        | _ -> ""

                    OMap.add (Id.createBroadcastId name |> VOption.defaultValue uniqueId) (BroadcastData <| name.ToLowerInvariant()) map
                | _ -> map
            ) map
        ) map

    let collectAllBroadcasts emptyNameId stage children =
        let map =
            children
            |> List.fold (fun map -> function
                | Choice2Of3 s -> collectBroadcastsAtEntity emptyNameId map s
                | _ -> map
            ) OMap.empty

        collectBroadcastsAtEntity emptyNameId map stage

    type InputSkeleton = {
        name: InputId
        block: BlockId optional
        shadow: BlockId optional
    }
    type FieldSkeleton = {
        name: string
        value: SValue
        /// broadcast ID or variable ID
        id: string optional
        variableType: VariableType optional
    }
    type BlockSkeleton<'Input> = {
        id: BlockId
        opcode: string
        inputs: OMap<InputId,'Input>
        fields: OMap<FieldId, FieldSkeleton>
        next: BlockId nullable
        topLevel: bool
        shadow: bool
        children: 'Input BlockSkeleton list

        parent: BlockId optional
        mutation: Mutation optional
        x: double optional
        y: double optional
    }
    type BlockBuilder = {
        currentTargetIsStage: bool
        currentTargetId: string
        parentOperandSpec: ArgInfo voption
        broadcastIdForEmptyBroadcastName: BroadcastId
        broadcastNameForEmptyBroadcastName: SValue

        globalVariableNameToId: Map<struct(string * VariableType), VariableOrListId> ref
    }

    module Builder =
        let createVariableId builder name variableType =
            let struct(id, map) =
                Id.createVariableId
                    (builder.currentTargetIsStage, builder.currentTargetId)
                    (!builder.globalVariableNameToId, name, variableType)
            builder.globalVariableNameToId := map
            id

        let createBroadcastId builder name =
            name
            |> Id.createBroadcastId
            |> VOption.defaultValue builder.broadcastIdForEmptyBroadcastName

    let private callArgMap0 = [EmptyArg]
    let procedureNameAsArgMap procedureName =
        match AstDefinitions.parseProcedureName procedureName with
        | ValueNone
        | ValueSome(_, []) -> callArgMap0
        | ValueSome(_, tail) ->

        [
        EmptyArg

        let mutable inputCount = 0
        for t, _ in tail do
            let inputOp =
                match t with
                | SType.N -> Op.math_number
                | SType.S -> Op.text
                | SType.B -> Op.boolean
            
            InputArg(inputOp, sprintf "input%d" inputCount, None)
            inputCount <- inputCount + 1
        ]

    let procedureNameToParameterIds procedureName =
        procedureNameAsArgMap procedureName
        |> List.choose (function InputArg(inputName = x) -> Some x | _ -> None)

    let blockSpecFromOperator operator = Map.tryFind operator sb2ExpressionSpecs
    let blockSpecFromExpression (ComplexExpression(operator = operator) as expression) =
        blockSpecFromOperator operator
        |> VOption.map (fun spec ->
            match expression with
            | ComplexExpression(_, O.call, Literal(_, SString procedureName)::_) ->
                { spec with argMap = procedureNameAsArgMap procedureName }

            | _ -> spec
        )

    [<Struct>]
    type BlocksAcc<'Input> = {
        acc: 'Input BlockSkeleton list
        previousBlock: 'Input BlockSkeleton voption
    }
    module BlocksAcc =
        let empty = { acc = []; previousBlock = ValueNone }
        let add { acc = acc; previousBlock = previousBlock } block =
            match previousBlock with
            | ValueSome previousBlock ->
                {
                    acc = { previousBlock with BlockSkeleton.next = Some block.id }::acc
                    previousBlock = ValueSome { block with parent = Some previousBlock.id }
                }

            | _ ->
                { acc = acc; previousBlock = ValueSome block }

        let toList { acc = acc; previousBlock = previousBlock } =
            let acc =
                match previousBlock with
                | ValueNone -> acc
                | ValueSome x -> x::acc

            List.rev acc

    let appendInputOperandBlocks block input = function
        | [] -> struct(block, input)
        | block0::blocks ->

        let block0 = { block0 with BlockSkeleton.parent = Some block.id }
        let operandBlocks = block0::blocks

        let block = { block with children = block.children @ operandBlocks }
        let input = { input with block = Some block0.id }
        block, input

    let createShadowField fieldName fieldValue shadowObscured = function
        | Op.math_number
        | Op.math_whole_number
        | Op.math_positive_number
        | Op.math_integer
        | Op.math_angle ->
            struct("NUM", if shadowObscured then SNumber 10. else fieldValue)

        | Op.text ->
            "TEXT", if shadowObscured then SValue.sEmptyString else fieldValue

        | Op.colour_picker ->
            let fieldValue =
                if shadowObscured then SString "#990000"
                else

                SValue.toNumber fieldValue
                |> Color.numberToColorCode
                |> SString

            "COLOUR", fieldValue

        | Op.event_broadcast_menu ->
            "BROADCAST_OPTION", if shadowObscured then SValue.sEmptyString else fieldValue

        | Op.sensing_of_object_menu ->
            let fieldValue =
                match fieldValue with
                | _ when shadowObscured -> SString "_stage_"
                | SString "Stage" -> SString "_stage_"
                | _ -> fieldValue

            fieldName, fieldValue

        | Op.note ->
            fieldName, if shadowObscured then SNumber 60. else fieldValue

        | Op.``music.menu.DRUM`` ->
            fieldName, if shadowObscured then SNumber 1. else fieldValue

        | Op.``music.menu.INSTRUMENT`` ->
            fieldName, if shadowObscured then SNumber 1. else fieldValue

        | Op.``videoSensing.menu.ATTRIBUTE`` ->
            fieldName, if shadowObscured then SString "motion" else fieldValue

        | Op.``videoSensing.menu.SUBJECT`` ->
            fieldName, if shadowObscured then SString "this sprite" else fieldValue

        | Op.``videoSensing.menu.VIDEO_STATE`` ->
            fieldName, if shadowObscured then SString "on" else fieldValue

        | _ ->
            fieldName, if shadowObscured then SValue.sEmptyString else fieldValue

    let emptyBlock = {
        id = Id ""
        opcode = ""
        inputs = OMap.empty
        fields = OMap.empty
        next = None
        topLevel = false
        shadow = false
        children = []

        parent = None
        mutation = None
        x = None
        y = None
    }
    let private addField name value block =
        let field = {
            FieldSkeleton.name = name
            value = SString value
            id = None
            variableType = None
        }
        { block with
            BlockSkeleton.fields =
                OMap.add (Id.create the<_> name) field block.fields
        }
    
    open Scratch.Json.Utf8
    let stringListSyntax = Syntax.jList Syntax.jString |> Syntax.box
    let scalarValueListSyntax = Syntax.jList Serialization.Sb2.Syntax.sValue |> Syntax.box

    let rec complexExpressionAsBlock builder (ComplexExpression(operator = operator; operands = operands) as expression) =
        let blockSpec = blockSpecFromExpression expression
        match blockSpec with
        | ValueNone -> ValueNone
        | ValueSome blockSpec ->

        let block =
            { emptyBlock with
                id = Id.newUniqueId() |> Id.create the<_>
                opcode =  blockSpec.opcode
            }
        let block =
            blockSpec.argMap
            |> Seq.mapi (fun i argMap -> operands.[i], argMap)
            |> Seq.fold (fun block (operand, operandSpec) ->
                let builder = { builder with parentOperandSpec = ValueSome operandSpec }
                match operandSpec with
                | EmptyArg -> block
                | InputArg(inputOperator, inputName, variableType) -> convertInputOperand builder (inputOperator, inputName, variableType) block operand
                | FieldArg(fieldName, variableType) -> convertFieldOperand builder (fieldName, variableType) block operand
            ) block

        let block =
            match operator with
            | O.comeToFront -> block |> addField "FRONT_BACK" "front"
            | O.``goBackByLayers:`` -> block |> addField "FORWARD_BACKWARD" "backward"
            | O.backgroundIndex -> block |> addField "NUMBER_NAME" "number"
            | O.sceneName -> block |> addField "NUMBER_NAME" "name"
            | O.costumeIndex -> block |> addField "NUMBER_NAME" "number"
            | O.costumeName -> block |> addField "NUMBER_NAME" "name"
            | _ -> block

        let block =
            match expression with
            | ComplexExpression(operator = O.stopScripts; operands = Literal(_, SString("other scripts in sprite" | "other scripts in stage"))::_) ->
                let mutation = {
                    Mutation.defaultValue with
                        tagName = Some "mutation"
                        hasnext = Some true
                        children = Some HUnit
                }
                { block with mutation = Some mutation }

            | ComplexExpression(operator = O.call; operands = Literal(_, SString procedureName)::_) ->
                let parameterIds =
                    procedureNameToParameterIds procedureName
                    |> Syntax.serializeString stringListSyntax

                let mutation = {
                    Mutation.defaultValue with
                        tagName = Some "mutation"
                        children = Some HUnit
                        proccode = Some procedureName
                        argumentids = Some parameterIds
                }
                { block with mutation = Some mutation }

            | ComplexExpression(operator = O.getParam; operands = _::Literal(_, returnCode)::_) ->
                let returnCode =
                    match builder.parentOperandSpec with
                    | ValueSome(InputArg(inputOp = "boolean")) -> "b"
                    | _ -> SValue.toString returnCode

                match returnCode with
                | "r" -> { block with opcode = "argument_reporter_string_number" }
                | "b" -> { block with opcode = "argument_reporter_boolean" }
                | _ -> block

            | _ -> block
        ValueSome block

    and blockExpressionAsBlocks builder acc (BlockExpression(body = expressions)) =
        expressions
        |> List.fold (fun acc expression ->
            match complexExpressionAsBlock builder expression with
            | ValueSome block -> BlocksAcc.add acc block
            | _ -> acc
        ) acc

    and convertInputOperand builder (inputOperator, inputName, variableType) block operand =
        let inputId = Id.create the<_> <| Id.newUniqueId()
        let inputName = Id.create the<_> inputName
        let input = {
            name = inputName
            block = None
            shadow = None
        }
        let block = { block with BlockSkeleton.inputs = block.inputs |> OMap.add inputName input }
        let operandBlocks, shadowObscured =
            match operand with
            | Block block -> blockExpressionAsBlocks builder BlocksAcc.empty block |> BlocksAcc.toList, true
            | Complex operand -> complexExpressionAsBlock builder operand |> ValueOption.toList, true
            | Literal _ -> [], false

        let struct(block, input) = appendInputOperandBlocks block input operandBlocks

        match inputOperator with
        | Op.boolean
        | Op.substack -> { block with inputs = OMap.add inputName input block.inputs }
        | _ ->

        let fieldValue =
            match operand with
            | Literal(_, v) -> v
            | _ -> SValue.sEmptyString

        let (Id fieldName) = inputName
        let struct(fieldName, fieldValue) = createShadowField fieldName fieldValue shadowObscured inputOperator
        let field =
            match inputOperator with
            | OpCodes.event_broadcast_menu ->
                let id =
                    fieldValue
                    |> SValue.toString
                    |> Builder.createBroadcastId builder
                    |> Id.toString

                let fieldValue =
                    match fieldValue with
                    | SString "" -> builder.broadcastNameForEmptyBroadcastName
                    | _ -> fieldValue

                {
                    name = fieldName
                    value = fieldValue
                    id = Some id
                    variableType = variableType
                }

            | _ ->
                {
                    name = fieldName
                    value = fieldValue
                    id = None
                    variableType = None
                }

        let block = {
            block with
                children = block.children @ [{
                    emptyBlock with
                        id = inputId
                        opcode = inputOperator
                        fields = OMap.add (Id.create the<_> fieldName) field OMap.empty
                        parent = Some block.id
                        shadow = true
                }]
        }
        let input = {
            input with
                shadow = Some inputId
                block =
                    match input.block with
                    | None -> Some inputId
                    | x -> x
        }
        { block with inputs = OMap.add inputName input block.inputs }

    and convertFieldOperand builder (fieldName, variableType) block operand =
        match operand with
        | Complex _
        | Block _ ->
            failwithf "field operand must be literal.\nexpected field name: %A\nexpected variable type: %A\nactual operand: %A"
                fieldName
                variableType
                operand

        | Literal(_, operand) ->

        let field = {
            name = fieldName
            value = operand
            id = None
            variableType = None
        }
        let field = 
            match fieldName with
            | "CURRENTMENU" ->
                let fieldValue =
                    match operand with
                    | SString "day of week" -> SString "DAYOFWEEK"
                    | _ -> SValue.toString(operand).ToUpperInvariant() |> SString

                { field with value = fieldValue }

            | "VARIABLE" -> { field with id = Builder.createVariableId builder (SValue.toString operand) VariableType.Scalar |> Id.toString |> Some }
            | "LIST" -> { field with id = Builder.createVariableId builder (SValue.toString operand) VariableType.List |> Id.toString |> Some }
            | "BROADCAST_OPTION" ->
                let name = SValue.toString operand
                { field with
                    id =
                        name
                        |> Builder.createBroadcastId builder
                        |> Id.toString
                        |> Some

                    value =
                        match operand with
                        | SString "" -> builder.broadcastNameForEmptyBroadcastName
                        | _ -> operand
                }

            | _ -> field

        let field =
            match variableType with
            | Some variableType -> { field with variableType = Some variableType }
            | _ -> field

        { block with fields = block.fields |> OMap.add (Id.create the<_> fieldName) field }

    let procedureHeaderAsBlock (ProcedureDefinition(_, name, parameters, isAtomic, _)) =
        let block =
            { emptyBlock with
                opcode = "procedures_definition"
                id = Id.newUniqueId() |> Id.create the<_>
            }

        let inputId = Id.newUniqueId() |> Id.create the<_>
        let inputName = Id.create the<_> "custom_block"
        let input = {
            name = inputName
            block = Some inputId
            shadow = Some inputId
        }
        let block = { block with inputs = OMap.add inputName input block.inputs }

        let argumentNames = [for ParameterDefinition(name = name) in parameters -> name] |> Syntax.serializeString stringListSyntax
        let argumentIds = procedureNameToParameterIds name |> Syntax.serializeString stringListSyntax
        let argumentValues = [for ParameterDefinition(defaultValue = value) in parameters -> value] |> Syntax.serializeString scalarValueListSyntax
        let prototype = {
            emptyBlock with
                id = inputId
                opcode = "procedures_prototype"
                shadow = true
                mutation = Some {
                    Mutation.defaultValue with
                        tagName = Some "mutation"
                        proccode = Some name
                        argumentnames = Some argumentNames
                        argumentids = Some argumentIds
                        argumentdefaults = Some argumentValues
                        warp = Some <| match isAtomic with Atomic -> true | NoAtomic -> false
                        children = Some HUnit
                }
        }
        { block with children = [prototype] }

    let procedureAsBlocks builder (ProcedureDefinition(body = body) as procedure) =
        let block = procedureHeaderAsBlock procedure
        let acc = BlocksAcc.empty
        let acc = BlocksAcc.add acc block
        let acc = blockExpressionAsBlocks builder acc body
        BlocksAcc.toList acc

    let listenerAsBlocks builder (ListenerDefinition(state, name, arguments, BlockExpression(bodyState, body))) =
        BlockExpression(bodyState, ComplexExpression(state, name, arguments)::body)
        |> blockExpressionAsBlocks builder BlocksAcc.empty
        |> BlocksAcc.toList

    let scriptAsBlocks builder = function
        | Expression e -> complexExpressionAsBlock builder e |> ValueOption.toList
        | Statements body -> blockExpressionAsBlocks builder BlocksAcc.empty body |> BlocksAcc.toList
        | Procedure p -> procedureAsBlocks builder p
        | Listener l -> listenerAsBlocks builder l

    let rec flatten blocks = seq {
        for block in blocks do
            yield { block with BlockSkeleton.children = [] }
            yield! flatten block.children
    }

    let workspaceScaleX = 1.5
    let workspaceScaleY = 2.2
    let scriptDataAsBlocks blockBuilder data =
        let { script = script; x = scriptX; y = scriptY } = data

        let blocks =
            match scriptAsBlocks blockBuilder script with
            | [] -> []
            | block0::blocks ->

            let block0 =
                { block0 with
                    topLevel = true
                    x = Some <| scriptX * workspaceScaleX
                    y = Some <| scriptY * workspaceScaleY
                    parent = None
                }

            block0::blocks

        flatten blocks

    let replaceOpCodeByTargetKind isStage blocks =
        blocks
        |> OMap.map (fun _ v ->
            if isStage && v.opcode = "event_whenthisspriteclicked" then
                { v with opcode = "event_whenstageclicked" }
            elif not isStage && v.opcode = "event_whenstageclicked" then
                { v with opcode = "event_whenthisspriteclicked" }
            else
                v
        )

    let blockPosition block =
        Position(
            block.x |> Option.defaultValue 0. |> round,
            block.y |> Option.defaultValue 0. |> round
        )

    let private trySimplifyPrimitiveBlock block =
        let findField block fieldName = block.fields.[Id.create the<_> fieldName]
        let findFieldValue block fieldName = (findField block fieldName).value
        let fieldNameAndId field =
            field.value |> SValue.toString,
            field.id |> Option.get |> Id.create the<_>

        let blockPosition block =
            if not block.topLevel then None else
            Some <| blockPosition block

        match block.opcode with
        | Op.math_number -> findFieldValue block "NUM" |> MathNumber |> ValueSome
        | Op.math_positive_number -> findFieldValue block "NUM" |> MathPositiveNumber |> ValueSome
        | Op.math_whole_number -> findFieldValue block "NUM" |> MathWholeNumber |> ValueSome
        | Op.math_integer -> findFieldValue block "NUM" |> MathInteger |> ValueSome
        | Op.math_angle -> findFieldValue block "NUM" |> MathAngle |> ValueSome
        | Op.colour_picker -> findFieldValue block "COLOUR" |> ColourPicker |> ValueSome
        | Op.text -> findFieldValue block "TEXT" |> Text |> ValueSome
        | Op.event_broadcast_menu ->
            let field = findField block "BROADCAST_OPTION"
            let name, id = fieldNameAndId field
            EventBroadcastMenu(name, id) |> ValueSome

        | Op.data_variable ->
            let field = findField block "VARIABLE"
            let name, id = fieldNameAndId field
            DataVariable(name, id, blockPosition block) |> ValueSome

        | Op.data_listcontents ->
            let field = findField block "LIST"
            let name, id = fieldNameAndId field
            DataListContents(name, id, blockPosition block) |> ValueSome

        | _ -> ValueNone

    let simplifyInputs inputs =
        let blockReference = function
            | Some id -> BlockReference id
            | _ -> EmptyBlock

        inputs
        |> OMap.map (fun _ input ->
            match input.block, input.shadow with
            | block, shadow when block = shadow -> blockReference block |> SameBlockShadow
            | block, None -> blockReference block |> BlockNoShadow
            | block, shadow -> DiffBlockShadow(blockReference block, blockReference shadow)
        )

    let simplifyFields fields =
        fields
        |> OMap.map (fun _ field -> {
            value = field.value
            name = field.id |> Option.map Some
        })

    let simplifyBlock block =
        match trySimplifyPrimitiveBlock block with
        | ValueSome b -> Simple b
        | _ ->

        let x, y =
            if not block.topLevel then None, None else

            let (Position(x, y)) = blockPosition block
            Some x, Some y

        CompressedBlock.Complex {
            opcode = Some block.opcode
            next = block.next
            parent = block.parent |> Option.map Some
            inputs = simplifyInputs block.inputs
            fields = simplifyFields block.fields
            shadow = block.shadow
            topLevel = block.topLevel
            x = x
            y = y
            comment = None // block.comment
            mutation = block.mutation
        }

    let simplifyBlocks blocks =
        blocks
        |> OMap.map (fun _ -> simplifyBlock)

    let rec compressInput blocks = function
        | BlockReference blockId ->
            match OMap.tryFind blockId !blocks with
            | ValueSome(Simple block) ->
                blocks := OMap.remove blockId !blocks
                ValueSome block

            | _ -> ValueNone

        | SameBlockShadow b -> compressInput blocks b |> VOption.map SameBlockShadow
        | BlockNoShadow b -> compressInput blocks b |> VOption.map BlockNoShadow
        | DiffBlockShadow(b1, b2) ->
            match compressInput blocks b1, compressInput blocks b2 with
            | ValueNone, ValueNone -> ValueNone
            | b1', b2' ->
                let b1 = VOption.defaultValue b1 b1'
                let b2 = VOption.defaultValue b2 b2'
                DiffBlockShadow(b1, b2) |> ValueSome

        | EmptyBlock

        | MathNumber _
        | MathPositiveNumber _
        | MathWholeNumber _
        | MathInteger _
        | MathAngle _
        | ColourPicker _
        | Text _

        | EventBroadcastMenu _
        | DataVariable _
        | DataListContents _
            -> ValueNone

    let compressInputsInBlock blocks ({ ComplexBlock.inputs = inputs } as block) =
        let blocks = ref blocks
        let inputs = 
            inputs
            |> OMap.map (fun _ input ->
                match compressInput blocks input with
                | ValueSome input -> input
                | _ -> input
            )

        let block = { block with inputs = inputs }
        struct(!blocks, block)

    let compressInputsInBlocks blocks =
        blocks
        |> OMap.foldOrdered (fun blocks id b ->
            if not <| OMap.containsKey id blocks then blocks else

            match b with
            | CompressedBlock.Complex b ->
                let struct(blocks, b) = compressInputsInBlock blocks b
                OMap.add id (CompressedBlock.Complex b) blocks

            | Simple _ as b -> OMap.add id b blocks
        ) blocks

    let scriptDataListAsBlocks builder scripts =
        let blocks =
            scripts
            |> List.fold (fun map script ->
                script
                |> scriptDataAsBlocks builder
                |> Seq.fold (fun map block ->
                    OMap.add block.id block map
                ) map
            ) OMap.empty

        let blocks = replaceOpCodeByTargetKind builder.currentTargetIsStage blocks
        let blocks = simplifyBlocks blocks
        let blocks = compressInputsInBlocks blocks
        for b in blocks do
            match b.Value with
            | CompressedBlock.Complex _
            | Simple(DataVariable _ | DataListContents _) -> ()
            | Simple _ -> failwithf "unexpected top level block: %A" b
        blocks

    let inline private mapStageOrNone f = function
        | Choice1Of2 x -> f x
        | _ -> None

    let inline private mapSpriteOrNone f = function
        | Choice2Of2 x -> f x |> Some
        | _ -> None

    let private md5ExtDelimiter = [|'.'|]
    let convertCostume isStage costume =
        let bitmapResolution = costume.bitmapResolution |> Option.defaultValue 1.

        let md5ext = costume.baseLayerMD5
        let md5ext, md5, ext =
            match md5ext.Split(md5ExtDelimiter, count = 2) with
            | [|md5; ext|] -> md5ext, md5, ext
            | _ ->
                let ext = "png"
                md5ext + ext, md5ext, "png"

        {
            name = costume.costumeName
            bitmapResolution = bitmapResolution
            rotationCenterX = if isStage then 240. * bitmapResolution else costume.rotationCenterX
            rotationCenterY = if isStage then 180. * bitmapResolution else costume.rotationCenterY
            md5ext = md5ext
            // skinId = null
            dataFormat = ext.ToLowerInvariant()
            assetId = md5
            //textLayerMD5 =
            //    match costume.textLayerMD5 with
            //    | Some textLayerMD5 -> Some textLayerMD5.Split('.', count = 2).[0]
            //    | _ -> None
        }
        //let assetFileName = sprintf "%f.%s" costume.baseLayerID ext
        //let textLayerFileName =
        //    match costume.textLayerID with
        //    | Some textLayerID -> sprintf "%f.png" textLayerID |> Some
        //    | _ -> None

    let convertSound sound =
        let md5ext = sound.md5
        let md5, ext =
            let parts = md5ext.Split(md5ExtDelimiter, count = 2)
            parts.[0], parts.[1].ToLowerInvariant()
        {
            name = sound.soundName

            format =
                sound.format
                |> Option.defaultWith (fun _ -> failwith "format not found")
                |> function
                    | EmptyFormat -> ""
                    | Adpcm -> "adpcm"

            rate =
                sound.rate
                |> Option.defaultWith (fun _ -> failwith "rate not found")
                |> function
                    | R11025 -> 11025.
                    | R22050 -> 22050.
                    | R48000 -> 48000.

            sampleCount =
                sound.sampleCount
                |> Option.defaultWith (fun _ -> failwith "sampleCount not found")

            md5ext = md5ext
            //data = None
            dataFormat = ext
            assetId = md5
        }

    let defaultExtensionIds = Set [
        "argument"
        "control"
        "data"
        "event"
        "looks"
        "math"
        "motion"
        "operator"
        "procedures"
        "sensing"
        "sound"
    ] 
    let collectStageExtensionIds stage =
        let rec ofComplexExpression ids (ComplexExpression(operator = op; operands = ops)) =
            match blockSpecFromOperator op with
            | ValueNone -> ids
            | ValueSome spec ->

            let ids =
                let id = spec.category
                if id = "" || Set.contains id defaultExtensionIds then ids else
    
                OMap.add id () ids

            ofExpressions ids ops

        and ofExpression ids = function
            | Literal _ -> ids
            | Complex x -> ofComplexExpression ids x
            | Block x -> ofBlock ids x

        and ofExpressions ids es = List.fold ofExpression ids es
        and ofBlock ids (BlockExpression(body = es)) = List.fold ofComplexExpression ids es

        let ofScript ids = function
            | Listener(ListenerDefinition(arguments = es; body = b)) -> ofBlock (ofExpressions ids es) b
            | Expression e -> ofComplexExpression ids e
            | Procedure(ProcedureDefinition(body = b))
            | Statements b -> ofBlock ids b

        stage.scripts
        |> List.fold (fun ids s ->
            s.script
            |> ofScript ids
        ) OMap.empty

    let fleshKey makeKey xs =
        let rec aux i =
            let newId = makeKey i
            if Set.contains newId xs
            then aux (i + 1)
            else newId
        aux 1

    let broadcastNameForEmptyName broadcastIdForEmptyName broadcasts =
        if not <| OMap.containsKey broadcastIdForEmptyName broadcasts then "UNUSED_MESSAGE" else

        broadcasts
        |> OMap.toSeqOrdered
        |> Seq.map (fun kv ->
            let (BroadcastData name) = kv.Value
            name
        )
        |> Set
        |> fleshKey (sprintf "message%d")

    // OMap [k2, "2"; kb, "b"; k1, "1"; ka, "a"] ⇒
    // OMap [k1, "1", k2; "2"; kb, "b"; ka, "a"]
    let reorderBroadcastsByEsPropertyKey map =
        let (|CanonicalNumericIndexString|) = function
            | "-0" -> ValueSome -0.
            | x ->
                let n = SValue.stringToNumber x
                if SValue.numberToString n = x then ValueSome n else ValueNone

        let isNegativeZero n = n = 0. && 1. / n < 0.

        let toArrayIndex = function
            | CanonicalNumericIndexString(ValueSome n)
                when not (isNegativeZero n) && double (uint32 n) = n ->
                    ValueSome n

            | _ -> ValueNone

        OMap.toSeqOrdered map
        |> Seq.indexed
        |> Seq.sortBy (fun (i, KeyValue(_, BroadcastData name)) ->
            match toArrayIndex name with

            // 配列インデックスでソート
            | ValueSome i -> Choice1Of2 i

            // map の順番 ( 追加順 ) でソート
            | _ -> Choice2Of2 i
        )
        |> Seq.map snd
        |> OMap.fromSeq

    let collectAllBroadcastsAndEmptyId entity entityExtension =

        // "broadcastMsgId-<uniqueId>"
        let broadcastIdForEmptyName = Id.createBroadcastIdCore <| Id.newUniqueId()

        // [""; "a"; "A"] ⇒
        // OMap [
        //     Id broadcastIdForEmptyName, BroadcastData ""
        //     Id "broadcastMsgId-a", BroadcastData "a"
        // ]
        let broadcasts =
            match entityExtension with
            | Choice2Of2 _ -> OMap.empty
            | Choice1Of2 { StageDataExtension.children = children } ->
                collectAllBroadcasts broadcastIdForEmptyName entity children

        let broadcasts = reorderBroadcastsByEsPropertyKey broadcasts

        // "message<uniqueNumber>"
        let broadcastNameForEmptyName = broadcastNameForEmptyName broadcastIdForEmptyName broadcasts
        
        // OMap [
        //     Id broadcastIdForEmptyName, BroadcastData ""
        //     Id "broadcastMsgId-a", BroadcastData "a"
        // ] ⇒
        // OMap [
        //     Id broadcastIdForEmptyName, BroadcastData broadcastNameForEmptyName
        //     Id "broadcastMsgId-a", BroadcastData "a"
        // ]
        let broadcasts =
            match OMap.tryFind broadcastIdForEmptyName broadcasts with
            | ValueNone -> broadcasts
            | ValueSome _ ->

                // keys [Id "a"; Id broadcastIdForEmptyName; Id "b"] ⇒
                // keys [Id "a"; Id "b"; Id broadcastIdForEmptyName]
                OMap.remove broadcastIdForEmptyName broadcasts
                |> OMap.add broadcastIdForEmptyName (BroadcastData broadcastNameForEmptyName)

        broadcasts, broadcastIdForEmptyName, broadcastNameForEmptyName

    let entityAsTarget globalVariableNameToId (entity, entityExtension) =
        let targetId = Id.newUniqueId()

        let isStage =
            match entityExtension with
            | Choice1Of2 _ -> true
            | Choice2Of2 _ -> false

        let broadcasts, broadcastIdForEmptyName, broadcastNameForEmptyName =
            collectAllBroadcastsAndEmptyId entity entityExtension

        let builder = {
            currentTargetIsStage = isStage
            currentTargetId = targetId
            parentOperandSpec = ValueNone
            globalVariableNameToId = globalVariableNameToId
            broadcastNameForEmptyBroadcastName = SString broadcastNameForEmptyName
            broadcastIdForEmptyBroadcastName = broadcastIdForEmptyName
        }
        let variables = OMap.fromSeq <| seq {
            for { VariableData.name = name } as v in entity.variables do
                let id = Builder.createVariableId builder name VariableType.Scalar

                let isCloud = match v.isPersistent with Persistent -> true | NoPersistent -> false
                KeyValuePair(id, VariableData(v.name, v.value, isCloud))
        }
        let lists = OMap.fromSeq <| seq {
            for v in entity.lists do
                let id = Builder.createVariableId builder v.listName VariableType.List

                // v.isPersistent はここで捨てられる
                KeyValuePair(id, ListData(v.listName, v.contents', isCloud = false))
        }
        let blocks = scriptDataListAsBlocks builder entity.scripts

        let costumes = [ for costume in entity.costumes do convertCostume isStage costume ]
        let sounds = [ for sound in entity.sounds do convertSound sound ]

        let paneOrder =
            match entityExtension with
            | Choice1Of2 _ -> infinity
            | Choice2Of2 e -> e.indexInLibrary

        let target = {
            isStage = isStage

            // Sb3 ではステージの名前は "Stage" のみ ( stage.objName は捨てられる )
            // TODO:
            // ステージを追跡するモニターの参照を変更する必要がある
            // 具体的には `stage.children.[i].target = stage.objName` なら `stage.children.[i].target` を "Stage" に変更する
            name = if isStage then "Stage" else entity.objName

            variables = variables
            lists = lists
            blocks = blocks
            broadcasts = broadcasts

            comments = OMap.empty

            currentCostume =
                entity.currentCostumeIndex
                |> Option.map (fun x ->

                    // -1..(costumes.Length - 1)
                    min (max 0. (floor x)) (double (List.length costumes - 1))
                )
                |> Option.defaultValue 0.

            costumes = costumes
            sounds = sounds

            volume = Target.defaultStage.volume
            layerOrder = None
            tempo = entityExtension |> mapStageOrNone (fun x -> x.tempoBPM)
            videoTransparency =
                entityExtension
                |> mapStageOrNone (fun x ->
                    x.videoAlpha
                    |> Option.map (fun videoAlpha ->
                        100. - (100. * videoAlpha)
                    )
                )

            videoState =
                entityExtension
                |> mapStageOrNone (fun x ->
                    Collections.Map.tryFind "videoOn" x.info
                    |> Option.map (fun videoOn ->
                        if videoOn = SValue.sTrue
                        then VideoState.On
                        else VideoState.Off
                    )
                    |> Option.defaultValue VideoState.On
                    |> Some
                )

            textToSpeechLanguage = Some None
            visible =
                entityExtension
                |> mapSpriteOrNone (fun x ->
                    match x.visible with
                    | Visible -> true
                    | Hidden -> false
                )

            x = entityExtension |> mapSpriteOrNone (fun x -> x.scratchX)
            y = entityExtension |> mapSpriteOrNone (fun x -> x.scratchY)
            size = entityExtension |> mapSpriteOrNone (fun x -> x.scale * 100.)
            direction = entityExtension |> mapSpriteOrNone (fun x -> x.direction)
            draggable = entityExtension |> mapSpriteOrNone (fun x -> x.isDraggable)

            rotationStyle =
                entityExtension
                |> mapSpriteOrNone (fun x ->
                    match x.rotationStyle with
                    | RotationStyle.None -> DontRotate
                    | RotationStyle.LeftRight -> LeftRight
                    | RotationStyle.Normal -> AllAround
                )
        }
        {| paneOrder = paneOrder; target = target |}

    let stageAsTargets globalVariableNameToId stage = [
        entityAsTarget globalVariableNameToId (stage, Choice1Of2 stage.ObjectDataExtension)
        for children in stage.ObjectDataExtension.children do
            match children with
            | Choice2Of3 sprite -> entityAsTarget globalVariableNameToId (sprite, Choice2Of2 sprite.ObjectDataExtension)
            | _ -> ()
    ]
    let ofStage stage = {
        targets =
            let globalVariableNameToId = ref Map.empty
            let targets = stageAsTargets globalVariableNameToId stage

            targets
            |> Seq.sortWith (fun l r -> int (l.paneOrder - r.paneOrder))
            |> Seq.map (fun x -> x.target)
            |> Seq.toList

        extensions = collectStageExtensionIds stage |> OMap.toSeqOrdered |> Seq.map (fun kv -> kv.Key) |> Seq.toList
        monitors = []
        meta = Meta.defaultValue
    }
