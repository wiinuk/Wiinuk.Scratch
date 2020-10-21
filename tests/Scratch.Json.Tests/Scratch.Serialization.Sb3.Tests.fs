module Scratch.Serialization.Sb3.Tests
open Xunit
open Utf8Json
open Scratch
open Scratch.Primitives
open Scratch.Json.PartialIsomorphisms
open Scratch.Json.Utf8
open Scratch.Serialization.Sb3.Ast
open System.Threading

module Sb3 = Scratch.Serialization.Sb3.Syntax
module Map = OMap


let forward x = Iso.forward (Iso.box x)
let parse syntax s =
    try Syntax.deserializeString (Syntax.box syntax) s |> Ok
    with :? JsonParsingException as e -> Error(e.GetUnderlyingStringUnsafe(), e.ActualChar, e.Offset, e)

[<Fact>]
let sb3ParseTest() =
    """{
        "opcode": "procedures_prototype",
        "next": null,
        "inputs": {},
        "fields": {},
        "shadow": true,
        "topLevel": false,
        "mutation": {
            "tagName": "mutation",
            "proccode": "procName",
            "argumentnames": "[]",
            "argumentids": "[]",
            "argumentdefaults": "[]",
            "warp": true,
            "children": []
        }
    }"""
        |> parse Sb3.jBlock
        =? Ok(Complex {
            opcode = Some "procedures_prototype"
            next = None
            parent = None
            inputs = Map.empty
            fields = Map.empty
            shadow = true
            topLevel = false
            x = None
            y = None
            comment = None
            mutation = Some {
                tagName = Some "mutation"
                children = Some HUnit
                proccode = Some "procName"
                argumentids = Some "[]"
                argumentdefaults = Some "[]"
                argumentnames = Some "[]"
                warp = Some true
                hasnext = None
            }
        })

    """{
        "opcode": "procedures_call",
        "next": null,
        "parent": null,
        "inputs": {
            "input0": [3, ".9gsg{9b;gw{LPXb/]Xd", [4, 10]],
            "input1": [3, "p@h=]fBku?a|H}:Q4]~G", [4, 10]]
        },
        "fields": {},
        "shadow": false,
        "topLevel": false,
        "mutation": {
            "tagName": "mutation",
            "children": [],
            "proccode": "procName %n , %n",
            "argumentids": "[\"input0\",\"input1\"]"
        }
    }"""
        |> parse Sb3.jBlock
        =? Ok(Complex {
            opcode = Some "procedures_call"
            next = None
            parent = Some None
            inputs = Map.ofSeq [
                Id "input0", DiffBlockShadow(BlockReference(Id ".9gsg{9b;gw{LPXb/]Xd"), MathNumber(SNumber 10.0))
                Id "input1", DiffBlockShadow(BlockReference(Id "p@h=]fBku?a|H}:Q4]~G"), MathNumber(SNumber 10.0))
            ]
            fields = Map.ofSeq []
            shadow = false
            topLevel = false
            x = None
            y = None
            comment = None
            mutation = Some {
                tagName = Some "mutation"
                children = Some HUnit
                proccode = Some "procName %n , %n"
                argumentids = Some """["input0","input1"]"""
                argumentdefaults = None
                argumentnames = None
                warp = None
                hasnext = None
            }
        })

    """[12, "varName", "Z.4Wj|XTRL+Xet*z}.}c-varName-", 123, 456]"""
        |> parse Sb3.jBlock
        =? Ok(Simple(DataVariable("varName", Id "Z.4Wj|XTRL+Xet*z}.}c-varName-", Some(Position(123.0, 456.0)))))

[<Fact>]
let jSimpleBlockParseTest() =
    """[7, "all"]"""
        |> parse Sb3.jSimpleBlock
        =? Ok(MathInteger(SString "all"))

    """[3, [12, "varName", "varId"], [10, "textValue"]]"""
        |> parse Sb3.jSimpleBlock
        =? Ok(DiffBlockShadow(DataVariable("varName", Id "varId", None), Text(SString "textValue")))

[<Fact>]
let jFieldParseTest() =
    """["X"]"""
        |> parse Sb3.jField
        =? Ok { value = SString "X"; name = None }

[<Fact>]
let jInputParseTest() =
    "[ 1, null ]"
        |> parse Sb3.jInput
        =? Ok(SameBlockShadow EmptyBlock)

module Target =
    let defaultStage = {
        isStage = true
        name = "Stage"
        variables = OMap.empty
        lists = OMap.empty
        broadcasts = OMap.empty
        blocks = OMap.empty
        comments = OMap.empty
        currentCostume = 0.
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

module Meta =
    let defaultValue = {
        semver = "3.0.0"
        vm = "0.2.0-prerelease.20190822194548"
        agent = None
    }

[<Struct; RequireQualifiedAccess>]
type VariableType =
    | Scalar
    | List
    | BroadcastMessage

module VariableType =
    let namingText = function
        | VariableType.Scalar -> ""
        | VariableType.List -> "list"
        | VariableType.BroadcastMessage -> "broadcast_msg"

module Id =
    open System
    open System.Text.RegularExpressions
    open System.Security.Cryptography


    let create (_: 'Phantom The) x: 'Phantom Id = Id x

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

module Project =
    open Scratch.Ast
    open System.Collections.Generic

    let defaultValue = {
        targets = [Target.defaultStage]
        monitors = []
        extensions = []
        meta = Meta.defaultValue
    }

    let idForEmptyBroadcastName = Id.newUniqueId()
    let globalBroadcastId broadcastNameToId name field =
        let name =
            match name with
            | "" -> idForEmptyBroadcastName
            | n -> n.ToLowerInvariant()

        let id = Id.escape name |> sprintf "broadcastMsgId-%s" |> Id.create the<BroadcastPhantom>
        struct(id, Map.add name id broadcastNameToId)

    let collectBroadcasts broadcastNameToId stage =
        //const allBroadcastMsgs = globalBroadcastMsgObj.globalBroadcastMsgs;
        //const allBroadcastMsgFields = globalBroadcastMsgObj.allBroadcastFields;
        //const oldEmptyMsgName = globalBroadcastMsgObj.emptyMsgName;

        if Map.containsKey idForEmptyBroadcastName broadcastNameToId then
            // Find a fresh 'messageN'
            let flesh

            let currIndex = 1;
            while (allBroadcastMsgs[`message${currIndex}`]) {
                currIndex += 1;
            }
            const newEmptyMsgName = `message${currIndex}`;
            // Add the new empty message name to the broadcast message
            // name map, and assign it the old id.
            // Then, delete the old entry in map.
            allBroadcastMsgs[newEmptyMsgName] = allBroadcastMsgs[oldEmptyMsgName];
            delete allBroadcastMsgs[oldEmptyMsgName];
            // Now update all the broadcast message fields with
            // the new empty message name.
            for (let i = 0; i < allBroadcastMsgFields.length; i++) {
                if (allBroadcastMsgFields[i].value === '') {
                    allBroadcastMsgFields[i].value = newEmptyMsgName;
                }
            }
        }
        // Traverse the broadcast message name map and create
        // broadcast messages as variables on the stage (which is this
        // target).
        for (const msgName in allBroadcastMsgs) {
            const msgId = allBroadcastMsgs[msgName];
            const newMsg = new Variable(
                msgId,
                msgName,
                Variable.BROADCAST_MESSAGE_TYPE,
                false
            );
            target.variables[newMsg.id] = newMsg;
        }
    let stageAsTarget globalVariableNameToId stageId stage =
        let variables = OMap.fromSeq <| seq {
            for v in stage.variables do
                let struct(id, globalVariableNameToId') = Id.createVariableId (true, stageId) (!globalVariableNameToId, v.name, VariableType.Scalar)
                globalVariableNameToId := globalVariableNameToId'

                let isCloud = match v.isPersistent with Persistent -> true | NoPersistent -> false
                KeyValuePair(id, VariableData(v.name, v.value, isCloud))
        }
        let lists = OMap.fromSeq <| seq {
            for v in stage.lists do
                let struct(id, globalVariableNameToId') = Id.createVariableId (true, stageId) (!globalVariableNameToId, v.listName, VariableType.List)
                globalVariableNameToId := globalVariableNameToId'

                // v.isPersistent はここで捨てられる
                KeyValuePair(id, ListData(v.listName, v.contents', isCloud = false))
        }
        let broadcasts = collectBroadcasts stage
        {
            isStage = true

            // Sb3 ではステージの名前は "Stage" のみ ( stage.objName は捨てられる )
            // TODO:
            // ステージを追跡するモニターの参照を変更する必要がある
            // 具体的には `stage.children.[i].target = stage.objName` なら `stage.children.[i].target` を "Stage" に変更する
            name = "Stage"

            variables = variables
            lists = lists
            broadcasts = broadcasts
            blocks = blocks

            comments = OMap.empty

            currentCostume = Option.defaultValue 0. stage.currentCostumeIndex
            costumes = costumes
            sounds = sounds

            volume = Target.defaultStage.volume
            layerOrder = None
            tempo = stage.ObjectDataExtension.tempoBPM

            videoTransparency =
                stage.ObjectDataExtension.videoAlpha
                |> Option.map (fun videoAlpha ->
                    100. - (100. * videoAlpha)
                )

            videoState =
                Collections.Map.tryFind "videoOn" stage.ObjectDataExtension.info
                |> Option.map (fun videoOn ->
                    if videoOn = SValue.sTrue
                    then VideoState.On
                    else VideoState.Off
                )

            textToSpeechLanguage = Some None
            visible = None
            x = None
            y = None
            size = None
            direction = None
            draggable = None
            rotationStyle = None
        }

    let ofStage stage = {
        targets = [
            stageAsTarget stage
            for child in stage.ObjectDataExtension.children do
                match child with
                | Choice2Of3 sprite -> entityAsTarget false sprite
                | _ -> ()
        ]
        extensions = collectStageExtensions stage
        monitors = []
        meta = Meta.defaultValue
    }

[<Fact>]
let jBlockRoundtripTest() = Scratch.Json.Tests.qcheck <| fun script ->
    let stage = { Scratch.Ast.StageData.defaultValue with scripts = [{ x = 0.; y = 0.; script = script }] }
    let project = Project.ofStage stage
    let sb3ProjectJson = Syntax.serializeString Sb3.jProject project
    let project' = roundTripSb3Json sb3ProjectJson
    let stage' = Project.toStage project'
    let script' = stage'.scripts.[0].script
    script =? script'
