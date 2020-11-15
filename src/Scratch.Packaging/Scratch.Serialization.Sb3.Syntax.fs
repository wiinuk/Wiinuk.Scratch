module Scratch.Serialization.Sb3.Syntax
open System.Collections.Generic
open Scratch
open Scratch.Primitives
open Scratch.Json
open Scratch.Json.PartialIsomorphisms
open Scratch.Json.PartialIsomorphisms.Iso
open Scratch.Json.PartialIsomorphisms.Reflected.Iso
open Scratch.Serialization
open Scratch.Serialization.Sb3.Ast
open Scratch.Json.Utf8
open Scratch.Json.Utf8.Syntax


let inline syntaxTypeIs (_: 'T The) (x: #ISyntax<'T>) = x

let jSValue = Sb2.Syntax.sValue

/// `[..., boolean?]`
let jOptionalBoolItem = (jBoolean, false) **! jEmptyArray

let variableData = isoFromUnion3 <@ VariableData @>
let jVariableData = jArray (jString ** jSValue ** jOptionalBoolItem) |>> variableData

let listData = isoFromUnion2 <@ ListData @>
let jListData = jArray (jString ** jImmutableArray jSValue ** jEmptyArray) |>> listData

let broadcastData = {
    forward = BroadcastData >> Ok
    reverse = fun (BroadcastData x) -> Ok x
}
let jBroadcastData = jString |>> broadcastData

let jId<'p> = jString |>> makeId |> syntaxTypeIs the<'p Id>
let jOMapWith keyMapping value =
    jKeyValues value
    |>> {
        forward = fun kvs -> result {
            let! kvs =
                kvs
                |> IArray.toSeqCopiable
                |> Result.mapSeq (fun kv -> result {
                    let! k = keyMapping.forward kv.Key
                    return KeyValuePair(k, kv.Value)
                })
            return OMap.fromSeq kvs
        }
        reverse = fun map -> result {
            let! kvs =
                OMap.toSeqOrdered map
                |> Result.mapSeq (fun kv -> result {
                    let! k = keyMapping.reverse kv.Key 
                    return KeyValuePair(k, kv.Value)
                })
            return IArray.ofSeq kvs
        }
    }

let jSimpleBlock =
    let svalue = Iso.box {
        forward = fun (Json(token = t)) -> result {
            match t with
            | JTrue -> return SBool true
            | JFalse -> return SBool false
            | JNumber x -> return SNumber x
            | JString x -> return SString x
            | _ -> return! Iso.error "boolean | number | string"
        }
        reverse = fun x -> result {
            match x with
            | SBool true -> return Json.jtrue
            | SBool false -> return Json.jfalse
            | SNumber x -> return Json.jnumber x
            | SString x -> return Json.jstring x
        }
    }
    let inline svalue1 f x1 = result { let! x1 = forward svalue x1 in return f x1 }
    let rec forward (Json(_, t)) = result {
        match t with
        | JNull -> return EmptyBlock
        | JString id -> return BlockReference(Id id)
        | JArray(Json(_, JNumber k)::xs) when double (int k) = k ->
            match int k, xs with
            | 1, [x1] -> let! x1 = forward x1 in return SameBlockShadow x1
            | 2, [x1] -> let! x1 = forward x1 in return BlockNoShadow x1
            | 3, [x1; x2] ->
                let! x1 = forward x1
                let! x2 = forward x2
                return DiffBlockShadow(x1, x2)

            | 4, [x1] -> return! svalue1 MathNumber x1
            | 5, [x1] -> return! svalue1 MathPositiveNumber x1
            | 6, [x1] -> return! svalue1 MathWholeNumber x1
            | 7, [x1] -> return! svalue1 MathInteger x1
            | 8, [x1] -> return! svalue1 MathAngle x1
            | 9, [Json(_, JString x1)] -> return ColourPicker x1
            | 10, [x1] -> return! svalue1 Text x1

            | 11, [Json(_, JString name); Json(_, JString id)] -> return EventBroadcastMenu(name, Id id)
            | 12, [Json(_, JString name); Json(_, JString id)] -> return DataVariable(name, Id id, None)
            | 12, [Json(_, JString name); Json(_, JString id); Json(_, JNumber x); Json(_, JNumber y)] ->
                return DataVariable(name, Id id, Some(Position(x, y)))
            | 13, [Json(_, JString name); Json(_, JString id)] ->
                return DataListContents(name, Id id, None)
            | 13, [Json(_, JString name); Json(_, JString id); Json(_, JNumber x); Json(_, JNumber y)] ->
                return DataListContents(name, Id id, Some(Position(x, y)))

            | _ -> return! Iso.error "jSimpleBlock"
        | _ -> return! Iso.error "jSimpleBlock"
    }
    let make n xs = Json(None, JArray(Json(None, JNumber(double n))::xs))
    let makeFromTokens n xs = make n <| List.map (fun t -> Json(None, t)) xs
    let position = function None -> [] | Some(Position(x, y)) -> [JNumber x; JNumber y]
    let svalue1 n v = result { let! v = reverse svalue v in return make n [v] }
    let rec reverse b = result {
        match b with
        | EmptyBlock -> return Json(None, JNull)
        | BlockReference(Id id) -> return Json(None, JString id)

        | SameBlockShadow block -> let! block = reverse block in return make 1 [block]
        | BlockNoShadow block -> let! block = reverse block in return make 2 [block]
        | DiffBlockShadow(block, shadow) ->
            let! block = reverse block
            let! shadow = reverse shadow
            return make 3 [block; shadow]

        | MathNumber v -> return! svalue1 4 v
        | MathPositiveNumber v -> return! svalue1 5 v
        | MathWholeNumber v -> return! svalue1 6 v
        | MathInteger v -> return! svalue1 7 v
        | MathAngle v -> return! svalue1 8 v
        | ColourPicker v -> return makeFromTokens 9 [JString v]
        | Text v -> return! svalue1 10 v

        | EventBroadcastMenu(name, Id id) -> return makeFromTokens 11 [JString name; JString id]
        | DataVariable(name, Id id, p) -> return makeFromTokens 12 (JString name::JString id::position p)
        | DataListContents(name, Id id, p) -> return makeFromTokens 13 (JString name::JString id::position p)
    }
    json
    |>> { forward = forward; reverse = reverse }

let jInput = jSimpleBlock

let field = isoFromRecord <@ fun r -> r.value^^r.name^^HUnit @>
let jField = jArray (jSValue ** jString **? jEmptyArray) |>> field

let mutation = isoFromRecord <@ fun r -> r.tagName^^r.children^^r.proccode^^r.argumentids^^r.argumentdefaults^^r.argumentnames^^r.warp^^r.hasnext^^HUnit @>
let jMutation =
    jObject (
        ("tagName", jString) @@?
        ("children", jArray jEmptyArray) @@?
        ("proccode", jString) @@?
        ("argumentids", jString) @@?
        ("argumentdefaults", jString) @@?
        ("argumentnames", jString) @@?
        ("warp", jBoolean) @@?
        ("hasnext", jBoolean) @@?
        jEmptyObject
    )
    |>> mutation

let complexBlock = isoFromRecord <@ fun (r: _ ComplexBlock) -> r.opcode^^r.next^^r.parent^^r.inputs^^r.fields^^r.shadow^^r.topLevel^^r.x^^r.y^^r.comment^^r.mutation^^HUnit @>
let jComplexBlock =
    jObject (
        ("opcode", jNullable jString) @@
        ("next", jNullable jId) @@
        ("parent", jNullable jId) @@?
        ("inputs", jOMapWith makeId jInput) @@
        ("fields", jOMapWith makeId jField) @@
        ("shadow", jBoolean) @@
        ("topLevel", jBoolean) @@
        ("x", jNumber) @@?
        ("y", jNumber) @@?
        ("comment", jId) @@?
        ("mutation", jMutation) @@?
        jEmptyObject
    )
    |>> complexBlock

let complex = isoFromUnion1 <@ Complex @>
let simple = isoFromUnion1 <@ Simple @>
let hlistSingleton = {
    forward = fun x -> HList.singleton x |> Ok
    reverse = fun x -> Ok x.head
}
let jBlock =
    (jComplexBlock |>> hlistSingleton |>> complex) <|>
    (jSimpleBlock |>> hlistSingleton |>> simple)

let comment = isoFromRecord <@ fun r -> r.blockId^^r.x^^r.y^^r.width^^r.height^^r.minimized^^r.text^^HUnit @>
let jComment =
    jObject (
        ("blockId", jNullable jId) @@
        ("x", jNullable jNumber) @@
        ("y", jNullable jNumber) @@
        ("width", jNumber) @@
        ("height", jNumber) @@
        ("minimized", jBoolean) @@
        ("text", jString) @@
        jEmptyObject
    )
    |>> comment

let costume = isoFromRecord <@ fun (r: Costume) -> r.assetId^^r.name^^r.bitmapResolution^^r.md5ext^^r.dataFormat^^r.rotationCenterX^^r.rotationCenterY^^HUnit @>
let jCostume =
    jObject (
        ("assetId", jString) @@
        ("name", jString) @@
        ("bitmapResolution", jNumber) @@?
        ("md5ext", jString) @@?
        ("dataFormat", jString) @@
        ("rotationCenterX", jNumber) @@?
        ("rotationCenterY", jNumber) @@?
        jEmptyObject
    )
    |>> costume

let rotationStyle = {
    forward = fun x -> result {
        match x with
        | "all around" -> return AllAround
        | "left-right" -> return LeftRight
        | "don't rotate" -> return DontRotate
        | _ -> return! Iso.error "\"rotationStyle\" | \"left-right\" | \"don't rotate\""
    }
    reverse = fun x -> result {
        match x with
        | AllAround -> return "all around"
        | LeftRight -> return "left-right"
        | DontRotate -> return "don't rotate"
    }
}
let jRotationStyle = jString |>> rotationStyle

let videoState = {
    forward = fun x -> result {
        match x with
        | "off" -> return VideoState.Off
        | "on" -> return VideoState.On
        | "on-flipped" -> return VideoState.OnFlipped
        | _ -> return! Iso.error "\"off\" | \"on\" | \"on-flipped\""
    }
    reverse = fun x -> result {
        match x with
        | VideoState.Off -> return "off"
        | VideoState.On -> return "on"
        | VideoState.OnFlipped -> return "on-flipped"
    }
}

let jVideoState = jString |>> videoState

let sound = isoFromRecord <@ fun r -> r.assetId^^r.name^^r.dataFormat^^r.format^^r.rate^^r.sampleCount^^r.md5ext^^HUnit @>
let jSound =
    jObject (
        ("assetId", jString) @@
        ("name", jString) @@
        ("dataFormat", jString) @@
        ("format", jString) @@?
        ("rate", jNumber) @@?
        ("sampleCount", jNumber) @@?
        ("md5ext", jString) @@?
        jEmptyObject
    )
    |>> sound

let target =
    isoFromRecord <@ fun r ->
        r.isStage^^
        r.name^^
        r.variables^^
        r.lists^^
        r.broadcasts^^
        r.blocks^^
        r.comments^^
        r.currentCostume^^
        r.costumes^^
        r.sounds^^

        r.volume^^
        r.layerOrder^^
        r.tempo^^
        r.videoTransparency^^
        r.videoState^^
        r.textToSpeechLanguage^^
        r.visible^^
        r.x^^
        r.y^^
        r.size^^
        r.direction^^
        r.draggable^^
        r.rotationStyle^^
        HUnit
    @>

let jTarget =
    jObject (
        ("isStage", jBoolean) @@
        ("name", jString) @@
        ("variables", jOMapWith makeId jVariableData) @@
        ("lists", jOMapWith makeId jListData) @@
        ("broadcasts", jOMapWith makeId jBroadcastData) @@
        ("blocks", jOMapWith makeId jBlock) @@
        ("comments", jOMapWith makeId jComment) @@
        ("currentCostume", jNumber) @@
        ("costumes", jList jCostume) @@
        ("sounds", jList jSound) @@
        
        ("volume", jNumber) @@?
        ("layerOrder", jNumber) @@?
        ("tempo", jNumber) @@?
        ("videoTransparency", jNumber) @@?
        ("videoState", jVideoState) @@?
        ("textToSpeechLanguage", jNullable jString) @@?
        ("visible", jBoolean) @@?
        ("x", jNumber) @@?
        ("y", jNumber) @@?
        ("size", jNumber) @@?
        ("direction", jNumber) @@?
        ("draggable", jBoolean) @@?
        ("rotationStyle", jRotationStyle) @@?
        jEmptyObject
    )
    |>> target

let jMonitorMode = jString

let monitor = isoFromRecord <@ fun r ->
    r.id^^
    r.mode^^
    r.opcode^^
    r.``params``^^
    r.spriteName^^
    r.value^^
    r.width^^
    r.height^^
    r.x^^
    r.y^^
    r.visible^^
    r.min^^
    r.max^^
    HUnit
@>

let jMonitor =
    jObject (
        ("id", jString) @@
        ("mode", jMonitorMode) @@
        ("opcode", jString) @@
        ("params", jOMap jSValue) @@
        ("spriteName", jNullable jString) @@
        ("value", jSValue) @@
        ("width", jNumber) @@?
        ("height", jNumber) @@?
        ("x", jNumber) @@
        ("y", jNumber) @@
        ("visible", jBoolean) @@
        ("min", jNumber) @@?
        ("max", jNumber) @@?
        jEmptyObject
    )
    |>> monitor

let meta = isoFromRecord <@ fun r -> r.semver^^r.vm^^r.agent^^HUnit @>

let jMeta =
    jObject (
        ("semver", jString) @@
        ("vm", jString) @@
        ("agent", jString) @@
        jEmptyObject
    )
    |>> meta

let project = isoFromRecord <@ fun r -> r.targets^^r.monitors^^r.extensions^^r.meta^^HUnit @>

let jProject =
    jObject (
        ("targets", jList jTarget) @@
        ("monitors", jList jMonitor) @@
        ("extensions", jList jString) @@
        ("meta", jMeta) @@
        jEmptyObject
    )
    |>> project
    :> _ ISyntax

let parseProjectWith _name source =
    Syntax.deserialize jProject source

let parseProject source = parseProjectWith "" source

let printProject data =
    Syntax.serialize jProject data
