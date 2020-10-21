module Scratch.Serialization.Sb3.Tests
open Xunit
open FsCheck
open Utf8Json
open Scratch
open Scratch.Primitives
open Scratch.Json.PartialIsomorphisms
open Scratch.Json.Utf8
open Scratch.Serialization.Sb3.Ast
open Scratch.Serialization.Sb3.Test.Helpers
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

open System.Collections.Generic
type RenameState = {
    oldIdToNewId: Dictionary<string, string>
    mutable nextVariableIndex: int
    mutable nextListIndex: int
    mutable nextBroadcastIndex: int
    mutable nextCommentIndex: int
    mutable nextBlockIndex: int
}

let newRenameState() = {
    oldIdToNewId = Dictionary()
    nextVariableIndex = 1
    nextListIndex = 1
    nextBroadcastIndex = 1
    nextCommentIndex = 1
    nextBlockIndex = 1
}
let renameId { oldIdToNewId = oldIdToNewId } prefix (index: _ byref) (id: 'T Id) =
    let id = Id.toString id

    let newId =
        let mutable newId = null
        if oldIdToNewId.TryGetValue(id, &newId) then newId else

        let newId = sprintf "%s%d" prefix index
        index <- Checked.(+) index 1
        oldIdToNewId.Add(id, newId)
        newId

    Id.create the<'T> newId

let renameVariableId state (id: VariableOrListId) = renameId state "v" &state.nextVariableIndex id
let renameListId state (id: VariableOrListId) = renameId state "l" &state.nextListIndex id
let renameBroadcastId state (id: BroadcastId) = renameId state "b" &state.nextBroadcastIndex id
let renameCommentId state (id: CommentId) = renameId state "c" &state.nextCommentIndex id
let renameBlockId state (id: BlockId) = renameId state "" &state.nextBlockIndex id

let renameOMapKeys renameKey kvs =
    kvs
    |> OMap.foldOrdered (fun map id v ->
        OMap.add (renameKey id) v map
    ) OMap.empty

let normalizeComments state comments =
    comments
    |> OMap.foldOrdered (fun map id comment ->
        let comment =
            { comment with
                blockId = comment.blockId |> Option.map (renameBlockId state)
            }
        OMap.add (renameCommentId state id) comment map

    ) OMap.empty

let rec normalizeSimpleBlock state = function
    | BlockReference id -> renameBlockId state id |> BlockReference

    | SameBlockShadow b -> normalizeSimpleBlock state b |> SameBlockShadow
    | BlockNoShadow b -> normalizeSimpleBlock state b |> BlockNoShadow
    | DiffBlockShadow(b1, b2) ->
        DiffBlockShadow(
            normalizeSimpleBlock state b1,
            normalizeSimpleBlock state b2
        )

    | EventBroadcastMenu(name, id) -> EventBroadcastMenu(name, renameBroadcastId state id)
    | DataVariable(name, id, p) -> DataVariable(name, renameVariableId state id, p)
    | DataListContents(name, id, p) -> DataListContents(name, renameListId state id, p)

    | EmptyBlock
    | MathNumber _
    | MathPositiveNumber _
    | MathWholeNumber _
    | MathInteger _
    | MathAngle _
    | ColourPicker _
    | Text _ as b -> b

let normalizeBlock state = function
    | Simple b -> normalizeSimpleBlock state b |> Simple
    | Complex b ->

    { b with
        comment = b.comment |> Option.map (renameCommentId state)
        inputs =
            b.inputs
            |> OMap.map (fun _ input ->
                normalizeSimpleBlock state input
            )

        next = b.next |> Option.map (renameBlockId state)

        // NOTE: ここで全ての `{ parent: null }` は `{ parent: undefined }` に変更される
        parent = b.parent |> Option.flatten |> Option.map (renameBlockId state >> Some)
    }
    |> Complex

let normalizeBlocks state blocks =
    blocks
    |> OMap.foldOrdered (fun map id block ->
        OMap.add (renameBlockId state id) (normalizeBlock state block) map
    ) OMap.empty

let normalizeTarget x =
    let map = newRenameState()
    { x with
        variables = renameOMapKeys (renameVariableId map) x.variables
        lists = renameOMapKeys (renameListId map) x.lists
        broadcasts = renameOMapKeys (renameBroadcastId map) x.broadcasts
        blocks = normalizeBlocks map x.blocks
        comments = normalizeComments map x.comments
    }

let normalizeMeta x =
    { x with
        vm = ""
        agent = None
    }
let normalizeProject x =
    { x with
        targets = List.map normalizeTarget x.targets
        meta = normalizeMeta x.meta
    }
let scriptToStage (script: unit Ast.Script) = { Ast.StageData.defaultValue with scripts = [{ x = 0.; y = 0.; script = script }] }

let (=?) l r =
    if not <| LanguagePrimitives.GenericEqualityER l r then
        let l, r = sprintf "%0A" l, sprintf "%0A" r
        let d = DiffMatchPatch.DiffMatchPatch.Default
        let es = d.DiffMain(l, r)
        d.DiffCleanupSemantic es
        let diffText =
            es
            |> Seq.map (fun e ->
                match e.Operation with
                | DiffMatchPatch.Delete -> "[- " + e.Text + " -]"
                | DiffMatchPatch.Insert -> "[+ " + e.Text + " +]"
                | DiffMatchPatch.Equal -> e.Text
            )
            |> String.concat ""

        Assert.True(false, sprintf "diff:\n%s\nl:\n%s\nr:\n%s" diffText l r)

let sb3NormalizeProperty script =
    let stage = scriptToStage script
    let sb3Project = Project.ofStage stage |> normalizeProject
    let sb3Project' = sb3Project |> AdaptorJs.sb3ToSb3 |> normalizeProject

    sb3Project.targets.[0].blocks =? sb3Project'.targets.[0].blocks
    sb3Project =? sb3Project'

[<Fact>]
let sb3NormalizePropertyTest() =
    Arb.register<Scratch.Runtime.Test.Helpers.Arbs>() |> ignore
    Json.Tests.qcheck sb3NormalizeProperty

open Scratch.Ast

[<Fact>]
let sb3NormalizeSimpleExpressionTest() =
    Expressions.hide ()
    |> Expression
    |> sb3NormalizeProperty

let exportScriptToSb3Property script =
    let stage = scriptToStage script

    let sb3ProjectFromFs = Project.ofStage stage |> AdaptorJs.sb3ToSb3
    let sb3ProjectFromJs = AdaptorJs.sb2ToSb3 stage

    let sb3ProjectFromFs = normalizeProject sb3ProjectFromFs
    let sb3ProjectFromJs = normalizeProject sb3ProjectFromJs

    sb3ProjectFromFs.targets.[0].blocks =? sb3ProjectFromJs.targets.[0].blocks
    sb3ProjectFromFs =? sb3ProjectFromJs


[<Fact>]
let exportScriptToSb3PropertyTest() =
    Arb.register<Scratch.Runtime.Test.Helpers.Arbs>() |> ignore
    Json.Tests.qcheck exportScriptToSb3Property

[<Fact>]
let exportEmptyBlockToSb3Test() =
    Statements(BlockExpression((), []))
    |> exportScriptToSb3Property

[<Fact>]
let exportEmptyListenerToSb3Test() =
    ListenerDefinition((), O.whenClicked, [], BlockExpression((), []))
    |> Listener
    |> exportScriptToSb3Property

[<Fact>]
let exportEmptyProcedureToSb3Test() =
    ProcedureDefinition((), "p", [], Atomic, BlockExpression((), []))
    |> Procedure
    |> exportScriptToSb3Property

(*
diff:
{
    targets = [
        {
            isStage = true
            name = "Stage"
            variables = OMap []
            lists = OMap []
            broadcasts = OMap []
            blocks = OMap []
            comments = OMap []
            currentCostume = [- 0 -][+ -1 +].0
            costumes = []
            sounds = []
            volume = Some 100.0
            layerOrder = [- Some 0.0 -][+ None +]
            tempo = Some 60.0
            videoTransparency = Some 50.0
            videoState = [- None -][+ Some On +]
            textToSpeechLanguage = Some None
            visible = None
            x = None
            y = None
            size = None
            direction = None
            draggable = None
            rotationStyle = None
        }
    ]
    monitors = []
    extensions = []
    meta = {
        semver = "3.0.0"
        vm = "0.2.0[+ -prerelease.20201016122132 +]"
        agent = Some "none"
    }
}
*)
