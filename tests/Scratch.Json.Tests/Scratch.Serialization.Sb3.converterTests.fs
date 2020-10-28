﻿namespace Scratch.Serialization.Sb3.Converter.Test
open Scratch
open Scratch.Serialization.Sb3
open Scratch.Serialization.Sb3.Ast
open Scratch.Primitives
open System.Collections.Generic
open Xunit

module ComplexBlock =
    let defaultValue = {
        opcode = None
        next = None
        parent = None
        inputs = OMap.empty
        fields = OMap.empty
        shadow = false
        topLevel = false
        x = None
        y = None
        comment = None
        mutation = None
    }
type RenameState = {
    oldIdToNewId: Dictionary<string, string>
    mutable nextVariableIndex: int
    mutable nextListIndex: int
    mutable nextBroadcastIndex: int
    mutable nextCommentIndex: int
    mutable nextBlockIndex: int
}

[<AutoOpen>]
module Helpers =
    open Scratch.Serialization.Sb3.OpCodeSpecs

    let sb3OpcodeToBlockSpec =
        sb2ExpressionSpecs
        |> Seq.map (fun kv ->
            let v = kv.Value
            let inputs, fields =
                v.argMap
                |> List.fold (fun (inputs, fields) arg ->
                    match arg with
                    | EmptyArg -> inputs, fields
                    | FieldArg(name, variableType) -> inputs, Map.add (Id.create the<FieldPhantom> name) {| variableType = variableType |} fields
                    | InputArg(op, name, variableType) -> Map.add (Id.create the<InputPhantom> name) {| op = op; variableType = variableType |} inputs, fields
                ) (Map.empty, Map.empty)

            v.opcode, {| inputs = inputs; fields = fields |}
        )
        |> Map

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

        let fieldNameToSpec =
            sb3OpcodeToBlockSpec
            |> Map.tryFind (Option.defaultValue "" b.opcode)
            |> VOption.map (fun spec -> spec.fields)
            |> VOption.defaultValue Map.empty

        { b with
            comment = b.comment |> Option.map (renameCommentId state)
            inputs =
                b.inputs
                |> OMap.map (fun name input ->
                    normalizeSimpleBlock state input
                )

            fields =
                b.fields
                |> OMap.map (fun name field ->
                    let variableType = Map.tryFind name fieldNameToSpec |> VOption.bind (fun x -> x.variableType |> VOption.unbox)
                    match variableType with
                    | ValueSome t ->
                        let renameId =
                            let renameIdInField rename state x = x |> Option.map (Option.map (Id.create the<_> >> rename state >> Id.toString))
                            match t with
                            | VariableType.BroadcastMessage -> renameIdInField renameBroadcastId
                            | VariableType.List -> renameIdInField renameListId
                            | VariableType.Scalar -> renameIdInField renameVariableId

                        { field with name = field.name |> renameId state }

                    | _ -> field
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

    let qcheck = Scratch.Json.Tests.qcheck


namespace Scratch.Serialization.Sb3.Converter
open Scratch
open Scratch.Ast
open Scratch.Primitives
open Scratch.Serialization.Sb3
open Scratch.Serialization.Sb3.Ast
open Scratch.Serialization.Sb3.Test.Helpers
open Scratch.Serialization.Sb3.Converter.Test
open Scratch.Serialization.Sb3.Converter.Test.Helpers
open System
open Xunit


module Tests =
    [<Fact>]
    let complexExpressionCompressTest() =
        let blockId = Id.create the<_>
        let fieldId = Id.create the<_>

        let e =
            ComplexExpression((), Symbol.``%``, [
                Expression.eNumber () 10.
                Expression.eNumber () 20.
            ])

        let sb3Project =
            { StageData.defaultValue with
                scripts = [{ x = 0.; y = 0.; script = Expression e }]
            }
            |> Project.ofStage
            |> normalizeProject

        sb3Project.targets.[0].blocks =? OMap.ofSeq [
            blockId "1", Complex {
                ComplexBlock.defaultValue with
                    opcode = Some "operator_mod"
                    topLevel = true
                    x = Some 0.
                    y = Some 0.

                    inputs = OMap.ofSeq [
                        fieldId "NUM1", SameBlockShadow(
                            MathNumber(SNumber 10.)
                        )
                        fieldId "NUM2", SameBlockShadow(
                            MathNumber(SNumber 20.)
                        )
                    ]
            }
        ]

type IpcTestFixture() =
    let client = AdaptorJs.startServerAndConnect() |> Async.RunSynchronously
    member _.AdaptorJsClient = client
    interface IDisposable with
        member _.Dispose() = client.Dispose() |> Async.RunSynchronously

type IpcTests(fixture: IpcTestFixture) =
    let client = fixture.AdaptorJsClient

    let sb3NormalizeProperty script =
        let stage = scriptToStage script
        let sb3Project = Project.ofStage stage |> normalizeProject
        let sb3Project' = sb3Project |> AdaptorJs.sb3ToSb3By client |> Async.RunSynchronously |> normalizeProject

        sb3Project.targets.[0].blocks =? sb3Project'.targets.[0].blocks
        sb3Project =? sb3Project'

    let exportScriptToSb3Property script =
        let stage = scriptToStage script

        let sb3ProjectFromFs = Project.ofStage stage |> AdaptorJs.sb3ToSb3By client |> Async.RunSynchronously
        let sb3ProjectFromJs = AdaptorJs.sb2ToSb3By client stage |> Async.RunSynchronously

        let sb3ProjectFromFs = normalizeProject sb3ProjectFromFs
        let sb3ProjectFromJs = normalizeProject sb3ProjectFromJs

        sb3ProjectFromFs.targets.[0].blocks =? sb3ProjectFromJs.targets.[0].blocks
        sb3ProjectFromFs =? sb3ProjectFromJs

    interface IClassFixture<IpcTestFixture>

    [<Fact>]
    member _.normalize() = qcheck sb3NormalizeProperty

    [<Fact>]
    member _.normalizeSimpleExpression() =
        Expressions.hide ()
        |> Expression
        |> sb3NormalizeProperty

    [<Fact>]
    member _.normalizeWhenKeyPressed() =
        ListenerDefinition((), O.whenKeyPressed, [Literal((), SString "0")], BlockExpression((), []))
        |> Listener
        |> sb3NormalizeProperty

    [<Fact>]
    member _.normalizeColorSees() =
        ComplexExpression((), O.``color:sees:``, [
            Literal((), SNumber 10.)
            Literal((), SNumber 20.)
        ])
        |> Expression
        |> sb3NormalizeProperty

    [<Fact>]
    member _.export() = qcheck exportScriptToSb3Property

    [<Fact>]
    member _.exportEmptyBlock() =
        Statements(BlockExpression((), []))
        |> exportScriptToSb3Property

    [<Fact>]
    member _.exportEmptyListener() =
        ListenerDefinition((), O.whenClicked, [], BlockExpression((), []))
        |> Listener
        |> exportScriptToSb3Property

    [<Fact>]
    member _.exportEmptyProcedure() =
        ProcedureDefinition((), "p", [], Atomic, BlockExpression((), []))
        |> Procedure
        |> exportScriptToSb3Property

    [<Fact>]
    member _.exportAbs() =
        ComplexExpression((), O.abs, [Literal((), SNumber 0.)])
        |> Expression
        |> exportScriptToSb3Property

    [<Fact>]
    member _.exportAnswer() =
        ComplexExpression((), O.answer, [])
        |> Expression
        |> exportScriptToSb3Property

    [<Fact>]
    member _.exportWhenIReceive() =
        ListenerDefinition((), O.whenIReceive, [Literal((), SString "")], BlockExpression((), []))
        |> Listener
        |> exportScriptToSb3Property

    [<Fact>]
    member _.exportBroadcast() =
        BlockExpression((), [
            ComplexExpression((), O.``broadcast:``, [Literal((), SString "A")])
        ])
        |> Statements
        |> exportScriptToSb3Property

    [<Fact>]
    member _.exportEmptyBroadcast() =
        BlockExpression((), [
            ComplexExpression((), O.``broadcast:``, [Literal((), SString "")])
        ])
        |> Statements
        |> exportScriptToSb3Property

    [<Fact>]
    member _.exportExpressionBroadcast() =
        BlockExpression((), [
            ComplexExpression((), O.``broadcast:``, [
                Expression.Complex(ComplexExpression((), O.answer, []))
            ])
        ])
        |> Statements
        |> exportScriptToSb3Property
