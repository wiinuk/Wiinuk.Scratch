namespace Scratch.Serialization.Sb3.Converter.Test
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
    open Scratch.Json.Utf8

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

    let normalizeMutation m =
        let roundtrip syntax x =
            try
                x
                |> Syntax.deserializeString syntax
                |> Syntax.serializeString syntax
            with _ ->
                x

        { m with
            argumentids = m.argumentids |> Option.map (roundtrip Project.stringListSyntax)
            argumentdefaults = m.argumentdefaults |> Option.map (roundtrip Project.scalarValueListSyntax)
            argumentnames = m.argumentnames |> Option.map (roundtrip Project.stringListSyntax)
        }

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

            mutation = Option.map normalizeMutation b.mutation
        }
        |> Complex

    let normalizeBlocks state blocks =
        blocks
        |> OMap.foldOrdered (fun map id block ->
            OMap.add (renameBlockId state id) (normalizeBlock state block) map
        ) OMap.empty

    let normalizeTarget x =
        let state = newRenameState()

        for kv in OMap.toSeqOrdered x.blocks do renameBlockId state kv.Key |> ignore
        for kv in OMap.toSeqOrdered x.variables do renameVariableId state kv.Key |> ignore
        for kv in OMap.toSeqOrdered x.lists do renameListId state kv.Key |> ignore
        for kv in OMap.toSeqOrdered x.broadcasts do renameBroadcastId state kv.Key |> ignore
        for kv in OMap.toSeqOrdered x.comments do renameCommentId state kv.Key |> ignore

        { x with
            variables = renameOMapKeys (renameVariableId state) x.variables
            lists = renameOMapKeys (renameListId state) x.lists
            broadcasts = renameOMapKeys (renameBroadcastId state) x.broadcasts
            blocks = normalizeBlocks state x.blocks
            comments = normalizeComments state x.comments
        }

    let normalizeMeta x =
        { x with
            vm = "0.0.0"
            agent = ""
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

    let qcheckWith = Scratch.Json.Tests.qcheckWith
    let qcheck = Scratch.Json.Tests.qcheck

module CostumeData =
    open Scratch.Ast

    let empty = {
        baseLayerMD5 = ""
        baseLayerID = 0.
        textLayerMD5 = None
        textLayerID = None
        bitmapResolution = None
        costumeName = ""
        rotationCenterX = 0.
        rotationCenterY = 0.
    }
    let dummy =
        { empty with
            baseLayerMD5 = "d41d8cd98f00b204e9800998ecf8427e.png"
        }

namespace Scratch.Serialization.Sb3.Converter
open Scratch
open Scratch.Ast
open Scratch.Packaging
open Scratch.Primitives
open Scratch.Serialization.Sb3
open Scratch.Serialization.Sb3.Ast
open Scratch.Serialization.Sb3.Test.Helpers
open Scratch.Serialization.Sb3.Converter.Test
open System
open System.IO
open System.IO.Compression
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

    let assertProjectEq p p' =
        match p.targets, p'.targets with
        | { blocks = bs }::_, { blocks = bs' }::_ -> bs =? bs'
        | _ -> ()
        p =? p'

    let addDummyCostumeIfEmpty stage =
        let addToEntity = function
            | { EntityData.costumes = [] } as x -> { x with costumes = [CostumeData.dummy] }
            | x -> x

        { stage with
            ObjectDataExtension =
                { stage.ObjectDataExtension with
                    StageDataExtension.children =
                        [ for c in stage.ObjectDataExtension.children do
                            match c with
                            | Choice2Of3 sprite -> addToEntity sprite |> Choice2Of3
                            | _ -> c
                        ]
                }
        }
        |> addToEntity

    let sb3NormalizeStageProperty (stage: unit StageData) =
        let stage = addDummyCostumeIfEmpty stage
        let sb3Project = Project.ofStage stage |> AdaptorJs.sb3ToSb3By client |> Async.RunSynchronously |> normalizeProject
        let sb3Project' = sb3Project |> AdaptorJs.sb3ToSb3By client |> Async.RunSynchronously |> normalizeProject

        assertProjectEq sb3Project sb3Project'

    let sb3NormalizeScriptProperty = sb3NormalizeStageProperty << scriptToStage

    let exportStageToSb3Property (stage: unit StageData) =
        let stage = addDummyCostumeIfEmpty stage
        let sb3ProjectFromFs = Project.ofStage stage
        let sb3ProjectFromJs = AdaptorJs.sb2ToSb3By client stage |> Async.RunSynchronously

        assertProjectEq (normalizeProject sb3ProjectFromFs) (normalizeProject sb3ProjectFromJs)

    let exportScriptToSb3Property = exportStageToSb3Property << scriptToStage

    let sb3BytesSaveAndLoadProperty package =
        let packageToSb3Bytes package =
            use stream = new MemoryStream()
            writeSb3PackageToStream stream package |> Async.RunSynchronously
            stream.Flush()
            stream.ToArray()

        let packageToSb2Bytes package =
            use stream = new MemoryStream()
            writeSb2PackageToStream stream package |> Async.RunSynchronously
            stream.Flush()
            stream.ToArray()

        let usingReadZipEntry stream fullName action = async {
            use zip = new ZipArchive(stream, ZipArchiveMode.Read, leaveOpen = true)
            match zip.GetEntry fullName with
            | null -> return raise <| FileNotFoundException("zip entry not found.", fullName)
            | entry ->
                use stream = entry.Open()
                return! action stream
        }
        let sb3BytesToProject bytes = async {
            use stream = new MemoryStream(bytes, 0, bytes.Length, writable = false, publiclyVisible = false)
            return! usingReadZipEntry stream "project.json" <| fun stream ->
                Json.Utf8.Syntax.deserializeStream Syntax.jProject stream
        }

        let projectFromFs =
            package
            |> packageToSb3Bytes
            |> AdaptorJs.packageBinaryToSb3BinaryBy client
            |> Async.RunSynchronously
            |> sb3BytesToProject
            |> Async.RunSynchronously

        let projectFromJs =
            package
            |> packageToSb2Bytes
            |> AdaptorJs.packageBinaryToSb3BinaryBy client
            |> Async.RunSynchronously
            |> sb3BytesToProject
            |> Async.RunSynchronously

        normalizeProject projectFromFs =? normalizeProject projectFromJs

    interface IClassFixture<IpcTestFixture>

    [<Fact>]
    member _.normalizeAnyScript() = qcheck sb3NormalizeScriptProperty

    [<Fact>]
    member _.normalizeSimpleExpression() =
        Expressions.hide ()
        |> Expression
        |> sb3NormalizeScriptProperty

    [<Fact>]
    member _.normalizeWhenKeyPressed() =
        ListenerDefinition((), O.whenKeyPressed, [Literal((), SString "0")], BlockExpression((), []))
        |> Listener
        |> sb3NormalizeScriptProperty

    [<Fact>]
    member _.normalizeColorSees() =
        ComplexExpression((), O.``color:sees:``, [
            Literal((), SNumber 10.)
            Literal((), SNumber 20.)
        ])
        |> Expression
        |> sb3NormalizeScriptProperty

    [<Fact>]
    member _.normalizeWaitElapsedFrom() =
        BlockExpression((), [
            ComplexExpression((), O.``wait:elapsed:from:``, [
                Literal((), SNumber 0.)
            ])
        ])
        |> Statements
        |> sb3NormalizeScriptProperty

    [<Fact>]
    member _.normalizePenAndMusicExtension() =
        BlockExpression((), [
            // pen
            ComplexExpression((), O.``changePenHueBy:``, [Literal((), SNumber 0.)])
            // music
            ComplexExpression((), O.``changeTempoBy:``, [Literal((), SNumber 0.)])
        ])
        |> Statements
        |> sb3NormalizeScriptProperty

    [<Fact>]
    member _.normalizeMusicExtensionInAbs() =
        ComplexExpression((), O.abs, [Expression.Complex(ComplexExpression((), O.tempo, []))])
        |> Expression
        |> sb3NormalizeScriptProperty

    [<Fact>]
    member _.normalizeComplexBlocksOrder() =
        BlockExpression((), [
            ComplexExpression((), O.``say:``, [
                Expression.Complex(ComplexExpression((), O.``stringLength:``, [
                    Expression.Complex(ComplexExpression((), O.``+``, [
                        Expression.Complex(ComplexExpression((), O.answer, []))
                        Literal((), SNumber 0.)
                    ]))
                ]))
            ])
            ComplexExpression((), O.bounceOffEdge, [])
            ComplexExpression((), O.bounceOffEdge, [])
        ])
        |> Statements
        |> sb3NormalizeScriptProperty

    [<Fact>]
    member _.normalizeExtensionIdOrder() =
        BlockExpression((), [
            ComplexExpression((), O.clearPenTrails, [])                             // pen
            ComplexExpression((), O.``changeTempoBy:``, [Literal((), SNumber 0.)])  // music
            ComplexExpression((), O.clearPenTrails, [])                             // pen
        ])
        |> Statements
        |> sb3NormalizeScriptProperty

    [<Fact>]
    member _.normalizeAnyStage() = qcheck sb3NormalizeStageProperty

    [<Fact>]
    member _.exportAnyScript() = qcheckWith (fun x -> { x with MaxTest = x.MaxTest * 10 }) exportScriptToSb3Property

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

    [<Fact>]
    member _.exportCall() =
        BlockExpression((), [
            ComplexExpression((), O.call, [Literal((), SString "p %n"); Literal((), SNumber 10.)])
        ])
        |> Statements
        |> exportScriptToSb3Property

    [<Fact>]
    member _.exportBroadcastEmptyAndA() =
        BlockExpression((), [
            ComplexExpression((), O.``broadcast:``, [Literal((), SString "")])
            ComplexExpression((), O.``broadcast:``, [Literal((), SString "a")])
        ])
        |> Statements
        |> exportScriptToSb3Property

    [<Fact>]
    member _.exportBroadcastAAnd0() =
        BlockExpression((), [
            ComplexExpression((), O.``broadcast:``, [Literal((), SString "a")])
            ComplexExpression((), O.``broadcast:``, [Literal((), SString "0")])
        ])
        |> Statements
        |> exportScriptToSb3Property

    [<Fact>]
    member _.exportG17Double() =
        ProcedureDefinition((), "%n", [
            ParameterDefinition((), "x1", SNumber 8.39380860293761)
        ], Atomic, BlockExpression((), [
        ]))
        |> Procedure
        |> exportScriptToSb3Property

    [<Fact>]
    member _.exportBroadcastInDoForeverIf() =
        BlockExpression((), [
            ComplexExpression((), O.doForeverIf, [
                Literal((), SBool true)
                Block(BlockExpression((), [
                    ComplexExpression((), O.``broadcast:``, [Literal((), SString "a")])
                ]))
            ])
        ])
        |> Statements
        |> exportScriptToSb3Property

    [<Fact>]
    member _.exportAnyStage() = qcheck exportStageToSb3Property

    [<Fact>]
    member _.exportDefaultStage() =
        StageData.defaultValue
        |> exportStageToSb3Property

    [<Fact>]
    member _.exportEmptyCostume() =
        { StageData.defaultValue with
            costumes = [CostumeData.empty]
        }
        |> exportStageToSb3Property

    [<Fact>]
    member _.exportCostumeWithBitmapResolution0() =
        { StageData.defaultValue with
            costumes = [
                { CostumeData.empty with
                    bitmapResolution = Some 0.
                }
            ]
        }
        |> exportStageToSb3Property

    [<Fact>]
    member _.exportDotCostumeMd5() =
        { StageData.defaultValue with
            costumes = [
                { CostumeData.empty with
                    baseLayerMD5 = "."
                    baseLayerID = 0.
                }
            ]
        }
        |> exportStageToSb3Property

    [<Fact>]
    member _.exportStageScript2() =
        let scriptData s = { x = 0.; y = 0.; script = s }
        { StageData.defaultValue with
            scripts = [
                ComplexExpression((), O.answer, [])
                    |> Expression
                    |> scriptData

                ComplexExpression((), O.``contentsOfList:``, [Literal((), SString "l")])
                    |> Expression
                    |> scriptData
            ]
        }
        |> exportStageToSb3Property

    [<Fact>]
    member _.exportVideoAlphaDefault() =
        let stage = StageData.defaultValue
        { stage with
            ObjectDataExtension =
            { stage.ObjectDataExtension with
                videoAlpha = None
            }
        }
        |> exportStageToSb3Property

    [<Fact>]
    member _.exportDuplicatedCostumeNameAnd2() =
        { StageData.defaultValue with
            costumes = [
                { CostumeData.empty with costumeName = "a" }    // ⇒ "a"
                { CostumeData.empty with costumeName = "a" }    // ⇒ "a2"
                { CostumeData.empty with costumeName = "a2" }   // ⇒ "a3"
            ]
        }
        |> exportStageToSb3Property

    [<Fact>]
    member _.exportDuplicatedCostumeName00() =
        { StageData.defaultValue with
            costumes = [
                { CostumeData.empty with costumeName = "a00" }   // ⇒ "a00"
                { CostumeData.empty with costumeName = "a00" }   // ⇒ "a2"
            ]
        }
        |> exportStageToSb3Property

    [<Fact>]
    member _.bytesEmpty() =
        {
            project = StageData.defaultValue
            images = []
            sounds = []
        }
        |> sb3BytesSaveAndLoadProperty

    [<Fact>]
    member _.bytesSimple() =
        {
            project =
                ComplexExpression((), O.timer, [])
                |> Expression
                |> scriptToStage

            images = []
            sounds = []
        }
        |> sb3BytesSaveAndLoadProperty
