namespace Scratch.Serialization.Sb3.Converter.Test
open Scratch
open Scratch.Serialization.Sb3
open Scratch.Serialization.Sb3.Ast
open Scratch.Primitives
open System.Collections.Generic
open Xunit


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

    let qcheck = Scratch.Json.Tests.qcheck

namespace Scratch.Serialization.Sb3.Converter
open Scratch
open Scratch.Ast
open Scratch.Serialization.Sb3
open Scratch.Serialization.Sb3.Test.Helpers
open Scratch.Serialization.Sb3.Converter.Test.Helpers
open System
open Xunit


type Tests() =
    let client = AdaptorJs.startServerAndConnect() |> Async.RunSynchronously

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

    interface IDisposable with
        member _.Dispose() = client.Dispose() |> Async.RunSynchronously

    [<Fact>]
    member _.sb3NormalizePropertyTest() = qcheck sb3NormalizeProperty

    [<Fact>]
    member _.sb3NormalizeSimpleExpressionTest() =
        Expressions.hide ()
        |> Expression
        |> sb3NormalizeProperty

    [<Fact>]
    member _.sb3NormalizeWhenKeyPressedTest() =
        ListenerDefinition((), O.whenKeyPressed, [Literal((), SString "0")], BlockExpression((), []))
        |> Listener
        |> sb3NormalizeProperty

    [<Fact>]
    member _.exportScriptToSb3PropertyTest() = qcheck exportScriptToSb3Property

    [<Fact>]
    member _.exportEmptyBlockToSb3Test() =
        Statements(BlockExpression((), []))
        |> exportScriptToSb3Property

    [<Fact>]
    member _.exportEmptyListenerToSb3Test() =
        ListenerDefinition((), O.whenClicked, [], BlockExpression((), []))
        |> Listener
        |> exportScriptToSb3Property

    [<Fact>]
    member _.exportEmptyProcedureToSb3Test() =
        ProcedureDefinition((), "p", [], Atomic, BlockExpression((), []))
        |> Procedure
        |> exportScriptToSb3Property

(*
テスト名:	Scratch.Serialization.Sb3.Converter.Tests.sb3NormalizePropertyTest
テストの完全名:	Scratch.Json.Tests.Scratch.Serialization.Sb3.Converter.Tests.Scratch.Serialization.Sb3.Converter.Tests.sb3NormalizePropertyTest
テスト ソース:	C:\Users\User\source\Repos\Wiinuk.Scratch\tests\Scratch.Json.Tests\Scratch.Serialization.Sb3.converterTests.fs : 行 193
テスト成果:	失敗
テスト継続期間:	0:00:00

テスト名:	Scratch.Serialization.Sb3.Converter.Tests.sb3NormalizePropertyTest
テスト成果:	失敗
結果  のスタック トレース:	
at Microsoft.FSharp.Core.PrintfModule.PrintFormatToStringThenFail@1637.Invoke(String message) in F:\workspace\_work\1\s\src\fsharp\FSharp.Core\printf.fs:line 1637
   at <StartupCode$FsCheck>.$Runner.get_throwingRunner@363.FsCheck-IRunner-OnFinished(String name, TestResult testResult)
   at FsCheck.Runner.check[a](Config config, a p)
   at FsCheck.Check.One[Testable](Config config, Testable property)
   at AssertionOperations.qcheckWith[a](FSharpFunc`2 withConfig, a property) in C:\Users\User\source\Repos\Wiinuk.Scratch\tests\Scratch.Test.Helpers\AssertionOperations.fs:line 44
   at Scratch.Json.Tests.qcheck[a](a test) in C:\Users\User\source\Repos\Wiinuk.Scratch\tests\Scratch.Json.Tests\Scratch.Json.Tests.fs:line 19
   at Scratch.Serialization.Sb3.Converter.Tests.sb3NormalizePropertyTest() in C:\Users\User\source\Repos\Wiinuk.Scratch\tests\Scratch.Json.Tests\Scratch.Serialization.Sb3.converterTests.fs:line 194
結果  のメッセージ:	
System.Exception : Falsifiable, after 4 tests (2 shrinks) (StdGen (1617369990,296809479)):
Original:
Listener
  (ListenerDefinition
     ((),whenKeyPressed,[Literal ((),SString "k
")],BlockExpression ((),[])))
Shrunk:
Listener
  (ListenerDefinition
     ((),whenKeyPressed,[Literal ((),SString "")],BlockExpression ((),[])))
with exception:
System.Exception: Error: commandName: "roundtrip-json", error: {
  "stack": "SyntaxError: Unexpected token ] in JSON at position 183
  at JSON.parse (\u003Canonymous\u003E)
  at sb3ToSb3Json (C:\\Users\\User\\source\\Repos\\Wiinuk.Scratch\\tests\\Scratch.Json.Tests\\bin\\Debug\\netcoreapp3.1\\adaptor.js:33:52)
  at protectedCallAsync (C:\\Users\\User\\source\\Repos\\Wiinuk.Scratch\\tests\\Scratch.Json.Tests\\bin\\Debug\\netcoreapp3.1\\adaptor.js:170:72)
  at protectedCallAsync (C:\\Users\\User\\source\\Repos\\Wiinuk.Scratch\\tests\\Scratch.Json.Tests\\bin\\Debug\\netcoreapp3.1\\adaptor.js:139:42)
  at Server.ipc.server.on (C:\\Users\\User\\source\\Repos\\Wiinuk.Scratch\\tests\\Scratch.Json.Tests\\bin\\Debug\\netcoreapp3.1\\adaptor.js:168:34)
  at Server.emit (C:\\Users\\User\\source\\Repos\\Wiinuk.Scratch\\node_modules\\event-pubsub\\es5.js:74:21)
  at Server.gotData (C:\\Users\\User\\source\\Repos\\Wiinuk.Scratch\\node_modules\\node-ipc\\dao\\socketServer.js:194:14)
  at Socket.emit (events.js:189:13)
  at addChunk (_stream_readable.js:284:12)
  at readableAddChunk (_stream_readable.js:261:13)",
  "message": "Unexpected token ] in JSON at position 183"
}
   at Microsoft.FSharp.Core.PrintfModule.PrintFormatToStringThenFail@1637.Invoke(String message) in F:\workspace\_work\1\s\src\fsharp\FSharp.Core\printf.fs:line 1637
   at Scratch.Serialization.Sb3.Test.Helpers.AdaptorJs.convertBy@148-1.Invoke(ValueTuple`2 _arg1) in C:\Users\User\source\Repos\Wiinuk.Scratch\tests\Scratch.Json.Tests\Scratch.Serialization.Sb3.Test.Helpers.fs:line 154
   at Microsoft.FSharp.Control.AsyncPrimitives.CallThenInvokeNoHijackCheck[a,b](AsyncActivation`1 ctxt, FSharpFunc`2 userCode, b result1) in F:\workspace\_work\1\s\src\fsharp\FSharp.Core\async.fs:line 417
   at Scratch.Serialization.Sb3.Test.Helpers.NodeIpcClientModule.sendAndResponse@67-5.Invoke(AsyncActivation`1 ctxt) in C:\Users\User\source\Repos\Wiinuk.Scratch\tests\Scratch.Json.Tests\Scratch.Serialization.Sb3.Test.Helpers.fs:line 67
   at Microsoft.FSharp.Control.Trampoline.Execute(FSharpFunc`2 firstAction) in F:\workspace\_work\1\s\src\fsharp\FSharp.Core\async.fs:line 109
--- End of stack trace from previous location where exception was thrown ---
   at Microsoft.FSharp.Control.AsyncResult`1.Commit() in F:\workspace\_work\1\s\src\fsharp\FSharp.Core\async.fs:line 350
   at Microsoft.FSharp.Control.AsyncPrimitives.RunSynchronouslyInAnotherThread[a](CancellationToken token, FSharpAsync`1 computation, FSharpOption`1 timeout) in F:\workspace\_work\1\s\src\fsharp\FSharp.Core\async.fs:line 863
   at Microsoft.FSharp.Control.AsyncPrimitives.RunSynchronously[T](CancellationToken cancellationToken, FSharpAsync`1 computation, FSharpOption`1 timeout) in F:\workspace\_work\1\s\src\fsharp\FSharp.Core\async.fs:line 890
   at Microsoft.FSharp.Control.FSharpAsync.RunSynchronously[T](FSharpAsync`1 computation, FSharpOption`1 timeout, FSharpOption`1 cancellationToken) in F:\workspace\_work\1\s\src\fsharp\FSharp.Core\async.fs:line 1154
   at Scratch.Serialization.Sb3.Converter.Tests.sb3NormalizeProperty(Script`1 script) in C:\Users\User\source\Repos\Wiinuk.Scratch\tests\Scratch.Json.Tests\Scratch.Serialization.Sb3.converterTests.fs:line 171
   at <StartupCode$Scratch-Json-Tests>.$Scratch.Serialization.Sb3.converterTests.sb3NormalizePropertyTest@194.Invoke(Script`1 script) in C:\Users\User\source\Repos\Wiinuk.Scratch\tests\Scratch.Json.Tests\Scratch.Serialization.Sb3.converterTests.fs:line 194
   at FsCheck.Testable.evaluate[a,b](FSharpFunc`2 body, a a)


*)
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
