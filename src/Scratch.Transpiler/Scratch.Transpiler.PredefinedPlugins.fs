module Scratch.Transpiler.PredefinedPlugins
open FSharp.Quotations
open Scratch
open Scratch.Ast.Transformers
open Scratch.MemoryModel
open Scratch.Primitives.Field
open Scratch.Reflection
open Scratch.Reflection.Member
open Scratch.Reflection.Expr
open Scratch.Transformers
open Scratch.Transpiler.Core
open Scratch.Transpiler.Environments
open Scratch.Transpiler.PredefinedPlugin
open Scratch.Transpiler.PredefinedPlugin.Helpers
module Field = Scratch.MemoryModel.Field
module E = Quotations.Patterns
module E = Quotations.DerivedPatterns
module E = Scratch.Reflection.TypedExpr
module E = Scratch.Reflection.Expr
type private E = Quotations.Expr

[<AutoOpen>]
module private Helpers =
    [<AbstractClass; Sealed>]
    type private CallExpressionMapHolder<'S,'E>
        when 'S :> IWithState<'S, State>

        and 'E :> IWithConfig<'E, Config>
        and 'E :> IWithEnvironment<'E, Environment>
        and 'E :> IWithBlockEnvironment<'E, BlockEnvironment>
        private () =

        static let value: Map<_, OptimizedClosures.FSharpFunc<SE<'S,'E>,_,_>> =
            [
                ScratchRuntime.blockLikeCalls()
                FSharpCore.fsharpCoreCallExpressions()
                ScratchRuntime.scratchCallExpressions()
                AsyncPlugin.asyncCallExpressions()
                CollectorPlugin.callExpressions()
            ]
            |> Seq.concat
            |> methodCasesToMap

        static member Value = value

    let notM, _ = findMethod <@ Operators.not @>
    let callExpressionPlugin' senv = function
        | CallMap CallExpressionMapHolder.Value senv x -> x

        | E.WhileLoop(test, body) as e -> ScratchRuntime.transpileRepeatUntil senv (E.Call(notM, [test])@+getLocation test) body (SourceCode.ofExpr e)

        // <@ for _ in %e1..%e2 do %body @> => `repeat (let i = %e1 in %e2 - i + 1) \{ %body }`
        // <@ for %i in %e1..%e2 do %body @> => `i; repeat (i <- %e1; %e2 - i + 1) \{ %body; i <- i + 1 }`
        | E.ForIntegerRangeLoop(v, e1, e2, body) as e when v.Type = typeof<int> && not v.IsMutable ->
            let (@+) e c = withLocationTyped c e
            let localName = (get FConfig senv.e).localName
            let includeV = body.GetFreeVars() |> Seq.contains v

            let i = TypedVar<int>(localName, isMutable = includeV)
            let l = getLocation e
            let l1 = getLocation e1
            let l2 = getLocation e2

            let e1 = e1 |> E.Cast<int>
            let e2 = e2 |> E.Cast<int>
            let count = <@ %(<@ %e2 - %(E.Var i @+l2) @> @+l2) + 1 @> @+l2

            if includeV then
                let count = count @+Location.merge l1 l2

                // TODO: Substitute “à‚Ì v' ‚©‚ç i' ‚É location ‚ð“`”d‚³‚¹‚é
                let i' = E.Var i :> Expr |> Some
                let body = body.Substitute(fun v' -> if v' = v then i' else None) |> E.Cast<unit>
                let body = <@ %body; %(E.VarSet(i, <@ %(E.Var i @+l) + 1 @> @+l) @+l) @> @+l
                ScratchRuntime.transpileRepeat senv (Some(i.Raw, e1)) count body (SourceCode.ofExpr e)

            else
                let count = E.Let(i, e1, count) @+l
                ScratchRuntime.transpileRepeat senv None count body (SourceCode.ofExpr e)

        | _ -> skip()
    let callExpressionPlugin() = { new ExpressionPluginProcess() with member _.Invoke senv e = callExpressionPlugin' senv e }

    let preTransformPlugin config e =
        let e =
            match FSharpCore.preTransformPlugin config e with
            | None -> e
            | Some e -> e

        match CollectorPlugin.preTransformPlugin config e with
        | None -> Some e
        | e -> e

    let expressionPlugin() =
        FSharpCore.fsharpCoreExpressionPlugin()
        </> ScratchRuntime.scratchPointerExpressionPlugin()
        </> ScratchRuntime.scratchExpressionPlugin()
        </> AsyncPlugin.asyncExpressionPlugin()
        </> CollectorPlugin.expressionPlugin()
        </> callExpressionPlugin()

    let externalCallItems() =
        let memoryP = findProperty <@@ Memory.memory @@>
        let outputE = findProperty <@@ ConsoleOperations.output @@>
        let outE = <@@ ConsoleOperations.out @@>
        let outLineE = <@@ ConsoleOperations.outLine @@>
        [
        [
            <@@ Memory.clear @@>
            <@@ Memory.push @@>
            <@@ Memory.memoryBegin @@>
            <@@ Memory.memoryEnd @@>
            <@@ Memory.read @@>
            <@@ Memory.write @@>
            <@@ Memory.get @@>
            <@@ Memory.set @@>
            <@@ Scratch.MemoryModel.Operators.(->.) @@>
            <@@ Scratch.MemoryModel.Operators.(->%): _ -> Field<_,N> -> double @@>
            <@@ Scratch.MemoryModel.Operators.(<-*) @@>
            <@@ Scratch.MemoryModel.Operators.(<-%): N Reference -> double -> _ @@>

            <@@ Vector.reference @@>
        ], fun senv _ ->
            addPropertyAsList senv SourceCode.empty NoExport memoryP
            Ok()
        [
            <@@ failwith @@>
            <@@ failwithf @@>
            <@@ printfn @@>
            <@@ printf @@>
            <@@ raise @@>
            Expr.Call(typeof<System.Diagnostics.Debug>.GetMethod("Assert", [|typeof<bool>|]), [ <@@ true @@> ])
        ], fun senv _ ->
            do addPropertyAsList senv SourceCode.empty NoExport outputE
            do transpileExternalItemSpecs senv outE
            do transpileExternalItemSpecs senv outLineE
            Ok()
        ]

    [<AbstractClass; Sealed>]
    type private ExternalCallItemMapHolder<'S,'E,'Envs>
        when 'S :> IWithState<'S, State>
        and 'S :> IWithEnvironment<'S, Environment>
        and 'S :> IWithExternalItemState<'S, ExternalItemState<'Envs>>

        and 'Envs :> IWithConfig<'Envs, Config>
        and 'Envs :> IWithEnvironment<'Envs, Environment>

        and 'E :> IWithConfig<'E, Config>
        and 'E :> IWithItemEnvironment<'E, ItemEnvironment>
        and 'E :> IWithEnvironment<'E, Environment>
        private () =

        static let value: Map<_, OptimizedClosures.FSharpFunc<SE<'S,'E>,_,_>> =
            [
                externalCallItems()
                CollectorPlugin.externalCallItems()
            ]
            |> Seq.concat
            |> methodCasesToMap

        static member Value = value

    let externalItemsCallPlugin' senv = function
        | CallMap ExternalCallItemMapHolder.Value senv x -> x
        | _ -> skip()

    let externalItemsCallPlugin() = { new ExternalItemsPluginProcess() with member _.Invoke senv e = externalItemsCallPlugin' senv e }

    let externalItemsPlugin() =
        FSharpCore.externalItemsPlugin()
        </> CollectorPlugin.externalItemsPlugin()
        </> externalItemsCallPlugin()

    let itemPlugin() = AsyncPlugin.asyncItemPlugin()

    let postTransformPlugin() = {
        transformExpression = Transformer.merged [
            PredefinedTransformers.constantFolding
            PredefinedTransformers.algebraicSimplification
        ]
        transformComplexExpressions = Transformer.merged [
            PredefinedTransformers.constantPropagation
            PredefinedTransformers.purePropagation
            PredefinedTransformers.copyPropagation
            PredefinedTransformers.copyPurePropagation
            PredefinedTransformers.eliminateDeadSetExpression
            PredefinedTransformers.eliminateSelfSetExpression
            PredefinedTransformers.eliminateDeadConditionExpression
            PredefinedTransformers.strengthReduction
        ]
        transformScript = PredefinedTransformers.eliminateTopLevelExpression
        transformEntity = Transformer.merged [
            PredefinedTransformers.inlineExpansion id
            PredefinedTransformers.joinWhenGreenFlags
            PredefinedTransformers.eliminateDeadProcedures
            PredefinedTransformers.eliminateUnusedVariables
        ]
    }

let predefinedPlugin = {
    preTransform = preTransformPlugin
    expression = expressionPlugin()
    item = itemPlugin()
    externalItems = externalItemsPlugin()
    postTransform = postTransformPlugin()
}
