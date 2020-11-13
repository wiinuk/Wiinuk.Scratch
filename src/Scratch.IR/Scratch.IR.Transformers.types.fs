namespace Scratch.IR.Transformers
open Scratch.Ast
open Scratch.IR
open Scratch.Primitives
open Scratch.Transformers

module R = Scratch.Transformers.TransformResult

type IRScriptData<'a> = ScriptData<'a Top,'a>
type IRStageDataChild<'a> = StageDataChild<'a Top,'a VariableDecl,'a ListDef,'a>

[<Struct>]
type internal ScriptId =
    | ProcedureName of name: ProcedureVar
    | ScriptIndex of index: int

[<Struct>]
type Settings = internal {
    maxIterationCount: int
}
[<Struct>]
type EntityEnv<'a> = internal {
    parent: IRStageData<'a> option
    children: IRStageDataChild<'a> list
}
[<Struct>]
type MemberEnv<'a> = internal {
    self: IREntityData<unit,'a>
}
[<Struct>]
type ScriptEnv<'a,'W> = internal {
    scriptId: ScriptId
    convertedScripts: PooledBuffer<'a IRScriptData,'W>
}
[<Struct>]
type ExpEnv<'a> = internal {
    procedureScript: Top<'a>
    atomicity: Atomicity
}

type ISettings =
    abstract Settings: Settings

type IEntityState<'a> =
    inherit ISettings
    abstract Entity: EntityEnv<'a>

type IMemberState<'a> =
    inherit IEntityState<'a>
    abstract Member: MemberEnv<'a>

type IScriptState<'a,'W> =
    inherit IMemberState<'a>
    abstract Script: ScriptEnv<'a,'W>

type IExpState<'a,'W> =
    inherit IScriptState<'a,'W>
    abstract Exp: ExpEnv<'a> with get, set

type IComposableShape<'C> =
    inherit IShape
    abstract Compose: 'C * 'C -> 'C
    abstract Empty: 'C

[<AbstractClass; NoEquality; NoComparison>]
type Composable<'Self,'Shape when 'Shape :> IComposableShape<'Self>>() = class end
module Composable =
    module private Shape =
        let get<'Shape when 'Shape :> IShape and 'Shape : struct> = Unchecked.defaultof<'Shape>

    [<GeneralizableValue>]
    let empty<'T,'Shape when 'Shape :> IComposableShape<'T> and 'Shape : struct and 'T :> Composable<'T,'Shape>> = Shape.get<'Shape>.Empty
    let compose<'T,'Shape when 'Shape :> IComposableShape<'T> and 'Shape : struct and 'T :> Composable<'T,'Shape>> t1 t2 = Shape.get<'Shape>.Compose(t1, t2)

[<AbstractClass>]
type ExpTransformer<'a>() =
    inherit Composable<ExpTransformer<'a>, ExpTransformerShape<'a>>()
    abstract Invoke: 'a Exp -> #IExpState<'a,'W> -> 'a Exp TransformResult

and [<Struct>] ExpTransformerShape<'a> =
    interface IComposableShape<ExpTransformer<'a>> with
        member _.Empty = { new ExpTransformer<_>() with member _.Invoke e _ = R.noModify e }
        member _.Compose(f1, f2) = {
            new ExpTransformer<_>() with
                member _.Invoke e s = R.compose f1.Invoke f2.Invoke e s
        }

[<AbstractClass>]
type TopTransformer<'a>() =
    inherit Composable<TopTransformer<'a>, TopTransformerShape<'a>>()
    abstract Invoke: 'a Top -> #IScriptState<'a,'W> -> 'a Top TransformResult

and [<Struct>] TopTransformerShape<'a> =
    interface IComposableShape<TopTransformer<'a>> with
        member _.Empty = { new TopTransformer<_>() with member _.Invoke e _ = R.noModify e }
        member _.Compose(f1, f2) = {
            new TopTransformer<_>() with
                member _.Invoke e s = R.compose f1.Invoke f2.Invoke e s
        }

[<AbstractClass>]
type EntityTransformer<'a>() =
    inherit Composable<EntityTransformer<'a>, EntityTransformerShape<'a>>()
    abstract Invoke: IREntityData<unit,'a> -> #IEntityState<'a> -> IREntityData<unit,'a> TransformResult

and [<Struct>] EntityTransformerShape<'a> =
    interface IComposableShape<EntityTransformer<'a>> with
        member _.Empty = { new EntityTransformer<_>() with member _.Invoke e _ = R.noModify e }
        member _.Compose(f1, f2) = {
            new EntityTransformer<_>() with
                member _.Invoke e s = R.compose f1.Invoke f2.Invoke e s
        }

type Plugins<'a> = {
    transformExp: 'a ExpTransformer
    transformTop: 'a TopTransformer
    transformEntity: 'a EntityTransformer
}
module Plugins =
    let empty = {
        transformExp = Composable.empty
        transformTop = Composable.empty
        transformEntity = Composable.empty
    }

[<Struct>]
type Config<'a> = {
    maxIterationCount: int
    plugins: Plugins<'a>
}
module Config =
    let toSettings config = { maxIterationCount = config.maxIterationCount }
