namespace Scratch.Ast.Transformers
open Scratch.Ast
open Scratch.Primitives
open Scratch.Transformers
open System.Collections.Generic
module R = Scratch.Transformers.TransformResult


[<Struct>]
type ScriptId =
    | ProcedureName of name: string
    | ScriptIndex of index: int

[<Struct>]
type ScriptEnv<'a> = {
    scriptId: ScriptId
    convertedScripts: PooledBuffer<'a ScriptData, HUnit>
}
[<Struct>]
type ProcedureEnv<'a> = {
    procedureScript: Script<'a>
    procedureBody: BlockExpression<'a>
}

type Plugins<'a> = {
    transformExpression: Transformer<BlockEnvs<'a>, Expression<'a>>
    transformComplexExpressions: Transformer<BlockEnvs<'a>, ComplexExpression<'a> list>
    transformScript: Transformer<ScriptEnvs<'a>, Script<'a>>
    transformEntity: Transformer<EntityEnvs<'a>, EntityData<unit,'a>>
}
and Config<'a> = {
    plugins: Plugins<'a>
    maxIterationCount: int
}
and [<Struct>] EntityEnv<'a> = {
    parent: StageData<'a> option
    children: StageDataChild<'a> list
}
and [<Struct>] MemberEnv<'a> = {
    self: EntityData<unit,'a>
}
and BlockEnvs<'a> = HTuple<
    (* procedure: *)ProcedureEnv<'a>,
    (* script: *)   ScriptEnv<'a>,
    (* member': *)  MemberEnv<'a>,
    (* entity: *)   EntityEnv<'a>,
    (* config: *)   Config<'a>
>
and ScriptEnvs<'a> = HTuple<
    (* script: *)   ScriptEnv<'a>,
    (* member': *)  MemberEnv<'a>,
    (* entity: *)   EntityEnv<'a>,
    (* config: *)   Config<'a>
>
and MemberEnvs<'a> = HTuple<
    (* member': *)  MemberEnv<'a>,
    (* entity: *)   EntityEnv<'a>,
    (* config: *)   Config<'a>
>
and EntityEnvs<'a> = HTuple<
    (* entity: *)   EntityEnv<'a>,
    (* config: *)   Config<'a>
>

module Plugins =
    let empty = {
        transformExpression = Transformer.noop
        transformComplexExpressions = Transformer.noop
        transformScript = Transformer.noop
        transformEntity = Transformer.noop
    }
    let merge p1 p2 = {
        transformExpression = R.compose p1.transformExpression p2.transformExpression
        transformComplexExpressions = R.compose p1.transformComplexExpressions p2.transformComplexExpressions
        transformScript = R.compose p1.transformScript p2.transformScript
        transformEntity = R.compose p1.transformEntity p2.transformEntity
    }

module Config =
    let defaultConfig = {
        maxIterationCount = 100
        plugins = Plugins.empty
    }

module BlockEnvs =
    let (|Procedure|) e = e.head
    let (|Script|) e = e.tail.head
    let (|Member|) e = e.tail.tail.head
    let (|Entity|) e = e.tail.tail.tail.head
    let (|Config|) e = e.tail.tail.tail.tail.head

    let ofScriptEnvs procedureEnv scriptEnvs = procedureEnv^^scriptEnvs

module ScriptEnvs =
    let (|Script|) e = e.head
    let (|Member|) e = e.tail.head
    let (|Entity|) e = e.tail.tail.head
    let (|Config|) e = e.tail.tail.tail.head
    let ofMemberEnvs scriptEnv memberEnvs = scriptEnv^^memberEnvs

module MemberEnvs =
    let (|Member|) e = e.tail.head
    let (|Entity|) e = e.tail.tail.head
    let (|Config|) e = e.tail.tail.tail.head
    let ofEntityEnvs memberEnv entityEnvs = memberEnv^^entityEnvs

module EntityEnvs =
    let (|Entity|) e = e.head
    let (|Config|) e = e.tail.head
    let ofConfigs entity configs = entity^^configs
