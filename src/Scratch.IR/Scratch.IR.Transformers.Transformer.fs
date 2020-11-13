module Scratch.IR.Transformers.Transformer
open Scratch.Ast
open Scratch.Primitives
open Scratch.Transformers
open Scratch.IR
open Scratch.IR.Transformers
open Scratch.IR.Source.Operators

module R = Scratch.Transformers.TransformResult


type internal IPlugins<'a> =
    abstract Plugin: Plugins<'a>

module internal State =
    let (|Plugins|) (s: IPlugins<_>) = s.Plugin
    let (|Settings|) (s: IEntityState<_>) = s.Settings
    let (|EntityState|) (s: IEntityState<_>) = s.Entity
    let (|MemberState|) (s: IMemberState<_>) = s.Member
    let (|ExpState|) (s: IExpState<_,_>) = s.Exp
    let (|ScriptState|) (s: IScriptState<_,_>) = s.Script
    let currentAtomicity (ExpState e) = e.atomicity

    [<Struct>]
    type State<'a> = {
        Plugins: Plugins<'a>
        Settings: Settings
    }
    with
        interface ISettings with
            member this.Settings = this.Settings

        interface IPlugins<'a> with
            member this.Plugin = this.Plugins

    let newConfigState plugins settings = {
        Plugins = plugins
        Settings = settings
    }

    type MemberState<'a> = {
        Plugins: Plugins<'a>
        Settings: Settings
        Entity: EntityEnv<'a>
        Member: MemberEnv<'a>
    }
    with
        interface IPlugins<'a> with
            member this.Plugin = this.Plugins

        interface IMemberState<'a> with
            member this.Settings = this.Settings
            member this.Entity = this.Entity
            member this.Member = this.Member

    let newMemberState m (e: 'S when 'S :> IEntityState<_> and 'S :> IPlugins<_>) = {
        Plugins = e.Plugin
        Settings = e.Settings
        Entity = e.Entity
        Member = m
    }

    type EntityState<'a> = {
        Plugins: Plugins<'a>
        Settings: Settings
        Entity: EntityEnv<'a>
    }
    with
        interface IPlugins<'a> with
            member this.Plugin = this.Plugins

        interface IEntityState<'a> with
            member this.Settings = this.Settings
            member this.Entity = this.Entity

    let newEntityState entity (settings: 'S when 'S :> ISettings and 'S :> IPlugins<_>) = {
        Plugins = settings.Plugin
        Settings = settings.Settings
        Entity = entity
    }

    type ScriptStates<'a,'W> = {
        Plugins: Plugins<'a>
        Settings: Settings
        Entity: EntityEnv<'a>
        Member: MemberEnv<'a>
        Script: ScriptEnv<'a,'W>
    }
    with
        interface IPlugins<'a> with
            member this.Plugin = this.Plugins

        interface IScriptState<'a,'W> with
            member this.Member = this.Member
            member this.Settings = this.Settings
            member this.Entity = this.Entity
            member this.Script = this.Script

    let newScriptState scriptState (memberState: 'S when 'S :> IMemberState<_> and 'S :> IPlugins<_>) = {
        Plugins = memberState.Plugin
        Settings = memberState.Settings
        Entity = memberState.Entity
        Member = memberState.Member
        Script = scriptState
    }
    type ExpStates<'a,'W> = {
        Plugins: Plugins<'a>
        Settings: Settings
        Entity: EntityEnv<'a>
        Member: MemberEnv<'a>
        Script: ScriptEnv<'a,'W>
        mutable Exp: ExpEnv<'a>
    }
    with
        interface IPlugins<'a> with
            member this.Plugin = this.Plugins

        interface IExpState<'a,'W> with
            member this.Settings = this.Settings
            member this.Entity = this.Entity
            member this.Member = this.Member
            member this.Exp
                with get() = this.Exp
                and set(v) = this.Exp <- v

            member this.Script = this.Script

    let newExpState exp (states: 'S when 'S :> IScriptState<_,_> and 'S :> IPlugins<_>) = {
        Plugins = states.Plugin
        Settings = states.Settings
        Entity = states.Entity
        Member = states.Member
        Script = states.Script
        Exp = exp
    }

[<AutoOpen>]
module internal StatisticsInternals =
    [<Struct>]
    type VariableStat = {
        readCount: int
        writeCount: int
        metaCount: int
    }
    module VariableStat =
        let empty = { readCount = 0; writeCount = 0; metaCount = 0 }
        let read1 = { empty with readCount = 1 }
        let write1 = { empty with writeCount = 1 }
        let meta1 = { empty with metaCount = 1 }

    [<Struct>]
    type ProcedureStat = {
        callCount: int
    }
    module ProcedureStat =
        let call1 = { callCount = 1 }

    [<Struct>]
    type ScriptStatTable = {
        variableStats: Map<Var, VariableStat>
        procedureStats: Map<ProcedureVar, ProcedureStat>
        scriptSize: int
    }
    type ScriptStat = {
        table: ScriptStatTable
        hasSelfCall: bool Lazy
    }

module internal Top =
    let withSource s = function
        | Top.Procedure x -> Top.Procedure <| Source.withSource s x
        | Top.Listener x -> Top.Listener <| Source.withSource s x
        | Top.VariableInit(VariableInitDef(v, x)) -> Top.VariableInit(VariableInitDef(v, Source.withSource s x))

    let source = function
        | Top.Procedure { source = s }
        | Top.Listener { source = s }
        | Top.VariableInit(VariableInitDef(_, { source = s })) -> s

[<AutoOpen>]
module private StaticticsHelpers =
    module ScriptStat =
        let empty = {
            variableStats = Map.empty
            procedureStats = Map.empty
            scriptSize = 0
        }
        let mergeVariable k s' s =
            let map = s.variableStats
            let v =
                match Map.tryFind k map with
                | ValueNone -> s'
                | ValueSome v -> {
                    readCount = v.readCount + s'.readCount
                    writeCount = v.writeCount + s'.writeCount
                    metaCount = v.metaCount + s'.metaCount
                }
            { s with variableStats = Map.add k v map }

        let mergeProcedure k s' s =
            let map = s.procedureStats
            let v =
                match Map.tryFind k map with
                | ValueNone -> s'
                | ValueSome v -> { callCount = v.callCount + s'.callCount }
            { s with procedureStats = Map.add k v map }

    let local s e =
        let s = { s with scriptSize = s.scriptSize + 1 }
        match e.value with
        | Var v -> ScriptStat.mergeVariable v VariableStat.read1 s
        | VarSet(v, _)
        | TupleSet(v, _, _) -> ScriptStat.mergeVariable v VariableStat.write1 s
        | Op((O.``showVariable:`` | O.``hideVariable:``), [{ value = Var v }]) -> ScriptStat.mergeVariable v VariableStat.meta1 s
        | Call(v, _) -> ScriptStat.mergeProcedure v ProcedureStat.call1 s
        | _ -> s

    let analyzeScriptTable s = function
        | Top.VariableInit(VariableInitDef(var, init)) ->
            let e = Exp.let' init.source var init (Exp.empty init.source)
            Exp.fold local s e

        | Top.Listener { value = ListenerDef(arguments = args; body = body) } ->
            let s = List.fold (Exp.fold local) s args
            Exp.fold local s body

        | Top.Procedure { value = ProcDef(body = body) } ->
            Exp.fold local s body

    let hasSelfCall stats = function
        | Top.Procedure p ->
            match Map.tryFind (ProcDef.var p) stats.procedureStats with
            | ValueNone -> false
            | ValueSome s -> 0 < s.callCount
        | _ -> false

    let analyzeScript script =
        let s = ScriptStat.empty
        let s = analyzeScriptTable s script
        {
            table = s
            hasSelfCall = lazy hasSelfCall s script
        }

    [<AutoOpen>]
    module ScriptStatIsTag =
        type private ScriptStatTagId = struct end
        [<Struct>]
        type ScriptStatTagShape =
            interface IIsTagIdShape with
                member _.Id = Tag.typeid the<ScriptStatTagId>

        let ScriptStatTag = IsTag.makeIsTag the<ScriptStatTagShape> (IsTagValue.reference the<ScriptStat Lazy>)

module internal ScriptStat =
    let procedureStatistics p =
        match p.source |> Tagged.tryFind ScriptStatTag with
        | ValueSome s -> s.Value
        | _ -> failwith "statistics not found"

    let updateTop s =
        let tag = Top.source s
        let tag = Tagged.add ScriptStatTag (lazy analyzeScript s) tag
        Top.withSource tag s

    let addStageStatistics stage =
        let addEntityStatistics entity =
            let scripts =
                entity.scripts
                |> List.map (fun s -> { s with script = updateTop s.script })
    
            { entity with scripts = scripts }

        stage
        |> addEntityStatistics
        |> EntityData.mapExtension (fun ({ StageDataExtension.children = children } as ex) ->
            let children =
                children
                |> List.map (function
                    | Choice2Of3 sprite -> Choice2Of3 <| addEntityStatistics sprite
                    | x -> x
                )
            { ex with StageDataExtension.children = children }
        )

    let topStatistics s =
        match Top.source s |> Tagged.tryFind ScriptStatTag with
        | ValueSome s -> s.Value
        | _ -> failwith "statistics not found"

module private Helpers =
    let rec mapListNoRemove t xs env =
        match xs with
        | [] -> R.noModify []
        | head::tail as ees ->
            let head_tail =
                struct(head, tail)
                |> R.sequence2
                    (fun head -> t head env)
                    (fun tail -> mapListNoRemove t tail env) 

            head_tail
            |> R.mapIfModified ees (fun struct(head, tail) -> head::tail)

    let rec transformExp e env =
        let (State.Plugins { transformExp = plugin }) = env
        R.compose transformExpWithoutPlugin plugin.Invoke e env

    and transformExpWithoutPlugin e env =
        let inline exp1 make e1 = transformExp e1 env |> R.mapIfModified e (fun e1 -> make e1 @+e.source)
        let inline exp2 make (e1, e2) =
            struct(e1, e2)
            |> R.sequence2
                (fun e1 -> transformExp e1 env)
                (fun e2 -> transformExp e2 env)
            |> R.mapIfModified e (fun struct(e1, e2) -> make(e1, e2) @+e.source)

        let inline exp3 make (e1, e2, e3) =
            struct(e1, struct(e2, e3))
            |> R.sequence2
                (fun e1 -> transformExp e1 env)
                (R.sequence2
                    (fun e2 -> transformExp e2 env)
                    (fun e3 -> transformExp e3 env))
            |> R.mapIfModified e (fun struct(e1, struct(e2, e3)) -> make(e1, e2, e3) @+e.source)

        let inline exps make es = mapListNoRemove transformExp es env |> R.mapIfModified e (fun es -> make es @+e.source)
        let inline listOps make xs = mapListNoRemove transformListOp xs env |> R.mapIfModified e (fun xs -> make xs @+e.source)

        match e.value with
        | Lit _
        | Var _ -> R.noModify e

        | Atom e1 ->
            match env.Exp.atomicity with
            | Atomic -> ()
            | NoAtomic -> env.Exp <- { env.Exp with atomicity = Atomic }

            exp1 Atom e1

        | Coerce(k, e1, t) -> exp1 (fun e1 -> Coerce(k, e1, t)) e1
        | TupleGet(e1, i) -> exp1 (fun e1 -> TupleGet(e1, i)) e1
        | TupleSet(v, i, e1) -> exp1 (fun e1 -> TupleSet(v, i, e1)) e1
        | VarSet(v, e1) -> exp1 (fun e1 -> VarSet(v, e1)) e1

        | Let(v, e1, e2) -> exp2 (fun (e1, e2) -> Let(v, e1, e2)) (e1, e2)
        | Seq(e1, e2) -> exp2 Seq (e1, e2)

        | If(e1, e2, e3) -> exp3 If (e1, e2, e3)

        | Call(p, es) -> exps (fun es -> Call(p, es)) es
        | NewTuple es -> exps NewTuple es
        | Op(op, es) -> exps (fun es -> Op(op, es)) es

        | ListOp(op, xs) -> listOps (fun xs -> ListOp(op, xs)) xs

    and transformListOp x env =
        match x with
        | Choice1Of2 e -> transformExp e env |> R.mapIfModified x Choice1Of2
        | _ -> R.noModify x

    module Top =
        let atomicity = function
            | Top.Procedure p -> ProcDef.atomicity p
            | Top.Listener _ -> NoAtomic

            // TODO: NoAtomic な変数初期化のサポート
            | Top.VariableInit _ -> Atomic

    let transformExpWith script body env =
        let env' = { procedureScript = script; atomicity = Top.atomicity script }
        let env = State.newExpState env' env
        transformExp body env
        |> R.mapIfModified body Exp.validate

    let transformListenerDef script ({ source = source; value = ListenerDef(name, parameters, body) } as d) env =
        let body = transformExpWith script body env
        body |> R.mapIfModified d (fun body -> ListenerDef(name, parameters, body) @+source)

    let transformProcDef script ({ source = source; value = ProcDef(header, body) } as d) env =
        let body = transformExpWith script body env
        body |> R.mapIfModified d (fun body -> ProcDef(header, body) @+source)

    let transformTop s env =
        let transformScript s env =
            match s with
            | Top.Listener x ->
                let x = transformListenerDef s x env
                x |> R.mapIfModified s Top.Listener

            | Top.Procedure x ->
                let x = transformProcDef s x env
                x |> R.mapIfModified s Top.Procedure

            | _ -> R.noModify s

        let State.Settings { maxIterationCount = maxCount } & State.Plugins { transformTop = plugin } = env
        let transformScriptRepeat = Transformer.transformRepeat (max 1 (maxCount / 8)) transformScript
        let s2 = R.compose plugin.Invoke transformScriptRepeat s env
        s2 |> R.mapIfModified s ScriptStat.updateTop

    [<Struct; RequireQualifiedAccess>]
    type ScriptsMapping<'S,'a> when 'S :> 'a Tagged IMemberState and 'S :> 'a Tagged IPlugins = { env: 'S } with
        interface R.IMapFoldListMapping<'a Tagged IRScriptData, int> with
            override f.Invoke(convertedScripts, i, d) =
                let id =
                    match d.script with
                    | Top.Procedure p -> ProcedureName(ProcDef.var p)
                    | _ -> ScriptIndex i

                let env =
                    f.env
                    |> State.newScriptState {
                        scriptId = id
                        convertedScripts = convertedScripts
                    } 
                let d' =
                    transformTop d.script env
                    |> R.mapIfModified d (fun s -> { d with script = s })

                struct(d', i + 1)

    let transformEntityScripts scripts env =
        let struct(datas, _) = R.mapFoldListV { ScriptsMapping.env = env } 0 scripts
        datas

    let transformEntity entity env =
        let extension = entity.ObjectDataExtension
        let unitEntity = EntityData.mapExtension ignore entity

        let (State.Plugins { transformEntity = f }) = env
        R.compose f.Invoke (fun entity env ->
            transformEntityScripts entity.scripts (State.newMemberState { self = entity } env)
            |> R.mapIfModified entity (fun scripts -> { entity with scripts = scripts })
        ) unitEntity env
        |> R.mapIfModified entity (EntityData.mapExtension (fun () -> extension))

    let transformSprite parent sprite env =
        let env' = { parent = Some parent; children = [] }
        let env = State.newEntityState env' env
        transformEntity sprite env

    let transformStageChild env parent child =
        match child with
        | Choice2Of3 x ->
            let x = transformSprite parent x env
            x |> R.mapIfModified child Choice2Of3

        | Choice1Of3 _
        | Choice3Of3 _ as x -> R.noModify x

    let transformStageChildren env parent children =
        R.mapList (fun c -> transformStageChild env parent c) children

    let transformStageOnce stage env =
        let transformStageDataExtension stage env =
            let extension = stage.ObjectDataExtension
            let children = transformStageChildren env stage extension.children
            children
            |> R.mapIfModified stage (fun children ->
                stage
                |> EntityData.mapExtension (fun e -> { e with children = children })
            )

        let transformStageLocal (stage: IRStageData<_>) env =
            let env' = {
                parent = None
                children = stage.ObjectDataExtension.children
            }
            let env = State.newEntityState env' env
            transformEntity stage env

        R.compose transformStageDataExtension transformStageLocal stage env
open Helpers

let internal transformEntityScripts scripts env = transformEntityScripts scripts env

let transformStage config (stage: _ IRStageData): _ IRStageData =
    let env = State.newConfigState config.plugins (Config.toSettings config)
    let stage = ScriptStat.addStageStatistics stage
    let stage' = Transformer.transformRepeat config.maxIterationCount transformStageOnce stage env
    R.valueOrOld stage stage'
