module Scratch.Ast.Transformers.Transformer
open Scratch.Ast
open Scratch.Primitives
open Scratch.Transformers
module R = Scratch.Transformers.TransformResult


let rec transformExpression e env =
    let (BlockEnvs.Config { plugins = { transformExpression = plugin } }) = env

    let transformExpression e env =
        match e with
        | Literal _ -> R.noModify e
        | Complex x -> let x = transformComplexExpression x env in x |> R.mapIfModified e Complex
        | Block x -> let x = transformBlockExpression x env in x |> R.mapIfModified e Block

    R.compose transformExpression plugin e env

and transformComplexExpression (ComplexExpression(state, operator, operands) as e) env =
    operands
    |> R.mapList (fun e -> transformExpression e env)
    |> R.mapIfModified e (fun operands -> ComplexExpression(state, operator, operands))

and transformComplexExpressions es env =
    let transformComplexExpressions' es env =
        match es with
        | [] -> R.noModify []
        | head::tail as ees ->
            let head_tail =
                struct(head, tail)
                |> R.sequence2
                    (fun head -> transformComplexExpression head env)
                    (fun tail -> transformComplexExpressions tail env) 

            head_tail
            |> R.mapIfModified ees (fun struct(head, tail) -> head::tail)

    let (BlockEnvs.Config { plugins = { transformComplexExpressions = plugin } }) = env
    R.compose transformComplexExpressions' plugin es env

and transformBlockExpression (BlockExpression(state, es) as e) env =
    let es = transformComplexExpressions es env
    es |> R.mapIfModified e (fun es -> BlockExpression(state, es))

let transformProcedureBody script body env =
    let env' = { procedureScript = script; procedureBody = body }
    let env = BlockEnvs.ofScriptEnvs env' env
    transformBlockExpression body env

let transformListenerDefinition script (ListenerDefinition(state, name, isAtomic, body) as d) env =
    let body = transformProcedureBody script body env
    body |> R.mapIfModified d (fun body -> ListenerDefinition(state, name, isAtomic, body))

let transformProcedureDefinition script (ProcedureDefinition(state, name, parameters, isAtomic, body) as d) env =
    let body = transformProcedureBody script body env
    body |> R.mapIfModified d (fun body -> ProcedureDefinition(state, name, parameters, isAtomic, body))

let transformScript s env =
    let transformScript s env =
        match s with
        | Listener x ->
            let x = transformListenerDefinition s x env
            x |> R.mapIfModified s Listener

        | Procedure x ->
            let x = transformProcedureDefinition s x env
            x |> R.mapIfModified s Procedure

        | _ -> R.noModify s

    let (ScriptEnvs.Config { plugins = { transformScript = plugin }; maxIterationCount = maxCount }) = env
    let transformScriptRepeat = Transformer.transformRepeat (max 1 (maxCount / 8)) transformScript
    let s2 = R.compose plugin transformScriptRepeat s env
    s2 |> R.mapIfModified s updateScriptStatistics

[<Struct; RequireQualifiedAccess>]
type private ScriptsMapping<'a> = { env: 'a Tagged MemberEnvs } with
    interface R.IMapFoldListMapping<'a Tagged ScriptData, int> with
        override f.Invoke(convertedScripts, i, d) =
            let id =
                match d.script with
                | Procedure(ProcedureDefinition(name = name)) -> ProcedureName name
                | _ -> ScriptIndex i

            let env =
                f.env
                |> ScriptEnvs.ofMemberEnvs {
                    scriptId = id
                    convertedScripts = PooledBuffer.uncheckedEscape convertedScripts
                } 
            let d' =
                transformScript d.script env
                |> R.mapIfModified d (fun s -> { d with script = s })

            struct(d', i + 1)

let transformEntityScripts scripts env =
    let struct(datas, _) = R.mapFoldListV { ScriptsMapping.env = env } 0 scripts
    datas

let transformEntity entity env =
    let extension = entity.ObjectDataExtension
    let unitEntity = EntityData.mapExtension ignore entity

    let (EntityEnvs.Config { plugins = { transformEntity = f } }) = env
    R.compose f (fun entity env ->
        transformEntityScripts entity.scripts (MemberEnvs.ofEntityEnvs { self = entity } env)
        |> R.mapIfModified entity (fun scripts -> { entity with scripts = scripts })
    ) unitEntity env
    |> R.mapIfModified entity (EntityData.mapExtension (fun () -> extension))

let transformSprite parent sprite env =
    let env' = { parent = Some parent; children = [] }
    let env = EntityEnvs.ofConfigs env' env
    transformEntity sprite env

let transformStageChild parent child env =
    match child with
    | Choice2Of3 x ->
        let x = transformSprite parent x env
        x |> R.mapIfModified child Choice2Of3

    | Choice1Of3 _
    | Choice3Of3 _ as x -> R.noModify x

let transformStageChildren parent children env =
    R.mapList (fun c -> transformStageChild parent c env) children

let transformStageOnce stage env =
    let transformStageDataExtension stage env =
        let extension = stage.ObjectDataExtension
        let children = transformStageChildren stage extension.children env
        children
        |> R.mapIfModified stage (fun children ->
            stage
            |> EntityData.mapExtension (fun e -> { e with children = children })
        )

    let transformStageLocal stage env =
        let env' = {
            parent = None
            children = stage.ObjectDataExtension.children
        }
        let env = EntityEnvs.ofConfigs env' env
        transformEntity stage env

    R.compose transformStageDataExtension transformStageLocal stage env

let transformStage config stage =
    let env = config^^HUnit
    let stage = addStageStatistics stage
    let stage' = Transformer.transformRepeat config.maxIterationCount transformStageOnce stage env
    R.valueOrOld stage stage'
