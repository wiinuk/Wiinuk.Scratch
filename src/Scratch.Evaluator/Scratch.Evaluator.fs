[<AutoOpen>]
module Scratch.Evaluator.Evaluator
open System
open Scratch
open Scratch.Primitives
open Scratch.Evaluator
open Scratch.Evaluator.Blocks
open Scratch.Ast
open Scratch.Threading



let registerWhenGreenFlags state =
    let evaluateSelfWhenGreenFlags self state =
        let state = { blockState = state; args = Map.empty; isAtomic = false; callStack = []; self = self }
        let stage = state.blockState
        for ListenerDefinition(body = body) as listener in self.shared.whenGreenFlag do
            let action = evaluateBlock state body :> ThreadTask<_>
            let name = listenerName stage.config self listener
            Scheduler.registerFiberWithSelf self name DateTime.MinValue &action stage.scheduler |> ignore

    evaluateSelfWhenGreenFlags state.stage state
    for self in state.objects do
        evaluateSelfWhenGreenFlags self state

let runScheduler state =
    try
        Scheduler.run state.scheduler
    with
        | StopException(StopAll, message) ->
            eprintfn "stack trace:"
            eprintfn "%s" message

let runWhenGreenFlags state =
    registerWhenGreenFlags state
    runScheduler state

let evaluateStage withConfig data =
    let config = withConfig <| EvaluateConfig.makeDefault()
    let state = EvaluateState.ofStageData config data
    runWhenGreenFlags state
    state

let evaluateSprite withConfig data =
    let config = withConfig <| EvaluateConfig.makeDefault()
    let data = { StageData.defaultValue with ObjectDataExtension = { StageData.defaultValue.ObjectDataExtension with children = [Choice2Of3 data] } }
    let state = EvaluateState.ofStageData config data
    runWhenGreenFlags state
    state

let evaluateStageProcedure procedureName args ({ EvaluateState.stage = stage } as state) =
    let ProcedureDefinition(state = location; isAtomic = isAtomic) as proc = stage.shared.procs.[procedureName]
    let isAtomic = match isAtomic with Atomic -> true | NoAtomic -> false
    let state = { blockState = state; self = stage; args = Map.empty; isAtomic = isAtomic; callStack = [] }
    let args = checkArgs &state location proc args
    try
        let name = $"%s{stage.shared.objectName}.%s{procedureName}{args}"
        let action = evaluateProcedureCall state location proc args :> ThreadTask<_>
        Scheduler.registerFiberWithSelf stage name DateTime.MinValue &action state.blockState.scheduler |> ignore
        Scheduler.run state.blockState.scheduler
    with
    | StopException(StopAll, message) ->
        eprintfn "stack trace:"
        eprintfn "%s" message
