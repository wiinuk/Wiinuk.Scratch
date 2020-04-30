namespace Scratch.Executor
open Scratch
open Scratch.Primitives
open Scratch.Threading
open Scratch.Executor
open Scratch.Executor.Blocks


[<Struct>]
type ThreadState<'a> = {
    self: 'a EntityState
    stage: StageState<'a>

    mutable position: Instruction index
    mutable data: Value Stack
    mutable flames: Flame Stack
    mutable currentFlame: Flame

    /// allow null
    mutable childThreadIds: int ResizeArray

    glideSecs: InstructionFiber<'a>
    ask: InstructionFiber<'a>
}

and InstructionFiber<'a> = IFiber<Flow, ThreadState<'a>, unit>

type [<Struct>] GlideSecsFiber<'a,'task> = private {
    mutable state: int
    /// state1field1
    mutable g: GlideSecs
    scheduler: Scheduler<unit,'task>
}
with
    member private f.State0(state: _ byref) =
        let data = &state.data
        let y2 = Stack.pop &data |> Value.toNumber
        let x2 = Stack.pop &data |> Value.toNumber
        let seconds = Stack.pop &data |> Value.toNumber
    
        let x1 = state.self.spriteDrawingData.x
        let y1 = state.self.spriteDrawingData.y

        f.state <- 1
        f.g <- {
            t1 = Scheduler.now f.scheduler
            duration = seconds
            x1 = x1
            y1 = y1
            xDelta = x2 - x1
            yDelta = y2 - y1
        }
        f.State1 &state

    member private f.State1(state: _ byref) =
        let t = Scheduler.now f.scheduler - f.g.t1
        let normalizedTime = min 1. (t.TotalSeconds / f.g.duration)
        moveTo state.self (f.g.x1 + normalizedTime * f.g.xDelta, f.g.y1 + normalizedTime * f.g.yDelta)

        if normalizedTime < 1. then
            Yield ThreadYieldForce
        else
            f <- Unchecked.defaultof<_>
            Return()

    interface InstructionFiber<'a> with
        member f.Next state =
            match f.state with
            | 0 -> f.State0 &state
            | _ -> f.State1 &state

type [<Struct>] AskFiber<'a,'View> when 'View :> IStageView<'a EntityState> and 'View : struct = private {
    mutable state: int
    mutable state1state2_question: string
    mutable state1state2state3_askVersion: AskVersion
    mutable view: 'View
}
with
    member private f.State0(state: _ byref) =
        let question = Stack.pop &state.data |> Value.toString
    
        let stage = state.stage
        let Version v as askVersion = stage.nextAskVersion
        stage.nextAskVersion <- Version(v + 1)
        f.state <- 1
        f.state1state2_question <- question
        f.state1state2state3_askVersion <- askVersion
        f.State1 &state

    member private f.State1(state: _ byref) =

        // すでに表示されている Ask が終わるまで待つ
        if state.stage.currentAskVersion < f.state1state2state3_askVersion then
            Yield ThreadYieldForce
        else
            f.state <- 2
            f.State2 &state

    member private f.State2(state: _ byref) =
        ask &f.view state.stage state.self f.state1state2_question
        f.state <- 3
        f.State3(&state)

    member private f.State3(state: _ byref) =

        // 自分の Ask が終わるまで待つ
        if state.stage.currentAskVersion = f.state1state2state3_askVersion then
            Yield ThreadYieldForce
        else
            f <- Unchecked.defaultof<_>
            Return()

    interface InstructionFiber<'a> with
        member f.Next state =
            match f.state with
            | 0 -> f.State0 &state
            | 1 -> f.State1 &state
            | 2 -> f.State2 &state
            | _ -> f.State3 &state

module GlideSecsFiber =
    let make scheduler = {
        GlideSecsFiber.state = 0
        g = Unchecked.defaultof<_>
        scheduler = scheduler
    }

module AskFiber =
    let make (view: _ inref) = {
        AskFiber.state = 0
        state1state2_question = null
        state1state2state3_askVersion = Version 0
        view = view
    }

type IExecutionObserver =
    abstract OnEnterExecute: state: 'a ThreadState byref -> unit
    abstract OnPreExecute: state: 'a ThreadState byref -> unit
    abstract OnLeaveExecute: state: 'a ThreadState byref * result: FiberResult<Flow, unit> byref -> unit

module ExecutionObserver =
    let preExecute (x: 'T byref when 'T :> IExecutionObserver and 'T : struct) (state: _ byref) = x.OnPreExecute &state

    [<Struct>]
    type Ignore = | Ignore with
        interface IExecutionObserver with
            member _.OnEnterExecute _ref = ()
            member _.OnPreExecute _ref = ()
            member _.OnLeaveExecute(_ref1, _ref2) = ()
    let ignore = Ignore

    [<Struct>]
    type Poly<'Observer> when 'Observer :> IExecutionObserver = private { mutable observer: 'Observer } with
        interface IExecutionObserver with
            member x.OnEnterExecute state = x.observer.OnEnterExecute &state
            member x.OnPreExecute state = x.observer.OnPreExecute &state
            member x.OnLeaveExecute(state, result) = x.observer.OnLeaveExecute(&state, &result)

    let poly (x: 'Observer when 'Observer :> IExecutionObserver and 'Observer : not struct) = { Poly.observer = x }
