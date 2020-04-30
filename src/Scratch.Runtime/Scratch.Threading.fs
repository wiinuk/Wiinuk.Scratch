namespace Scratch.Threading
open NonStructuralComparison
open System
open System.Diagnostics
open Scratch.Primitives
open System.Runtime.CompilerServices


type Flow =
    /// 次のスレッドに実行を譲る。
    /// atomic 内では ThreadYieldImmediate に変わる
    | ThreadYield
    /// 次のスレッドに実行を譲らず、現在のスレッドの実行を継続する
    | ThreadYieldImmediate
    /// atomic 内でも、必ず次のスレッドに実行を譲る
    | ThreadYieldForce

    /// 指定した時間経過後に現在のスレッドを再開する
    | Sleep of TimeSpan
    /// 現在のスレッドを停止する
    | Abort

[<Struct>]
type ThreadInfo = {
    id: int
    name: string
}

[<NoComparison; NoEquality>]
type Thread<'Self,'Fiber> = {
    id: int
    name: string
    mutable sleepEnd: DateTime
    mutable runningTask: 'Fiber
    self: 'Self
}
type ThreadTask<'Self,'Result> = fiber<Flow, struct(ThreadInfo * 'Self), 'Result>
type ThreadTask<'Self> = ThreadTask<'Self, unit>
type task<'Result> = ThreadTask<unit, 'Result>
type task = unit task
type Thread<'Self> = Thread<'Self, ThreadTask<'Self>>

type DeterministicTimeConfig = {
    processSpan: TimeSpan
    flameSpan: TimeSpan
}

type DeterministicTime = {
    processSpan: TimeSpan
    flameSpan: TimeSpan
    mutable current: DateTime
}
type FlameTime = {
    flameSpan: TimeSpan
    mutable nextFlameTime: DateTime
}

[<Struct>]
type FlameTimeMode<'a> =
    | Turbo
    | Limited of 'a

type TimeMode<'a,'b> =
    | RealTime of realTime: 'a
    | DeterministicTime of deterministicTime: 'b

[<Struct>]
type SchedulerConfig = {
    /// None = System.DateTime.Now
    startTime: DateTime option
    flame: TimeSpan FlameTimeMode
    /// DeterministicTime = tick span
    deterministic: TimeMode<unit, DeterministicTimeConfig>
}
module SchedulerConfig =
    let defaultConfig = {
        startTime = None
        flame = Limited <| TimeSpan.FromSeconds(1. / 30.)
        deterministic = RealTime()
    }

[<Struct>]
[<NoComparison; NoEquality>]
type Colony<'T> = private Colony of 'T voption ResizeArray
module Colony =
    let make() = Colony <| ResizeArray()
    let addLast (Colony xs) x = xs.Add(ValueSome x)

    let clear (Colony xs) = xs.Clear()
    let count (Colony xs) = xs.Count
    let tryItem i (Colony xs) = xs.[i]
    let ignoreAt i (Colony xs) = xs.[i] <- ValueNone

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let ignoreWith predicate (Colony xs) =
        let mutable i = 0
        while i < xs.Count do
            match xs.[i] with
            | ValueSome x when predicate x -> xs.[i] <- ValueNone
            | _ -> ()
            i <- i + 1

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let exists predicate (Colony xs) =
        let mutable result = false
        let mutable i = 0
        while
            begin
                if i < xs.Count then
                    match xs.[i] with
                    | ValueSome x when predicate x ->
                        result <- true
                        false
                    | _ -> true
                else false
            end
            do i <- i + 1
        result

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let iter action (Colony xs) =
        let mutable i = 0
        while i < xs.Count do
            match xs.[i] with
            | ValueSome x -> action x
            | _ -> ()
            i <- i + 1

    let rec private shrinkAux i skipCount (Colony xs) =
        if i < xs.Count then
            match xs.[i] with
            | ValueNone -> shrinkAux (i + 1) (skipCount + 1) (Colony xs)
            | x ->
                if skipCount <> 0 then
                    xs.[i] <- ValueNone
                    xs.[i - skipCount] <- x
                shrinkAux (i + 1) skipCount (Colony xs)
        else
             xs.RemoveRange(xs.Count - skipCount, skipCount)

    [<NoGcAllocation(AllocatableTypes = [|typeof<exn>|])>]
    let shrink xs = shrinkAux 0 0 xs

[<NoComparison; NoEquality>]
type Scheduler<'Self,'Fiber> = {
    threads: Thread<'Self,'Fiber> Colony
    mutable nextThreadId: int

    mutable timerStart: DateTime
    timerEpoch: DateTime
    flameTime: FlameTime FlameTimeMode
    time: TimeMode<DateTime * Stopwatch, DeterministicTime>
}
type Scheduler<'Self> = Scheduler<'Self, ThreadTask<'Self>>

module Scheduler =

    // // 決定論的時間
    // { flameTime = 1s; processSpan = 0.3s }
    // { current = 0.0s; nextFlameTime = 1s }
    //
    // // begin step
    // now(s) = 0.0s
    // { current = 0.3s; nextFlameTime = 1s }
    // now(s) = 0.3s
    // { current = 0.6s; nextFlameTime = 1s }
    // now(s) = 0.6s
    // { current = 0.9s; nextFlameTime = 1s }
    // now(s) = 0.9s
    // { current = 0.9s; nextFlameTime = 1s }
    // now(s) = 0.9s
    // { current = 0.9s; nextFlameTime = 1s }
    // // end step
    // { current = 1.0s; nextFlameTime = 2s }
    //
    // // begin step
    // now(s) = 1.0s
    // { current = 1.3s ; nextFlameTime = 2s }
    // ...
    let now scheduler =
        match scheduler.time with
        | RealTime(t, w) -> t + w.Elapsed
        | DeterministicTime({ current = current; processSpan = processSpan } as time) ->
            match scheduler.flameTime with
            | Turbo -> time.current <- current + processSpan
            | Limited flameTime ->
                if current + processSpan < flameTime.nextFlameTime then
                    time.current <- current + processSpan
            current

    let nextFlame scheduler =
        match scheduler.time with
        | RealTime _ ->
            match scheduler.flameTime with
            | Turbo -> ()
            | Limited flameTime ->
                let now = now scheduler
                let sleepTime = flameTime.nextFlameTime - now
                flameTime.nextFlameTime <- now + flameTime.flameSpan
                if TimeSpan.Zero < sleepTime then
                    System.Threading.Thread.Sleep sleepTime

        | DeterministicTime time ->
            match scheduler.flameTime with
            | Turbo -> time.current <- time.current + time.flameSpan
            | Limited flameTime ->
                time.current <- flameTime.nextFlameTime
                flameTime.nextFlameTime <- flameTime.nextFlameTime + flameTime.flameSpan

    let private stepThreadOfFlow thread scheduler =
        let task: #IFiber<_,_,_> byref = &thread.runningTask
        let mutable info = struct({ id = thread.id; name = thread.name }, thread.self)
        let mutable running = true
        while
            match task.Next &info with
            | Yield y ->
                match y with
                | ThreadYield
                | ThreadYieldForce ->
                    false

                | ThreadYieldImmediate -> true

                | Sleep s ->
                    thread.sleepEnd <- now scheduler + s
                    false

                | Abort ->
                    running <- false
                    false

            | Return() ->
                running <- false
                false
            do ()
        running

    let rec private stepThreads i ({ threads = threads } as scheduler) =
        if Colony.count threads <= i then () else

        match Colony.tryItem i threads with
        | ValueNone -> stepThreads (i + 1) scheduler
        | ValueSome thread ->

        let now' = now scheduler
        if now' <= thread.sleepEnd then
            stepThreads (i + 1) scheduler
        else
            let running = stepThreadOfFlow thread scheduler
            if not running then Colony.ignoreAt i threads
            stepThreads (i + 1) scheduler

    let step ({ threads = threads } as scheduler) =
        stepThreads 0 scheduler
        Colony.shrink threads
        nextFlame scheduler

    let run scheduler =
        while Colony.count scheduler.threads <> 0 do
            step scheduler

    let registerFiberWithSelf (self: 'Self when 'Self : not struct) name sleepEnd (task: _ inref) scheduler =
        let thread = {
            id = scheduler.nextThreadId
            name = name
            sleepEnd = sleepEnd
            runningTask = task
            self = self
        }
        Colony.addLast scheduler.threads thread
        scheduler.nextThreadId <- scheduler.nextThreadId + 1
        thread

    let registerFiberWith name sleepEnd (task: _ inref) scheduler =
        registerFiberWithSelf () name sleepEnd &task scheduler

    let registerFiber name (task: _ inref) scheduler =
        registerFiberWith name DateTime.MinValue &task scheduler

    let make config =
        let startTime = config.startTime |> Option.defaultValue DateTime.Now
        {
            threads = Colony.make()
            nextThreadId = 1

            timerStart = startTime

            timerEpoch = startTime
            flameTime =
                match config.flame with
                | Turbo -> Turbo
                | Limited flameSpan -> Limited {
                    flameSpan = flameSpan
                    nextFlameTime = startTime + flameSpan
                }

            time =
                match config.deterministic with
                | RealTime() -> RealTime(startTime, Stopwatch.StartNew())
                | DeterministicTime time -> DeterministicTime {
                    current = startTime
                    flameSpan = time.flameSpan
                    processSpan = time.processSpan
                }
        }

    let reset scheduler =
        Colony.clear scheduler.threads
        scheduler.nextThreadId <- 1
        scheduler.timerStart <- scheduler.timerEpoch

[<Struct>]
[<NoComparison; NoEquality>]
type Atomic<'Y,'E,'R> = AsAtomic of fiber<'Y,'E,'R> with
    interface ISyntaxInfrastructure

module Atomic =
    type private AtomicFlow = | AtomicFlow
    with
        interface IFunc<Flow, Flow> with
            member _.Invoke f =
                match f with
                | ThreadYield -> ThreadYieldImmediate
                | ThreadYieldForce
                | ThreadYieldImmediate
                | Sleep _
                | Abort as x -> x

    let doAtomic (f: _ inref) =
        let m = AtomicFlow
        AsAtomic(Fiber.map &m &f)
