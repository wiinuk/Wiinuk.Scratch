module Scratch.Primitives.Fiber
open System.Runtime.InteropServices
open System.Collections.Generic


let next (g: 'F byref when 'F :> fiber<_,_,_>) (env: _ byref) = g.Next &env
let nextClass (g: 'F when 'F : not struct) (env: _ byref) =
    let mutable g = g
    next &g &env

[<Struct; NoEquality; NoComparison; StructLayout(LayoutKind.Auto)>]
type Result<'T,'E,'R> = internal { x: 'R } with
    interface fiber<'T,'E,'R> with
        override f.Next _ = Return f.x

let result x = { Result.x = x }

[<Struct; NoEquality; NoComparison>]
type Zero<'T,'E> = internal | Zero with
    interface fiber<'T,'E,unit> with
        override _.Next _ = Return()

let zero = Zero

[<Struct; NoEquality; NoComparison; StructLayout(LayoutKind.Auto)>]
type Singleton<'T,'E> = private {
    mutable state: bool // false: init, true: end
    x: 'T
}
with
    interface fiber<'T,'E,unit> with
        override x.Next _ =
            match x.state with
            | false -> x.state <- true; Yield x.x
            | true -> Return()

let singleton x = { Singleton.state = false; x = x }

[<Struct; NoEquality; NoComparison; StructLayout(LayoutKind.Auto)>]
type Bind<'F1,'R1,'F2,'R2,'B,'T,'E> when
    'F1 :> fiber<'T,'E,'R1> and
    'F2 :> fiber<'T,'E,'R2> and
    'B :> func<'R1,'F2> = internal {

    // type BindState =
    //     | Init = 0
    //     | Yield1 = 1
    //     | Yield2 = 2 of 'F2
    //     | End = 3 of 'R2
    mutable state: byte
    mutable state2v1: 'F2
    mutable state3v1: 'R2

    mutable fiber1: 'F1
    mutable binder: 'B
}
with
    member private f.Yield2(e: _ byref) =
        match f.state2v1.Next &e with
        | Yield x -> Yield x
        | Return x ->
            f.state <- 3uy
            f.state3v1 <- x
            Return x

    member private f.Yield1(e: _ byref) =
        match f.fiber1.Next &e with
        | Yield x -> Yield x
        | Return x ->
            f.state2v1 <- f.binder.Invoke x
            f.state <- 2uy
            f.Yield2 &e

    member private f.Init(e: _ byref) =
        f.state <- 1uy
        f.Yield1 &e

    interface fiber<'T,'E,'R2> with
        override f.Next e =
            match f.state with
            | 0uy -> f.Init &e
            | 1uy -> f.Yield1 &e
            | 2uy -> f.Yield2 &e
            | _ -> Return f.state3v1
let bind (binder: _ inref) (fiber: _ inref) = {
    state = 0uy
    state2v1 = Unchecked.defaultof<_>
    state3v1 = Unchecked.defaultof<_>

    fiber1 = fiber
    binder = binder
}

[<Struct; NoEquality; NoComparison; StructLayout(LayoutKind.Auto)>]
type Combine<'F1,'F2,'T,'E,'R> when
    'F1 :> fiber<'T,'E,unit> and
    'F2 :> fiber<'T,'E,'R> = internal {

    // type BindState =
    //     | Init = 0
    //     | Yield1 = 1
    //     | Yield2 = 2
    //     | End = 3 of 'R2
    mutable state: byte
    mutable state3v1: 'R

    mutable fiber1: 'F1
    mutable fiber2: 'F2
}
with
    member private f.Yield2(e: _ byref) =
        match f.fiber2.Next &e with
        | Yield x -> Yield x
        | Return x ->
            f.state <- 3uy
            f.state3v1 <- x
            Return x

    member private f.Yield1(e: _ byref) =
        match f.fiber1.Next &e with
        | Yield x -> Yield x
        | Return() ->
            f.state <- 2uy
            f.Yield2 &e

    member private f.Init(e: _ byref) =
        f.state <- 1uy
        f.Yield1 &e

    interface fiber<'T,'E,'R> with
        override f.Next e =
            match f.state with
            | 0uy -> f.Init &e
            | 1uy -> f.Yield1 &e
            | 2uy -> f.Yield2 &e
            | _ -> Return f.state3v1

[<Struct; NoEquality; NoComparison>]
type Delay<'D,'F,'T,'E,'R> when 'D :> func<unit,'F> and 'F :> fiber<'T,'E,'R> = {
    mutable state: bool
    mutable state1v1: 'F

    mutable f: 'D
}
with
    interface fiber<'T,'E,'R> with
        override f.Next e =
            match f.state with
            | false ->
                f.state1v1 <- f.f.Invoke()
                f.state <- true
                f.state1v1.Next &e

            | true -> f.state1v1.Next &e

let delay (f: _ inref) = {
    Delay.state = false
    state1v1 = Unchecked.defaultof<_>
    f = f
}

let combine (fiber1: _ inref) (continuation: _ inref) = {
    Combine.state = 0uy
    state3v1 = Unchecked.defaultof<_>

    fiber1 = fiber1
    fiber2 = delay &continuation
}

[<Struct; NoEquality; NoComparison; StructLayout(LayoutKind.Auto)>]
type For<'S,'U,'A,'F,'E,'T> when 'S :> 'U seq and 'A :> IFunc<'U,'F> and 'F :> fiber<'T,'E,unit> = internal {
    // type ForState =
    //     | Init = 0
    //     | Yield1 = 1 of 'E IEnumerator * 'F
    //     | End = 2
    mutable state: byte
    mutable state1v1: 'U System.Collections.Generic.IEnumerator
    mutable state1v2: 'F

    mutable action: 'A
    mutable xs: 'S
}
with
    member private f.MoveNext(e: _ IEnumerator, env: _ byref) =
        let mutable result = Return()
        while
            if e.MoveNext() then
                f.state1v2 <- f.action.Invoke(e.Current)
                match f.state1v2.Next &env with
                | Yield x ->
                    f.state <- 1uy
                    f.state1v1 <- e
                    result <- Yield x
                    false

                | Return() -> true
            else
                e.Dispose()
                f.state1v1 <- null
                f.state <- 2uy
                false
            do ()
        result

    interface fiber<'T,'E,unit> with
        override f.Next e =
            match f.state with
            | 0uy -> f.MoveNext(f.xs.GetEnumerator(), &e)
            | 1uy ->
                match f.state1v2.Next &e with
                | Yield x -> Yield x
                | Return() -> f.MoveNext(f.state1v1, &e)
            | _ ->
                Return()

let for' action (xs: 'S when 'S : not struct) = {
    For.state = 0uy
    state1v1 = null
    state1v2 = Unchecked.defaultof<_>

    action = Func.ofFun action
    xs = xs
}

[<Struct; NoEquality; NoComparison; StructLayout(LayoutKind.Auto)>]
type While<'Test,'Body,'F,'T,'E> when 'Test :> func<unit, bool> and 'Body :> func<unit,'F> and 'F :> fiber<'T,'E, unit> = {
    // type WhileState<'F> when 'F :> fiber<'T,unit> =
    //     | Init = 0
    //     | FiberYield = 1 of 'F
    //     | End = 2
    mutable state: byte
    mutable state1v1: 'F

    mutable test: 'Test
    mutable body: 'Body
}
with
    member private f.DoTest(e: _ byref) =
        let mutable result = Return()
        while
            if f.test.Invoke() then
                f.state1v1 <- f.body.Invoke()
                match f.state1v1.Next &e with
                | Yield x ->
                    f.state <- 1uy
                    result <- Yield x
                    false

                | Return() -> true
            else
                f.state <- 2uy
                false
            do ()
        result

    interface fiber<'T,'E, unit> with
        override f.Next e =
            match f.state with
            | 0uy -> f.DoTest &e
            | 1uy ->
                match f.state1v1.Next &e with
                | Yield x -> Yield x
                | Return() -> f.DoTest &e
            | _ -> Return()
let while' (test: _ inref) (body: _ inref) = {
    While.state = 0uy
    state1v1 = Unchecked.defaultof<_>
    test = test
    body = body
}
[<Struct; NoEquality; NoComparison; StructLayout(LayoutKind.Auto)>]
type TryWith<'Body,'F1,'F2,'Handler,'T,'E,'R> when
    'Body :> func<unit,'F1> and
    'F1 :> fiber<'T,'E,'R> and
    'F2 :> fiber<'T,'E,'R> and
    'Handler :> func<exn,'F2> = {

    // type TryWithState =
    //     | Init = 0
    //     | TryFiber = 1 of 'F1
    //     | HandlerFiber = 2 of 'F2
    //     | End = 3 of 'R
    mutable state: byte
    mutable state1v1: 'F1
    mutable state2v1: 'F2
    mutable state3v1: 'R

    mutable body: 'Body
    mutable handler: 'Handler
}
with
    member private f.HandlerFiber(e: _ byref) =
        match f.state2v1.Next &e with
        | Yield x -> Yield x
        | Return x ->
            f.state <- 3uy
            f.state3v1 <- x
            Return x

    member private f.TryFiber(e: _ byref) =
        try
            match f.state1v1.Next &e with
            | Yield x -> Yield x
            | Return x ->
                f.state <- 3uy
                f.state3v1 <- x
                Return x
        with
        | exn ->
            f.state2v1 <- f.handler.Invoke(exn)
            f.state <- 2uy
            f.HandlerFiber &e

    interface fiber<'T,'E,'R> with
        override f.Next e =
            match f.state with
            | 0uy ->
                f.state1v1 <- f.body.Invoke()
                f.state <- 1uy
                f.TryFiber &e
            | 1uy -> f.TryFiber &e
            | 2uy -> f.HandlerFiber &e
            | _ -> Return f.state3v1
let tryWith (body: _ inref) (handler: _ inref) = {
    TryWith.state = 0uy
    state1v1 = Unchecked.defaultof<_>
    state2v1 = Unchecked.defaultof<_>
    state3v1 = Unchecked.defaultof<_>

    body = body
    handler = handler
}

[<Struct; NoEquality; NoComparison; StructLayout(LayoutKind.Auto)>]
type ChoiceOf2<'F1,'F2,'T,'E,'R> when 'F1 :> fiber<'T,'E,'R> and 'F2 :> fiber<'T,'E,'R> = {
    n: byte
    mutable f1: 'F1
    mutable f2: 'F2
}
with
    interface fiber<'T,'E,'R> with
        member f.Next e =
            match f.n with
            | 1uy -> f.f1.Next &e
            | _ -> f.f2.Next &e

let choice1 f1 = { ChoiceOf2.n = 1uy; f1 = f1; f2 = Unchecked.defaultof<_> }
let choice2 f2 = { ChoiceOf2.n = 2uy; f1 = Unchecked.defaultof<_>; f2 = f2 }

[<NoEquality; NoComparison>]
type Box<'F,'T,'E,'R> when 'F :> fiber<'T,'E,'R> and 'F : struct = internal { mutable f: 'F } with
    interface fiber<'T,'E,'R> with
        member f.Next e = f.f.Next &e

let box (f: _ inref) = { Box.f = f }

[<Struct; NoEquality; NoComparison>]
type Environment<'T,'E> = internal | Environment with
    interface fiber<'T,'E,'E> with
        member _.Next e = Return e

let environment = Environment

[<Struct; NoEquality; NoComparison>]
type Map<'Mapping,'F,'T1,'E,'R,'T2> when 'Mapping :> func<'T1,'T2> and 'F :> fiber<'T1,'E,'R> = {
    mutable f: 'F
    mutable mapping: 'Mapping
}
with
    interface fiber<'T2,'E,'R> with
        member f.Next e =
            match f.f.Next &e with
            | Return x -> Return x
            | Yield y -> Yield(f.mapping.Invoke y)
let map (mapping: _ inref) (f: _ inref) = {
    f = f
    mapping = mapping
}
