module Scratch.Json.PartialIsomorphisms.Iso
open System
open Scratch.Primitives


let inline error message = Error(IsoError.ofMessage message)

let forward (f: 'F when 'F :> IsoR<_,_> and 'F : not struct) x =
    // TODO: inref
    let mutable x = x
    let mutable result = Unchecked.defaultof<_>
    if f.Forward(&x, &result) then Ok result
    else error ""

let reverse (f: 'F when 'F :> IsoR<_,_> and 'F : not struct) x =
    // TODO: inref
    let mutable x = x
    let mutable result = Unchecked.defaultof<_>
    if f.Reverse(&x, &result) then Ok result
    else error ""

let box (f: IsoR<_,_>) = f

[<Struct; NoEquality; NoComparison>]
type Combine<'a,'b,'c,'f1,'f2> when 'f1 :> IsoR<'a,'b> and 'f2 :> IsoR<'b,'c> = private {
    mutable f1: 'f1
    mutable f2: 'f2
}
with
    interface IsoR<'a,'c> with
        member f.Forward(x, result) =
            let mutable x' = Unchecked.defaultof<_>
            f.f1.Forward(&x, &x') &&
            f.f2.Forward(&x', &result)

        member f.Reverse(x, result) =
            let mutable x' = Unchecked.defaultof<_>
            f.f2.Reverse(&x, &x') &&
            f.f1.Reverse(&x', &result)

let (>><<) f1 f2 = { Combine.f1 = f1; f2 = f2 }

[<Struct; NoEquality; NoComparison>]
type Choice<'a,'b,'f1,'f2> when 'f1 :> IsoR<'a,'b> and 'f2 :> IsoR<'a,'b> = private {
    mutable f1: 'f1
    mutable f2: 'f2
}
with
    interface IsoR<'a,'b> with
        member f.Forward(x, result) =
            f.f1.Forward(&x, &result) ||
            f.f2.Forward(&x, &result)

        member f.Reverse(x, result) =
            f.f2.Reverse(&x, &result) ||
            f.f1.Reverse(&x, &result)

let (<|>) iso1 iso2 = { Choice.f1 = iso1; f2 = iso2 }

[<Struct; NoEquality; NoComparison>]
type Choice<'a,'b> = {
    fs: IsoR<'a,'b> list
}
with
    interface IsoR<'a,'b> with
        member f.Forward(x, result) =
            let mutable list = f.fs
            let mutable success = false
            while
                match list with
                | [] -> false
                | f::fs ->
                    if f.Forward(&x, &result) then 
                        success <- true
                        false
                    else
                        list <- fs
                        true
               do ()
            success

        member f.Reverse(x, result) =
            let mutable list = f.fs
            let mutable success = false
            while
                match list with
                | [] -> false
                | f::fs ->
                    if f.Reverse(&x, &result) then
                        success <- true
                        false
                    else
                        list <- fs
                        true
                do ()
            success

let choice fs = { Choice.fs = fs }

[<Struct; NoEquality; NoComparison>]
type Reverse<'a,'b,'f> when 'f :> IsoR<'a,'b> = private {
    mutable f: 'f
}
with
    interface IsoR<'b,'a> with
        member f.Forward(x, result) = f.f.Reverse(&x, &result)
        member f.Reverse(x, result) = f.f.Forward(&x, &result)

type ValueToTuple1<'a> = | ValueToTuple1 with
    interface IsoR<'a,HCons<'a,HUnit>> with
        member _.Forward(x, result) = result <- { head = x; tail = HUnit }; true
        member _.Reverse(x, result) = result <- x.head; true
let valueToTuple1 = ValueToTuple1

[<Struct; NoEquality; NoComparison>]
type SingletonWith<'a> = private {
    equals: OptimizedClosures.FSharpFunc<'a,'a,bool>
    value: 'a
    errorMessage: string
}
with
    interface IsoR<HUnit,'a> with
        member f.Forward(_ref, result) = result <- f.value; true
        member f.Reverse(x, _ref) = f.equals.Invoke(x, f.value)

let singletonWith equals x = {
    SingletonWith.equals = OptimizedClosures.FSharpFunc<_,_,_>.Adapt equals
    value = x
    errorMessage = sprintf "singletonWith(%A)" x
}
let singleton x = singletonWith (=) x

// TODO:
[<Struct; NoEquality; NoComparison>]
type OfFunc<'a,'b> = { f1: Func<'a, Result<'b, IsoError>>; f2: Func<'b, Result<'a, IsoError>> } with
    interface IsoR<'a,'b> with
        member f.Forward(x, result) =
            match f.f1.Invoke x with
            | Ok x -> result <- x; true
            | Error _ -> false

        member f.Reverse(x, result) =
            match f.f2.Invoke x with
            | Ok x -> result <- x; true
            | Error _ -> false

[<Struct; NoComparison; NoEquality>]
type OfFuncIOV<'T1,'T2> = {
    forward: FuncIOV<'T1,'T2, bool>
    reverse: FuncIOV<'T2,'T1, bool>
}
with
    interface IsoR<'T1,'T2> with
        member f.Forward(x, result) = f.forward.Invoke(&x, &result)
        member f.Reverse(x, result) = f.reverse.Invoke(&x, &result)

[<Struct; NoEquality; NoComparison>]
type OfFun<'a,'b> = {
    forward: 'a -> Result<'b, IsoError>
    reverse: 'b -> Result<'a, IsoError>
}
with
    interface IsoR<'a,'b> with
        member f.Forward(x, result) =
            match f.forward x with
            | Ok x -> result <- x; true
            | _ -> false

        member f.Reverse(x, result) =
            match f.reverse x with
            | Ok x -> result <- x; true
            | _ -> false

module Values =
    type ValueToTuple1<'a> = | ValueToTuple1 with
        interface IsoR<'a,MVCons<'a,HUnit>> with
            member _.Forward(x, result) = result.vhead <- x; true
            member _.Reverse(x, result) = result <- x.vhead; true
    let valueToTuple1 = ValueToTuple1
