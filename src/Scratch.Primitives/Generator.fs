module Scratch.Primitives.Generator
open System.Runtime.InteropServices


let getFiber (g: 'G inref when 'G :> generator<_,_,_>) = g.GetFiber()
let run (env: _ byref) (g: _ inref) =
    let f = getFiber &g
    let mutable result = Unchecked.defaultof<_>
    while
        match f.Next &env with
        | Yield() -> (); true
        | Return x -> result <- x; false
        do ()
    result

[<Struct; NoEquality; NoComparison>]
type Delay<'F,'T,'E,'R> when 'F :> fiber<'T,'E,'R> = internal {
    f: unit -> 'F
}
with
    interface generator<'F,'T,'E,'R> with
        member g.GetFiber() = g.f()

let delay f = { Delay.f = f }
