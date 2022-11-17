module Scratch.IR.Exp.Tests
open Scratch.IR
open Xunit

let exp = Exp.builderWith ()

[<Fact>]
let getFreeVarsTest() =
    let newVar name = Var.newStorage name true ExpTypes.number
    let freeVars = Exp.getFreeVars >> Seq.toList

    let true' = exp.bool true
    let zero = exp.number 0.
    let a = newVar "a"
    let b = newVar "b"
    let a' = exp.var a
    let b' = exp.var b

    freeVars a' =? [a]
    freeVars (exp.newTuple [b'; a']) =? [b; a]
    freeVars (exp.newTuple [a'; b']) =? [a; b]

    freeVars (exp.let'(a, zero, a')) =? []
    freeVars (exp.let'(a, zero, b')) =? [b]

    freeVars (exp.tupleSet(a, 0, zero)) =? [a]

    freeVars (exp.if'(true', exp.let'(a, zero, a'), a')) =? [a]
