namespace Scratch

module Operators =
    let inline (~%) (x: ^a): ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)
    let inline ``#if-fsharp`` x = x; ()
    let inline ``#if-else-fsharp`` x f = if true then x else f()

[<AutoOpen>]
module ConsoleOperations =
    open Operators

    [<ReflectedDefinition>]
    let output = defineList []

    [<ReflectedDefinition>]
    let out v =
        ``#if-fsharp`` (printf "%s" v)
        if SList.length output = 0 then
            SList.push output v
        else
            SList.set output (SList.length output) (SList.get output (SList.length output) + v)

    [<ReflectedDefinition>]
    let outLine v =
        out v
        SList.push output ""
        ``#if-fsharp`` (printfn "")
