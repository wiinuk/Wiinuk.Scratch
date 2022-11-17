module Scratch.Transpiler.Generics.Tests
open Xunit
open Scratch
open Scratch.Operators
open Scratch.MemoryModel
open Scratch.MemoryModel.Operators
open Scratch.Reflection.Member
open Scratch.Transpiler.Tests
open Scratch.Transpiler.Environments


[<Fact>]
let genericIdTest() =
    let listTD = typedefof<_ list>
    let listOfIntT = typeof<int list>
    let listOfStringT = typeof<string list>

    typeId listOfIntT <>? typeId listOfStringT
    typeId listTD <>? typeId listOfIntT
    typeId listTD <>? typeId listOfStringT

    let lengthMD, _ = findMethod <@ List.length @>
    let lengthOfIntM = lengthMD.MakeGenericMethod typeof<int>
    let lengthOfStringM = lengthMD.MakeGenericMethod typeof<string>

    methodId lengthOfIntM <>? methodId lengthOfStringM
    methodId lengthMD <>? methodId lengthOfIntM
    methodId lengthMD <>? methodId lengthOfStringM

[<ReflectedDefinition>]
let rec length' sum = function
    | [] -> sum
    | _::xs -> length' (sum + 1) xs

[<ReflectedDefinition>]
let length xs = length' 0 xs

[<ReflectedDefinition>]
let outCollectorInfo() =
    Collector.collect()
    printfn "totalObjectCount = %d" Collector.Diagnostics.totalObjectCount
    printfn "totalReferenceCount = %d" Collector.Diagnostics.totalReferenceCount

[<Fact>]
let genericsTest() =
    <@
        printfn "%d" (length [0;1])
        printfn "%d" (length [[]; ["a"]; ["b"]])
        printfn "%d" (length [struct("a", 2); struct("b", 3); struct("c", 4); struct("d", 5)])

        outCollectorInfo()
    @>
    |> startAsStdSprite
    =? [
        "2"
        "3"
        "4"
        "totalObjectCount = 0"
        "totalReferenceCount = 0"
        ""
    ]

type Bin<'a> = Nil | Cons of 'a * Bin<'a * 'a>

[<ReflectedDefinition>]
let rec count<'a> : 'a Bin -> int = function
    | Nil -> 0
    | Cons(_, xs) -> 1 + 2 * count xs

[<ReflectedDefinition>]
let rec f1<'T> (n: int) (x: 'T): unit =
    if 0 < n then
        printfn "%d" n
        f1 (n - 1) (Some x)

[<ReflectedDefinition>]
let rec f2<'T> (x: 'T) (n: int): unit =
    if 0 < n then
        printfn "%d" n
        f2 struct(n, x) (n - 1) 

[<Fact>]
let polymorphicRecursionTest() =
    <@
        printfn "%d" <| count (Cons("a", Cons(("b", "c"), Nil)))
        outCollectorInfo()
    @>
    |> startAsStdSprite
    =? [
        "3"
        "totalObjectCount = 0"
        "totalReferenceCount = 0"
        ""
    ]

    <@
        f1 3 "A"
        outCollectorInfo()
    @>
    |> startAsStdSprite
    =? [
        "3"
        "2"
        "1"
        "totalObjectCount = 0"
        "totalReferenceCount = 0"
        ""
    ]

    try <@ f2 "a" 3 @> |> startAsStdSprite =? ["3"; "2"; "1"]
    with TranspileException(TranspileError(info = MethodGenericInstanceTooMany _)) -> ()

type IGreeter = abstract Greet: name: string -> unit

[<ReflectedDefinition>]
let greetPersons (g: 'T when 'T :> IGreeter and 'T : not struct) =
    g.Greet "Alice"
    g.Greet "Bob"

[<ReflectedDefinition>]
type HelloGreeter = | HelloGreeter with
    interface IGreeter with
        member _.Greet n = out "Hello "; outLine n
        
[<ReflectedDefinition>]
type YoGreeter = | YoGreeter with
    interface IGreeter with
        member _.Greet n = out "Yo "; outLine n

[<Fact>]
let genericDevirtualizeTest() =
    <@
        greetPersons HelloGreeter
        greetPersons YoGreeter
    @>
    |> startAsStdSprite
    =? [
        "Hello Alice"
        "Hello Bob"
        "Yo Alice"
        "Yo Bob"
        ""
    ]

[<Fact>]
let vectorSortTest() =
    <@
        let xs = Allocator.mallocVectorExtend Size.typeSize<N> 10
        Vector.set xs 0 %8
        Vector.set xs 1 %2
        Vector.set xs 2 %5
        Vector.set xs 3 %3
        Vector.set xs 4 %0
        Vector.set xs 5 %7
        Vector.set xs 6 %6
        Vector.set xs 7 %4
        Vector.set xs 8 %1
        Vector.set xs 9 %9
    
        for i in 0 .. xs->%Vector.count - 1 do
            out (Word.toString (Vector.get xs i))
        outLine ""
    
        Vector.sort Word.Compare xs
    
        for i in 0 .. xs->%Vector.count - 1 do
            out (Word.toString (Vector.get xs i))
        outLine ""
    
        Allocator.free xs
    @>
    |> startAsStdSprite
    =? [
        "8253076419"
        "0123456789"
        ""
    ]
