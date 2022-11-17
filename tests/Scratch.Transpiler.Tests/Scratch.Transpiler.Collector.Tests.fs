module Scratch.Transpiler.Collector.Tests
open Scratch
open Scratch.Transpiler.Tests
open Scratch.MemoryModel
open Xunit

type Point = {
    x: double
    y: double
}

[<Fact>]
let simpleTest() =
    <@
        let p = { x = 10.; y = 20. }

        out "p.x = "; outLine (string p.x)
        out "p.y = "; outLine (string p.y)

        Collector.collect()
        out "totalObjectCount = "; outLine (string Collector.Diagnostics.totalObjectCount)
    @>
    |> startAsStdSprite =? [
        "p.x = 10"
        "p.y = 20"
        "totalObjectCount = 1"
        ""
    ]

[<Fact>]
let callLetTest() =
    <@
        let outCollectorInfo() =
            Collector.collect()
            printfn "totalObjectCount = %d" Collector.Diagnostics.totalObjectCount
            printfn "totalReferenceCount = %d" Collector.Diagnostics.totalReferenceCount

        let f p1 =
            let mutable p = p1
            printfn "p.x = %f" p.x
            printfn "p.y = %f" p.y
            outCollectorInfo()

            p <- { x = 30.; y = 40. }
            printfn "p.x = %f" p.x
            printfn "p.y = %f" p.y
            outCollectorInfo()

            p

        let p = f { x = 10.; y = 20. }
        printfn "p.x = %f" p.x
        printfn "p.y = %f" p.y
        outCollectorInfo()
    @>
    |> startAsStdSprite =? [
        "p.x = 10"
        "p.y = 20"
        "totalObjectCount = 1"
        "totalReferenceCount = 2"

        "p.x = 30"
        "p.y = 40"
        "totalObjectCount = 2"
        "totalReferenceCount = 2"

        "p.x = 30"
        "p.y = 40"
        "totalObjectCount = 1"
        "totalReferenceCount = 1"

        ""
    ]

[<Struct>]
type Line = {
    pos1: Point
    pos2: Point
}

[<Fact>]
let instructTest() =
    <@
        let outCollectorInfo() =
            Collector.collect()
            printfn "totalObjectCount = %d" Collector.Diagnostics.totalObjectCount
            printfn "totalReferenceCount = %d" Collector.Diagnostics.totalReferenceCount

        let f l =
            printfn "l.pos1 = %f, %f" l.pos1.x l.pos1.y
            printfn "l.pos2 = %f, %f" l.pos2.x l.pos2.y
            let mutable l2 = l
            l2 <- { pos1 = { x = 100.; y = 200. }; pos2 = l.pos2 }
            printfn "l2.pos1 = %f, %f" l2.pos1.x l2.pos1.y
            printfn "l2.pos2 = %f, %f" l2.pos2.x l2.pos2.y
            l2.pos1.x

        let x = f { pos1 = { x = 10.; y = 20. }; pos2 = { x = 30.; y = 40. } }
        printfn "x = %f" x
        outCollectorInfo()
    @>
    |> startAsStdSprite
    =? [
        "l.pos1 = 10, 20"
        "l.pos2 = 30, 40"
        "l2.pos1 = 100, 200"
        "l2.pos2 = 30, 40"
        "x = 100"
        "totalObjectCount = 0"
        "totalReferenceCount = 0"
        ""
    ]
