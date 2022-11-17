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
