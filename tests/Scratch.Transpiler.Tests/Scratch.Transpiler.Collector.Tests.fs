module Scratch.Transpiler.Collector.Tests
open Scratch
open Scratch.Transpiler.Tests
open Scratch.MemoryModel
open Xunit

type Point = {
    x: double
    y: double
}

[<Struct>]
type Line = {
    pos1: Point
    pos2: Point
}

[<Fact>]
let instructTest() =
    <@
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
    @>
    |> startAsStdSprite
    =? [
        "l.pos1 = 10, 20"
        "l.pos2 = 30, 40"
        "l2.pos1 = 100, 200"
        "l2.pos2 = 30, 40"
        "x = 100"
        ""
    ]
