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
            let mutable l2 = l
            l2 <- { pos1 = { x = 100.; y = 200. }; pos2 = l.pos2 }
            l2.pos1.x

        let x = f { pos1 = { x = 10.; y = 20. }; pos2 = { x = 30.; y = 40. } }
        out (string x)
    @>
    |> startAsStdSprite
    =? [
        "100"
    ]
