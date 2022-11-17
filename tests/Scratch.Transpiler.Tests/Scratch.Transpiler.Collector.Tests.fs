module Scratch.Transpiler.Collector.Tests
open Scratch
open Scratch.Transpiler.Tests
open Scratch.MemoryModel
open Xunit

type Point = {
    x: double
    y: double
}

let forceInitialize() =
    Allocator.initialized <- false
    Collector.initialized <- false

[<Fact>]
let addAndDeleteTest() =
    forceInitialize()

    let t = Allocator.mallocVectorExtend Word.size 0
    let p = Collector.newRc t 2
    Collector.addReference (NonNil p)
    Collector.deleteReference (NonNil p)

    Collector.collect()
    Collector.Diagnostics.totalReferenceCount =? 0
    Collector.Diagnostics.totalObjectCount =? 0

[<Fact>]
let modifyPattern1Test() =
    forceInitialize()
    let t = Allocator.mallocVectorExtend Word.size<N> 0

    let p1 = Collector.newRc t 2
    Collector.addReference (NonNil p1)
    Collector.addReference (NonNil p1)
    Collector.deleteReference (NonNil p1)

    let p2 = Collector.newRc t 2
    Collector.addReference (NonNil p2)
    Collector.addReference (NonNil p2)
    Collector.deleteReference (NonNil p2)
    Collector.deleteReference (NonNil p2)
    Collector.deleteReference (NonNil p1)

    Collector.collect()
    Collector.Diagnostics.totalReferenceCount =? 0
    Collector.Diagnostics.totalObjectCount =? 0

[<Fact>]
let modifyPattren2Test() =
    forceInitialize()
    let point = Allocator.mallocVectorExtend Word.size<N> 0

    let p1 = Collector.newRc point 2 // new p1
    Collector.addReference (NonNil p1) // p1.rc = 1
    Collector.addReference (NonNil p1) // p1.rc = 2
    Collector.deleteReference (NonNil p1) // p1.rc = 1

    let p2 = Collector.newRc point 2 // new p2
    Collector.addReference (NonNil p2) // p2.rc = 1
    Collector.addReference (NonNil p2) // p2.rc = 2
    Collector.deleteReference (NonNil p2) // p2.rc = 1
    Collector.deleteReference (NonNil p1) // p1.rc = 0

    Collector.addReference (NonNil p2) // p2.rc = 2
    Collector.deleteReference (NonNil p2) // p2.rc = 1

    Collector.deleteReference (NonNil p2) // p2.rc = 0

    Collector.collect()
    Collector.Diagnostics.totalReferenceCount =? 0
    Collector.Diagnostics.totalObjectCount =? 0
