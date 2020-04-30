module Scratch.Json.Syntax.Tests
open Xunit
open Scratch.Primitives
open Scratch.Json
open Scratch.Json.PartialIsomorphisms
open Scratch.Json.PartialIsomorphisms.Iso
open Scratch.Json.PartialIsomorphisms.Reflected.Iso


type U1 = | C1 of x1: int | C2 of x2: string
type U2 = | C1 of x1: int | C2 of x2: string

module Parser =
    open Scratch.Json.Utf8
    let parse = Syntax.deserializeString Syntax.json

[<Fact>]
let tokenEqualsTest() =
    Json.tokenEquals (Parser.parse """{"x":10,"y":20}""") (Parser.parse """{ "y": 20, "x": 10 }""")

let raiseIfError = function Ok x -> x | Error error -> raise <| exn(IsoError.messageOrNull error, IsoError.expectionOrNull error)
let forward f x = forward f x |> raiseIfError
let reverse f x = reverse f x |> raiseIfError

[<Fact>]
let isoFromUnionTest() =
    let some = isoFromUnion1 <@ Some: int -> _ @> :> IsoR<_,_>
    forward some (HList.singleton 10) =? Some 10
    reverse some (Some 10) =? HList.singleton 10
    throws (lazy reverse some None)

    let ok = isoFromUnion1 <@ Ok: _ -> Result<int,string> @> :> IsoR<_,_>
    forward ok (HList.singleton 10) =? Ok 10
    reverse ok (Ok 10) =? HList.singleton 10
    throws (lazy reverse ok (Error "error"))

    let c1 = isoFromUnion1 <@ U1.C1 @> :> IsoR<_,_>
    forward c1 (HList.singleton 10) =? U1.C1 10
    reverse c1 (U1.C1 10) =? HList.singleton 10
    throws (lazy reverse c1 (U1.C2 "a"))

    let c1 = isoFromUnion1 <@ U2.C1 @> :> IsoR<_,_>
    forward c1 (HList.singleton 10) =? U2.C1 10
    reverse c1 (U2.C1 10) =? HList.singleton 10
    throws (lazy reverse c1 (U2.C2 "a"))

type Person = {
    name: string
    age: int
    books: string list
}

[<Fact>]
let isoFromRecordTest() =
    let person = isoFromRecord <@ fun x -> x.name^^x.age^^x.books^^HUnit @> :> IsoR<_,_>
    reverse person {name="a";age=10;books=["X"]} =? "a"^^10^^["X"]^^HUnit

    let ref = isoFromRecord <@ fun (x: int ref) -> x.contents^^HUnit @> :> IsoR<_,_>
    forward ref (10^^HUnit) =? { contents = 10 }
    reverse ref { contents = 10 } =? (10^^HUnit)
