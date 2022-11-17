module Scratch.Json.PartialIsomorphisms.Tests
open Scratch.Primitives
open Scratch.Json.PartialIsomorphisms.Reflected.Iso.Values
open Xunit


let invokeFuncIOV (f: FuncIOV<_,_,_>) x =
    let mutable x = x
    let mutable result = Unchecked.defaultof<_>
    if f.Invoke(&x, &result) then Some result
    else None

let nil = HUnit
let (^) x xs = { vhead = x; vtail = xs }

type Union1 =
    | C0
    | C1 of c1f1: char
    | C2 of c2f1: int * c2f2: string
    | C3 of c3f1: nativeint * c3f2: uint64 * c3f3: single

[<Fact>]
let classUnionTest() =
    let c0 = isoFromUnion0 <@ C0 @>
    invokeFuncIOV c0.forward nil =? Some C0
    invokeFuncIOV c0.reverse C0 =? Some nil
    invokeFuncIOV c0.reverse (C2(10, "alice")) =? None

    let c1 = isoFromUnion1 <@ C1 @>
    invokeFuncIOV c1.forward ('X'^nil) =? Some(C1 'X')
    invokeFuncIOV c1.reverse (C1 'Y') =? Some('Y'^nil)
    invokeFuncIOV c1.reverse (C2(10, "alice")) =? None

    let c3 = isoFromUnion3 <@ C3 @>
    invokeFuncIOV c3.forward (10n^20UL^30.f^nil) =? Some(C3(10n, 20UL, 30.f))
    invokeFuncIOV c3.reverse (C3(2n, 3UL, 4.f)) =? Some(2n^3UL^4.f^nil)
    invokeFuncIOV c3.reverse (C1 'A') =? None

[<Struct>]
type ValueUnion1 =
    | C0
    | C1 of c1f1: char
    | C2 of c2f1: int * c2f2: string
    | C3 of c3f1: nativeint * c3f2: uint64 * c3f3: single

[<Fact>]
let valueUnionTest() =
    let c0 = isoFromUnion0 <@ C0 @>
    invokeFuncIOV c0.forward nil =? Some C0
    invokeFuncIOV c0.reverse C0 =? Some nil
    invokeFuncIOV c0.reverse (C2(10, "alice")) =? None

    let c1 = isoFromUnion1 <@ C1 @>
    invokeFuncIOV c1.forward ('X'^nil) =? Some(C1 'X')
    invokeFuncIOV c1.reverse (C1 'Y') =? Some('Y'^nil)
    invokeFuncIOV c1.reverse (C2(10, "alice")) =? None

    let c3 = isoFromUnion3 <@ C3 @>
    invokeFuncIOV c3.forward (10n^20UL^30.f^nil) =? Some(C3(10n, 20UL, 30.f))
    invokeFuncIOV c3.reverse (C3(2n, 3UL, 4.f)) =? Some(2n^3UL^4.f^nil)
    invokeFuncIOV c3.reverse (C1 'A') =? None

type Record1 = {
    f1: string
    f2: int
    f3: string
}

[<Fact>]
let classRecordTest() =
    let r1 = isoFromRecord <@ fun r -> r.f1^r.f2^r.f3^HUnit @>
    invokeFuncIOV r1.forward ("alice"^10^"bob"^nil) =? Some { f1 = "alice"; f2 = 10; f3 = "bob" }

    let r1 = isoFromRecord <@ fun r -> r.f2^r.f1^r.f3^HUnit @>
    invokeFuncIOV r1.forward (10^"alice"^"bob"^nil) =? Some { f1 = "alice"; f2 = 10; f3 = "bob" }
    invokeFuncIOV r1.reverse { f1 = "charley"; f2 = 20; f3 = "dave" } =? Some(20^"charley"^"dave"^nil)

    throws <| lazy isoFromRecord <@ fun r -> r.f1^r.f2^HUnit @>
    throws <| lazy isoFromRecord <@ fun r -> r.f1^r.f2^r.f1^HUnit @>
    throws <| lazy isoFromRecord <@ fun r -> r.f1^r.f2^r.f3^r.f3^HUnit @>

[<Struct>]
type ValueRecord1 = {
    f1: string
    f2: int
    f3: string
}

[<Fact>]
let valueRecordTest() =
    let r1 = isoFromRecord <@ fun r -> r.f1^r.f2^r.f3^HUnit @>
    invokeFuncIOV r1.forward ("alice"^10^"bob"^nil) =? Some { f1 = "alice"; f2 = 10; f3 = "bob" }

    let r1 = isoFromRecord <@ fun r -> r.f2^r.f1^r.f3^HUnit @>
    invokeFuncIOV r1.forward (10^"alice"^"bob"^nil) =? Some { f1 = "alice"; f2 = 10; f3 = "bob" }
    invokeFuncIOV r1.reverse { f1 = "charley"; f2 = 20; f3 = "dave" } =? Some(20^"charley"^"dave"^nil)

    throws <| lazy isoFromRecord <@ fun r -> r.f1^r.f2^HUnit @>
    throws <| lazy isoFromRecord <@ fun r -> r.f1^r.f2^r.f1^HUnit @>
    throws <| lazy isoFromRecord <@ fun r -> r.f1^r.f2^r.f3^r.f3^HUnit @>
