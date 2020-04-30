module Scratch.Value.Tests
open System
open Scratch
open Xunit
open FSharp.Core.Operators


[<Fact>]
let numberPropertyTest() = qcheck <| fun x ->
    let c = Value.Number x
    c.Tag =? 0

    c.StringOrDefault =? null
    if Double.IsNaN x then
        Double.IsNaN c.NumberOrDefault =? true
    else
        c.NumberOrDefault =? x
    c.BoolOrDefault =? false

    c.IsNumber =? true
    c.IsFalse =? false
    c.IsTrue =? false
    c.IsBool =? false
    c.IsString =? false

    if Double.IsNaN x then
        match c.NumberOrNone with
        | ValueSome v -> Double.IsNaN v =? true
        | _ -> failwithf ""
    else
        c.NumberOrNone =? ValueSome x

    c.BoolOrNone =? ValueNone
    c.StringOrNone =? ValueNone

[<Fact>]
let boolPropertyTest() = qcheck <| fun x ->
    let c = Value.Bool x
    c.Tag =? 1

    c.StringOrDefault =? null
    c.NumberOrDefault =? 0.
    c.BoolOrDefault =? x

    c.IsNumber =? false
    c.IsFalse =? not x
    c.IsTrue =? x
    c.IsBool =? true
    c.IsString =? false

    c.NumberOrNone =? ValueNone
    c.BoolOrNone =? ValueSome x
    c.StringOrNone =? ValueNone

[<Fact>]
let stringPropertyTest() = qcheck <| fun x ->
    let c = Value.String x
    c.Tag =? 2

    c.StringOrDefault =? x
    c.NumberOrDefault =? 0.
    c.BoolOrDefault =? false

    c.IsNumber =? false
    c.IsFalse =? false
    c.IsTrue =? false
    c.IsBool =? false
    c.IsString =? true

    c.NumberOrNone =? ValueNone
    c.BoolOrNone =? ValueNone
    c.StringOrNone =? ValueSome x

let wrapAndCompareProperty hasNan wrap x1 x2 =
    let v1, v2 = wrap x1, wrap x2

    (v1 = v2) =? (x1 = x2)
    if v1 = v2 then hash v1 =? hash v2
    compare v1 v2 =? compare x1 x2

    // compare nan nan = 0
    // (nan = nan) = false
    if compare v1 v2 = 0 && not hasNan then v1 =? v2
    if compare v1 v2 <> 0 then (v1 = v2) =? false

[<Fact>]
let numberComparisonPropertyTest() = qcheck <| fun x1 x2 ->
    wrapAndCompareProperty true Value.Number x1 x2

[<Fact>]
let stringComparisonPropertyTest() = qcheck <| fun x1 x2 ->
    wrapAndCompareProperty false Value.String x1 x2

[<Fact>]
let boolComparisonPropertyTest() = qcheck <| fun x1 x2 ->
    wrapAndCompareProperty false Value.Bool x1 x2
