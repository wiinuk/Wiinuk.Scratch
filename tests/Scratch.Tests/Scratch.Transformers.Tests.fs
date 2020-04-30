module Scratch.Transformers.Tests
open Xunit
module R = Scratch.Transformers.TransformResult


[<Fact>]
let mapListTest() =
    R.mapList (fun _ -> R.noModify 999) [0..4] =? R.noModify [0..4]
    R.mapList (function 1 -> R.modified 100 | x -> R.noModify x) [0..4] =? R.modified [0;100;2;3;4]
    R.mapList (function 1 -> R.modified 100 | 3 -> R.modified 300 | x -> R.noModify x) [0..4] =? R.modified [0;100;2;300;4]
    R.mapList (function 1 -> R.removed 100 | x -> R.noModify x) [0..4] =? R.modified [0;2;3;4]

    R.mapList (function 1 -> R.modified 100 |> R.withIsSkip true | 3 -> R.modified 300 | x -> R.noModify x) [0..4] =? (R.modified [0;100;2;3;4] |> R.withIsSkip true)
    R.mapList (function 1 -> R.skip 100 | 3 -> R.modified 300 | x -> R.noModify x) [0..4] =? R.skip [0..4]
    R.mapList (function 1 -> R.modified 100 | 3 -> R.modified 300 |> R.withIsSkip true | x -> R.noModify x) [0..4] =? (R.modified [0;100;2;300;4] |> R.withIsSkip true)
    R.mapList (function 1 -> R.modified 100 | 3 -> R.skip 300 | x -> R.noModify x) [0..4] =? (R.modified [0;100;2;3;4] |> R.withIsSkip true)

[<Fact>]
let composeTest() =
    R.compose (fun x _ -> R.modified (x + 1)) (fun x _ -> R.noModify x) 10 () =? R.modified 11
