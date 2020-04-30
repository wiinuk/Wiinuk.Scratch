module Scratch.Package.Tests
open Xunit
open Scratch.Ast
open Scratch.Packaging


[<Fact>]
let resourceMD5Test() =
    let path = "./Stage1.sb2"
    let stageData = readSb2Project path |> Async.RunSynchronously

    let iter data =
        for c in data.costumes do
            readSb2ResourceMD5 path Image (int c.baseLayerID) |> Async.RunSynchronously =? c.baseLayerMD5

            match c.textLayerID, c.textLayerMD5 with
            | None, None -> ()
            | Some id, Some md5 -> readSb2ResourceMD5 path Image (int id) |> Async.RunSynchronously =? md5
            | _ -> failwithf ""

        for s in data.sounds do
            readSb2ResourceMD5 path Sound (int s.soundID) |> Async.RunSynchronously =? s.md5

    iter stageData

    for s in StageData.sprites stageData do
        iter s
