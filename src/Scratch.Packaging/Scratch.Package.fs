[<AutoOpen>]
module Scratch.Package
open System
open System.IO
open System.IO.Compression
open Scratch.Json.Utf8
open Scratch.Packaging
open Scratch.Primitives
open Scratch.Serialization


let readSb2ResourceMD5 sb2Path resourceType resourceId = async {
    use zip = ZipFile.OpenRead sb2Path
    let part =
        match resourceType with
        | Image -> imageExtensions
        | Sound -> soundExtensions
        |> Seq.tryPick (fun ext ->
            sprintf "%d%s" resourceId ext
            |> zip.GetEntry
            |> Option.ofObj
            |> Option.map (fun part -> part, ext)
        )

    match part with
    | None -> return failwithf "EntryNotFound(%A, %d)" resourceType resourceId
    | Some(part, ext) ->

    use binary = part.Open()
    let! md5 = streamMD5String binary
    return md5 + ext
}
let readSprite2Project sprite2Path = usingReadZipEntry sprite2Path "sprite.json" <| fun stream ->
    Syntax.deserializeStream (Sb2.Syntax.spriteData HasDefault.option) stream

let readSb2Project sb2Path = usingReadZipEntry sb2Path "project.json" <| fun stream ->
    Syntax.deserializeStream (Sb2.Syntax.stageData HasDefault.option) stream

let readSb2Package sb2Path = async {
    let! stage = readSb2Project sb2Path
    use zip = ZipFile.OpenRead sb2Path

    let tryNameToInt n =
        match Path.GetFileNameWithoutExtension n |> Int32.TryParse with
        | false, _ -> None
        | true, n -> Some n

    let entries =
        zip.Entries
        |> Seq.sortBy (fun e -> tryNameToInt e.Name)
        |> Seq.map (fun e -> (sb2Path, e.FullName, Path.GetExtension e.Name))
        |> Seq.cache

    let filterOfExt exts entries =
        entries
        |> Seq.filter (fun (_, _, ext) -> Set.contains ext exts)
        |> Seq.map ResourceInZip
        |> Seq.toList

    return {
        PackageData.project = stage
        images = entries |> filterOfExt imageExtensions
        sounds = entries |> filterOfExt soundExtensions
    }
}

let fixSb2Package packageData = fixPackage packageData

let writeSb2Package sb2Path packageData = async {
    let! packageData = fixPackage packageData
    return! checkOrRaisePackage packageData <| writeFixedSb2Package sb2Path packageData
}
