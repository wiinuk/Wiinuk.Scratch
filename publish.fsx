#load "Shell.fsx"
#load ".paket/load/netstandard2.0/Argu.fsx"
open System.IO
open Argu

type Argument =
    | [<Mandatory; Unique>] Version of string
    | [<Mandatory; Unique>] Key of string
    | Test
with
    interface IArgParserTemplate with
        member x.Usage =
            match x with
            | Version _ -> "package version."
            | Key _ -> "nuget api key."
            | Test -> "run of test mode."

let args =
    let parser = ArgumentParser.Create(programName = __SOURCE_FILE__)
    let results = parser.ParseCommandLine(Array.tail fsi.CommandLineArgs)
    {|
        test = results.Contains Test
        version = results.GetResult Version
        key = results.GetResult Key
    |}

let mask s n =
    let n = max 0 (min n (String.length s))
    s.[0..n-1] + String.replicate (s.Length - n) "*"

start "dotnet tool restore"
start "dotnet paket restore"
start "dotnet build --configuration Release src/Scratch"
let parent = "src/Scratch/bin/Release"
let package = Directory.EnumerateFiles(parent, "*.nupkg") |> Seq.exactlyOne
start "dotnet paket pack --template src/Scratch/paket.template %s --version %s" parent args.version
if args.test
then
    printfn "[test mode] dotnet nuget push %s --api-key %s --source https://api.nuget.org/v3/index.json" package (mask args.key 5)
else
    start "dotnet nuget push %s --api-key %s --source https://api.nuget.org/v3/index.json" package args.key
