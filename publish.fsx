#load "Shell.fsx"
//#r "nuget: Argu"
//open Argu
open System.IO

//type Argument =
//    | [<Mandatory; Unique>] Version of string
//    | [<Mandatory; Unique>] Key of string
//    | Test
//with
//    interface IArgParserTemplate with
//        member x.Usage =
//            match x with
//            | Version _ -> "package version."
//            | Key _ -> "nuget api key."
//            | Test -> "run of test mode."

#nowarn "0386"
[<Struct; StructuredFormatDisplay "{Display}"; NoEquality; NoComparison>]
type SensitiveString = private SensitiveString of string with
    member private x.Display = let (SensitiveString x) = x in String.replicate x.Length "*"
    override x.ToString() = x.Display
    override _.Equals _ = failwith ""
    override _.GetHashCode() = failwith ""

module SensitiveString =
    let ofString x = SensitiveString x
    let toString (SensitiveString x) = x

let args =
    //let parser = ArgumentParser.Create(programName = __SOURCE_FILE__)
    //let results = parser.ParseCommandLine(Array.tail fsi.CommandLineArgs)
    //{|
    //    test = results.Contains Test
    //    version = results.GetResult Version
    //    key = SensitiveString.ofString <| results.GetResult Key
    //|}
    let args = List.ofArray fsi.CommandLineArgs
    let find key xs =
        xs
        |> Seq.pairwise
        |> Seq.tryPick (fun (k, v) -> if k = key then Some v else None)
        |> Option.defaultWith (fun _ -> failwith $"ERROR: missing parameter: '{key}'")
    {|
        test = List.contains "--test" args
        version = find "--version" args
        key = SensitiveString.ofString <| find "--key" args
        projectUrl = find "--project-url" args
    |}
printfn $"args: %A{args}"

start "dotnet tool restore"
start "dotnet paket restore"
start "dotnet build --configuration Release src/Scratch"
let parent = "src/Scratch/bin/Release"
start $"dotnet paket pack --template src/Scratch/paket.template --version {args.version} --project-url {args.projectUrl} {parent}"

let package = Directory.EnumerateFiles(parent, "*.nupkg") |> Seq.exactlyOne
if args.test
then
    printfn $"[test mode] dotnet nuget push {package} --api-key {args.key} --source https://api.nuget.org/v3/index.json"
else
    start $"dotnet nuget push {package} --api-key {SensitiveString.toString args.key} --source https://api.nuget.org/v3/index.json"
