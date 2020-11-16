#load "../../tests/analyzers/AssemblyAnalyzer.fsx"
#r "nuget: Argu"
open System.IO
open System.Text
open System.Text.RegularExpressions
open Argu


type AnalyzeFromMsBuildConfig = {
    outputPath: string
    references: string list
}
let analyzeFromMsBuild { outputPath = outputPath; references = references } =
    let searchDirectories =
        references
        |> Seq.map Path.GetDirectoryName
        |> Seq.toList

    Directory.EnumerateFiles outputPath
    |> Seq.filter (fun p -> Regex.IsMatch(input = Path.GetFileName p, pattern = @"^Scratch.*\.dll$"))
    |> AssemblyAnalyzer.varidateAssemblies (fun c -> { c with searchDirectories = c.searchDirectories @ searchDirectories })

type PostBuildArg =
    | [<Unique; MainCommand; Hidden>] ScriptFileName of string
    | Command_File of path: string
    | [<Unique>] Output_Path of path: string
    | [<AltCommandLine "-r">] Reference_Path of path: string
with
    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | ScriptFileName _ -> ""
            | Command_File _ -> "path to file that recorded long command line."
            | Output_Path _ -> "output directory path."
            | Reference_Path _ -> "referenced assembly path."

let splitChars = [|' '; '='|]
let split (line: string) =
    let index = line.IndexOfAny splitChars
    if index < 0 then [line] else

    let s1 = line.Substring(0, index)
    let p = index + 1
    if p = line.Length then [s1] else

    let s2 = line.Substring(p, line.Length - p)
    [s1; s2]

let parser = ArgumentParser.Create<PostBuildArg> $"fsi {__SOURCE_FILE__}"
let rec parseCommand depth config inputs =
    if 10 < depth then failwith $"Too deep command call: {depth}" else

    parser.ParseCommandLine(inputs, raiseOnUsage = true).GetAllResults()
    |> Seq.fold (fun s -> function
        | ScriptFileName _ -> s
        | Output_Path x -> { s with outputPath = x }
        | Reference_Path x -> { s with references = s.references @ [x] }
        | Command_File path ->
            let inputs = File.ReadLines(path, Encoding.UTF8) |> Seq.collect split |> Seq.toArray
            parseCommand (depth + 1) config inputs
    ) config

try
    let config = { outputPath = ""; references = [] }
    let config = parseCommand 0 config fsi.CommandLineArgs
    analyzeFromMsBuild config

with
| :? ArguParseException as e ->
    eprintfn $"{e.Message}"
    exit <| int e.ErrorCode

| e ->
    eprintfn $"{e.Message}"
    System.Environment.ExitCode <- -1
