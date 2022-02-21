module Scratch.Detranspiler.Tests
open System.Text
open Scratch
open Scratch.Evaluator
open Scratch.Transpiler
open Scratch.Threading
open Xunit


module Isolated =
    open System
    open System.Diagnostics
    open System.IO
    open Argu
    open Scratch.IsolatedTester


    let parser = ArgumentParser.Create<TesterCLIArgs>()

    let tfm =
#if NET6_0
        "net6.0"
#endif
    let testerPath = Path.Combine(__SOURCE_DIRECTORY__, sprintf "../scratch-tester/bin/Debug/%s/scratch-tester.dll" tfm)

    type ProcessExit = {
        name: string
        args: string
        encoding: Encoding
        exitCode: int
        outputs: string seq
        errors: string seq
    }
    type ProcessStartConfig = {
        encoding: Encoding
    }
    let start withConfig name args = async {
        let { ProcessStartConfig.encoding = encoding } = withConfig { encoding = Console.OutputEncoding }
        let startInfo =
            ProcessStartInfo(
                name,
                args,
                RedirectStandardOutput = true,
                StandardOutputEncoding = encoding,
                RedirectStandardError = true,
                StandardErrorEncoding = encoding,
                CreateNoWindow = true,
                UseShellExecute = false
            )
        use child = new Process(StartInfo = startInfo, EnableRaisingEvents = true)
        let outputs = ResizeArray()
        let errors = ResizeArray()
        child.OutputDataReceived.Add <| fun e -> outputs.Add e.Data
        child.ErrorDataReceived.Add <| fun e -> errors.Add e.Data

        let exited = Async.AwaitEvent child.Exited
        if not <| child.Start() then failwith "A new process was not started."
        child.BeginOutputReadLine()
        child.BeginErrorReadLine()

        let! _ = exited
        return {
            name = name
            args = args
            encoding = encoding
            exitCode = child.ExitCode
            outputs = outputs
            errors = errors
        }
    }

    let startf withConfig name format = Printf.ksprintf (start withConfig name) format

    let runTester command = async {
        let inputPath = Path.GetTempFileName()
        try
            toJsonFile inputPath (command: Command)
            let args = parser.PrintCommandLineArgumentsFlat [Input inputPath]
            return! startf id "dotnet" "\"%s\" %s" testerPath args
        finally
            File.Delete inputPath
    }

module Compiler =
    open System.IO
    open Scratch.IsolatedTester
    open Scratch.Detranspiler.Test
    module E = Quotations.DerivedPatterns
    module E = Quotations.Patterns

    let compileAndTranspileInSandbox references stageModuleName source =
        CompilerProcess.compileToAssembly references stageModuleName source <| fun dllPath -> async {

        let commandResultPath = Path.GetTempFileName()
        try
            let command = LoadAndTranspile {
                dllPath = dllPath
                stageModuleName = stageModuleName
                entryPointName = Some "entryPoint"
                outPath = Some commandResultPath
            }
            let! exit = Isolated.runTester command
            if exit.exitCode < 0 then
                let command = sprintf "%s %s" exit.name exit.args
                let output = exit.outputs |> Seq.filter (isNull >> not) |> String.concat "\n"
                let error = exit.errors |> Seq.filter (isNull >> not) |> String.concat "\n"
                return failwithf "tester error. [%d]\ncommand:\n%s\nencoding: %s\noutput:\n%s\nerror:\n%s"
                    exit.exitCode command exit.encoding.WebName output error
            else

            let result = ofJsonFile<LoadAndTranspileResult> commandResultPath

            match result with
            | TranspileOk x -> return x
            | RequireReflectedDefintitionError -> return failwithf "RequireReflectedDefintition(%A)" (references, stageModuleName, source)
            | TranspileError e -> return failwithf "%A" e
        finally
            File.Delete commandResultPath
    }

let evaluateStdStage stage =
    let withConfig config =
        { config with
            showState = showLoc
            schedulerConfig =
            { config.schedulerConfig with
                startTime = Some System.DateTime.MinValue
                flame = Turbo
                deterministic = DeterministicTime {
                    processSpan = System.TimeSpan.FromTicks 1L
                    flameSpan = System.TimeSpan.FromSeconds (1. / 60.)
                }
            }
        }
    let state = evaluateStage withConfig stage
    match Map.tryFind "out" state.stage.lists with
    | ValueNone -> []
    | ValueSome xs -> List.ofSeq xs

let stdStageRoundTripTest stage = Async.StartAsTask <| async {
    let stage1 = transpileStage stage
    let out1 = evaluateStdStage stage1
    let source, warnings =
        try pretty showLoc stage1
        with DetranspileException(location, e, st) -> failwithf "DetranspileException(%A, %A, %A)" location e st

    let references = [
        typeof<Scratch.Primitives.hunit>.Assembly.Location
        typeof<Scratch.Stage>.Assembly.Location
    ]
    let! stage2 = Compiler.compileAndTranspileInSandbox references stage1.objName source
    let out2 = evaluateStdStage stage2
    do out1 =? out2
}

[<Fact>]
let simpleTest() =
    <@
        let out = defineList []
        SList.push out "test"
    @>
    |> stdStageRoundTripTest

[<Fact>]
let doUntilTest() =
    <@
        let mutable i = 0
        let out = defineList []

        repeatUntil (fun _ -> i < 4) <| fun _ ->
            SList.push out i
            i <- i + 1

        repeatUntil (fun _ -> i < 8) <| fun _ ->
            SList.push out i
            SList.push out i
            i <- i + 1
    @>
    |> stdStageRoundTripTest

[<Fact>]
let ifTest() =
    <@
        let out = defineList []
        let f b =
            SList.push out "f1"
            if b then
                SList.push out "f2"

        let g b =
            if b then
                SList.push out "g1"
            else
                SList.push out "g2"

        f true
        f false
        g true
        g false
    @>
    |> stdStageRoundTripTest

[<Fact>]
let timerTest() =
    <@
        repeat 5 <| fun _ -> out (string timer)
    @>
    |> stdStageRoundTripTest
