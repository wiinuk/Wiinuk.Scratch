open Argu
open Scratch.IsolatedTester

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<TesterCLIArgs>(programName = "scratch-tester.exe")
    try
        let r = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        r.TryGetResult(<@ Input @>)
        |> Option.map processFromJsonFile
        |> Option.defaultValue -1

    with
    | :? ArguParseException as e ->
        printfn "%s" e.Message
        int e.ErrorCode
