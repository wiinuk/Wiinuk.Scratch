module Scratch.Serialization.Sb3.Test.Helpers
open Scratch.Primitives


module AdaptorJs =
    open Scratch.Json.Utf8
    open Scratch.Serialization
    open System.IO


    let usingTempFile action = async {
        let path = Path.GetTempFileName()
        try
            return! action path
        finally
            if File.Exists path then
                File.Delete path
    }

    let adaptorJsPath = "./adaptor.js"
    let convertFile serialize deserialize execShell project =
        usingTempFile <| fun inputPath ->
        usingTempFile <| fun outputPath -> async {
            let json = serialize project
            do! File.WriteAllTextAsync(path = inputPath, contents = json) |> Async.AwaitTask
            do! execShell adaptorJsPath inputPath outputPath
            let! json' = File.ReadAllTextAsync outputPath |> Async.AwaitTask
            return deserialize json'
        }

    let sb3ToSb3 project =
        project
        |> convertFile
            (Syntax.serializeString Sb3.Syntax.jProject)
            (Syntax.deserializeString Sb3.Syntax.jProject)
            (Shell.startAsync """node "%s" roundtripJson "%s" --outPath "%s" """)
        |> Async.RunSynchronously

    let sb2ToSb3 stage =
        stage
        |> convertFile
            (Syntax.serializeString (Sb2.Syntax.stageData HasDefault.unchecked))
            (Syntax.deserializeString Sb3.Syntax.jProject)
            (Shell.startAsync """node "%s" importSb2Json "%s" --outPath "%s" """)
        |> Async.RunSynchronously
