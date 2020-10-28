module Scratch.Serialization.Sb3.Test.Helpers
open Scratch.Primitives
open System
open System.IO.Pipes
open System.Text.Json
open System.Text.Json.Serialization


type NodeIpcClient = {
    pipe: NamedPipeClientStream
    serializerOptions: JsonSerializerOptions
    readBuffer: byte array
}
with
    member x.Dispose() = x.pipe.Dispose()
    interface IDisposable with
        member x.Dispose() = x.Dispose()

module NodeIpcClient =
    let connect id = async {
        let pipe = new NamedPipeClientStream(sprintf "tmp-app.%s" id)
        let! cancel = Async.CancellationToken
        do! pipe.ConnectAsync cancel |> Async.AwaitTask

        let serializerOptions = JsonSerializerOptions()
        serializerOptions.Converters.Add <| JsonFSharpConverter()
        return {
            pipe = pipe
            serializerOptions = serializerOptions
            readBuffer = Array.zeroCreate 4096
        }
    }

    let send { pipe = pipe; serializerOptions = serializerOptions } name value = async {
        let utf8Json = JsonSerializer.SerializeToUtf8Bytes(struct {| ``type`` = name+""; data = value |}, serializerOptions)
        let utf8Json = Array.append utf8Json "\f"B
        let! cancel = Async.CancellationToken
        do! pipe.WriteAsync(utf8Json, 0, utf8Json.Length, cancel) |> Async.AwaitTask

        pipe.WaitForPipeDrain()
    }
    let readTail ({ readBuffer = buffer } as client) = async {
        let bytes = ResizeArray buffer
        let mutable next = true
        while next do
            let! cancel = Async.CancellationToken
            let! readCount = client.pipe.ReadAsync(buffer, 0, buffer.Length, cancel) |> Async.AwaitTask
            bytes.AddRange buffer.[0..readCount-1]
            if readCount < buffer.Length then
                next <- false

        if bytes.[bytes.Count-1] <> '\f'B then return failwithf "required '\\f'" else

        let r = JsonSerializer.Deserialize<struct {| ``type``:string; data: 'Result |}>(ReadOnlySpan(bytes.ToArray(), 0, length = bytes.Count - 1), client.serializerOptions)
        return struct(r.``type``, r.data)
    }
    let sendAndResponse (_: 'Result The) ({ readBuffer = buffer } as client) name value = async {
        do! send client name value

        let! cancel = Async.CancellationToken
        let! readCount = client.pipe.ReadAsync(buffer, 0, buffer.Length, cancel) |> Async.AwaitTask

        if readCount = buffer.Length then return! readTail client else
        if buffer.[readCount-1] <> '\f'B then return failwithf "required '\\f'" else

        let r = JsonSerializer.Deserialize<struct {| ``type``: string; data: 'Result |}>(ReadOnlySpan(buffer, 0, length = readCount - 1), client.serializerOptions)
        return struct(r.``type``, r.data)
    }

module AdaptorJs =
    open Scratch.Json.Utf8
    open Scratch.Serialization
    open System.IO
    open System.Diagnostics


    let adaptorJsPath = "./adaptor.js"

    let usingTempFile action = async {
        let path = Path.GetTempFileName()
        try
            return! action path
        finally
            if File.Exists path then
                File.Delete path
    }

    let convertFile serialize deserialize execShell project =
        usingTempFile <| fun inputPath ->
        usingTempFile <| fun outputPath -> async {
            let json = serialize project
            let! cancel = Async.CancellationToken
            do! File.WriteAllTextAsync(path = inputPath, contents = json, cancellationToken = cancel) |> Async.AwaitTask
            do! execShell adaptorJsPath inputPath outputPath
            let! cancel = Async.CancellationToken
            let! json' = File.ReadAllTextAsync(outputPath, cancel) |> Async.AwaitTask
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

    type Client = {
        ipcClient: NodeIpcClient
    }
    with
        member x.Dispose() = async {
            try do! NodeIpcClient.send x.ipcClient "stop" ()
            finally x.ipcClient.Dispose()
        }
        interface IDisposable with
            member x.Dispose() = x.Dispose() |> Async.RunSynchronously

    let startServerAndConnect() = async {
        let id = sprintf "server_%s" <| Guid.NewGuid().ToString "N"

        do! Shell.startAsync "node \"%s\" start-server --id \"%s\"" adaptorJsPath id |> Async.StartChild |> Async.Ignore
        let! client = NodeIpcClient.connect id
        return {
            ipcClient = client
        }
    }
    let pretty (e: JsonElement) =
        if e.ValueKind = JsonValueKind.Undefined then "undefined" else
        JsonSerializer.Serialize(e, JsonSerializerOptions(WriteIndented = true))

    let convertBy serialize deserialize commandName client value = async {
        let json = serialize value
        let! struct(_, result) =
            struct
                {|
                name = commandName
                args = struct {| projectJson = json |}
                |}
            |> NodeIpcClient.sendAndResponse the<JsonElement> client.ipcClient "exec"

        match result.GetProperty("tag").GetString() with
        | "Ok" ->
            let resultJson = result.GetProperty("value").GetProperty("projectJson").GetString()
            return deserialize resultJson

        | "Error" ->
            return
                result.GetProperty "value"
                |> pretty
                |> failwithf "Error: commandName: %A, error: %s" commandName

        | x ->
            return failwithf "unknown tag: %A" x
    }
    let sb3ToSb3By client value =
        value
        |> convertBy
            (Syntax.serializeString Sb3.Syntax.jProject)
            (Syntax.deserializeString Sb3.Syntax.jProject)
            "roundtrip-json"
            client

    let sb2ToSb3By client value =
        value
        |> convertBy
            (Syntax.serializeString (Sb2.Syntax.stageData HasDefault.unchecked))
            (Syntax.deserializeString Sb3.Syntax.jProject)
            "import-sb2-json"
            client
