module Scratch.Serialization.Sb3.Test.Helpers
open Scratch.Primitives
open System
open System.Buffers
open System.Net.WebSockets
open System.Runtime.ExceptionServices
open System.Text.Json
open System.Text.Json.Serialization


type WebSocketIpcClient = {
    socket: ClientWebSocket
    serializerOptions: JsonSerializerOptions
    buffer: byte array
}
with
    member x.Dispose() = x.socket.Dispose()
    interface IDisposable with
        member x.Dispose() = x.Dispose()

module WebSocketIpcClient =
    let connect address = async {
        let maxRetry = 10
        let retryMs = 1500

        let serializerOptions =
            let o = JsonSerializerOptions()
            o.Converters.Add <| JsonFSharpConverter()
            o

        let mutable client = null
        let mutable next = true
        let mutable retryCount = 0
        while next do
            client <- new ClientWebSocket()
            try
                let! cancel = Async.CancellationToken
                do! client.ConnectAsync(Uri address, cancel) |> Async.AwaitTask
                next <- false

            with e ->
                let info = ExceptionDispatchInfo.Capture e

                client.Dispose()
                if maxRetry < retryCount then
                    info.Throw()
                else
                    retryCount <- retryCount + 1
                    do! Async.Sleep retryMs

        return {
            socket = client
            serializerOptions = serializerOptions
            buffer = Array.zeroCreate 4096
        }
    }

    let send { socket = client; serializerOptions = serializerOptions } messageType data = async {
        let! cancel = Async.CancellationToken

        let utf8Bytes = JsonSerializer.SerializeToUtf8Bytes(struct {| ``type`` = messageType + ""; data = data |}, serializerOptions)
        do! client.SendAsync(ArraySegment utf8Bytes, WebSocketMessageType.Text, endOfMessage = true, cancellationToken = cancel) |> Async.AwaitTask
    }
    let sendAndReceive (_: 'Result The) ({ socket = socket; serializerOptions = serializerOptions; buffer = buffer } as client) messageType data = async {
        do! send client messageType data

        let! cancel = Async.CancellationToken
        let! r = socket.ReceiveAsync(ArraySegment buffer, cancel) |> Async.AwaitTask
        match r.MessageType with
        | WebSocketMessageType.Text -> ()
        | WebSocketMessageType.Close ->
            let! cancel = Async.CancellationToken
            do! socket.CloseAsync(WebSocketCloseStatus.NormalClosure, "", cancel) |> Async.AwaitTask
            failwithf "server is closed; %A: %s" r.CloseStatus r.CloseStatusDescription

        | t ->
            let! cancel = Async.CancellationToken
            do! socket.CloseAsync(WebSocketCloseStatus.InvalidMessageType, "", cancel) |> Async.AwaitTask
            failwithf "unexpected message type: %A" t

        if r.EndOfMessage then
            return JsonSerializer.Deserialize(ReadOnlySpan(buffer, start = 0, length = r.Count), serializerOptions)
        else
            let bytes = ArraySegment(buffer, offset = 0, count = r.Count) |> ResizeArray
            let mutable endOfMessage = r.EndOfMessage
            while not endOfMessage do
                let! r = socket.ReceiveAsync(ArraySegment buffer, cancel) |> Async.AwaitTask
                ArraySegment(buffer, offset = 0, count = r.Count) |> bytes.AddRange
                endOfMessage <- r.EndOfMessage

            return JsonSerializer.Deserialize<'Result>(ReadOnlySpan(bytes.ToArray()), serializerOptions)
    }

module AdaptorJs =
    open Scratch.Json.Utf8
    open Scratch.Serialization
    open System.IO


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

    module IpcClient = WebSocketIpcClient
    type Client = {
        ipcClient: WebSocketIpcClient
    }
    with
        member x.Dispose() = async {
            try do! IpcClient.send x.ipcClient "stop" ()
            finally x.ipcClient.Dispose()
        }
        interface IDisposable with
            member x.Dispose() = x.Dispose() |> Async.RunSynchronously

    let startServerAndConnect() = async {
        let port = 55523
        do! Shell.startAsync "node \"%s\" start-server --port %d --silent" adaptorJsPath port |> Async.StartChild |> Async.Ignore
        let! client = IpcClient.connect <| sprintf "ws://localhost:%d" port
        return {
            ipcClient = client
        }
    }
    let pretty (e: JsonElement) =
        if e.ValueKind = JsonValueKind.Undefined then "undefined" else
        JsonSerializer.Serialize(e, JsonSerializerOptions(WriteIndented = true))

    let deserializeJsonElement (_: 'T The) (element: JsonElement) options =
        let bufferWriter = ArrayBufferWriter()
        use writer = new Utf8JsonWriter(bufferWriter)
        element.WriteTo writer
        writer.Flush()
        JsonSerializer.Deserialize<'T>(bufferWriter.WrittenSpan, options)

    let exec resultType client commandName args = async {
        let! result =
            struct
                {|
                name = commandName + ""
                args = args
                |}
            |> IpcClient.sendAndReceive the<JsonElement> client.ipcClient "exec"

        match result.GetProperty("tag").GetString() with
        | "Ok" -> return deserializeJsonElement resultType (result.GetProperty "value") client.ipcClient.serializerOptions
        | "Error" ->
            return
                result.GetProperty "value"
                |> pretty
                |> failwithf "Error: commandName: %A, error: %s" commandName

        | x ->
            return failwithf "unknown tag: %A" x
    }
    let convertBy serialize deserialize commandName client value = async {
        let json: string = serialize value
        let! result = exec the<JsonElement> client commandName struct {| projectJson = json |}
        return deserialize <| result.GetProperty("projectJson").GetString()
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

    let packageBinaryToSb3BinaryBy client value = async {
        let! result = exec the<struct {| binary: byte[] |}> client "roundtrip-package" struct {| binary = (value: byte[]) |}
        return result.binary
    }
