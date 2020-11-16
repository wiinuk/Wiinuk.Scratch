[<AutoOpen>]
module internal Scratch.Json.Utf8.JsonParsingException
open Utf8Json
open Scratch.Json.PartialIsomorphisms


[<Struct>]
type Position = { line: int; column: int }
type Location = { path: string; position1: Position; position2: Position }

let makeParsingException (reader: JsonReader) message inner =
    let bytes = reader.GetBufferUnsafe()
    let offset = reader.GetCurrentOffsetUnsafe()
    let actual =
        match bytes with
        | null -> "[eos]"
        | _ when offset < 0 || bytes.Length <= offset -> "[EOS]"
        | _ -> string (char bytes.[offset])

    JsonParsingExceptionWithInner(message, bytes, offset, offset, actual, inner)

let raiseParsingExceptionOfParseError error =
    let e = error.messageOrExn
    let message, inner = IsoError.messageOrNull e, IsoError.expectionOrNull e
    raise <| makeParsingException error.reader message inner

let raiseParsingExceptionWithInner (r: _ byref) message inner =
    raise <| makeParsingException r message inner

let raiseParsingException (r: _ inref) message =
    raise <| makeParsingException r message null

let raiseParsingExceptionOfIsoError (r: _ inref) error =
    raise <| makeParsingException r (IsoError.messageOrNull error) (IsoError.expectionOrNull error)

let printParsingException source (e: JsonParsingException) =
    /// liner search
    let indexToPosition (source: string) index =
        let rec aux l c i =
            if i < String.length source && i < index then
                if source.[i] = '\n' then
                    aux (l + 1) 1 (i + 1)
                else
                    aux l (c + 1) (i + 1)
            else { line = l; column = c }
        aux 1 1 0

    let buildSourceTextCore p1 p2 fileLines =
        fileLines
        |> Seq.mapi (fun i line -> struct(i + 1, line))
        |> Seq.filter (fun struct(l, _) -> p1.line - 1 <= l && l <= p2.line + 1)
        |> Seq.collect (fun struct(lineNumber, line) ->
            let lineWithNum = sprintf "%4d | %s" lineNumber line
            if p1.line <= lineNumber && lineNumber <= p2.line then
                let columnMin = if lineNumber = p1.line then p1.column + 1 else 1
                let columnMax = if lineNumber = p2.line then p2.column else String.length line
                let underLine = String.replicate (columnMin - 1) " " + String.replicate (columnMax - columnMin + 1) "^"
                let messageLine = sprintf "     | %s" underLine
                [|
                    lineWithNum
                    messageLine
                |]
            else
                [|lineWithNum|]
        )

    let offset = e.Offset
    let position = indexToPosition source offset
    let sourceLines = source.Split '\n'
    let sourceText = sourceLines |> buildSourceTextCore position { position with column = position.column + 1 }
    printfn "(%d, %d): %s, offset: %d actualChar: %A" position.line position.column e.Message e.Offset e.ActualChar
    for line in sourceText do
        printfn "%s" line

