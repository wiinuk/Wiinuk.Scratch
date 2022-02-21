[<AutoOpen>]
module internal Scratch.Json.Utf8.JsonReader
open Utf8Json
open System
open System.Text
type private T = Utf8Json.JsonToken


let skipWhiteSpace (r: JsonReader byref) = r.SkipWhiteSpace()
let readIsBeginArray (r: JsonReader byref) = r.ReadIsBeginArray()
let readIsBeginArrayWithVerify (r: JsonReader byref) = r.ReadIsBeginArrayWithVerify()
let readIsBeginObject (r: JsonReader byref) = r.ReadIsBeginObject()
let readIsBeginObjectWithVerify (r: JsonReader byref) = r.ReadIsBeginObjectWithVerify()
let readIsValueSeparator (r: JsonReader byref) = r.ReadIsValueSeparator()

let getBufferUnsafe (r: JsonReader byref) = r.GetBufferUnsafe()

let tryReadEndArrayOrValueSeparator (r: _ byref) (c: _ byref) (success: _ outref) =
    skipWhiteSpace &r
    if r.ReadIsEndArray() then success <- true; false else
    if c.count <> 0 && not (r.ReadIsValueSeparator()) then false else
    c.count <- c.count + 1
    true

let tryReadEndObjectOrValueSeparator (r: _ byref) (count: _ byref) (success: _ outref) =
    skipWhiteSpace &r
    if r.ReadIsEndObject() then success <- true; false else
    if count <> 0 && not (r.ReadIsValueSeparator()) then false else
    count <- count + 1
    true

let private nullTokenSegment = ArraySegment "null"B
module internal StringEncoding =
    let UTF8 = UTF8Encoding(encoderShouldEmitUTF8Identifier = false)

let private createParsingException (r: JsonReader byref) expected =
    let pos = r.GetCurrentOffsetUnsafe()
    let bytes = r.GetBufferUnsafe()
    let actual = bytes[r.GetCurrentOffsetUnsafe()] |> char |> string
    let actual =
        try
            match r.GetCurrentJsonToken() with
            | T.Number ->
                let ns = r.ReadNumberSegment()
                StringEncoding.UTF8.GetString(ns.Array, ns.Offset, ns.Count)

            | T.String ->  "\"" + r.ReadString() + "\""
            | T.True -> "true"
            | T.False -> "false"
            | T.Null -> "null"
            | _ -> actual
        with _ -> actual

    JsonParsingException(
        $"expected:'{expected}', actual:'{actual}', at offset:{pos}",
        bytes,
        pos,
        r.GetCurrentOffsetUnsafe(),
        actual
    )

let private createParsingExceptionMessage (r: JsonReader inref) message =
    let bytes = r.GetBufferUnsafe()
    let offset = r.GetCurrentOffsetUnsafe()
    let actual = bytes[offset] |> char |> string
    let pos = offset
    JsonParsingException(message, bytes, pos, pos, actual)

let readStringSegmentRaw (r: JsonReader byref) =
    if r.ReadIsNull() then nullTokenSegment else

    let bytes = r.GetBufferUnsafe()
    if bytes[let x = r.GetCurrentOffsetUnsafe() in r.AdvanceOffset 1; x] <> '"'B then
        raise <| createParsingException &r "\""

    let from = r.GetCurrentOffsetUnsafe()
    let mutable i = r.GetCurrentOffsetUnsafe()
    let mutable result = ArraySegment()
    while
        begin
            if i < bytes.Length then
                match bytes[i] with
                | '"'B ->
                    r.AdvanceOffset((i + 1) - from)
                    result <- ArraySegment(bytes, from, r.GetCurrentOffsetUnsafe() - from - 1);
                    false

                | '\\'B ->
                    i <- i + 2
                    true

                | _ ->
                    i <- i + 1
                    true
            else
                raise <| createParsingExceptionMessage &r "not found end string."
        end
        do ()
    result
