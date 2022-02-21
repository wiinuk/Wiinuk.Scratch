[<AutoOpen>]
module internal Scratch.Json.Utf8.JsonWriter
open Utf8Json
open Utf8Json.Internal

let writeBeginArray (w: JsonWriter byref) = w.WriteBeginArray()
let writeBeginObject (w: JsonWriter byref) = w.WriteBeginObject()
let writeRaw (w: JsonWriter byref) (rawValue: byte[]) = w.WriteRaw rawValue

[<Struct>]
type private JsonStringWriter = {
    mutable i: int
    text: string
    start: int
    mutable offset: int
    mutable buffer: byte array
    mutable lastWrite: int
    mutable minWriteLength: int
}

let hexDigitToByte d =
    let offset = if d <= 9 then int '0'B else int 'a'B - 10
    byte (d + offset)

let private pincr (x: _ byref) =
    let r = x
    x <- x + 1
    r

let private writeEscape (w: _ byref) c =
    let mutable c1 = '\000'B
    let mutable c2 = '\000'B
    let size =
        match c with
        | '"' -> c1 <- '"'B; 1
        | '\\' -> c1 <- '\\'B; 1
        | '\b' -> c1 <- 'b'B; 1
        | '\n' -> c1 <- 'n'B; 1
        | '\f' -> c1 <- 'f'B; 1
        | '\r' -> c1 <- 'r'B; 1
        | '\t' -> c1 <- 't'B; 1
        | _ ->
            c1 <- hexDigitToByte (int c / 0x10 % 0x10)
            c2 <- hexDigitToByte (int c % 0x10)
            2

    if size = 1 then
        w.minWriteLength <- w.minWriteLength + 2
        BinaryUtil.EnsureCapacity(&w.buffer, w.start, w.minWriteLength)

        w.offset <- w.offset + StringEncoding.UTF8.GetBytes(w.text, w.lastWrite, w.i - w.lastWrite, w.buffer, w.offset)
        w.lastWrite <- w.i + 1

        w.buffer[pincr &w.offset] <- '\\'B
        w.buffer[pincr &w.offset] <- c1
    else
        w.minWriteLength <- w.minWriteLength + 6
        BinaryUtil.EnsureCapacity(&w.buffer, w.start, w.minWriteLength)
        
        w.offset <- w.offset + StringEncoding.UTF8.GetBytes(w.text, w.lastWrite, w.i - w.lastWrite, w.buffer, w.offset)
        w.lastWrite <- w.i + 1

        w.buffer[pincr &w.offset] <- '\\'B
        w.buffer[pincr &w.offset] <- 'u'B
        w.buffer[pincr &w.offset] <- '0'B
        w.buffer[pincr &w.offset] <- '0'B
        w.buffer[pincr &w.offset] <- c1
        w.buffer[pincr &w.offset] <- c2

let writeString (w: JsonWriter byref) = function
    | null -> w.WriteNull()
    | x ->

    let mutable segment = w.GetBuffer()
    let mutable state = {
        i = 0
        text = x
        start = segment.Count
        offset = segment.Count
        buffer = segment.Array
        lastWrite = 0
        minWriteLength = JsonReader.StringEncoding.UTF8.GetMaxByteCount(String.length x) + "\"\"".Length
    }
    BinaryUtil.EnsureCapacity(&state.buffer, state.offset, state.minWriteLength)

    state.buffer[pincr &state.offset] <- '\"'B

    while
        begin
            if state.i < String.length state.text then
                match state.text[state.i] with
                | '\\'
                | '"' as c -> writeEscape &state c
                | c when c <= '\x1F' -> writeEscape &state c
                | _ -> ()
                true
            else
                false
        end
        do state.i <- state.i + 1

    if state.lastWrite <> state.text.Length then
        state.offset <- state.offset + StringEncoding.UTF8.GetBytes(state.text, state.lastWrite, state.text.Length - state.lastWrite, state.buffer, state.offset)

    state.buffer[pincr &state.offset] <- '\"'B

    w <- JsonWriter state.buffer
    w.AdvanceOffset state.offset 
