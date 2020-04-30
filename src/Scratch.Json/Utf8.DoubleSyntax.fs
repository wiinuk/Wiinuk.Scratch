[<AutoOpen>]
module internal Scratch.Json.Utf8.DoubleSyntax

let private _nanSymbol = "NaN"B
let readNaNTail (r: _ byref) (x: _ outref) =
    let bytes = getBufferUnsafe &r
    let offset = r.GetCurrentOffsetUnsafe()
    if offset + 2 < bytes.Length then
        if bytes.[offset + 1] <> 'a'B then false else
        if bytes.[offset + 2] <> 'N'B then false else
        r.AdvanceOffset 3
        x <- nan
        true
    else
        false

let private _infititySymbol = "Infinity"B
let private _minfititySymbol = "-Infinity"B
let readInfinityTail (r: _ byref) (x: _ outref) sign =
    let bytes = getBufferUnsafe &r
    let offset = r.GetCurrentOffsetUnsafe()
    if offset + 7 < bytes.Length then
        if bytes.[offset + 1] <> 'n'B then false else
        if bytes.[offset + 2] <> 'f'B then false else
        if bytes.[offset + 3] <> 'i'B then false else
        if bytes.[offset + 4] <> 'n'B then false else
        if bytes.[offset + 5] <> 'i'B then false else
        if bytes.[offset + 6] <> 't'B then false else
        if bytes.[offset + 7] <> 'y'B then false else
        r.AdvanceOffset 8
        x <- sign * infinity
        true
    else
        false

let tryReadDouble (r: _ byref) (result: _ outref) =
    skipWhiteSpace &r
    let bytes = r.GetBufferUnsafe()
    let offset = r.GetCurrentOffsetUnsafe()
    if offset < bytes.Length then
        match bytes.[offset] with
        | 'N'B -> readNaNTail &r &result
        | 'I'B -> readInfinityTail &r &result 1.
        | '-'B ->
            if offset + 1 < bytes.Length && bytes.[offset + 1] = 'I'B then
                r.AdvanceOffset 1
                readInfinityTail &r &result -1.
            else

            result <- r.ReadDouble()
            true

        | '0'B
        | '1'B
        | '2'B
        | '3'B
        | '4'B
        | '5'B
        | '6'B
        | '7'B
        | '8'B
        | '9'B ->
            result <- r.ReadDouble()
            true

        | _ -> false
    else false

let writeDouble (w: _ byref) x =
    if x <> x then writeRaw &w _nanSymbol
    elif x = infinity then w.WriteRaw _infititySymbol
    elif x = -infinity then w.WriteRaw _minfititySymbol
    else w.WriteDouble x
