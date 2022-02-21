namespace Scratch.Primitives
open System
open System.Text
open System.Runtime.InteropServices

#nowarn "0060" // Override implementations in augmentations are now deprecated. Override implementations should be given as part of the initial declaration of a type.


// type Document =
//     | Empty
//     | Rune of char
//     | Text of string
//     | NewLine
//     | Breakable of string
//     | Append of Document * Document
//     | Nest of Document
//     | Group of Document
//     | Replicate of Document * int

[<System.Flags>]
type private Bits =
    | Empty             = 0b00000000000_0_0000us
    | Text              = 0b00000000000_0_0001us
    | Rune              = 0b00000000000_0_0010us
    | NewLine           = 0b00000000000_0_0011us
    | Breakable         = 0b00000000000_0_0100us
    | Append            = 0b00000000000_0_0101us
    | ManyList          = 0b00000000000_0_0110us
    | ManyArray         = 0b00000000000_0_0111us
    | ManyResizeArray   = 0b00000000000_0_1000us

    | Group             = 0b00000000000_1_0000us
    | NoGroup           = 0b00000000000_0_0000us

    | TagMask           = 0b00000000000_0_1111us
    | IsGroupMask       = 0b00000000000_1_0000us
    | TrailingMask      = 0b11111111111_0_0000us

    | TrailingShift     = 5us

    | MaxTraniling      = 0b11111111111us

/// alternative tuple for field reference
type private Pair<'T1,'T2> = { item1: 'T1; item2: 'T2 }

[<Struct; NoComparison; NoEquality; StructLayout(LayoutKind.Auto); StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type Document = private {
    /// { trailingCount: 11bit, isGroup: 1bit; kind: 4bit }
    bits: Bits

    nestLevel: uint16

    charOrWidth: int32

    /// | Empty => null
    /// | Text => string
    /// | Rune => null
    /// | NewLine => null
    /// | Breakable => string
    /// | Append => Pair<Document, Document>
    /// | ManyList => Document list
    /// | ManyArray => Document array
    /// | ManyResizeArray => Document ResizeArray
    obj: obj
}

type private Tag = Bits

module Document =
    type Config = {
        newLine: string
        indent: string
        maxWidth: int
    }
    module Config =
        let defaultConfig = {
            newLine = "\r\n"
            indent = "    "
            maxWidth = 120
        }

    [<AutoOpen>]
    module private Helpers =
        let nestLevel (x: Document inref) = x.nestLevel
        let tag (x: _ inref) = x.bits &&& Bits.TagMask
        let isGroup (x: _ inref) = x.bits &&& Bits.IsGroupMask = Bits.Group
        let trailingCount (x: _ inref) = int32 (uint32 (x.bits &&& Bits.TrailingMask) >>> int Bits.TrailingShift)

        let width (x: _ inref) = match tag &x with Tag.Rune -> 1 | _ -> x.charOrWidth

        [<Struct; StructLayout(LayoutKind.Auto)>]
        [<NoComparison; NoEquality>]
        type RenderState = {
            buffer: StringBuilder
            line: int
            column: int
        }
        [<Struct; StructLayout(LayoutKind.Auto)>]
        type RenderEnv = {
            config: Config
            isNewLine: bool
            level: uint16
            indentLength: byte
        }

        let setPosition l c (s: _ inref) = { s with line = l; column = c }
        let append w (x: string) (s: _ inref) =
            s.buffer.Append x |> ignore
            setPosition s.line (s.column + w) &s

        let appendChar (x: char) (s: _ inref) =
            s.buffer.Append x |> ignore
            setPosition s.line (s.column + 1) &s

        let appendIndent (e: _ inref) (s: _ inref) =
            let mutable s' = s
            let i = e.config.indent
            for _ in 1..int e.level do s' <- append (int e.indentLength) i &s'
            s'
        let appendNewLine (e: _ inref) (s: _ inref) =
            let s = append 0 e.config.newLine &s
            let s = setPosition (s.line + 1) 0 &s
            appendIndent &e &s

        let rec build1 (e: _ inref) (s: _ inref) (x: _ inref) =
            let e =
                { e with
                    level = Checked.(+) e.level (nestLevel &x)
                    isNewLine = if isGroup &x then e.config.maxWidth < s.column + width &x else e.isNewLine
                }

            match tag &x with
            | Tag.Empty -> s
            | Tag.Rune ->
                appendChar (char x.charOrWidth) &s

            | Tag.Text ->
                let t = x.obj :?> string
                append x.charOrWidth t &s

            | Tag.NewLine -> appendNewLine &e &s
            | Tag.Breakable ->
                let v = x.obj :?> string

                if e.isNewLine (* || s.width < c.maxWidth *)
                then appendNewLine &e &s
                else append x.charOrWidth v &s

            | Tag.Append ->
                let lr = x.obj :?> Pair<Document, Document>
                let s = build &e &s &lr.item1
                build &e &s &lr.item2

            | Tag.ManyList ->
                let mutable state = s
                for x in x.obj :?> Document list do
                    state <- build &e &state &x
                state

            | Tag.ManyArray ->
                let mutable state = s
                let xs = x.obj :?> Document array
                let mutable i = 0
                while
                    begin
                        if i < xs.Length then
                            state <- build &e &state &xs[i]
                            i <- i + 1
                            true
                        else
                            false
                    end
                    do ()
                state

            | Tag.ManyResizeArray ->
                let mutable state = s
                for x in x.obj :?> Document ResizeArray do
                    state <- build &e &state &x
                state

            | _ -> failwith ""

        and build (e: _ inref) (s: _ inref) (x: _ inref) =
            let mutable s = build1 &e &s &x
            for _ in 1..trailingCount &x do
                s <- build1 &e &s &x
            s

    let renderWith c x =
        let e = {
            config = c
            isNewLine = c.maxWidth < 0 + width &x
            level = 0us
            indentLength = Checked.byte c.indent.Length
        }
        let s = {
            buffer = StringBuilder()
            line = 0
            column = 0
        }
        build &e &s &x |> ignore
        s.buffer.ToString()

    let render d = renderWith Config.defaultConfig d

    let empty<'a> = { bits = Bits.Empty; charOrWidth = 0; nestLevel = 0us; obj = null }
    
    let private uncheckedOfResizeArray (xs: _ ResizeArray) =
        match xs.Count with
        | 0 -> empty
        | 1 -> xs[0]
        | _ ->

        let mutable w = 0
        let mutable e = xs.GetEnumerator()
        while e.MoveNext() do
            let x = e.Current
            w <- w + width &x

        { bits = Bits.ManyResizeArray; nestLevel = 0us; charOrWidth = w; obj = xs }

    let private listSequence = function
        | [] -> empty
        | [x] -> x
        | xs ->

        let mutable list = xs
        let mutable w = 0
        while
            begin
                match list with
                | [] -> false
                | x::xs ->
                    w <- w + width &x
                    list <- xs
                    true
            end
            do ()

        { bits = Bits.ManyList ||| Bits.NoGroup; nestLevel = 0us; charOrWidth = w; obj = xs }

    let private arraySequence = function
        | [||] -> empty
        | [|x|] -> x
        | xs ->
            let mutable w = 0
            for x in xs do w <- w + width &x
            { bits = Bits.ManyArray ||| Bits.NoGroup; nestLevel = 0us; charOrWidth = w; obj = Array.copy xs }

    let private seqSequence (xs: _ seq) =
        use e = xs.GetEnumerator()
        if e.MoveNext() then
            let x1 = e.Current
            if e.MoveNext() then
                uncheckedOfResizeArray <| ResizeArray xs
            else
                x1
        else
            empty

    module Constructors =
        let empty<'a> = empty<'a>
        let private text' width (x: string) =
            { bits = Bits.Text; charOrWidth = width; nestLevel = 0us; obj = x }

        let textWith width text =
            match text with
            | null
            | "" -> empty
            | _ -> text' width text

        let text = function
            | null
            | "" -> empty
            | x -> text' x.Length x

        let rune c = { bits = Bits.Rune; charOrWidth = int<char> c; nestLevel = 0us; obj = null }
        let breakable x =
            let x = match x with null -> "" | x -> x
            { bits = Bits.Breakable; charOrWidth = x.Length; nestLevel = 0us; obj = x }
        let newline<'a> = { bits = Bits.NewLine; charOrWidth = 0; nestLevel = 0us; obj = null }
        let group x = { x with bits = x.bits ||| Bits.Group }
        let nest x = { x with nestLevel = Checked.(+) x.nestLevel 1us }
        let append d1 d2 =
            match tag &d1, tag &d2 with
            | Tag.Empty, _ -> d2
            | _, Tag.Empty -> d1
            | _ -> { bits = Bits.Append; nestLevel = 0us; charOrWidth = width &d1 + width &d2; obj = { item1 = d1; item2 = d2 } }

        let replicate count x =
            if count <= 0 then empty else
            if count = 1 then x else

            let oldCount = Checked.(+) (trailingCount &x) 1
            let newCount = Checked.(*) oldCount count
            let newTrailing = Checked.uint16 (Checked.(-) newCount 1)

            // TODO: 最大値に達した場合、新しく子ノードを確保する方法もある
            if uint16 Bits.MaxTraniling < newTrailing then raise <| OverflowException()

            // Trailing 以外を残したあと、新しい Trailing を追加
            let bits = x.bits &&& ~~~Bits.TrailingMask
            // `LanguagePrimitives.EnumOfValue` は具体型のときボックス化を伴わない
            let bits = bits ||| ((LanguagePrimitives.EnumOfValue newTrailing <<< int Bits.TrailingShift) &&& Bits.TrailingMask)

            { x with bits = bits }

        let sequence xs =
            match xs: Document seq with
            | :? list<Document> as xs -> listSequence xs
            | :? array<Document> as xs -> arraySequence xs
            | xs -> seqSequence xs

        /// `" "`
        let ws<'a> = text " "
        /// newline or `" "`
        let ns<'a> = breakable " "
        /// newline or `"; "`
        let nsc<'a> = breakable "; "
        /// newline or `""`
        let ne<'a> = breakable ""
        /// newline
        let nl<'a> = newline

    /// `xs[0] ++ sep ++ xs[1] ++ ... ++ xs[N]`
    let concat sep (xs: #seq<_>) =
        use e = xs.GetEnumerator()
        if e.MoveNext() then
            let x0 = e.Current
            if e.MoveNext() then
                let x1 = e.Current
                let acc = ResizeArray()
                acc.Add x0
                acc.Add sep
                acc.Add x1
                while e.MoveNext() do
                    acc.Add sep
                    acc.Add e.Current
                uncheckedOfResizeArray acc
            else
                x0
        else
            empty

    let concatMap f sep xs = concat sep <| Seq.map f xs

    module Operators =
        open Constructors
        let (++) d1 d2 = append d1 d2
        let (+.) d s = append d (text s)
        let (.+) s d = append (text s) d
        let (.+.) s1 s2 = append (text s1) (text s2)

open Document.Constructors
type Document with
    member d.StructuredFormatDisplay =
        let s = Document.renderWith { Document.Config.defaultConfig with newLine = "⏎"; indent = "•"; maxWidth = Int32.MaxValue } d
        if 20 < s.Length then s[0..19] + "…"
        else s

    override d.ToString() = Document.render d
