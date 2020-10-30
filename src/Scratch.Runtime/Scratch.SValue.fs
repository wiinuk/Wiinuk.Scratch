namespace Scratch
open Scratch.Primitives


[<Struct>]
type SValue =
    | SNumber of doubleValue: double
    | SBool of boolValue: bool
    | SString of stringValue: string

module private SNumber =
    open System

    [<Struct>]
    type Span = {
        Source: string
        Current: int
        End: int
    }
    module Span =
        module P = NativeInterop.NativePtr

        let length x = x.End - x.Current
        let isEmpty x = length x <= 0
        let tryGetCons x (head: _ outref) (tail: _ outref) =
            if length x = 0 then false else

            head <- x.Source.[x.Current]
            tail <- { x with Current = x.Current + 1 }
            true

        let tryGetCons2 x (head1: _ outref) (head2: _ outref) (tail: _ outref) =
            if length x <= 1 then false else

            head1 <- x.Source.[x.Current]
            head2 <- x.Source.[x.Current + 1]
            tail <- { x with Current = x.Current + 2 }
            true

        let tryGetHead x (head: _ outref) =
            if length x <= 0 then false else

            head <- x.Source.[x.Current]
            true

        let ofString source = { Source = source; Current = 0; End = source.Length }
        let equals s1 s2 =
            let l1 = length s1
            if l1 <> length s2 then false else

            let i1 = s1.Current
            let i2 = s2.Current

            let rec aux i =
                if i = l1 then true else
                if s1.Source.[i1 + i] <> s2.Source.[i2 + i] then false
                else aux (i + 1)

            aux 0

        let toString (x: Span) = x.Source.Substring(x.Current, length x)

        let trim cs =
            let source = cs.Source
            let mutable current = cs.Current
            let end' = cs.End
            while current <> end' && Char.IsWhiteSpace source.[current] do
                current <- current + 1

            let current = current
            let mutable end' = end'
            while current <> end' && Char.IsWhiteSpace source.[end' - 1] do
                end' <- end' - 1

            { cs with Current = current; End = end' }

    /// [0-9a-fA-F]
    let readHex (s: _ byref) (v: _ byref) =
        let mutable c = '\000'
        let mutable tail = s
        if Span.tryGetCons s &c &tail && (('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')) then
            s <- tail
            let hex2int c = (int c &&& 15) + (int c >>> 6) * 9
            v <- v * 16I + bigint(hex2int c)
            true
        else false

    /// \s*
    let skipSpaces0 (s: _ byref) =
        let mutable c = '\000'
        let mutable tail = s
        while Span.tryGetCons s &c &tail && Char.IsWhiteSpace c do
            s <- tail

    /// [+-]?
    let optSign (s: _ byref) =
        let mutable c = '\000'
        let mutable tail = s
        if Span.tryGetCons s &c &tail then
            match c with
            | '-' -> s <- tail; -1
            | '+' -> s <- tail; 1
            | _ -> 1
        else 1

    /// 0[xX]
    let skip0X (s: _ byref) =
        let mutable c0 = '\000'
        let mutable c1 = '\000'
        let mutable tail = s
        if Span.tryGetCons2 s &c0 &c1 &tail && c0 = '0' && (c1 = 'x' || c1 = 'X') then
            s <- tail
            true
        else
            false

    /// [0-9.]
    let isDoubleHead (s: _ inref) =
        let mutable c = '\000'
        Span.tryGetHead s &c && (('0' <= c && c <= '9') || c = '.')

module SValue =
    open SNumber
    open System
    open System.Globalization
    open System.Text.RegularExpressions


    let sEmptyString = SString ""
    let sZero = SNumber 0.
    let sFalse = SBool false
    let sTrue = SBool true

    let private invariantCultureNumberFormatWithoutSymbol =
        let f = CultureInfo.InvariantCulture.NumberFormat.Clone() :?> NumberFormatInfo
        f.PositiveInfinitySymbol <- ""
        f.NegativeInfinitySymbol <- ""
        f.NaNSymbol <- ""
        f

    let stringToNumber = function
        | "" -> 0.
        | x ->

        let mutable r = 0.
        if Double.TryParse(x, NumberStyles.Float, invariantCultureNumberFormatWithoutSymbol, &r) then r else

        let x = Span.ofString x
        let t = Span.trim x
        if Span.equals t (Span.ofString "Infinity") then infinity else
        if Span.equals t (Span.ofString "-Infinity") then -infinity else
        if Span.equals t (Span.ofString "NaN") then nan else
        0.

    // TODO:
    let toNumber = function
        | SNumber x -> x
        | SBool true -> 1.
        | SBool false -> 0.
        | SString x -> stringToNumber x

    let toBool = function
        | SBool x -> x
        | SString ""
        | SString "false" -> false
        | x -> toNumber x <> 0.


    let numberToString = function

        // `0` -or- `-0`
        | 0. -> "0"

        | x ->

        if Double.IsNaN x then "NaN"
        elif Double.IsPositiveInfinity x then "Infinity"
        elif Double.IsNegativeInfinity x then "-Infinity"

        // round trip
        else x.ToString "G17"

    let toString = function
        | SNumber x -> numberToString x
        | SString x -> x
        | SBool true -> "true"
        | SBool false -> "false"

    let digitRegex = Regex @"^\d+$"
    let (|DigitLike|) x =
        let x = toString x
        if digitRegex.IsMatch x then ValueSome(double x) else ValueNone

    let compareSValue x1 x2 =
        match x1, x2 with
        | (SNumber x1 | DigitLike(ValueSome x1)), (SNumber x2 | DigitLike(ValueSome x2))
            when not (Double.IsNaN x1) && not (Double.IsNaN x2) ->
            if x1 < x2 then -1 elif x1 = x2 then 0 else 1

        | _ ->
            let x1 = toString x1
            let x2 = toString x2
            String.Compare(x1, x2, StringComparison.OrdinalIgnoreCase)

    let equalsSValue x1 x2 =
        match x1, x2 with
        | (SNumber x1 | DigitLike(ValueSome x1)), (SNumber x2 | DigitLike(ValueSome x2))
            when not (Double.IsNaN x1) && not (Double.IsNaN x2) ->
            x1 = x2

        | _ ->
            let x1 = toString x1
            let x2 = toString x2
            String.Equals(x1, x2, StringComparison.OrdinalIgnoreCase)

    let tryParseSNumber source =
        let source = Span.ofString source
        let mutable s = source
            
        skipSpaces0 &s
        let sign = optSign &s
        if skip0X &s then
            let mutable v = 0I
            if readHex &s &v then
                while readHex &s &v do ()
                skipSpaces0 &s
                if Span.isEmpty s then // ^\s*[+-]?0[xX][0-9a-fA-F]+\s*$
                    ValueSome(double v * double sign)
                else
                    ValueNone
            else
                ValueNone
        else
            if isDoubleHead &s then // ^\s*[+-]?[0-9.]
                let mutable r = 0.
                if System.Double.TryParse(Span.toString(Span.trim source), &r) then
                    ValueSome r
                else
                    ValueNone
            else ValueNone

    let tryParseSBool x =
        match String.length x with
        | 4 ->
            match x.[0], x.[1], x.[2], x.[3] with
            | ('T' | 't'), ('R' | 'r'), ('U' | 'u'), ('E' | 'e') -> ValueSome true
            | _ -> ValueNone
        | 5 ->
            match x.[0], x.[1], x.[2], x.[3], x.[4] with
            | ('F' | 'f'), ('A' | 'a'), ('L' | 'l'), ('S' | 's'), ('E' | 'e') -> ValueSome false
            | _ -> ValueNone
        | _ ->
            ValueNone

    let asciiDigitRegex = Regex @"^[0-9]+$"
    let isCloudValue = function
        | SNumber x -> not <| Double.IsNaN x && not <| Double.IsInfinity x
        | SString x -> asciiDigitRegex.IsMatch x
        | SBool _ -> false

[<AutoOpen>]
module SValuePatterns =
    let (|TryParseSNumber|_|) x = SValue.tryParseSNumber x |> VOption.box
    let (|TryParseSBool|_|) x = SValue.tryParseSBool x |> VOption.box

type SValue with
    static member op_Implicit s = SValue.toString s
    static member op_Implicit s = SValue.toNumber s
    static member op_Implicit s = SValue.toBool s
    static member op_Implicit x = SString x
    static member op_Implicit x = SNumber x
    static member op_Implicit x = SBool x

[<Struct>]
[<NoComparison; NoEquality>]
type 'T SList = private SList of 'T ResizeArray
