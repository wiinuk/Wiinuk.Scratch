module Scratch.Transpiler.Format
open System
open Scratch.Reflection
open Scratch.Reflection.Expr
module E = Quotations.DerivedPatterns
module E = Quotations.Patterns


let (|MakeFormatClosure|_|) = function
    | E.Call(None, useFormatM, [E.Coerce(E.NewObject(format5C, [E.String format as formatE]), format4T)]) when
        format5C.DeclaringType.GetGenericTypeDefinition() = typedefof<PrintfFormat<_,_,_,_,_>> &&
        format4T.GetGenericTypeDefinition() = typedefof<PrintfFormat<_,_,_,_>> -> Some(useFormatM, format, SourceCode.ofExpr formatE)
    | _ -> None

let (|CallWithFormat|_|) = function
    | MakeFormatClosure(m, format, formatL) -> Some(m, format, formatL, [])
    | E.Applications(E.Let(_, MakeFormatClosure(f, format, formatL), _), args) -> Some(f, format, formatL, List.concat args)
    | _ -> None

module FormatParser =
    open FParsec

    let text0 = manyChars (noneOf "%" <|> (pstring "%%" >>% '%'))

    // %b : bool
    // %s : string
    // %d, %i : int
    // %f, %F, %g, %G : float
    // %A : any value
    let placeholder = pstring "%" >>. anyOf "bsdifFgGGA"
    let simpleFormat = text0 .>>. many (placeholder .>>. text0) .>> eof
    let parseFormat s =
        match run simpleFormat s with
        | Success(x, _, _) -> Result.Ok x
        | Failure(_, e, _) -> Result.Error <| int e.Position.Index

type StringInterpolationError =
    | FormatError of int * SourceCode
    | ArgumentsLengthMismatch of SourceCode
    | InvalidFormatArgmentType of Type

let (|CallWithStringInterpolation|_|) = function
    | CallWithFormat(m, format, formatL, args) ->
        match FormatParser.parseFormat format with
        | Error e -> Some(Error(FormatError(e, formatL)))
        | Ok(s, xs) ->
            if List.length xs <> List.length args then Some(Error(ArgumentsLengthMismatch formatL)) else
            Some(Ok(m, formatL, s, List.zip xs args |> List.map (fun ((p, s), arg) -> p, arg, s)))
    | _ -> None
