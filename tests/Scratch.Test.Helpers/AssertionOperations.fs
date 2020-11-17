namespace global
open FsCheck
open System
open Xunit


[<Struct; CustomEquality; CustomComparison; StructuredFormatDisplay("{Display}")>]
type ([<EqualityConditionalOn; ComparisonConditionalOn>] 'a) CustomToString = {
    value: 'a
    show: 'a -> string
}
    with
    member x.Display = x.show x.value
    override x.ToString() = x.show x.value
    override x.Equals y =
        match y with
        | :? ('a CustomToString) as y -> Unchecked.equals x.value y.value
        | _ -> false
    override x.GetHashCode() = Unchecked.hash x.value
    interface 'a CustomToString IComparable with
        override x.CompareTo y = Unchecked.compare x.value y.value

[<AutoOpen>]
module private AssertionOperationHelpers =
    let qcheckConfig = { Config.QuickThrowOnFailure with QuietOnSuccess = true }

[<AutoOpen>]
module AssertionOperations =
    let (=?) l r =
        if not <| LanguagePrimitives.GenericEqualityER l r then
            Assert.True(false, sprintf "%A =? %A" l r)

    let (<>?) l r =
        if LanguagePrimitives.GenericEqualityER l r then
            Assert.True(false, sprintf "%A <>? %A" l r)

    let throws x =
        let r = try let (Lazy x) = x in Ok x with e -> Error e
        match r with
        | Ok x -> failwithf "throws %A" x
        | _ -> ()

    let qcheckWith withConfig property =
        Check.One(withConfig qcheckConfig, property)

    let qcheck property = qcheckWith id property


namespace Scratch.Test

module AssertionWithDiff =
    open DiffMatchPatch
    open FSharp.Compiler.SourceCodeServices
    open Scratch.Test.AnsiEscape
    open Scratch.Test.AnsiEscape.Styling.Constructors
    open System.Text.RegularExpressions
    open type System.Environment

    [<AutoOpen>]
    module private Privates =
        type C = FSharp.Compiler.SourceCodeServices.FSharpTokenColorKind

        let insertK = [Color8 Color8.Green]
        let insertT = [BackgroundColor8 Color8.Green; Color8 Color8.Black]

        let deleteK = [Color8 Color8.Red]
        let deleteT = [BackgroundColor8 Color8.Red; Color8 Color8.Black; CrossedOut]

        let desc = [Color8 Color8.Black; BackgroundColor8Bright Color8.Black]

        let tokenizer = FSharpSourceTokenizer([], None)
        let colorKindToStyle = Map [
            C.Number, [Color8Bright Color8.Green]

            let string = [Color8 Color8.Yellow]
            C.String, string
            C.Text, string

            C.Comment, [Color8 Color8.Green]

            let identifier = []
            C.Identifier, identifier
            C.UpperIdentifier, identifier

            let keyword = [Color8 Color8.Blue]
            C.Keyword, keyword
            C.PreprocessorKeyword, keyword

            C.InactiveCode, [Color8Bright Color8.Black]

            let operator = [Color8Bright Color8.Blue]
            C.Operator, operator
            C.Punctuation, operator
        ]
        let newLinePattern = Regex @"(\r\n|\r|\n)"

    let stylingAsFSharpTokens x =
        newLinePattern.Split x
        |> Seq.chunkBySize 2
        |> Seq.mapFold (fun state lineAndNewLine ->
            let line, newLine =
                match lineAndNewLine with
                | [|line; newLine|] -> line, newLine
                | _ -> lineAndNewLine.[0], ""

            let t = tokenizer.CreateLineTokenizer line
            let rec tokens acc lastColumn state =
                match t.ScanToken state with
                | Some token, state ->
                    let t = line.[token.LeftColumn..token.RightColumn], Some token
                    tokens (t::acc) token.RightColumn state
        
                | _, state ->
                    let last = if lastColumn+1 < line.Length then line.[lastColumn+1..] + newLine else newLine
                    let acc = (last, None)::acc
                    List.rev acc, state
        
            tokens [] 0 state

        ) FSharpTokenizerLexState.Initial
        |> fst
        |> Seq.concat
        |> Seq.map (fun (s, t) ->
            let colorClass = t |> Option.map (fun t -> t.ColorClass) |> Option.defaultValue FSharpTokenColorKind.Default
            let style = Map.tryFind colorClass colorKindToStyle |> Option.defaultValue []
            div style s
        )

    let buildDiffText l r =
        let d = DiffMatchPatch.Default
        let es = d.DiffMain(l, r)
        d.DiffCleanupSemantic es

        seq {
            div desc "diff:"; br
            for e in es do
                match e.Operation with
                | Delete -> div deleteK "[- "; div deleteT e.Text; div deleteK " -]"
                | Insert -> div insertK "[+ "; div insertT e.Text; div insertK " +]"
                | Equal -> yield! stylingAsFSharpTokens e.Text
            br
            div desc "l:"; br
            yield! stylingAsFSharpTokens l; br
            div desc "r:"; br
            yield! stylingAsFSharpTokens r
        }
        |> Styling.render (fun c ->
            { c with
                maxColorLevel =
                    match GetEnvironmentVariable "NO_COLOR", GetEnvironmentVariable "FORCE_COLOR" with
                    | null, null ->
                        match GetEnvironmentVariable "CI" with
                        | null -> ColorLevel.NoColor
                        | _ -> min ColorLevel.Basic c.maxColorLevel

                    | _, null -> ColorLevel.NoColor
                    | _ -> c.maxColorLevel
            }
        )

    module Operators =
        let (=?) l r =
            if not <| LanguagePrimitives.GenericEqualityER l r then
                Assert.True(false, buildDiffText $"%0A{l}" $"%0A{r}")
