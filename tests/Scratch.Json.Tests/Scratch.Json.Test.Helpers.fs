module Scratch.Json.Test.Helpers
open System.Linq
open Scratch
open Scratch.Ast
open Scratch.AstDefinitions
open Scratch.Serialization
open FsCheck


module Diff =
    open DiffMatchPatch
    open System
    open System.Collections.Generic
    open System.Text
    open System.Text.RegularExpressions
    type String with
        member s.JavaSubstring(``begin``, ``end``) = s.Substring(``begin``, ``end`` - ``begin``)

    let private spaces = [| for c in Char.MinValue..Char.MaxValue do if Char.IsWhiteSpace c then c |]

    type DiffMatchPatch with
        member private _.diff_linesToWordsMunge(text, lineArray: ResizeArray<_>, lineHash: Dictionary<_,_>, maxLines) =
            let mutable lineStart = 0
            let mutable lineEnd = -1
            let mutable line = ""
            let chars = StringBuilder()
            let r = Regex
            while lineEnd < String.length text - 1 do
                lineEnd <- text.IndexOfAny(spaces, lineStart)
                if lineEnd = -1 then
                    lineEnd <- text.Length - 1

                line <- text.JavaSubstring(lineStart, lineEnd + 1)

                if lineHash.ContainsKey line then
                    chars.Append(char lineHash[line]) |> ignore
                else
                    if lineArray.Count = maxLines then
                        line <- text.Substring lineStart
                        lineEnd <- text.Length

                    lineArray.Add line
                    lineHash.Add(line, lineArray.Count - 1)
                    chars.Append(char (lineArray.Count - 1)) |> ignore

                lineStart <- lineEnd + 1

            chars.ToString()

        member d.DiffLinesToWords(text1, text2) =
          let lineArray = ResizeArray()
          let lineHash = System.Collections.Generic.Dictionary()
          // e.g. linearray[4] == "Hello\n"
          // e.g. linehash.get("Hello\n") == 4

          // "\x00" is a valid character, but various debuggers don't like it.
          // So we'll insert a junk entry to avoid generating a null character.
          lineArray.Add ""

          // Allocate 2/3rds of the space for text1, the rest for text2.
          let chars1 = d.diff_linesToWordsMunge(text1, lineArray, lineHash, 40000)
          let chars2 = d.diff_linesToWordsMunge(text2, lineArray, lineHash, 65535)
          chars1, chars2, lineArray

        member d.DiffMainLines(text1, text2) =
            let text1, text2, lineArray = d.DiffLinesToChars(text1, text2)
            let ds = d.DiffMain(text1, text2, false)
            d.DiffCharsToLines(ds, lineArray)
            d.DiffCleanupSemantic ds
            ds
        member d.DiffMainWords(text1, text2) =
            let text1, text2, lineArray = d.DiffLinesToWords(text1, text2)
            let ds = d.DiffMain(text1, text2, false)
            d.DiffCharsToLines(ds, lineArray)
            d.DiffCleanupSemantic ds
            ds

    let printDiffLine ds =
        let o = obj()
        let writeWithColor c x = lock o <| fun _ ->
            let c' = Console.ForegroundColor
            Console.ForegroundColor <- c
            Console.Write (x + "")
            Console.ForegroundColor <- c'
        ds
        |> Seq.iter (fun { Text = text; Operation = op } ->
            match op with
            | Equal -> Console.Write text
            | Delete -> writeWithColor ConsoleColor.Red text
            | Insert -> writeWithColor ConsoleColor.Green text
        )
        printfn ""

    let prettyDiff ds =
        ds
        |> Seq.map (fun x ->
            let s = x.Text
            match x.Operation with
            | Equal -> s
            | Delete -> "[- " + s + " -]"
            | Insert -> "[+ " + s + " +]"
        )
        |> String.concat ""

    let diffWords text1 text2 = DiffMatchPatch.Default.DiffMainWords(text1, text2)
