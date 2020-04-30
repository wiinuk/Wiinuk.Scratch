namespace Scratch.Transpiler
open FSharp.Quotations
open Scratch.Reflection
open Scratch.Transformers
open System.Runtime.InteropServices


type Loc = Location option Tagged

[<Struct; StructLayout(LayoutKind.Auto)>]
type SourceCode = private {
    expr: Expr
    isEmpty: bool
}
module SourceCode =
    let empty = { expr = Unchecked.defaultof<_>; isEmpty = true }
    let ofExpr e = { expr = e; isEmpty = false }
    let location s = if s.isEmpty then None else Expr.getLocation s.expr
    let tag e: Loc = Tagged.empty (location e)

    let buildText s = if s.isEmpty then "" else sprintf "%A" s.expr
    let buildErrorText s = if s.isEmpty then "" else sprintf "%A" s.expr
