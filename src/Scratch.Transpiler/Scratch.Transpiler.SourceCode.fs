namespace Scratch.Transpiler
open FSharp.Quotations
open Scratch.Reflection
open Scratch.Transformers
open System.Runtime.InteropServices
open System.Reflection


type Loc = Location option Tagged

[<Struct; StructLayout(LayoutKind.Auto)>]
type SourceCode = private {
    exprOrMethodOrNull: obj
}
module SourceCode =
    let empty = { exprOrMethodOrNull = null }
    let isEmpty s = isNull s.exprOrMethodOrNull
    let ofExpr e = { exprOrMethodOrNull = (e: Expr) }
    let ofMethod m = { exprOrMethodOrNull = (m: MethodBase) }
    let location s =
        match s.exprOrMethodOrNull with
        | :? Expr as e -> Expr.getLocation e
        | :? MethodBase as m ->
            Some {
                path = $"{m}|{m.ReflectedType}"
                position1 = { line = 0; column = 0 }
                position2 = { line = 0; column = 0 }
            }
        | _ -> None

    let tag e: Loc = Tagged.empty (location e)

    let buildText s =
        match s.exprOrMethodOrNull with
        | :? Expr as e -> $"%A{e}"
        | :? MethodBase as m -> $"{m}"
        | _ -> ""

    let buildErrorText s = buildText s
