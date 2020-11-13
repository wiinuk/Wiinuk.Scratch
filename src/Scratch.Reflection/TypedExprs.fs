namespace Scratch.Reflection
open System
open FSharp.Quotations
open Scratch.Primitives
open Scratch.Reflection
module E = FSharp.Quotations.Patterns


[<Struct>]
type TypedVar<'T> internal (var: Var) =
    new (name, ?isMutable) = TypedVar(Var(name, typeof<'T>, ?isMutable = isMutable))
    member _.Raw = var

module TypedExpr =
    [<RequiresExplicitTypeArguments>]
    let CastVar<'T> (untypedVar: Var) =
        if typeof<'T> <> untypedVar.Type then
            $"untypedVar: {untypedVar} = type: %s{typeof<'T>.FullName}"
            |> InvalidCastException
            |> raise

        TypedVar<'T> untypedVar

    let Value(value: 'T) = E.Value value |> E.Cast<'T>
    let Var(variable: 'T TypedVar) = E.Var variable.Raw |> E.Cast<'T>
    let VarSet(variable: 'T TypedVar, value: 'T Expr) = E.VarSet(variable.Raw, value) |> E.Cast<unit>
    let Let(letVariable: 'T TypedVar, letExpr: 'T Expr, body: 'R Expr) = E.Let(letVariable.Raw, letExpr, body) |> E.Cast<'R>
    let Lambda(parameter: 'T TypedVar, body: 'R Expr) = E.Lambda(parameter.Raw, body) |> E.Cast<'T -> 'R>
    let NewUnionCase(TypedUnionCaseInfo(case, toList, _): TypedUnionCaseInfo<'U,'T,'Fs,'F>, es) =
        E.NewUnionCase(case, HList.toList toList es) |> E.Cast<'U>

    let UnionCaseTest(e: 'U Expr, TypedUnionCaseInfo(case = case): TypedUnionCaseInfo<'U,_,_,_>) =
        E.UnionCaseTest(e, case) |> E.Cast<bool>

    let InstancePropertyGet(this: 'T Expr, TypedPropertyInfo p: TypedPropertyInfo<'T,'P>): 'P Expr =
        E.PropertyGet(this, p) |> E.Cast<'P>
