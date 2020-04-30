namespace Scratch.Reflection
open System
open System.Linq.Expressions
open Scratch.Primitives
open Scratch.Reflection
type private E = Linq.Expressions.Expression


type ITypedExpression<'E> =
    abstract Raw: 'E

type ITypedExpression<'T,'E> =
    inherit ITypedExpression<'E>

[<Struct; NoComparison>]
type TypedParameterExpression<'T> = TypedParameterExpression of ParameterExpression with
    interface ITypedExpression<'T, ParameterExpression> with
        member e.Raw = let (TypedParameterExpression e) = e in e

[<Struct; NoComparison>]
type TypedExpression<'T> = TypedExpression of Expression with
    interface ITypedExpression<'T, Expression> with
        member e.Raw = let (TypedExpression e) = e in e

module TypedExpression =
    let raw (x: 'T when 'T :> ITypedExpression<_> and 'T : struct) = x.Raw
    let rawParameterOf (x: 'T when 'T :> ITypedExpression<_> and 'T : struct) = x.Raw :> ParameterExpression
    let rawOf (_: 'T The) (x: 'E when 'E :> ITypedExpression<'T,_> and 'E : struct) = x.Raw :> Expression

    [<RequiresExplicitTypeArguments>]
    let fromRawUnchecked<'T> x: 'T TypedExpression = TypedExpression x

    let internal verifyType<'T> t =
        if typeof<'T>.IsAssignableFrom t then ()
        elif typeof<'T> = typeof<unit> then ()
        else failwithf "expected type: %A, actual type: %A" typeof<'T>.FullName t.FullName

open TypedExpression


[<AbstractClass; Sealed>]
type TypedExpression =

    [<RequiresExplicitTypeArguments>]
    static member Parameter<'T>(name, isByRef): 'T TypedParameterExpression =
        let t = typeof<'T>
        let t = if isByRef then t.MakeByRefType() else t
        TypedParameterExpression(E.Parameter(t, name))

    [<RequiresExplicitTypeArguments>]
    static member Parameter<'T> name = TypedExpression.Parameter<'T>(name, isByRef = false)

    static member Raw expression = raw expression

    [<RequiresExplicitTypeArguments>]
    static member FromRaw<'T>(parameter: ParameterExpression): 'T TypedParameterExpression =
        verifyType<'T> parameter.Type
        TypedParameterExpression parameter

    [<RequiresExplicitTypeArguments>]
    static member FromRaw<'T>(expression: Expression) =
        verifyType<'T> expression.Type
        fromRawUnchecked<'T> expression

    static member Block(variables, expressions) =
        if Seq.isEmpty expressions then fromRawUnchecked<unit>(E.Empty()) else

        let expressions = Seq.map (rawOf the<unit>) expressions
        let variables = Seq.map rawParameterOf variables 
        fromRawUnchecked<unit>(E.Block(variables, expressions))

    static member Block(variables, expressions, expression) =
        let variables = Seq.map rawParameterOf variables
        let exprs = Seq.map (rawOf the<unit>) expressions
        fromRawUnchecked<'T> (E.Block(variables, Seq.append exprs [rawOf the<'T> expression]))

    static member Sequential(arg0, arg1) =
        fromRawUnchecked<'T>(E.Block(rawOf the<unit> arg0, rawOf the<'T> arg1))

    static member Sequential(arg0, arg1, arg2) =
        fromRawUnchecked<'T>(E.Block(rawOf the<unit> arg0, rawOf the<unit> arg1, rawOf the<'T> arg2))

    static member Sequential expressions =
        if Seq.isEmpty expressions then fromRawUnchecked<unit>(E.Empty()) else

        let expressions = expressions |> Seq.map (rawOf the<unit>)
        fromRawUnchecked<unit>(E.Block expressions)

    static member Assign(left, right) =
        fromRawUnchecked<unit>(E.Assign(rawOf the<'T> left, rawOf the<'T> right))

    static member Call(``method``, arg1, arg2) =
        let (TypedMethodInfo m): TypedMethodInfo<HTuple<'T1,'T2>, HTuple<'R>> = ``method``
        fromRawUnchecked<'R>(E.Call(m, rawOf the<'T1> arg1, rawOf the<'T2> arg2))

    static member Field(expression, field)=
        let (TypedFieldInfo f): TypedFieldInfo<'This,'Field> = field
        fromRawUnchecked<'Field>(E.Field(rawOf the<'This> expression, f))

    static member Constant(value: 'T) = fromRawUnchecked<'T>(E.Constant(value, typeof<'T>))

    static member Lambda(parameters: HCons<#ITypedExpression<'T1,_>, HCons<#ITypedExpression<'T2,_>, HUnit>>, body) =
        let { head = param1; tail = { head = param2; tail = HUnit } } = parameters
        let param1 = rawParameterOf param1
        let param2 = rawParameterOf param2
        let body = rawOf the<'R> body
        E.Lambda<FuncIOV<'T1,'T2,'R>>(body, param1, param2)

    static member Condition(test, ifTrue, ifFalse) =
        fromRawUnchecked<'T>(E.Condition(rawOf the<bool> test, rawOf the<'T> ifTrue, rawOf the<'T> ifFalse))

    static member Equal(left, right) =
        fromRawUnchecked<bool>(E.Equal(rawOf the<'T> left, rawOf the<'T> right))

    static member Property(instance, property) =
        let (TypedPropertyInfo p): TypedPropertyInfo<'This,'Property> = property
        fromRawUnchecked<'Property>(E.Property(rawOf the<'This> instance, p))
