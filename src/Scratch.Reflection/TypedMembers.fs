namespace Scratch.Reflection
open System.Reflection
open FSharp.Reflection
open FSharp.Quotations
open Scratch.Primitives
open Scratch.Primitives.HList.Types
open Scratch.Reflection.Member
open Scratch.Reflection.Expr
module E = FSharp.Quotations.Patterns


[<Struct>]
[<NoComparison; NoEquality>]
type TypedPropertyInfo<'T,'P> = TypedPropertyInfo of PropertyInfo

type ITypedUnionCaseInfo<'Union> =
    abstract Raw: UnionCaseInfo

[<Struct>]
[<NoComparison; NoEquality>]
type TypedUnionCaseInfo<'Union,'T,'Fields,'FieldProperties when 'T :> HList and 'FieldProperties :> HList> =
    | TypedUnionCaseInfo of case: UnionCaseInfo * toExprs: ToListBox<'T,Expr> * fields: 'FieldProperties
with
    interface ITypedUnionCaseInfo<'Union> with
        member x.Raw = let (TypedUnionCaseInfo(case = case)) = x in case

[<Struct>]
[<NoComparison; NoEquality>]
type TypedUnionCaseInfoAny<'Union> =
    | TypedUnionCaseInfoAny of case: UnionCaseInfo * properties: PropertyInfo array
with
    interface ITypedUnionCaseInfo<'Union> with
        member x.Raw = let (TypedUnionCaseInfoAny(case, _)) = x in case

[<Struct>]
[<NoComparison; NoEquality>]
type TypedMethodInfo<'ParameterTypes,'ResultTypes> = TypedMethodInfo of MethodInfo

[<Struct>]
[<NoComparison; NoEquality>]
type TypedFieldInfo<'T,'F> = TypedFieldInfo of FieldInfo

type ILInfra = interface end
[<Struct>]
type RefPhantom<'a> = { ref: 'a } with
    interface ILInfra

module TypedFieldInfo =
    let create (field: Expr<'This -> 'Field>): TypedFieldInfo<'This,'Field> = findField field |> TypedFieldInfo

module TypedPropertyInfo =
    let raw (TypedPropertyInfo p) = p

    let getMethod (TypedPropertyInfo p: TypedPropertyInfo<'This,'Property>): TypedMethodInfo<HTuple<'This>,HTuple<'Property>> =
        TypedMethodInfo(p.GetGetMethod())

    let setMethod (TypedPropertyInfo p: TypedPropertyInfo<'This,'Property> when 'This : not struct): TypedMethodInfo<HTuple<'This,'Property>,HUnit> =
        TypedMethodInfo(p.GetSetMethod())

    let setMethodV (TypedPropertyInfo p: TypedPropertyInfo<'This,'Property> when 'This : struct): TypedMethodInfo<HTuple<RefPhantom<'This>,'Property>,HUnit> =
        TypedMethodInfo(p.GetSetMethod())

module TypedUnionCaseInfo =
    [<AutoOpen>]
    module private Privates =
        type P<'U,'X> = TypedPropertyInfo<'U,'X>
        type U<'U,'T,'Fs,'F when 'T :> HList and 'F :> HList> = TypedUnionCaseInfo<'U,'T,'Fs,'F>

        let findCase e =
            e
            |> tryPick (function E.NewUnionCase(c, _) | E.UnionCaseTest(_, c) -> Some c | _ -> None)
            |> function
                | Some c -> c, c.GetFields()
                | _ -> failwithf "e.g. <@ AnyUnionCase @>"

        let findCaseWithArity arity e =
            e
            |> tryPick (function E.NewUnionCase(c, _) | E.UnionCaseTest(_, c) -> Some c | _ -> None)
            |> function
                | Some c when c.GetFields().Length = arity -> c, c.GetFields()
                | _ -> failwith $"e.g. <@ AnyUnionCase @> when fieldCount = {arity}"

        [<Struct>]
        type ExprLub<'T1,'T2> = | ExprLub
        with
            interface ILub<'T1 Expr, 'T2 Expr, Expr> with
                override _.Left x = upcast x
                override _.Right x = upcast x

    let raw (case: 'C when 'C :> ITypedUnionCaseInfo<_> and 'C : struct) = case.Raw
    let tag case = raw(case).Tag
    let name case = raw(case).Name
    let fields (TypedUnionCaseInfo(fields = fields)) = fields
    let declaringType case = raw(case).DeclaringType

    let constructor (TypedUnionCaseInfo(case, _, _): TypedUnionCaseInfo<'U,'T,'Fs,'Ps>): TypedMethodInfo<'Fs,HCons<'U,HUnit>> =
        let m = FSharpValue.PreComputeUnionConstructorInfo(case, allowAccessToPrivateRepresentation = true)
        TypedMethodInfo m

    let createAny (e: Expr<'Args -> 'U>): TypedUnionCaseInfoAny<'Union> =
        let c, fs = findCase e
        TypedUnionCaseInfoAny(c, fs)

    let create0 (e: Expr<'U>): U<'U,HUnit,HUnit,HUnit> =
        TypedUnionCaseInfo(findCaseWithArity 0 e |> fst, ToListBox HUnitToList, HUnit)

    let create1 (e: Expr<'T1 -> 'U>): U<'U, HTuple<'T1 Expr>, HTuple<'T1>, HTuple<P<'U,'T1>>> =
        let c, fs = findCaseWithArity 1 e
        TypedUnionCaseInfo(c, ToListBox(HSingleToList ExprLub), TypedPropertyInfo fs.[0]^^HUnit)

    let create2 (e: Expr<('T1 * 'T2) -> 'U>): U<'U, HTuple<'T1 Expr,'T2 Expr>, HTuple<'T1,'T2>, HTuple<P<'U,'T1>,P<'U,'T2>>> =
        let c, fs = findCaseWithArity 2 e
        TypedUnionCaseInfo(
            c,
            ToListBox(HListToList(ExprLub, HSingleToList ExprLub)),
            TypedPropertyInfo fs.[0]^^TypedPropertyInfo fs.[1]^^HUnit
        )

    let create3 (e: Expr<('T1 * 'T2 * 'T3) -> 'U>): U<'U,HTuple<'T1 Expr,'T2 Expr,'T3 Expr>, HTuple<'T1,'T2,'T3>, HTuple<P<'U,'T1>,P<'U,'T2>,P<'U,'T3>>> =
        let c, fs = findCaseWithArity 3 e
        TypedUnionCaseInfo(
            c,
            ToListBox(HListToList(ExprLub, HListToList(ExprLub, HSingleToList ExprLub))),
            TypedPropertyInfo fs.[0]^^TypedPropertyInfo fs.[1]^^TypedPropertyInfo fs.[2]^^HUnit
        )

    let create4 (e: Expr<('T1 * 'T2 * 'T3 * 'T4) -> 'U>): U<'U, HTuple<'T1 Expr,'T2 Expr,'T3 Expr,'T4 Expr>, HTuple<'T1,'T2,'T3,'T4>, HTuple<P<'U,'T1>,P<'U,'T2>,P<'U,'T3>,P<'U,'T4>>> =
        let c, fs = findCaseWithArity 4 e
        TypedUnionCaseInfo(
            c,
            ToListBox(HListToList(ExprLub, HListToList(ExprLub, HListToList(ExprLub, HSingleToList ExprLub)))),
            TypedPropertyInfo fs.[0]^^TypedPropertyInfo fs.[1]^^TypedPropertyInfo fs.[2]^^TypedPropertyInfo fs.[3]^^HUnit
        )

    let create5 (e: Expr<('T1 * 'T2 * 'T3 * 'T4 * 'T5) -> 'U>): U<'U, HTuple<'T1 Expr,'T2 Expr,'T3 Expr,'T4 Expr,'T5 Expr>, HTuple<'T1,'T2,'T3,'T4>, HTuple<P<'U,'T1>,P<'U,'T2>,P<'U,'T3>,P<'U,'T4>,P<'U,'T5>>> =
        let c, fs = findCaseWithArity 5 e
        TypedUnionCaseInfo(
            c,
            ToListBox(HListToList(ExprLub, HListToList(ExprLub, HListToList(ExprLub, HListToList(ExprLub, HSingleToList ExprLub))))),
            TypedPropertyInfo fs.[0]^^TypedPropertyInfo fs.[1]^^TypedPropertyInfo fs.[2]^^TypedPropertyInfo fs.[3]^^TypedPropertyInfo fs.[4]^^HUnit
        )
