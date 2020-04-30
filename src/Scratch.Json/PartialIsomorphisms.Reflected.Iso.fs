module Scratch.Json.PartialIsomorphisms.Reflected.Iso
open FSharp.Quotations
open Scratch.Primitives
open Scratch.Reflection
open Scratch.Json.PartialIsomorphisms
open Scratch.Json.PartialIsomorphisms.Iso


let isoFromUnion0 caseExpr = expressionFromUnion0 caseExpr |> compileToIso
let isoFromUnion1 caseExpr = expressionFromUnion1 caseExpr |> compileToIso
let isoFromUnion2 caseExpr = expressionFromUnion2 caseExpr |> compileToIso
let isoFromUnion3 caseExpr = expressionFromUnion3 caseExpr |> compileToIso
let isoFromUnion4 caseExpr = expressionFromUnion4 caseExpr |> compileToIso
let isoFromUnion5 caseExpr = expressionFromUnion5 caseExpr |> compileToIso

let isoFromRecord recordToTuple = expressionFromRecord recordToTuple |> compileToIso

module Values =
    let isoFromUnion0 (e: Expr<'Union>): OfFuncIOV<HUnit,'Union> =
        isoExpressionFromUnionNCore (TypedUnionCaseInfo.create0 e) |> compileToIsoR

    let isoFromUnion1 (e: Expr<'T1 -> 'Union>): OfFuncIOV<mvcons<'T1,HUnit>,'Union> =
        isoExpressionFromUnionN e |> compileToIsoR

    let isoFromUnion2 (e: Expr<'T1 * 'T2 -> 'Union>): OfFuncIOV<mvcons<'T1,mvcons<'T2,HUnit>>,'Union> =
        isoExpressionFromUnionN e |> compileToIsoR

    let isoFromUnion3 (e: Expr<'T1 * 'T2 * 'T3 -> 'Union>): OfFuncIOV<mvcons<'T1,mvcons<'T2,mvcons<'T3,HUnit>>>,'Union> =
        isoExpressionFromUnionN e |> compileToIsoR

    let isoFromUnion4 (e: Expr<'T1 * 'T2 * 'T3 * 'T4 -> 'Union>): OfFuncIOV<mvcons<'T1,mvcons<'T2,mvcons<'T3,mvcons<'T4,HUnit>>>>,'Union> =
        isoExpressionFromUnionN e |> compileToIsoR

    let isoFromUnion5 (e: Expr<'T1 * 'T2 * 'T3 * 'T4 * 'T5 -> 'Union>): OfFuncIOV<mvcons<'T1,mvcons<'T2,mvcons<'T3,mvcons<'T4,mvcons<'T5,HUnit>>>>>,'Union> =
        isoExpressionFromUnionN e |> compileToIsoR
    
    let isoFromUnion6 (e: Expr<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 -> 'Union>): OfFuncIOV<mvcons<'T1,mvcons<'T2,mvcons<'T3,mvcons<'T4,mvcons<'T5,mvcons<'T6,HUnit>>>>>>,'Union> =
        isoExpressionFromUnionN e |> compileToIsoR
    
    let isoFromUnion7 (e: Expr<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 -> 'Union>): OfFuncIOV<mvcons<'T1,mvcons<'T2,mvcons<'T3,mvcons<'T4,mvcons<'T5,mvcons<'T6,mvcons<'T7,HUnit>>>>>>>,'Union> =
        isoExpressionFromUnionN e |> compileToIsoR

    let isoFromUnion8 (e: Expr<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8 -> 'Union>): OfFuncIOV<mvcons<'T1,mvcons<'T2,mvcons<'T3,mvcons<'T4,mvcons<'T5,mvcons<'T6,mvcons<'T7,mvcons<'T8,HUnit>>>>>>>>,'Union> =
        isoExpressionFromUnionN e |> compileToIsoR

    let isoFromRecord e = isoExpressionFromRecord e |> compileToIsoR

