module internal Scratch.Transpiler.PredefinedPlugin.Helpers
open FSharp.Reflection
open Scratch
open Scratch.MemoryModel
open Scratch.MemoryModel.TypeSpec
open Scratch.Primitives
open Scratch.Primitives.Field
open Scratch.Reflection
open Scratch.Reflection.Expr
open Scratch.Reflection.Member
open Scratch.Ast
open Scratch.Transformers
open Scratch.Transpiler
open Scratch.Transpiler.Format
open Scratch.Transpiler.Environments
open Scratch.Transpiler.Core
open Scratch.IR
open System.Runtime.CompilerServices
open System.Text.RegularExpressions

module A = Scratch.Ast.Expression
module A = Scratch.Ast.Expressions
module E = FSharp.Quotations.Patterns
module Exp = Exp.Op
type private QVar = Quotations.Var

//let getPrimitiveStorage source spec =
//    match getStorage (SourceCode.location source) spec with
//    | [e] -> Ok e
//    | es -> TypeSizeMismatch(actual = List.length es, expected = 1) |> raiseError source

let inline transpilePrimitiveBinaryCallExpression senv x1 x2 call make =
    let e1 = transpilePrimitiveExpression senv x1
    let e2 = transpilePrimitiveExpression senv x2
    make (getLocation call |> Tagged.empty) e1 e2 |> Ok

let transpileExpression senv e = transpileExpression senv e |> Ok
let transpilePrimitiveExpression senv e = transpilePrimitiveExpression senv e |> Ok
let context = ResultBuilder
let inline skip() = Error None

[<MethodImpl(MethodImplOptions.NoInlining)>]
let raiseError source error =
    let t = System.Diagnostics.StackTrace(1, true)
    TranspileError(error, source, Some t) |> Some |> Error

let underlyingMemberType env s t = context {
    match underlyingMemberType env t with
    | None -> return! InvalidExpressionType t |> raiseError s
    | Some t -> return t
}

let transpileBlock senv proc e = context {
    let env = BlockEnvironments.make({ proc = proc }, senv.e)
    let state = ref senv.s.contents
    let stats = transpileBlock { s = state; e = env } e
    senv.s.contents <- state.contents
    return stats, state.contents
}
module Var =
    let newVarFromQVar env source (v: QVar) = context {
        let! t = underlyingMemberType env source v.Type
        return Var.newStorage v.Name v.IsMutable t
    }

let registerLambdaProcedure senv name atomicity = context {
    let config = get FConfig senv.e
    let name = match name with None -> config.lambdaName | Some n -> n
    let var = Var.newProc name [] ExpTypes.empty
    return {
        procedureVar = var

        procedureThis = None
        parameters = []
        atomicity = atomicity
        isExport = NoExport
        inlining = None
    }
}
let registerLocalLambdaProcedure senv atomicity = context {
    let config = get FConfig senv.e
    let varName = (get FBlockEnvironment senv.e).proc.procedureVar |> Var.name
    let baseName = varName // match varName with None -> config.lambdaName | Some n -> n
    let name = baseName + config.namespaceSeparator + config.localName
    return! registerLambdaProcedure senv (Some name) atomicity
}
//let transpileAndAcc senv transpiler e = context {
//    let Get FExpressionState { acc = acc } & state = senv.s.contents
//    senv.s := state |> wiz FExpressionState { acc = [] }
//    let! e = transpiler senv e
//    let Get FExpressionState { acc = eAcc } & state = senv.s.contents
//    senv.s := state |> wiz FExpressionState { acc = acc }
//    return eAcc, e
//}
//let transpileExpressionAndAcc senv e = transpileAndAcc senv transpileExpression e
//let transpilePrimitiveExpressionAndAcc senv e = transpileAndAcc senv transpilePrimitiveExpression e

let registerAndTranspileLocalLambda senv e = context {
    let! lambda = registerLocalLambdaProcedure senv Atomic
    let! stats, _ = transpileBlock senv lambda e

    let script = Top.proc (getLoc e) lambda.procedureVar Atomic [] stats
    do addScriptData senv { x = 0.; y = 0.; script = script }
    return lambda
}

type OutEnv<'BlockEnvironments> = {
    outEnv: 'BlockEnvironments
    lastOutAsNewLine: bool
    formatSource: SourceCode
    out: ProcedureSpec
    outLine: ProcedureSpec
}

let outString senv source isLast s =
    let { lastOutAsNewLine = lastOutAsNewLine; out = outS; outLine = outLineS } = senv.e
    let out = if lastOutAsNewLine && isLast then outLineS else outS
    Exp.call (SourceCode.tag source) out.procedureVar [s]

let outStringLiteral senv isLast s =
    let { formatSource = formatS } = senv.e
    outString senv formatS isLast (Exp.string (SourceCode.tag formatS) s)

let outValue senv isLast (ExprType argT & argE) = context {
    let argSource = SourceCode.ofExpr argE

    match underlyingPrimitiveType argT with
    | None -> return! InvalidInterpolationCall(InvalidFormatArgmentType argT) |> raiseError argSource

    // "%s", <@ "abc" @> -> `"abc"`
    // "%s", <@ "" @> -> `""`
    | Some(Typed SType.S) ->
        let! arg = transpilePrimitiveExpression { e = senv.e.outEnv; s = senv.s } argE
        return outString senv (SourceCode.ofExpr argE) isLast arg

    // "%f", <@ 12.34 @> -> `12.34 :> S`
    // "%b", <@ true @> -> `true :> S`
    | Some _ ->
        let! arg = transpilePrimitiveExpression { e = senv.e.outEnv; s = senv.s } argE
        let s = SourceCode.ofExpr argE
        let a = SourceCode.tag s
        return outString senv s isLast (Exp.toS a arg)
}

let localOutEnv senv lastOutAsNewLine formatS callS =
    let outM, _ = findMethod <@ Scratch.ConsoleOperations.out @>
    let outS = lookupOrRaiseProcedure callS (methodId outM) senv.e
    let outLineM, _ = findMethod <@ Scratch.ConsoleOperations.outLine @>
    let outLineS = lookupOrRaiseProcedure callS (methodId outLineM) senv.e
    let env = { outEnv = senv.e; lastOutAsNewLine = lastOutAsNewLine; formatSource = formatS; out = outS; outLine = outLineS }
    let senv = { e = env; s = senv.s }
    senv

let formatCallToOutCall senv lastOutAsNewLine formatL head tail source = context {
    let senv = localOutEnv senv lastOutAsNewLine formatL source

    let head =
        match tail with

        // <@ printf "" @> -> <@ out "" @>
        // <@ printfn "" @> -> <@ outLine "" @>
        | [] -> outStringLiteral senv true head

        // <@ printf "%s" ... @>
        | _ -> outStringLiteral senv false head

    let rec aux senv xs = context {
        match xs with
        | [] -> return []
        | [_, argE, ""] ->
            let! v = outValue senv true argE
            return [v]

        | [_, argE, s] ->
            let! v = outValue senv false argE
            let s = outStringLiteral senv true s
            return [v; s]

        | (_, argE, "")::xs ->
            let! v = outValue senv false argE
            let! os = aux senv xs
            return v::os

        | (_, argE, s)::xs ->
            let! v = outValue senv false argE
            let s = outStringLiteral senv false s
            let! os = aux senv xs
            return v::s::os
    }

    let! tail = aux senv tail
    return Exp.concat (SourceCode.tag source) (head::tail)
}

let iwordToConstant l u = Exp.lit l (toV u)

let inline binaryTypePred3 pred make senv = function
    | struct((_, [t1; t2; tr], [x1; x2]), e) when pred t1 t2 tr -> transpilePrimitiveBinaryCallExpression senv x1 x2 e make
    | _ -> skip()

let inline binaryTypePred1 pred make senv = function
    | struct((_, [t1], [x1; x2]), e) when pred t1 -> transpilePrimitiveBinaryCallExpression senv x1 x2 e make
    | _ -> skip()

let inline unary f senv = function
    | struct((_, _, [x1]), _) -> f senv x1
    | _ -> skip()

let inline unaryWithE f senv = function
    | struct((_, _, [x1]), e) -> f senv x1 e
    | _ -> skip()

let inline unaryWithS f senv e =
    unaryWithE (fun senv x1 e -> f senv x1 (SourceCode.ofExpr e)) senv e

let inline single e f x = [e], f x

let inline unaryType1WithE f senv = function
    | struct((_, [t1], [e1]), e) -> f senv t1 e1 e
    | _ -> skip()

let inline unaryType2WithS f senv = function
    | struct((_, [t1;t2], [e1]), e) -> f senv t1 t2 e1 (SourceCode.ofExpr e)
    | _ -> skip()

let inline nullaryWithS f senv = function
    | struct((_, _, []), e) -> f senv (SourceCode.ofExpr e)
    | _ -> skip()

let inline binaryWithE f senv = function
    | struct((_, _, [e1; e2]), e) -> f senv e1 e2 e
    | _ -> skip()

let inline binaryWithS f senv e =
    binaryWithE (fun senv e1 e2 e -> f senv e1 e2 (SourceCode.ofExpr e)) senv e

let inline binaryPrimitive f = binaryWithS <| fun senv e1 e2 s -> context {
    let! e1 = transpilePrimitiveExpression senv e1
    let! e2 = transpilePrimitiveExpression senv e2
    return f (SourceCode.tag s) e1 e2
}
let inline ternaryWithS f senv = function
    | struct((_, _, [e1; e2; e3]), e) -> f senv e1 e2 e3 (SourceCode.ofExpr e)
    | _ -> skip()

let inline mathFunction f senv struct(args, e) = context {
    match args with
    | _, ts, [e1] when List.forall ((=) typeof<double>) ts ->
        let! e1 = transpilePrimitiveExpression senv e1
        return Exp.``computeFunction:of:`` (getLoc e) f e1
    | _ -> return! skip()
}

let inline spriteInstanceCallWithS (template: Quotations.Expr<Sprite -> _>) f = [template :> Quotations.Expr], fun senv struct(args, e) ->
    match args with
    | Some(SpriteOrProcedureThis senv ()), _, args -> f senv args (SourceCode.ofExpr e)
    | _ -> skip()

let casesToMap key cases =
    seq {
        for es, f in cases do
            for e in es -> key e, OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
    }
    |> Map.ofSeq

let methodCasesToMap cases = casesToMap (findMethod >> fst >> MemberId.methodId) cases

let tryFindMethod (m: System.Reflection.MethodInfo) map =
    let md = if m.IsGenericMethod then m.GetGenericMethodDefinition() else m
    let id = MemberId.methodId md
    match Map.tryFind id map with
    | ValueNone -> ValueNone
    | ValueSome f ->
        let ts = if m.IsGenericMethod then m.GetGenericArguments() |> Array.toList else []
        ValueSome struct(f, ts)

let (|CallMap|_|) map senv = function
    | E.Call(this, m, args) as e ->
        match tryFindMethod m map with
        | ValueNone -> None
        | ValueSome struct(f: OptimizedClosures.FSharpFunc<_,_,_>, ts) -> Some(f.Invoke(senv, struct((this, ts, args), e)))
    | _ -> None

let memoryId = propertyId <| findProperty <@@ Memory.memory @@>
let lookupOrRaiseMemory senv source = context {
    return lookupOrRaiseList source memoryId senv.e
}

let stopAsAnyValue env source t = context {
    let! t = underlyingMemberType env source t
    let l = SourceCode.tag source
    let stop = Exp.stopScripts l StopScriptKind.All
    let anyValues =
        match t with
        | UnboxedType ts ->
            ts
            |> List.map (fun t ->
                match MemberType.underlyingType t with
                | Any -> Exp.reinterpret l (Exp.string l "") ExpTypes.any
                | Typed t -> Exp.lit l (SType.parameterDefaultValue t)
            )
            |> Exp.newTuple l

    return Exp.seq l stop anyValues
}

let addPropertyAsList senv source export p =
    if lookupProperty p (get FExternalItemState senv.s.contents).externalEnv |> VOption.isSome then () else

    let varName = (get FConfig (get FExternalItemState senv.s.contents).externalEnv).propertyName p
    let var = Var.newSimple varName

    let state = ref <| States.make (get FExternalItemState senv.s.contents).externalState
    let spec = registerList { s = state; e = (get FExternalItemState senv.s.contents).externalEnv } source export None var NoPersistent IArray.empty

    modifyRef FExternalItemState senv.s <| fun s -> { s with externalState = get FState state.contents }
    addExternalSpec senv (propertyId p) (ListSpec spec)
    modifyRef FExternalItemState senv.s <| fun s ->
        let l = SourceCode.location source
        let (@+) e l = withLocationTyped l e
        let init = <@ Scratch.SListOperations.defineList %(<@ []: IWord list @> @+ l) @> @+ l
        
        { s with externalAcc = ExternalList(spec, init)::s.externalAcc }

let (@+) e l = withLocation l e
