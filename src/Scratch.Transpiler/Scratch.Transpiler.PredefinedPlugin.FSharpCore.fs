module internal Scratch.Transpiler.PredefinedPlugin.FSharpCore
open FSharp.Reflection
open Scratch
open Scratch.Ast
open Scratch.IR
open Scratch.MemoryModel
open Scratch.MemoryModel.TypeSpec
open Scratch.Primitives.Field
open Scratch.Reflection
open Scratch.Reflection.Expr
open Scratch.Reflection.Member
open Scratch.Transformers
open Scratch.Transpiler
open Scratch.Transpiler.Format
open Scratch.Transpiler.PredefinedPlugin.Helpers
open System

module E = Quotations.Patterns
module E = Quotations.DerivedPatterns
module A = Scratch.Ast.Expression
module A = Scratch.Ast.Expressions
module Exp = Exp.Op
type private E = Quotations.Expr
type private V = Reflection.FSharpValue


[<AutoOpen>]
module private Helpers =
    let tryGetBinaryOpMethod operatorMethodName (t1: Type) (t2: Type) =
        let m1 = t1.GetMethod(operatorMethodName, [|t1; t2|])
        let m2 = if t1.Equals t2 then null else t2.GetMethod(operatorMethodName, [|t1; t2|])
        match m1, m2 with 
        | null, null -> None
        | m, null | null, m -> Some m
        | _ -> None

    let (|BinaryOpMethodCall|_|) operatorMethodName = function
        | _, [t1; t2; _], [e1; e2] ->
            match tryGetBinaryOpMethod operatorMethodName t1 t2 with
            | Some m -> Some(m, e1, e2)
            | None -> None
        | _ -> None

    let (|UnwrapFSharpCoreInlineOpMethod|_|) = function
        | E.SpecificCall <@ (+) @> (BinaryOpMethodCall "op_Addition" (m, e1, e2))
        | E.SpecificCall <@ (-) @> (BinaryOpMethodCall "op_Subtraction" (m, e1, e2))
        | E.SpecificCall <@ (*) @> (BinaryOpMethodCall "op_Multiply" (m, e1, e2))
        | E.SpecificCall <@ (/) @> (BinaryOpMethodCall "op_Division" (m, e1, e2))
        | E.SpecificCall <@ (%) @> (BinaryOpMethodCall "op_Modulus" (m, e1, e2)) ->
            Some(m, [e1; e2])

        | E.SpecificCall <@ (~-) @> (_, [t1], [e1]) ->
            let m1 = t1.GetMethod("op_UnaryNegation", [|t1|])
            match m1 with
            | null -> None
            | m -> Some(m, [e1])

        | _ -> None

    let transformInlineOperatorCall = function
        | UnwrapFSharpCoreInlineOpMethod(m, args) as e ->
            E.Call(m, args)
            |> withLocation (getLocation e)
            |> Some

        | _ -> None

    let preTransforms = [transformInlineOperatorCall]

    let externalItemsPlugin' senv e = context {
        match e with
        | UnwrapFSharpCoreInlineOpMethod(m, args) as e ->
            E.Call(m, args)
            |> withLocation (getLocation e)
            |> transpileExternalItemSpecs senv
        
        | _ -> return! skip()
    }
    let (|NaNGet|_|) : E -> _ = (|SpecificPropertyGet|_|) <@@ nan @@>

let preTransformPlugin _ e = Reflection.Transformers.Transformer.transform 100 preTransforms e |> Some
let externalItemsPlugin() = { new ExternalItemsPluginProcess() with member _.Invoke senv e = externalItemsPlugin' senv e }

let fsharpCoreCallExpressions() = [

    // <@ ignore $e @> => `$e :> empty`
    single <@@ ignore @@> unaryWithS <| fun senv e1 e -> context {
        let! v = transpileExpression senv e1
        return Exp.toEmpty (SourceCode.tag e) v
    }
    // <@ failwith $m @> => `outLine $m; $stopAllSctipts :?> $resultType`
    single <@@ failwith @@> unaryWithE <| fun senv format e -> context {
        let callS = SourceCode.ofExpr e
        let formatS = SourceCode.ofExpr format
        let! format = transpilePrimitiveExpression senv format
        let out = outString (localOutEnv senv true formatS callS) callS true format
        let! stop = stopAsAnyValue senv.e callS e.Type
        return Exp.seq (SourceCode.tag callS) out stop
    }
    // <@ $x1 + $x2 @>
    [ <@@ (+) @@> ], fun senv struct(args, e) ->
        match args with
        | _, [t1; t2; tr], [x1; x2] ->
            if isConcatType t1 t2 tr then transpilePrimitiveBinaryCallExpression senv x1 x2 e Exp.``concatenate:with:``
            elif isNumericBinaryType t1 t2 tr || isAddressAddType t1 t2 tr then transpilePrimitiveBinaryCallExpression senv x1 x2 e Exp.``+``
            else skip()
        | _ -> skip()

    // <@ $x1 - $x2 @>
    [ <@@ (-) @@> ], binaryTypePred3 isSubtractType Exp.``-``

    // <@ $x1 * $x2 @>
    [ <@@ (*) @@> ], binaryTypePred3 isNumericBinaryType Exp.``*``

    // <@ $x1 / $x2 @>
    [ <@@ (/) @@> ], binaryTypePred3 isNumericBinaryType Exp.``/``

    // <@ $x1 % $x2 @>
    [ <@@ (%) @@> ], binaryTypePred3 isNumericBinaryType Exp.``%``

    // <@ -(%e) @> => <@ -1 * %e @>
    [ <@@ Operators.(~-) @@> ], unaryType1WithE <| fun senv t1 e1 e -> context {
        match underlyingPrimitiveType t1 with
        | Some(Typed SType.N) ->
            let! e1 = transpilePrimitiveExpression senv e1
            let l = getLoc e
            return Exp.``*`` l (Exp.number l -1.) e1
        | _ -> return! skip()
    }

    // <@ $x1 = $x2 @>
    [ <@@ (=) @@> ], binaryTypePred1 isPrimitiveType Exp.``=``

    // <@ $x1 <> $x2 @> -> `not ($x1 = $x2)`
    [ <@@ (<>) @@> ], binaryTypePred1 isPrimitiveType <| fun l x1 x2 -> Exp.not l (Exp.``=`` l x1 x2)

    // <@ $x1 < $x2 @>
    [ <@@ (<) @@> ], binaryTypePred1 isPrimitiveType Exp.``<``

    // <@ $x1 > $x2 @>
    [ <@@ (>) @@> ], binaryTypePred1 isPrimitiveType Exp.``>``

    // <@ $x1 <= $x2 @> -> `not ($x1 > $x2)`
    [ <@@ (<=) @@> ], binaryTypePred1 isPrimitiveType <| fun l x1 x2 -> Exp.not l (Exp.``>`` l x1 x2)

    // <@ $x1 >= $x2 @> -> `not ($x1 < $x2)`
    [ <@@ (>=) @@> ], binaryTypePred1 isPrimitiveType <| fun l x1 x2 -> Exp.not l (Exp.``<`` l x1 x2)

    // <@ floor %e1 @>
    [ <@@ Operators.floor @@>], mathFunction MathFunctionKind.Floor
    // <@ ceil %e1 @>
    [ <@@ Operators.ceil @@> ], mathFunction MathFunctionKind.Ceiling
    // <@ log10 %e1 @>
    [ <@@ Operators.log10 @@> ], mathFunction MathFunctionKind.Log
    // <@ log %e1 @>
    [ <@@ Operators.log @@> ], mathFunction MathFunctionKind.Ln
    // <@ abs %e1 @>
    [ <@@ Operators.abs @@> ], mathFunction MathFunctionKind.Abs
    // <@ sqrt %e1 @>
    [ <@@ Operators.sqrt @@> ], mathFunction MathFunctionKind.Sqrt

    // TODO: 新しいファイル ( `PredefinedPlugin.System.fs` など ) を作って移動
    [ <@@ Math.Round: float * MidpointRounding -> _ @@> ], fun senv struct(args, e) -> context {
        match args with
        | _, _, [e1; E.Value(:? MidpointRounding as v, t)] when t = typeof<MidpointRounding> && v = MidpointRounding.AwayFromZero ->
            let! e1 = transpilePrimitiveExpression senv e1
            return Exp.rounded (getLoc e) e1
        | _ -> return! skip()
    }
    // <@ max %e1 %e2 @> => <@ let temp1 = %e1; let temp2 = %e2; if temp1 < temp2 then temp2 else temp1 @>
    [ <@@ Operators.max @@> ], fun senv struct(args, e) -> context {
        match args with
        | _, [t], [e1; e2] when t = typeof<double> || t = typeof<int> ->
            let! e1 = transpileExpression senv e1
            let! e2 = transpileExpression senv e2
            let a = getLoc e
            let temp = (get FConfig senv.e).localName
            return
                Exp.letScope a temp e1 <| fun temp1 ->
                Exp.letScope a temp e2 <| fun temp2 ->
                Exp.if' a (Exp.``<`` a temp1 temp2) temp2 temp1

        | _ -> return! skip()
    }
    // <@ min %e1 %e2 @> => <@ let temp1 = %e1; let temp2 = %e2; if temp2 < temp1 then temp2; temp1 @>
    [ <@@ Operators.min @@> ], fun senv struct(args, e) -> context {
        match args with
        | _, [t], [e1; e2] when t = typeof<double> || t = typeof<int> ->
            let! e1 = transpileExpression senv e1
            let! e2 = transpileExpression senv e2
            let a = getLoc e
            let temp = (get FConfig senv.e).localName
            return
                Exp.letScope a temp e1 <| fun temp1 ->
                Exp.letScope a temp e2 <| fun temp2 ->
                Exp.if' a (Exp.``<`` a temp2 temp1) temp2 temp1

        | _ -> return! skip()
    }

    // <@ sign $e1 @>
    // => <@
    //     // double のときは NaN を 0 に変換するため `let temp = e1 + 0`
    //     let temp = e1
    //     if 0. < temp then
    //         1.
    //     else
    //         if temp < 0. then
    //             -1.
    //         else
    //             0.
    // @>
    // NOTE: F#: `sign(0. / 0.)` => raise ArithmeticException
    [ <@@ Operators.sign @@> ], fun senv struct(args, e) -> context {
        match args with
        | _, [t1], [e1] when t1 = typeof<double> || t1 = typeof<int> ->
            let a = getLoc e
            let! e1 = transpileExpression senv e1
            let zero = Exp.number a 0.
            let e1 = if t1 = typeof<double> then Exp.``+`` a e1 zero else e1
            return
                Exp.letScope a (get FConfig senv.e).localName e1 <| fun temp ->
                Exp.if' a
                    (Exp.``<`` a zero temp)
                    (Exp.number a 1.)
                    (Exp.if' a
                        (Exp.``<`` a temp zero)
                        (Exp.number a -1.)
                        zero
                    )

        | _ -> return! skip()
    }

    // <@ Operators.string $x1 @>
    [ <@@ Operators.string @@> ], fun senv struct(args, e) -> context {
        match args with
        | _, [t1], [x1] when t1 = typeof<string> -> return! transpileExpression senv x1
        | _, [t1], [x1] when t1 = typeof<int> || t1 = typeof<double> ->
            let! e1 = transpilePrimitiveExpression senv x1
            return Exp.toS (getLoc e) e1
        | _ -> return! skip()
    }
    [ <@@ ExtraTopLevelOperators.double @@>; <@@ Operators.float @@> ], fun senv struct(args, _) ->
        match args with
        | _, [t1], [x1] when t1 = typeof<int> || t1 = typeof<double> -> transpileExpression senv x1
        | _ -> skip()

    [ <@@ Operators.int @@> ], fun senv struct(args, _) -> context {
        match args with
        | _, [t1], [x1] when t1 = typeof<int> -> return! transpileExpression senv x1

        // TODO: DoubleToInt32 operation
        | _, [UnderlyingPrimitiveType(Typed SType.N)], [x1] -> return! transpileExpression senv x1

        | _ -> return! skip()
    }
    // <@ Operators.not $x1 @>
    single <@@ Operators.not @@> unaryWithS <| fun senv x1 e -> context {
        let! e1 = transpilePrimitiveExpression senv x1
        return Exp.not (SourceCode.tag e) e1
    }
    // <@ ($e1: IWord) :?> #IWord @>
    [ <@@ ("" :> _ seq) :?> string @@> ], fun senv struct(args, e) -> context {
        match args with
        | _, t1::_, e1::_ when e1.Type = typeof<IWord> && typeof<IWord>.IsAssignableFrom t1 ->
            let source = SourceCode.ofExpr e
            let! e1 = transpileExpression senv e1
            let! t1 = underlyingMemberType senv.e source t1
            return Exp.reinterpret (SourceCode.tag source) e1 t1
        | _ -> return! skip()
    }
    // <@ raise (new MatchFailureException(%path, %line, %column) :> exn) @>
    [ <@@ Operators.raise @@> ], fun senv struct(args, e) -> context {
        match args with
        | None, _, [E.Coerce(E.NewObject(ctor, [E.String path; E.Int32 line; E.Int32 column]), _)] when ctor.DeclaringType = typeof<MatchFailureException> ->
            let s = SourceCode.ofExpr e
            let m = Exp.string (SourceCode.tag s) $"MatchFailureException(%A{path}, {line}, {column})"
            let out = outString (localOutEnv senv true s s) s true m
            let! stop = stopAsAnyValue senv.e s e.Type
            return Exp.seq (SourceCode.tag s) out stop

        | _ -> return! skip()
    }
    // <@ assert %e1 @> => <@ if not %e1 then stopAllScripts() @>
    [ Quotations.Expr.Call(typeof<System.Diagnostics.Debug>.GetMethod("Assert", [|typeof<bool>|]), [ <@@ false @@> ]) ], fun senv struct(args, e) -> context {
        match args with
        | _, _, [e1] ->
            let l = getLocation e
            let notM, _ = findMethod <@ not @>
            let failwithMD, _ = findMethod <@ failwith @>
            let failwithM = failwithMD.MakeGenericMethod typeof<unit>
            let e =
                E.IfThenElse(
                    E.Call(notM, [e1])@+l,
                    E.Call(failwithM, [E.Value("assert failure", typeof<string>)@+l])@+l,
                    E.Value((), typeof<unit>)@+l
                )@+l
            return! transpileExpression senv e
        | _ -> return! skip()
    }
    [ <@@ LanguagePrimitives.FloatWithMeasure @@> ], fun senv struct(args, _) -> context {
        match args with
        | _, _, [e1] -> return! transpileExpression senv e1
        | _ -> return! skip()
    }
]

let fsharpCoreExpressionPlugin' senv e = context {
    match e with
    | CallWithStringInterpolation r as e ->
        let s = SourceCode.ofExpr e
        match r with
        | Error e' -> return! InvalidInterpolationCall e' |> raiseError s
        | Ok(GenericMethodDefinition m, formatL, head, tail) ->
            if (let m', _ = findMethod <@ failwithf @> in m = m') then
                let! out = formatCallToOutCall senv true formatL head tail s
                let! stop = stopAsAnyValue senv.e s e.Type
                return Exp.seq (SourceCode.tag s) out stop

            elif (let m', _ = findMethod <@ printfn @> in m = m') then
                return! formatCallToOutCall senv true formatL head tail s

            elif (let m', _ = findMethod <@ printf @> in m = m') then
                return! formatCallToOutCall senv false formatL head tail s

            else
                return! UnsupportedFormatCall m |> raiseError s

    | E.Value(x, t) when typeof<IWord>.IsAssignableFrom t -> return Exp.lit (getLoc e) (x :?> IWord).Value

    // <@ Case @>
    | E.NewUnionCase(c, []) when typeof<IWord>.IsAssignableFrom c.DeclaringType ->
        return V.MakeUnion(c, [||], true) :?> IWord |> iwordToConstant (getLoc e)

    // <@ Case? $e1 @> => `$e1 = $(case.W)`
    | E.UnionCaseTest(e1, c) when typeof<IWord>.IsAssignableFrom c.DeclaringType && c.GetFields().Length = 0 ->
        let l = getLoc e
        let v = V.MakeUnion(c, [||], true) :?> IWord |> iwordToConstant l
        let! e1 = transpilePrimitiveExpression senv e1
        return Exp.``=`` l e1 v

    // <@ nan @>
    | NaNGet _ as e ->
        let l = getLoc e
        let zero = Exp.number l 0.
        return Exp.``/`` l zero zero

    | _ -> return! skip()
}
let fsharpCoreExpressionPlugin() = { new ExpressionPluginProcess() with member _.Invoke senv e = fsharpCoreExpressionPlugin' senv e }
