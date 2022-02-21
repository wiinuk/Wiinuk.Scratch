module internal Scratch.Transpiler.PredefinedPlugin.ScratchRuntime
open Scratch.Transpiler
open Scratch
open Scratch.Ast
open Scratch.IR
open Scratch.MemoryModel
open Scratch.MemoryModel.TypeSpec
open Scratch.Primitives
open Scratch.Primitives.Field
open Scratch.Reflection
open Scratch.Transpiler.PredefinedPlugin.Helpers
module A = Scratch.Ast.Expression
module A = Scratch.Ast.Expressions
module Field = Scratch.MemoryModel.Field
module E = Quotations.Patterns
module E = Quotations.DerivedPatterns
module Exp = Exp.Op
type private E = Quotations.Expr


[<AutoOpen>]
module private Helpers =
    let (|NewNil|_|) : E -> _ = (|SpecificNewUnionCase|_|) <@@ Nil @@>
    let (|NewNonNil|_|) : E -> _ = (|SpecificNewUnionCase|_|) <@@ NonNil @@>
    let (|NilTest|_|) : E -> _ = (|SpecificUnionCaseTest|_|) <@@ Nil @@>
    let (|NonNilTest|_|) : E -> _ = (|SpecificUnionCaseTest|_|) <@@ NonNil @@>
    let (|NonNilGet|_|) : E -> _ = (|SpecificUnionCaseFieldGet|_|) <@@ function NonNil x -> Some x | _ -> None @@>

    let push senv listSource list e1 source = context {
        let! x1 = transpilePrimitiveExpression senv e1
        return Exp.``append:toList:`` (SourceCode.tag source) x1 (Exp.list (SourceCode.tag listSource) list.listVar)
    }

    let scratchPointerExpressionPlugin' senv e = context {
        match e with

        // <@ Nil @> => `0`
        | NewNil _ -> return Exp.number (getLoc e) 0.
        // <@ NonNil $e1 @> => `$e1`
        | NewNonNil(_, [e1]) -> return! transpileExpression senv e1

        // <@ Nil? $e1 @> => `$e1 = 0`
        | NilTest(_, e1) as e ->
            let l = getLoc e
            let! x1 = transpilePrimitiveExpression senv e1
            return Exp.``=`` l x1 (Exp.number l 0.)

        // <@ NonNil? $e1 @> => `not($e1 = 0)`
        | NonNilTest(_, e1) as e ->
            let l = getLoc e
            let! x1 = transpilePrimitiveExpression senv e1
            return Exp.not l (Exp.``=`` l x1 (Exp.number l 0.))

        // <@ let (NonNil x) = $e1 @>
        | NonNilGet(_, e1) -> return! transpileExpression senv e1

        | _ -> return! skip()
    }

    let entityNames senv e = context {
        let source = SourceCode.ofExpr e
        match e with
        | PropertyOrVarOrFieldGet senv x1 ->
            match lookupSpec x1 senv.e with
            | ValueNone -> return! raiseError source <| SpecNotFound x1
            | ValueSome s ->

            match s with
            | ListSpec s -> return Choice1Of2 s.listVar
            | VarSpec s -> return Choice2Of2 s.var
            | ProcedureSpec _
            | SpriteSpec _ -> return! raiseError source RequireVariableExpression

        | _ -> return! raiseError source RequireVariableExpression
    }

    let (|OpUnaryNegationCall|_|) : E -> _ = E.(|SpecificCall|_|) <@@ Operators.(~-) @@>

    let (|SpriteXGet|_|) : E -> _ = (|SpecificPropertyGet|_|) <@ fun (s: Sprite) -> s.X @>
    let (|SpriteYGet|_|) : E -> _ = (|SpecificPropertyGet|_|) <@ fun (s: Sprite) -> s.Y @>
    let (|SpriteSizeSet|_|): E -> _ = (|SpecificPropertySet|_|) <@ fun (s: Sprite) v -> s.Size <- v @>
    let scratchExpressionPlugin' senv e = context {
        match e with
        | E.Coerce(e1, t) when t = typeof<IWord> -> return! transpileExpression senv e1
        | E.Coerce(e1, GenericTypeDefinition d) when d = typedefof<fiber<_,_,_>> -> return! transpileExpression senv e1
        | SpriteXGet(Some(SpriteOrProcedureThis senv ()), _) -> return Exp.xpos (getLoc e)
        | SpriteYGet(Some(SpriteOrProcedureThis senv ()), _) -> return Exp.ypos (getLoc e)
        | SpriteSizeSet(Some(SpriteOrProcedureThis senv ()), _, e1) ->
            let! x1 = transpilePrimitiveExpression senv e1
            return Exp.``setSizeTo:`` (getLoc e) x1

        | _ -> return! skip()
    }

    let transpileForever senv body source = context {
        let a = SourceCode.tag source
        let! body = transpileExpression senv body
        let e = Exp.doForever a body
        return
            match (get FBlockEnvironment senv.e).proc with
            | { atomicity = NoAtomic } -> Exp.atom a e
            | { atomicity = Atomic } -> e
    }

let transpileRepeatAtomic senv indexInit count body source = context {
    match indexInit with
    | Some(indexVar, indexInit) ->
        let! var = Var.newVarFromQVar senv.e source indexVar
        let indexSpec = { var = var; export = NoExport; persistence = NoPersistent }
        return! localEnv senv (addVarSpec indexVar (VarSpec indexSpec)) <| fun senv -> result {
            let! init = transpilePrimitiveExpression senv indexInit
            let! count = transpilePrimitiveExpression senv count
            let! body = transpileExpression senv body
            return
                Exp.let' (SourceCode.tag source) var init (
                    Exp.doRepeat (SourceCode.tag source) count body
                )
        }
    | None ->
        let! count = transpilePrimitiveExpression senv count
        let! body = transpileExpression senv body
        return Exp.doRepeat (SourceCode.tag source) count body
}
let transpileRepeat senv indexInit count body source = context {
    let! e = transpileRepeatAtomic senv indexInit count body source
    return
        match (get FBlockEnvironment senv.e).proc with
        | { atomicity = NoAtomic } -> Exp.atom (SourceCode.tag source) e
        | { atomicity = Atomic } -> e
}

let transpileRepeatUntil senv isStop ifFalse source = context {
    let l = SourceCode.tag source
    let! isStop = transpilePrimitiveExpression senv isStop
    let! ifFalse = transpileExpression senv ifFalse
    let e = Exp.doUntil l isStop ifFalse
    return
        match (get FBlockEnvironment senv.e).proc with
        | { atomicity = NoAtomic } -> Exp.atom l e
        | { atomicity = Atomic } -> e
}

let scratchExpressionPlugin() = { new ExpressionPluginProcess() with member _.Invoke senv e = scratchExpressionPlugin' senv e }
let scratchPointerExpressionPlugin() = { new ExpressionPluginProcess() with member _.Invoke senv e = scratchPointerExpressionPlugin' senv e }

let scratchCallExpressions() = [
    // <@ -($e1) @> => <@ -1 * $e1 @>
    single <@@ N.(~-) @@> unaryWithS <| fun senv e1 s -> context {
        let! e1 = transpilePrimitiveExpression senv e1
        let l = SourceCode.tag s
        return Exp.``*`` l (Exp.number l -1.) e1
    }

    // <@ toS ... @> => <@ ... @> | <@ ... + "" @>
    [ <@@ WordOperations.toS @@>; <@@ Word.toString @@> ], unaryWithS <| fun senv e1 s -> context {
        let! x1 = transpilePrimitiveExpression senv e1
        match underlyingPrimitiveType e1.Type with
        | Some(Typed SType.S) -> return x1
        | _ -> return Exp.toS (SourceCode.tag s) x1
    }
    // <@ toN ... @> => <@ ... @> | <@ ... + 0 @>
    single <@@ WordOperations.toN @@> unaryWithS <| fun senv e1 s -> context {
        let! x1 = transpilePrimitiveExpression senv e1
        match underlyingPrimitiveType e1.Type with
        | Some(Typed SType.N) -> return x1
        | _ -> return Exp.toN (SourceCode.tag s) x1
    }
    // <@ % $e @> => <@ $e @>
    single <@@ Scratch.Operators.(~%) : bool -> B @@> unaryType2WithS <| fun senv t1 t2 e1 s ->
        if
            (t1 = typeof<bool> && t2 = typeof<B>) ||
            (t1 = typeof<string> && t2 = typeof<S>) ||
            (t1 = typeof<int> && t2 = typeof<N>) ||
            (t1 = typeof<double> && t2 = typeof<N>) ||

            (t1 = typeof<B> && t2 = typeof<bool>) ||
            (t1 = typeof<S> && t2 = typeof<string>) ||
            (t1 = typeof<N> && t2 = typeof<double>) ||
            (t1 = typeof<N> && t2 = typeof<int>)
        then
            transpileExpression senv e1
        else
            InvelidImplicitConversionType(t1, t2) |> raiseError s

    // <@ Memory.memoryEnd() @> => `SList.length(MemoryOperations.memory) + 1`
    single <@@ Memory.memoryEnd @@> nullaryWithS <| fun senv source -> context {
        let! s = lookupOrRaiseMemory senv source
        let l = SourceCode.tag source
        return Exp.``+`` l (Exp.``lineCountOfList:`` l (Exp.list l s.listVar)) (Exp.number l 1.)
    }
    // <@ Memory.get reference memberOffset @> => `SList.get(MemoryOperations.memory, reference + memberOffset)`
    // <@ reference.(memberOffset) @> => `SList.get(MemoryOperations.memory, reference + memberOffset)`
    [ <@@ Memory.get @@>; <@@ Scratch.MemoryModel.Operators.(->.) @@>; <@@ Operators.(->%): _ -> Field<_, N> -> double @@> ], binaryWithE <| fun senv reference memberOffset e -> context {
        let s = SourceCode.ofExpr e
        let! m = lookupOrRaiseMemory senv s
        let! wordType = underlyingMemberType senv.e s e.Type

        let! reference = transpilePrimitiveExpression senv reference
        let! memberOffset = transpilePrimitiveExpression senv memberOffset

        let a = SourceCode.tag s
        return Exp.reinterpret a (Exp.``getLine:ofList:`` a (Exp.``+`` a reference memberOffset) (Exp.list a m.listVar)) wordType
    }
    // <@ Memory.memoryBegin() @> => <@ 1 @>
    single <@@ Memory.memoryBegin @@> nullaryWithS <| fun _ s -> context {
        return Exp.number (SourceCode.tag s) 1.
    }

    // <@ Memory.read reference @> => `SList.get(MemoryOperations.memory, reference)`
    single <@@ Memory.read @@> unaryWithE <| fun senv reference e -> context {
        let s = SourceCode.ofExpr e
        let! m = lookupOrRaiseMemory senv s
        let! wordType = underlyingMemberType senv.e s e.Type
        let a = SourceCode.tag s

        let! reference = transpilePrimitiveExpression senv reference
        return Exp.reinterpret a (Exp.``getLine:ofList:`` a reference (Exp.list a m.listVar)) wordType
    }
    // <@ SList.length $x1 @>
    [ <@@ SList.length @@> ], fun senv struct(args, e) -> context {
        match args with
        | _, t1::_, (PropertyOrVarOrFieldGet senv k as listE)::_ when isPrimitiveType t1 ->
            let s = lookupOrRaiseList (SourceCode.ofExpr listE) k senv.e
            let a = getLoc e
            return Exp.``lineCountOfList:`` a (Exp.list a s.listVar)
        | _ -> return! skip()
    }
    // <@ SList.get $list $line @>
    [ <@@ SList.get @@> ], fun senv struct(args, e) -> context {
        match args with
        | _, t1::_, (PropertyOrVarOrFieldGet senv k as listE)::lineE::_ when isPrimitiveType t1 ->
            let listS = SourceCode.ofExpr listE
            let! elementType = underlyingMemberType senv.e listS t1
            let s = lookupOrRaiseList listS k senv.e
            let! line = transpilePrimitiveExpression senv lineE
            let a = getLoc e
            return Exp.reinterpret a (Exp.``getLine:ofList:`` a line (Exp.list a s.listVar)) elementType

        | _ -> return! skip()
    }
    // <@ SList.set $list $line $value @>
    [ <@@ SList.set @@> ], fun senv struct(args, e) -> context {
        match args with
        | _, t1::_, (PropertyOrVarOrFieldGet senv k as listE)::lineE::valueE::_ when isPrimitiveType t1 ->
            let s = lookupOrRaiseList (SourceCode.ofExpr listE) k senv.e
            let! line = transpilePrimitiveExpression senv lineE
            let! value = transpilePrimitiveExpression senv valueE
            let a = getLoc e
            return Exp.``setLine:ofList:to:`` a line (Exp.list a s.listVar) value

        | _ -> return! skip()
    }
    [ <@@ SList.removeAll @@> ], fun senv struct(args, e) -> context {
        match args with
        | _, [t1], [PropertyOrVarOrFieldGet senv k as listE] when isPrimitiveType t1 ->
            let s = lookupOrRaiseList (SourceCode.ofExpr listE) k senv.e
            let listA = getLoc listE
            return Exp.``deleteLine:ofList:`` (getLoc e) (Exp.string listA "all") (Exp.list listA s.listVar)

        | _ -> return! skip()
    }
    [ <@@ SList.join @@> ], fun senv struct(args, e) -> context {
        match args with
        | _, _, (PropertyOrVarOrFieldGet senv k as listE)::_ ->
            let s = lookupOrRaiseList (SourceCode.ofExpr listE) k senv.e
            return Exp.``contentsOfList:`` (getLoc e) (Exp.list (getLoc listE) s.listVar)
        | _ -> return! skip()
    }
    // <@ defineRecordMember <@ $recordField @> @> => `$(memberOffset <@ $recordField @>)`
    [ <@@ Field.defineRecordField @@> ], fun _ struct(args, e) -> context {
        match args with
        | _, _, [E.QuoteTyped recordField] ->
            return
                recordField
                |> Expr.tryPick (function
                    | E.PropertyGet(_, m, _) -> Some m
                    | _ -> None
                )
                |> Option.defaultWith (fun _ ->
                    failwith $"invalid record field expr: %A{recordField}"
                )
                |> Field.recordFieldRaw
                |> double
                |> Exp.number (getLoc e)

        | _ -> return! skip()
    }
    // <@ defineTypeSize<$t> @> => `$(typeSizeRaw $t)`
    // <@ Size.typeSize<$t> @> => `$(typeSizeRaw $t)`
    [ <@@ SizeOperations.defineTypeSize<_> @@>; <@@ Size.typeSize<_> @@> ], fun _ struct(args, e) -> context {
        match args with
        | _, [t], _ ->
            return
                t
                |> Size.typeSizeRaw
                |> double
                |> Exp.number (getLoc e)

        | _ -> return! skip()
    }
    // <@ Reference.unsafeOfAddress<$t> $address @> => `$address`
    // TODO: add nil address check
    single <@@ Reference.unsafeOfAddress<_> @@> unary transpileExpression

    [
        <@@ WordOperations.toV @@>
        <@@ Size.toNumber @@>
        <@@ Field.toNumber @@>
        <@@ Field.unsafeOfNumber<_,_> @@>
        <@@ Address.ofNumber @@>
        <@@ Pointer.toAddress @@>
        <@@ Pointer.unsafeOfAddress<_> @@>
        <@@ Reference.toAddress @@>
        <@@ Reference.toPointer @@>
    ], unary transpileExpression

    // <@ Field.reference %e1 %e2 @> => `%e1 + %e2`
    [ <@@ Field.reference @@>; <@@ Operators.(->*) @@> ], binaryPrimitive Exp.``+``

    // <@ unsafeReference p index size @> -> `p + index * size`
    single <@@ Sequence.unsafeReference @@> ternaryWithS <| fun senv p index size s -> context {
        let! p = transpilePrimitiveExpression senv p
        let! index = transpilePrimitiveExpression senv index
        let! size = transpilePrimitiveExpression senv size
        let l = SourceCode.tag s
        return Exp.``+`` l p (Exp.``*`` l index size)
    }
    // TODO: register nil variable
    single <@@ PointerOperations.nil @@> nullaryWithS <| fun _ s -> context {
        return Exp.number (SourceCode.tag s) 0.
    }

    single <@@ Address.(+) @@> binaryWithS <| fun senv e1 e2 e -> context {
        let! x1 = transpilePrimitiveExpression senv e1
        match e2 with

        // <@ %e1 + -(%e2) @> => %e1 - %e2
        | OpUnaryNegationCall(_, _, [e2]) ->
            let! x2 = transpilePrimitiveExpression senv e2
            return Exp.``-`` (SourceCode.tag e) x1 x2
        | _ ->
            let! x2 = transpilePrimitiveExpression senv e2
            return Exp.``+`` (SourceCode.tag e) x1 x2
    }
    single <@@ N.WithMeasure @@> unary transpileExpression

    single <@@ PrimitiveOperations.showVariable @@> unaryWithS <| fun senv e1 s -> context {
        let a = SourceCode.tag s
        match! entityNames senv e1 with
        | Choice1Of2 list -> return Exp.``showList:`` a (Exp.list (getLoc e1) list)
        | Choice2Of2 var -> return Exp.``showVariable:`` a var
    }
    single <@@ PrimitiveOperations.hideVariable @@> unaryWithS <| fun senv e1 s -> context {
        let a = SourceCode.tag s
        match! entityNames senv e1 with
        | Choice1Of2 list -> return Exp.``hideList:`` a (Exp.list (getLoc e1) list)
        | Choice2Of2 var -> return Exp.``hideVariable:`` a var
    }
]

let blockLikeCalls() = [
    // <@ forever (fun _ -> $body) @> => acc: `doForever(\{ ...$bodyAcc })`, expr: ``
    [ <@@ PrimitiveOperations.forever @@> ], fun senv struct(args, e) -> context {
        match args with
        | _, _, [E.Lambda(_, body)] -> return! transpileForever senv body (SourceCode.ofExpr e)
        | _ -> return! skip()
    }
    // <@ repeat $count (fun _ -> $body) @> => acc: `...$countAcc; doRepeat($count, \{ ...$bodyAcc })`, expr: ``
    [ <@@ PrimitiveOperations.repeat @@> ], fun senv struct(args, e) -> context {
        match args with
        | _, _, [count; E.Lambda(_, body)] -> return! transpileRepeat senv None count body (SourceCode.ofExpr e)
        | _ -> return! skip()
    }
    // <@ repeatUntil (fun _ -> $isStop) (fun _ -> $ifFalse) @> =>  acc: `...$isStopAcc; doUntil($isStop, \{ $ifFalseAcc; ...$isStopAcc })`, expr: ``
    [ <@@ PrimitiveOperations.repeatUntil @@> ], fun senv struct(args, e) ->
        match args with
        | _, _, [E.Lambda(_, isStop); E.Lambda(_, ifFalse)] -> transpileRepeatUntil senv isStop ifFalse (SourceCode.ofExpr e)
        | _ -> skip()

    // <@ SList.push $list $x @>
    [ <@@ SList.push @@> ], fun senv struct(args, e) -> context {
        match args with
        | _, t1::_, (PropertyOrVarOrFieldGet senv k as list)::e1::_ ->
            let source = SourceCode.ofExpr e
            if not <| isPrimitiveType t1 then return! InvalidListPushType t1 |> raiseError source else

            let listSource = SourceCode.ofExpr list
            let l = lookupOrRaiseList listSource k senv.e
            return! push senv listSource l e1 source
        | _ -> return! skip()
    }
    // <@ Memory.push $e1 @> => `SList.push(MemoryOperations.memory, $e1)`
    single <@@ Memory.push @@> unaryWithS <| fun senv e1 s -> context {
        let! l = lookupOrRaiseMemory senv s
        return! push senv s l e1 s
    }
    // <@ Memory.clear() @> => `SList.clear(MemoryOperations.memory)`
    single <@@ Memory.clear @@> nullaryWithS <| fun senv s -> context {
        let! l = lookupOrRaiseMemory senv s
        let a = SourceCode.tag s
        return Exp.``deleteLine:ofList:`` a (Exp.string a "all") (Exp.list a l.listVar)
    }
    // <@ reference.(memberOffset) <- value @> => `SList.set(MemoryOperations.memory, reference + memberOffset, value)`
    // <@ Memory.set reference memberOffset value @> => `SList.set(MemoryOperations.memory, reference + memberOffset, value)`
    [ <@@ Memory.set @@> ], ternaryWithS <| fun senv reference memberOffset value s -> context {
        let! l = lookupOrRaiseMemory senv s
        let a = SourceCode.tag s

        let! reference = transpilePrimitiveExpression senv reference
        let! memberOffset = transpilePrimitiveExpression senv memberOffset
        let! value = transpilePrimitiveExpression senv value

        return Exp.``setLine:ofList:to:`` a (Exp.``+`` a reference memberOffset) (Exp.list a l.listVar) value
    }

    // <@ Memory.write reference value @> => `SList.set(MemoryOperations.memory, reference, value)`
    [ <@@ Memory.write @@>; <@@ Operators.(<-*) @@>; <@@ Operators.(<-%): N Reference -> double -> _ @@> ], binaryWithS <| fun senv reference value s -> context {
        let! l = lookupOrRaiseMemory senv s

        let! reference = transpilePrimitiveExpression senv reference
        let! value = transpilePrimitiveExpression senv value

        let a = SourceCode.tag s
        return Exp.``setLine:ofList:to:`` a reference (Exp.list a l.listVar) value
    }
    // <@ swap<'T> r1 r2 @>
    // =>
    // ```
    // t.1 <- memory[r1]
    // t.2 <- memory[r1 + 1]
    // ...
    //
    // memory[r1] <- memory[r2]
    // memory[r1 + 1] <- memory[r2 + 1]
    // ...
    //
    // memory[r2] <- t.1
    // memory[r2+1] <- r.2
    // ...
    // ```
    [ <@@ Reference.swap @@> ], fun senv struct(args, e) -> context {
        let add l p i = if i = 0 then p else Exp.``+`` l p (Exp.number l (double i))

        match args with

        // 型のサイズが小さいときのみの特殊化 & インライン展開
        // 型のサイズが大きいときは ReflectedDefinition が付いた swap 関数が使われる
        | _, [UnderlyingMemberType senv.e (UnboxedType ts)], [reference1; reference2] when List.length ts <= 2 ->
            let s = SourceCode.ofExpr e

            let! reference1 = transpilePrimitiveExpression senv reference1
            let! reference2 = transpilePrimitiveExpression senv reference2
            let! memory = lookupOrRaiseMemory senv s

            let a = SourceCode.tag s
            let memory = Exp.list a memory.listVar

            let ts = ts |> List.map ExpType.ofMemberType

            let readRerefence1 =
                ts
                |> List.mapi (fun i t -> Exp.reinterpret a (Exp.``getLine:ofList:`` a (add a reference1 i) memory) t)
                |> Exp.newTuple a

            let setReference1OfReference2 =
                ts
                |> List.mapi (fun i t ->
                    let get = Exp.reinterpret a (Exp.``getLine:ofList:`` a (add a reference2 i) memory) t
                    Exp.``setLine:ofList:to:`` a (add a reference1 i) memory get
                )
                |> Exp.concat a

            let writeReference2OfTemp temp =
                ts
                |> List.mapi (fun i _ ->
                    Exp.``setLine:ofList:to:`` a (add a reference2 i) memory (Exp.tupleGet a temp i)
                )
                |> Exp.concat a

            return Exp.letScope a (get FConfig senv.e).localName readRerefence1 <| fun temp ->
                Exp.concat a [
                    setReference1OfReference2
                    writeReference2OfTemp temp
                ]

        | _ -> return! skip()
    }
    single <@@ PrimitiveOperations.stopAllScripts @@> unaryWithS <| fun _ _ s -> context {
        return Exp.stopScripts (SourceCode.tag s) StopScriptKind.All
    }
    spriteInstanceCallWithS <@ fun s -> s.Clone @> <| fun _ _ s -> context {
        return Exp.createCloneOf (SourceCode.tag s) "_myself_"
    }
    spriteInstanceCallWithS <@ fun s -> s.DeleteClone() @> <| fun _ _ s -> context {
        return Exp.deleteClone (SourceCode.tag s)
    }
    spriteInstanceCallWithS <@ fun s -> s.GotoXY @> <| fun senv args s -> context {
        match args with
        | [e1; e2] ->
            let! x1 = transpilePrimitiveExpression senv e1
            let! x2 = transpilePrimitiveExpression senv e2
            return Exp.``gotoX:y:`` (SourceCode.tag s) x1 x2

        | _ -> return! skip()
    }
    spriteInstanceCallWithS <@ fun s -> s.Show @> <| fun _ _ s -> context {
        return Exp.show (SourceCode.tag s)
    }
    spriteInstanceCallWithS <@ fun s -> s.Hide @> <| fun _ _ s -> context {
        return Exp.hide (SourceCode.tag s)
    }
    [ <@@ fun (s: Sprite) -> s.LookLikeFrom @@>; <@@ fun (s: Sprite) -> s.LookLike @@> ], fun senv struct(args, e) -> context {
        match args with
        | Some(SpriteOrProcedureThis senv ()), _, [e1] ->
            let l1 = getLoc e1
            let! x1 = transpilePrimitiveExpression senv e1 
            let x1 =
                match x1 with

                // 数値リテラルは直接設定できない
                // `lookLike: 12` => `lookLike: (12 + 0)`
                | { value = Lit(SNumber _) } -> Exp.``+`` l1 x1 (Exp.number l1 0.)

                | x1 -> x1

            return Exp.``lookLike:`` (getLoc e) x1

        | _ -> return! skip()
    }
    spriteInstanceCallWithS <@ fun s -> s.SetColorEffect @> <| fun senv args s -> context {
        match args with
        | [e1] ->
            let! x1 = transpilePrimitiveExpression senv e1
            return Exp.``setGraphicEffect:to:`` (SourceCode.tag s) Filter.Color x1

        | _ -> return! skip()
    }
    spriteInstanceCallWithS <@ fun s -> s.SetGhostEffect @> <| fun senv args s -> context {
        match args with
        | [e1] ->
            let! x1 = transpilePrimitiveExpression senv e1
            return Exp.``setGraphicEffect:to:`` (SourceCode.tag s) Filter.Ghost x1

        | _ -> return! skip()
    }
]
