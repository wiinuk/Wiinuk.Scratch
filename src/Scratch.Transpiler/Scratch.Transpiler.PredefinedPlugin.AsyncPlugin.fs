module internal Scratch.Transpiler.PredefinedPlugin.AsyncPlugin
open FSharp.Quotations
open FSharp.Reflection
open Scratch
open Scratch.IR
open Scratch.Primitives
open Scratch.Primitives.Field
open Scratch.Reflection
open Scratch.Reflection.Expr
open Scratch.Ast
open Scratch.Transformers
open Scratch.Transpiler
open Scratch.Transpiler.PredefinedPlugin.Helpers

module E = FSharp.Quotations.Patterns
module E = FSharp.Quotations.DerivedPatterns
module A = Scratch.Ast.Expressions
module Field = Scratch.MemoryModel.Field
module Exp = Exp.Op
module Top = Top.Listener
type private E = Quotations.Expr
type private QVar = Quotations.Var
type private T = Reflection.FSharpType


[<AutoOpen>]
module private AsyncHelpers =
    let isGeneratorBuilderType t =
        typeof<IGeneratorBuilder>.IsAssignableFrom t

    [<return: Struct>]
    let (|UnitLambda|_|) = function
        | E.Lambda(unitVar, body) when unitVar.Type = typeof<unit> -> ValueSome body
        | _ -> ValueNone

    [<return: Struct>]
    let (|BuilderCall|_|) b = function
        | E.Call(Some(E.Var b'), m, es) when b = b' -> ValueSome struct(m.Name, es)
        | _ -> ValueNone

    /// <summary>&lt;@ (let $b = %generator in $b) { %body } @&gt;</summary>
    let (|GeneratorBody|_|) = function
        | E.Application(E.Lambda(b, e), generator & ExprType et) when isGeneratorBuilderType et ->
            match e with
            | BuilderCall b ("Run", [BuilderCall b ("Delay", [UnitLambda e])])
            | BuilderCall b ("Delay", [UnitLambda e]) -> Some(generator, b, e)

            | _ -> None
        | _ -> None

//    let newStorage senv location export vType name = context {
//        return newStorage senv location export (localName senv.e name) vType
//    }
//    let newTempStorage senv location t = context {
//        let Get FExpressionState { acc = acc } & state = senv.s.contents
//        let tempSpec = newTempStorage senv location t
//        senv.s := state |> wiz FExpressionState { acc = acc }
//        return tempSpec
//    }

    [<Struct>]
    type GeneratorEnvironment = {
        builderVar: QVar
        sourceCode: SourceCode
    }

    type IWithGeneratorEnvironment<'Self,'Field> =
        abstract Field: 'Field
        abstract WithField: 'Field -> 'Self

    [<Struct>]
    type GeneratorEnvironmentFieldShape<'T,'F> when 'T :> IWithGeneratorEnvironment<'T,'F> =
        /// `generatorEnvironment` field operations
        | FGeneratorEnvironment
    with
        interface IFieldShape<'T,'F> with
            member _.Get x = x.Field
            member _.With(x, v) = x.WithField v

    let currentSource senv = (get FGeneratorEnvironment senv.e).sourceCode
    let currentSourceTag b = SourceCode.tag (currentSource b)

    let rec transpileGeneratorBody senv e =
        local FE senv (map FGeneratorEnvironment (fun g -> { g with sourceCode = SourceCode.ofExpr e })) <| fun senv ->
        let b = (get FGeneratorEnvironment senv.e).builderVar

        match e with

        // <@ let! $v = %e1 in %e2 @> => <@ $b.Bind(%e1, fun $v -> %(c e2)) @>
        // <@ do! %e1 in %e2 @> => <@ $b.Bind(%e1, fun () -> %(c e2)) @>
        // <@ do! %e1 @> => <@ $b.Bind(%e1, fun () -> $b.Return()) @>
        | BuilderCall b ("Bind", [e1; E.Lambda(v, e2)])
        
        // <@ let $v = %e1 in %e2 @> => <@ let $v = %e1 in %(c e2) @>
        | E.Let(v, e1, e2) -> transpileGeneratorLet senv (v, e1, e2)

        // <@ return %e1 @> => <@ $b.Return %e1 @>
        // <@ return! %e1 @> => <@ $b.ReturnFrom %e1 @>
        | BuilderCall b ("Return", [e1])
        | BuilderCall b ("ReturnFrom", [e1]) -> transpileExpression senv e1

        // <@ do %e1 in %e2 @> => <@ do %e1 in %(c e2) @>
        | E.Sequential(e1, e2) -> transpileGeneratorSequential senv (e1, e2)

        // <@ %e1; %e2 @> => <@ $b.Combine(%(c e1), $b.Delay(fun () -> %(c e2))) @>
        | BuilderCall b ("Combine", [e1; BuilderCall b ("Delay", [UnitLambda e2])]) -> transpileGeneratorCombine senv (e1, e2)

        // <@ if %test then %ifTrue else %ifFalse @> => <@ if %test then %(c ifTrue) else %(c ifFalse) @>
        // <@ if %test then %ifTrue @> => <@ if %test then %(c ifTrue) else $b.Zero() @>
        // <@ match %test with ... | $pi -> $ei @> => <@ match %test with ... | $pi -> %(c ei) @>
        | E.IfThenElse(testE, ifTrueE, ifFalseE) -> transpileGeneratorIf senv (testE, ifTrueE, ifFalseE)

        // <@ %e1 @> => <@ %e1; $b.Zero() @>
        | BuilderCall b ("Zero", []) -> transpileGeneratorZero senv

        | _ -> InvalidGeneratorExpressionForm |> raiseError (currentSource senv)

    and transpileGeneratorLet senv (v, e1, e2) = context {
        let source = currentSource senv
        let! x1 = transpileExpression senv e1
        let! var = Var.newVarFromQVar senv.e source v
        let spec = VarSpec { var = var; export = NoExport; persistence = NoPersistent }
        return! local FE senv (addVarSpec v spec) <| fun senv -> context {
            let! x2 = transpileGeneratorBody senv e2
            return Exp.let' (SourceCode.tag source) var x1 x2
        }
    }
    and transpileGeneratorSequential senv (e1, e2) = context {
        let! e1 = transpileExpression senv e1
        let! e2 = transpileGeneratorBody senv e2
        return Exp.seq (currentSourceTag senv) e1 e2
    }
    and transpileGeneratorCombine senv (e1, e2) = context {
        let! e1 = transpileGeneratorBody senv e1
        let! e2 = transpileGeneratorBody senv e2
        return Exp.seq (currentSourceTag senv) e1 e2
    }
    and transpileGeneratorIf senv (testE, ifTrueE, ifFalseE) = context {
        let! test = transpilePrimitiveExpression senv testE
        let! ifTrue = transpileGeneratorBody senv ifTrueE
        let! ifFalse = transpileGeneratorBody senv ifFalseE
        return Exp.if' (currentSourceTag senv) test ifTrue ifFalse
    }
    and transpileGeneratorZero senv = context {
        return Exp.empty (currentSourceTag senv)
    }

    type GeneratorEnvironments<'Tail>
        when 'Tail :> IWithBlockEnvironment<'Tail, BlockEnvironment>
        and 'Tail :> IWithEnvironment<'Tail, Environment>
        and 'Tail :> IWithConfig<'Tail, Config>
        = {
        GeneratorEnvironment: GeneratorEnvironment
        Tail: 'Tail
    }
    with
        interface IWithGeneratorEnvironment<GeneratorEnvironments<'Tail>, GeneratorEnvironment> with
            member e.Field = e.GeneratorEnvironment
            member e.WithField v = { e with GeneratorEnvironment = v }

        interface IWithBlockEnvironment<GeneratorEnvironments<'Tail>, BlockEnvironment> with
            member e.Field = get FBlockEnvironment e.Tail
            member e.WithField v = { e with Tail = wiz FBlockEnvironment v e.Tail }

        interface IWithEnvironment<GeneratorEnvironments<'Tail>, Environment> with
            member e.Field = get FEnvironment e.Tail
            member e.WithField v = { e with Tail = wiz FEnvironment v e.Tail }

        interface IWithConfig<GeneratorEnvironments<'Tail>, Config> with
            member e.Field = get FConfig e.Tail
            member e.WithField v = { e with Tail = wiz FConfig v e.Tail }

    let transpileGeneratorBodyWith senv b e =
        let env = { GeneratorEnvironment = { sourceCode = SourceCode.ofExpr e; builderVar = b }; Tail = senv.e }
        let senv = { s = senv.s; e = env }
        transpileGeneratorBody senv e

    let transpileGeneratorBlock senv b e = transpileGeneratorBodyWith senv b e

    let transpileGeneratorProcedure senv proc b e = context {
        let state = ref senv.s.contents
        let env = BlockEnvironments.make({ proc = proc }, senv.e)
        let! stats = transpileGeneratorBlock { s = state; e = env } b e
        senv.s.contents <- state.contents
        return stats
    }
    let transpileGeneratorWhenGreenFlag senv b e = context {
        let proc = {
            procedureVar = Var.newProc (get FConfig senv.e).lambdaName [] ExpTypes.empty

            procedureThis = None
            parameters = []
            atomicity = NoAtomic
            isExport = NoExport
            inlining = None
        }
        let l = getLocation e |> Tagged.empty
        //let e =
        //    if e.Type <> typeof<unit> then E.Sequential(e, E.Value(()) |> withLocation l) |> withLocation l
        //    else e

        let! body = transpileGeneratorProcedure senv proc b e
        let listener = Top.whenGreenFlag l body
        do Environments.addScriptData senv { x = 0.; y = 0.; script = listener }
    }

    let unitLikeGeneratorGet generatorType =
        let isUnitLike t =
            match T.GetUnionCases(t, allowAccessToPrivateRepresentation = true) with
            | [|c|] -> c.GetFields().Length = 0
            | _ -> false
    
        assert (isUnitLike typeof<TightropeBuilder>)
        function
        | E.Call _
        | E.PropertyGet _
        | E.Var _ as e when maxCost e < HasSideEffect && e.Type = generatorType -> ValueSome()
        | _ -> ValueNone

    /// generator { ... }
    [<return: Struct>]
    let (|GeneratorCall|_|) = unitLikeGeneratorGet typeof<TightropeBuilder>

    /// atomic { ... }
    [<return: Struct>]
    let (|AtomicGet|_|) = unitLikeGeneratorGet typeof<AtomicGeneratorBuilder>

    [<return: Struct>]
    let (|LiteralLikeStringMemberGet|_|) = function

        // <@ let n = "s" @>
        | E.PropertyGet(None, E.PropertyGetterWithReflectedDefinition(E.String n) & p, [])
            when Reflection.FSharpType.IsModule p.DeclaringType && not p.CanWrite ->
            ValueSome n

        // <@ [<Literal>] let n = "s" @>
        | E.FieldGet(None, f) when f.IsLiteral && f.FieldType = typeof<string> ->
            f.GetRawConstantValue() :?> string |> ValueSome

        | _ -> ValueNone

    let (|ForeverAsyncCall|_|) = E.(|SpecificCall|_|) <@ PrimitiveOperations.foreverAsync @>
    let (|RepeatAsyncCall|_|) = E.(|SpecificCall|_|) <@ PrimitiveOperations.repeatAsync @>
    let (|RepeatUntilAsyncCall|_|) = E.(|SpecificCall|_|) <@ PrimitiveOperations.repeatUntilAsync @>

    let asyncExpressionPlugin' senv e = context {
        let source = SourceCode.ofExpr e
        match e with
        | GeneratorBody(GeneratorCall _, b, body) ->
            return! transpileGeneratorBodyWith senv b body

        | GeneratorBody(AtomicGet _, b, body) ->
            match (get FBlockEnvironment senv.e).proc.atomicity with
            | NoAtomic -> return! RequireAtomicity Atomic |> raiseError source
            | Atomic -> return! transpileGeneratorBodyWith senv b body

        | GeneratorBody(ForeverAsyncCall(_, _, _), b, body) ->
            let! body = transpileGeneratorBodyWith senv b body
            return Exp.doForever (SourceCode.tag source) body

        | GeneratorBody(RepeatAsyncCall(_, _, [count]), b, body) ->
            // TODO: impl async in atomic op
            // <@
            // let repeatSay n s =
            //     let m = "say " + s
            //     runSynchronously(repeatAsync n <| generator {
            //         out m
            //     })
            // @> =>
            // ```
            // let repeatSay.m = ""
            // let repeatSay(n, s) {
            //     repeatSay.m <- concat("say ", s)
            //     repeatSay._atomic(n)
            //     doRepeat n {
            //         call out (repeatSay.m)
            //     }
            // }
            // ```
            let! count = transpilePrimitiveExpression senv count
            let! body = transpileGeneratorBodyWith senv b body

            return Exp.doRepeat (getLoc e) count body

        // <@ do! repeatUntilAsync (fun _ -> $isStop) { ...$ifFalse } @> => acc: `...$isStopAcc; doUntil($isStop, \{ $ifFalseAcc; ...$isStopAcc })`, expr: ``
        | GeneratorBody(RepeatUntilAsyncCall(_, _, [E.Lambda(_, isStop)]), b, ifFalseBody) ->
            let! isStop = transpilePrimitiveExpression senv isStop
            let! ifFalse = transpileGeneratorBodyWith senv b ifFalseBody
            return Exp.doUntil (SourceCode.tag source) isStop ifFalse

        | _ -> return! skip()
    }

    let transpileGeneratorWhenIReceive senv nameLocation name b e = context {
        let! proc = registerLambdaProcedure senv None NoAtomic
        let! stats = transpileGeneratorProcedure senv proc b e
        let script = Top.whenIReceive nameLocation name stats
        do Environments.addScriptData senv { x = 0.; y = 0.; script = script }
    }
    let transpileGeneratorWhenCloned senv location b e = context {
        let! proc = registerLambdaProcedure senv None NoAtomic
        let! stats = transpileGeneratorProcedure senv proc b e
        let script = Top.whenCloned location stats
        do Environments.addScriptData senv { x = 0.; y = 0.; script = script }
    }
    let transpileItems senv e = context {
        do clearItemAcc senv
        do transpileItems senv e
    }
    let transpileAccAsWhenGreenFlag senv = context {
        do transpileAccAsWhenGreenFlag senv
    }

    let (|StartMainLookCall|_|) : E -> _ = E.(|SpecificCall|_|) <@@ Control.startMainLoop @@>
    let (|WhenGreenFlagCall|_|) = E.(|SpecificCall|_|) <@ Control.whenGreenFlag @>
    let (|WhenIReceiveCall|_|) = E.(|SpecificCall|_|) <@ Control.whenIReceive @> 
    let (|WhenClonedPropertyGet|_|) = (|SpecificPropertyGet|_|) <@ fun (s: Sprite) -> s.WhenCloned @>
    let (|SpriteWhenGreenFlagGet|_|) = (|SpecificPropertyGet|_|) <@ fun (s: Sprite) -> s.WhenGreenFlag @>
    let (|SpriteWhenIReceiveCall|_|) = E.(|SpecificCall|_|) <@ fun (s: Sprite) -> s.WhenIReceive @>

    let inline transpileBuilderItem senv f e2 = context {
        do! transpileAccAsWhenGreenFlag senv
        do! f senv
        do! transpileItems senv e2
    }
    let asyncItemPlugin' senv e =
        match e with

        // <@ ...; startMainLoop() @>
        | StartMainLookCall _ -> transpileAccAsWhenGreenFlag senv

        // <@ %builder { %e1 }; %e2  @>
        | E.Sequential(GeneratorBody(builder, b, e1), e2) ->
            let e1 = expressionPreTransform senv.e e1

            match builder with

            // <@ whenGreenFlag { %e1 } @>
            // <@ do self.WhenGreenFlag { %e1 } @>
            | WhenGreenFlagCall _
            | SpriteWhenGreenFlagGet(Some(SpriteThis senv ()), _) ->
                transpileBuilderItem senv (fun senv -> transpileGeneratorWhenGreenFlag senv b e1) e2

            // <@ do self.WhenIReceive %name { %e1 } @>
            | SpriteWhenIReceiveCall(Some(SpriteThis senv ()), _, [E.String name | LiteralLikeStringMemberGet name as nameE])

            // <@ whenIReceive %name { %e1 } @>
            | WhenIReceiveCall(_, _, [E.String name as nameE]) ->
                transpileBuilderItem senv (fun senv -> transpileGeneratorWhenIReceive senv (getLoc nameE) name b e1) e2

            // <@ do self.WhenCloned { ... } @>
            | WhenClonedPropertyGet(Some(SpriteThis senv ()), _) ->
                transpileBuilderItem senv (fun senv -> transpileGeneratorWhenCloned senv (getLoc e) b e1) e2

            | _ -> skip()

        | _ -> skip()

let asyncCallExpressions() = [
    // <@ do! awaitAtomic (%atomicF ...) @> => acc: `%atomicF ...`, expr: ``
    [ <@@ Control.awaitAtomic @@> ], fun senv struct(args, _) -> context {
        match args with
        | _, _, [E.Call _ | E.Application _ as e1] -> return! transpileExpression senv e1
        | _ -> return! skip()
    }
    single <@@ Control.waitElapsedFrom @@> unaryWithE <| fun senv seconds e -> context {
        let! seconds = transpilePrimitiveExpression senv seconds
        return Exp.``wait:elapsed:from:`` (getLoc e) seconds
    }
    // <@ broadcast %n @>
    [ <@ Control.broadcast @> ], fun _ struct(args, e) -> context {
        match args with
        | _, _, [E.String n | LiteralLikeStringMemberGet n as e1] ->
            return Exp.``broadcast:`` (getLoc e) (Exp.string (getLoc e1) n)

        | _, _, [e1] -> return! raiseError (SourceCode.ofExpr e1) StringLiteralExpressionOnly
        | _ -> return! skip()
    }
    // <@ do! broadcastAndWait %n @>
    [ <@ Control.broadcastAndWait @> ], fun _ struct(args, e) -> context {
        match args with
        | _, _, [E.String n as e1] ->
            return Exp.doBroadcastAndWait (getLoc e) (Exp.string (getLoc e1) n)

        | _, _, [e1] -> return! raiseError (SourceCode.ofExpr e1) StringLiteralExpressionOnly
        | _ -> return! skip()
    }
    [ <@ PrimitiveOperations.waitUntilAsync @> ], fun senv struct(args, e) -> context {
        match args with
        | _, _, [E.Lambda(_, isNextE)] ->
            let! isNext = transpilePrimitiveExpression senv isNextE
            return Exp.doWaitUntil (getLoc e) isNext

        | _ -> return! skip()
    }
    single <@@ SensingOperations.doAsk @@> unaryWithE <| fun senv question e -> context {
        let! question = transpileExpression senv question
        return Exp.doAsk (getLoc e) question
    }
]

let asyncExpressionPlugin() = { new ExpressionPluginProcess() with member _.Invoke senv e = asyncExpressionPlugin' senv e }
let asyncItemPlugin() = { new ItemPluginProcess() with member _.Invoke senv e = asyncItemPlugin' senv e }
