module Scratch.IR.Transformers.Tests
open DiffMatchPatch
open Scratch
open Scratch.IR
open Scratch.IR.Transformers
open Scratch.Primitives
open Scratch.Transformers
open Xunit
open Scratch.Ast

module Exp = Exp.Op
//module A = Scratch.Ast.Expressions
//module A = Scratch.Ast.Expression
module T = Scratch.IR.Transformers.PredefinedTransformers
module R = Scratch.Transformers.TransformResult

[<AutoOpen>]
module private Helpers =
    let mapChoice3 f1 f2 f3 = function
        | Choice1Of3 x -> Choice1Of3(f1 x)
        | Choice2Of3 x -> Choice2Of3(f2 x)
        | Choice3Of3 x -> Choice3Of3(f3 x)

    module ScriptData =
        let mapCore scriptMapping { ScriptData.x = x; y = y; script = script } =
            { x = x; y = y; script = scriptMapping script }

        let map f x = mapCore (Script.map f) x

    module EntityData =
        let mapCore (extensionMapping, scriptMapping, variableMapping, listMapping) x = {
            objName = x.objName
            variables = List.map variableMapping x.variables
            lists = List.map listMapping x.lists
            scripts = List.map (ScriptData.mapCore scriptMapping) x.scripts
            costumes = x.costumes
            sounds = x.sounds
            currentCostumeIndex = x.currentCostumeIndex
    
            ObjectDataExtension = extensionMapping x.ObjectDataExtension
        }
        let map extensionMapping stateMapping x =
            mapCore (extensionMapping, Script.map stateMapping, VariableData.map stateMapping, ListData.map stateMapping) x

    module SpriteData =
        let mapCore (scriptMapping, variableMapping, listMapping) x = EntityData.mapCore (id, scriptMapping, variableMapping, listMapping) x
        let map f x = mapCore (Script.map f, VariableData.map f, ListData.map f) x

    module StageDataExtension =
        let mapCore (scriptMapping, variableMapping, listMapping, stateMapping) { children = children; penLayerMD5 = penLayerMD5; penLayerID = penLayerID; tempoBPM = tempoBPM; videoAlpha = videoAlpha; info = info } = {
            children = List.map (fun x -> mapChoice3 id (SpriteData.mapCore (scriptMapping, variableMapping, listMapping)) (ListData.map stateMapping) x) children
            penLayerMD5 = penLayerMD5
            penLayerID = penLayerID
            tempoBPM = tempoBPM
            videoAlpha = videoAlpha
            info = info
        }
        let map f x = mapCore (Script.map f, VariableData.map f, ListData.map f, f) x

    module StageData =
        let mapCore (scriptMapping, variableMapping, listMapping, stateMapping) x =
            EntityData.mapCore (StageDataExtension.mapCore (scriptMapping, variableMapping, listMapping, stateMapping), scriptMapping, variableMapping, listMapping) x
        let map f x = mapCore (Script.map f, VariableData.map f, ListData.map f, f) x

    module Source =
        let map sourceMapping valueMapping x = { source = sourceMapping x.source; value = valueMapping x.value }

    module Exp =
        let isValid e =
            try Exp.validate e |> ignore; true
            with _ -> false

        let map f x =
            let rec map x =
                Source.map f (function
                    | Lit x -> Lit x
                    | Let(v, e1, e2) -> Let(v, map e1, map e2)
                    | VarSet(v, e1) -> VarSet(v, map e1)
                    | Var v -> Var v
                    | Seq(e1, e2) -> Seq(map e1, map e2)
                    | If(e1, e2, e3) -> If(map e1, map e2, map e3)

                    | NewTuple es -> NewTuple(List.map map es)
                    | TupleGet(e1, i) -> TupleGet(map e1, i)
                    | TupleSet(v, i, e1) -> TupleSet(v, i, map e1)

                    | Op(op, es) -> Op(op, List.map map es)
                    | ExtOp(spec, es) -> ExtOp(spec, List.map map es)
                    | ListOp(op, xs) -> ListOp(op, List.map (function Choice1Of2 e -> Choice1Of2(map e) | Choice2Of2 x -> Choice2Of2(Source.map f id x)) xs)

                    | Call(p, es) -> Call(p, List.map map es)

                    | Coerce(k, e1, t) -> Coerce(k, map e1, t)

                    | Atom e1 -> Atom(map e1)
                ) x
            map x

    module ListenerDef =
        let map f x = Source.map f (fun (ListenerDef(name, args, body)) -> ListenerDef(name, List.map (Exp.map f) args, Exp.map f body)) x

    module VariableInitDef =
        let map f (VariableInitDef(v, e)) = VariableInitDef(v, Exp.map f e)

    module ProcDef =
        let map f x = Source.map f (fun (ProcDef(ProcHeader(v, ps, a), e)) -> ProcDef(ProcHeader(v, List.map (Source.map f id) ps, a), Exp.map f e)) x

    module Top =
        let map f = function
            | Top.Listener x -> Top.Listener <| ListenerDef.map f x
            | Top.VariableInit x -> Top.VariableInit <| VariableInitDef.map f x
            | Top.Procedure x -> Top.Procedure <| ProcDef.map f x

    module VariableDef =
        let map f { state = state; isPersistent = isPersistent; var = var; init = init; view = view } = {
            state = f state
            isPersistent = isPersistent
            var = var
            init = init
            view = view
        }

    module IRStageData =
        let map f (x: _ IRStageData): _ IRStageData = StageData.mapCore (Top.map f, VariableDef.map f, VariableDef.map f, f) x

    let emptyState = Tagged.empty()
    let optimize emptyPluginsWith stage =
        Transformer.transformStage { plugins = emptyPluginsWith Plugins.empty; maxIterationCount = 100 } stage
        |> IRStageData.map (fun _ -> emptyState)

    let (=?) l r =
        if not (l = r) then
            let l = Pretty.pretty l
            let r = Pretty.pretty r
            let d = DiffMatchPatch.Default
            let es = d.DiffMain(l, r)
            d.DiffCleanupSemantic es
            let diffText =
                es
                |> Seq.map (fun e ->
                    match e.Operation with
                    | Delete -> "[- " + e.Text + " -]"
                    | Insert -> "[+ " + e.Text + " +]"
                    | Equal -> e.Text
                )
                |> String.concat ""

            Assert.True(false, sprintf "diff:\n%s\nl:\n%s\nr:\n%s" diffText l r)

    let scriptData s = { x = 0.; y = 0.; script = s }
    let def n = VariableDecl.make () n NoPersistent
    let proc n atomicity ps body = body |> Top.proc () n atomicity ps |> scriptData
    let atomicProc n = proc n Atomic
    let noAtomicProc n = proc n NoAtomic
    let (<-.) x e = Exp.varSet () x e
    let read x = Exp.var () x
    let n x = Exp.number () x
    let (+.) x y = Exp.``+`` () x y
    /// `let var = x`
    let let' = Exp.let' ()
    /// `in`
    let (@@) = id
    /// `;`
    let (@) = Exp.seq ()
    let empty = Exp.empty ()
    let if' = Exp.if' ()
    let call = Exp.call ()

    let whenGreenFlag = Top.Listener.whenGreenFlag () >> scriptData

    /// `$n: () -> ()`
    let newProcVar n = Var.newProc n [] ExpTypes.empty
    /// `$n: number`
    let newVar n = Var.newStorage n false ExpTypes.number
    /// `mutable $n: number`
    let newVarMutable n = Var.newStorage n true ExpTypes.number

    let stageFromScripts scripts =
        let variables = seq {
            for d in scripts do
                match d.script with
                | Top.Procedure { value = ProcDef(ProcHeader(_, ps, _), body) } ->
                    let ps = ps |> Seq.map (fun p -> p.value) |> Set.ofSeq
                    for v in Exp.getFreeVars body do
                        if Set.contains v ps |> not then v

                | Top.Listener { value = ListenerDef(_, args, body) } ->
                    for arg in args do yield! Exp.getFreeVars arg
                    yield! Exp.getFreeVars body

                | Top.VariableInit(VariableInitDef(_, body)) ->
                    yield! Exp.getFreeVars body
        }
        let variables =
            variables
            |> Seq.distinct
            |> Seq.sort
            |> Seq.map def
            |> Seq.toList

        { StageData.defaultValue with
            variables = variables
            scripts = scripts
        }
        |> IRStageData.map Tagged.empty

    let stageFromNoAtomicProcedure name parameters body =
        stageFromScripts [noAtomicProc name parameters body]

    let stageFromAtomicProcedure name parameters body =
        stageFromScripts [atomicProc name parameters body]

    let bool = Exp.bool ()

open FsCheck
[<AutoOpen>]
module private FsCheckHelpers =
    let quickcheck test = qcheckWith (fun c -> { c with Arbitrary = typeof<Scratch.Runtime.Test.Helpers.Arbs> :: c.Arbitrary }) test

[<Fact>]
let pureProragationTest() =
    let p = newProcVar "p"
    let x = newVar "x"
    let y = newVarMutable "y"

    let stage =
        stageFromAtomicProcedure p [] (
            let' x (n 10. +. n 20.) @@
            Exp.show () @
            (y <-. read x)
        )

    let optimized =
        stageFromAtomicProcedure p [] (
            Exp.show () @
            (y <-. (n 10. +. n 20.))
        )
    stage
    |> optimize (fun p -> {
        p with transformExp = T.purePropagation
    })
    =? optimized

[<Fact>]
let purePropagationDoubleVariableReferenceTest() =
    let p = newProcVar "p"
    let x = newVar "x"
    let y = newVarMutable "y"

    let stage =
        stageFromAtomicProcedure p [] (
            let' x (n 10. +. n 20.) @@
            Exp.show () @
            (y <-. read x +. read x)
        )
    stage
    |> optimize (fun p -> { p with transformExp = T.purePropagation })
    =? stage

[<Fact>]
let constantPropagationTest() =
    let p = newProcVar "p"
    let a = newVar "a"
    let x = newVar "x"
    let y = newVarMutable "y"
    let z = newVarMutable "z"

    let stage =
        stageFromAtomicProcedure p [] (
            let' x (read a) @@
            Exp.show () @
            (y <-. read x) @
            (z <-. read x)
        )
    let optimized =
        stageFromAtomicProcedure p [] (
            Exp.show () @
            (y <-. read a) @
            (z <-. read a)
        )
    stage
    |> optimize (fun p -> {
        p with transformExp = T.constantPropagation
    })
    =? optimized

[<Fact>]
let constantPropagationBrunchTest() =
    let p = newProcVar "p"
    let a = newVar "a"
    let b = Var.newStorage "b" false ExpTypes.bool
    let x = newVar "x"
    let y = newVarMutable "y"
    let z = newVarMutable "z"

    let stage =
        stageFromAtomicProcedure p [] (
            let' x (read a) @@
            if' (read b) (
                y <-. read x
            ) (
                empty
            ) @
            (z <-. read x)
        )
    let optimized =
        stageFromAtomicProcedure p [] (
            if' (read b) (
                y <-. read a
            ) (
                empty
            ) @
            (z <-. read a)
        )
    stage
    |> optimize (fun p -> { p with transformExp = T.constantPropagation })
    =? optimized

[<Fact>]
let constantPropagationDoRepeatTest() =
    let p = newProcVar "p"
    let x = newVar "x"
    let a = newVar "a"
    let y = newVarMutable "y"
    let count = newVar "count"
    let z = newVarMutable "z"
    let w = newVarMutable "w"
    let stage =
        stageFromNoAtomicProcedure p [] (
            let' x (read a) @@
            (y <-. read x) @
            Exp.doRepeat () (read count) (
                z <-. read x
            ) @
            (w <-. read x)
        )
    let optimized =
        stageFromNoAtomicProcedure p [] (
            (y <-. read a) @
            Exp.doRepeat () (read count) (
                z <-. read a
            ) @
            (w <-. read a)
        )
    stage
    |> optimize (fun p -> { p with transformExp = T.constantPropagation })
    =? optimized

[<Fact>]
let constantPropagationDoWaitUntilTest() =
    let p = newProcVar "p"
    let x = newVar "x"
    let a = newVar "a"
    let y = newVarMutable "y"
    let pred = Var.newStorage "pred" false ExpTypes.bool
    let z = newVarMutable "z"
    let stage =
        stageFromAtomicProcedure p [] (
            let' x (read a) @@
            (y <-. read x) @
            Exp.doWaitUntil () (read pred) @
            (z <-. read x)
        )
    let optimized =
        stageFromAtomicProcedure p [] (
            (y <-. read a) @
            Exp.doWaitUntil () (read pred) @
            (z <-. read a)
        )
    stage
    |> optimize (fun p -> { p with transformExp = T.constantPropagation })
    =? optimized

[<Fact>]
let constantPropagationInIfBlockTest() =
    let p = newProcVar "p"
    let cond = Var.newStorage "cond" false ExpTypes.bool
    let y = newVar "y"
    let x = newVar "x"
    let z = newVarMutable "z"

    let stage =
        stageFromAtomicProcedure p [] (
            if' (read cond) (
                let' y (read x) @@
                (z <-. read y)
            ) empty
        )
    let optimized =
        stageFromAtomicProcedure p [] (
            if' (read cond) (
                z <-. read x
            ) empty
        )
    stage
    |> optimize (fun p -> { p with transformExp = T.constantPropagation })
    =? optimized

[<Fact>]
let eliminateDeadLetExpressionTest() =
    let p = newProcVar "p"
    let x = newVarMutable "x"
    let stage =
        stageFromAtomicProcedure p [] (
            Exp.show() @
            let' x (n 10.) @@
            Exp.hide()
        )
    let optimized =
        stageFromAtomicProcedure p [] (
            Exp.show() @
            Exp.hide()
        )
    stage
    |> optimize (fun p ->
        { p with transformExp = T.eliminateDeadLetExpression }
    )
    =? optimized

[<Fact>]
let eliminateDeadProceduresTest() =
    let deadRec = newProcVar "deadRec"
    let deadMutualRec1 = newProcVar "deadMutualRec1"
    let deadMutualRec2 = newProcVar "deadMutualRec2"
    let alive1 = newProcVar "alive1"
    let alive2 = newProcVar "alive2"

    let stage =
        stageFromScripts [
            atomicProc deadRec [] (
                call deadRec []
            )

            atomicProc deadMutualRec1 [] (
                call deadMutualRec2 []
            )
            atomicProc deadMutualRec2 [] (
                call deadMutualRec1 []
            )

            atomicProc alive1 [] (
                empty
            )
            atomicProc alive2 [] (
                call alive1 []
            )
            whenGreenFlag (
                call alive2 []
            )
        ]
    let optimized =
        stageFromScripts [
            atomicProc alive1 [] (
                empty
            )
            atomicProc alive2 [] (
                call alive1 []
            )
            whenGreenFlag (
                call alive2 []
            )
        ]

    stage
    |> optimize (fun p -> { p with transformEntity = T.eliminateDeadProcedures })
    =? optimized

[<Fact>]
let joinWhenGreenFlagsTest() =
    let x = newVarMutable "x"
    let p = newProcVar "p"

    let stage =
        stageFromScripts [
            whenGreenFlag (x <-. n 0.)
            atomicProc p [] empty
            whenGreenFlag (x <-. n 1.)
            whenGreenFlag (Exp.``wait:elapsed:from:`` () (n 0.))

            whenGreenFlag (x <-. n 2.)
            whenGreenFlag (x <-. n 3.)
            Top.Listener.whenCloned () empty |> scriptData

            whenGreenFlag (x <-. n 4.)
            whenGreenFlag (x <-. n 5.)
        ]
    let optimized =
        stageFromScripts [
            atomicProc p [] empty
            whenGreenFlag (
                (x <-. n 0.) @
                (x <-. n 1.)
            )
            whenGreenFlag (Exp.``wait:elapsed:from:`` () (n 0.))
            whenGreenFlag (
                (x <-. n 2.) @
                (x <-. n 3.)
            )
            Top.Listener.whenCloned () empty |> scriptData

            whenGreenFlag (
                (x <-. n 4.) @
                (x <-. n 5.)
            )
        ]

    stage
    |> optimize (fun p -> { p with transformEntity = T.joinWhenGreenFlags })
    =? optimized

[<Fact>]
let inlineExpansionTest() =
    let parameterDef x = Param.make () x ExpTypes.number
    let param { Source.value = x } = read x
    let p1 = Var.newProc "p1" [ExpTypes.number; ExpTypes.number] ExpTypes.number
    let p2 = Var.newProc "p2" [] ExpTypes.number
    let a = parameterDef "a"
    let b = parameterDef "b"

    let stage = stageFromScripts [
        atomicProc p1 [a; b] (
            param a +. param b
        )
        atomicProc p2 [] (
            call p1 [n 10.; n 20.]
        )
    ]
    let optimized = stageFromScripts [
        atomicProc p1 [a; b] (
            param a +. param b
        )
        atomicProc p2 [] (
            n 10. +. n 20.
        )
    ]
    stage
    |> optimize (fun p ->
        { p with
            transformEntity = T.inlineExpansion id
            transformExp = T.constantPropagation
        }
    )
    =? optimized

[<Fact>]
let inlineExpansionAtomicityTest() =
    let atomic1 = Var.newProc "atomic1" [] ExpTypes.number
    let atomic2 = Var.newProc "atomic2" [] ExpTypes.number
    let p1 = Var.newProc "p1" [] ExpTypes.number
    let p2 = Var.newProc "p2" [] ExpTypes.number

    let stage = stageFromScripts [
        atomicProc atomic1 [] (
            n 10.
        )
        noAtomicProc p1 [] (
            call atomic1 []
        )

        noAtomicProc p2 [] (
            n 20.
        )
        atomicProc atomic2 [] (
            call p2 []
        )
    ]
    stage
    |> optimize (fun p -> { p with transformEntity = T.inlineExpansion id })
    =? stage

[<Fact>]
let inlineExpansionMutualRecutionTest() =
    let x = newVarMutable "x"
    let mutual1 = newProcVar "mutual1"
    let mutual2 = newProcVar "mutual2"
    let mutual3 = newProcVar "mutual3"

    let stage = stageFromScripts [
        noAtomicProc mutual1 [] (
            (x <-. read x +. n 10.) @
            Exp.``wait:elapsed:from:`` () (n 0.) @
            call mutual3 []
        )
        noAtomicProc mutual2 [] (
            call mutual1 []
        )
        noAtomicProc mutual3 [] (
            call mutual2 []
        )
        whenGreenFlag (
            call mutual3 []
        )
    ]
    let optimized = stageFromScripts [
        noAtomicProc mutual2 [] (
            (x <-. read x +. n 10.) @
            Exp.``wait:elapsed:from:`` () (n 0.) @
            call mutual2 []
        )
        whenGreenFlag (
            call mutual2 []
        )
    ]
    stage
    |> optimize (fun p -> {
        p with
            transformEntity =
                Composable.compose
                    (T.inlineExpansion <| fun c -> { c with maxProcedureSize = 100 })
                    T.eliminateDeadProcedures
    })
    =? optimized

module eliminateDeadConditionExpressionTests =
    let private optimize = optimize <| fun p -> {
        p with
            transformExp = T.eliminateDeadConditionExpression
            transformEntity = T.eliminateUnusedVariables
    }

    // `doIfElse $test $ifTrue $ifFalse` ( truthy $test ) => `$ifTrue`
    // `doIfElse $test $ifTrue $ifFalse` ( falsy $test ) => `$ifFalse`
    [<Fact>]
    let constantIfElsePropertyTest() = quickcheck <| fun test (NormalFloat ifTrue) (NormalFloat ifFalse) ->
        let p = Var.newProc "p" [] ExpTypes.number
        let ifTrue = n ifTrue
        let ifFalse = n ifFalse

        stageFromAtomicProcedure p [] (
            if' (bool test) ifTrue ifFalse
        )
        |> optimize
        =? stageFromAtomicProcedure p [] (
            if test then ifTrue else ifFalse
        )

    // `doIf $test $ifTrue` ( truthy $test ) => `$ifTrue`
    // `doIf $test $ifTrue` ( falsy $test ) => ``
    [<Fact>]
    let constantIfEmptyPropertyTest() = quickcheck <| fun test (NormalFloat ifTrue) ->
        let p = newProcVar "p"
        let ifTrue = newVarMutable "x" <-. n ifTrue

        stageFromAtomicProcedure p [] (
            if' (bool test) ifTrue empty
        )
        |> optimize
        =? stageFromAtomicProcedure p [] (
            if test then ifTrue else empty
        )

    // `doRepeat $count $body` ( atomic, $count <= 0 ) => ``
    // `doRepeat 1 $body` ( atomic ) => `$body`
    [<Fact>]
    let constantDoRepeatCountPropertyTest() = quickcheck <| fun count ->
        let p = newProcVar "p"
        let body = Exp.show ()
        let repeat = Exp.doRepeat () (n (double count)) body

        stageFromAtomicProcedure p [] (
            repeat
        )
        |> optimize
        =? stageFromAtomicProcedure p [] (
            if count <= 0 then empty
            elif count = 1 then body
            else repeat
        )

    // `doUntil $test $ifFalse` ( atomic, truthy $test ) => ``
    // `doUntil $test $ifFalse` ( false $test ) => `doForever $ifFalse`
    [<Fact>]
    let constantDoUntilCountPropertyTest() = quickcheck <| fun test (NormalFloat ifFalse) ->
        let p = newProcVar "p"
        let ifFalse = Exp.toEmpty () (n ifFalse)

        stageFromAtomicProcedure p [] (
            Exp.doUntil () (bool test) ifFalse
        )
        |> optimize
        =? stageFromAtomicProcedure p [] (
            if test then empty else (Exp.doForever () ifFalse)
        )
