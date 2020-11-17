module Scratch.Ast.Transformers.Tests
open FsCheck
open Scratch
open Scratch.Ast
open Scratch.Ast.Transformers
open Scratch.Primitives
open Scratch.Runtime.Test
open Scratch.Transformers
open Xunit
module A = Scratch.Ast.Expressions
module A = Scratch.Ast.Expression
module T = Scratch.Ast.Transformers.PredefinedTransformers
module R = Scratch.Transformers.TransformResult

[<AutoOpen>]
module private Helpers =
    let optimize emptyPluginsWith stage =
        Transformer.transformStage { plugins = emptyPluginsWith Plugins.empty; maxIterationCount = 100 } stage
        |> StageData.map (fun _ -> Tagged.empty())

    let (=?) l r =
        if not (l = r) then
            let l = Pretty.pretty l
            let r = Pretty.pretty r
            Assert.True(false, Scratch.Test.AssertionWithDiff.buildDiffText l r)

    let def n v = VariableData.make () n v
    let atomicProc n ps body = ScriptData.procedure () n ps Atomic body
    let noAtomicProc n ps body = ScriptData.procedure () n ps NoAtomic body
    let (<-.) x e = A.``setVar:to:`` () x e
    let read x = A.readVariable () x
    let n x = A.eNumber () x
    let (+.) x y = A.``+`` () x y
    let scriptData s = { x = 0.; y = 0.; script = s }

    let getVars x =
        x
        |> Expression.flow
            (fun x y -> x @ y |> Ok)
            (fun vars -> function
                | Complex(ComplexExpression(operator = operator; operands = operands)) ->
                    match operator, operands with
                    | O.readVariable, [Literal(_, SString n)]
                    | O.``changeVar:by:``, [Literal(_, SString n); _]
                    | O.``setVar:to:``, [Literal(_, SString n); _]
                    | O.``hideVariable:``, [Literal(_, SString n)]
                    | O.``showVariable:``, [Literal(_, SString n)] -> n::vars |> Ok
                    | _ -> Ok vars
                | _ -> Ok vars
            ) []
        |> Result.defaultValue []
        |> List.rev

    let stageFromScripts scripts =
        let variables =
            scripts
            |> Seq.map (fun d ->
                match d.script with
                | Expression x -> Complex x
                | Statements x
                | Procedure(ProcedureDefinition(body = x))
                | Listener(ListenerDefinition(body = x)) -> Block x
            )
            |> Seq.collect getVars
            |> Seq.distinct
            |> Seq.sort
            |> Seq.map (fun n -> def n SType.N)
            |> Seq.toList

        { StageData.defaultValue with
            variables = variables
            scripts = scripts
        }
        |> StageData.map Tagged.empty

    let stageFromNoAtomicProcedure name parameters body =
        stageFromScripts [noAtomicProc name parameters body]

    let stageFromAtomicProcedure name parameters body =
        stageFromScripts [atomicProc name parameters body]

[<Fact>]
let pureProragationTest() =
    let stage = stageFromAtomicProcedure "p" [] [
        "x" <-. (n 10. +. n 20.)
        A.show ()
        "y" <-. read "x"
    ]
    let optimized = stageFromAtomicProcedure "p" [] [
        A.show ()
        "y" <-. (n 10. +. n 20.)
    ]
    stage
    |> optimize (fun p -> {
        p with
            transformComplexExpressions = T.purePropagation
            transformEntity = T.eliminateUnusedVariables
    })
    =? optimized

[<Fact>]
let purePropagationDoubleVariableReferenceTest() =
    let stage = stageFromAtomicProcedure "p" [] [
        "x" <-. (n 10. +. n 20.)
        A.show ()
        "y" <-. read "x" +. read "x"
    ]
    stage
    |> optimize (fun p -> { p with transformComplexExpressions = T.purePropagation })
    =? stage

[<Fact>]
let copyPropagationTest() =
    let stage = stageFromAtomicProcedure "p" [] [
        "x" <-. read "a"
        A.show ()
        "y" <-. read "x"
        "a" <-. n 20.
        "z" <-. read "x"
    ]
    let optimized = stageFromAtomicProcedure "p" [] [
        "x" <-. read "a"
        A.show ()
        "y" <-. read "a"
        "a" <-. n 20.
        "z" <-. read "x"
    ]
    stage
    |> optimize (fun p -> {
        p with
            transformComplexExpressions = T.copyPropagation
            transformEntity = T.eliminateUnusedVariables
    })
    =? optimized

[<Fact>]
let copyPropagationRemoveDefinitionTest() =
    let stage = stageFromAtomicProcedure "p" [] [
        "x" <-. read "a"
        A.show()
        "y" <-. read "x"
    ]
    let optimized = stageFromAtomicProcedure "p" [] [
        A.show()
        "y" <-. read "a"
    ]

    stage
    |> optimize (fun p -> {
        p with
            transformComplexExpressions = T.copyPropagation
            transformEntity = T.eliminateUnusedVariables
    })
    =? optimized

[<Fact>]
let copyPropagationBrunchTest() =
    let stage = stageFromAtomicProcedure "p" [] [
        "x" <-. read "a"
        A.doIfElse () (read "b") () [
            "y" <-. read "x"
        ] () [
            "a" <-. n 20.
        ]
        "z" <-. read "x"
    ]
    let optimized = stageFromAtomicProcedure "p" [] [
        "x" <-. read "a"
        A.doIfElse () (read "b") () [
            "y" <-. read "a"
        ] () [
            "a" <-. n 20.
        ]
        "z" <-. read "x"
    ]
    stage
    |> optimize (fun p -> { p with transformComplexExpressions = T.copyPropagation })
    =? optimized

[<Fact>]
let copyPropagationDoRepeatTest() =
    let stage = stageFromNoAtomicProcedure "p" [] [
        "x" <-. read "a"
        "y" <-. read "x"
        A.doRepeat () (read "count") () [
            "z" <-. read "x"
        ]
        "w" <-. read "x"
    ]
    let optimized = stageFromNoAtomicProcedure "p" [] [
        "x" <-. read "a"
        "y" <-. read "a"
        A.doRepeat () (read "count") () [
            "z" <-. read "x"
        ]
        "w" <-. read "x"
    ]
    stage
    |> optimize (fun p -> { p with transformComplexExpressions = T.copyPropagation })
    =? optimized

[<Fact>]
let copyPropagationDoWaitUntilTest() =
    let stage = stageFromAtomicProcedure "p" [] [
        "x" <-. read "a"
        "y" <-. read "x"
        A.doWaitUntil () (read "pred")
        "z" <-. read "x"
    ]
    let optimized = stageFromAtomicProcedure "p" [] [
        "x" <-. read "a"
        "y" <-. read "a"
        A.doWaitUntil () (read "pred")
        "z" <-. read "x"
    ]
    stage
    |> optimize (fun p -> { p with transformComplexExpressions = T.copyPropagation })
    =? optimized

[<Fact>]
let copyPropagationInIfBlockTest() =
    let stage = stageFromAtomicProcedure "p" [] [
        A.doIf () (read "cond") () [
            "y" <-. read "x"
            "x" <-. read "y"
        ]
    ]
    let optimized = stageFromAtomicProcedure "p" [] [
        A.doIf () (read "cond") () [
            "y" <-. read "x"
            "x" <-. read "x"
        ]
    ]
    stage
    |> optimize (fun p -> { p with transformComplexExpressions = T.copyPropagation })
    =? optimized

[<Fact>]
let eliminateDeadSetExpressionTest() =
    let stage = stageFromAtomicProcedure "p" [] [
        A.show()
        "x" <-. n 10.
        A.hide()
    ]
    let optimized = stageFromAtomicProcedure "p" [] [
        A.show()
        A.hide()
    ]
    stage
    |> optimize (fun p ->
        { p with
            transformComplexExpressions = T.eliminateDeadSetExpression
            transformEntity = T.eliminateUnusedVariables
        }
    )
    =? optimized

[<Fact>]
let eliminateDeadSetExpressionShadowingTest() =
    let stage = StageData.map Tagged.empty <| {
        StageData.defaultValue with
            variables = [
                def "x" SType.N
            ]
            scripts = [
                atomicProc "p" [] [
                    "x" <-. n 10.
                ]
            ]
            ObjectDataExtension = {
                StageData.defaultValue.ObjectDataExtension with
                    children = [
                        Choice2Of3 {
                            SpriteData.defaultValue with
                                variables = [
                                    def "x" SType.N
                                ]
                                scripts = [
                                    atomicProc "q" [] [
                                        "x" <-. read "x"
                                    ]
                                ]
                        }
                    ]
            }
    }
    let optimized = StageData.map Tagged.empty <| {
        StageData.defaultValue with
            variables = [
                def "x" SType.N
            ]
            scripts = [
                atomicProc "p" [] []
            ]
            ObjectDataExtension = {
                StageData.defaultValue.ObjectDataExtension with
                    children = [
                        Choice2Of3 {
                            SpriteData.defaultValue with
                                variables = [
                                    def "x" SType.N
                                ]
                                scripts = [
                                    atomicProc "q" [] [
                                        "x" <-. read "x"
                                    ]
                                ]
                        }
                    ]
            }
    }
    stage
    |> optimize (fun p -> { p with transformComplexExpressions = T.eliminateDeadSetExpression })
    =? optimized

[<Fact>]
let eliminateDeadSetExpressionShadowingSpriteTest() =
    let stage = StageData.map Tagged.empty <| {
        StageData.defaultValue with
            variables = [
                def "y" SType.N
            ]
            scripts = [
                atomicProc "p" [] [
                    "y" <-. read "y"
                ]
            ]
            ObjectDataExtension = {
                StageData.defaultValue.ObjectDataExtension with
                    children = [
                        Choice2Of3 {
                            SpriteData.defaultValue with
                                variables = [
                                    def "y" SType.N
                                ]
                                scripts = [
                                    atomicProc "q" [] [
                                        "y" <-. n 20.
                                    ]
                                ]
                        }
                    ]
            }
    }
    let optimized = StageData.map Tagged.empty <| {
        StageData.defaultValue with
            variables = [
                def "y" SType.N
            ]
            scripts = [
                atomicProc "p" [] [
                    "y" <-. read "y"
                ]
            ]
            ObjectDataExtension = {
                StageData.defaultValue.ObjectDataExtension with
                    children = [
                        Choice2Of3 {
                            SpriteData.defaultValue with
                                variables = [
                                    def "y" SType.N
                                ]
                                scripts = [
                                    atomicProc "q" [] []
                                ]
                        }
                    ]
            }
    }
    stage
    |> optimize (fun p -> { p with transformComplexExpressions = T.eliminateDeadSetExpression })
    =? optimized

[<Fact>]
let eliminateDeadSetExpressionStageSetTest() =
    let stage = StageData.map Tagged.empty <| {
        StageData.defaultValue with
            variables = [
                def "z1" SType.N
            ]
            ObjectDataExtension = {
                StageData.defaultValue.ObjectDataExtension with
                    children = [
                        Choice2Of3 {
                            SpriteData.defaultValue with
                                scripts = [
                                    atomicProc "q" [] [
                                        "z1" <-. n 30.
                                    ]
                                ]
                        }
                    ]
            }
    }
    let optimized = StageData.map Tagged.empty <| {
        StageData.defaultValue with
            variables = [
                def "z1" SType.N
            ]
            ObjectDataExtension = {
                StageData.defaultValue.ObjectDataExtension with
                    children = [
                        Choice2Of3 {
                            SpriteData.defaultValue with
                                scripts = [
                                    atomicProc "q" [] []
                                ]
                        }
                    ]
            }
    }
    stage
    |> optimize (fun p -> { p with transformComplexExpressions = T.eliminateDeadSetExpression })
    =? optimized

[<Fact>]
let eliminateDeadSetExpressionStageUpdateSpriteSetTest() =
    let stage = StageData.map Tagged.empty <| {
        StageData.defaultValue with
            variables = [
                def "z2" SType.N
            ]
            scripts = [
                atomicProc "p" [] [
                    "z2" <-. read "z2"
                ]
            ]
            ObjectDataExtension = {
                StageData.defaultValue.ObjectDataExtension with
                    children = [
                        Choice2Of3 {
                            SpriteData.defaultValue with
                                scripts = [
                                    atomicProc "q" [] [
                                        "z2" <-. n 40.
                                    ]
                                ]
                        }
                    ]
            }
    }
    stage
    |> optimize (fun p -> { p with transformComplexExpressions = T.eliminateDeadSetExpression })
    =? stage

[<Fact>]
let eliminateDeadSetExpressionStageSetSpriteUpdateTest() =
    let stage = StageData.map Tagged.empty <| {
        StageData.defaultValue with
            variables = [
                def "z4" SType.N
            ]
            scripts = [
                atomicProc "p" [] [
                    "z4" <-. n 60.
                ]
            ]
            ObjectDataExtension = {
                StageData.defaultValue.ObjectDataExtension with
                    children = [
                        Choice2Of3 {
                            SpriteData.defaultValue with
                                scripts = [
                                    atomicProc "q" [] [
                                        "z4" <-. read "z4"
                                    ]
                                ]
                        }
                    ]
            }
    }
    stage
    |> optimize (fun p -> { p with transformComplexExpressions = T.eliminateDeadSetExpression })
    =? stage

[<Fact>]
let eliminateDeadProceduresTest() =
    let stage = stageFromScripts [
        atomicProc "DeadRec" [] [
            A.call () "DeadRec" []
        ]

        atomicProc "DeadMutualRec1" [] [
            A.call () "DeadMutualRec2" []
        ]
        atomicProc "DeadMutualRec2" [] [
            A.call () "DeadMutualRec1" []
        ]

        atomicProc "Alive1" [] [
        ]
        atomicProc "Alive2" [] [
            A.call () "Alive1" []
        ]
        A.whenGreenFlag () [
            A.call () "Alive2" []
        ]

        atomicProc "Alive3" [] []
        scriptData <| Expression(A.call () "Alive3" [])

        atomicProc "Alive4" [] []
        scriptData <| Statements(BlockExpression((), [A.call () "Alive4" []]))
    ]
    let optimized = stageFromScripts [
        atomicProc "Alive1" [] [
        ]
        atomicProc "Alive2" [] [
            A.call () "Alive1" []
        ]
        A.whenGreenFlag () [
            A.call () "Alive2" []
        ]

        atomicProc "Alive3" [] []
        scriptData <| Expression(A.call () "Alive3" [])

        atomicProc "Alive4" [] []
        scriptData <| Statements(BlockExpression((), [A.call () "Alive4" []]))
    ]

    stage
    |> optimize (fun p -> { p with transformEntity = T.eliminateDeadProcedures })
    =? optimized

[<Fact>]
let joinWhenGreenFlagsTest() =
    let stage = stageFromScripts [
        A.whenGreenFlag () ["x" <-. n 0.]
        atomicProc "p" [] []
        A.whenGreenFlag () ["x" <-. n 1.]
        A.whenGreenFlag () [A.``wait:elapsed:from:`` () (n 0.)]

        A.whenGreenFlag () ["x" <-. n 2.]
        scriptData <| Expression(ComplexExpression((), O.readVariable, [A.eString () "a"]))
        A.whenGreenFlag () ["x" <-. n 3.]
        A.whenCloned () []

        A.whenGreenFlag () ["x" <-. n 4.]
        scriptData <| Statements(BlockExpression((), []))
        A.whenGreenFlag () ["x" <-. n 5.]
    ]
    let optimized = stageFromScripts [
        atomicProc "p" [] []
        A.whenGreenFlag () ["x" <-. n 0.
                            "x" <-. n 1.]
        A.whenGreenFlag () [A.``wait:elapsed:from:`` () (n 0.)]

        scriptData <| Expression(ComplexExpression((), O.readVariable, [A.eString () "a"]))
        A.whenGreenFlag () ["x" <-. n 2.
                            "x" <-. n 3.]
        A.whenCloned () []

        scriptData <| Statements(BlockExpression((), []))
        A.whenGreenFlag () ["x" <-. n 4.
                            "x" <-. n 5.]
    ]

    stage
    |> optimize (fun p -> { p with transformEntity = T.joinWhenGreenFlags })
    =? optimized

[<Fact>]
let joinWhenGreenFlagsSimpleTest() =
    let stage = stageFromScripts [
        A.whenGreenFlag () ["x" <-. n 0.]
        A.whenGreenFlag () ["x" <-. n 1.]
    ]
    let optimized = stageFromScripts [
        A.whenGreenFlag () ["x" <-. n 0.
                            "x" <-. n 1.]
    ]

    stage
    |> optimize (fun p -> { p with transformEntity = T.joinWhenGreenFlags })
    =? optimized

[<Fact>]
let inlineExpansionTest() =
    let parameterDef x = ParameterDefinition((), x, SType.parameterDefaultValue SType.N)
    let param x = A.getParam () x

    let stage = stageFromScripts [
        atomicProc "p1" [parameterDef "a"; parameterDef "b"] [
            "p1.result" <-. param "a" +. param "b"
        ]
        atomicProc "p2" [] [
            A.call () "p1" [n 10.; n 20.]
        ]
    ]
    let optimized = stageFromScripts [
        atomicProc "p1" [parameterDef "a"; parameterDef "b"] [
            "p1.result" <-. param "a" +. param "b"
        ]
        atomicProc "p2" [] [
            "p2.a" <-. n 10.
            "p2.b" <-. n 20.
            "p1.result" <-. read "p2.a" +. read "p2.b"
        ]
    ]
    stage
    |> optimize (fun p -> { p with transformEntity = T.inlineExpansion id })
    =? optimized

[<Fact>]
let inlineExpansionAtomicityTest() =
    let stage = stageFromScripts [
        atomicProc "atomic1" [] [
            "atomic1.result" <-. n 10.
        ]
        noAtomicProc "p1" [] [
            A.call () "atomic1" []
        ]

        noAtomicProc "p2" [] [
            "p2.result" <-. n 20.
        ]
        atomicProc "atomic2" [] [
            A.call () "p2" []
        ]
    ]
    stage
    |> optimize (fun p -> { p with transformEntity = T.inlineExpansion id })
    =? stage

[<Fact>]
let inlineExpansionMutualRecutionTest() =
    let stage = stageFromScripts [
        noAtomicProc "mutual1" [] [
            "x" <-. read "x" +. n 10.
            A.``wait:elapsed:from:`` () (n 0.)
            A.call () "mutual3" []
        ]
        noAtomicProc "mutual2" [] [
            A.call () "mutual1" []
        ]
        noAtomicProc "mutual3" [] [
            A.call () "mutual2" []
        ]
        A.whenGreenFlag () [
            A.call () "mutual3" []
        ]
    ]
    let optimized = stageFromScripts [
        noAtomicProc "mutual2" [] [
            "x" <-. read "x" +. n 10.
            A.``wait:elapsed:from:`` () (n 0.)
            A.call () "mutual2" []
        ]
        A.whenGreenFlag () [
            A.call () "mutual2" []
        ]
    ]
    stage
    |> optimize (fun p -> {
        p with
            transformEntity = Transformer.merged [
                T.inlineExpansion <| fun c -> { c with maxProcedureSize = 100 }
                T.eliminateDeadProcedures
            ]
    })
    =? optimized

[<Fact>]
let eliminateDeadConditionExpressionTest() =
    let optimize = optimize <| fun p -> {
        p with
            transformComplexExpressions = T.eliminateDeadConditionExpression
            transformEntity = T.eliminateUnusedVariables
    }
    let stageFromAtomicDo body = stageFromAtomicProcedure "p" [] body

    // `doIfElse $test $ifTrue $ifFalse` ( truthy $test ) => `$ifTrue`
    // `doIfElse $test $ifTrue $ifFalse` ( falsy $test ) => `$ifFalse`
    quickcheck <| fun test (NormalFloat ifTrue) (NormalFloat ifFalse) ->
        let ifTrue = ["a" <-. n ifTrue]
        let ifFalse = ["a" <-. n ifFalse]
        stageFromAtomicDo [
            A.doIfElse () (Literal((), test)) () ifTrue () ifFalse
        ]
        |> optimize
        =? stageFromAtomicDo (
            if SValue.toBool test then ifTrue else ifFalse
        )

    // `doIf $test $ifTrue` ( truthy $test ) => `$ifTrue`
    // `doIf $test $ifTrue` ( falsy $test ) => ``
    quickcheck <| fun test (NormalFloat ifTrue) ->
        let ifTrue = ["a" <-. n ifTrue]
        stageFromAtomicDo [
            A.doIf () (Literal((), test)) () ifTrue
        ]
        |> optimize
        =? stageFromAtomicDo (
            if SValue.toBool test then ifTrue else []
        )

    // `doRepeat $count $body` ( atomic, $count <= 0 ) => ``
    // `doRepeat 1 $body` ( atomic ) => `$body`
    quickcheck <| fun count (NormalFloat body) ->
        let body = ["a" <-. n body]
        let repeat = A.doRepeat () (Literal((), SNumber(double count))) () body
        stageFromAtomicDo [
            repeat
        ]
        |> optimize
        =? stageFromAtomicDo (
            if count <= 0 then []
            elif count = 1 then body
            else [repeat]
        )

    // `doUntil $test $ifFalse` ( atomic, truthy $test ) => ``
    // `doUntil $test $ifFalse` ( false $test ) => `doForever $ifFalse`
    quickcheck <| fun test (NormalFloat ifFalse) ->
        let ifFalse = ["a" <-. n ifFalse]

        stageFromAtomicDo [
            A.doUntil () (Literal((), test)) () ifFalse
        ]
        |> optimize
        =? stageFromAtomicDo (
            if SValue.toBool test then [] else [A.doForever () () ifFalse]
        )

[<Fact>]
let algebraicSimplificationTest() =
    let optimize = optimize <| fun p -> { p with transformExpression = T.algebraicSimplification }
    let stageFromAtomicDo body = stageFromAtomicProcedure "p" [] body

    stageFromAtomicDo [
        "a" <-. A.``/`` () (read "x") (n 0.)
    ]
    |> optimize
    =? stageFromAtomicDo [
        "a" <-. A.``*`` () (read "x") (n infinity)
    ]
