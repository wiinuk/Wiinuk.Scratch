module Scratch.IR.Tests
open Scratch
open Scratch.Ast
open Scratch.Evaluator
open Scratch.Primitives
open Scratch.Transformers
open Scratch.IR
open Xunit

module E = Quotations.Patterns
module Exp = Exp.Op
module Types = ExpTypes
module VOption = ValueOption
#nowarn "0086"


[<AutoOpen>]
module private Helpers =
    let empty = Tagged.empty ()

    module Top =
        let getVars = function
            | Top.Listener { value = ListenerDef(body = e) }
            | Top.VariableInit(VariableInitDef(init = e)) -> Exp.getFreeVars e
            | Top.Procedure { value = ProcDef(header = ProcHeader(parameters = ps); body = e) } ->
                let ps = ps |> List.map (fun p -> p.value)
                Exp.getFreeVars e |> Seq.filter (fun v -> not <| List.contains v ps)

        let getListVars = function
            | Top.Listener { value = ListenerDef(body = e) }
            | Top.VariableInit(VariableInitDef(init = e))
            | Top.Procedure { value = ProcDef(body = e) } ->
                e
                |> Exp.fold (fun lists e ->
                    match e.value with
                    | ListOp(_, operands) ->
                        operands
                        |> List.fold (fun lists -> function
                            | Choice2Of2 list -> list.value::lists
                            | _ -> lists
                        ) lists
                    | _ -> lists
                ) []
                |> List.rev

    module StageData =
        let fromScripts scripts =
            let variables =
                scripts
                |> Seq.collect Top.getVars
                |> Seq.distinct
                |> Seq.map (fun n -> VariableDecl.make empty n NoPersistent)
                |> Seq.toList

            let lists =
                scripts
                |> Seq.collect Top.getListVars
                |> Seq.distinct
                |> Seq.map (fun n -> ListInitData.make empty n IArray.empty)
                |> Seq.toList

            { StageData.defaultValue with
                variables = variables
                lists = lists
                scripts = scripts |> List.map (fun x -> { x = 0.; y = 0.; script = x })
            }

    let list state listName =
        Map.tryFind listName state.stage.lists |> VOption.map (Seq.map SValue.toString >> Seq.toList)

    type TopBuilder<'a> with
        member private b.Var s n t =
            let s = Option.defaultValue b.DefaultSource s
            IR.Param.make s n t

        member private b.Get s p =
            let s = Option.defaultValue b.DefaultSource s
            Exp.param s p

        member b.procedure(name, parameter1, scope, ?atomicity, ?param1Source, ?getParam1Source, ?callSource, ?source) =
            let param1, type1 = parameter1
            let p1 = b.Var param1Source param1 type1
            let body = scope (b.Get getParam1Source p1)
            let proc, var = b.procAndNewVar(name, [p1], body, ?atomicity = atomicity, ?source = source)
            proc, fun (x) -> Exp.call (Option.defaultValue b.DefaultSource callSource) var [x]

        member b.procedure(name, parameter1, parameter2, scope, ?atomicity, ?param1Source, ?getParam1Source, ?param2Source, ?getParam2Source, ?callSource, ?source) =
            let (param1, type1), (param2, type2) = parameter1, parameter2

            let p1 = b.Var param1Source param1 type1
            let p2 = b.Var param2Source param2 type2
            let body = scope (b.Get getParam1Source p1, b.Get getParam2Source p2)
            let proc, var = b.procAndNewVar(name, [p1; p2], body, ?atomicity = atomicity, ?source = source)
            proc, fun (x1, x2) -> Exp.call (Option.defaultValue b.DefaultSource callSource) var [x1; x2]

        member private __.NewProcVar(name, ps, resultType) =
            let parameterTypes = ps |> List.map (fun x -> Var.varType x.value)
            Var.newProc name parameterTypes resultType

        member b.procedureRec(name, parameter1, resultType, scope, ?atomicity, ?param1Source, ?getParam1Source, ?callSource, ?source) =
            let param1, type1 = parameter1
            let p1 = b.Var param1Source param1 type1
            let var = b.NewProcVar(name, [p1], resultType)
            let call x = Exp.call (Option.defaultValue b.DefaultSource callSource) var [x]
            let body = scope (call, b.Get getParam1Source p1)
            let proc = b.proc(var, [p1], body, ?atomicity = atomicity, ?source = source)
            proc, call

    let top = Top.builderWith empty
    let exp = top.Exp
    let op = exp.Op
    let listener = top.Listener

    let startStandardIrStage withConfig stage =
        let config = {| ir = IRToStageData.Config.defaultConfig; evaluate = EvaluateConfig.makeDefault() |}
        let config = if true then withConfig config else config

        let data = IRToStageData.convertWith (fun _ -> config.ir) stage
        let state = Evaluator.evaluateStage (fun _ -> config.evaluate) data
        list state "output" |> VOption.defaultValue []

[<Fact>]
let simpleTest() =
    let stage =
        let outputV = Var.newSimple "output"
        StageData.fromScripts [
            top.Listener.whenGreenFlag(
                op.``append:toList:``(exp.string "hello", outputV)
            )
        ]

    stage
    |> startStandardIrStage (fun c -> {| c with ir = { c.ir with maxStackCapacity = 8 } |})
    =? ["hello"]

[<Fact>]
let multiValueCallTest() =
    let stage =
        let divmod, callDivmod =
            top.procedure("divmod", ("x", Types.number), ("y", Types.number), (fun (x, y) ->
                exp.newTuple [
                    op.``/``(x, y)
                    op.``%``(x, y)
                ]
            ), Atomic)

        let outputV = Var.newSimple "output"
        StageData.fromScripts [
            divmod
            top.Listener.whenGreenFlag(
                exp.letScope("tuple2", callDivmod(exp.number 10., exp.number 4.)) <| fun tuple2 -> exp.concat [
                    op.``append:toList:``(exp.tupleGet(tuple2, 0), outputV)
                    op.``append:toList:``(exp.tupleGet(tuple2, 1), outputV)
                ]
            )
        ]

    stage
    |> startStandardIrStage (fun c -> {| c with ir = { c.ir with maxStackCapacity = 8 } |})
    =? ["2.5"; "2"]

// ```
// @async let out2 (n: S) =
//     let mutable i = 0
//     doRepeat 2 \{
//         append:toList: (concatenate:with: n (i :> string)) outputV
//         i <- i + 1
//     }
// ```
let out2 outputV =
    top.procedure("out2", ("n", Types.string), fun n -> exp.concat [
        exp.letMutableScope("i", exp.number 0.) <| fun i iV ->

        op.doRepeat(exp.number 2., exp.concat [
            op.``append:toList:``(op.``concatenate:with:``(n, exp.toS i), outputV)
            exp.varSet(iV, op.``+``(i, exp.number 1.))
        ])
    ], NoAtomic)

[<Fact>]
let multiThreadLetTest() =
    let stage =
        let outputV = Var.newSimple "output"
        let out2, callOut2 = out2 outputV
        StageData.fromScripts [
            out2
            listener.whenGreenFlag(callOut2(exp.string "A"))
            listener.whenGreenFlag(callOut2(exp.string "B"))
        ]


    stage
    |> startStandardIrStage id
    =? ["A0"; "B0"; "A1"; "B1"]

[<Fact>]
let recLetTest() =
    let stage =
        let output = Var.newSimple "output"
        (*
        let outN n =
            if (n < 1)
            then ()
            else
                let temp = n * 10
                outN (n - 1)
                append:toList: (temp :> S) output
        *)
        let outN, callOutN = top.procedureRec("outN", ("n", Types.number), Types.empty, (fun (callOutN, n) ->
            exp.if' (
                op.``<``(n, exp.number 1.),
                exp.empty(),
                exp.concat [
                    exp.letScope("temp", op.``*``(n, exp.number 10.)) <| fun temp -> exp.concat [
                    callOutN(op.``-``(n, exp.number 1.))
                    op.``append:toList:``(exp.toS temp, output)
                    ]
                ]
            )
        ), atomicity = NoAtomic)

        StageData.fromScripts [
            outN
            listener.whenGreenFlag(callOutN(exp.number 3.))
        ]

    stage
    |> startStandardIrStage id
    =? ["10"; "20"; "30"]

[<Fact>]
let stackInitTest() =
    let list s = list s >> VOption.defaultValue []
    let listSnapshots s = s.stage.lists |> Map.map (fun _ xs -> Seq.toList xs)
    let runAndAbortN n state =
        Scratch.Threading.Scheduler.reset state.scheduler
        registerWhenGreenFlags state
        for _ in 1..n do Scratch.Threading.Scheduler.step state.scheduler

    let stage =
        let outputV = Var.newSimple "output"
        let out2, callOut2 = out2 outputV
        StageData.fromScripts [
            out2
            listener.whenGreenFlag(callOut2(exp.string "A"))
        ]

    let data = IRToStageData.convertWith (fun c -> { c with maxStackCapacity = 5 }) stage
    let state = EvaluateState.ofStageData (EvaluateConfig.makeDefault()) data

    runAndAbortN 1 state
    list state "output" =? ["A0"]
    let lists1 = listSnapshots state

    runAndAbortN 1 state
    list state "output" =? ["A0"]
    let lists2 = listSnapshots state

    lists2 =? lists1
