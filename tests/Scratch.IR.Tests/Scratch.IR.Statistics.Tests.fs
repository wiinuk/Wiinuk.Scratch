module Scratch.IR.Statistics.Tests
open Scratch.Ast
open Scratch.IR
open Xunit

module Exp = Exp.Op


[<AutoOpen>]
module private Helpers =
    let infer xs =
        let xs = xs |> List.map (fun s -> { x = 0.; y = 0.; script = s })
        let stat = EntityStat.make { StageData.defaultValue with scripts = xs }
        xs
        |> Seq.choose (function { script = Top.Procedure p } -> Some <| ProcDef.var p | _ -> None)
        |> Seq.choose (fun v ->
            match EntityStat.procedureYieldability stat v with
            | NeverYield -> None
            | y -> Some(v, y)
        )
        |> Map.ofSeq

    let newVar n = Var.newProc n [] ExpTypes.empty

    let empty = ()
    let top = Top.builderWith ()
    let exp = top.Exp
    let op = exp.Op

    let atomic var body = top.proc(var, [], exp.concat body)
    let noatomic var body = top.proc(var, [], exp.concat body, NoAtomic)
    let atom e = Exp.atom empty e

    let opNeverYield = Exp.hide empty
    let opNormalYield = op.doRepeat(exp.number 1., exp.empty())
    let opForceYield = Exp.doWaitUntil empty (Exp.mousePressed empty)
    let call var = exp.call(var, [])

    let a = newVar "a"
    let b = newVar "b"
    let c = newVar "c"
    let d = newVar "d"

    let letX = exp.letScope("x", exp.number 0.) <| fun _ -> exp.empty()
    let stackDepths procedures =
        let scripts = procedures |> List.map (fun s -> { x = 0.; y = 0.; script = s })
        let stat = EntityStat.make { StageData.defaultValue with scripts = scripts }
        scripts
        |> Seq.choose (function { script = Top.Procedure p } -> Some <| ProcDef.var p | _ -> None)
        |> Seq.map (fun v ->
            let d = EntityStat.procedureStackDepth stat v
            v, d
        )
        |> Map.ofSeq


[<Fact>]
let simpleTest() =
    [
        atomic a [opNeverYield]
        atomic b [opNormalYield]
        atomic c [opForceYield]
    ]
    |> infer
    =? Map [
        c, ForceYield
    ]

    [
        noatomic a [opNeverYield]
        noatomic b [opNormalYield]
        noatomic c [opForceYield]
    ]
    |> infer
    =? Map [
        b, NormalYield
        c, ForceYield
    ]

[<Fact>]
let callTest() =
    [
        atomic a [opForceYield]
        atomic b [call a]
    ]
    |> infer
    =? Map [
        a, ForceYield
        b, ForceYield
    ]

    [
        noatomic a [opNormalYield]
        atomic b [call a]
    ]
    |> infer
    =? Map [
        a, NormalYield
    ]

    [
        noatomic a [opNeverYield]
        noatomic b [call a]
    ]
    |> infer
    =? Map []

[<Fact>]
let recCallTest() =
    [
        noatomic a [
            opNormalYield
            call b
        ]
        atomic b [
            opForceYield
            call a
        ]
        noatomic c [
            call a
        ]
        atomic d [
            call c
        ]
    ]
    |> infer
    =? Map [
        a, ForceYield
        b, ForceYield
        c, ForceYield
        d, ForceYield
    ]

[<Fact>]
let atomNormalYieldTest() =
    [
        noatomic a [opNormalYield]
        noatomic b [atom (call a)]
    ]
    |> infer
    =? Map [
        a, NormalYield
    ]

[<Fact>]
let atomForceYieldTest() =
    [
        noatomic a [opForceYield]
        noatomic b [atom (call a)]
        atomic c [call b]
    ]
    |> infer
    =? Map [
        a, ForceYield
        b, ForceYield
        c, ForceYield
    ]
    
[<Fact>]
let simpleStackDepthTest() =
    [
        noatomic c [opNormalYield; letX; letX]
    ]
    |> stackDepths
    =? Map [
        c, Finite 2
    ]

[<Fact>]
let callAtomicTest() =
    [
        atomic a [letX]
        noatomic b [opNormalYield; call a; letX; letX]
    ]
    |> stackDepths
    =? Map [
        a, Finite 0
        b, Finite 2
    ]

[<Fact>]
let callNoatomicTest() =
    [
        noatomic a [opNormalYield; letX]
        noatomic b [opNormalYield; call a; letX; letX]
        noatomic c [opNormalYield; call a; call b]
    ]
    |> stackDepths
    =? Map [
        a, Finite 1
        b, Finite 3
        c, Finite 3
    ]

[<Fact>]
let callInfinityTest() =
    [
        noatomic a [opNormalYield; letX; call b]
        noatomic b [opNormalYield; call a; letX]
        noatomic c [opNormalYield; call b; letX]
    ]
    |> stackDepths
    =? Map [
        a, Infinity
        b, Infinity
        c, Infinity
    ]

[<Fact>]
let callAtomicInfinityTest() =
    [
        atomic a [letX; call b]
        atomic b [call a; letX]
        noatomic c [opNormalYield; call b; letX]
    ]
    |> stackDepths
    =? Map [
        a, Finite 0
        b, Finite 0
        c, Finite 1
    ]
