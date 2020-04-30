module Scratch.Detranspiler.Typing.Tests
open Scratch
open Scratch.Ast
open Scratch.AstDefinitions
open Scratch.Transpiler
open Scratch.Detranspiler
open Xunit
type private T = Ast.TsType
module T = Scratch.AstDefinitions.TsType
module A = Scratch.Ast.Expressions


let inferStage showLocation data =
    let tsType struct(_, TypeScheme(_, t)) = t
    let ws = ResizeArray()
    try
        let env = {
            config = makeDefaultConfig showLocation
            warnings = ws
            resolveEnv = {
                typeNameMemo = makeMemo()
                assemblyEntityMemo = makeMemo()
            }
        }
        let data = data |> infer env
        let vs = data.variables |> List.map (fun v -> v.name, tsType v.state)
        let ls = data.lists |> List.map (fun x -> x.listName, tsType x.state)
        let ps =
            data.scripts
            |> List.choose (fun s ->
                match s.script with
                | Procedure(ProcedureDefinition(state = struct(_, TypeScheme(vs, t)); name = name)) ->
                    match t with
                    | T.Named(_, pts) -> Some(name, (List.map ignore vs, pts))
                    | _ -> failwith ""
                | _ -> None
            )
        vs, ls, ps
    with DetranspileException(location, e, _) -> failwithf "%A" (location, e, Seq.toList ws)

let transpileAndInfer entryPoint =
    let data: _ StageData = transpileStage entryPoint
    inferStage showLoc data

[<Fact>]
let simpleVariableTest() =
    let ts = <@ let out = "" in () @> |> transpileAndInfer
    ts =? (["out", T.gString], [], [])

[<Fact>]
let simpleProcedureTest() =
    let ts = <@ let f() = () in () @> |> transpileAndInfer
    ts =? ([], [], ["f", ([], [])])

[<Fact>]
let simpleGenericProcedureTest() =
    let ts = <@ let f (a: int) = () in () @> |> transpileAndInfer
    ts =? ([], [], ["f %n", ([()], [T.GVar 0])])

[<Fact>]
let assignTest() =
    <@
    let mutable out = ""
    let f x = out <- x
    ()
    @>
    |> transpileAndInfer =? (
        ["out", T.gString],
        [],
        ["f %s", ([], [T.gString])]
    )

[<Fact>]
let warningTest() =
    <@
        let mutable out = ""
        let unuseList = defineList []
        let unuseVar = 0
        let f x y =
            out <- string(x + y)
            out <- "bad"
        ()
    @>
    |> transpileAndInfer
    =? (
        [
        "out", T.gString
        "unuseVar", T.gNumber
        ],
        [
        "unuseList", T.GVar 0
        ],
        [
        "f %n %n", ([], [T.gNumber; T.gNumber])
        ]
    )

[<Fact>]
let unboundParameterWarningTest() =
    { StageData.defaultValue with
        scripts = [
            ScriptData.procedure () "f" [] Atomic [A.doIf () (A.getParam () "x") () []]
        ]
    }
    |> inferStage (fun () -> "")
    =? (
        [],
        [],
        [
        "f", ([], [])
        ]
    )