module Scratch.Executor.Tests
open System
open Scratch
open Scratch.Primitives
open Scratch.Threading
open Scratch.Ast
open Scratch.Executor
open Scratch.Executor.Diagnostics
open Scratch.Executor.Diagnostics.Printer
open Scratch.Executor.Executor
open Scratch.Transpiler
open Xunit

module A = Expression
module A = Expressions
module A = AstDefinitions
module P = ParameterDefinition


let findStageList name state =
    state
    |> ExecutionState.stage
    |> EntityState.list name
    |> Option.defaultValue []

let startAsStdStage expr =
    let image = expr |> transpileStage |> Compiler.compileToImage
    let state = Executor.runImage image
    let list = findStageList "output" state
    list |> List.map SValue.toString

[<Struct>]
type ConsoleObserver = | ConsoleObserver with
    interface IExecutionObserver with
        member _.OnEnterExecute _ref = ()
        member _.OnPreExecute state =
            printInstruction stdout state.stage.image state.self.entityImage state.currentFlame.procedure state.position
            printf " "
            printf "["
            for i in Index.zero..1..state.data.next-1 do
                printf "%A; " <| state.data.items.Address i
            printf "]"
            printfn ""
        member _.OnLeaveExecute(_ref1, _ref2) = ()

let startSpriteInStage whenGreenFlagBody =
    let stage = Scratch.Evaluator.Tests.makeSpriteInStage whenGreenFlagBody
    let schedulerConfig = {
        SchedulerConfig.deterministic = DeterministicTime {
            flameSpan = TimeSpan.FromSeconds(1. / 60.)
            processSpan = TimeSpan.FromTicks 1L
        }
        startTime = Some <| DateTime(2010, 1, 1)
        flame = Turbo
    }
    let state =
        stage
        |> Compiler.compileToImage
        |> Executor.runImage
    let spriteName = match stage.ObjectDataExtension.children.[0] with Choice2Of3 x -> x.objName | _ -> failwith ""
    state.mainStage.originalSpriteMap.[spriteName]

let ``changeGraphicEffect:by:`` name value =
    ComplexExpression((), O.``changeGraphicEffect:by:``, [name; A.eNumber () value])

[<Fact>]
let simpleTest() =
    <@
        if true then
            SList.push output "TRUE"
        else
            SList.push output ("FALSE" + " Error")
    @>
    |> startAsStdStage
    =? ["TRUE"]

[<Fact>]
let atomicProcedureTest() =
    <@
        let out2 x = tightrope {
            do! repeatAsync 2 {
                out (string x)
                out ";"
            }
            outLine ""
        }
        let mutable flag = false
        let f x y = atomic {
            if flag then
                do! out2 (x - y)
                flag <- false
            else
                do! out2 (x / y)
                flag <- true
        }
        whenGreenFlag {
            do! repeatAsync 2 {
                do! awaitAtomic(f 10. 20.)
            }
        }
        whenGreenFlag {
            do! repeatAsync 2 {
                do! awaitAtomic(f 1. 2.)
            }
        }
        startMainLoop()
    @>
    |> startAsStdStage
    =? ["0.5;0.5;"; "-1;-1;"; "0.5;0.5;"; "-1;-1;"; ""]

[<Fact>]
let filterTest() =
    let state = startSpriteInStage [
        ``changeGraphicEffect:by:`` (A.eString () "fisheye") 10.

        ``changeGraphicEffect:by:`` (A.eString () "brightness") 10.
        ``changeGraphicEffect:by:`` (A.``concatenate:with:`` () (A.eString () "bright") (A.eString () "ness")) 5.
        
        ``changeGraphicEffect:by:`` (A.eString () "pixelate") 20.
        ``changeGraphicEffect:by:`` (A.eString () "mosaic") 25.

        ``changeGraphicEffect:by:`` (A.eString () "color") 25.
        ``changeGraphicEffect:by:`` (A.``concatenate:with:`` () (A.eString () "co") (A.eString () "lor")) 5.

        ``changeGraphicEffect:by:`` (A.eString () "ghost") 30.
        ``changeGraphicEffect:by:`` (A.``concatenate:with:`` () (A.eString () "g") (A.eString () "host")) 5.

        ``changeGraphicEffect:by:`` (A.eString () "UNKNOWN") 40.
    ]
    let data = state.drawingData.filters
    data.fisheye =? 10.
    data.brightness =? 15.
    data.pixelate =? 20.
    data.mosaic =? 25.
    data.color =? 30.
    data.ghost =? 35.
    data.others.["UNKNOWN"] =? 40.

let makeStage location whenGreenFlagBody =
    let stage = StageData.defaultValue
    let stage =
        { stage with
            scripts = [
                A.whenGreenFlag location whenGreenFlagBody
            ]
        }
    stage


[<Fact>]
let locationTest() =
    let stage = StageData.defaultValue
    let stage =
        { stage with
            lists = [
                {
                    listName = "output"
                    state = "@output"
                    isPersistent = NoPersistent
                    contents' = IArray.empty
                    x = 0.
                    y = 0.
                    width = 100.
                    height = 100.
                    visible = Hidden
                }
            ]
            scripts = [
                A.whenGreenFlag "@whenGreenFlag" [
                    A.``append:toList:`` "@append" "output" (A.``+`` "@+" (A.eNumber "@10" 10.) (A.eNumber "@20" 20.))
                ]
            ]
        }
    let image = Compiler.compileToImage stage
    
    // Number 10.
    // Number 20.
    // Add
    // AppendStageList output
    // Return
    let whenGreenFlag = image.procedures.Address(IArray.ref 0 image.stageImage.whenGreenFlags)
    let i = whenGreenFlag.startAddress
    let location i = IndexMap.tryFind i image.locationMap

    whenGreenFlag.codeLength =? 5
    location (i + 0) =? ValueSome "@10"
    location (i + 1) =? ValueSome "@20"
    location (i + 2) =? ValueSome "@+"
    location (i + 3) =? ValueSome "@append"

let runAsStdStage stage =
    let image = stage |> Compiler.compileToImage
    let state = Executor.runImage image
    let list = findStageList "output" state
    list |> Seq.map SValue.toString |> Seq.toList

[<Fact>]
let orElseTest() =
    /// getParam
    let p = A.getParam ()
    /// eNumber
    let n = double >> A.eNumber ()
    let s = A.eString()

    let (.<) l r = A.``<`` () l r
    let (.|) l r = A.``|`` () l r
    let stage: unit StageData =
        { StageData.defaultValue with
            lists = [
                ListData.make () "output" []
            ]
            scripts = [
                ScriptData.procedure () "f" [P.make () "x" SType.N] Atomic [
                    A.doIfElse () (
                        (p"x" .< n 10) .| (p"x" .< n 20)
                    ) () [
                        A.``append:toList:`` () "output" (s"TRUE")
                    ] () [
                        A.``append:toList:`` () "output" (s"FALSE")
                    ]
                ]
                A.whenGreenFlag () [
                    A.call () "f" [n 5]
                    A.call () "f" [n 15]
                    A.call () "f" [n 25]
                ]
            ]
        }

    runAsStdStage stage =? [
        "TRUE"
        "TRUE"
        "FALSE"
    ]

[<Fact>]
let showHideTest() =
    let sprite = startSpriteInStage [
        A.hide ()
    ]
    sprite.spriteDrawingData.visible =? false

    let sprite = startSpriteInStage [
        A.show ()
    ]
    sprite.spriteDrawingData.visible =? true
