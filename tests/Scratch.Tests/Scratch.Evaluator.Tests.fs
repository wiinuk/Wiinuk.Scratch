module Scratch.Evaluator.Tests
open Scratch
open Scratch.Primitives
open Scratch.Ast
open Scratch.Evaluator
open Scratch.Runtime.Test
open Xunit

type private K = Scratch.KeyCode


[<Fact>]
let keyCodeTest() =
    let k = SNumber >> Scratch.KeyCode.getKeyCode

    k nan =? K.N // "NaN"[0]

    k -infinity =? K.Minus // "-Infinity"[0]
    k -999e100 =? K.Minus // "-9.99e+102"[0]
    k -1. =? K.Minus // "-1"[0]

    k 0. =? K.Key0 // "0"[0]
    k 5.12334567 =? K.Key5 // "5.12334567"[0]
    k 9. =? K.Key9 // "9"[0]
    k 10. =? K.Key1 // "10"[0]


    k 32. =? K.Space
    k 32.5 =? K.Key3 // "32.5"[0]
    k 37. =? K.Left // '%'
    k 38. =? K.Up // '&'
    k 39. =? K.Right // '\''
    k 40. =? K.Down // '('


    k 48. =? K.Key0
    k 48.1 =? K.Key0
    k 48.5 =? K.Key0
    k 57. =? K.Key9

    k 58. =? K.Colon
    k 58.5 =? K.Colon
    k 64. =? K.At
    k 64.5 =? K.At

    k 65. =? K.A
    k 65.5 =? K.A
    k 89.9 =? K.Y
    k 90. =? K.Z

    k 90.1 =? K.Key9 // "90.1"[0]

    k 999999.123456e123 =? K.Key9 // "9.999991235e+128"[0]
    k infinity =? K.I // 'Infinity'[0]

    let k = SString >> KeyCode.getKeyCode
    k "" =? K.None
    k "space" =? K.Space
    k "Space" =? K.S
    k "SPACE" =? K.S
    k "left arrow" =? K.Left
    k "up arrow" =? K.Up
    k "right arrow" =? K.Right
    k "down arrow" =? K.Down

    k "a" =? K.A
    k "A" =? K.A
    k " A" =? K.Space
    k "a " =? K.A
    k "abc" =? K.A

    k "z" =? K.Z
    k "Z" =? K.Z
    k "@" =? K.At

    k "η" =? enum (int 'Η')
    k "文" =? enum (int '文')

    let k = SBool >> KeyCode.getKeyCode
    k true =? K.T // "true"[0]
    k false =? K.F // "false"[0]

module A = Expression
module A = Expressions

let makeSpriteInStage whenGreenFlagBody =
    let sprite =
        { SpriteData.defaultValue with
            scripts = [
                A.whenGreenFlag () whenGreenFlagBody
            ]
        }
    let stage = StageData.defaultValue
    let stage =
        { stage with
            ObjectDataExtension =
            { stage.ObjectDataExtension with
                children = [Choice2Of3 sprite]
            }
        }
    stage

[<Fact>]
let sayAndThinkTest() =
    let stage =
        makeSpriteInStage [
            ComplexExpression((), O.``say:``, [A.eString () "HELLO!"])
        ]
    let state = evaluateStage id stage
    let data = (IArray.item 0 state.originalSprites).sprite.Value.spriteDrawingData
    data.sayKind =? Say
    data.sayText =? "HELLO!"

    let stage =
        makeSpriteInStage [
            ComplexExpression((), O.``think:``, [A.eString () "HMM..."])
        ]
    let state = evaluateStage id stage
    let data = (IArray.item 0 state.originalSprites).sprite.Value.spriteDrawingData
    data.sayKind =? Think
    data.sayText =? "HMM..."

[<Fact>]
let emptyListSetLastTest() =
    let stage =
        makeSpriteInStage [
            A.``setLine:ofList:to:`` () "output" (A.eString () "last") (A.eString () "AAA")
        ]
    let stage = {
        stage with
            lists = [
                {
                    state = ()
                    listName = "output"
                    contents' = IArray.empty
                    isPersistent = NoPersistent
                    x = 0.
                    y = 0.
                    width = 0.
                    height = 0.
                    visible = Hidden
                }
            ]
    }
    let state = evaluateStage id stage
    state.stage.lists["output"].Count =? 0

    throws <| lazy evaluateStage (fun c -> { c with useRangeCheck = true }) stage

[<Fact>]
let plusParseStringTest() =
    let s = A.eString ()
    let n = A.eNumber ()
    let zero = n 0.
    let false' = A.``=`` () zero (n 1.)
    let nan = A.``/`` () zero zero
    let (+) = A.``+`` ()
    let (^) = A.``concatenate:with:`` ()
    let out x = A.``append:toList:`` () "output" x

    let stage = {
        StageData.defaultValue with
            lists = [ListData.make () "output" []]
            scripts = [
                A.whenGreenFlag () [
                    out (nan + nan)
                    out ((s"-INF" ^ s"INITY") + zero)
                    out ((s"NA" ^ s"N") + zero)
                    out ((s"Na" ^ s"N") + zero)
                    out (false' + zero)
                ]
            ]
    }
    stage
    |> evaluateStage id
    |> (fun s -> s.stage.lists["output"] |> Seq.map SValue.toString |> Seq.toList)
    =? ["0"; "0"; "0"; "0"; "0"]

[<Fact>]
let footerStatementValidationFailureTest() =
    let stage = {
        StageData.defaultValue with
            scripts = [
                A.whenGreenFlag "A" [
                    A.doForever "B" "" []
                    A.show "Show"
                ]
            ]
    }
    throws (lazy evaluateStage id stage)

[<Fact>]
let footerStatementValidationFailurePropertyTest() = quickcheck <| fun (FooterStatement footerStatement) ->
    let stage = {
        StageData.defaultValue with
            scripts = [
                A.whenGreenFlag () [
                    footerStatement
                    A.show ()
                ]
            ]
    }
    throws (lazy evaluateStage id stage)

[<Fact>]
let procedureParameterCountMismatchFailureTest() =
    let stage = {
        StageData.defaultValue with
            scripts = [
                ScriptData.procedure () "f" [ParameterDefinition((), "p1", SNumber 1.)] Atomic []
            ]
    }
    throws (lazy evaluateStage id stage)

[<Fact>]
let procedureParameterCountMismatchFailure2Test() =
    let stage = {
        StageData.defaultValue with
            scripts = [
                ScriptData.procedure () "f %n" [ParameterDefinition((), "p1", SNumber 1.); ParameterDefinition((), "p2", SNumber 1.)] Atomic []
            ]
    }
    throws (lazy evaluateStage id stage)

[<Fact>]
let checkListMaxCountLimitTest() =
    let stage = {
        StageData.defaultValue with
            lists = [
                ListData.make () "list" []
            ]
            scripts = [
                A.whenGreenFlag () [
                    A.doRepeat () (A.eNumber () 11.) () [
                        A.``append:toList:`` () "list" (A.eString () "abc")
                    ]
                ]
            ]
    }
    throws (lazy evaluateStage (fun c -> { c with useLengthCheck = true; listMaxLength = Some 10 }) stage)

[<Fact>]
let appendToListMaxLengthLimitTest() =
    let stage = {
        StageData.defaultValue with
            lists = [
                ListData.make () "list" []
            ]
            scripts = [
                A.whenGreenFlag () [
                    A.``append:toList:`` () "list" (A.eString () "A")
                    A.``append:toList:`` () "list" (A.eString () "B")
                    A.``append:toList:`` () "list" (A.eString () "C")
                    A.``append:toList:`` () "list" (A.eString () "D")
                ]
            ]
    }
    let state =
        stage
        |> evaluateStage (fun c -> { c with listMaxLength = Some 3 })

    state.stage.lists["list"] |> Seq.map SValue.toString |> Seq.toList
    =? ["A"; "B"; "C"]

[<Fact>]
let insertToListMaxLengthLimitTest() =
    let stage = {
        StageData.defaultValue with
            lists = [
                ListData.make () "list" []
            ]
            scripts = [
                A.whenGreenFlag () [
                    A.``append:toList:`` () "list" (A.eString () "A")
                    A.``append:toList:`` () "list" (A.eString () "B")
                    A.``append:toList:`` () "list" (A.eString () "C")
                    A.``insert:at:ofList:`` () "list" (A.eNumber () 2.) (A.eString () "X")
                ]
            ]
    }
    let state =
        stage
        |> evaluateStage (fun c -> { c with listMaxLength = Some 3 })

    state.stage.lists["list"] |> Seq.map SValue.toString |> Seq.toList
    =? ["A"; "X"; "B"]

[<Fact>]
let cloudListValidationFailureTest() =
    let stage = {
        StageData.defaultValue with
            lists = [
                { ListData.make () "list" [] with isPersistent = Persistent }
            ]
    }
    throws (lazy evaluateStage id stage)

[<Fact>]
let cloudVariableCountValidationFailureTest() =
    let stage = {
        StageData.defaultValue with
            variables = [
                for i in 1..11 do
                    { VariableData.make () (sprintf "☁ x%d" i) SType.S with isPersistent = Persistent }
            ]
    }
    throws (lazy evaluateStage id stage)

[<Fact>]
let cloudVariableInSpriteValidationFailureTest() =
    let stage = {
        StageData.defaultValue with
            ObjectDataExtension = {
                StageData.defaultValue.ObjectDataExtension with
                    children = [
                        Choice2Of3 {
                            SpriteData.defaultValue with
                                variables = [
                                    { VariableData.make () "☁ x" SType.S with isPersistent = Persistent }
                                ]
                        }
                    ]
            }
    }
    throws (lazy evaluateStage id stage)

[<Fact>]
let cloudVariableSetStringFailureTest() =
    let stage = {
        StageData.defaultValue with
            variables = [
                { VariableData.make () "☁ x" SType.S with isPersistent = Persistent }
            ]
            scripts = [
                A.whenGreenFlag () [
                    A.``setVar:to:`` () "☁ x" (A.eString () "abc")
                ]
            ]
    }
    throws (lazy evaluateStage (fun c -> { c with useCloudValueCheck = true }) stage)

[<Fact>]
let changeCloud() =
    let stage = {
        StageData.defaultValue with
            lists = [
                ListData.make () "list" []
            ]
            variables = [
                { VariableData.make () "☁ x" SType.N with isPersistent = Persistent }
            ]
            scripts = [
                A.whenGreenFlag () [
                    A.doRepeat () (A.eNumber () 3.) () [
                        A.``append:toList:`` () "list" (A.readVariable () "☁ x")
                    ]
                ]
            ]
    }
    let mutable map = Map.empty
    let incrementCloud = Cloud.poly {
        new ICloud with
            override _.Get n =
                let v =
                    match Map.tryFind n map with
                    | ValueSome v -> SNumber <| SValue.toNumber v + 1.
                    | _ -> SValue.sZero

                map <- Map.add n v map
                v

            override _.Set(n, x) = map <- Map.add n x map
    }
    let state =
        stage
        |> evaluateStage (EvaluateConfig.withCloud incrementCloud)

    state.stage.lists["list"] |> Seq.map SValue.toString |> Seq.toList
    =? ["0"; "1"; "2"]
