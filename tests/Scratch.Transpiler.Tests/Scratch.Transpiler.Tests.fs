module Scratch.Transpiler.Tests
open Xunit
open FSharp.Quotations
open Scratch
open Scratch.MemoryModel
open Scratch.MemoryModel.Operators
open Scratch.MemoryModel.TypeSpec
open Scratch.Primitives
open Scratch.Primitives.Document.Constructors
open Scratch.Primitives.Document.Operators
open Scratch.Reflection
open Scratch.Reflection.Member
open Scratch.Ast
open Scratch.Ast.Transformers
open Scratch.Evaluator
open Scratch.Executor
open Scratch.Operators
open Scratch.Transpiler
open Pretty
open Format
open Format.FormatParser
module A = Scratch.Ast.Expressions
module P = Scratch.Ast.ParameterDefinition
module E = Quotations.Patterns
module E = Quotations.DerivedPatterns

type T = | T

let generator = tightrope
let show x = { value = x; show = pretty }
let (=@?) l r = show l =? show r

[<Fact>]
let errorSourceTextTest() =
    buildSourceTextCore { line = 1; column = 1 } { line = 1; column = 3 } ["line1";"line2"] |> Seq.toList =? [
        "   1 | line1"
        "     |  ^^"
        "   2 | line2"
    ]
    buildSourceTextCore { line = 3; column = 1 } { line = 5; column = 3 } ["line1";"line2";"line3";"line4";"line5";"line6";"line7"] |> Seq.toList =? [
        "   2 | line2"
        "   3 | line3"
        "     |  ^^^^"
        "   4 | line4"
        "     | ^^^^^"
        "   5 | line5"
        "     | ^^^"
        "   6 | line6"
    ]

[<Fact>]
let documentTest() =
    let d =
        group (text "let" ++ nest (ns ++ text "name" ++ ns ++ text "=")) ++
        nest (
            ns ++
            group (text "[" ++
                nest (ns ++ group (text "a" ++ nest (ns ++ text "+" ++ ns ++ text "b"))) ++
                nest (nsc ++ group (text "x" ++ nest (ns ++ text "+" ++ ns ++ text "y"))) ++ nsc ++
                text "]"
            )
        )

    let c = { Document.Config.defaultConfig with indent = "•"; newLine = "⏎" }
    let renderWith = Document.renderWith
    d |> renderWith { c with maxWidth = 0 } =? "let⏎•name⏎•=⏎•[⏎••a⏎•••+⏎•••b⏎••x⏎•••+⏎•••y⏎•]"
    d |> renderWith { c with maxWidth = 10 } =? "let name =⏎•[⏎••a + b⏎••x + y⏎•]"
    d |> renderWith { c with maxWidth = 20 } =? "let name =⏎•[ a + b; x + y; ]"
    d |> renderWith { c with maxWidth = 30 } =? "let name = [ a + b; x + y; ]"


    let d = text "[" ++ nest (ne ++ group (text "x1" ++ nsc ++ text "x2")) ++ nsc ++ text "]"
    d |> renderWith { c with maxWidth = 0 } =? "[⏎•x1⏎•x2⏎]"
    d |> renderWith { c with maxWidth = 8 } =? "[⏎•x1; x2⏎]"
    d |> renderWith { c with maxWidth = 120 } =? "[x1; x2; ]"

[<Fact>]
let parseOperatorNameTest() =
    let parseOperatorName = Expr.parseOperatorName
    parseOperatorName "Op_" =? ValueNone
    parseOperatorName "op_" =? ValueSome ""
    parseOperatorName "op_Addition" =? ValueSome "(+)"
    parseOperatorName "op_Plus" =? ValueSome "(+)"
    parseOperatorName "op_PlusBang" =? ValueSome "(+!)"
    parseOperatorName "op_UnaryPlus" =? ValueSome "(~+)"
    parseOperatorName "op_TwiddlePlus" =? ValueSome "(~+)"
    parseOperatorName "op_PlusX" =? ValueNone
    parseOperatorName "op_XPlus" =? ValueNone

[<Fact>]
let specificNewUnionCaseTest() =
    match <@ None: int option @> with
    | SpecificNewUnionCase <@ Some @> _ -> failwith ""
    | SpecificNewUnionCase <@ None @> ([t], []) -> t =? typeof<int>
    | _ -> failwith ""

    match <@ Some 10 @> with
    | SpecificNewUnionCase <@ None @> _ -> failwith ""
    | SpecificNewUnionCase <@ Some @> ([t], [E.Int32 10]) -> t =? typeof<int>
    | _ -> failwith ""
    
    match <@ Error "e": Result<int, string> @> with
    | SpecificNewUnionCase <@ Ok @> _ -> failwith ""
    | SpecificNewUnionCase <@ Error @> ([t1; t2], [E.String "e"]) -> t1 =? typeof<int>; t2 =? typeof<string>
    | _ -> failwith ""


[<Struct>]
type X =
    | C1 of f11: int * f12: int
    | C2 of f21: int * f22: int

[<Fact>]
let specificUnionCaseFieldGetTest() =
    let propertyGet =
        <@ function C2(_,x) -> Some x | _ -> None @>
        |> Expr.tryPick (function E.PropertyGet _ as e -> Some e | _ -> None)
        |> Option.get

    match propertyGet with
    | SpecificUnionCaseFieldGet <@ function C1(x,_) -> Some x | _ -> None @> ([], _)
    | SpecificUnionCaseFieldGet <@ function C1(_,x) -> Some x | _ -> None @> ([], _)
    | SpecificUnionCaseFieldGet <@ function C2(x,_) -> Some x | _ -> None @> ([], _) -> failwith ""
    | SpecificUnionCaseFieldGet <@ function C2(_,x) -> Some x | _ -> None @> ([], _) -> ()
    | _ -> failwith ""


[<Struct>]
type StructA = {
    a1: S Vector Reference
    a2: N
}

[<Fact>]
let memberOffsetTest() =
    Field.defineRecordField <@ fun x -> x.a1 @> =? Field 0
    Field.defineRecordField <@ fun x -> x.a2 @> =? Field 1

let transpile = transpileStage
let defaultData = StageData.defaultValue
let evaluate = evaluateStage


[<Fact>]
let typeSizeTest() =
    defineTypeSize<StructA> =? Size.unsafeOfNumber<StructA> 2

let valueLayout t =
    match valueLayout t with
    | None -> failwithf "valueLayout undefined: %A" t
    | Some l ->

    l
    |> List.map (fun (UnderlyingTypeSpec(t, p, k)) ->
        let p = withMemberPath Main.defaultConfig "" p
        let t = match t with Any -> "Any" | Typed SType.B -> "B" | Typed SType.N -> "N" | Typed SType.S -> "S"
        let k = match k with Kind.Primitive -> "#" | Kind.Collectable -> "*"
        sprintf "%s:%s::%s" p t k
    )

[<Struct>]
type StructUnit = | StructUnit
type ClassUnit = | ClassUnit
[<Struct>]
type StructEnum = | StructEnum1 | StructEnum2
type ClassEnum = | ClassEnum1 | ClassEnum2
[<Struct>]
type StructOption = | StructNone | StructSome of someValue1: int * someValue2: string
type ClassOption = | ClassNone | ClassSome of someValue1: int * someValue2: string

[<Struct>]
type IWordConstrainedStructRecord<'T when 'T :> IWord> = {
    f1: int
    f2: IWord
    f3: 'T
}

type ClassRecordLike<'T1,'T2> = ClassRecordLike of 'T1 * 'T2
[<Struct>]
type StructRecordLike<'T1,'T2> = StructRecordLike of 'T1 * 'T2

type ClassOptionLike<'T> =
    | ClassNone
    | ClassSome of 'T

[<Struct>]
type StructOptionLike<'T> =
    | StructNone
    | StructSome of 'T

type ClassComplex<'T1,'T2> =
    | Complex1
    | Complex2
    | Complex3 of 'T1
    | Complex4 of 'T2

[<Struct>]
type StructComplex<'T1,'T2> =
    | Complex1
    | Complex2
    | Complex3 of t1: 'T1
    | Complex4 of t2: 'T2

[<Fact>]
let underlyingLayoutTest() =
    valueLayout typeof<unit> =? []
    valueLayout typeof<double> =? [":N::#"]
    valueLayout typeof<S> =? [":S::#"]

    valueLayout typeof<StructUnit> =? []
    valueLayout typeof<ClassUnit> =? []
    valueLayout typeof<StructEnum> =? [":S::#"]
    valueLayout typeof<ClassEnum> =? [":S::#"]
    valueLayout typeof<ClassRecordLike<string,int>> =? [":N::*"]
    valueLayout typeof<ClassRecordLike<unit,ClassUnit>> =? []
    valueLayout typeof<StructRecordLike<string,int>> =? [
        ".0:S::#"
        ".1:N::#"
    ]
    valueLayout typeof<StructRecordLike<StructUnit, ClassUnit>> =? []
    valueLayout typeof<ClassOptionLike<int>> =? [":N::*"]
    valueLayout typeof<StructOptionLike<int>> =? [
        ".tag:S::#"
        ".0:N::#"
    ]
    valueLayout typeof<ClassComplex<int,string>> =? [":N::*"]
    valueLayout typeof<StructComplex<int,string>> =? [
        ".tag:S::#"
        ".0:Any::#"
    ]
    valueLayout typeof<StructComplex<int,ClassOptionLike<int>>> =? [
        ".tag:S::#"
        ".0:N::*"
        ".0:N::#"
    ]

    valueLayout typedefof<IWordConstrainedStructRecord<_>> =? [
        ".f1:N::#"
        ".f2:Any::#"
        ".f3:Any::#"
    ]
    valueLayout typeof<struct(struct(int * IWord * ClassRecordLike<int,string>) * unit * string)> =? [
        ".1.1:N::#"
        ".1.2:Any::#"
        ".1.3:N::*"
        ".3:S::#"
    ]
    valueLayout typeof<struct(hunit * hunit)> =? []
    valueLayout typeof<hunit * hunit> =? []
    valueLayout typeof<hunit * ClassRecordLike<hunit, hunit>> =? []

module ClassTests =
    type Base(x) =
        member _.X = x
        member _.Out x = printfn "%s" x

    [<ReflectedDefinition>]
    type ClassSimple(_x: string) = class end

    [<ReflectedDefinition>]
    type ClassDo(x: string) =
        do stdout.Write x

    [<ReflectedDefinition>]
    type ClassInherit(x) =
        inherit Base(x)

    [<ReflectedDefinition>]
    type ClassSelf(_x) as _self = class end

    [<ReflectedDefinition>]
    type ClassSelfCall(x) as self =
        do self.Out x
        member _.Out x = printfn "%s" x

    [<ReflectedDefinition>]
    type ClassInheritSelfCall(x) as self =
        inherit Base(x)
        do self.Out x

    [<ReflectedDefinition>]
    type ClassFieldInit(x: string) =
        let mutable value1 = x
        member _.Value1 = value1

    [<ReflectedDefinition>]
    type Class(x) as self =
        inherit Base(x + "A")
        let mutable value1 = self.X
        do self.Out()
        let mutable value2 = self.X
        member _.Out() = ()

    let classLambdas =
        [
            <@@ ClassSimple @@>
            <@@ ClassDo @@>
            <@@ ClassInherit @@>
            <@@ ClassSelf @@>
            <@@ ClassSelfCall @@>
            <@@ ClassInheritSelfCall @@>
            <@@ ClassFieldInit @@>
            <@@ Class @@>
        ]
        |> Seq.map (fun e -> e.Type.GetGenericArguments().[1].Name, e)
        |> Map.ofSeq

[<Fact>]
let classWithReflectedDefinitionTest() =
    for kv in ClassTests.classLambdas do
        match kv.Value with
        | Lambdas(_, E.NewObject(Scratch.Transpiler.Class.ClassWithReflectedDefinition r, _)) ->
            match r with
            | Ok(c, _) ->
                let this = c.this
                let parameters = c.constructorParameters
                let baseArgs = c.baseConstructorArguments

                let vars = Set.singleton this + Set.ofSeq parameters
                let isKnownFreeVars (e: Expr) = e.GetFreeVars() |> Seq.forall (fun v -> Set.contains v vars)

                List.distinct parameters =? parameters
                parameters |> Seq.map (fun p -> p.Type) |> Seq.toArray |> this.Type.GetConstructor <>? null

                for e in baseArgs do isKnownFreeVars e =? true
                baseArgs |> Seq.map (fun p -> p.Type) |> Seq.toArray |> this.Type.BaseType.GetConstructor <>? null

                for e in c.tops.afterInit do isKnownFreeVars e =? true
                for e in c.tops.beforeInit do isKnownFreeVars e =? true

            | Error e -> failwithf "%A" e
        | e -> failwithf "%s: %A" kv.Key e

[<Fact>]
let emptyScriptTest() =
    <@ () @>
    |> transpile
    =@? defaultData

[<Fact>]
let inlineProcedureTest() =
    <@
        let f x y = x + y
        ()
    @>
    |> transpile
    |> StageData.map ignore
    =@? { defaultData with
            variables =
                [
                VariableData.make () "f.result" SType.N
                ]
            scripts =
                [
                ScriptData.procedure () "f %n %n" [P.make () "x" SType.N; P.make () "y" SType.N] Atomic [
                    A.``setVar:to:`` () "f.result" (A.``+`` () (A.getParam () "x") (A.getParam () "y"))
                ]
                ]
        }

[<ReflectedDefinition; Export>]
let f x y = x + y

[<Fact>]
let reflectedDefinitonProcedureTest() =
    <@ export f @>
    |> transpile
    |> StageData.map ignore
    =@? { defaultData with
            variables =
                [
                VariableData.make () "f.result" SType.N
                ]
            scripts =
                [
                ScriptData.procedure () "f %n %n" [P.make () "x" SType.N; P.make () "y" SType.N] Atomic [
                    A.``setVar:to:`` () "f.result" (A.``+`` () (A.getParam () "x") (A.getParam () "y"))
                ]
                ]
    }

[<Fact>]
let inlineProcedureTest2() =
    <@
        let p1 x y = x + y
        let p2 x y = p1 x y + y
        ()
    @>
    |> transpileStageWith (fun c -> { c with plugin = { c.plugin with postTransform = Scratch.Ast.Transformers.Plugins.empty } })
    |> StageData.map ignore
    =@? {
        defaultData with
            variables =
                [
                VariableData.make () "p1.result" SType.N
                VariableData.make () "p2.result" SType.N
                VariableData.make () "p2._" SType.N
                ]
            scripts =
                [
                ScriptData.procedure () "p1 %n %n" [P.make () "x" SType.N; P.make () "y" SType.N] Atomic [
                    A.``setVar:to:`` () "p1.result" (A.``+`` () (A.getParam () "x") (A.getParam () "y"))
                ]
                ScriptData.procedure () "p2 %n %n" [P.make () "x" SType.N; P.make () "y" SType.N] Atomic [
                    A.call () "p1 %n %n" [A.getParam () "x"; A.getParam () "y"]
                    A.``setVar:to:`` () "p2._" (A.readVariable () "p1.result")
                    A.``setVar:to:`` () "p2.result" (A.``+`` () (A.readVariable () "p2._") (A.getParam () "y"))
                ]
                ]
    }

let startAsStdSpriteWith' withTranspileConfig withEvaluateConfig withExecuteConfig e =
    let getOutput { Evaluator.stage = stage } =
        match Map.tryFind "output" stage.lists with
        | ValueNone -> []
        | ValueSome output ->
            output
            |> Seq.map SValue.toString
            |> Seq.toList

    let data =
        e
        |> transpileStageWith withTranspileConfig

    let output1 =
        data
        |> evaluate (fun c -> withEvaluateConfig { c with showState = showLoc })
        |> getOutput

    let output2 =
        let image =
            data
            |> Executor.Compiler.compileToImage

        let state =
            image
            |> Executor.Executor.runImageWith withExecuteConfig

        let stage = state.mainStage.stageState
        stage.entityImage.listVariables
        |> IArray.toSeqCopiable
        |> Seq.tryFindIndex (fun v -> "output" = v.listName)
        |> function
            | None -> []
            | Some index ->
                stage.listVariables.[index]
                |> Seq.map Value.toString
                |> Seq.toList

    output2 =? output1
    output1

let startAsStdSpriteWith withTranspileConfig withEvaluateConfig e = startAsStdSpriteWith' withTranspileConfig withEvaluateConfig id e
let startAsStdSprite e = startAsStdSpriteWith id id e

[<Fact>]
let inlineProcedureCallTest() =
    <@
        let output = defineList []
        let p1 x y = x + y
        let p2 () = p1 (p1 1 2 + p1 3 4) 5
        
        SList.push output (p2 ())
    @>
    |> startAsStdSprite
    =? ["15"]

[<Fact>]
let inlineWhenGreenFlagTest() =
    <@
        let output = defineList []
        let p1 x y = x + y
        let x1 = p1 1 2
        SList.push output x1
        let p2 x y = x + y + y
        let x2 = p2 1 2
        SList.push output x2
    @>
    |> startAsStdSprite
    =? ["3";"5"]

[<Fact>]
let operatorsTest() =
    <@
        let output = defineList []
        SList.push output (1 <= 2)
        SList.push output (1 <= 1)
        SList.push output (2 <= 1)

        SList.push output (1 >= 2)
        SList.push output (1 >= 1)
        SList.push output (2 >= 1)

        SList.push output (1 <> 1)
        SList.push output (2 <> 1)
    @>
    |> startAsStdSprite
    =? [
        "true"; "true"; "false"
        "false"; "true"; "true"

        "false"; "true"
    ]

[<ReflectedDefinition>]
let rec (.^) x count =
    if count = 0 then 1
    else (.^) x (count - 1) * x

[<Fact>]
let selfCallProcedureTest() =
    <@
        let output = defineList []
        SList.push output (2 .^ 4)
    @>
    |> startAsStdSprite
    =? ["16"]

[<Fact>]
let callInSequenceTest() =
    <@
        let output = defineList []
        let g x = SList.push output x
        let f1() = g "A"; "B"
        SList.push output (f1())

        let f2() = SList.push output "C"; "D"
        SList.push output (f2())
    @>
    |> startAsStdSprite
    =? ["A";"B";"C";"D"]

[<Fact>]
let pipeTest() =
    <@
        let output = defineList []
        let f x = x + 1

        SList.push output (f <| 2)
        SList.push output (10 |> f)

        let f2 x y z = x - y - z
        SList.push output (f2 1 2 <| 3)
        SList.push output (30 |> f2 10 20)
    @>
    |> startAsStdSprite
    =? ["3";"11";"-4";"-40"]

[<ReflectedDefinition>]
let rec isOdd x =
    if x = 0 then false
    else isEven (x - 1)

and [<ReflectedDefinition>] isEven x =
    if x = 0 then true
    else isOdd (x - 1)

[<Fact>]
let mutualRecursionTest() =
    <@
        let output = defineList []
        SList.push output <| SString "isEven 4 ="
        SList.push output <| SBool (isEven 4)
        SList.push output <| SString "isEven 5 ="
        SList.push output <| SBool (isEven 5)

        SList.push output <| SString "isOdd 5 ="
        SList.push output <| SBool (isOdd 5)
        SList.push output <| SString "isOdd 4 ="
        SList.push output <| SBool (isOdd 4)
    @>
    |> startAsStdSprite
    =? [
        "isEven 4 =";"true"
        "isEven 5 =";"false"
        "isOdd 5 =";"true"
        "isOdd 4 =";"false"
    ]


[<Fact>]
let selfRecursionTest() =
    <@
        let output = defineList []

        let rec fib n = if n < 2 then n else fib (n - 2) + fib (n - 1)

        let rec fib' n = if n < 2 then n else fib'' (n - 2) + fib'' (n - 1)
        and fib'' n = if n < 2 then n else fib' (n - 2) + fib' (n - 1)

        SList.push output <| "fib 10 = " + string (fib 10)
        SList.push output <| "fib' 10 = " + string (fib' 10)
        SList.push output <| "fib'' 10 = " + string (fib'' 10)
    @>
    |> startAsStdSprite
    =? [
        "fib 10 = 55"
        "fib' 10 = 55"
        "fib'' 10 = 55"
    ]

[<ReflectedDefinition; Export>]
let useWord (x: IWord) (y: #IWord) = struct(struct(x, y), struct(x, y))

[<Fact>]
let parameterTest() =
    <@
        export useWord
    @>
    |> transpile
    |> StageData.map ignore
    =@? {
        defaultData with
            variables =
                [
                VariableData.make () "useWord@V.result.1.1" SType.S
                VariableData.make () "useWord@V.result.1.2" SType.S
                VariableData.make () "useWord@V.result.2.1" SType.S
                VariableData.make () "useWord@V.result.2.2" SType.S
                ]
            scripts =
                [
                ScriptData.procedure () "useWord@V %s %s" [P.make () "x" SType.S; P.make () "y" SType.S] Atomic [
                    A.``setVar:to:`` () "useWord@V.result.1.1" (A.getParam () "x")
                    A.``setVar:to:`` () "useWord@V.result.1.2" (A.getParam () "y")
                    A.``setVar:to:`` () "useWord@V.result.2.1" (A.getParam () "x")
                    A.``setVar:to:`` () "useWord@V.result.2.2" (A.getParam () "y")
                ]
                ]
    }

[<Fact>]
let useTupleTest() =
    <@
        let output = defineList []
        let divmod struct(x, y) = struct(x / y, x % y)

        let pointAdd3 struct(struct(x1, y1), struct(x2, y2), struct(x3, y3)) = struct(x1 + x2 + x3, y1 + y2 + y3)

        let struct(x, y) = divmod struct(10., 4.)
        SList.push output <| "divmod (10, 4) = (" + string x + ", " + string y + ")"

        let struct(px, py) = pointAdd3 struct(struct(1, 2), struct(10, 20), struct(100, 200))
        SList.push output <| "pointAdd3 ((1, 2), (10, 20), (100, 200)) = (" + string px + ", " + string py + ")"
    @>
    |> startAsStdSprite
    =? [
        "divmod (10, 4) = (2.5, 2)"
        "pointAdd3 ((1, 2), (10, 20), (100, 200)) = (111, 222)"
    ]

[<Fact>]
let memoryTest() =
    <@
        let output = defineList []

        Memory.clear()
        SList.push output <| S "memoryEnd = " + toS (Memory.memoryEnd())
        Memory.push(N 0.)
        Memory.push(S "a")
        Memory.push(Scratch.Operators.(~%) true: B)
        SList.push output <| S "memoryEnd = " + toS (Memory.memoryEnd())
    @>
    |> startAsStdSprite
    =? ["memoryEnd = 1"; "memoryEnd = 4"]

[<Fact>]
let repeatTest() =
    <@
        let output = defineList []

        let f() =
            let mutable n = 0
            repeat (let x = 2 in x + x) <| fun _ ->
                SList.push output n
                n <- n + 1
        f()
    @>
    |> startAsStdSprite
    =? ["0"; "1"; "2"; "3"]

[<Fact>]
let repeatUntilTest() =
    <@
        let output = defineList []
        let pred n =
            SList.push output "P"
            SList.push output <| string n
            1 < n

        let f() =
            let mutable n = 0
            repeatUntil (fun _ -> pred n) <| fun _ ->
                SList.push output "R"
                SList.push output <| string n
                n <- n + 1
        f()
    @>
    |> startAsStdSprite
    =? ["P"; "0"; "R"; "0"; "P"; "1"; "R"; "1"; "P"; "2"]

[<Fact>]
let forIntegerRangeLoopTest() =
    <@
        let f() =
            for i in (outLine "A"; -1)..(outLine "B"; 1) do
                outLine(string i)
        f()
    @>
    |> startAsStdSprite
    =? ["A"; "B"; "-1"; "0"; "1"; ""]

[<Fact>]
let forIntegerRangeLoopNoLoopTest() =
    <@
        let f() =
            for i in (outLine "A"; 1)..(outLine "B"; -1) do
                outLine(string i)
        f()
    @>
    |> startAsStdSprite
    =? ["A"; "B"; ""]

[<Fact>]
let forIntegerRangeLoopNoVarTest() =
    <@
        let f() =
            let mutable i = -1
            for _ in (outLine "A"; -1)..(outLine "B"; 1) do
                outLine(string i)
                i <- i + 1
        f()
    @>
    |> startAsStdSprite
    =? ["A"; "B"; "-1"; "0"; "1"; ""]

[<Fact>]
let forIntegerRangeLoopNoVarNoLoopTest() =
    <@
        let f() =
            let mutable i = -1
            for _ in (outLine "A"; 1)..(outLine "B"; -1) do
                outLine(string i)
                i <- i + 1
        f()
    @>
    |> startAsStdSprite
    =? ["A"; "B"; ""]

[<Struct>]
type Point = {
    x: N
    y: N
}

[<Fact>]
let memberAndTypeTest() =
    <@
        let output = defineList []

        let Point_x = Field.defineRecordField <@ fun x -> x.x @>
        let Point_y = Field.defineRecordField <@ fun x -> x.y @>
        let Point_size = defineTypeSize<Point>

        SList.push output <| Field.toNumber Point_x
        SList.push output <| Field.toNumber Point_y
        SList.push output <| Size.toNumber Point_size
    @>
    |> startAsStdSprite
    =? ["0"; "1"; "2"]

[<ReflectedDefinition>]
let externalX = 1 + 2
[<ReflectedDefinition>]
let externalY = externalX + 1
[<ReflectedDefinition>]
let externalF() = externalX + externalY

[<Fact>]
let externalPropertyGetTest() =
    <@
        let output = defineList []

        SList.push output externalX
        SList.push output externalY
        SList.push output <| externalF()
    @>
    |> startAsStdSprite
    =? ["3"; "4"; "7"]

[<ReflectedDefinition>]
let mutable externalMutableX = 10

[<Fact>]
let externalPropertySetTest() =
    <@
        externalMutableX <- 20
    @>
    |> transpileStageWith (fun c -> { c with plugin = { c.plugin with postTransform = Plugins.empty } })
    |> evaluate (fun c -> { c with showState = showLoc })
    |> fun s -> s.stage.values.["externalMutableX"].contents
    =? SNumber 20.

[<Fact>]
let pointerTest() =
    <@
        let output = defineList []

        let f x =
            match x with
            | NonNil(_x: N Reference) -> SList.push output "NonNil"
            | Nil -> SList.push output "Nil"

        let g x = f(NonNil x)

        f Nil
    @>
    |> startAsStdSprite
    =? ["Nil"]

module L = Scratch.SList

[<Fact>]
let orElseTest() =
    <@
        let output = defineList []
        let pushBool x = if x then L.push output "true" else L.push output "false"
        let isEven c =
            L.push output "E"
            c % 2 = 0

        let isLT10 c =
            L.push output "L"
            c < 10

        let f1 c = (c % 2 = 0) || c < 10
        pushBool <| f1 1
        pushBool <| f1 2
        pushBool <| f1 10
        pushBool <| f1 11

        L.push output ""
        
        let f2 c = (c % 2 = 0) || isLT10 c
        pushBool <| f2 1
        pushBool <| f2 2
        pushBool <| f2 10
        pushBool <| f2 11

        L.push output ""

        let f3 c = isEven c || c < 10
        pushBool <| f3 1
        pushBool <| f3 2
        pushBool <| f3 10
        pushBool <| f3 11

        L.push output ""

        let f4 c = isEven c || isLT10 c
        pushBool <| f4 1
        pushBool <| f4 2
        pushBool <| f4 10
        pushBool <| f4 11
    @>
    |> startAsStdSprite
    =? [
        "true"; "true"; "true"; "false"
        ""
        "L"; "true"; "true"; "true"; "L"; "false"
        ""
        "E"; "true"; "E"; "true"; "E"; "true"; "E"; "false"
        ""
        "E"; "L"; "true"; "E"; "true"; "E"; "true"; "E"; "L"; "false"
    ]

[<Fact>]
let andAlsoTest() =
    <@
        let output = defineList []
        let pushBool x = if x then L.push output "true" else L.push output "false"
        let isEven c =
            L.push output "E"
            c % 2 = 0

        let isLT10 c =
            L.push output "L"
            c < 10

        let f1 c = (c % 2 = 0) && c < 10
        pushBool <| f1 1
        pushBool <| f1 2
        pushBool <| f1 10
        pushBool <| f1 11

        L.push output ""
        
        let f2 c = (c % 2 = 0) && isLT10 c
        pushBool <| f2 1
        pushBool <| f2 2
        pushBool <| f2 10
        pushBool <| f2 11

        L.push output ""

        let f3 c = isEven c && c < 10
        pushBool <| f3 1
        pushBool <| f3 2
        pushBool <| f3 10
        pushBool <| f3 11

        L.push output ""

        let f4 c = isEven c && isLT10 c
        pushBool <| f4 1
        pushBool <| f4 2
        pushBool <| f4 10
        pushBool <| f4 11
    @>
    |> startAsStdSprite
    =? [
        "false"; "true"; "false"; "false"
        ""
        "false"; "L"; "true"; "L"; "false"; "false"
        ""
        "E"; "false"; "E"; "true"; "E"; "false"; "E"; "false"
        ""
        "E"; "false"; "E"; "L"; "true"; "E"; "L"; "false"; "E"; "false"
    ]

[<Fact>]
let getCharTest() =
    <@
        let output = defineList []
        L.push output <| getChar "" 0

        L.push output <| getChar "abc" 0
        L.push output <| getChar "abc" 1
        L.push output <| getChar "abc" 2
        L.push output <| getChar "abc" 3
        L.push output <| getChar "abc" 4
    @>
    |> startAsStdSprite
    =? [""; ""; "a"; "b"; "c"; ""]

[<Fact>]
let notTest() =
    <@
        let output = defineList []
        L.push output <| not true
        L.push output <| not false
    @>
    |> startAsStdSprite
    =? ["false"; "true"]

[<Fact>]
let combineTest() =
    <@
        let output = defineList []
        L.push output <| combine "ab" "cd"
        L.push output <| combine "" "cd"
        L.push output <| combine "ab" ""
        L.push output <| combine "" ""
    @>
    |> startAsStdSprite
    =? ["abcd"; "cd"; "ab"; ""]

[<ReflectedDefinition>]
let externalList = defineList []

[<Fact>]
let listLengthTest() =
    <@

        L.push output <| string (L.length externalList)
        L.push externalList "a"
        L.push output <| string (L.length externalList)

        let list = defineList []
        L.push output <| string (L.length list)
        L.push list "a"
        L.push output <| string (L.length list)

    @>
    |> startAsStdSprite
    =? ["0"; "1"; "0"; "1"]

[<Fact>]
let listSetTest() =
    <@
        L.push externalList "a"
        L.push output <| string (L.length externalList)
        L.push output <| L.get externalList 1

        let list = defineList []
        L.push list "a"
        L.push output <| string (L.length list)
        L.push output <| L.get list 1
    @>
    |> startAsStdSprite
    =? ["1"; "a"; "1"; "a"]

[<Fact>]
let outputTest() =
    <@
        L.push output "aaa"
    @>
    |> startAsStdSprite
    =? ["aaa"]

[<Fact>]
let nilTest() =
    <@
        let output = defineList []
        L.push output (nil: int Pointer)
    @>
    |> startAsStdSprite
    =? ["0"]

[<Fact>]
let formatParseTest() =
    parseFormat "abc%Xabc" =? Error 4
    parseFormat "abcdef%" =? Error 7

    parseFormat "" =? Ok("", [])
    parseFormat "abc%%" =? Ok("abc%", [])
    parseFormat "aa%Abb%dcc%f" =? Ok("aa", ['A', "bb"; 'd', "cc"; 'f', ""])
    parseFormat "%Add%d%fee" =? Ok("", ['A', "dd"; 'd', ""; 'f', "ee"])

[<Fact>]
let stringInterpolationTest() =
    let failwithfM, _ = findMethod <@ failwithf @>

    match <@ failwithf "" @> with
    | CallWithStringInterpolation(Ok(GenericMethodDefinition m, _, "", [])) -> m =? failwithfM
    | _ -> failwithf ""

    match <@ failwithf "aa%Abb%dcc%f" 10 20 30. @> with
    | CallWithStringInterpolation(Ok(GenericMethodDefinition m, _, "aa", ['A', E.Int32 10, "bb"; 'd', E.Int32 20, "cc"; 'f', E.Double 30., ""])) -> m =? failwithfM
    | _ -> failwithf ""

    match <@ failwithf "%Add%d%fee" 4 5 6. @> with
    | CallWithStringInterpolation(Ok(GenericMethodDefinition m, _, "", ['A', E.Int32 4, "dd"; 'd', E.Int32 5, ""; 'f', E.Double 6., "ee"])) -> m =? failwithfM
    | _ -> failwithf ""

[<Fact>]
let ifFSharpTest() =
    <@
        L.push output "aaa"
        ``#if-fsharp`` (L.push output "bbb")
        L.push output "ccc"

        L.push output (``#if-else-fsharp`` ("ddd") (fun _ -> "eee"))
    @>
    |> startAsStdSprite
    =? ["aaa"; "ccc"; "eee"]

[<Fact>]
let outTest() =
    <@
        outLine ""
        out "aa"; out <| string 10; out "bb"; out <| string 20; out "cc"; outLine <| string 30
    @>
    |> startAsStdSprite
    =? [
        ""
        "aa10bb20cc30"
        ""
    ]

[<Fact>]
let formatCallTest() =
    <@
        printfn ""
        printfn "aa%Abb%dcc%f" 10 20 30.
        printfn "%Add%d%fee" 4 5 6.
        
        printf ""
        printf "aa%Abb%dcc%f" 10 20 30.
        printf " "
        printf "%Add%d%fee" 4 5 6.
    @>
    |> startAsStdSprite
    =? [
        ""
        "aa10bb20cc30"
        "4dd56ee"
        "aa10bb20cc30 4dd56ee"
    ]

[<Fact>]
let formatFailCallTest() =
    <@
        printfn "aaa"
        failwithf "aa%Abb%dcc%f" 10 20 30.
        printfn "bbb"
    @>
    |> startAsStdSprite
    =? [
        "aaa"
        "aa10bb20cc30"
        ""
    ]

[<Fact>]
let failwithCallTest() =
    <@
        printfn "aaa"
        let x: int = failwith "error"
        printfn "bbb"
        outLine <| string x
    @>
    |> startAsStdSprite
    =? [
        "aaa"
        "error"
        ""
    ]

[<Fact>]
let makeStructFailureTest() =
    <@
        let f() =
            struct(
                (outLine "A"; 10),
                (outLine "B"; failwith "Error" : double),
                (outLine "C"; "_")
            )
        let _ = f()
        ()
    @>
    |> startAsStdSprite
    =? ["A"; "B"; "Error"; ""]

[<Fact>]
let ignoreTest() =
    <@
        out "A"
        let _ = (out "B"; 1)
        out "C"
        ignore (out "D"; 1)
        out "E"
    @>
    |> startAsStdSprite
    =? ["ABCDE"]

[<Fact>]
let addressAddTest() =
    <@
        let add (a: Address) x = a + x
        let add' a x = Address.(+)(a, x)
        ()
    @>
    |> startAsStdSprite
    =? []

[<Fact>]
let referenceToAddressTest() =
    <@
        let f (x: int Reference) = Reference.toAddress x
        ()
    @>
    |> startAsStdSprite
    =? []

type U =
    | CaseA
    | CaseB
    | CaseC
    interface IWord with
        override x.Value = enumLikeUnionToV x

[<Fact>]
let enumLikeUnionTest() =
    <@
        let output = defineList []
        L.push output CaseA
        L.push output CaseB
        L.push output CaseC
    @>
    |> startAsStdSprite
    =? ["CaseA"; "CaseB"; "CaseC"]

    <@
        let output = defineList []

        let f = function
            | CaseA -> "A"
            | CaseB -> "B"
            | CaseC -> "C"

        L.push output <| f CaseA
        L.push output <| f CaseB
        L.push output <| f CaseC
    @>
    |> startAsStdSprite
    =? ["A";"B";"C"]

[<ReflectedDefinition>]
let castS (w: IWord) = w :?> S

[<Fact>]
let iwordUnboxTest() =
    <@
        out <| Word.toString (castS (S "abc"))
    @>
    |> startAsStdSprite
    =? ["abc"]

[<Fact>]
let addressOfNumberTest() =
    <@
        let f x = Address.ofNumber x
        ()
    @>
    |> startAsStdSprite
    =? []

[<Fact>]
let referenceToPointerTest() =
    <@
        let f (x: int Reference) = Reference.toPointer x
        ()
    @>
    |> startAsStdSprite
    =? []

[<Fact>]
let doRepeatAndProcTest() =
    <@
        out "A"
        let a s = generator {
            do! repeatAsync 2 {
                out s
                out "a"
            }
        }
        whenGreenFlag {
            do! repeatAsync 2 {
                do! a "B"
                out "B"
            }
        }
        whenGreenFlag {
            do! repeatAsync 3 {
                out "C"
                do! a "C"
            }
        }
        startMainLoop()
    @>
    |> startAsStdSprite
    =? ["ABaCCaBaCaBBaCCaBaCaBCCaCa"]

[<Fact>]
let repeatUntilAsyncTest() =
    <@
        whenGreenFlag {
            let mutable i = 0.
            do! repeatUntilAsync (fun _ -> 2. < i) {
                out (string i)
                i <- i + 1.
            }
        }
        startMainLoop()
    @>
    |> startAsStdSprite
    =? ["012"]

[<Fact>]
let atomicProcedureTest() =
    <@
        out "A"

        let b() = generator {
            do! repeatAsync 2 {
                out "b"
            }
        }

        let a() = atomic {
            do! repeatAsync 2 {
                out "a"
                do! b()
            }
        }

        whenGreenFlag {
            do! repeatAsync 2 {
                out "B"
                do! awaitAtomic(a())
            }
        }
        whenGreenFlag {
            do! repeatAsync 2 {
                out "C"
            }
        }
        startMainLoop()
    @>
    |> startAsStdSprite
    =? ["ABabbabbCBabbabbC"]

[<Fact>]
let recursiveProcTest() =
    <@
        out "A"

        let rec a i s = generator {
            out "a("
            out (string i)
            out ","
            out s
            out ")"
            if 0 < i then
                do! repeatAsync 2 {
                    do! a (i - 1) (s + "a")
                }
        }

        whenGreenFlag {
            out "B"
            do! a 2 "B"
        }
        startMainLoop()
    @>
    |> startAsStdSprite
    =? ["ABa(2,B)a(1,Ba)a(0,Baa)a(0,Baa)a(1,Ba)a(0,Baa)a(0,Baa)"]

[<Fact>]
let broadcastTest() =
    <@
        let mutable emitCount = 0

        whenIReceive "e" {
            out "A"
        }
        whenIReceive "E" {
            out "B"
            do! repeatAsync 2 {
                if emitCount < 3 then
                    emitCount <- emitCount + 1
                    broadcast "e"
                    out "C"
                out "D"
            }
            out "E"
        }
        whenGreenFlag {
            emitCount <- emitCount + 1
            broadcast "e"
            out "F"
        }
        startMainLoop()
    @>
    |> startAsStdSprite

    // scratch: "FABCDABCDABDDE", sulfurous: "FABABABDDE"
    =? ["FABCDABCDABDDDDEEE"]

[<Fact>]
let broadcastAndWaitTest() =
    <@
        let mutable emitCount = 0
        whenIReceive "e" {
            out "A"
        }
        whenIReceive "E" {
            out "B"
            if emitCount < 3 then
                emitCount <- emitCount + 1
                broadcast "e"
                out "C"
        }
        whenGreenFlag {
            emitCount <- emitCount + 1
            do! broadcastAndWait "e"
            out "D"
        }
        startMainLoop()
    @>
    |> startAsStdSprite

    // scratch: "ABCDABCAB", sulfrous: "ABABABD"
    =? ["ABCABCABD"]

[<Fact>]
let broadcastRecursionTest() =
    <@
        out "A"
        let mutable emitCount = 0

        whenIReceive "e" {
            out "e("
            out (string emitCount)
            out ")"
            if emitCount < 3 then
                emitCount <- emitCount + 1
                broadcast "e"
        }
        whenGreenFlag {
            out "B"
            emitCount <- emitCount + 1
            broadcast "e"
        }
        startMainLoop()
    @>
    |> startAsStdSprite
    =? ["ABe(1)e(2)e(3)"]

[<Fact>]
let broadcastMutualRecursionTest() =
    <@
        out "A"
        let mutable emitCount = 0

        whenIReceive "e2" {
            out "e2("
            out (string emitCount)
            out ")"
            broadcast "e1"
        }
        whenIReceive "e1" {
            out "e1("
            out (string emitCount)
            out ")"
            if emitCount < 3 then
                emitCount <- emitCount + 1
                broadcast "e2"
        }
        whenGreenFlag {
            out "B"
            emitCount <- emitCount + 1
            broadcast "e1"
        }
        startMainLoop()
    @>
    |> startAsStdSprite
    =? ["ABe1(1)e2(2)e1(2)e2(3)e1(3)"]

[<Fact>]
let broadcastDoRepeatTest() =
    <@
        let mutable emitCount = 0

        whenIReceive "e" {
            out "A"
            if emitCount < 3 then
                emitCount <- emitCount + 1
                broadcast "e"
                out "B"

            do! repeatAsync 1 { () }
            out "F"
        }
        whenGreenFlag {
            emitCount <- emitCount + 1
            broadcast "e"
        }
        startMainLoop()
    @>
    |> startAsStdSprite

    // scratch: "ABABAF", sulfurous: "AAAF"
    =? ["ABABAFFF"]

[<Fact>]
let noAtomicWaitTest() =
    <@
        out "A"
        whenGreenFlag {
            out "B1"
            do! waitElapsedFrom 0.
            out "B2"
        }
        whenGreenFlag {
            out "C1"
            do! waitElapsedFrom 0.
            out "C2"
        }
        startMainLoop()
    @>
    |> startAsStdSprite
    =? ["AB1C1B2C2"]

[<Fact>]
let atomicWaitTest() =
    <@
        out "A"

        let b() = generator {
            out "b1"
            do! waitElapsedFrom 0.
            out "b2"
        }

        // atomic
        let a() = atomic {
            out "a1"
            do! b()
            out "a2"
        }

        whenGreenFlag {
            out "B1"
            do! awaitAtomic(a())
            out "B2"
        }
        whenGreenFlag {
            out "C1"
            do! waitElapsedFrom 0.
            out "C2"
        }
        startMainLoop()
    @>
    |> startAsStdSprite
    =? ["AB1a1b1C1b2a2B2C2"]

[<Fact>]
let waitUntilTest() =
    <@
        let mutable x = 0
        whenGreenFlag {
            do! waitUntilAsync (fun _ -> 2 < x)
            outLine "end wait"
        }
        whenGreenFlag {
            do! repeatAsync 4 {
                x <- x + 1
                outLine ("change " + string x)
            }
            outLine "end change"
        }
        startMainLoop()
    @>
    |> startAsStdSprite
    =? [
        "change 1"
        "change 2"
        "change 3"
        "end wait"
        "change 4"
        "end change"
        ""
    ]

[<Fact>]
let atomicWaitUntilTest() =
    <@
        let mutable x = 0
        let f() = atomic {
            do! waitUntilAsync (fun _ -> 5 < x)
            out "OK"
        }
        whenGreenFlag {
            do! awaitAtomic(f())
        }
        whenGreenFlag {
            do! repeatAsync 10 {
                x <- x + 1
            }
        }
        startMainLoop()
    @>
    |> startAsStdSprite
    =? ["OK"]

[<Sealed; ReflectedDefinition>]
type CloneTestSprite(options) as self =
    inherit Sprite(options)

    let mutable v = "X"

    do self.WhenCloned {
        v <- v + "C"
        outLine (v + "(" + string self.X + ", " + string self.Y + ")")
    }
    do self.WhenGreenFlag {
        do! repeatAsync 3 {
            self.Clone()
            self.GotoXY(self.X + 10., self.Y + 10.)
            v <- v + "G"
        }
    }

[<Fact>]
let cloneTest() =
    <@
        defineSprite CloneTestSprite
        startMainLoop()
    @>
    |> startAsStdSprite
    =? ["XC(0, 0)"; "XGC(10, 10)"; "XGGC(20, 20)"; ""]

[<Fact>]
let whileTest() =
    <@
        let f() =
            let mutable i = 0
            while i < 3 do
                outLine(string i)
                i <- i + 1
        f()
    @>
    |> startAsStdSprite
    =? ["0"; "1"; "2"; ""]

[<Fact>]
let unarySubtractTest() =
    <@
        let f x = -x
        printfn "%f" (f 0.)
        printfn "%f" (f 10.)
        printfn "%f" (f -10.)
    @>
    |> startAsStdSprite
    =? ["0"; "-10"; "10"; ""]

[<Fact>]
let minusZeroTest() =
    <@
        printfn "%f" (1. / 0.)
        printfn "%f" (1. / (-1. * 0.))
    @>
    |> startAsStdSprite
    =? ["Infinity"; "-Infinity"; ""]

[<Fact>]
let maxTest() =
    <@
        printfn "%d" (max (outLine "A"; 30) (outLine "B"; 20))
    @>
    |> startAsStdSprite
    =? ["A"; "B"; "30"; ""]

[<ReflectedDefinition>]
module FSharpOpTest =
    [<Struct>]
    type D = { v: double } with
        static member (+) (v1, v2) = { v = v1.v + v2.v }
        static member (-) (v1, v2) = { v = v1.v - v2.v }
        static member (*) (v1, v2) = { v = v1.v * v2.v }
        static member (/) (v1, v2) = { v = v1.v / v2.v }
        static member (~-) v1 = { v = -v1.v }

    [<Sealed>]
    type Sprite1(options) as self =
        inherit Sprite(options)

        do self.WhenGreenFlag {
            let v = { v = 1. } + { v = 2. } - { v = 3. } * { v = 4. } / -{ v = 6. }
            outLine (string v.v)
        }

[<Fact>]
let fsharpOpTest() =
    <@
        defineSprite FSharpOpTest.Sprite1
        
        let v = { FSharpOpTest.v = 10. } + { FSharpOpTest.v = 20. }
        outLine (string v.v)
        startMainLoop()
    @>
    |> startAsStdSprite
    =? ["30"; "5"; ""]

[<Struct>]
type Pair<'T1,'T2> = { mutable _1: 'T1; mutable _2: 'T2 }

[<Fact>]
let setMutableRecordFieldTest() =
    <@
        let mutable x = { _1 = 10.; _2 = { _1 = { _1 = "A"; _2 = 20. }; _2 = 30. } }
        x._2._1 <- { _1 = "A2"; _2 = 22. }

        outLine (string x._1)
        outLine (string x._2._1._1)
        outLine (string x._2._1._2)
        outLine (string x._2._2)
    @>
    |> startAsStdSprite
    =? ["10"; "A2"; "22"; "30"; ""]

[<Struct>]
type Vec = {
    x: double
    y: double
}
[<Struct>]
type Rect = {
    mutable center: Vec
    mutable extend: Vec
}
[<Struct>]
type Player = {
    name: string
    mutable rect: Rect
    velocity: Vec
}

[<Sealed; ReflectedDefinition>]
type SetMutableRecordFieldTestSprite(options) as self =
    inherit Sprite(options)

    let mutable player1 = {
        name = "Player1"
        rect = {
            center = { x = 11.; y = 21. }
            extend = { x = 31.; y = 41. }
        }
        velocity = { x = 51.; y = 61. }
    }
    do self.WhenGreenFlag {
        player1.rect.center <- { x = 101.; y = 201. }

        outLine player1.name
        outLine (string player1.rect.center.x)
        outLine (string player1.rect.center.y)
        outLine (string player1.rect.extend.x)
        outLine (string player1.rect.extend.y)
        outLine (string player1.velocity.x)
        outLine (string player1.velocity.y)
    }

[<Fact>]
let setMutableRecordFieldSpriteTest() =
    <@
        defineSprite SetMutableRecordFieldTestSprite

        let mutable player0 = {
            name = "Player0"
            rect = {
                center = { x = 10.; y = 20. }
                extend = { x = 30.; y = 40. }
            }
            velocity = { x = 50.; y = 60. }
        }
        player0.rect.center <- { x = 100.; y = 200. }

        outLine player0.name
        outLine (string player0.rect.center.x)
        outLine (string player0.rect.center.y)
        outLine (string player0.rect.extend.x)
        outLine (string player0.rect.extend.y)
        outLine (string player0.velocity.x)
        outLine (string player0.velocity.y)

        startMainLoop()
    @>
    |> startAsStdSprite
    =? [
        "Player0"
        "100"
        "200"
        "30"
        "40"
        "50"
        "60"

        "Player1"
        "101"
        "201"
        "31"
        "41"
        "51"
        "61"
        ""
    ]

[<ReflectedDefinition>]
module AccessStageListInSpriteTestStage =
    let output = defineList []
    let out v = SList.push output v
    
    type AccessStageListInSpriteWhenGreenFlagTestSprite(options) as self =
        inherit Sprite(options)
        do self.WhenGreenFlag { out "A" }

    type AccessStageListInSpriteLocalFunctionTestSprite(options) as self =
        inherit Sprite(options)

        let outA() = out "A"
        do self.WhenGreenFlag { outA() }
[<Fact>]
let accessStageListInSpriteLocalFunctionTest() =
    <@
        defineSprite AccessStageListInSpriteTestStage.AccessStageListInSpriteLocalFunctionTestSprite
        startMainLoop()
    @>
    |> startAsStdSprite
    =? ["A"]

[<Fact>]
let accessStageListInSpriteWhenGreenFlagTest() =
    <@
        defineSprite AccessStageListInSpriteTestStage.AccessStageListInSpriteWhenGreenFlagTestSprite
        startMainLoop()
    @>
    |> startAsStdSprite
    =? ["A"]

[<ReflectedDefinition>]
type AccessSpriteVariableInSpriteLocalFunctionTestSprite(options) as self =
    inherit Sprite(options)

    let mutable x = 100.
    let f() =
        x <- x + 1.
        out(string x)

    do self.WhenGreenFlag { f() }

[<Fact>]
let accessSpriteVariableInSpriteLocalFunctionTest() =
    <@
        defineSprite AccessSpriteVariableInSpriteLocalFunctionTestSprite
        startMainLoop()
    @>
    |> startAsStdSprite
    =? ["101"]

[<ReflectedDefinition>]
type AccessSpriteVariableInSpriteWhenGreenFlagTestSprite(options) as self =
    inherit Sprite(options)

    let mutable x = 200.
    do self.WhenGreenFlag { x <- x + 1.; out(string x) }

[<Fact>]
let accessSpriteVariableInSpriteWhenGreenFlagTest() =
    <@
        defineSprite AccessSpriteVariableInSpriteWhenGreenFlagTestSprite
        startMainLoop()
    @>
    |> startAsStdSprite
    =? ["201"]


[<ReflectedDefinition>]
type AccessClassSelfTestSprite(options) as self =
    inherit Sprite(options)
    let f() =
        out "A"
        self.GotoXY(0., 0.)
        out "B"

    do self.WhenGreenFlag { f() }

[<Fact>]
let accessClassSelfTest() =
    <@
        defineSprite AccessClassSelfTestSprite
        startMainLoop()
    @>
    |> startAsStdSprite
    =? ["AB"]

[<Struct>]
type MyValueOption<'T> =
    | VNone
    | VSome of 'T

[<Fact>]
let valueOnlyUnionTest() =
    <@
        let f = function
            | VSome(VSome struct(a, b)) -> a + b
            | _ -> "Other"

        out (f VNone)
        out (f (VSome(VSome struct("A", "B"))))
    @>
    |> startAsStdSprite
    =? ["OtherAB"]


[<Fact>]
let signTest() =
    <@
    outLine (string (sign 10))
    outLine (string (sign -10))
    outLine (string (sign 0))

    outLine (string (sign 10.))
    outLine (string (sign -10.))
    outLine (string (sign 0.))
    outLine (string (sign (0. / 0.)))
    @>
    |> startAsStdSprite
    =? ["1";"-1";"0";"1";"-1";"0";"0";""]

[<Fact>]
let withMeasureTest() =
    <@
        out(string<int> %(N.WithMeasure(%10)))
    @>
    |> startAsStdSprite
    =? ["10"]

[<ReflectedDefinition>]
type InitIntToString = | InitIntToString with
    interface IFunc<struct(S Reference * int), hunit> with
        member _.Invoke struct(r, i) = r <-% "item" + string i; HUnit

[<Fact>]
let newVectorTest() =
    <@
        let v = Vector.newVector 3 InitIntToString
        outLine (string ((v->%Vector.count): int))
        outLine %(Vector.get v 0)
        outLine %(Vector.get v 1)
        outLine %(Vector.get v 2)
    @>
    |> startAsStdSprite
    =? ["3"; "item0"; "item1"; "item2"; ""]

[<ReflectedDefinition>]
type StringConcat = | StringConcat with
    interface IFunc<struct(string * S Reference), string> with
        member _.Invoke struct(a, s) = a + "," + %Memory.read s

[<Fact>]
let vectorFoldTest() =
    <@
        let xs = Vector.newVector 3 InitIntToString
        out <| Vector.fold StringConcat "init" xs
    @>
    |> startAsStdSprite
    =? ["init,item0,item1,item2"]

[<AutoOpen; ReflectedDefinition>]
module NoAllocFunTest =
    type Compose<'T1,'T2,'T3,'F1,'F2> when 'F1 :> IFunc<'T1,'T2> and 'F2 :> IFunc<'T2,'T3> = {
        f1: 'F1
        f2: 'F2
    }
    with
        interface IFunc<'T1,'T3> with
            member f.Invoke x = f.f2.Invoke(f.f1.Invoke x)
    let compose f1 f2 = { f1 = f1; f2 = f2 }

    let doubleApply (f: 'F when 'F :> IFunc<_,_> and 'F : not struct) x = f.Invoke (f.Invoke x)

    type Mul2 = | Mul2 with
        interface IFunc<int, int> with
            member _.Invoke x = x * 2

    type Incr = | Incr with
        interface IFunc<int, int> with
            member _.Invoke x = x + 1

[<Fact>]
let noAllocFunTest() =
    <@
        // x1 = (10 * 2 + 1) * 2 + 1
        let x1 = doubleApply (compose Mul2 Incr) 10

        // x2 = ((((20 + 1) * 2) + 1) * 2)
        let x2 = doubleApply (compose Incr Mul2) 20

        printfn "%d" x1
        printfn "%d" x2
        printfn "heapSize = %d" <| Allocator.Diagnostics.getHeapSize()
    @>
    |> startAsStdSpriteWith (fun c -> { c with plugin = { c.plugin with postTransform = Plugins.empty } }) id
    =? ["43"; "86"; "heapSize = 0"; ""]

[<Fact>]
let valueOnlyNewUnionCaseTest() =
    <@
        let printN = function
            | StructNone -> printfn "StructNone"
            | StructSome x -> printfn "StructSome %f" x

        let printS = function
            | StructNone -> printfn "StructNone"
            | StructSome x -> printfn "StructSome \"%s\"" x

        printN StructNone
        printN(StructSome 123.)
        printS StructNone
        printS(StructSome "ABC")
    @>
    |> startAsStdSprite
    =? [
        "StructNone"
        "StructSome 123"
        "StructNone"
        "StructSome \"ABC\""
        ""
    ]

[<ReflectedDefinition>]
let typeFunction<[<Measure>]'u> = LanguagePrimitives.FloatWithMeasure<'u> 123.0

[<ReflectedDefinition>]
type CallTypeFunctionInSpriteTestSprite(options) as this =
    inherit Sprite(options)
    do this.WhenGreenFlag {
        out (string typeFunction)
    }

[<Fact>]
let callTypeFunctionInSpriteTest() =
    <@ defineSprite CallTypeFunctionInSpriteTestSprite @>
    |> startAsStdSprite
    =? ["123"]

[<ReflectedDefinition>]
type SpriteOnlyStackTestSprite(options) as this =
    inherit Sprite(options)
    do this.WhenGreenFlag {
        let mutable x = 2
        do! repeatAsync 3 {
            outLine (string x)
            x <- x * x
        }
    }

[<Fact>]
let spriteOnlyStackTest() =
    <@ defineSprite SpriteOnlyStackTestSprite @>
    |> startAsStdSpriteWith id (fun c -> { c with useVariableDefinitionCheck = true })
    =? ["2"; "4"; "16"; ""]

[<ReflectedDefinition>]
type ReturnStackDeleteCloneTestSprite(options) as this =
    inherit Sprite(options)

    let f() = tightrope {
        let mutable x = 0
        do! foreverAsync {
            out (string x)
            x <- x + 1
            if x = 2 then
                do! this.DeleteClone()
        }
    }
    do this.WhenGreenFlag { this.Clone() }
    do this.WhenCloned { do! f() }

[<ReflectedDefinition>]
type DeleteCloneAfterOperationTestSprite(options) as this =
    inherit Sprite(options)

    do this.WhenGreenFlag {
        outLine "A"
        do! this.DeleteClone()
        outLine "B"
    }

[<Fact>]
let deleteCloneAfterOperationTest() =
    <@ defineSprite DeleteCloneAfterOperationTestSprite @>
    |> startAsStdSprite
    =? ["A"; "B"; ""]

[<ReflectedDefinition>]
type ManyThreadStackTestSprite(options) as self =
    inherit Sprite(options)

    let mutable id = 0
    do self.WhenGreenFlag {
        do! repeatAsync 300 {
            id <- id + 1
            self.Clone()
        }
    }
    do self.WhenCloned {
        let mutable x = 0
        do! repeatAsync 2 {
            out (string id)
            out "-"
            outLine (string x)
            x <- x + 1
        }
    }

[<Fact>]
let manyThreadStackTest() =
    <@
        defineSprite ManyThreadStackTestSprite
        startMainLoop()
    @>
    |> startAsStdSpriteWith id (fun c -> { c with useLengthCheck = true; listMaxLength = Some 2048 })
    =? [
        for x in 1..300 do
            for y in 0..1 ->
                sprintf "%d-%d" x y
        yield ""
    ]

[<Fact>]
let emptyRentStackTest() =
    let stage =
        <@
        whenGreenFlag {
            do! foreverAsync {
                do! waitElapsedFrom 1.
                SList.push output "loop"
            }
        }
        startMainLoop()
        @>
        |> transpileStage

    stage.scripts
    |> Seq.tryFind (function { script = Script.Procedure _ } -> true | _ -> false)
    =? None

    stage.lists
    |> Seq.tryFind (fun l -> l.listName <> "output")
    =? None

[<Fact>]
let doAskAndAnswerTest() =
    let view() =
        let mutable question = ""
        StageView.poly { new StageView.IgnoreBase<_>() with
            override _.ShowStageQuestion q = question <- q
            override _.HideStageQuestion() = question <- ""
            override _.ShowInputBox(_, f) = HUnit.toUnit <| Func.invoke &f (String.replicate 2 question)
        }
    <@
    whenGreenFlag {
        do! doAsk "abc"
        outLine answer
    }
    startMainLoop()
    @>

    |> startAsStdSpriteWith' id (EvaluateConfig.withView <| view()) (ExecutionConfig.withView <| view())
    =? ["abcabc"; ""]

[<Fact>]
let joinSingleListItemTest() =
    <@
    let list = defineList []
    SList.push list "A"
    SList.push list "B"

    out (SList.join list)
    @>
    |> startAsStdSprite
    =? ["AB"]

[<Fact>]
let joinNonSingleListItemTest() =
    <@
    let list = defineList []
    SList.push list "A"
    SList.push list "BC"

    out (SList.join list)
    @>
    |> startAsStdSprite
    =? ["A BC"]

[<Fact>]
let joinEmptyListItemTest() =
    <@
    let list = defineList []
    SList.push list "A"
    SList.push list ""

    out (SList.join list)
    @>
    |> startAsStdSprite
    =? ["A "]
