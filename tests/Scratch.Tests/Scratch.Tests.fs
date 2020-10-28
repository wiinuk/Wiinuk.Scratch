module Scratch.Tests
open System
open FSharp.Reflection
open Scratch.Ast
open Scratch.Primitives
open Scratch.Reflection
open Scratch.Reflection.Expr
open Scratch.Reflection.Transformers
open Scratch.Runtime.Test
open Scratch.AstDefinitions
open Scratch.Threading
open Xunit
open FsCheck
module A = Scratch.Ast.Expression
module A = Scratch.Ast.Expressions
module E = FSharp.Quotations.DerivedPatterns
module E = FSharp.Quotations.Patterns
type E = FSharp.Quotations.Expr


[<Fact>]
let tryParseSNumberTest() =
    let parse = SValue.tryParseSNumber
    parse "\u3000-0x01Fe0\u3000" =? ValueSome -8160.0
    parse "\u3000-01234\u3000" =? ValueSome -1234.0
    parse "\u3000-01.23e4\u3000" =? ValueSome -12300.0
    parse "\u3000-.123\u3000" =? ValueSome -0.123
    parse "\u3000-123.\u3000" =? ValueSome -123.
    parse "\u3000-.2e3\u3000" =? ValueSome -200.

[<Fact>]
let emptyStringToNumberTest() =
    SValue.toNumber (SString "") =? 0.

let makeOptions (runtime: Runtime) (prototype: Entity option) =
    FSharpValue.MakeRecord(
        typeof<EntityInitializeOptions>,
        [|runtime :> obj; prototype :> obj|],
        allowAccessToPrivateRepresentation = true
    ) :?> EntityInitializeOptions

[<Sealed>]
type Sprite1(options) =
    inherit Sprite(options)

    let mutable v1 = 0
    let mutable v2 = 1
    let xs1 = defineList []
    member _.V1 with get() = v1 and set v = v1 <- v
    member _.V2 with get() = v2 and set v = v2 <- v
    member _.Xs1 = xs1

[<Fact>]
let cloneCompilerTest() =
    let cloneF = CloneCompiler.compileCloneFunc typeof<Sprite1>

    let s1 = Sprite1(makeOptions Runtime.globalRuntime None)

    s1.V1 <- 10
    s1.V2 <- 20
    SList.push s1.Xs1 30

    let s2 = cloneF.Invoke(s1, makeOptions Runtime.globalRuntime (Some(upcast s1))) :?> Sprite1

    s2.V1 =? 10
    s2.V2 =? 20
    SList.length s2.Xs1 =? 1
    SList.get s2.Xs1 1 =? 30

    s1.V1 <- 100
    s1.V2 <- 200
    SList.push s1.Xs1 300

    s2.V1 =? 10
    s2.V2 =? 20
    SList.length s2.Xs1 =? 1
    SList.get s2.Xs1 1 =? 30

    s1.V1 =? 100
    s1.V2 =? 200
    SList.length s1.Xs1 =? 2
    SList.get s1.Xs1 1 =? 30
    SList.get s1.Xs1 2 =? 300

[<Fact>]
let locationTest() =
    let e = E.Value(())
    getLocation e =? None

    let forall e l2 =
        let l = getLocation e
        let e2 = withLocation l2 e
        getLocation e =? l
        getLocation e2 =? l2

    let es = [e; <@@ () @@> ]
    let ls = [None; Some { path = ""; position1 = { line = 10; column = 20 }; position2 = { line = 30; column = 40 } }]
    for e in es do
        for l in ls do
            forall e l


    let es = [E.Value 10 |> E.Cast<int>; <@ 20 @> ]
    let forall e l2 =
        let l = getLocation e
        let e2 = withLocationTyped l2 e
        getLocation e =? l
        getLocation e2 =? l2

    for e in es do
        for l in ls do
            forall e l

[<Struct>]
type U1 = | C1 of x1: int | C2 of x2: string
type U2 = | C1 of x1: int | C2 of x2: string
with
    member _.P = ()


[<Fact>]
let casePropertyGetCoreceTest() =
    let f = transformUnionCasePropertyGetCorece
    transformDeepOnce f <@ function U1.C1 x -> x | _ -> failwith "error" @> |> Option.map string =? None
    transformDeepOnce f <@ function U2.C1 x -> x | _ -> failwith "error" @> |> Option.isSome =? true
    transformDeepOnce f <@ function U2.C1 _ -> 0 | _ -> failwith "error" @> |> Option.map string =? None
    transformDeepOnce f <@ function U2.C1 _ as x -> x.P | _ -> failwith "error" @> |> Option.map string =? None

[<ReflectedDefinition>]
type T = { v: int } with
    member x.g(a, b, c) = x.v + a + b + c
    member x.f a (b, c) d = x.v + a + b + c + d
    static member m a (b, c) d = a + b + c + d
    static member n(a, b, c) = a + b + c
    
let p x = printfn "%A" x; x

[<Fact>]
let universalCallTest() =
    let (|P|_|) = function E.SpecificCall <@ p @> (_,_,[x]) -> Some x | _ -> None
    let (|PInt|_|) = function P(E.Int32 x) -> Some x | _ -> None

    match <@ fun (x: T) -> x.f 1 (2, 3) 4 @> with
    | E.Lambda(x, UniversalCall(Some(E.Var x'), _, [E.Int32 1; E.Int32 2; E.Int32 3; E.Int32 4])) when x = x' -> ()
    | e -> failwithf "%A" e

    match <@ fun (x: T) -> (p x).f (p 1) (p 2, p 3) (p 4) @> with
    | E.Lambda(x, UniversalCall(Some(P(E.Var x')), _, [PInt 1; PInt 2; PInt 3; PInt 4])) when x = x' -> ()
    | e -> failwithf "%A" e

    match <@ T.m 1 (2, 3) 4 @> with
    | UniversalCall(None, _, [E.Int32 1; E.Int32 2; E.Int32 3; E.Int32 4]) -> ()
    | e -> failwithf "%A" e

    match <@ T.m (p 1) (p 2, p 3) (p 4) @> with
    | UniversalCall(None, _, [PInt 1; PInt 2; PInt 3; PInt 4]) -> ()
    | e -> failwithf "%A" e

    match <@ fun (x: T) -> (p x).g(p 1, p 2, p 3) @> with
    | E.Lambda(x, UniversalCall(Some(P(E.Var x')), _, [PInt 1; PInt 2; PInt 3])) when x = x' -> ()
    | e -> failwithf "%A" e

    match <@ T.n(p 1, p 2, p 3) @> with
    | UniversalCall(None, _, [PInt 1; PInt 2; PInt 3]) -> ()
    | e -> failwithf "%A" e


[<ReflectedDefinition>]
type T<'A> =
    static member InfinityLoopSetter with set(v: 'A): unit = T<'A option>.InfinityLoopSetter <- Some v
    static member InfinityLoopGetter: unit = T<'A option>.InfinityLoopGetter

    static member ShowSomes(i: int, x: 'A): string =
        if i = 0 then sprintf "%A" x
        else T<'A option>.ShowSomes(i - 1, Some x)

[<ReflectedDefinition>]
type T2 =
    static member showSomes<'A> (i: int) (x: 'A): string =
        if i = 0 then sprintf "%A" x
        else T2.showSomes (i - 1) (Some x)

[<Fact>]
let genericTypeGenericMethodDefinitionTest() =
    let m = typeof<T<char>>.GetMethod "ShowSomes"
    Member.genericTypeGenericMethodDefinition m =? typedefof<T<_>>.GetMethod "ShowSomes"

    let p = typeof<T<char>>.GetProperty "InfinityLoopSetter"
    Member.genericTypeGenericMethodDefinition p.SetMethod =? typedefof<T<_>>.GetProperty("InfinityLoopSetter").SetMethod

    let p = typeof<T<char>>.GetProperty "InfinityLoopGetter"
    Member.genericTypeGenericMethodDefinition p.GetMethod =? typedefof<T<_>>.GetProperty("InfinityLoopGetter").GetMethod

    let m = typeof<T2>.GetMethod("showSomes").MakeGenericMethod(typeof<char>)
    Member.genericTypeGenericMethodDefinition m =? typeof<T2>.GetMethod("showSomes")

[<Fact>]
let omapTest() =
    OMap.toListOrdered (OMap.ofList [1, "A"; 3, "C"; 1, "a"]) =? [3, "C"; 1, "a"]
    OMap.toListSorted (OMap.ofList [1, "A"; 3, "C"; 1, "a"]) =? [1, "a"; 3, "C"]

[<Fact>]
let omapEqualityTest() =
    OMap.ofList ["a", 1; "a", 2] = OMap.ofList ["a", 2]

let schedulerNowProperty (NormalFloat processSpan) (NormalFloat flameSpan) =
    /// -1.<s> < s && s < 1.<s>
    let toMiniSpan s = TimeSpan.FromSeconds (s % 1.)
    let processSpan = toMiniSpan processSpan
    let flameSpan = toMiniSpan flameSpan

    let config = {
            startTime = Some(DateTime(2000, 1, 1))
            flame = Turbo
            deterministic = DeterministicTime {
                processSpan = processSpan
                flameSpan = flameSpan
            }
    }
    let s = Scheduler.make config
    let t1 = Scheduler.now s
    let t2 = Scheduler.now s
    t2 - t1 =? processSpan

    Scheduler.nextFlame s
    let t3 = Scheduler.now s
    t3 - t2 =? processSpan + flameSpan

[<Fact>]
let schedulerNowTest() = qcheck schedulerNowProperty

[<Fact>]
let lambdasTest() =
    match <@ fun struct(x, a, b) -> x + a + b @> with
    | Lambdas([[arg0]], body) -> body.GetFreeVars() |> Set =? Set [arg0]
    | _ -> ()

    match <@ fun (x, a, b) -> x + a + b @> with
    | Lambdas([[x; a; b]], body) -> body.GetFreeVars() |> Set =? Set [a; b; x]
    | _ -> ()

[<Fact>]
let lambdasAndApplicationsTest() =
    let check = function
        | E.Let(_, Lambdas(vvs, _), Applications(_, xxs)) ->
            (vvs, xxs)
            ||> List.iter2 (List.iter2 (fun v x -> v.Type =? x.Type))

        | _ -> failwith ""

    check <@ let f struct(a, b) = a + b in f(10, 20) @>
    check <@ let f () = () in f () @>
    check <@ let f () () = () in f () () @>


type F<'T> = | F with member _.M<'U>() = ()

[<Fact>]
let methodIdTest() =
    let id = MemberId.methodId
    let ``F<>.M<>`` = id <| typedefof<F<_>>.GetMethod "M"

    let ``F<int>.M<>`` = id <| typeof<F<int>>.GetMethod "M"
    let ``F<double>.M<>`` = id <| typeof<F<double>>.GetMethod "M"

    ``F<>.M<>`` <>? ``F<int>.M<>``
    ``F<int>.M<>`` <>? ``F<double>.M<>``

    let ``F<>.M<char>`` = id <| typedefof<F<_>>.GetMethod("M").MakeGenericMethod(typeof<char>)
    let ``F<>.M<string>`` = id <| typedefof<F<_>>.GetMethod("M").MakeGenericMethod(typeof<string>)
    ``F<>.M<>`` <>? ``F<>.M<char>``
    ``F<>.M<char>`` <>? ``F<>.M<string>``

    let ``F<int>.M<char>`` = id <| typeof<F<int>>.GetMethod("M").MakeGenericMethod(typeof<char>)
    let ``F<int>.M<string>`` = id <| typeof<F<int>>.GetMethod("M").MakeGenericMethod(typeof<string>)
    let ``F<double>.M<char>`` = id <| typeof<F<double>>.GetMethod("M").MakeGenericMethod(typeof<char>)

    ``F<int>.M<>`` <>? ``F<int>.M<char>``
    ``F<int>.M<char>`` <>? ``F<int>.M<string>``
    ``F<double>.M<char>`` <>? ``F<int>.M<char>``

    ``F<int>.M<char>`` <>? ``F<>.M<>``
    ``F<int>.M<char>`` <>? ``F<>.M<char>``
    ``F<int>.M<char>`` <>? ``F<int>.M<>``

let newLine = "\n"
let pretty x = Pretty.prettyWith (fun c -> { c with document = { c.document with maxWidth = 30; newLine = newLine } }) x
let (!) x = A.eNumber () (double x)

[<Fact>]
let prettyNameTest() =
    { StageData.defaultValue with
        variables = [VariableData.make () "show" SType.N]
        scripts = [
            ScriptData.procedure () "show" [ParameterDefinition.make () "show" SType.N] NoAtomic [
                Expressions.show ()
                Expressions.``setVar:to:`` () "show" (Expressions.getParam () "show")
                Expressions.call () "show" [Expressions.getParam () "show"]
            ]
        ]
    }
    |> pretty
    =? String.concat newLine [
        "stage Stage"
        "let show = 1"
        "@async let show show ="
        "    system show ()"
        "    stage show <- show"
        "    show show"
        ""
    ]

[<Fact>]
let prettyOpTest() =
    { StageData.defaultValue with
        scripts = [
            ScriptData.procedure () "f" [ParameterDefinition.make () "x" SType.N] Atomicity.Atomic [
                A.call () "f" [A.``+`` () (A.``*`` () !1 !2) !3]
                A.call () "f" [A.``/`` () (A.``-`` () !4 !5) !6]
            ]
        ]
    }
    |> pretty
    =? String.concat newLine [
        "stage Stage"
        "let f x ="
        "    f (1 * 2 + 3)"
        "    f ((4 - 5) / 6)"
        ""
    ]

[<Fact>]
let procedureNameManglingRoundTripTest() = quickcheck <| fun (NonNull baseName) types ->
    let n = mangleProcedureName baseName (List.toSeq types)
    let baseName', types' = demangleProcedureName n
    let types' = types' |> List.map (function Ok x -> x | Error x -> failwithf "unknown type '%c'" x)

    (baseName, types) =? (baseName', types')
