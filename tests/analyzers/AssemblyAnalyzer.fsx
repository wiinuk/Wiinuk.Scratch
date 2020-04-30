#load "CecilEx.fsx"
#load "SeqEx.fsx"
open System.Collections.Generic
open System.Runtime.CompilerServices
open Mono.Cecil
open Mono.Cecil.Cil
open CecilEx
open Print
type C = System.ConsoleColor


[<Struct>]
type CallSite = {
    body: MethodBody
    instruction: Instruction
    sequencePoint: SequencePoint Lazy
}
type History = CallSite list
type Allocation = {
    allocSite: CallSite
    allocType: TypeDefinition
    callHistory: History
    dependentedNewobjs: struct(MethodBody * Instruction) list
}

type MethodKey = (struct(int * string * unit list))

[<RequireQualifiedAccess>]
type OutputLevel =
    | Error
    | Warning
    | Info
    | All

type VaridateConfig = {
    searchDirectories: string list
    verifyAttributes: bool
    outputLevel: OutputLevel
}

type NoGcAllocationConfig = {
    outputLevel: OutputLevel
    allocatableTypes: TypeDefinition list
    treatAsNoAllocation: bool
}

type ISR =
    abstract Value_OfNamedParaeeter_Was_Ignored: unit -> PrintfFormat<obj -> string -> string -> 'r,'d,'e,'r>
    abstract MethodWasTreatedAsUnAllocated: unit -> PrintfFormat<'r,'d,'e,'r>
    abstract InternalCallsCanNotBeTracked: unit -> PrintfFormat<'r,'d,'e,'r>
    abstract VirtualCallsCanNotBeTracked: unit -> PrintfFormat<'r,'d,'e,'r>
    abstract MethodBodyNotFound: unit -> PrintfFormat<'r,'d,'e,'r>
    abstract VirtualMethodsCanNotBeTrackedAtRuntime: unit -> PrintfFormat<'r,'d,'e,'r>
    abstract CalliCanNotBeTracked: unit -> PrintfFormat<'r,'d,'e,'r>
    abstract JmpCanNotBeTracked: unit -> PrintfFormat<'r,'d,'e,'r>
    abstract TheMethodWasIgnored: unit -> PrintfFormat<'r,'d,'e,'r>
    abstract OkIllegalGcAllocationWasNotFound: unit -> PrintfFormat<'r,'d,'e,'r>
    abstract IllegalGcAllocationWasFound: unit -> PrintfFormat<string -> 'r,'d,'e,'r>
    abstract NearestCallSiteIsHere: unit -> PrintfFormat<string -> 'r,'d,'e,'r>
    abstract ActualAllocationOpcodeIsHere: unit -> PrintfFormat<string -> 'r,'d,'e,'r>

let defaultSR() = {
    new ISR with
        member _.Value_OfNamedParaeeter_Was_Ignored() = "value '%O' of named parameter %s.%s was ignored."
        member _.MethodWasTreatedAsUnAllocated() = "method was treated as un allocated."
        member _.InternalCallsCanNotBeTracked() = "internal calls can not be tracked."
        member _.VirtualCallsCanNotBeTracked() = "virtual calls can not be tracked."
        member _.MethodBodyNotFound() = "method body not found."
        member _.VirtualMethodsCanNotBeTrackedAtRuntime() = "virtual methods can not be tracked at runtime."
        member _.CalliCanNotBeTracked() = "calli can not be tracked."
        member _.JmpCanNotBeTracked() = "jmp can not be tracked."
        member _.TheMethodWasIgnored() = "the method was ignored."
        member _.OkIllegalGcAllocationWasNotFound() = "OK. Illegal GC allocation was not found."
        member _.IllegalGcAllocationWasFound() = "Illegal GC allocation was found. ( %s )"
        member _.NearestCallSiteIsHere() = "Nearest call site is here. ( %s )"
        member _.ActualAllocationOpcodeIsHere() = "Actual allocation opcode is here. ( %s )"
}
let srs = Map [
    "ja-JP", {
        new ISR with
            member _.Value_OfNamedParaeeter_Was_Ignored() = "値 '%O' (名前付きパラメータ %s.%s) は無視されました。"
            member _.MethodWasTreatedAsUnAllocated() = "メソッドは非確保として扱われました。"
            member _.InternalCallsCanNotBeTracked() = "内部呼び出しは追跡できません。"
            member _.VirtualCallsCanNotBeTracked() = "仮想呼び出しは追跡できません。"
            member _.MethodBodyNotFound() = "メソッド本体が見つかりません。"
            member _.VirtualMethodsCanNotBeTrackedAtRuntime() = "仮想メソッドは実行時には追跡できません。"
            member _.CalliCanNotBeTracked() = "calli は追跡できません。"
            member _.JmpCanNotBeTracked() = "jmp は追跡できません。"
            member _.TheMethodWasIgnored() = "メソッドは無視されました。"
            member _.OkIllegalGcAllocationWasNotFound() = "OK. 不法な GC 確保は見つかりませんでした。"
            member _.IllegalGcAllocationWasFound() = "不法な GC 確保が見つかりました。( %s )"
            member _.NearestCallSiteIsHere() = "最も近い呼び出し箇所はここ。( %s )"
            member _.ActualAllocationOpcodeIsHere() = "実際の確保コードはここ。( %s )"
    }
]

let sr =
    match Map.tryFind System.Globalization.CultureInfo.InstalledUICulture.Name srs with
    | None -> defaultSR()
    | Some sr -> sr

let printIgnoreMessage (config, c) v name =
    if config.verifyAttributes then
        cprintfn (fun c -> { c with fore = C.DarkGray }) (sr.Value_OfNamedParaeeter_Was_Ignored()) v c.attributeType.Name name

let parseAllocatableTypes env (name, v: CustomAttributeArgument) =
    match v.Value with
    | :? array<CustomAttributeArgument> as ts ->
        ts
        |> Seq.choose (fun t ->
            match t.Value with
            | :? TypeReference as t -> t.Resolve() |> Some
            | v ->
                printIgnoreMessage env v name
                None
        )
        |> Seq.toList
        |> Some

    | _ ->
        printIgnoreMessage env v name
        None

let parseOutputLevel env (name, v) =
    match v with
    | StringOrEnumArgument "Warn" _
    | StringOrEnumArgument "Warning" _ -> Some OutputLevel.Warning

    | StringOrEnumArgument "Info" _
    | StringOrEnumArgument "Infomation" _ -> Some OutputLevel.Info

    | _ ->
        printIgnoreMessage env v name
        None

let printRestMessages env rest =
    for p, a in rest.arguments.Value do
        printIgnoreMessage env a.Value p.Name

    for p in rest.properties.Value do
        printIgnoreMessage env p.Argument.Value p.Name

    for f in rest.fields.Value do
        printIgnoreMessage env f.Argument.Value f.Name

let parseNoGcAllocation config = function
    | Name "NoGcAllocation" (NamedValueOptional "AllocatableTypes" (types, (NamedValueOptional "OutputLevel" (level, (NamedValueOptional "TreatAsNoAllocation" (treatAsNoAllocation, rest))))) as c) ->

        let env = config, c
        let ts = types |> Option.bind (parseAllocatableTypes env) |> Option.defaultValue []
        let level = level |> Option.bind (parseOutputLevel env) |> Option.defaultValue config.outputLevel
        let treatAsNoAllocation =
            match treatAsNoAllocation with
            | None -> false
            | Some(name, v) ->
                match v.Value with
                | :? bool as x -> x
                | _ ->
                    printIgnoreMessage env v name
                    false

        if config.verifyAttributes then
            printRestMessages env rest

        Some {
            treatAsNoAllocation = treatAsNoAllocation
            allocatableTypes = ts
            outputLevel = level
        }
    | _ -> None

let noGcAllocationConfigFromMethod config (m: MethodDefinition) =
    m.CustomAttributes
    |> Seq.tryPick (parseNoGcAllocation config)
    |> Option.orElseWith(fun () ->
        match m.DeclaringType with
        | null -> None
        | t -> t.CustomAttributes |> Seq.tryPick (parseNoGcAllocation config)
    )

type AllocEnv = {
    outputLevel: OutputLevel
    cache: Dictionary<MethodKey, Allocation seq>
    varidateConfig: VaridateConfig
}
[<Struct>]
type AllocCollecterEnv = {
    callSite: struct(MethodBody * Instruction)
    config: AllocEnv
    visitedMethods: HashSet<MethodKey>
}
let withCallSite body i env = { env with callSite = struct(body, i) }
let methodKey (m: MethodReference) = struct(m.MetadataToken.ToInt32(), m.Module.Name, [])
let pushCallHistory history alloc =
    { alloc with
        callHistory = history::alloc.callHistory
    }
let addDependency body i alloc =
    { alloc with
        dependentedNewobjs = struct(body, i)::alloc.dependentedNewobjs
    }

let withEnvMethod env m c =
    let struct(b, i) = env.callSite
    let s = match b with null -> m | _ -> b.Method
    { c with site = s; operation = i }

let withOp operation c =
    { c with site = operation.env.body.Method; operation = operation.instruction }

let info env m message =
    if OutputLevel.Info <= env.config.outputLevel then
        lprintfn (fun c -> { withEnvMethod env m c with color = C.DarkCyan }) "info: %s" message

let rec collectAllocs' env (m: MethodDefinition) =
    let s = noGcAllocationConfigFromMethod env.config.varidateConfig m
    match s with
    | Some s when s.treatAsNoAllocation ->
        info env m (sprintf <| sr.MethodWasTreatedAsUnAllocated())
        List.toSeq []

    | _ ->

    if not m.HasBody then
        if m.IsInternalCall || m.IsPInvokeImpl then
            info env m (sprintf <| sr.InternalCallsCanNotBeTracked())

        elif m.IsAbstract then
            if OutputLevel.Warning <= env.config.outputLevel then
                lprintfn (fun c -> { withEnvMethod env m c with color = C.Yellow }) "warning: %s" (sprintf <| sr.VirtualCallsCanNotBeTracked())
        else
            lprintfn (fun c -> { c with site = m; color = C.Red }) "error: %s" (sprintf <| sr.MethodBodyNotFound())

        List.toSeq []
    else

    let body = m.Body
    let debug = m.DebugInformation
    let openv = { body = body; data = () }

    body.Instructions
    |> Seq.collect (fun i ->
        let operation = { instruction = i; env = openv }
        match i.OpCode.Code with
        | C.Box ->
            let t: TypeReference = operandOrRaise operation
            if t.IsValueType then
                let alloc = {
                    allocSite = {
                        sequencePoint = lazy nearestSeqiencePoint debug i
                        instruction = i
                        body = body
                    }
                    allocType = t.Resolve()

                    callHistory = []
                    dependentedNewobjs = []
                }
                [alloc] :> _ seq
            else upcast []

        | C.Newarr ->
            let et: TypeReference = operandOrRaise operation
            let alloc = {
                allocSite = {
                    sequencePoint = lazy nearestSeqiencePoint debug i
                    instruction = i
                    body = body
                }
                allocType = ArrayType(et).Resolve()

                callHistory = []
                dependentedNewobjs = []
            }
            upcast [alloc]

        | C.Newobj ->
            seq {
                let m: MethodReference = operandOrRaise operation
                let callSite = {
                    sequencePoint = lazy nearestSeqiencePoint debug i
                    instruction = i
                    body = body
                }
                if not m.DeclaringType.IsValueType then
                    yield {
                        allocSite = callSite
                        allocType = m.DeclaringType.Resolve()

                        callHistory = []
                        dependentedNewobjs = []
                    }
                for alloc in collectAllocsCached (withCallSite body i env) m do
                    yield addDependency body i alloc |> pushCallHistory callSite
            }

        | C.Call ->
            let m: MethodReference = operandOrRaise operation
            let m = m.Resolve()
            let callSite = {
                sequencePoint = lazy nearestSeqiencePoint debug i
                instruction = i
                body = body
            }
            collectAllocsCached (withCallSite body i env) m
            |> Seq.map (pushCallHistory callSite)

        | C.Callvirt ->
            let m: MethodReference = operandOrRaise operation
            let m = m.Resolve()
            let m =
                if m.IsVirtual && not m.IsFinal then
                    let resolvedMethod =
                        match i.Previous with
                        | null -> None
                        | prev ->

                        match prev.OpCode.Code, prev.Operand with
                        | Code.Constrained, (:? TypeReference as constrainedType) ->
                            if constrainedType.IsGenericParameter then
                                None
                            else

                            // constrained. %Type
                            // callvirt instance %ParentMethod
                            match findOverridedMethod (constrainedType.Resolve()) m with
                            | [m] -> Some m
                            | _ -> None

                        | _ -> None

                    match resolvedMethod with
                    | Some m -> Some m
                    | None ->
                        if OutputLevel.Warning <= env.config.outputLevel then
                            lprintfn (fun c -> withOp operation { c with color = C.Yellow }) "warning: %s" (sprintf <| sr.VirtualMethodsCanNotBeTrackedAtRuntime())
                        None
                else
                    Some m

            match m with
            | None -> upcast []
            | Some m ->
                let callSite = {
                    sequencePoint = lazy nearestSeqiencePoint debug i
                    instruction = i
                    body = body
                }
                collectAllocsCached (withCallSite body i env) m
                |> Seq.map (pushCallHistory callSite)

        | C.Calli ->
            if OutputLevel.Warning <= env.config.outputLevel then
                lprintfn (fun c -> withOp operation { c with color = C.Yellow }) "warning: %s" (sprintf <| sr.CalliCanNotBeTracked())
            upcast []

        | C.Jmp ->
            if OutputLevel.Warning <= env.config.outputLevel then
                lprintfn (fun c -> withOp operation { c with color = C.Yellow }) "warning: %s" (sprintf <| sr.JmpCanNotBeTracked())
            upcast []

        | _ -> upcast []
    )

and collectAllocsCached env m =
    RuntimeHelpers.EnsureSufficientExecutionStack()

    let cache, k = env.config.cache, methodKey m
    if not <| env.visitedMethods.Add k then Seq.empty else

    let mutable result = null
    if cache.TryGetValue(k, &result) then result else

    let r = collectAllocs' env <| m.Resolve()
    cache.Add(k, r)
    r

let isAllocatable allocConfig t =
    allocConfig.allocatableTypes
    |> Seq.exists (fun allocType ->
        isAssignableFrom allocType t
    )

let validAllocation allocConfig allocation =
    isAllocatable allocConfig allocation.allocType ||
    allocation.dependentedNewobjs
    |> List.exists (fun struct(body, i) ->
        match i.OpCode.Code with
        | C.Newobj ->
            let m = i.Operand :?> MethodReference
            let t = m.DeclaringType
            if not t.IsValueType then
                isAllocatable allocConfig <| t.Resolve()
            else
                false
        | _ -> false
    )

let callSiteId s = struct(s.instruction.Offset, s.body.Method.MetadataToken, s.body.Method.Module.Name)
let callSiteEq s1 s2 =
    s1.instruction.Offset = s2.instruction.Offset && (
        let m1 = s1.body.Method
        let m2 = s2.body.Method
        m1.MetadataToken = m2.MetadataToken &&
        m1.Module.Name = m2.Module.Name
    )

let printNoGcAllocation cache varidateConfig allocConfig m =
    if allocConfig.treatAsNoAllocation then
        if OutputLevel.Info <= allocConfig.outputLevel then
            lprintfn (fun c -> { c with site = m; color = C.DarkCyan }) "info: %s" (sprintf <| sr.TheMethodWasIgnored())
    else

    let env = {
        config = {
            cache = cache
            outputLevel = allocConfig.outputLevel
            varidateConfig = varidateConfig
        }
        callSite = struct(null, null)
        visitedMethods = HashSet()
    }
    let allocs = collectAllocsCached env m

    let badAllocs =
        allocs
        |> Seq.filter (not << validAllocation allocConfig)
        |> Seq.map (fun a ->
            let nearestCallSite = a.callHistory |> List.tryFindBack (fun h -> not (isNull h.sequencePoint.Value))
            a, nearestCallSite
        )
        //|> Seq.distinctBy (fun (a, nearest) ->
        //    match nearest with
        //    | None -> Choice1Of2(callSiteId a.allocSite)
        //    | Some x -> Choice2Of2(callSiteId x)
        //)
        |> Seq.cache

    if Seq.isEmpty badAllocs then
        if OutputLevel.All <= varidateConfig.outputLevel then
            lprintfn (fun c -> { c with site = m; color = C.DarkGreen }) (sr.OkIllegalGcAllocationWasNotFound())
    else
        lprintfn (fun c -> { c with site = m; color = C.Yellow }) "warning: %s" <| sprintf (sr.IllegalGcAllocationWasFound()) m.Name

        let badAllocs, rest = Seq.splitAt 1 badAllocs
        for alloc, nearestCallSite in badAllocs do
            match nearestCallSite with
            | Some nearestCallSite when not (callSiteEq nearestCallSite alloc.allocSite) ->
                stdout.Write "    "
                let withConfig c =
                    { c with
                        point = nearestCallSite.sequencePoint.Value
                        site = nearestCallSite.body.Method
                        operation = nearestCallSite.instruction
                        color = C.Yellow
                    }
                lprintfn withConfig "warning: %s" <| sprintf (sr.NearestCallSiteIsHere()) m.Name
            | _ -> ()

            stdout.Write "    "
            let withConfig c =
                { c with
                    point = alloc.allocSite.sequencePoint.Value
                    site = alloc.allocSite.body.Method
                    operation = alloc.allocSite.instruction
                    color = C.Yellow
                }
            lprintfn withConfig "warning: %s" <| sprintf (sr.ActualAllocationOpcodeIsHere()) m.Name

        if not <| Seq.isEmpty rest then
            cprintfn (fun c -> { c with fore = C.DarkGray }) "    ..."

let defaultVaridateConfig = {
    verifyAttributes = true
    outputLevel = OutputLevel.Warning
    searchDirectories = []
}

let varidateAllNoGcAllocations withConfig (asm: AssemblyDefinition) =
    let config = withConfig defaultVaridateConfig
    let cache = Dictionary()
    asm.Methods
    |> Seq.choose (fun m ->
        noGcAllocationConfigFromMethod config m
        |> Option.map (fun config -> m, config)
    )
    |> Seq.iter (fun (m, allocConfig) ->
        printNoGcAllocation cache config allocConfig m
    )

let varidateAssemblies withConfig assemblyPaths =
    let config = withConfig defaultVaridateConfig

    for path in Seq.distinct assemblyPaths do
        readAssembly (fun c -> { c with searchDirs = config.searchDirectories }) path
        |> varidateAllNoGcAllocations (fun _ -> config)


lazy
    let asm = typeof<int>.Assembly.Location |> readAssembly id

    let createInstanceM =
        asm.GetType("System.Activator").Methods
        |> Seq.filter (fun m -> m.Name = "CreateInstance" && m.HasParameters && m.Parameters.Count = 2)
        |> Seq.item 2

    printNoGcAllocation (Dictionary()) defaultVaridateConfig {
        outputLevel = OutputLevel.Info
        allocatableTypes = []
        treatAsNoAllocation = false
        } createInstanceM

//asm.MainModule
//m.ReturnType.Module.AssemblyReferences
//    m.Body
//    m.Body.Instructions
//)
//|> Seq.map (fun i ->
//    i.OpCode.Code
//    Code.Box
//    Code.Call
//    Code.Calli
//    Code.Callvirt
//    Code.Constrained
//    Code.Cpobj
//)
