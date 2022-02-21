[<AutoOpen>]
module Scratch.Detranspiler.Environments
open System
open System.Collections.Generic
open System.Diagnostics
open System.Reflection
open System.Runtime.CompilerServices
open Scratch.Primitives
open Scratch.Primitives.Document
open Scratch.Reflection
open Scratch.Ast
open Scratch.AstDefinitions
open FSharpFakeFriends

module E = Quotations.Patterns
type private B = System.Reflection.BindingFlags


[<Struct>]
type TVar = TVar of int

[<RequireQualifiedAccess>]
type VTsType =
    | Named of name: string * VTsType list
    /// String singleton types;
    /// empty = never
    | StringSs of string list
    | Or of VTsType * VTsType

    /// `?a`
    | Var of name: string * v: TVar
    
module VTsType =
    let types = {
        tNumber = VTsType.Named("number", [])
        tString = VTsType.Named("string", [])
        tBoolean = VTsType.Named("boolean", [])
        tUnit = VTsType.Named("()", [])
    }
    
type private V = VTsType

[<Struct>]
type GlobalName = GlobalName of string list * string
module GlobalName =
    let singleton name = GlobalName([], name)
    let child (GlobalName(ns, n)) name = GlobalName(ns @ [n], name)
    let parent = function
        | GlobalName([], _) -> None
        | GlobalName(x::xs, _) ->
            let rec unLast = function
                | x, [] -> [], x
                | x1, x2::xs ->
                    let xs, l = unLast (x2, xs)
                    x1::xs, l
            Some(GlobalName(unLast (x, xs)))
[<Struct>]
type LocalName = LocalName of string list * string
type LongNameKind = Global | Local
[<Struct>]
type LongName = LongName of string list * string
module LongName =
    let ofGlobalName (GlobalName(xs, x)) = LongName(xs, x)
    let ofLocalName (LocalName(xs, x)) = LongName(xs, x)

type DetranspileErrorInfo =
    | UnknownOperation of operator: string
    | UnexpectedOperandType of operator: string * opeandIndex: int * expectedOperandType: V OperandType
    | UnexpectedBlock
    | VariableNotFound of varName: string
    | ProcedureNotFound of name: string
    | ParameterNotFound of name: string
    | ListNotFound of name: string
    | SpriteNotFound of name: string

    | ParameterCountDifference of procName: string

    | NotImplementedExpression of string ComplexExpression
    | NotImplementedStatement of string ComplexExpression
    | NotImplementedListener of string ListenerDefinition

type DetranspileError<'a> = DetranspileError of location: 'a * info: DetranspileErrorInfo * traceOrNull: StackTrace

[<MethodImpl(MethodImplOptions.NoInlining)>]
let contextError location error =
    let t = StackTrace(skipFrames = 1, fNeedFileInfo = true)
    DetranspileError(location, error, t) |> Context.error

// TODO:
let buildError error = $"%A{error}"
let buildDetranspileExceptionMessage (DetranspileError(location, error, stack)) =
    let stack =
        match stack with
        | null -> ""
        | st -> $"--->\r\n{st}\r\n<---\r\n"

    let locationText = location
    $"{stack}{locationText}error: {buildError error}"

exception DetranspileException of Location: string * Info: DetranspileErrorInfo * TraceOrNull: StackTrace
with
    override x.Message = buildDetranspileExceptionMessage (DetranspileError(x.Location, x.Info, x.TraceOrNull))

type UnifyErrorInfo =
    | DifferenceType of V * V
    | MonomorphismRestriction of TypeScheme
    | InfiniteType of varName: string * TVar * V

type DetranspileWarningInfo =
    | UnifyWarning of UnifyErrorInfo
    | ErrorWarning of DetranspileErrorInfo

[<Struct>]
type DetranspileWarning<'a> = DetranspileWarning of location: 'a * DetranspileWarningInfo

[<Struct>]
type NonEmptyList<'a> = NonEmptyList of 'a * 'a list with
    interface 'a seq with
        override self.GetEnumerator() =
            let (NonEmptyList(x, xs)) = self
            let mutable current = x
            let mutable list = xs
            let mutable state = 0
            {
                new System.Collections.Generic.IEnumerator<_> with
                    override _.Current = current
                    override _.Current = box current
                    override _.MoveNext() =
                        match state with
                        | 0 ->
                            state <- 1
                            true

                        | 1 ->
                            match list with
                            | x::xs ->
                                current <- x
                                list <- xs
                                true

                            | [] ->
                                state <- -1
                                false
                        | _ ->
                            false

                    override _.Reset() =
                        current <- x
                        list <- xs
                        state <- 0

                    override _.Dispose() = ()


            }
    interface System.Collections.IEnumerable with
        override self.GetEnumerator() =
            (self :> _ seq).GetEnumerator() :> System.Collections.IEnumerator

type nel<'a> = 'a NonEmptyList
module Nel =
    let singleton x = NonEmptyList(x, [])
    let map f (NonEmptyList(x, xs)) = NonEmptyList(f x, List.map f xs)
    let toSeq (NonEmptyList _ as xs) = xs :> _ seq
    let head (NonEmptyList(x, _)) = x
    let tail (NonEmptyList(_, xs)) = xs

[<Struct>]
type Memo<'k,'v> = Memo of System.Collections.Concurrent.ConcurrentDictionary<'k,'v>
let makeMemoWith comparer =
    Memo(new System.Collections.Concurrent.ConcurrentDictionary<_,_>(concurrencyLevel = 1, collection = Seq.empty, comparer = comparer))
let makeMemo() = makeMemoWith LanguagePrimitives.FastGenericEqualityComparer

let memoizeFixEnv f =
    let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
    let mutable memo: System.Collections.Concurrent.ConcurrentDictionary<_,_> = null
    let mutable func = null
    let fix x = memo.GetOrAdd(x, valueFactory = func)
    func <- Func<_,_>(fun x -> f.Invoke(fix, x))
    fun (Memo m) x -> memo <- m; fix x

let memoizeEnv f (Memo m) x = m.GetOrAdd(x, valueFactory = Func<_,_>(f))

let inline skip _ = Context.error None

let typeNames = memoizeFixEnv <| fun typeNames (t: Type) ->
    let nameSpace = function
        | null
        | "" -> []
        | ns when 0 <= ns.IndexOf Type.Delimiter -> ns.Split Type.Delimiter |> Array.toList
        | ns -> [ns]

    match t.DeclaringType with
    | null ->
        let ns = nameSpace t.Namespace
        let n = t.Name
        let g = GlobalName(ns, n)
        if not t.IsGenericType then Nel.singleton g
        else
            let arity = t.GetGenericArguments().Length
            let suffix = "`" + string arity
            if n.EndsWith suffix then
                let n' = n[0 .. n.Length - 1 - suffix.Length]
                NonEmptyList(GlobalName(ns, n'), [g])
            else
                Nel.singleton g

    | dt ->
        if dt.IsGenericType then failwith $"generic declaring type: %A{dt}, %A{t}" else
        typeNames dt |> Nel.map (fun n -> GlobalName.child n t.Name)

[<Struct>]
type Val = {
    logicalName: string
    compiledName: string
}
[<Struct>]
type Entity = {
    logicalName: string
    compiledName: string

    compiledNameToVals: Map<string, Val>
    compiledNameToEntities: Map<string, Entity>
}
let rec toEntity e =
    let t = Entity.moduleOrNamespaceType e
    let vals =
        ModuleOrNamespaceType.allValsAndMembers t
        |> Seq.map (fun v ->
            let v = {
                compiledName = Val.compiledName v
                logicalName = Val.logicalName v
            }
            v.compiledName, v
        )
        |> Map.ofSeq

    let entities =
        ModuleOrNamespaceType.allEntities t
        |> Seq.map (fun e -> let e = toEntity e in e.compiledName, e)
        |> Map.ofSeq
    {
        compiledName = Entity.compiledName e
        logicalName = Entity.logicalName e

        compiledNameToVals = vals
        compiledNameToEntities = entities
    }

let readAssemblyEntity = memoizeEnv <| fun path -> usingFSharpAssembly path <| fun cs ->
    cs |> List.map (PickledCcuInfo.mspec >> toEntity)

let rec typeCompiledName (t: Type) =
    let nameSpace = function
        | null
        | "" -> []
        | ns when 0 <= ns.IndexOf Type.Delimiter -> ns.Split Type.Delimiter |> Array.toList
        | ns -> [ns]

    match t.DeclaringType with
    | null ->
        let ns = nameSpace t.Namespace
        let n = t.Name
        ns, n

    | dt ->
        if dt.IsGenericType then failwith $"generic declaring type: %A{dt}, %A{t}" else
        let dns, dn = typeCompiledName dt
        dns @ [dn], t.Name

let memberCompiledName (m: MemberInfo) =
    let tns, tn = typeCompiledName m.DeclaringType
    tns @ [tn], m.Name

type ResolveEnv = {
    assemblyEntityMemo: Memo<string, Entity list>
    typeNameMemo: Memo<Type, GlobalName nel>
}

let memberLogicalName env m =
    let compiledName = memberCompiledName m
    readAssemblyEntity env.assemblyEntityMemo m.DeclaringType.Assembly.Location
    |> Seq.choose (fun asm ->
        let rec query compiledName entity =
            match compiledName with
            | [], name ->
                entity.compiledNameToVals
                |> Map.tryFind name

            | nsHead::ns, n ->
                let e =
                    entity.compiledNameToEntities
                    |> Map.tryFind nsHead

                match e with
                | ValueNone -> ValueNone
                | ValueSome e -> query (ns, n) e

        match query compiledName asm with
        | ValueNone -> None
        | ValueSome v -> Some v.logicalName
    )
    |> Seq.tryHead
    
let tryGetCompilationSourceName (m: MemberInfo) =
    m.CustomAttributes
    |> Seq.tryPick (fun a ->
        if a.AttributeType = typeof<CompilationSourceNameAttribute> then
            a.ConstructorArguments[0].Value :?> string |> Some
        else None
    )

let tryGetCompiledName (m: MemberInfo) =
    m.CustomAttributes
    |> Seq.tryPick (fun a ->
        if a.AttributeType = typeof<CompiledNameAttribute> then
            a.ConstructorArguments[0].Value :?> string |> Some
        else None
    )

let memberSourceName env m =
    let sourceName = tryGetCompilationSourceName m
    let sourceName =
        match sourceName with
        | Some _ -> sourceName
        | None ->

        match m.MemberType with
        | MemberTypes.Property ->
            match tryGetCompiledName m with
            | Some n when n = m.Name -> memberLogicalName env m
            | _ -> None
        | _ -> None

    sourceName |> Option.defaultValue m.Name

let memberNames env m =
    let sourceName = memberSourceName env m
    match m.DeclaringType with
    | null -> Nel.singleton <| GlobalName([], sourceName)
    | t ->
        typeNames env.typeNameMemo t
        |> Nel.map (fun n -> GlobalName.child n sourceName)

let findNames env e =
    e
    |> Expr.tryPick (function
        | E.Call(_, m, _) -> Some <| memberNames env m
        | E.PropertyGet(_, p, _)
        | E.PropertySet(_, p, _, _) -> Some <| memberNames env p
        | _ -> None
    )
    |> Option.defaultWith (fun _ -> invalidArg "e" "e.g. <@ id @>")

let findName env e = findNames env e |> Nel.head
let typeName env t = typeNames env t |> Nel.head
let memberName env m = memberNames env m |> Nel.head

[<RequiresExplicitTypeArguments>]
let nameOf<'T> env = typeName env.typeNameMemo typeof<'T>

[<RequireQualifiedAccess>]
type Precedence =
    /// `10` | `()` | `System.DateTime.Now` | `List.empty[0]`
    | Primitive
    /// `f x`
    | Application
    /// `f(x)` | `self.F()`
    | Call
    /// `x **op y`
    | Pown
    /// `x *op y` | `x /op y` | `x %op y`
    | Mul
    /// `x -op y` | `x +op y`
    | Add
    /// `x !=op y` | `x <op y` | `x >op y` | `x = y` | `x |op y` | `x &op y`, 
    | Relation
    /// `x <- v`
    | VarSet
    /// `lazy x`
    | Lazy
    /// `do x` | `do! x`
    | Do
    /// `if x then y` | `if x then y else z`
    | If
    /// `x; y`
    | Sequence
    /// `f<ts>` ...
    | Expression

type Typed<'a> = (struct('a * TypeScheme))

[<Struct>]
type ProcedureSpec<'a> = {
    uniqueName: string
    procedure: 'a Typed ProcedureDefinition
}
[<Struct>]
type ValueSpec = {
    uniqueName: string
}
[<Struct>]
type MemberSpec = {
    alternativeNames: LocalName Set
}

[<Struct>]
type SpriteSpec = {
    uniqueName: string
}

type Spec<'a> =
    | ValueSpec of ValueSpec
    | ProcedureSpec of 'a ProcedureSpec
    | MemberSpec of MemberSpec
    | SpriteSpec of SpriteSpec
    | OpenedMemberSpec

[<Struct>]
type SourceSpecs<'a> = {
    fsNamespace: Map<LongName, 'a Spec>

    variableOrListSpecs: Map<string, ValueSpec>
    procedureSpecs: Map<string, 'a ProcedureSpec>
    parameterSpecs: Map<string, ValueSpec>
    spriteSpecs: Map<string, SpriteSpec>
}
[<Struct>]
type SpriteEnv = {
    selfName: Document
}

type Plugin<'a> = {
    expression: PluginProcess<'a,'a Typed ComplexExpression>
    statement: PluginProcess<'a,'a Typed ComplexExpression>
    listener: PluginProcess<'a,'a Typed ListenerDefinition>
    globalNames: GlobalName seq
}
and PluginProcess<'a,'t> = 't -> Context<'a ExpressionEnv, unit, 'a DetranspileError option, struct(Document * Precedence)>
and DetranspileConfig<'a> = {
    showLocation: 'a -> string
    plugin: 'a Plugin
    renderConfig: Document.Config
    ignoreInitialListValues: bool
    treatStringAsNumberIfPossible: bool
    treatStringAsBoolIfPossible: bool
}
and DetranspileEnv<'a> = {
    config: 'a DetranspileConfig
    warnings: 'a DetranspileWarning ResizeArray
    resolveEnv: ResolveEnv
}
and PrettyEnv<'a> = {
    prettyEnv: 'a DetranspileEnv
    specs: 'a SourceSpecs

    /// `self` in `type S(...) as self = ...`
    self: SpriteEnv option
    entryPoint: Document
    primTypes: TsType PrimitiveTypes
}
and [<Struct>] ExpressionEnv<'a> = {
    expressionEnv: 'a PrettyEnv
    atomicity: Atomicity
    precedence: Precedence
}

let registerGlobalNames names specs =
    names
    |> Seq.fold (fun specs name ->
        { specs with fsNamespace = Map.add (LongName.ofGlobalName name) (MemberSpec { alternativeNames = Set.empty }) specs.fsNamespace }
    ) specs

let coreOpenNames env =
    let members = [
        <@@ global.Scratch.SListOperations.defineList @@>
        <@@ global.Scratch.PseudoAttributes.attributes @@>
        <@@ global.Scratch.AtomicGeneratorBuilderOperations.atomic @@>
        <@@ global.Scratch.GeneratorBuilderOperations.tightrope @@>
        <@@ global.Scratch.Control.whenGreenFlag @@>
        <@@ global.Scratch.PrimitiveOperations.forever @@>
        <@@ global.FSharp.Core.Operators.int @@>
        <@@ global.FSharp.Core.ExtraTopLevelOperators.double @@>
    ]
    let types = [
        typeof<global.FSharp.Core.ReflectedDefinitionAttribute>
        typeof<global.Scratch.CostumeAttribute>
    ]

    Seq.append (Seq.collect (findNames env) members) (Seq.collect (typeNames env.typeNameMemo) types)
    |> Seq.choose GlobalName.parent
    |> Set.ofSeq
    |> Set.toSeq

let moduleNames env t = seq {
    for GlobalName(ns, n) as name in typeNames env.typeNameMemo t do

        // TODO:
        let moduleP = "Module"
        if n.EndsWith moduleP then
            GlobalName(ns, n[0..n.Length-1-moduleP.Length])
        else
            name

    for p in t.GetProperties(B.Static ||| B.Public ||| B.DeclaredOnly) do
        yield! memberNames env p

    // TODO: skip extension methods

    for m in t.GetMethods(B.Static ||| B.Public ||| B.DeclaredOnly) do
        if m.IsSpecialName && (m.Name.StartsWith "get_" || m.Name.StartsWith "set_") then ()
        else
            for GlobalName(ns, n) as name in memberNames env m do
                name

                // let (|...|...|) = ...
                if 2 < n.Length && n[0] = '|' && n[n.Length-1] = '|' then
                    for pn in n.Split '|' do
                        if pn <> "_" then
                            GlobalName(ns, pn)
}
let unionNames env cases t = seq {
    // namespace global
    // type Maybe = Nothing | Just of int
    //
    // (*
    // global.Maybe
    // global.Maybe.Nothing
    // global.Maybe.Just
    // global.Nothing
    // global.Just
    // *)
    let names = typeNames env.typeNameMemo t
    yield! names
    for c: Reflection.UnionCaseInfo in cases do
        let cn = c.Name
        for tn in names do
            GlobalName.child tn cn

            match GlobalName.parent tn with
            | None -> GlobalName.singleton cn
            | Some parent -> GlobalName.child parent cn
}
let otherTypeNames env t = seq {
    yield! typeNames env.typeNameMemo t
    for p in t.GetProperties(B.Static ||| B.Public ||| B.DeclaredOnly) do
        yield! memberNames env p

    for m in t.GetMethods(B.Static ||| B.Public ||| B.DeclaredOnly) do
        yield! memberNames env m
}
let assemblyGlobalNames env (asm: Assembly) =
    asm.ExportedTypes
    |> Seq.collect (function
        | ModuleType _ as t -> moduleNames env t
        | RecordType _ as t -> typeNames env.typeNameMemo t |> Nel.toSeq

        // public nested type Choice<_,_>.Choice1Of2<_,_>
        | UnionType _ & DeclaringType(UnionType _) -> Seq.empty

        | UnionType cases as t -> unionNames env cases t
        | ExceptionRepresentationType _ as t -> typeNames env.typeNameMemo t |> Nel.toSeq

        // public nested type option<_>.Tags
        | DeclaringType(UnionType _) -> Seq.empty

        | t -> otherTypeNames env t
    )
    |> Seq.distinct

let systemGlobalNames env =
    [
        typeof<global.System.Object>
        typeof<global.System.String>
        typeof<global.System.Int32>
        typeof<global.System.Double>
    ]
    |> Seq.collect (typeNames env.typeNameMemo >> Nel.toSeq)

let coreGlobalNames env = assemblyGlobalNames env typeof<Scratch.Sprite>.Assembly
let fsharpCoreGlobalNames env = assemblyGlobalNames env typeof<unit>.Assembly

let registerCoreGlobalNames env specs =
    registerGlobalNames (systemGlobalNames env) specs
    |> registerGlobalNames (fsharpCoreGlobalNames env)
    |> registerGlobalNames (coreGlobalNames env)
    
let freshLongName baseName specs =
    let enumName n i = n + "'" + string i
    if Map.containsKey (LongName([], baseName)) specs.fsNamespace then
        let rec aux i =
            let newName = enumName baseName i
            if Map.containsKey (LongName([], newName)) specs.fsNamespace then
                aux (i + 1)
            else
                newName
        aux 1
    else
        baseName

let registerVarOrList name specs =
    let uniqueName = freshLongName name specs
    let spec = { ValueSpec.uniqueName = uniqueName }
    let specs =
        { specs with
            fsNamespace = Map.add (LongName([], uniqueName)) (ValueSpec spec) specs.fsNamespace
            variableOrListSpecs = Map.add name spec specs.variableOrListSpecs
        }
    spec, specs

let registerParameter (ParameterDefinition(name = name)) specs =
    let uniqueName = freshLongName name specs
    let spec = { ValueSpec.uniqueName = uniqueName }
    let specs =
        { specs with
            fsNamespace = Map.add (LongName([], uniqueName)) (ValueSpec spec) specs.fsNamespace
            parameterSpecs = Map.add name spec specs.parameterSpecs
        }
    spec, specs

let registerParameters ps specs = List.fold (fun specs p -> registerParameter p specs |> snd) specs ps

let formatRegex = System.Text.RegularExpressions.Regex @"\s*(?<!\\)%.\s*"
let escapeRegex = System.Text.RegularExpressions.Regex @"\\(.)"

let registerDataSpecs data specs =
    let registerVar data specs = registerVarOrList data.name specs
    let registerList data specs = registerVarOrList data.listName specs
    let registerProcedure (ProcedureDefinition(name = name) as p) specs =

        // 名前の型情報 ( `%s` など ) を削除
        let varName = escapeRegex.Replace(formatRegex.Replace(name, ""), "$1")
        let uniqueName = freshLongName varName specs

        let spec = { uniqueName = uniqueName; procedure = p }
        let specs =
            { specs with
                fsNamespace = Map.add (LongName([], uniqueName)) (ProcedureSpec spec) specs.fsNamespace
                procedureSpecs = Map.add name spec specs.procedureSpecs
            }
        spec, specs

    let specs = List.fold (fun specs d -> registerVar d specs |> snd) specs data.variables
    let specs = List.fold (fun specs d -> registerList d specs |> snd) specs data.lists
    let specs =
        data.scripts
        |> List.fold (fun specs d ->
            match d with
            | { script = Procedure d } -> registerProcedure d specs |> snd
            | _ -> specs
        ) specs
    specs

let registerSpriteSpec sprite specs =
    let name = sprite.objName
    let uniqueName = freshLongName name specs
    let spec = { SpriteSpec.uniqueName = uniqueName }
    { specs with
        fsNamespace = Map.add (LongName([], uniqueName)) (SpriteSpec spec) specs.fsNamespace
        spriteSpecs = Map.add name spec specs.spriteSpecs
    }

let addAlternativeName shortName name s map = Map.add name (MemberSpec { s with alternativeNames = Set.add shortName s.alternativeNames }) map
let openName specs openName =
    let rec opened openName name =
        match openName, name with
        | GlobalName([], on), LongName(ns::nss, n) ->
            if on = ns then Some(LocalName(nss, n))
            else None

        | GlobalName(ons::onss, on), LongName(ns::nss, n) ->
            if ons = ns then opened (GlobalName(onss, on)) (LongName(nss, n))
            else None

        | GlobalName([], _), LongName([], _)
        | GlobalName(_::_, _), LongName([], _) -> None

    let addOpened map name = function
        | ValueSpec _
        | ProcedureSpec _
        | SpriteSpec _
        | OpenedMemberSpec -> map
        | MemberSpec s ->
            match opened openName name with
            | None -> map
            | Some shortName ->
                let map = addAlternativeName shortName name s map
                Map.add (LongName.ofLocalName shortName) OpenedMemberSpec map

    { specs with fsNamespace = specs.fsNamespace |> Map.fold addOpened specs.fsNamespace }

let openNames names specs = Seq.fold openName specs names

let inline lookupCore getMap error location name = context {
    let! env = Context.environment
    match Map.tryFind name <| getMap env.specs with
    | ValueSome s -> return s
    | ValueNone -> return! contextError location <| error name
}
let lookupVariableOrList location varName =
    lookupCore (fun s -> s.variableOrListSpecs) VariableNotFound location varName

let lookupParameter location paramName =
    lookupCore (fun s -> s.parameterSpecs) ParameterNotFound location paramName

let lookupProcedure location procedureName =
    lookupCore (fun s -> s.procedureSpecs) ProcedureNotFound location procedureName

let lookupSprite location spriteName =
    lookupCore (fun s -> s.spriteSpecs) SpriteNotFound location spriteName

module Graph =
    let get (xs: _ ResizeArray) i = xs[i]
    let set (xs: _ ResizeArray) i v = xs[i] <- v
    let add (xs: _ ResizeArray) x = xs.Add x
    let count (xs: _ ResizeArray) = xs.Count

    let rec visit g v scc (S: _ Stack) inS low num (time: _ byref) =
        time <- time + 1
        set low v time
        set num v time
        S.Push v
        set inS v true
        for e in get g v do
            let w = e
            if num[w] = 0 then
                visit g w scc S inS low num &time
                low[v] <- min low[v] low[w]
            elif inS[w] then
                low[v] <- min low[v] num[w]

        if low[v] = num[v] then
            add scc (ResizeArray())
            while
                begin
                    let w = S.Pop()
                    inS[w] <- false
                    scc[scc.Count-1].Add(w)
                    v <> w
                end
                do ()
            
    let stronglyConnectedComponents' g scc =
        let n = count g
        let num = ResizeArray(Seq.replicate n 0)
        let low = ResizeArray(Seq.replicate n 0)
        let S = Stack()
        let inS = ResizeArray(Seq.replicate n false)
        let mutable time = 0
        for u in 0..n-1 do
            if num[u] = 0 then
                visit g u scc S inS low num &time

    let stronglyConnectedComponents g =
        let keyToIndex = g |> Seq.mapi (fun i (k, _) -> k, i) |> Map.ofSeq
        let indexToEntry = g |> ResizeArray
        let g' = g |> Seq.map (fun (_, (_, es)) -> es |> Seq.map (fun e -> keyToIndex[e]) |> ResizeArray) |> ResizeArray

        let scc = ResizeArray()
        stronglyConnectedComponents' g' scc
        scc |> Seq.map (Seq.map (get indexToEntry))

module PluginProcess =
    let merge p1 p2 x envState =
        match p1 x envState with
        | Ok xState -> Ok xState

        // skip
        | Error None -> p2 x envState

        | Error(Some _) as e1 ->
            match p2 x envState with
            | Error None -> e1
            | r -> r

    let ofAssoc getKey assoc =
        let map = Map assoc
        fun e ->
            match Map.tryFind (getKey e) map with
            | ValueNone -> skip()
            | ValueSome p -> p e

    let ofComplexExpressionAssoc assoc = ofAssoc (fun (ComplexExpression(operator = operator)) -> operator) assoc
    let ofListenerAssoc assoc = ofAssoc (fun (ListenerDefinition(name = name)) -> name) assoc

module Plugin =
    open PluginProcess

    let empty = {
        expression = skip
        statement = skip
        listener = skip
        globalNames = Seq.empty
    }
    let merge p1 p2 = {
        expression = merge p1.expression p2.expression
        statement = merge p1.statement p2.statement
        listener = merge p1.listener p2.listener
        globalNames = Seq.append p1.globalNames p2.globalNames |> Seq.distinct
    }
