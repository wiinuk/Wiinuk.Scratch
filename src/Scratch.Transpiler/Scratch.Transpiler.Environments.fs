[<AutoOpen>]
module Scratch.Transpiler.Environments
open System
open System.Diagnostics
open System.Reflection
open FSharp.Reflection
open FSharp.Quotations
open Scratch
open Scratch.Ast
open Scratch.AstDefinitions
open Scratch.Ast.Transformers
open Scratch.IR
open Scratch.MemoryModel
open Scratch.Primitives
open Scratch.Primitives.Field
open Scratch.Reflection
open Scratch.Reflection.Expr
open Scratch.Reflection.Transformers
open Scratch.Transformers
open Scratch.Transpiler
open System.Collections.Generic

module E = Quotations.Patterns
module E = Quotations.DerivedPatterns
module R = Scratch.Transformers.TransformResult
type private B = System.Reflection.BindingFlags
type private QVar = Quotations.Var

[<Struct>]
type ParameterSpec = {
    parameterIRVar: Var
    parameterVar: QVar
}

[<Struct>]
type ListSpec = {
    listVar: SimpleVar
}
[<Struct>]
type Export = Export | NoExport
type ProcedureSpec = {
    procedureVar: ProcedureVar

    procedureThis: QVar option
    parameters: ParameterSpec list
    atomicity: Atomicity

    isExport: Export
    inlining: InliningOptions option
}

type SpriteSpec = {
    spriteUniqueName: string

    this: QVar
    selfField: FieldInfo option
    spriteSource: SourceCode

    items: Expr
}

type VariableSpec = {
    var: Var
    export: Export
    persistence: Persistence
}

type Spec =
    | ProcedureSpec of ProcedureSpec
    | ListSpec of ListSpec
    | VarSpec of VariableSpec
    | SpriteSpec of SpriteSpec

type Namespace = Map<string, unit>
let uniqueName indexedName baseName v map =
    let rec aux i n =
        if Map.containsKey n map then
            aux (i + 1) (indexedName baseName i)
        else
            n, Map.add n v map
    aux 1 baseName

[<DebuggerHidden>]
let (^^) x xs = { head = x; tail = xs }

type Id =
    | VarId of QVar
    | MethodId of MethodId
    | PropertyId of PropertyId
    | TypeId of TypeId
    | FieldId of FieldId
    | UnionCaseId of UnionCaseId

[<Struct>]
type State = {
    data: EntityData<unit, Loc Top, Loc VariableDecl, Loc ListDef, Loc>
    spriteNamespace: Namespace
}

let typeId s = TypeId (MemberId.typeId s)
let methodId m = MethodId (MemberId.methodId m)
let propertyId p = PropertyId (MemberId.propertyId p)
let fieldId f = FieldId (MemberId.fieldId f)
let unionCaseId u = UnionCaseId (MemberId.unionCaseId u)

[<AutoOpen>]
module Error =
    open Format
    open System.IO

    type TranspileErrorInfo =
        | InvalidProcedureParameterType of param: QVar
        | InvalidVarType of var: QVar
        | InvalidProcedureResultType of Type
        | InvalidPropertyType of PropertyInfo
        | InvalidExpressionType of Type

        | InvalidExpressionForm

        | ProcedureNotFound of id: Id
        | ListNotFound of id: Id
        | VarNotFound of var: QVar
        | PropertyNotFound of property: PropertyInfo
    
        | ParameterCountMismatch of name: string
        | TypeSizeMismatch of expected: int * actual: int

        | StringLiteralCanNotContainNewLine

        | InvalidInterpolationCall of StringInterpolationError

        | InvalidListPushType of Type

        | MethodNotFound of MethodInfo
        | UnsupportedFormatCall of MethodInfo

        | RequireAtomicity of Atomicity

        | InvelidImplicitConvertionType of fromType: Type * toType: Type
        | MethodCallWithoutReflectedDefinition of MethodInfo

        | InvalidGeneratorExpressionForm
        | InvalidSpriteImportForm
        | InvalidClassDefinition of errorMessage: string * classInit: Expr
        | InvalidSpriteDefinitionForm

        | InvalidFieldType of FieldInfo
        | FieldNotFound of FieldInfo

        | InvalidUnionType of UnionCaseInfo

        | MethodGenericInstanceTooMany of genericMethodDefinition: MethodInfo * count: int

        | StringLiteralExpressionOnly

        | RequireVariableExpression

        | SpecNotFound of Id

        | RequireMutableStructHome of home: Expr
        | RequireListOrVar
        | StorageVarNotFound of Id

        | WaitUntilAsyncConditionTooComplicated

        | InvalidPropertyPersistence of PropertyInfo * Persistence
        | PersistentVariablesMustBeWritable of PropertyInfo

    [<Struct>]
    type TranspileError = TranspileError of info: TranspileErrorInfo * source: SourceCode * stackTrace: StackTrace option

    let buildSourceTextCore p1 p2 fileLines =
        fileLines
        |> Seq.mapi (fun i line -> struct(i + 1, line))
        |> Seq.filter (fun struct(l, _) -> p1.line - 1 <= l && l <= p2.line + 1)
        |> Seq.collect (fun struct(lineNumber, line) ->
            let lineWithNum = $"%4d{lineNumber} | {line}"
            if p1.line <= lineNumber && lineNumber <= p2.line then
                let columnMin = if lineNumber = p1.line then p1.column + 1 else 1
                let columnMax = if lineNumber = p2.line then p2.column else String.length line
                let underLine = String.replicate (columnMin - 1) " " + String.replicate (columnMax - columnMin + 1) "^"
                let messageLine = $"     | {underLine}"
                [|
                    lineWithNum
                    messageLine
                |]
            else
                [|lineWithNum|]
        )

    let locationText = function
        | None -> None
        | Some { path = path; position1 = { line = line; column = column } } -> $"{path}({line},{column})" |> Some

    let buildSourceText = function
        | None -> None
        | Some l ->
            try File.ReadLines l.path |> buildSourceTextCore l.position1 l.position2 |> Some
            with _ -> None

    // TODO:
    let buildError error = $"%A{error}"

    let transpileErrorNumber =
        let tag = Reflection.FSharpValue.PreComputeUnionTagReader typeof<TranspileErrorInfo>
        fun (e: TranspileErrorInfo) -> tag e + 1

    let buildTranspileExceptionMessage (TranspileError(error, code, stack)) =
        let location = SourceCode.location code

        let stack =
            match stack with
            | None -> ""
            | Some st -> $"--->\r\n{st}\r\n<---\r\n"
                
        let errorNumber = transpileErrorNumber error
        let locationText = locationText location |> Option.map (fun s -> s + ": ") |> Option.defaultValue ""
        let exprText = $"\r\nexpr: {SourceCode.buildErrorText code}"

        let sourceText =
            match buildSourceText location with
            | None -> ""
            | Some source -> source |> String.concat "\r\n" |> sprintf "\r\n%s"

        sprintf "%s%serror SC%04d: %s%s%s" stack locationText errorNumber (buildError error) sourceText exprText

    exception TranspileException of TranspileError
    with
        override x.Message = buildTranspileExceptionMessage x.Data0

    let raiseError source error = raise <| TranspileException(TranspileError(error, source, None))

type PluginError = Error.TranspileError option

[<RequireQualifiedAccess>]
type OutputLevel =
    | Slient
    | Error
    | Warning
    | Info
    | Debug
    | Verbose

type IWithE<'Self,'Field> =
    abstract Field: 'Field
    abstract WithField: 'Field -> 'Self

[<Struct>]
type EFieldShape<'T,'F> when 'T :> IWithE<'T,'F> =    
    /// `e` field operations
    | FE
with
    interface IFieldShape<'T,'F> with
        member _.Get x = x.Field
        member _.With(x, v) = x.WithField v

type IWithConfig<'Self,'Field> =
    abstract Field: 'Field
    abstract WithField: 'Field -> 'Self
    
[<Struct>]
type ConfigFieldShape<'T,'F> when 'T :> IWithConfig<'T,'F> =
    /// `config` field operations
    | FConfig
with
    interface IFieldShape<'T,'F> with
        member _.Get x = x.Field
        member _.With(x, v) = x.WithField v

type IWithEnvironment<'Self,'Field> =
    abstract Field: 'Field
    abstract WithField: 'Field -> 'Self

[<Struct>]
type EnvironmentFieldShape<'T,'F> when 'T :> IWithEnvironment<'T,'F> =
    /// `environment` field operations
    | FEnvironment
with
    interface IFieldShape<'T,'F> with
        member _.Get x = x.Field
        member _.With(x, v) = x.WithField v

type IWithBlockEnvironment<'Self,'Field> =
    abstract Field: 'Field
    abstract WithField: 'Field -> 'Self
    
[<Struct>]
type BlockEnvironmentFieldShape<'T,'F> when 'T :> IWithBlockEnvironment<'T,'F> =
    /// `blockEnvironment` field operations
    | FBlockEnvironment
with
    interface IFieldShape<'T,'F> with
        member _.Get x = x.Field
        member _.With(x, v) = x.WithField v

type IWithItemEnvironment<'Self,'Field> =
    abstract Field: 'Field
    abstract WithField: 'Field -> 'Self

[<Struct>]
type ItemEnvironmentFieldShape<'T,'F> when 'T :> IWithItemEnvironment<'T,'F> =
    /// `itemEnvironment` field operations
    | FItemEnvironment
with
    interface IFieldShape<'T,'F> with
        member _.Get x = x.Field
        member _.With(x, v) = x.WithField v

type IWithState<'Self,'Field> =
    abstract Field: 'Field
    abstract WithField: 'Field -> 'Self

[<Struct>]
type StateFieldShape<'T,'F> when 'T :> IWithState<'T,'F> =
    /// `state` field operations
    | FState
with
    interface IFieldShape<'T,'F> with
        member _.Get x = x.Field
        member _.With(x, v) = x.WithField v

type IWithExternalItemState<'Self,'Field> =
    abstract Field: 'Field
    abstract WithField: 'Field -> 'Self

[<Struct>]
type ExternalItemStateFieldShape<'T,'F> when 'T :> IWithExternalItemState<'T,'F> =
    /// `externalItemState` field operations
    | FExternalItemState
with
    interface IFieldShape<'T,'F> with
        member _.Get x = x.Field
        member _.With(x, v) = x.WithField v

type IWithItemState<'Self,'Field> =
    abstract Field: 'Field
    abstract WithField: 'Field -> 'Self

[<Struct>]
type ItemStateFieldShape<'T,'F> when 'T :> IWithItemState<'T,'F> =
    /// `itemState` field operations
    | FItemState
with
    interface IFieldShape<'T,'F> with
        member _.Get x = x.Field
        member _.With(x, v) = x.WithField v

type IWithStageDataExtension<'Self,'Field> =
    abstract Field: 'Field
    abstract WithField: 'Field -> 'Self

[<Struct>]
type StageDataExtensionFieldShape<'T,'F> when 'T :> IWithStageDataExtension<'T,'F> =
    /// `stageDataExtension` field operators
    | FStageDataExtension
with
    interface IFieldShape<'T,'F> with
        member _.Get x = x.Field
        member _.With(x, v) = x.WithField v

[<Struct>]
type SE<'S,'E> = { s: 'S ref; e: 'E } with
    interface IWithE<SE<'S,'E>,'E> with
        member x.Field = x.e
        member x.WithField v = let x = x in { x with e = v }

type PluginProcess<'E,'S,'T,'R> = SE<'S,'E> -> 'T -> Result<'R, PluginError>

let inline compose c1 c2 e s =
    match c1 e s with
    | Ok _ as r -> r
    | Error None -> c2 e s
    | Error(Some _) as e1 ->
        match c2 e s with
        | Error None -> e1
        | r -> r
        
[<AbstractClass>]
type ComposableFunction<'Self>() =
    abstract Compose: 'Self -> 'Self
        
let (</>) (x: ComposableFunction<_>) y = x.Compose y

type EnvironmentRareUpdate = {
    sprite: SpriteSpec option
    memberTypeCache: Dictionary<Type, ExpType option>
}

[<Struct>]
type Environment = {
    vars: Map<Id, Spec>
    rare: EnvironmentRareUpdate
}
[<Struct>]
type BlockEnvironment = {
    proc: ProcedureSpec
}

[<Struct>]
type ItemState = {
    itemAcc: (Expr * Location option) list
}

[<Struct>]
type ItemEnvironment = {
    depth: int
    bySprite: SpriteSpec option
}

type ExternalSpec =
    | ExternalProcedure of ProcedureSpec * Expr
    | ExternalList of ListSpec * Expr
    | ExternalVariable of VariableSpec * Expr
    | ExternalSprite of SpriteSpec

type Config = {
    indexedName: (* baseName: *) string -> (* index: *) int -> string
    resultName: string
    localName: string
    namespaceSeparator: string
    lambdaName: string
    methodName: MethodInfo -> string
    propertyName: PropertyInfo -> string
    fieldName: FieldInfo -> string
    stackName: string
    tupleFieldName: int -> string
    spriteName: Type -> string
    typeName: Type -> string
    unionCaseName: UnionCaseInfo -> string
    unionCaseFieldName: UnionCaseFieldInfo -> string
    unionCaseTagName: string
    unionCaseUnifiedFieldName: string * string * string list -> string
    unionCaseUninitField: SValue

    outputLevel: OutputLevel
    plugin: Plugin
    maxGenericInstantiateCount: int

    ir: IRToStageData.Config
}
and Plugin = {
    preTransform: Config -> Expr -> Expr option
    /// <exception cref="Exception">Thrown when fetal error.</exception>
    expression: ExpressionPluginProcess
    /// <exception cref="Exception">Thrown when fetal error.</exception>
    item: ItemPluginProcess
    externalItems: ExternalItemsPluginProcess
    postTransform: Loc Plugins
}
and [<AbstractClass>] ExpressionPluginProcess() =
    inherit ComposableFunction<ExpressionPluginProcess>()
    override c1.Compose c2 = {
        new ExpressionPluginProcess() with
            member _.Invoke e s = compose c1.Invoke c2.Invoke e s
    }
    abstract Invoke: SE<'S,'E> -> Expr -> Result<Loc Exp, PluginError>
        when 'S :> IWithState<'S, State>
        
        and 'E :> IWithConfig<'E, Config>
        and 'E :> IWithEnvironment<'E, Environment>
        and 'E :> IWithBlockEnvironment<'E, BlockEnvironment>

and [<AbstractClass>] ItemPluginProcess() =
    inherit ComposableFunction<ItemPluginProcess>()
    override c1.Compose c2 = {
        new ItemPluginProcess() with
            member _.Invoke e s = compose c1.Invoke c2.Invoke e s
    }
    abstract Invoke: SE<'S,'E> -> Expr -> Result<unit, PluginError>
        when 'S :> IWithState<'S, State>
        and 'S :> IWithItemState<'S, ItemState>
        
        and 'E :> IWithConfig<'E, Config>
        and 'E :> IWithEnvironment<'E, Environment>

and [<AbstractClass>] ExternalItemsPluginProcess() =
    inherit ComposableFunction<ExternalItemsPluginProcess>()
    override c1.Compose c2 = {
        new ExternalItemsPluginProcess() with
            member _.Invoke e s = compose c1.Invoke c2.Invoke e s
    }
    abstract Invoke: SE<'S,'E> -> Expr -> Result<unit, PluginError>
        when 'S :> IWithState<'S, State>
        and 'S :> IWithEnvironment<'S, Environment>
        and 'S :> IWithExternalItemState<'S, ExternalItemState<'Envs>>
        and 'Envs :> IWithConfig<'Envs, Config>
        and 'Envs :> IWithEnvironment<'Envs, Environment>

        and 'E :> IWithConfig<'E, Config>
        and 'E :> IWithItemEnvironment<'E, ItemEnvironment>
        and 'E :> IWithEnvironment<'E, Environment>

and [<Struct>] Environments = { Environment: Environment; Config: Config } with
    interface IWithEnvironment<Environments, Environment> with
        member x.Field = x.Environment
        member x.WithField v = let x = x in { x with Environment = v }

    interface IWithConfig<Environments, Config> with
        member x.Field = x.Config
        member x.WithField v = let x = x in { x with Config = v }

and BlockEnvironments = { BlockEnvironment: BlockEnvironment; Environment: Environment; Config: Config } with
    interface IWithBlockEnvironment<BlockEnvironments, BlockEnvironment> with
        member x.Field = x.BlockEnvironment
        member x.WithField v = { x with BlockEnvironment = v }

    interface IWithEnvironment<BlockEnvironments, Environment> with
        member x.Field = x.Environment
        member x.WithField v = { x with Environment = v }

    interface IWithConfig<BlockEnvironments, Config> with
        member x.Field = x.Config
        member x.WithField v = { x with Config = v }

and ItemEnvironments<'Tail> when 'Tail :> IWithConfig<'Tail, Config> and 'Tail :> IWithEnvironment<'Tail, Environment> = { ItemEnvironment: ItemEnvironment; Tail: 'Tail } with
    interface IWithItemEnvironment<ItemEnvironments<'Tail>, ItemEnvironment> with
        member x.Field = x.ItemEnvironment
        member x.WithField v = { x with ItemEnvironment = v }

    interface IWithConfig<ItemEnvironments<'Tail>, Config> with
        member x.Field = get FConfig x.Tail
        member x.WithField v = { x with Tail = wiz FConfig v x.Tail }

    interface IWithEnvironment<ItemEnvironments<'Tail>, Environment> with
        member x.Field = get FEnvironment x.Tail
        member x.WithField v = { x with Tail = wiz FEnvironment v x.Tail }

and [<Struct>] MethodDefinitionSpec = { instantiatedMethodCount: int }
and ExternalItemState<'Environments> = {
    externalEnv: 'Environments
    externalState: State
    externalMethodDefinitions: Map<MethodId, MethodDefinitionSpec>
    externalAcc: ExternalSpec list
}

[<Struct>]
type ExternalItemStates<'Environments> when 'Environments :> IWithEnvironment<'Environments, Environment> = {
    ExternalItemState: ExternalItemState<'Environments>
}
with
    interface IWithExternalItemState<ExternalItemStates<'Environments>, ExternalItemState<'Environments>> with
        member x.Field = x.ExternalItemState
        member _.WithField v = { ExternalItemState = v }

    interface IWithState<ExternalItemStates<'Environments>, State> with
        member x.Field = x.ExternalItemState.externalState
        member x.WithField v = { ExternalItemState = { externalEnv = x.ExternalItemState.externalEnv; externalState = v; externalAcc = x.ExternalItemState.externalAcc; externalMethodDefinitions = x.ExternalItemState.externalMethodDefinitions } }

    interface IWithEnvironment<ExternalItemStates<'Environments>, Environment> with
        member x.Field = get FEnvironment x.ExternalItemState.externalEnv
        member x.WithField v = { ExternalItemState = { externalEnv = wiz FEnvironment v x.ExternalItemState.externalEnv; externalState = x.ExternalItemState.externalState; externalAcc = x.ExternalItemState.externalAcc; externalMethodDefinitions = x.ExternalItemState.externalMethodDefinitions } }

module ExternalItemStates =
    let make externalItemState = {
        ExternalItemState = externalItemState
    }

module Environments =
    let make(environment, tail) = {
        Environment = environment
        Config = get FConfig tail
    }

module BlockEnvironments =
    let make(blockEnvironment, tail) = {
        BlockEnvironment = blockEnvironment
        Environment = get FEnvironment tail
        Config = get FConfig tail
    }

module ItemEnvironments =
    let make(itemEnvironment, tail) = {
        ItemEnvironment = itemEnvironment
        Tail = tail
    }

[<Struct>]
type ExpressionStates<'Tail> when 'Tail :> IWithState<'Tail, State> = {
    Tail: 'Tail
}
with
    interface IWithState<ExpressionStates<'Tail>, State> with
        member x.Field = get FState x.Tail
        member x.WithField v = { Tail = wiz FState v x.Tail }

module ExpressionStates =
    let make tail = {
        Tail = tail
    }

[<Struct>]
type States = {
    State: State
}
with
    interface IWithState<States, State> with
        member x.Field = x.State
        member _.WithField v = { State = v }

module States =
    let make state = {
        State = state
    }

[<Struct>]
type ItemStates = {
    ItemState: ItemState
    State: State
}
with
    interface IWithItemState<ItemStates, ItemState> with
        member x.Field = x.ItemState
        member x.WithField v = { ItemState = v; State = x.State }

    interface IWithState<ItemStates, State> with
        member x.Field = x.State
        member x.WithField v = { ItemState = x.ItemState; State = v }

module ItemStates =
    let make (itemState, tail) = {
        ItemState = itemState
        State = get FState tail
    }

[<Struct>]
type SpriteStates<'a> = {
    StageDataExtension: StageDataExtension<'a Top,'a VariableDecl,'a ListDef,'a>
    State: State
}
with
    interface IWithStageDataExtension<SpriteStates<'a>, StageDataExtension<'a Top,'a VariableDecl,'a ListDef,'a>> with
        member x.Field = x.StageDataExtension
        member x.WithField v = { StageDataExtension = v; State = x.State }

    interface IWithState<SpriteStates<'a>, State> with
        member x.Field = x.State
        member x.WithField v = { StageDataExtension = x.StageDataExtension; State = v }

module SpriteStates =
    let make(stageDataExtension, state) = {
        StageDataExtension = stageDataExtension
        State = state
    }

let newEmptyEnv() = {
    vars = Map.empty
    rare = {
        sprite = None
        memberTypeCache = Dictionary()
    }
}

let emptyState data = {
    data = data
    spriteNamespace = Map.empty
}

let rec choice env state e = function
    | [] -> None
    | p::ps ->

    match p env state e with
    | Some _ as r -> r
    | None -> choice env state e ps

let inline localEnv senv mapEnv f = f { s = senv.s; e = mapEnv senv.e }

let inline run senv f =
    let r, state = f senv.e senv.s.contents
    senv.s.contents <- state
    r

let lookupSpec k env = Map.tryFind k (get FEnvironment env).vars
let lookupVar v env = lookupSpec (VarId v) env
let lookupMethod m env = lookupSpec (methodId m) env
let lookupProperty p env = lookupSpec (propertyId p) env
let lookupSprite s env = lookupSpec (typeId s) env
let lookupField f env = lookupSpec (fieldId f) env

let addExport e t = match e with Export -> Tagged.addDefault ExportTag t | NoExport -> t
let withExportTag l e = addExport e <| Tagged.empty l
let addInliningOptions v t = match v with Some v -> Tagged.add InliningOptionsTag v t | _ -> t

let registerProcedure varName parameterTypes =
    // ```
    // let ``%`` x y = x % y
    // let ``%`` x y = x % y
    // ```
    //
    // varName    = @"%"
    // baseName   = @"\%"
    // procName   = @"\% %n %n"
    // uniqueName = @"\% %n %n.1"
    let baseName = escapeProcedureName varName
    let sigName = parameterTypes |> Seq.map (SType.scratchParameterTypeName >> (+) " %") |> String.concat ""
    let procName = baseName + sigName
    procName

let declareVariable export isPersistent isMutable name varType =
    {
        var = Var.newStorage name isMutable varType
        export = export
        persistence = isPersistent
    }

let implementVariable senv source varSpec init = runRef FState senv.s <| fun state ->
    let a = withExportTag (SourceCode.location source) varSpec.export
    let v = VariableDecl.make a varSpec.var varSpec.persistence
    let init = Top.VariableInit(VariableInitDef(varSpec.var, init))
    let s = { x = 0.; y = 0.; script = init }
    let state =
        let data = state.data
        { state with
            data =
            { data with
                variables = v::data.variables
                scripts = s::data.scripts
            }
        }
    (), state

let declareSprite senv varName = runRef FState senv.s <| fun state ->
    let uniqueName, ns = uniqueName (get FConfig senv.e).indexedName varName () state.spriteNamespace
    uniqueName, { state with spriteNamespace = ns }

let addScriptData senv script = modifyRef FState senv.s <| fun state ->
    { state with data = { state.data with scripts = script::state.data.scripts } }

let addScript senv script = addScriptData senv { x = 0.; y = 0.; script = script }

let registerList senv source export view var isPersistent values = runRef FState senv.s <| fun state ->
    let view =
        match view with
        | Some v -> v
        | None -> { x = 0.; y = 0.; width = 0.; height = 0.; visible = Hidden }

    let v = { 
        state = withExportTag (SourceCode.location source) export
        isPersistent = isPersistent
        var = var
        init = values
        view = view
    }
    let data = state.data
    let spec = {
        listVar = var
    }
    let state = {
        state with
            data = { data with lists = v::data.lists }
    }
    spec, state

let addSpec i s env = env |> map FEnvironment (fun e -> { e with vars = Map.add i s e.vars })
let addVarSpec v s env = addSpec (VarId v) s env
let addMethodSpec m s env = addSpec (methodId m) s env
let addPropertySpec p s env = addSpec (propertyId p) s env
let addSpriteSpec t s env = addSpec (typeId t) s env
let addFieldSpec f s env = addSpec (fieldId f) s env

let addParameters ps env =
    ps
    |> List.fold (fun env p ->
        let spec = { var = p.parameterIRVar; export = NoExport; persistence = NoPersistent }
        addVarSpec p.parameterVar (VarSpec spec) env
    ) env

let addExternalSpec senv id spec = modifyRef FExternalItemState senv.s <| fun s -> { s with externalEnv = addSpec id spec s.externalEnv }
let pushExternalAcc senv spec = modifyRef FExternalItemState senv.s <| fun s -> { s with externalAcc = spec::s.externalAcc }

/// `this` in `type Sprite1(x) as this = ...`
let (|SpriteThis|_|) senv this =
    match (get FEnvironment senv.e).rare.sprite, this with
    | Some s, E.Var this when s.this = this -> Some()
    | _ -> None

/// (`this` in `type Sprite1(x) as this = ...`) or (`this` in `member this.M() = ...`)
let (|SpriteOrProcedureThis|_|) senv this =
    match this with
    | SpriteThis senv () -> Some()
    | _ ->

    match (get FBlockEnvironment senv.e).proc.procedureThis, this with
    | Some v, E.Var this when v = this -> Some()
    | _ -> None

let (|PropertyOrVarOrFieldGet|_|) env = function
    | E.PropertyGet(None, p, []) -> propertyId p |> Some
    | E.Var v -> VarId v |> Some
    | E.FieldGet(Some(SpriteOrProcedureThis env ()), f) -> fieldId f |> Some
    | _ -> None

module Plugin =
    let empty = {
        preTransform = fun _ _ -> None
        expression = { new ExpressionPluginProcess() with override _.Invoke _ _ = Error None }
        item = { new ItemPluginProcess() with override _.Invoke _ _ = Error None }
        externalItems = { new ExternalItemsPluginProcess() with override _.Invoke _ _ = Error None }
        postTransform = Plugins.empty
    }
    let merge plugin1 plugin2 = {
        preTransform = fun c e -> plugin1.preTransform c e |> Option.defaultValue e |> plugin2.preTransform c
        expression = plugin1.expression </> plugin2.expression
        item = plugin1.item </> plugin2.item
        externalItems = plugin2.externalItems </> plugin1.externalItems
        postTransform = Plugins.merge plugin1.postTransform plugin2.postTransform
    }

module Config =
    let mergePlugin config plugin = {
        config with plugin = Plugin.merge config.plugin plugin
    }

let inMember m ts = List.map (fun (UnderlyingTypeSpec(t, path, k)) -> UnderlyingTypeSpec(t, MemberPath(m, path), k)) ts

/// `fun a (b, c) d -> $body` => `fun a b c d -> $body`
let lambdaFlatten = function  
    | Lambdas(vvs, body) -> Seq.foldBack (fun v body -> Expr.Lambda(v, body)) (Seq.concat vvs) body
    | e -> e

let removeClassInitCheck sprite body =
    let self = match sprite.selfField with None -> Choice1Of2 sprite.this | Some self -> Choice2Of2 self
    transformDeepOnce (Class.transformCheckThis self (Expr.Var sprite.this)) body
    |> Option.defaultValue body

[<AutoOpen>]
module internal FixScratch3BoolExpr =
    let xunused = 10

    // NOTE: Scratch3 では、bool 即値は保存できない
    /// `true` => `0 = 0`
    /// `false` => `0 = 1`
    let boolToKnownBooleanExpression l x =
        if x then Expressions.``=`` l (Expression.eNumber l 0.) (Expression.eNumber l 0.)
        else Expressions.``=`` l (Expression.eNumber l 0.) (Expression.eNumber l 1.)

    let replaceOperandToBooleanExpression e =
        let toKnownBoolExpr = function
            | OperandType.Expression t, Complex(ComplexExpression(operator = KnownOperatorInfo(ValueSome { resultType = r }))) & operand
                when t = TsType.gBoolean && r <> TsType.gBoolean ->

                let s = Expression.state operand
                Expressions.``=`` s operand (boolToKnownBooleanExpression s true)
                |> R.modified

            | _, operand -> R.noModify operand

        match e with
        | ComplexExpression(s, KnownOperatorInfo(ValueSome { operands = specs }) & operator, operands)
            when List.length operands = List.length specs ->

            let mutable i = 0
            operands
            |> R.mapList (fun operand ->
                let meta = List.item (let x = i in i <- i + 1; x) specs
                toKnownBoolExpr (meta.operandType, operand)
            )
            |> R.mapIfModified e (fun operands ->
                ComplexExpression(s, operator, operands)
            )
        | _ -> R.noModify e

    let expressionToKnownBooleanExpression x _ =
        match x with
        | Literal(s, SBool x) -> boolToKnownBooleanExpression s x |> R.modified
        | Complex e ->
            replaceOperandToBooleanExpression e
            |> R.mapIfModified x Complex

        | _ -> R.noModify x

    let operandToKnownBooleanExpression es _ =
        match es with
        | e::es as ees ->
            replaceOperandToBooleanExpression e
            |> R.mapIfModified ees (fun e -> e::es)

        | _ -> R.noModify es

let eliminateFooterTailStatements xs _ =
    let (|IsFooter|) = function
        | ComplexExpression(operator = KnownOperatorInfo(ValueSome { isFooter = isFooter })) -> isFooter
        | _ -> false

    let (|IsNoReturn|) = function
        | ComplexExpression(operator = O.doForever | KnownOperatorInfo(ValueSome { control = Control.Stop })) -> true
        | _ -> false

    let wrap state footer =
        Expressions.doIf state (Expression.eBool state true) state [footer]

    match xs with
    | IsFooter true as footer::tail ->
        match footer with
        | IsNoReturn true -> R.modified [footer]
        | ComplexExpression(state = state) -> R.modified (wrap state footer::tail)

    | xs -> R.noModify xs

let fixBlocks data =
    let config =
        { Config.defaultConfig with
            plugins = {
                Plugins.empty with
                    transformExpression = expressionToKnownBooleanExpression
                    transformComplexExpressions = R.compose operandToKnownBooleanExpression eliminateFooterTailStatements
            }
            maxIterationCount = 1
        }
    Transformer.transformStage config data

let postTransform config data =
    let config = {
        plugins = config.plugin.postTransform
        maxIterationCount = 100
    }
    Transformer.transformStage config data

/// location and empty tags
let getLoc e = e |> getLocation |> Tagged.empty

let implMethod (thisType: Type) (virtualMethod: MethodInfo) =
    match virtualMethod.DeclaringType with
    | d when not d.IsInterface ->
        thisType.GetMethods(B.NonPublic ||| B.Public ||| B.Instance)
        |> Seq.tryFind (fun m -> m.GetBaseDefinition() = virtualMethod)

    | interfaceType ->
        let map = thisType.GetInterfaceMap interfaceType
        map.InterfaceMethods
        |> Array.tryFindIndex (fun m -> m = virtualMethod)
        |> Option.map (fun i -> map.TargetMethods.[i])

let devirtualize thisType (m: MethodInfo) =
    if not m.IsVirtual then Some m else

    match implMethod thisType m with

    // m' が override されないか確認
    | Some m' when thisType.IsValueType || thisType.IsSealed || m'.IsFinal -> Some m'

    | _ -> None

let (|NonVirtualInstanceCall|_|) = function
    | E.Call(Some(ExprType thisT as this), m, args) ->
        match devirtualize thisT m with
        | None -> None
        | Some m -> Some(this, m, args)
    | _ -> None

[<AutoOpen>]
module TranspilerPersistenceExtensions =
    module Persistence =
        let ofMemberAttributes (p: MemberInfo) =
            if p.GetCustomAttributes<PersistentAttribute>(``inherit`` = true) |> Seq.isEmpty
            then NoPersistent
            else Persistent
