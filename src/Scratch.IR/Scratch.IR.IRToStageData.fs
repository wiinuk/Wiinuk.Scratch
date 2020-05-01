namespace Scratch.IR
open Scratch
open Scratch.Ast
open Scratch.AstDefinitions
open Scratch.IR
open Scratch.IR.Source.Operators
open Scratch.Primitives
open System.Collections.Generic
open System

module A = Expression
module A = Expressions
module Types = ExpTypes
module Types = MemberTypes


[<AutoOpen>]
module private IRToStageDataHelpers =
    [<Struct; RequireQualifiedAccess>]
    type UniqueName = private { name: string }
    module UniqueName =
        let name { UniqueName.name = x } = x

    [<AutoOpen>]
    module UniqueNameOperators =
        let (|UniqueName|) x = UniqueName.name x

    [<Struct>]
    type Namespace<'v> when 'v : equality = {
        names: HashSet<string>
        varToName: Dictionary<'v, UniqueName>
    }
    module Namespace =
        let newEmpty() = { varToName = Dictionary(); names = HashSet() }
        let copy parent = { varToName = Dictionary parent.varToName; names = HashSet parent.names }

        let uniqueName naming ns baseName key =
            let mutable r = Unchecked.defaultof<_>
            if ns.varToName.TryGetValue(key, &r) then r else

            let uniqueName =
                if not <| ns.names.Contains baseName then baseName else
                let rec aux baseName i =
                    let n = naming struct {| baseName = baseName; index = i |}
                    if not <| ns.names.Contains n then n
                    else aux baseName (Checked.(+) i 1)
                aux baseName 1

            ns.names.Add uniqueName |> ignore
            ns.varToName.Add(key, { UniqueName.name = uniqueName })
            { UniqueName.name = uniqueName }

    [<Struct>]
    type ListSpec = {
        listUniqueName: UniqueName
    }
    type UnderlyingParameterSpec = {
        parameterName: UniqueName
        parameterDefaultValue: SValue
    }
    [<Struct>]
    type ParameterSpec = {
        index: int
        underlyingParameters: UnderlyingParameterSpec list
    }
    [<Struct>]
    type UnderlyingVariableSpec = {
        underlyingName: UniqueName
        underlyingType: VType
    }
    [<Struct>]
    type VariableSpec = {
        underlyingVariables: UnderlyingVariableSpec list
    }
    type ThreadStackLocalSpec =  {
        stackMemory: ListSpec
        stackParameter: Var
    }
    type LocalRepresentation =
        | VariableLocal
        | StaticStackLocal of stack: ListSpec
        | ThreadStackLocal of ThreadStackLocalSpec

    type StorageSpec =
        | VariableStorage of VariableSpec
        | StaticStackStorage of stackList: ListSpec * indexes: int list
        | ThreadStackStorage of memory: ListSpec * stackParameterName: UniqueName * indexes: int list

    type ProcedureBodySpec = {
        parameters: ExpType list

        local: LocalRepresentation

        resultType: ExpType
        resultStorage: StorageSpec

        //resultNames: string list

        // - NoAtomic と指定されている
        // or
        // - Atomic と指定されているが、waitUntil などのように、atomic と指定していてもスレッドを譲る操作を含む可能性がある
        //hasYield: bool

        //isRecursive: bool
    }
    type ProcedureId = {
        name: UniqueName
        procedureVar: SimpleVar
    }
    type ScriptSpec = {
        scriptId: ProcedureId voption
        body: ProcedureBodySpec
    }
    type ProcedureSpec = {
        procedureId: ProcedureId
        body: ProcedureBodySpec
    }
    module ScriptSpec =
        let ofProcedureSpec p = { scriptId = ValueSome p.procedureId; body = p.body }
        let listenerSpec = {
            scriptId = ValueNone
            body = {
                parameters = []
                resultStorage = VariableStorage { underlyingVariables = [] }
                resultType = Types.empty
                local = VariableLocal
            }
        }

    type Spec =
        | VariableSpec of VariableSpec
        | ListSpec of ListSpec
        | ProcedureSpec of ProcedureSpec

        | StorageSpec of StorageSpec
        | ParameterSpec of ParameterSpec

    [<Struct>]
    type LateState<'a,'v> =
        | Factory of factory: ('a -> 'v)
        | Initialized of value: 'v

    type Late<'a,'v> = LateState<'a,'v> ref
    module Late =
        let create factory = ref (Factory factory)
        // TODO: thread safe
        let force ref arg =
            match ref.contents with
            | Initialized x -> x
            | Factory f ->
                let x = f arg
                ref.contents <- Initialized x
                x

    module SValue =
        let defaultVariableValue = function
            | Typed t -> SType.parameterDefaultValue t
            | Any -> SString ""

    //[<Struct>]
    //type Stack<'a> = { head: 'a Reference; item0: 'a Sequence }
    //module Stack =
    //    let stackMemory = defineList []
    //    let stackPool = defineList []

    //    /// unsafe
    //    let get stack n = SList.get stackMemory (SList.get stackMemory stack + n)
    //    /// unsafe
    //    let set stack n value = SList.set stackMemory (SList.get stackMemory stack + n) value

    //    let ensure stack count =
    //        if stack < SList.get stackMemory stack - count
    //        then SList.set stackMemory stack (SList.get stackMemory stack - count)
    //        else stopAllScripts ()

    //    /// unsafe
    //    let popN stack n = SList.set stackMemory stack (SList.get stackMemory stack + n)

    //    let mutable rent_result = 0
    //    let capacity = 1024
    //    let rent() =
    //        if SList.length stackPool = 0 then
    //            rent_result <- SList.length stackMemory + 1
    //            repeat (capacity + 1) <| fun _ -> SList.push stackMemory 0
    //            SList.set stackMemory rent_result (rent_result + 1 + capacity)
    //        else
    //            rent_result <- SList.getLast stackPool
    //            SList.removeLast stackPool

    //    let return' stack = SList.push stackPool stack

    type VariableHome = Stage | Self
    type EntityState<'a> = {
        entity: IREntityData<unit,'a>
        entityStat: 'a EntityStat

        childSprites: Map<string, IREntityData<unit,'a> * 'a EntityStat>
        parent: EntityState<'a> option
        procedureNamespace: ProcedureVar Namespace
        variableOrListNamespace: struct {| var: SimpleVar; index: int |} Namespace

        procedureSpecs: Dictionary<ProcedureVar, ProcedureSpec>
        specs: Dictionary<SimpleVar, Spec>

        runtimeProcedures: Dictionary<RuntimeProcedures, Late<'a, ProcedureSpec>>
        runtimeVariables: Dictionary<RuntimeVariables, Late<'a, VariableSpec>>
        runtimeLists: Dictionary<RuntimeLists, Late<'a, ListSpec>>

        accumulatedVariables: 'a VariableData ResizeArray
        accumulatedLists: 'a ListVariableData ResizeArray
        accumulatedScripts: 'a ScriptData ResizeArray
    }
    type ProcedureState<'a> = {
        parameterNamespace: struct {| var: SimpleVar; index: int |} Namespace
        accumulatedStatements: 'a ComplexExpression ResizeArray
        procedure: ScriptSpec
        localSpecs: Dictionary<SimpleVar, Spec>
        mutable parameterCount: int
        mutable localCount: int
    }
    type IEntityBuilder<'a> =
        abstract Config: IRToStageData.Config
        abstract Entity: 'a EntityState

    type IProcedureBuilder<'a> =
        inherit IEntityBuilder<'a>
        abstract Procedure: 'a ProcedureState

    type ISelf<'Self> when 'Self :> 'Self ISelf = interface end

    [<Struct>]
    type EntityBuilder<'a> = {
        Config: IRToStageData.Config
        Entity: EntityState<'a>
    }
    with
        interface ISelf<'a EntityBuilder>
        interface IEntityBuilder<'a> with
            member x.Config = x.Config
            member x.Entity = x.Entity

    type ProcedureBuilder<'a> = {
        EntityBuilder: EntityBuilder<'a>
        Procedure: ProcedureState<'a>
    }
    with
        interface ISelf<'a ProcedureBuilder>
        interface IEntityBuilder<'a> with
            member x.Config = x.EntityBuilder.Config
            member x.Entity = x.EntityBuilder.Entity

        interface IProcedureBuilder<'a> with
            member x.Procedure = x.Procedure

module private BuilderHelpers =
    let (|Config|) (b: 'B when 'B :> IEntityBuilder<_> and 'B :> 'B ISelf) = b.Config
    let (|EntityState|) (b: 'B when 'B :> IEntityBuilder<_> and 'B :> 'B ISelf) = b.Entity
    let (|ProcState|) (b: 'B when 'B :> IProcedureBuilder<_> and 'B :> 'B ISelf) = b.Procedure

    let toListAndClear (vs: _ ResizeArray) =
        if vs.Count = 0 then [] else
        let xs = Seq.toList vs
        vs.Clear()
        xs

    let resolveRuntimeVariable (EntityState e) state var = Late.force e.runtimeVariables.[var] state
    let resolveRuntimeList (EntityState e) state list = Late.force e.runtimeLists.[list] state
    let resolveRuntimeProcedure (EntityState e) state proc = Late.force e.runtimeProcedures.[proc] state

    let declareList (Config c & EntityState e) var =
        let name = Namespace.uniqueName c.naming.indexedName e.variableOrListNamespace (Var.name var) {| var = var; index = 0 |}
        let spec = { listUniqueName = name }
        e.specs.Add(var, ListSpec spec)
        spec

    let home (EntityState e) = function
        | Stage -> Option.defaultValue e e.parent
        | Self -> e

    [<Struct>]
    type EmitListOptions<'a> = {
        state: 'a
        home: VariableHome

        contents': SValue iarray
        isPersistent: Persistence

        x: double
        y: double
        width: double
        height: double
        visible: Visibility
    }
    let emitList b (options: _ inref) spec =
        let e = home b options.home
        e.accumulatedLists.Add {
            state = options.state
            listName = UniqueName.name spec.listUniqueName
            contents' = options.contents'
            isPersistent = options.isPersistent

            x = options.x
            y = options.y
            width = options.width
            height = options.height
            visible = options.visible
        }
    let defineListAndInit b (options: _ inref) var =
        let { listUniqueName = name } & spec = declareList b var
        emitList b &options spec
        let e = home b options.home
        let s = &options.state
        e.accumulatedScripts.Add <| A.whenGreenFlag s [A.``deleteLine:ofList:`` s (UniqueName.name name) (A.eString s "all")]
        spec

    let defineStandardListAndInit b s home name =
        let var = Var.newSimple name
        let options = {
            state = s
            contents' = IArray.empty
            isPersistent = NoPersistent
            home = home

            x = 0.
            y = 0.
            width = 0.
            height = 0.
            visible = Hidden
        }
        defineListAndInit b &options var

    let addVariableSpec (Config c & EntityState e) var =
        let vars =
            match Var.varType var with
            | UnboxedType t ->
                t
                |> List.mapi (fun i t ->
                    let m = MemberType.memberName t
                    let t = MemberType.underlyingType t
                    let name = Namespace.uniqueName c.naming.indexedName e.variableOrListNamespace (Var.name var + m) {| var = Var.simple var; index = i |}
                    {
                        underlyingName = name
                        underlyingType = t
                    }
                )
        let spec = { underlyingVariables = vars }
        e.specs.Add(Var.simple var, VariableSpec spec)
        spec

    let private emitVariable (EntityState e) state spec =
        for v in spec.underlyingVariables do
            e.accumulatedVariables.Add {
                state = state
                isPersistent = NoPersistent
                name = UniqueName.name v.underlyingName
                value = SValue.defaultVariableValue v.underlyingType
            }

    let defineVariable b var state =
        let spec = addVariableSpec b var
        emitVariable b state spec
        spec

    let underlyingParameterType = function
        | Any -> SType.S
        | Typed t -> t

    let expTypeToUnderlyingTypes (UnboxedType ts) = seq {
        for t in ts do
            let t = MemberType.underlyingType t
            yield underlyingParameterType t
    }
    let procedureHiddenParameters body =
        match body.local with
        | VariableLocal
        | StaticStackLocal _ -> []
        | ThreadStackLocal { stackParameter = stack } -> [stack]

    let addProcedureSpec (Config c & EntityState e & b) state var local =
        let parameterTypes = Var.trivia(var).parameterTypes
        let resultType = Var.trivia(var).resultType
        let resultName = c.naming.combineNamespaceItem (Var.name var, c.naming.resultName)
        let result = Var.newStorage resultName false resultType
        let resultStorage = VariableStorage <| defineVariable b result state
        let body = {
            parameters = parameterTypes
            resultStorage = resultStorage
            resultType = resultType
            local = local
        }
        let underlyingParameterTypes = seq {
            for p in procedureHiddenParameters body do
                yield! expTypeToUnderlyingTypes <| Var.varType p

            for t in parameterTypes do
                yield! expTypeToUnderlyingTypes t
        }
        let procName = mangleProcedureName (Var.name var) underlyingParameterTypes

        let name = Namespace.uniqueName c.naming.indexedName e.procedureNamespace procName var
        let spec = {
            procedureId = {
                name = name
                procedureVar = Var.simple var
            }
            body = body
        }
        e.specs.Add(Var.simple var, ProcedureSpec spec)
        e.procedureSpecs.Add(var, spec)
        spec

    module private Errors =
        let resolvedSpecIsNotParameter var spec =
            invalidOp <| sprintf "var %A to spec ( %A ) referenced, but it is not parameter"
                (Var.name var)
                spec

        let varUndefined var (specs: Dictionary<_,_>) =
            invalidOp <| sprintf "var %A ( %A ) referenced, but it is not defined, defined vars is %A"
                (Var.name var)
                (Var.varType var)
                (specs.Keys |> Seq.map Var.name |> Seq.toList)

    let resolveParameter (ProcState p) var =
        let mutable r = Unchecked.defaultof<_>
        let specs = p.localSpecs
        if not <| specs.TryGetValue(Var.simple var, &r) then Errors.varUndefined var specs else

        match r with
        | ParameterSpec p -> p
        | VariableSpec _
        | StorageSpec _
        | ProcedureSpec _
        | ListSpec _ -> Errors.resolvedSpecIsNotParameter var r

    let resolvePrimitiveParameter b var =
        (resolveParameter b var).underlyingParameters |> List.exactlyOne

    let resolveVariable (EntityState e) var =
        match e.specs.[Var.simple var] with
        | VariableSpec var -> var
        | StorageSpec _
        | ParameterSpec _
        | ListSpec _
        | ProcedureSpec _ -> failwith ""

    // <@ v <- ... @>, <@ v @>
    let defineVariableStorage b state name storageType =
        let var = Var.newStorage name false storageType
        VariableStorage <| defineVariable b var state

    let defineLocalIndexes (ProcState p) localType =
        match localType with
        | UnboxedType localType ->
            localType
            |> List.map (fun _ -> let i = p.localCount in p.localCount <- Checked.(+) i 1; i)

    let globalStackStorageIndex state stackName index =
        A.``-`` state (A.``lineCountOfList:`` state (UniqueName.name stackName)) (A.eNumber state (double index))

    let threadStackStorageIndex b state stackName index =
        let stackMemory = resolveRuntimeList b state StackMemory
        let stackMemoryName = UniqueName.name stackMemory.listUniqueName
        A.``+`` state (A.``getLine:ofList:`` state stackMemoryName (A.getParam state (UniqueName.name stackName))) (A.eNumber state (double index))

    let defineStorage (ProcState p & b) state name storageType =
        match p.procedure.body.local with
        | VariableLocal -> defineVariableStorage b state name storageType

        // <@ v.[v.length - N] <- ... @>, <@ v.[v.length - N] @>
        | StaticStackLocal stackList -> StaticStackStorage(stackList, defineLocalIndexes b storageType)
        | ThreadStackLocal { stackMemory = memory; stackParameter = stack } ->
            let { parameterName = stackName } = resolvePrimitiveParameter b stack
            ThreadStackStorage(memory, stackName, defineLocalIndexes b storageType)

    let getStorage b state storage =
        match storage with
        | VariableStorage s -> s.underlyingVariables |> List.map (fun v -> UniqueName.name v.underlyingName |> A.readVariable state)
        | StaticStackStorage({ listUniqueName = stackName }, indexes) ->
            indexes
            |> List.map (fun index ->
                let index = globalStackStorageIndex state stackName index
                A.``getLine:ofList:`` state (UniqueName.name stackName) index
            )

        // `stackMemory[stackMemory[stack] + index]`
        | ThreadStackStorage(stackMemory, stack, indexes) ->
            indexes
            |> List.map (fun index ->
                let index = threadStackStorageIndex b state stack index
                A.``getLine:ofList:`` state (UniqueName.name stackMemory.listUniqueName) index
            )

    let setStoragePrimitive b state storage index primitiveValue =
        match storage with
        | VariableStorage x ->
            A.``setVar:to:`` state (UniqueName.name x.underlyingVariables.[index].underlyingName) primitiveValue

        // `procStack[procStack.count - index] <- value`
        | StaticStackStorage({ listUniqueName = stackName }, indexes) ->
            let index = globalStackStorageIndex state stackName indexes.[index]
            A.``setLine:ofList:to:`` state (UniqueName.name stackName) index primitiveValue

        // type ThreadStack = { top: IWord Reference; items: IWord Sequence }
        // val stack: ThreadStack Reference @stackMemory
        // => `stack.top[index] <- value`
        // => `stackMemory[stackMemory[stack] + index] <- value`
        | ThreadStackStorage({ listUniqueName = stackMemoryName }, stackName, indexes) ->
            let index = threadStackStorageIndex b state stackName indexes.[index]
            A.``setLine:ofList:to:`` state (UniqueName.name stackMemoryName) index primitiveValue

    let setStorage b state storage value = List.mapi (setStoragePrimitive b state storage) value

    module RuntimeSpecs =
        let stackMemory (Config c & b) s = defineStandardListAndInit b s Stage <| c.naming.runtimeListName StackMemory
        let stackPool (Config c & b) s = defineStandardListAndInit b s Stage <| c.naming.runtimeListName StackPool
        let defaultStack (Config c & b) s =
            let var = Var.newStorage (c.naming.runtimeVariableName DefaultStack) true Types.number
            defineVariable b var s

        let defineEnsureStack (Config c & EntityState e & b) s =
            let atomicity = Atomic
            let var = Var.newVar (c.naming.runtimeProcedureName EnsureStack) { parameterTypes = [Types.number; Types.number]; resultType = Types.empty }

            (*
            ```
            let ensure stack count =
                if stack < SList.get stackMemory stack - count
                then SList.set stackMemory stack (SList.get stackMemory stack - count)
                else stopAllScripts ()
            ```
            *)
            let parameters = [
                ParameterDefinition(s, "stack", SType.parameterDefaultValue SType.N)
                ParameterDefinition(s, "count", SType.parameterDefaultValue SType.N)
            ]
            let body =
                let { listUniqueName = stackMemory } = resolveRuntimeList b s StackMemory
                let stack = A.getParam s "stack"

                /// `stackMemory[stack] - count`
                let nextHead = A.``-`` s (A.``getLine:ofList:`` s (UniqueName.name stackMemory) stack) (A.getParam s "count")

                BlockExpression(s, [
                    A.doIfElse
                        s (A.``<`` s stack nextHead)
                        s [A.``setLine:ofList:to:`` s (UniqueName.name stackMemory) stack nextHead]
                        s [A.stopScripts s "all"]
                ])

            let spec = addProcedureSpec b s var VariableLocal
            let proc = ProcedureDefinition(s, UniqueName.name spec.procedureId.name, parameters, atomicity, body)
            e.accumulatedScripts.Add { x = 0.; y = 0.; script = Procedure proc }
            spec

        let defineRentStack (Config c & EntityState e & b) s =
            let maxThreadStackDepth (EntityState e) =
                let stage = e.parent |> Option.defaultValue e
                let entities = seq { yield stage.entity, stage.entityStat; for kv in stage.childSprites -> kv.Value }
                let depths = seq {
                    for e, stat in entities do
                        for s in e.scripts do
                            match s.script with
                            | Top.Listener l -> yield EntityStat.listenerStackDepth stat l
                            | _ -> ()
                }
                depths
                |> Seq.tryMaxBound Infinity
                |> VOption.defaultValue (Finite 0)

            (*
            let mutable rent_result = 0
            let rent() =
                if SList.length stackPool = 0 then
                    rent_result <- SList.length stackMemory + 1
                    repeat ($capacity + 1) <| fun _ -> SList.push stackMemory $uninitValue
                    SList.set stackMemory rent_result (rent_result + 1 + $capacity)
                else
                    rent_result <- SList.getLast stackPool
                    SList.removeLast stackPool
            *)
            let atomicity = Atomic
            let name = c.naming.runtimeProcedureName RentStack

            let var = Var.newVar name { parameterTypes = []; resultType = Types.number }
            let spec = addProcedureSpec b s var VariableLocal

            let maxStackDepth =
                match maxThreadStackDepth b with
                | Infinity -> c.maxStackCapacity
                | Finite x -> x

            let capacity = double <| min c.maxStackCapacity maxStackDepth
            let { listUniqueName = UniqueName stackPool } = resolveRuntimeList b s StackPool
            let { underlyingName = UniqueName resultName } =
                match spec.body.resultStorage with
                | VariableStorage { underlyingVariables = [result] } -> result
                | _ -> failwith ""

            let result = A.readVariable s resultName
            let { listUniqueName = UniqueName stackMemory } = resolveRuntimeList b s StackMemory

            let body = [
                A.doIfElse
                    s (A.``=`` s (A.``lineCountOfList:`` s stackPool) (A.eNumber s 0.))
                    s [
                        A.``setVar:to:`` s resultName (A.``+`` s (A.``lineCountOfList:`` s stackMemory) (A.eNumber s 1.))
                        A.doRepeat s (A.eNumber s (capacity + 1.)) s [
                            A.``append:toList:`` s stackMemory (Literal(s, c.uninitValue))
                        ]
                        A.``setLine:ofList:to:`` s stackMemory result (A.``+`` s result (A.eNumber s (capacity + 1.)))
                    ]
                    s [
                        A.``setVar:to:`` s resultName (A.``getLine:ofList:`` s stackPool (A.eString s "last"))
                        A.``deleteLine:ofList:`` s stackPool (A.eString s "last")
                    ]
            ]
            let body = BlockExpression(s, body)
            let proc = ProcedureDefinition(s, UniqueName.name spec.procedureId.name, [], atomicity, body)
            e.accumulatedScripts.Add { x = 0.; y = 0.; script = Procedure proc }
            spec

        let defineInitDefaultStack (Config c & EntityState e & b) s =
            let defaultStack = resolveRuntimeVariable b s DefaultStack |> VariableStorage
            let rentStack = resolveRuntimeProcedure b s RentStack
            (*
            let initDefaultStack() =
                if defaultStack = 0 then
                    rentStack()
                    defaultStack <- rentStack_result
            *)
            let name = c.naming.runtimeProcedureName InitDefaultStack
            let atomicity = Atomic
            let var = Var.newProc name [] Types.empty
            let spec = addProcedureSpec b s var VariableLocal

            let getDefaultStack = getStorage b s defaultStack |> List.exactlyOne
            let body = [
                A.doIf
                    s (A.``=`` s getDefaultStack (A.eNumber s 0.))
                    s (
                        A.call s (UniqueName.name rentStack.procedureId.name) []::
                        setStorage b s defaultStack (getStorage b s rentStack.body.resultStorage)
                    )
            ]
            let body = BlockExpression(s, body)
            let proc = ProcedureDefinition(s, UniqueName.name spec.procedureId.name, [], atomicity, body)
            e.accumulatedScripts.Add { x = 0.; y = 0.; script = Procedure proc }
            spec

    let addRuntimeVariables (EntityState e & b) =
        e.runtimeLists.Add(StackMemory, Late.create <| RuntimeSpecs.stackMemory b)
        e.runtimeLists.Add(StackPool, Late.create <| RuntimeSpecs.stackPool b)
        e.runtimeVariables.Add(DefaultStack, Late.create <| RuntimeSpecs.defaultStack b)

    let addRuntimeProcedures (EntityState e & b) =
        e.runtimeProcedures.Add(EnsureStack, Late.create <| RuntimeSpecs.defineEnsureStack b)
        e.runtimeProcedures.Add(RentStack, Late.create <| RuntimeSpecs.defineRentStack b)
        e.runtimeProcedures.Add(InitDefaultStack, Late.create <| RuntimeSpecs.defineInitDefaultStack b)

    let newStageEntityState children entity =
        {
            entity = entity
            entityStat = EntityStat.make entity
            parent = None
            childSprites = Map.ofSeq <| seq { for c in children -> c.objName, (c, EntityStat.make c) }
            procedureNamespace = Namespace.newEmpty()
            procedureSpecs = Dictionary()
            variableOrListNamespace = Namespace.newEmpty()
            specs = Dictionary()
            accumulatedVariables = ResizeArray()
            accumulatedLists = ResizeArray()
            accumulatedScripts = ResizeArray()

            runtimeProcedures = Dictionary()
            runtimeVariables = Dictionary()
            runtimeLists = Dictionary()
        }

    let newSpriteEntityState parent children entity =
        {
            entity = entity
            entityStat = parent.childSprites.[entity.objName] |> snd
            parent = Some parent
            childSprites = Map.ofSeq <| seq { for c in children -> c.objName, (c, EntityStat.make c) }

            // 親の procedure を子は引き継がない
            procedureNamespace = Namespace.newEmpty()
            procedureSpecs = Dictionary()

            variableOrListNamespace = Namespace.copy parent.variableOrListNamespace
            specs = Dictionary parent.specs

            accumulatedVariables = ResizeArray()
            accumulatedLists = ResizeArray()
            accumulatedScripts = ResizeArray()

            runtimeLists = parent.runtimeLists
            runtimeVariables = parent.runtimeVariables
            runtimeProcedures = Dictionary()
        }

    let newProcState parent procedure =
        let (EntityState e) = parent
        {
            localSpecs = Dictionary e.specs
            parameterNamespace = Namespace.newEmpty()
            accumulatedStatements = ResizeArray()
            procedure = procedure
            parameterCount = 0
            localCount = 0
        }

module private Builder =
    open BuilderHelpers

    let config (Config c) = c
    let localCount (ProcState p) = p.localCount
    let currentProcedure (ProcState p) = p.procedure
    let resolveProcedure (EntityState e) p = e.procedureSpecs.[p]
    let resolveRuntimeProcedure b state p = resolveRuntimeProcedure b state p
    let resolveRuntimeList b state l = resolveRuntimeList b state l
    let resolveRuntimeVariable b state v = resolveRuntimeVariable b state v
    let resolveLocalSpec (ProcState p) var = p.localSpecs.[Var.simple var]
    let resolvePrimitiveParameter b var = resolvePrimitiveParameter b var
    let resolveVariable b var = resolveVariable b var
    let resolveList (EntityState e) var =
        match e.specs.[var] with
        | ListSpec l -> l
        | ParameterSpec _
        | ProcedureSpec _
        | VariableSpec _
        | StorageSpec _ -> failwith ""

    let declaredParameters (ProcState p) =
        p.localSpecs
        |> Seq.choose (fun kv ->
            match kv.Value with
            | ParameterSpec p -> Some p
            | _ -> None
        )
        |> Seq.sortBy (fun p -> p.index)

    let addVariableSpec b var = addVariableSpec b var
    let declareList b var = declareList b var
    let defineStandardListAndInit b state name = defineStandardListAndInit b state name
    let addProcedureSpec b state var local = addProcedureSpec b state var local

    let accumulateStatement (ProcState p) s = p.accumulatedStatements.Add s
    let accumulateStatements (ProcState p) s = p.accumulatedStatements.AddRange s
    let accumulatedStatementsIsEmpty (ProcState p) = p.accumulatedStatements.Count = 0

    let dropAccumulatedStatements (ProcState p) =
        let xs = ResizeArray p.accumulatedStatements
        p.accumulatedStatements.Clear()
        xs

    let accumulateScript (EntityState e) s = e.accumulatedScripts.Add s
    let accumulateVariable (EntityState e) v = e.accumulatedVariables.Add v
    let accumulateList (EntityState e) l = e.accumulatedLists.Add l

    let dropAccumulatedVariables (EntityState e) = toListAndClear e.accumulatedVariables
    let dropAccumulatedScripts (EntityState e) = toListAndClear e.accumulatedScripts
    let dropAccumulatedLists (EntityState e) = toListAndClear e.accumulatedLists

    let procedureQualifiedName (Config c & ProcState p) name =
        let procName = match p.procedure.scriptId with ValueSome n -> Var.name n.procedureVar | _ -> c.naming.anonName
        c.naming.combineNamespaceItem(procName, name)

    let addVarAsLocalStorage (ProcState p & b) state var =
        let storage = defineStorage b state (procedureQualifiedName b (Var.name var)) (Var.varType var)
        p.localSpecs.Add(Var.simple var, StorageSpec storage)
        storage

    let addVarAsParameter (Config c & ProcState p) var =
        let ps =
            match Var.varType var with
            | UnboxedType ts ->
                ts
                |> List.mapi (fun i t ->
                    let m = MemberType.memberName t
                    let t = MemberType.underlyingType t
                    let name = Namespace.uniqueName c.naming.indexedName p.parameterNamespace (Var.name var + m) {| var = Var.simple var; index = i |}
                    {
                        parameterName = name
                        parameterDefaultValue = SValue.defaultVariableValue t
                    }
                )
        let index = let x = p.parameterCount in p.parameterCount <- x + 1; x
        let param = {
            index = index
            underlyingParameters = ps
        }
        p.localSpecs.Add(Var.simple var, ParameterSpec param)
        param

    let newProcedureBuilder (Config c & EntityState e & b) procedure =
        let procState = newProcState b procedure
        {
            EntityBuilder = {
                Config = c
                Entity = e
            }
            Procedure = procState
        }

    let newEntityBuilder config parent children entity =
        match parent with

        // stage
        | None ->
            let b = {
                Config = config
                Entity = newStageEntityState children entity
            }
            addRuntimeVariables b
            addRuntimeProcedures b
            b

        // sprite
        | Some(EntityState parent) ->
            let b = {
                Config = config
                Entity = newSpriteEntityState parent children entity
            }
            addRuntimeProcedures b
            b

    let defineTempStorage (Config c & b) state storageType =
        defineStorage b state (procedureQualifiedName b c.naming.localName) storageType

    let getStorage b state storage = getStorage b state storage
    let setStoragePrimitive b state storage index value = setStoragePrimitive b state storage index value
    let setStorage b state storage value = setStorage b state storage value

    let procedureHiddenParameters body = procedureHiddenParameters body

module private EmitterHelpers =
    let classifyProcedure (BuilderHelpers.EntityState e) proc =
        let y = EntityStat.procedureYieldability e.entityStat proc
        let r = EntityStat.procedureRecursivity e.entityStat proc
        let d = EntityStat.procedureStackDepth e.entityStat proc
        struct(y, r, d)

    let rec isKnownIgnorableExpression = function
        | Literal _ -> true
        | Block _ -> false
        | Complex(ComplexExpression(_, operator, operands)) ->

        let isKnownIgnorableOperator =
            match operator with
            | O.``getLine:ofList:`` -> true
            | KnownOperatorInfo(ValueSome { operatorCost = cost }) when cost < Reflection.Cost.Pure -> true
            | _ -> true

        if isKnownIgnorableOperator then List.forall isKnownIgnorableExpression operands
        else false

    let ignoreExpressions es =
        if not <| List.forall isKnownIgnorableExpression es then
            es
            |> Seq.map Pretty.prettyExpression
            |> String.concat ", "
            |> failwithf "internal error: (%s)"

module private Emitters =
    open EmitterHelpers

    let emitHiddenParameterCall b state callee args =
        match callee.body.local with

        // `@async let asyncCaller stack = syncCallee(...args)`
        | VariableLocal
        | StaticStackLocal _ ->
            A.call state (UniqueName.name callee.procedureId.name) args |> Builder.accumulateStatement b

        | ThreadStackLocal _ ->

        let caller = Builder.currentProcedure b
        match caller.body.local with

        // ここで asyncCallee は ForceYield ( 同期的なプロシージャから呼んでも強制的に他のスレッドに動作を譲る ) な操作を含まない
        //     なぜなら、もし asyncCallee が ForceYield な操作を含むなら、
        //     syncCaller は ThreadStackLocal ( スタックを引数として受け取る ) として実装されなければならないから
        // なので、asyncCallee が実行されている間は他のスレッドに動作を譲ることはないので、1つの defaultStack を使いまわすことができる
        // ```
        // let syncCaller () =
        //     initDefualtStack()
        //     asyncCallee(defaultStack, ...args)
        // ```
        | VariableLocal
        | StaticStackLocal _ ->
            let initDefaultStack = Builder.resolveRuntimeProcedure b state InitDefaultStack
            let initDefaultStack = UniqueName.name initDefaultStack.procedureId.name

            Builder.accumulateStatement b (A.call state initDefaultStack [])
            let defaultStack = Builder.resolveRuntimeVariable b state DefaultStack
            let { underlyingName = UniqueName defaultStack } = List.exactlyOne defaultStack.underlyingVariables
            let defaultStack = A.readVariable state defaultStack

            A.call state (UniqueName.name callee.procedureId.name) (defaultStack::args) |> Builder.accumulateStatement b

        // 現在の stack パラメータを使う
        | ThreadStackLocal { stackParameter = stack } ->
            let stack = Builder.resolvePrimitiveParameter b stack
            let stack = A.getParam state (UniqueName.name stack.parameterName)

            A.call state (UniqueName.name callee.procedureId.name) (stack::args) |> Builder.accumulateStatement b

    let rec convertExp b x =
        match x.value with
        | Lit xs -> convertLit x.source xs
        | Var v -> convertVar b x.source v
        | VarSet(v, value) -> convertVarSet b x.source (v, value)
        | Let(var, value, scope) -> convertLet b x.source (var, value, scope)
        | Call(proc, args) -> convertCall b x.source (proc, args)
        | If(test, ifTrue, ifFalse) -> convertIf b x.source (test, ifTrue, ifFalse)
        | Seq(first, last) -> convertSeq b (first, last)
        | Op(operator, operands) -> convertOp b x.source (operator, operands)
        | ListOp(op, ops) -> convertListOp b x.source (op, ops)
        | NewTuple xs -> List.collect (convertExp b) xs
        | TupleGet(x, index) -> convertTupleGet b (x, index)
        | TupleSet(v, index, value) -> convertTupleSet b x.source (v, index, value)
        | Coerce(kind, value, newType) -> convertCoerce b x.source (kind, value, newType)
        | Atom e -> convertAtom b x.source e

    and convertEmptyExp b x =
        match convertExp b x with
        | [] -> ()
        | _ -> failwith ""

    and convertPrimitiveExp b x = convertExp b x |> List.exactlyOne

    and convertLit state v = [Literal(state, v)]

    and convertVar b state v =
        match Builder.resolveLocalSpec b v with
        | StorageSpec storage -> Builder.getStorage b state storage
        | ParameterSpec param -> List.map (fun v -> A.getParam state (UniqueName.name v.parameterName)) param.underlyingParameters
        | VariableSpec var -> List.map (fun v -> A.readVariable state (UniqueName.name v.underlyingName)) var.underlyingVariables
        | ListSpec list -> [A.eString state (UniqueName.name list.listUniqueName)]
        | ProcedureSpec _ -> failwith ""

    and convertVarSet b state (var, value) =
        match Builder.resolveLocalSpec b var with
        | StorageSpec storage ->
            let value = convertExp b value
            Builder.setStorage b state storage value |> Builder.accumulateStatements b
            []

        | VariableSpec var ->
            let value = convertExp b value
            List.iter2 (fun v value ->
                A.``setVar:to:`` state (UniqueName.name v.underlyingName) value |> Builder.accumulateStatement b
            ) var.underlyingVariables value
            []

        | ParameterSpec _
        | ListSpec _
        | ProcedureSpec _ -> failwith ""

    // `let x = v in s...` => acc: `vAcc...; x <- v`, expr: `x`
    and convertLet b state (var, value, scope) =
        let var = Builder.addVarAsLocalStorage b state var
        let value = convertExp b value
        Builder.setStorage b state var value |> Builder.accumulateStatements b
        convertExp b scope

    // `callee(args...)` =>
    // acc: ```
    //     argsAcc...
    //     call callee a b
    //     temp <- call_result
    // ```,
    // expr: `temp`
    and convertCall b state (callee, args) =
        let args = List.collect (convertExp b) args

        // `call callee args...` OR
        // `call callee stack args...` OR
        // `call callee defaultStack args...`
        let callee = Builder.resolveProcedure b callee
        emitHiddenParameterCall b state callee args

        // `temp <- proc_result`
        let temp = Builder.defineTempStorage b state callee.body.resultType
        Builder.setStorage b state temp (Builder.getStorage b state callee.body.resultStorage) |> Builder.accumulateStatements b

        // `temp`
        Builder.getStorage b state temp

    // `if test then ifTrue else ifFalse` =>
    // acc: `acc...; testAcc...; doIfElse test { ifTrueAcc...; result <- ifTrue } { ifFalseAcc...; result <- ifFalse }`,
    // expr: `result`
    and convertIf b state (test, ifTrue, ifFalse) =
        let result = Builder.defineTempStorage b state (Exp.varType ifTrue)

        let test' = convertPrimitiveExp b test
        let testAcc = Builder.dropAccumulatedStatements b

        let ifTrue' = convertExp b ifTrue
        let ifTrueAcc = Builder.dropAccumulatedStatements b
        ifTrueAcc.AddRange <| Builder.setStorage b ifTrue.source result ifTrue'
        let ifTrueBody = List.ofSeq ifTrueAcc

        let ifFalse' = convertExp b ifFalse
        let ifFalseAcc = Builder.dropAccumulatedStatements b
        ifFalseAcc.AddRange <| Builder.setStorage b ifFalse.source result ifFalse'
        let ifFalseBody = List.ofSeq ifFalseAcc

        Builder.accumulateStatements b testAcc
        A.doIfElse state test' ifTrue.source ifTrueBody ifFalse.source ifFalseBody |> Builder.accumulateStatement b

        Builder.getStorage b state result

    and convertSeq b (first, last) =
        ignoreExpressions <| convertExp b first
        convertExp b last

    and convertOp b state (operator, operands) =
        match operator with
        | KnownOperatorInfo ValueNone -> raise <| NotImplementedException()
        | KnownOperatorInfo(ValueSome info) ->

        match info.kind with
        | Kind.Expression -> [Complex(ComplexExpression(state, operator, List.map (convertPrimitiveExp b) operands))]
        | Kind.Statement ->
            convertStatementOp b state info (operator, operands)
            []

    and convertStatementOp b state info (operator, operands) =
        match operator with
        | O.call
        | O.``setVar:to:``
        | O.``changeVar:by:``
        | O.doIfElse -> failwithf ""

        | O.doIf ->
            match operands with
            | [test; ifTrue] -> convertDoIf b state (test, ifTrue)
            | _ -> failwithf ""

        | O.doRepeat ->
            match operands with
            | [count; body] -> convertDoRepeat b state (count, body)
            | _ -> failwithf ""

        | O.doUntil ->
            match operands with
            | [test; ifFalse] -> convertDoUntil b state (test, ifFalse)
            | _ -> failwithf ""

        | O.doWaitUntil ->
            match operands with
            | [waitIfFalse] -> convertDoWaitUntil b state waitIfFalse
            | _ -> failwithf ""

        | O.doForever ->
            match operands with
            | [body] -> convertDoForever b state body
            | _ -> failwithf ""

        | O.``forward:``
        | O.``turnRight:``
        | O.``turnLeft:``
        | O.``heading:``
        | O.``pointTowards:``
        | O.``gotoX:y:``
        | O.``gotoSpriteOrMouse:``
        | O.``changeXposBy:``
        | O.``xpos:``
        | O.``changeYposBy:``
        | O.``ypos:``
        | O.``bounceOffEdge``
        | O.``setRotationStyle``
        | O.``lookLike:``
        | O.``nextCostume``
        | O.``showBackground:``
        | O.startScene
        | O.nextBackground
        | O.nextScene
        | O.startSceneAndWait
        | O.``say:duration:elapsed:from:``
        | O.``say:``
        | O.``think:duration:elapsed:from:``
        | O.``think:``
        | O.``changeGraphicEffect:by:``
        | O.``setGraphicEffect:to:``
        | O.filterReset
        | O.``changeSizeBy:``
        | O.``setSizeTo:``
        | O.show
        | O.hide
        | O.comeToFront
        | O.``goBackByLayers:``
        | O.``playSound:``
        | O.doPlaySoundAndWait
        | O.stopAllSounds
        | O.playDrum
        | O.``rest:elapsed:from:``
        | O.``noteOn:duration:elapsed:from:``
        | O.``instrument:``
        | O.``changeVolumeBy:``
        | O.``setVolumeTo:``
        | O.``changeTempoBy:``
        | O.``setTempoTo:``
        | O.clearPenTrails
        | O.putPenDown
        | O.putPenUp
        | O.``penColor:``
        | O.``setPenHueTo:``
        | O.``changePenHueBy:``
        | O.``setPenShadeTo:``
        | O.``changePenShadeBy:``
        | O.``penSize:``
        | O.``changePenSizeBy:``
        | O.stampCostume
        | O.``showVariable:``
        | O.``hideVariable:``
        | O.``broadcast:``
        | O.doBroadcastAndWait
        | O.``wait:elapsed:from:``
        | O.doReturn
        | O.``glideSecs:toX:y:elapsed:from:``
        | O.stopScripts
        | O.createCloneOf
        | O.doAsk
        | O.timerReset
        | O.stopAll
        | O.stopScripts
        | O.deleteClone ->
            convertNormalStatementOp b state info (operator, operands)

        | O.warpSpeed
        | O.doForeverIf
        | O.doWhile
        | _ -> failwithf ""

    // `doIf test ifTrue` =>
    // acc: `testAcc...; doIf test { ifTrueAcc...; }`
    // expr: ``
    and convertDoIf b state (test, ifTrue) =
        let test' = convertPrimitiveExp b test
        let testAcc = Builder.dropAccumulatedStatements b

        convertEmptyExp b ifTrue
        let ifTrueAcc = Builder.dropAccumulatedStatements b
        let ifTrueBody = List.ofSeq ifTrueAcc

        Builder.accumulateStatements b testAcc
        A.doIf state test' ifTrue.source ifTrueBody |> Builder.accumulateStatement b

    // `doRepeat count body` =>
    // acc: `countAcc...; doRepeat count { bodyAcc... }`
    and convertDoRepeat b state (count, body) =
        let count' = convertPrimitiveExp b count
        let countAcc = Builder.dropAccumulatedStatements b

        convertEmptyExp b body
        let bodyAcc = Builder.dropAccumulatedStatements b
        let bodyAcc = List.ofSeq bodyAcc

        Builder.accumulateStatements b countAcc
        A.doRepeat state count' body.source bodyAcc |> Builder.accumulateStatement b

    // `doUntil test ifFalse` =>
    // acc: `acc...; testAcc..; doUntil test { ifFalseAcc...; testAcc...; }`
    and convertDoUntil b state (test, ifFalse) =
        let acc = Builder.dropAccumulatedStatements b

        let test' = convertPrimitiveExp b test
        let testAcc = Builder.dropAccumulatedStatements b

        convertEmptyExp b ifFalse
        let ifFalseAcc = Builder.dropAccumulatedStatements b
        ifFalseAcc.AddRange testAcc
        let ifFalseBody = List.ofSeq ifFalseAcc

        Builder.accumulateStatements b acc
        Builder.accumulateStatements b testAcc
        Builder.accumulateStatement b <| A.doUntil state test' test.source ifFalseBody

    // `doWaitUntil waitIfFalse` =>
    // acc: `doWaitUntil waitIfFalse`
    //
    // `doWaitUntil waitIfFalse` =>
    // if %isNextAcc = `` then
    //     acc: `doWaitUntil %isNext`, expr: ``
    // else
    //     extern: `%atomicity let temp() = %isNextAcc...; doIf (not %test) \{ repeat 1 \{}; temp() }`
    //     acc: `call temp()`, expr: ``
    //
    // - doWaitUntil は Atomic なプロシージャ内で実行されるときでも、実行を譲る
    // - doUntil は Atomic なプロシージャ内で実行されるときに、実行を譲らない
    // なので、Acc があるときの doWaitUntil を、doUntil で置き換えることはできない。
    // - Acc 付きの doWaitUntil は、再起呼び出しを含むプロシージャによる実装とほぼ同じ
    //   - わずかに違う ( Yield と YieldForce )
    //   - 一回待つごとにスタックを消費していく
    //   - 置換えは現実的ではない
    and convertDoWaitUntil b state waitIfFalse =
        let acc = Builder.dropAccumulatedStatements b
        let waitIfFalse = convertPrimitiveExp b waitIfFalse
        if Builder.accumulatedStatementsIsEmpty b |> not then failwithf "" else

        Builder.accumulateStatements b acc
        Builder.accumulateStatement b <| A.doWaitUntil state waitIfFalse

    // `doForever body` =>
    // acc: `acc...; doForever { ...bodyAcc }`
    and convertDoForever b state body =
        let acc = Builder.dropAccumulatedStatements b
        convertEmptyExp b body
        let bodyAcc = Builder.dropAccumulatedStatements b

        Builder.accumulateStatements b acc
        Builder.accumulateStatement b <| A.doForever state body.source (List.ofSeq bodyAcc)

    and convertOperand b (operand, spec) =
        match spec with
        | OperandType.Block _
        | OperandType.VariadicExpressions -> failwithf ""

        | OperandType.Expression _
        | OperandType.ListVariableExpression _
        | OperandType.Reporter
        | OperandType.Rotation
        | OperandType.Stop
        | OperandType.StopScript
        | OperandType.Variable -> convertPrimitiveExp b operand

    and convertNormalStatementOp b state info (operator, operands) =
        let operands = List.map2 (fun operand spec -> convertOperand b (operand, spec)) operands info.operands
        ComplexExpression(state, operator, operands) |> Builder.accumulateStatement b

    and convertTupleGet b (x, index) = convertExp b x |> List.item index |> List.singleton

    and convertTupleSet b state (var, index, value) =
        match Builder.resolveLocalSpec b var with
        | StorageSpec storage ->
            let value = convertPrimitiveExp b value
            let storage = Builder.setStoragePrimitive b state storage index value
            Builder.accumulateStatement b storage
            []

        | VariableSpec var ->
            let value = convertPrimitiveExp b value
            let var = var.underlyingVariables.[index]
            A.``setVar:to:`` state (UniqueName.name var.underlyingName) value |> Builder.accumulateStatement b
            []

        | ParameterSpec _
        | ListSpec _
        | ProcedureSpec _ -> failwith ""

    and convertListOperand b (operand, spec) =
        match spec with
        | OperandType.Block _
        | OperandType.VariadicExpressions -> failwithf ""

        | OperandType.Expression _
        | OperandType.Reporter
        | OperandType.Rotation
        | OperandType.Stop
        | OperandType.StopScript
        | OperandType.ListVariableExpression _
        | OperandType.Variable ->
            match operand with
            | Choice1Of2 e -> convertPrimitiveExp b e
            | Choice2Of2 listVar ->
                let { listUniqueName = UniqueName list } = Builder.resolveList b listVar.value
                A.eString listVar.source list

    and convertListOp b state (operator, operands) =
        match operator with
        | KnownOperatorInfo ValueNone -> raise <| NotImplementedException()
        | KnownOperatorInfo(ValueSome info) ->

        let operands = List.map2 (fun operand spec -> convertListOperand b (operand, spec)) operands info.operands
        let operation = ComplexExpression(state, operator, operands)

        match info.kind with
        | Kind.Expression -> [Complex operation]
        | Kind.Statement -> Builder.accumulateStatement b operation; []

    and convertCoerce b state (kind, value, newType) =
        let valueType = Exp.varType value
        match kind with
        | CoerceKind.Reinterpret -> convertExp b value
        | CoerceKind.Convert ->

        // `(x: N) :> N` => `x`
        if ExpType.equals valueType newType then convertExp b value

        // `(x: N) :> void` => ``
        elif ExpType.equals newType Types.empty then
            ignoreExpressions <| convertExp b value
            []

        else

        match valueType, newType with
        | UnboxedType valueType, UnboxedType newType ->

        if List.length valueType <> List.length newType then failwith "" else

        let value = convertExp b value

        List.map3 (fun v t t' ->

            // `(x: N) :> N` => `x`
            // `(x: N) :> any` => `x`
            if MemberType.equals t t' || MemberType.equals t' Types.litAny then v

            // `(x: N) :> B` => `not (not x)`
            elif MemberType.equals t' Types.litBool then A.not state (A.not state v)

            // `(x: S) :> N` => `x + 0`
            elif MemberType.equals t' Types.litNumber then A.``+`` state v (A.eNumber state 0.)

            elif MemberType.equals t' Types.litString then A.``concatenate:with:`` state v (A.eString state "")

            else failwith ""

        ) value valueType newType

    and emitAtomicScopeProcedure b state scope =
        let baseName = Builder.procedureQualifiedName b (Builder.config b).naming.atomicWrapperName

        let p = Builder.currentProcedure b
        let var = Var.newProc baseName p.body.parameters ExpTypes.empty

        let spec = Builder.addProcedureSpec b state var p.body.local

        let parameters =
            Builder.declaredParameters b
            |> Seq.collect (fun p -> p.underlyingParameters)
            |> Seq.map (fun p -> ParameterDefinition(state, UniqueName.name p.parameterName, p.parameterDefaultValue))
            |> Seq.toList

        let scopeE = convertExp b scope
        let scopeAcc = Builder.dropAccumulatedStatements b

        let name = UniqueName.name spec.procedureId.name
        let impl = ScriptData.procedure state name parameters Atomic (Seq.toList scopeAcc)
        Builder.accumulateScript b impl
        spec, scopeE
    (*
    `atomic $scope` =>
    accProc: `let proc.atomic ($parameters) = $scope.acc`
    expr: `$scope.expr`
    acc: [proc.atomic ...$parameters]
    *)
    and convertAtom b state scope =
        let parameters =
            Builder.declaredParameters b
            |> Seq.collect (fun p -> p.underlyingParameters)
            |> Seq.cache

        let acc = Builder.dropAccumulatedStatements b
        let proc, e = emitAtomicScopeProcedure b state scope
        Builder.accumulateStatements b acc

        let parameters =
            parameters
            |> Seq.map (fun p -> A.getParam state (UniqueName.name p.parameterName))
            |> Seq.toList

        let call = A.call state (UniqueName.name proc.procedureId.name) parameters
        Builder.accumulateStatement b call
        e

    let convertParameters b ps =
        ps
        |> List.collect (fun { source = state; value = p } ->
            let p = Builder.addVarAsParameter b p

            p.underlyingParameters
            |> List.map (fun p ->
                ParameterDefinition(state, UniqueName.name p.parameterName, p.parameterDefaultValue)
            )
        )

    let convertProcedureBodyWithoutStackOperation b body =
        let proc = Builder.currentProcedure b
        let result = convertExp b body
        let stats = Builder.dropAccumulatedStatements b
        let setResult = Builder.setStorage b body.source proc.body.resultStorage result
        stats.AddRange setResult
        stats

    (*
    `let proc a b = ...HasYield` =>
    `let proc stack a b =
        ensureStack(stack, $localCount)
        ...HasYield
        $popStack(stack, $localCount)
    `
    *)
    let convertProcedureBody b body =
        let state = body.source
        let body = convertProcedureBodyWithoutStackOperation b body

        let proc = Builder.currentProcedure b
        match proc.body.local with
        | VariableLocal _ -> body

        // スタックの確保・解放操作を挿入
        | StaticStackLocal { listUniqueName = UniqueName stack } ->
            let localCount = Builder.localCount b
            if localCount <> 0 then
                let alloc = A.``append:toList:`` state stack (Literal(state, Builder.config(b).uninitValue))
                for _ in 1..localCount do body.Insert(0, alloc)

                let clear =  A.``deleteLine:ofList:`` state stack (A.eString state "last")
                for _ in 1..localCount do body.Add clear

            body

        | ThreadStackLocal { stackMemory = { listUniqueName = UniqueName stackMemory }; stackParameter = stackParameter } ->

        if Builder.localCount b = 0 then body else

        let localCount = A.eNumber state (double (Builder.localCount b))
        let { procedureId = { name = ensureStack } } = Builder.resolveRuntimeProcedure b state EnsureStack
        let { parameterName = stack } = Builder.resolvePrimitiveParameter b stackParameter
        let stack = A.getParam state (UniqueName.name stack)

        // `ensureStack(stack, $localCount)`
        body.Insert(0, A.call state (UniqueName.name ensureStack) [stack; localCount])

        // $popStack: `stackMemory[stack] <- stackMemory[stack] + $localCount`
        body.Add <| A.``setLine:ofList:to:`` state stackMemory stack (A.``+`` state (A.``getLine:ofList:`` state stackMemory stack) localCount)

        body

    let convertHiddenParameters b state =
        let proc = Builder.currentProcedure b
        Builder.procedureHiddenParameters proc.body
        |> List.collect (fun p ->
            let p = Builder.addVarAsParameter b p
            p.underlyingParameters
            |> List.map (fun p ->
                ParameterDefinition(state, UniqueName.name p.parameterName, p.parameterDefaultValue)
            )
        )

    let convertProcedure b { source = state; value = ProcDef(ProcHeader(var, parameters, atomicity), body) } =
        let proc = Builder.resolveProcedure b var
        let name = UniqueName.name proc.procedureId.name
        let b' = Builder.newProcedureBuilder b (ScriptSpec.ofProcedureSpec proc)
        let parameters = convertHiddenParameters b' state @ convertParameters b' parameters
        let body = Seq.toList <| convertProcedureBody b' body
        ProcedureDefinition(state, name, parameters, atomicity, BlockExpression(state, body))

    (*
    `whenGreenFlag [] { ...NoYield }` =>
    `whenGreenFlag [] { ...NoYield }`

    `whenGreenFlag [] { ...HasYield }` =>
    `
    @async let _whenGreenFlag stack =
        call ensureStack(stack, $localCount)
        ...HasYield
        $popStack(stack, $localCount)
        $returnStack(stack)

    whenGreenFlag [] {
        call rentStack()
        call _whenGreenFlag(rentStack_result)
    }
    `
    *)
    let addListenerWrapperProcSpec b state listenerName =
        let c = Builder.config b
        let proc = Var.newVar (c.naming.listenerWrapperName <| Symbol.name listenerName) { parameterTypes = []; resultType = Types.empty }
        Builder.addProcedureSpec b state proc

    let convertListenerWrapperProc b { source = s; value = ListenerDef(name = name; body = body) } =
        let stack = Var.newStorage (Builder.config(b).naming.stackName) false Types.number
        let local = { stackMemory = Builder.resolveRuntimeList b s StackMemory; stackParameter = stack }
        let wrapperProcSpec = addListenerWrapperProcSpec b s name (ThreadStackLocal local)
        let b = Builder.newProcedureBuilder b (ScriptSpec.ofProcedureSpec wrapperProcSpec)
        let parameters = convertHiddenParameters b s
        let body = convertProcedureBody b body

        // ...
        // $returnStack: `stackPool.add stack`
        let { parameterName = stack } = Builder.resolvePrimitiveParameter b stack
        let stack = A.getParam s (UniqueName.name stack)
        let { listUniqueName = stackPool } = Builder.resolveRuntimeList b s StackPool
        body.Add <| A.``append:toList:`` s (UniqueName.name stackPool) stack

        let proc = ProcedureDefinition(s, UniqueName.name wrapperProcSpec.procedureId.name, parameters, NoAtomic, BlockExpression(s, Seq.toList body))
        Builder.accumulateScript b { x = 0.; y = 0.; script = Procedure proc }
        wrapperProcSpec

    (*
    whenGreenFlag [] {
        call rentStack()
        _whenGreenFlag(rentStack_result)
    }
    *)
    let convertListenerBody (BuilderHelpers.EntityState e & b) ({ source = source; value = ListenerDef(body = body) } as listener) =
        match EntityStat.listenerYieldability e.entityStat listener, EntityStat.listenerStackDepth e.entityStat listener with
        | NeverYield, _
        | (ForceYield | NormalYield), Finite 0 ->
            let body = convertProcedureBody b body
            BlockExpression(source, Seq.toList body)

        | (ForceYield | NormalYield), _ ->
            let wrapperProc = convertListenerWrapperProc b listener

            let rentStack = Builder.resolveRuntimeProcedure b source RentStack
            let body = [
                A.call source (UniqueName.name rentStack.procedureId.name) []
                A.call source (UniqueName.name wrapperProc.procedureId.name) (Builder.getStorage b source rentStack.body.resultStorage)
            ]
            BlockExpression(source, body)

    let convertListener b ({ source = source; value = ListenerDef(name, args, _) } as listener) =
        let spec = ScriptSpec.listenerSpec
        let b' = Builder.newProcedureBuilder b spec
        let args = List.collect (convertExp b') args
        let body = convertListenerBody b' listener
        ListenerDefinition(source, name, args, body)

    // `do whenGreenFlag () \{ var <- $init }`
    let convertVariableInit b (VariableInitDef(var, init)) =
        let source = init.source

        // TODO:
        let var = { var with trivia = { var.trivia with varTrivia = { isMutable = true; varType = Var.varType var } } }
        let body = Exp.varSet source var init

        let listener = ListenerDef(O.whenGreenFlag, [], body) @+source

        convertListener b listener

    let convertScript b = function
        | Top.Procedure proc -> Procedure <| convertProcedure b proc
        | Top.Listener listener -> Listener <| convertListener b listener
        | Top.VariableInit init -> Listener <| convertVariableInit b init

    let convertScriptData b { x = x; y = y; script = script } =
        Builder.accumulateScript b { x = x; y = y; script = convertScript b script }

    let convertVariable b v =
        let spec = Builder.resolveVariable b v.var

        for { underlyingName = n; underlyingType = t } in spec.underlyingVariables do
            Builder.accumulateVariable b {
                state = v.state
                name = UniqueName.name n
                isPersistent = v.isPersistent
                value = SValue.defaultVariableValue t
            }

    // ```
    // do whenGreenFlag () \{
    //     deleteLine:ofList: "all" $listName
    //     append:toList: $init0 $listName
    //     …
    //     append:toList: $initN $listName
    // }
    // ```
    let convertListInit b v =
        let source = v.state
        let list = v.var @+source
        let deleteLine = Exp.Op.``deleteLine:ofList:`` source (Exp.string source "all") list
        let appendItems =
            v.init
            |> IArray.toSeqCopiable
            |> Seq.map (fun x -> Exp.Op.``append:toList:`` source (Exp.lit source x) list)
            |> Seq.toList

        let body = Exp.concat source (deleteLine::appendItems)
        let listener = ListenerDef(O.whenGreenFlag, [], body) @+source
        let init = convertListener b listener
        Builder.accumulateScript b { x = 0.; y = 0.; script = Listener init }

    let convertList b v =
        let spec = Builder.resolveList b v.var

        let view = &v.view
        Builder.accumulateList b {
            listName = UniqueName.name spec.listUniqueName
            state = v.state
            isPersistent = v.isPersistent
            contents' = IArray.empty

            x = view.x
            y = view.y
            height = view.height
            width = view.width
            visible = view.visible
        }
        convertListInit b v

    let declareTop b s =
        let declareProcedureLocal b ({ source = state } as p) =
            let var = ProcDef.var p

            match classifyProcedure b var with
            | NeverYield, NoRecursive, _
            | (ForceYield | NormalYield), (NoRecursive | Recursive), Finite 0 -> VariableLocal

            | NeverYield, Recursive, _ ->
                let config = Builder.config b
                let name = config.naming.combineNamespaceItem(Var.name var, config.naming.stackName)
                let spec = Builder.defineStandardListAndInit b state Self name
                StaticStackLocal spec

            | (ForceYield | NormalYield), (NoRecursive | Recursive), _ ->
                let config = Builder.config b
                let stackMemory = Builder.resolveRuntimeList b state StackMemory
                let stackParameter = Var.newStorage config.naming.stackName false Types.number
                ThreadStackLocal { stackMemory = stackMemory; stackParameter = stackParameter }

        match s.script with
        | Top.Procedure({ source = state } as proc) ->
            let local = declareProcedureLocal b proc
            ignore<ProcedureSpec> <| Builder.addProcedureSpec b state (ProcDef.var proc) local

        | Top.Listener _
        | Top.VariableInit _ -> ()

    let convertEntity b convertExtension (x: IREntityData<'Ex1,_>): EntityData<'Ex2,_> =

        // 宣言
        for v in x.variables do Builder.addVariableSpec b v.var |> ignore
        for l in x.lists do Builder.declareList b l.var |> ignore
        for s in x.scripts do declareTop b s

        // 変換
        for v in x.variables do convertVariable b v
        for l in x.lists do convertList b l
        for s in x.scripts do convertScriptData b s

        let e = convertExtension x.ObjectDataExtension
        {
            objName = x.objName
            costumes = x.costumes
            sounds = x.sounds
            currentCostumeIndex = x.currentCostumeIndex

            variables = Builder.dropAccumulatedVariables b
            lists = Builder.dropAccumulatedLists b
            scripts = Builder.dropAccumulatedScripts b

            ObjectDataExtension = e
        }

    let convertSprite parent x =
        let b = Builder.newEntityBuilder (Builder.config parent) (Some parent) [] (EntityData.mapExtension ignore x)
        convertEntity b id x

    let convertStageChildren parentBuilder x = {
        children =
            x.children
            |> List.map (function
                | Choice1Of3 x -> Choice1Of3 x
                | Choice2Of3 x -> Choice2Of3 <| convertSprite parentBuilder x
                | Choice3Of3 x -> Choice3Of3 x
            )

        penLayerMD5 = x.penLayerMD5
        penLayerID = x.penLayerID
        tempoBPM = x.tempoBPM
        videoAlpha = x.videoAlpha
        info = x.info
    }

    let convertStage config x =
        let b = Builder.newEntityBuilder config (None: _ EntityBuilder option) (StageData.sprites x |> Seq.map (EntityData.mapExtension ignore)) (EntityData.mapExtension ignore x)
        convertEntity b (convertStageChildren b) x

[<AutoOpen>]
module IRToStageDataConverters =
    module IRToStageData =
        let convertExprWith withConfig e =
            let config = withConfig IRToStageData.Config.defaultConfig
            let x = StageData.defaultValue
            let b = Builder.newEntityBuilder config (None: _ EntityBuilder option) (StageData.sprites x |> Seq.map (EntityData.mapExtension ignore)) (EntityData.mapExtension ignore x)
            let b = Builder.newProcedureBuilder b ScriptSpec.listenerSpec
            Emitters.convertExp b e,
            Seq.toList <| Builder.dropAccumulatedStatements b,
            Builder.dropAccumulatedVariables b,
            Builder.dropAccumulatedLists b,
            Builder.dropAccumulatedScripts b

        let convertWith withConfig (x: IRStageData<_>): StageData<_> = Emitters.convertStage (withConfig IRToStageData.Config.defaultConfig) x
        let convert x = convertWith id x
