module Scratch.Detranspiler.Typing
open Scratch
open Scratch.Ast
open Scratch.Primitives
open AstDefinitions
type private T = Ast.TsType
type private O<'t> = 't Ast.OperandType
type private V = VTsType


[<Struct>]
type InferState = {
    maxTVar: int
    typeMap: Map<TVar, V>
}
[<Struct>]
type VTyped<'a> = VTyped of 'a * V
type InferEnv<'a> = {
    inferEnv: 'a DetranspileEnv
    types: V PrimitiveTypes
    gtypes: T PrimitiveTypes

    variableSpecs: Map<string, 'a VTyped VariableData>
    listSpecs: Map<string, 'a VTyped ListVariableData>
    procedureSpecs: Map<string, 'a VTyped ProcedureDefinition>

    inferState: InferState ref

    addUnifyWarning: bool
}
[<Struct>]
type InferProcedureEnv<'a> = {
    procedureEnv: 'a InferEnv
    parameterTypeMap: Map<string, V>
}
let newTVar state =
    let maxTVar = (!state).maxTVar
    state := { !state with maxTVar = Checked.(+) maxTVar 1 }
    TVar maxTVar

let typed' t x = VTyped(x, t)
let vtyped state name x = typed' (V.Var(name, newTVar state)) x

let raiseError env location error = raise <| DetranspileException(env.procedureEnv.inferEnv.config.showLocation location, error, null)
let addWarning env location warning = env.inferEnv.warnings.Add <| DetranspileWarning(location, warning)

let inline tryGetOrAddWarning env location name types error =
    match Map.tryFind name types with
    | ValueSome _ as t -> t
    | ValueNone ->
        addWarning env location <| ErrorWarning(error name)
        ValueNone

let getParameterOrAddWarning env location name = tryGetOrAddWarning env.procedureEnv location name env.parameterTypeMap ParameterNotFound
let getVariableOrAddWarning env location name = tryGetOrAddWarning env.procedureEnv location name env.procedureEnv.variableSpecs VariableNotFound
let getListOrAddWarning env location name = tryGetOrAddWarning env.procedureEnv location name env.procedureEnv.listSpecs ListNotFound
let getProcedureOrAddWarning env location name = tryGetOrAddWarning env location name env.procedureSpecs ProcedureNotFound

let freshName vs baseName =
    let isFreshName vs name = List.forall (fun (n, _) -> n <> name) vs
    if isFreshName vs baseName then baseName else

    let rec aux tvars baseName i =
        let name = baseName + "#" + string i
        if isFreshName tvars name then name else aux tvars baseName (i + 1)

    aux vs baseName 1

let rec freeVarsType typeMap tvars = function
    | V.Named(_, ts) -> List.fold (freeVarsType typeMap) tvars ts
    | V.Or(t1, t2) -> freeVarsType typeMap (freeVarsType typeMap tvars t1) t2
    | V.StringSs _ -> tvars
    | V.Var(varName, var) ->
        match Map.tryFind var typeMap with
        | ValueSome t -> freeVarsType typeMap tvars t
        | ValueNone ->
            if List.exists (fun (_, vt) -> vt = var) tvars then tvars
            else (freshName tvars varName, var)::tvars

let rec generalizeType typeMap tvars = function
    | V.Named(n, ts) -> T.Named(n, List.map (generalizeType typeMap tvars) ts)
    | V.Or(t1, t2) -> T.Or(generalizeType typeMap tvars t1, generalizeType typeMap tvars t2)
    | V.StringSs xs -> T.StringSs xs
    | V.Var(_, var) as t ->
        match Map.tryFind var typeMap with
        | ValueSome t -> generalizeType typeMap tvars t
        | ValueNone ->
            match List.tryFindIndex (fun (_, vt) -> vt = var) tvars with
            | Some i -> T.GVar i
            | None -> failwith $"freevar %A{t}"

let generalize typeMap t =
    let tvars = freeVarsType typeMap [] t |> List.rev
    let t = generalizeType typeMap tvars t
    let vs = List.map (fst >> TypeVar) tvars
    TypeScheme(vs, t)

let rec instantiateWith tvs = function
    | T.Named(n, ts) -> V.Named(n, List.map (instantiateWith tvs) ts)
    | T.Or(t1, t2) -> V.Or(instantiateWith tvs t1, instantiateWith tvs t2)
    | T.GVar i -> List.item i tvs
    | T.StringSs xs -> V.StringSs xs

let typeVarMap env tvs =
    List.map (fun (TypeVar n) -> V.Var(n, newTVar env.inferState)) tvs

let instantiate env (TypeScheme(tvs, t)) =
    instantiateWith (typeVarMap env tvs) t

let instantiateOperands env { typeVariables = tvs; operands = ts; resultType = t } =
    let tvs = typeVarMap env tvs
    let instantiate = instantiateWith tvs
    let instantiateOperandType info =
        match info.operandType with
        | O.Expression t -> O.Expression(instantiate t)
        | O.ListVariableExpression t -> O.ListVariableExpression(instantiate t)
        | O.Variable -> O.Variable
        | O.ProcedureNameAndExpressions -> O.ProcedureNameAndExpressions
        | O.ParameterName -> O.ParameterName
        | O.Block -> O.Block
        | O.StringLiterals ss -> O.StringLiterals ss

    List.map instantiateOperandType ts, instantiate t

let rec occur typeMap v1 = function
    | V.Named(_, ts) -> List.exists (occur typeMap v1) ts

    | V.Var(_, v2) ->
        if v1 = v2 then true else
        match Map.tryFind v2 typeMap with
        | ValueNone -> false
        | ValueSome t2 -> occur typeMap v1 t2

    | V.Or(t1, t2) -> occur typeMap v1 t1 || occur typeMap v1 t2
    | V.StringSs _ -> false

let rec unify' typeMap location t1 t2 =
    match t1, t2 with

    // unify (int | char) (char | int) -> id, []
    // TODO:
    | V.Or(t11, t12), t2 ->
        match unify' typeMap location t11 t2 with
        | typeMap, [] -> typeMap, []
        | _ -> unify' typeMap location t12 t2

    | t1, V.Or(t21, t22) ->
        match unify' typeMap location t1 t21 with
        | typeMap, [] -> typeMap, []
        | _ -> unify' typeMap location t1 t22

    | V.Var(_, v1), V.Var(_, v2) ->
        if v1 = v2 then typeMap, [] else
        match Map.tryFind v1 typeMap, Map.tryFind v2 typeMap with
        | ValueSome t1, _ -> unify' typeMap location t1 t2
        | _, ValueSome t2 -> unify' typeMap location t1 t2
        | ValueNone, ValueNone -> Map.add v1 t2 typeMap, []

    | V.Var(n1, v1), t2 ->
        match Map.tryFind v1 typeMap with
        | ValueSome t1 -> unify' typeMap location t1 t2
        | ValueNone ->
            if occur typeMap v1 t2 then typeMap, [location, UnifyWarning(InfiniteType(n1, v1, t2))] else
            Map.add v1 t2 typeMap, []

    | t1, V.Var(n2, v2) ->
        match Map.tryFind v2 typeMap with
        | ValueSome t2 -> unify' typeMap location t1 t2
        | ValueNone ->
            if occur typeMap v2 t1 then typeMap, [location, UnifyWarning(InfiniteType(n2, v2, t1))] else
            Map.add v2 t1 typeMap, []

    | V.StringSs ts1, V.StringSs ts2 ->
        if Set.ofList ts1 = Set.ofList ts2 then
            typeMap, []
        else
            typeMap, [location, UnifyWarning(DifferenceType(t1, t2))]

    | V.Named(n1, es1), V.Named(n2, es2) ->
        if n1 <> n2 then
            typeMap, [location, UnifyWarning(DifferenceType(t1, t2))]
        else
            let rec aux typeMap errors es1 es2 =
                match es1, es2 with
                | [], [] -> typeMap, []
                | e1::es1, e2::es2 ->
                    let typeMap, errors' = unify' typeMap location e1 e2
                    aux typeMap (errors @ errors') es1 es2

                | _::_, []
                | [], _::_ -> typeMap, (errors @ [location, UnifyWarning(DifferenceType(t1, t2))])

            aux typeMap [] es1 es2

    | V.Named _, V.StringSs _
    | V.StringSs _, V.Named _ -> typeMap, [location, UnifyWarning(DifferenceType(t1, t2))]

let unify ({ inferState = state } as env) location t1 t2 =
    let typeMap, errors = unify' (!state).typeMap location t1 t2
    if env.addUnifyWarning then
        for location, error in errors do addWarning env location error
    state := { !state with typeMap = typeMap }

let unifyInProcedure env location t1 t2 = unify env.procedureEnv location t1 t2
let unifyToString env location t = unifyInProcedure env location t env.procedureEnv.types.tString
let unifyToUnit env location t = unifyInProcedure env location t env.procedureEnv.types.tUnit

type N = System.Globalization.NumberStyles
let sValueType ({ types = types } as env) = function
    | SBool _ -> types.tBoolean
    | SNumber _ -> types.tNumber
    | SString v ->
        match env.inferEnv.config, v with
        | { treatStringAsNumberIfPossible = true }, TryParseSNumber _ -> types.tNumber
        | { treatStringAsBoolIfPossible = true }, TryParseSBool _ -> types.tBoolean
        | _ -> types.tString

let tryZip xs1 xs2 = if List.length xs1 <> List.length xs2 then None else List.zip xs1 xs2 |> Some

let expressionLocation e = let (VTyped(state, _)) = Expression.state e in state
let blockLocation (BlockExpression(state = VTyped(state, _))) = state

let procedureType (ProcedureDefinition(name = n; parameters = ps)) = 
    let n = $"proc.<{n}>"
    let pts = ps |> Seq.map (fun (ParameterDefinition(state = VTyped(_, t))) -> t) |> Seq.toList
    V.Named(n, pts)

let rec inferOperand env location operator operand operandIndex operandType =
    match operandType, operand with
    | O.Block, Block block -> inferBlock env block
    | O.Expression t, expr -> unifyInProcedure env location (inferExpression env expr) t

    | O.ListVariableExpression t, Literal(VTyped(location, varType), SString listName) ->
        unifyToString env location varType
        match getListOrAddWarning env location listName with
        | ValueNone -> ()
        | ValueSome { ListVariableData.state = VTyped(_, vtype) } ->
            unifyInProcedure env location vtype t

    | O.StringLiterals ss, Literal(VTyped(l, t), SString s) when Set.contains s ss ->
        unifyToString env l t

    | O.Expression _, _
    | O.Block, _
    | O.ListVariableExpression _, _
    | O.StringLiterals _, _
    | O.ProcedureNameAndExpressions, _
    | O.ParameterName, _
    | O.Variable, _  ->
        raiseError env location <| UnexpectedOperandType(Symbol.name operator, operandIndex, expectedOperandType = operandType)

and inferOperation env location operator operands =
    match Map.tryFind operator knownAllOperatorMap with
    | ValueNone -> raiseError env location <| UnknownOperation (Symbol.name operator)
    | ValueSome operationType ->
        let operandTypes, resultType = instantiateOperands env.procedureEnv operationType
        match tryZip operands operandTypes with
        | None -> raiseError env location <| UnknownOperation (Symbol.name operator)
        | Some xs ->
            xs
            |> List.iteri (fun i (operand, operandType) ->
                let location = expressionLocation operand
                inferOperand env location operator operand i operandType
            )
        resultType

and inferExpression env = function
    | Literal(VTyped(location, vt), x) ->
        let t = sValueType env.procedureEnv x
        unifyInProcedure env location t vt
        t

    | Complex e -> inferComplexExpression env e
    | Block x ->
        inferBlock env x
        env.procedureEnv.types.tUnit

and inferComplexExpression env (ComplexExpression(VTyped(location, vtype), operator, operands)) =
    let t =
        match operator, operands with
        | O.getParam, [Literal(VTyped(varL, varType), SString paramName); Literal(VTyped(reporterL, reporterType), SString "r")] ->
            unifyToString env varL varType
            unifyToString env reporterL reporterType
            match getParameterOrAddWarning env varL paramName with
            | ValueSome v -> v

            // パラメータが存在しないとき
            | ValueNone -> V.Var(paramName, newTVar env.procedureEnv.inferState)

        | O.readVariable, [Literal(VTyped(location, varType), SString varName)] ->
            unifyToString env location varType
            match getVariableOrAddWarning env location varName with
            | ValueSome { VariableData.state = VTyped(_, vtype) } -> vtype

            // 変数が存在しないとき
            | ValueNone -> V.Var(varName, newTVar env.procedureEnv.inferState)

        | O.``getAttribute:of:``, [Literal(_, SString("x position" | "y position" | "direction" | "costume #" | "size" | "volume" | "background #" | "backdrop #")); _] -> env.procedureEnv.types.tNumber
        | O.``getAttribute:of:``, [Literal(_, SString("costume name" | "backdrop name")); _] -> env.procedureEnv.types.tString

        // "ToString": 'a. 'a -> string
        | O.``concatenate:with:``, ([Literal(_, SString "") as empty; x] | [x; Literal(_, SString "") as empty]) ->
            ignore <| inferExpression env empty
            ignore <| inferExpression env x
            env.procedureEnv.types.tString

        // "ToNumber": 'a. 'a -> number
        | O.``+``, ([Literal(_, SNumber 0.) as zero; x] | [x; Literal(_, SNumber 0.) as zero]) ->
            ignore <| inferExpression env zero
            ignore <| inferExpression env x
            env.procedureEnv.types.tNumber

        | _ -> inferOperation env location operator operands

    unifyInProcedure env location vtype t
    t

and inferStatement env (ComplexExpression(VTyped(location, statementType), operator, operands)) =
    unifyToUnit env location statementType

    match operator, operands with
    // `call %name (...)`
    | O.call, Literal(VTyped(location, varType), SString name)::args ->
        unifyToString env location varType

        match getProcedureOrAddWarning env.procedureEnv location name with
        | ValueNone -> ()
        | ValueSome(ProcedureDefinition(state = VTyped(_, procType); name = name; parameters = parameters) as p) ->
            let procType' = procedureType p
            unifyInProcedure env location procType procType'

            match tryZip args parameters with
            | None -> raiseError env location <| ParameterCountDifference name
            | Some xs ->

            for arg, ParameterDefinition(state = VTyped(_, vtype)) in xs do
                match arg with
                | Block block ->

                    // Expression の引数にブロックが現れる事はないが対応
                    addWarning env.procedureEnv (blockLocation block) <| ErrorWarning UnexpectedBlock
                    inferBlock env block

                | arg ->
                    let location = expressionLocation arg
                    unifyInProcedure env location (inferExpression env arg) vtype

    // `%var <- %value`
    | O.``setVar:to:``, [Literal(VTyped(location, varType), SString varName); value] ->
        unifyToString env location varType
        let valueType = inferExpression env value
        match getVariableOrAddWarning env location varName with
        | ValueNone -> ()
        | ValueSome { VariableData.state = VTyped(_, vtype) } ->
            let location = expressionLocation value
            unifyInProcedure env location vtype valueType

    // `%var += %value`
    | O.``changeVar:by:``, [Literal(VTyped(location, varType), SString var); value] ->
        unifyToString env location varType
        let valueType = inferExpression env value
        match getVariableOrAddWarning env location var with
        | ValueNone -> ()
        | ValueSome { VariableData.state = VTyped(_, vtype) } -> unifyInProcedure env location vtype env.procedureEnv.types.tNumber
        let location = expressionLocation value
        unifyInProcedure env location valueType env.procedureEnv.types.tNumber

    | (O.``hideVariable:`` | O.``showVariable:``), [Literal(VTyped(location, varType), SString _)] ->
        unifyToString env location varType

    | _ ->
        let resultType = inferOperation env location operator operands
        unifyToUnit env location resultType

and inferBlock env (BlockExpression(VTyped(location, returnType), ss)) =
    for s in ss do inferStatement env s
    unifyToUnit env location returnType
    
let knownListenerHeaderMap = knownListenerHeaders |> Map.ofSeq
let inferListenerDefinition env (ListenerDefinition(VTyped(location, returnType), name, args, body)) =
    match Map.tryFind name knownListenerHeaderMap with
    | ValueNone -> addWarning env location <| ErrorWarning(UnknownOperation(Symbol.name name))
    | ValueSome operandTypes ->

    match tryZip operandTypes args with
    | None -> addWarning env location <| ErrorWarning(UnknownOperation(Symbol.name name))
    | Some xs ->
        for operandType, operand in xs do
            let location = expressionLocation operand

            match operandType, operand with
            | ListenerHeaderType.AnyOrKeyName, Literal(VTyped(l, t), SString _) -> unify env l t env.types.tString
            | ListenerHeaderType.Bool, Literal(VTyped(l, t), SBool _) -> unify env l t env.types.tBoolean
            | ListenerHeaderType.EventName, Literal(VTyped(l, t), SString _) -> unify env l t env.types.tString
            | ListenerHeaderType.Null, Block(BlockExpression(VTyped(l, t), [])) -> unify env l t env.types.tUnit
            | ListenerHeaderType.String, Literal(VTyped(l, t), SString _) -> unify env l t env.types.tString

            | ListenerHeaderType.AnyOrKeyName, _
            | ListenerHeaderType.Bool, _
            | ListenerHeaderType.EventName, _
            | ListenerHeaderType.Null, _
            | ListenerHeaderType.String, _ -> addWarning env location <| ErrorWarning(UnknownOperation(Symbol.name name))

    inferBlock { procedureEnv = env; parameterTypeMap = Map.empty } body
    unify env location returnType env.types.tUnit

let extendOfVariableAndLists env data =
    { env with
        variableSpecs = data.variables |> List.fold (fun map v -> Map.add v.name v map) env.variableSpecs
        listSpecs = data.lists |> List.fold (fun map v -> Map.add v.listName v map) env.listSpecs

        // procedure は拡張しない
        procedureSpecs =
            data.scripts
            |> List.fold (fun map s ->
                match s.script with
                | Procedure(ProcedureDefinition(name = name) as p) -> Map.add name p map
                | _ -> map
            ) Map.empty
    }

let inferProcedure env (ProcedureDefinition(state = VTyped(location, procType); parameters = parameters; body = body) as p) =
    let parameterTypeMap =
        parameters
        |> Seq.map (fun (ParameterDefinition(VTyped(_, vtype), name, _) ) -> name, vtype)
        |> Map.ofSeq

    inferBlock { procedureEnv = env; parameterTypeMap = parameterTypeMap } body

    let procType' = procedureType p
    unify env location procType procType'

let inferEntity env data =
    for s in data.scripts do
        match s.script with
        | Procedure p -> inferProcedure env p
        | Listener s -> inferListenerDefinition env s
        | Expression _
        | Statements _ -> ()

    // variable と variable の初期値を単一化 ( 初期値はユーザーが簡単に設定できないので警告を出さない )
    for { state = VTyped(location, vtype); value = value } in data.variables do
        unify { env with addUnifyWarning = false } location vtype (sValueType env value)

    // list と list の初期値を単一化 ( 初期値はユーザーが簡単に設定できないので警告を出さない )
    for { state = VTyped(location, vtype); contents' = contents } in data.lists do
        for x in IArray.toSeqCopiable contents do
            unify { env with addUnifyWarning = false } location vtype (sValueType env x)

    // expression と statements の型を推論 ( 実行されないので警告を出さない )
    for s in data.scripts do
        match s.script with
        | Procedure _
        | Listener _ -> ()
        | Expression body -> inferComplexExpression { procedureEnv = { env with addUnifyWarning = false }; parameterTypeMap = Map.empty } body |> ignore
        | Statements body -> inferBlock { procedureEnv = { env with addUnifyWarning = false }; parameterTypeMap = Map.empty } body

let rec freeVarsExpression typeMap tvars = function
    | Literal _ -> tvars
    | Complex x -> freeVarsComplexExpression typeMap tvars x
    | Block x -> freeVarsBlock typeMap tvars x

and freeVarsComplexExpression typeMap tvars (ComplexExpression(VTyped(_, t), _, ops)) =
    let tvars = freeVarsType typeMap tvars t
    List.fold (freeVarsExpression typeMap) tvars ops

and freeVarsBlock typeMap tvars (BlockExpression(_, ss)) = List.fold (freeVarsComplexExpression typeMap) tvars ss

let typeScheme0 t = TypeScheme([], t)
let generalizeTypeToScheme typeMap tvars t = typeScheme0 <| generalizeType typeMap tvars t

let rec generalizeExpression typeMap tvars = function
    | Literal(VTyped(s, t), x) -> Literal(struct(s, generalizeTypeToScheme typeMap tvars t), x)
    | Complex x -> Complex(generalizeComplexExpression typeMap tvars x)
    | Block x -> Block(generalizeBlock typeMap tvars x)

and generalizeComplexExpression typeMap tvars (ComplexExpression(VTyped(s, t), op, ops)) =
    ComplexExpression(
        struct(s, generalizeTypeToScheme typeMap tvars t),
        op,
        List.map (generalizeExpression typeMap tvars) ops
    )

and generalizeBlock typeMap tvars (BlockExpression(VTyped(state, t), ss)) =
    BlockExpression(
        struct(state, generalizeTypeToScheme typeMap tvars t),
        List.map (generalizeComplexExpression typeMap tvars) ss
    )

let generalizeProcedure typeMap (ProcedureDefinition(VTyped(location, procType), name, parameters, isAtomic, body)) =
    let tvars =
        let tvars = []
        let tvars = freeVarsType typeMap tvars procType
        let tvars = List.fold (fun tvars (ParameterDefinition(state = VTyped(_, t))) -> freeVarsType typeMap tvars t) tvars parameters
        let tvars = freeVarsBlock typeMap tvars body
        tvars

    let procType = TypeScheme(List.map (fst >> TypeVar) tvars, generalizeType typeMap tvars procType)
    let parameters =
        parameters
        |> List.map (fun (ParameterDefinition(VTyped(location, t), name, value)) ->
            let t = typeScheme0 <| generalizeType typeMap tvars t
            ParameterDefinition(struct(location, t), name, value)
        )
    let body = generalizeBlock typeMap tvars body
    ProcedureDefinition(struct(location, procType), name, parameters, isAtomic, body)

let generalizeListener env (ListenerDefinition(VTyped(location, _), name, args, body)) =
    let typeMap = env.inferState.contents.typeMap

    let tvars = []
    let tvars = List.fold (fun tvars -> function Block(BlockExpression(_, [])) -> tvars | x -> freeVarsExpression typeMap tvars x) tvars args
    let tvars = freeVarsBlock typeMap tvars body

    let args = args |> List.map (generalizeExpression typeMap tvars)
    let body = generalizeBlock typeMap tvars body

    match tvars with
    | _::_ ->
        let t = TypeScheme(tvars |> List.map (fst >> TypeVar), env.gtypes.tUnit)
        addWarning env location <| UnifyWarning(MonomorphismRestriction t)
    | _ -> ()

    // TODO:
    ListenerDefinition(struct(location, typeScheme0 env.gtypes.tUnit), name, args, body)

let generalizeEntity generalizeExtension env data =
    let typeMap = env.inferState.contents.typeMap
    let generalizeTopLevel (VTyped(location, vtype)) =
        let t = generalize typeMap vtype
        match t with
        | TypeScheme(_::_, _) -> addWarning env location <| UnifyWarning(MonomorphismRestriction t)
        | _ -> ()
        struct(location, t)
    {
        objName = data.objName
        variables = data.variables |> List.map (VariableData.map generalizeTopLevel)
        lists = data.lists |> List.map(ListData.map generalizeTopLevel)
        scripts =
            data.scripts
            |> List.map (fun s ->
                {
                    x = s.x
                    y = s.y
                    script =
                        match s.script with
                        | Procedure x -> Procedure <| generalizeProcedure typeMap x
                        | Listener x -> Listener <| generalizeListener env x
                        | Expression x ->

                            // 実行されないので MonomorphismRestriction 警告は出さない
                            let tvars = freeVarsComplexExpression typeMap [] x
                            Expression <| generalizeComplexExpression typeMap tvars x

                        | Statements x ->

                            // 実行されないので MonomorphismRestriction 警告は出さない
                            let tvars = freeVarsBlock typeMap [] x
                            Statements <| generalizeBlock typeMap tvars x
                }
            )
        costumes = data.costumes
        sounds = data.sounds
        currentCostumeIndex = data.currentCostumeIndex
        ObjectDataExtension = generalizeExtension data.ObjectDataExtension
    }

let generalizeStageDataExtension env data = {
    children =
        data.children
        |> List.map (function
            | Choice1Of3 x -> Choice1Of3 x
            | Choice2Of3 x -> Choice2Of3 <| generalizeEntity id env x
            | Choice3Of3 x -> Choice3Of3(ListData.map (fun (VTyped(x, _)) -> struct(x, typeScheme0 env.gtypes.tUnit)) x)
        )

    penLayerMD5 = data.penLayerMD5
    penLayerID = data.penLayerID
    tempoBPM = data.tempoBPM
    videoAlpha = data.videoAlpha
    info = data.info
}

let generalizeStage env data = generalizeEntity (generalizeStageDataExtension env) env data

let infer env data =
    let inferState = ref {
        maxTVar = 0
        typeMap = Map.empty
    }
    let env = {
        types = VTsType.types
        gtypes = TsType.types
        inferEnv = env
        variableSpecs = Map.empty
        listSpecs = Map.empty
        procedureSpecs = Map.empty
        inferState = inferState

        addUnifyWarning = true
    }
    let data = StageData.map (vtyped inferState "") data

    let env = extendOfVariableAndLists env data
    inferEntity env data

    for data in StageData.sprites data do
        let env = extendOfVariableAndLists env data
        inferEntity env data

    generalizeStage env data
