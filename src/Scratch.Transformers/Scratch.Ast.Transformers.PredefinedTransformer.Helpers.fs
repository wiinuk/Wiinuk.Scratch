[<AutoOpen>]
module internal Scratch.Ast.Transformers.PredefinedTransformer.Helpers
open Scratch
open Scratch.Ast
open Scratch.AstDefinitions
open Scratch.Primitives
open Scratch.Reflection
open Scratch.Transformers
open Scratch.Ast.Transformers
module R = Scratch.Transformers.TransformResult


[<Struct>]
type PartialActivePatternResult<'T> =
    /// `ValueNone`
    | N
    /// `ValueSome x`
    | Y of 'T

[<Struct>]
type PartialActivePatternResult<'T1,'T2> =
    /// `ValueNone`
    | N2
    /// `ValueSome struct(x1, x2)`
    | Y2 of 'T1 * 'T2

let (|ReadVariable|) = function
    | ComplexExpression(operator = O.readVariable; operands = [Literal(_, SString valueVarName)]) ->
        Y valueVarName

    | _ -> N

let (|Add|) = function
    | Complex(ComplexExpression(operator = O.``+``; operands = [e1; e2])) -> Y2(e1, e2)
    | _ -> N2

let (|Sub|) = function
    | Complex(ComplexExpression(operator = O.``-``; operands = [e1; e2])) -> Y2(e1, e2)
    | _ -> N2

let (|Mul|) = function
    | Complex(ComplexExpression(operator = O.``*``; operands = [e1; e2])) -> Y2(e1, e2)
    | _ -> N2

let (|Div|) = function
    | Complex(ComplexExpression(operator = O.``/``; operands = [e1; e2])) -> Y2(e1, e2)
    | _ -> N2

let (|Number|) = function
    | Literal(_, SNumber x) -> Y x
    | _ -> N

let hasExportTag tagged = Tagged.contains ExportTag tagged

let removeTags x = Tagged.withTags Tags.empty x

let operatorCost name =
    Map.tryFind name knownAllOperatorMap
    |> VOption.map (fun op -> op.operatorCost)
    |> VOption.defaultValue Unknown

let knownExpressionResultTypeMap = 
    let rec resultTypeToSType = function
        | n when n = TsType.gNumber -> Some SType.N
        | n when n = TsType.gBoolean -> Some SType.B
        | n when n = TsType.gString -> Some SType.S

        | TsType.StringSs _ -> Some SType.S
        | TsType.GVar _ -> None
        | TsType.Or(t1, t2) ->
            let t1 = resultTypeToSType t1
            let t2 = resultTypeToSType t2
            if t1 = t2 then t1 else None

        | _ -> None

    knownAllOperatorMap
    |> Map.toSeq
    |> Seq.choose (fun (k, op) -> resultTypeToSType op.resultType |> Option.map (fun t -> k, t))
    |> Map.ofSeq

let isNumberWithoutNaN e =
    let complex (ComplexExpression(operator = operator)) =
        match Map.tryFind operator knownExpressionResultTypeMap with
        | ValueSome SType.N ->

            // a / b
            match operator with
            | O.xpos
            | O.ypos
            | O.heading
            | O.costumeIndex
            | O.backgroundIndex
            | O.scale
            | O.volume
            | O.tempo
            | O.``lineCountOfList:``
            | O.``stringLength:``

            | O.mouseX
            | O.mouseY
            | O.timer
            | O.timestamp
            | O.timeAndDate -> true

            // `∞ + (−∞)`, `(−∞) + ∞`
            | O.``+``
            // `∞ − ∞`, `(−∞) - (-∞)`
            | O.``-``
            // `0 * ±∞`, `±∞ * 0`
            | O.``*``
            // `0 / 0`, `±∞ / ±∞`
            | O.``/``

            // `randomFrom:to: ±∞ x`, ...
            | O.``randomFrom:to:``

            | O.abs
            | O.sqrt
            | O.``%``
            | O.``\\``
            | O.rounded
            | O.``computeFunction:of:``
            | O.``distanceTo:``
            | _ -> false
        | _ -> false

    let svalue = function
        | SNumber x when System.Double.IsNaN x -> false
        | SNumber _ -> true
        | SString _
        | SBool _ -> false

    let expression = function
        | Literal(value = v) -> svalue v
        | Block _ -> false
        | Complex x -> complex x

    expression e

let maxCostWith maxCost customCost e =
    let rec collectMax currentCost maxCost measure' xs =
        if maxCost <= currentCost then currentCost else

        match xs with
        | [] -> currentCost
        | x::xs -> collectMax (max (measure' x) currentCost) maxCost measure' xs

    let rec expression = function
        | Literal _ -> LiteralLike
        | Block x -> block x
        | Complex x -> complex x
    
    and block (BlockExpression(body = body)) = collectMax Cost.min maxCost complex body
    and complex c =
        match customCost c with
        | ValueSome c -> c
        | ValueNone ->
            let (ComplexExpression(operator = operator; operands = operands)) = c
            collectMax (operatorCost operator) maxCost expression operands

    expression e

let maxCost e = maxCostWith Cost.max (fun _ -> ValueNone) e

let maxCostWithoutReadVariable e =
    e
    |> maxCostWith HasSideEffect (function
        | ReadVariable(Y _) -> ValueSome Cost.min
        | _ -> ValueNone
    )

let hasVariableDefinitionAtSelf varName self = self.variables |> List.exists (fun v -> v.name = varName)


let getOrAnalyzeScript s =
    match Script.state s |> Tagged.tryFind ExpressionStatisticsTag with
    | ValueSome s -> s.Value
    | ValueNone ->
        failwith "variable statistics not found"
        analyzeScript s

let hasProcedureReference procedureName script =
    let s = getOrAnalyzeScript script
    match Map.tryFind procedureName s.procedureStatistics with
    | ValueNone -> false
    | ValueSome s -> 1 <= s.callCount

let hasProcedureReferenceInEntity procedureName data =
    data.scripts
    |> List.exists (fun s -> hasProcedureReference procedureName s.script)

let hasVariableInScript varName s =
    let s = getOrAnalyzeScript s
    match Map.tryFind varName s.variableStatistics with
    | ValueSome x -> x.metaCount + x.writeCount + x.readCount <> 0
    | ValueNone -> false

// 変数がプロシージャのみで参照されているか判定する
let isLocal varName env =
    let BlockEnvs.Member { self = self } & BlockEnvs.Entity { EntityEnv.children = children } & BlockEnvs.Script script = env
    
    // TODO: 現在のエンティティに登録されていない変数 ( スプライト内のステージ変数への参照 ) は今のところ除外する
    let isStageVariableInSprite = hasVariableDefinitionAtSelf varName self |> not
    if isStageVariableInSprite then false else

    // まずエンティティに登録されているスクリプトから探す
    let hasVariableReference =

        // 変数へ参照があるスクリプトを探す
        self.scripts
        |> Seq.indexed
        |> Seq.exists (fun (i, s) ->
            match s.script, script.scriptId with
    
            // 現在のプロシージャは除外する
            | Listener _, ScriptIndex i' when i = i' -> false
            | Procedure(ProcedureDefinition(name = n)), ProcedureName n' when n = n' -> false
    
            | script, _ -> hasVariableInScript varName script
        )

    if hasVariableReference then false else
    
    // 子スプライトから探す
    let hasVariableInChildren =
        children
        |> List.exists (function
            | Choice2Of3 entity ->
    
                // スプライトの同名の変数によって隠されているので探さない
                if entity.variables |> Seq.exists (fun v -> v.name = varName) then false else
    
                entity.scripts
                |> List.forall (fun s -> Script.hasVariable varName s.script)

            | _ -> false
        )
    not hasVariableInChildren

let isSingleAssignInLocal varName env =
    let isAssignAfterReadInLocal varName env =
        let (BlockEnvs.Procedure { procedureBody = body }) = env
        body
        |> Block
        |> Expression.flow
            (fun state1 state2 -> Ok(state1 && state2))
            (fun initialized -> function
                | Complex(ComplexExpression(operator = operator; operands = operands)) ->
                    match operator, operands with
                    | O.readVariable, [Literal(_, SString n)] when n = varName -> if initialized then Ok initialized else Error()
                    | O.``changeVar:by:``, [Literal(_, SString n); _] when n = varName -> Error()
                    | O.``setVar:to:``, [Literal(_, SString n); _] when n = varName -> if initialized then Error() else Ok true
                    | _ -> Ok initialized
                | _ -> Ok initialized
            ) false
    
        |> Result.isOk

    let isSingleAssignInLocal' varName env =
        let (BlockEnvs.Procedure { procedureScript = script }) = env
        let s = getOrAnalyzeScript script
        match Map.tryFind varName s.variableStatistics with
        | ValueNone -> true
        | ValueSome x -> x.writeCount <= 1

    isSingleAssignInLocal' varName env && isAssignAfterReadInLocal varName env

// 局所単一代入かどうか判定する
//     変数がプロシージャのみで参照されている
//     変数への代入が1回のみ
//     変数への代入前に参照されない
let isLocalSingleAssignment varName env =
    let isLocal = isLocal varName env
    if isLocal then
        isSingleAssignInLocal varName env
    else
        false

let replaceVar varName value es =
    let rec expression e =
        match e with
        | Literal _ -> R.noModify e
        | Block x -> block x |> R.mapIfModified e Block
        | Complex(ComplexExpression(operator = O.readVariable; operands = [Literal(_, SString n)])) when n = varName -> R.modified value
        | Complex x -> complex x |> R.mapIfModified e Complex
    
    and block (BlockExpression(state, body) as e) =
        R.mapList complex body
        |> R.mapIfModified e (fun body -> BlockExpression(state, body))
    
    and complex (ComplexExpression(state, operator, operands) as e) =
        R.mapList expression operands
        |> R.mapIfModified e (fun operands -> ComplexExpression(state, operator, operands))
    
    R.mapList complex es

let readVariables e = Expression.fold (fun vs -> function ReadVariable(Y n) -> n::vs | _ -> vs) [] e

let replaceVarToVarIfNotAssigned oldVar newValue e =
    let vars = readVariables newValue

    let rec expression e =
        match e with
        | Literal _ -> R.noModify e
        | Block x -> block x |> R.mapIfModified e Block
        
        // 対象の変数を置換する
        | Complex(ComplexExpression(operator = O.readVariable; operands = [Literal(_, SString n)])) when n = oldVar -> R.modified newValue

        | Complex x -> complex x |> R.mapIfModified e Complex
    
    and complexes body = R.mapList complex body
    
    and block (BlockExpression(state, body) as e) =
        complexes body
        |> R.mapIfModified e (fun body -> BlockExpression(state, body))
    
    and complex (ComplexExpression(state, operator, operands) as e) =
        match operator, operands with
        
        // 変数への代入が見つかった
        | (O.``changeVar:by:`` | O.``setVar:to:``), [Literal(_, SString n) as nameE; x1] when n = oldVar || List.contains n vars ->
            expression x1
            |> R.mapIfModified e (fun x1 -> ComplexExpression(state, operator, [nameE; x1]))
            |> R.withIsSkip true
        
        // 変数への代入があるかもしれない
        // TODO: プロシージャの実装を見て判断することもできる
        | O.call, _
        
        // 他のスクリプトに実行を譲るブロック
        | O.startSceneAndWait, _
        | O.``say:duration:elapsed:from:``, _
        | O.``think:duration:elapsed:from:``, _
        | O.doPlaySoundAndWait, _
        | O.``rest:elapsed:from:``, _
        | O.``noteOn:duration:elapsed:from:``, _
        | O.doBroadcastAndWait, _
        | O.``glideSecs:toX:y:elapsed:from:``, _
        | O.``wait:elapsed:from:``, _
        | O.doAsk, _ ->
            R.mapList expression operands
            |> R.mapIfModified e (fun operands -> ComplexExpression(state, operator, operands))
            |> R.withIsSkip true

        // 他のスクリプトに実行を譲るブロック
        | O.doWaitUntil, _ -> R.skip e
        
        // TODO: atomic な場合は解析を続けられる
        | (O.doForeverIf | O.doWhile), [_test; Block _ifTrue] -> R.skip e
        
        | O.doIf, [test; Block ifTrue] ->
            R.sequence2 expression block struct(test, ifTrue)
            |> R.mapIfModified e (fun struct(test, ifTrue) -> ComplexExpression(state, operator, [test; Block ifTrue]))
        
        | O.doIfElse, [test; Block ifTrue; Block ifFalse] ->
            R.sequence2 expression (R.parallel2 block block) struct(test, struct(ifTrue, ifFalse))
            |> R.mapIfModified e (fun struct(test, struct(ifTrue, ifFalse)) -> ComplexExpression(state, operator, [test; Block ifTrue; Block ifFalse]))
        
        // 他のスクリプトに実行を譲るブロック
        // TODO: atomic な場合は解析を続けられる
        | O.doRepeat, [_count; Block _body] -> R.skip e
        
        // TODO: 解析を中断することもできる
        | O.doReturn, [] -> R.skip e
        
        // 他のスクリプトに実行を譲るブロック
        // TODO: atomic な場合は解析を続けられる
        | O.doUntil, [_test; Block _ifFalse] -> R.skip e
        | O.doWaitUntil, [_test] -> R.skip e
        | O.doForever, [Block _body] -> R.skip e
        
        // NOTE: 解析を中断することもできる
        | O.stopAll, [] -> R.noModify e
        | O.stopScripts, [_stopKind] -> R.noModify e
        | O.deleteClone, [] -> R.noModify e
        | _ ->
        
        R.mapList expression operands
        |> R.mapIfModified e (fun operands -> ComplexExpression(state, operator, operands))
    
    complexes e

let (|SetVar_To_|) = function
    | ComplexExpression(operator = O.``setVar:to:``; operands = [Literal(_, SString varName); value]) ->
        Y2(varName, value)
    | _ -> N2

let hasReferenceAtChildren varName children =
    children
    |> List.exists (function
        | Choice2Of3 s ->

            // スプライトの同名の変数によって隠されていない
            s.variables |> List.forall (fun v -> v.name <> varName) &&

            s.scripts |> List.exists (fun d -> hasVariableInScript varName d.script)
        | _ -> false
    )

let hasReferenceAtSelf varName self = self.scripts |> List.exists (fun d -> hasVariableInScript varName d.script)

let isExportVarData = function
    | { VariableData.isPersistent = Persistent } -> true
    | v -> hasExportTag v.state

let definedVariable varName entity = entity.variables |> List.tryFind (fun v -> v.name = varName)
let resolveVariable varName env =
    let BlockEnvs.Entity { parent = parent } & BlockEnvs.Member { self = self } = env
    let definitionAtSelf = definedVariable varName self
    let varDef =
        match definitionAtSelf with
        | Some _ as r -> r
        | _ ->

        match parent with
        | None -> None
        | Some parent ->

        definedVariable varName parent

    varDef, Option.isSome definitionAtSelf

let hasVariableReadAtSelf varName self =
    self.scripts
    |> List.exists (fun s ->
        let st = getOrAnalyzeScript s.script
        match Map.tryFind varName st.variableStatistics with
        | ValueNone -> false
        | ValueSome v -> 0 < v.readCount
    )
let hasVariableReadAtChildren varName children =
    children
    |> List.exists (function
        | Choice2Of3 s ->

            // スプライトの同名の変数によって隠されていない
            s.variables |> List.forall (fun v -> v.name <> varName) &&

            hasVariableReadAtSelf varName s

        | _ -> false
    )

let hasSelfCall = function
    | Procedure(ProcedureDefinition(name = n)) as p ->
        let stats = getOrAnalyzeScript p
        match Map.tryFind n stats.procedureStatistics with
        | ValueNone -> false
        | ValueSome s -> 0 < s.callCount

    | _ -> false

let scriptSize s = getOrAnalyzeScript(s).scriptSize

[<AutoOpen>]
module Evaluations =
    open Scratch.Evaluator
    open Scratch.Evaluator.Expressions

    let newDefaultBlockState() =
        let config = {
            EvaluateConfig.makeDefault() with
                randomNext = fun _ -> failwithf "randomNext is invalid operation"
                randomNextDouble = fun _ -> failwithf "randomNextDouble is invalid operation"
        }
        let stage = StageData.defaultValue
        let state = EvaluateState.ofStageData config stage
        let state = {
            blockState = state
            self = state.stage
            isAtomic = true
            args = Map.empty
            callStack = []
        }
        state

    [<AbstractClass; Sealed>]
    type private BlockStateHolder<'a> private () =
        static let value: BlockState<'a,_,_> = newDefaultBlockState()
        static member Value = value

    let evaluateExpressionWithGlobalContext e =
        let state = BlockStateHolder.Value
        try Ok <| evaluateExpression &state e
        with e -> Error e

let isRootScript = function
    | Listener _
    | Expression _
    | Statements _ -> true
    | Procedure _ as s when hasExportTag (Script.state s) -> true
    | _ -> false

let findProcedure name self =
    self.scripts
    |> List.tryFind (function { script = Procedure(ProcedureDefinition(name = name')) } -> name' = name | _ -> false)

let rec collectAliveProcedureReferences aliveSet self = function
    | Procedure(ProcedureDefinition(name = name)) when Set.contains name aliveSet -> aliveSet
    | script ->

    let aliveSet =
        match script with
        | Procedure(ProcedureDefinition(name = name)) -> Set.add name aliveSet
        | _ -> aliveSet

    getOrAnalyzeScript(script).procedureStatistics
    |> Map.fold (fun aliveSet name stat ->
        if 1 <= stat.callCount then
            match findProcedure name self with
            | None -> aliveSet
            | Some s -> collectAliveProcedureReferences aliveSet self s.script
        else
            aliveSet
    ) aliveSet

let yieldNotContanins block =
    block
    |> BlockExpression.tryPick (fun (ComplexExpression(operator = operator)) ->
        match operator with

        // TODO: プロシージャの実装を見て判断することもできる
        | O.call

        | O.startSceneAndWait
        | O.``say:duration:elapsed:from:``
        | O.``think:duration:elapsed:from:``
        | O.doPlaySoundAndWait
        | O.``rest:elapsed:from:``
        | O.``noteOn:duration:elapsed:from:``
        | O.doBroadcastAndWait
        | O.``glideSecs:toX:y:elapsed:from:``
        | O.``wait:elapsed:from:``
        | O.doAsk

        | O.doWaitUntil
        | O.doForeverIf
        | O.doWhile
        | O.doRepeat

        | O.doReturn

        | O.doUntil
        | O.doWaitUntil
        | O.doForever

        | O.stopAll
        | O.stopScripts
        | O.deleteClone -> Some()
        | _ -> None
    )
    |> Option.isNone

let scriptName = function
    | Procedure(ProcedureDefinition(name = name)) -> name
    | _ -> "λ"

let inlining locals (ProcedureDefinition(parameters = parameters; body = BlockExpression(body = body))) arguments env =
    let BlockEnvs.Entity { parent = parent } & BlockEnvs.Member { self = self } & BlockEnvs.Procedure { procedureScript = script } = env

    let uniqueName state baseName locals =
        let rec aux i n =
            if hasVariableDefinitionAtSelf n self || Option.exists (hasVariableDefinitionAtSelf n) parent || Map.containsKey n locals then
                aux (i + 1) (baseName + "#" + string i)
            else
                let v = { state = state; name = n; isPersistent = NoPersistent; value = SType.parameterDefaultValue SType.N }
                n, v, Map.add n v locals

        aux 1 baseName

    let setLocals, (bindings, locals) =
        List.zip parameters arguments
        |> List.mapFold (fun (bindings, locals) (ParameterDefinition(name = parameterName), argument) ->
            let state = Expression.state argument |> Tagged.withTags Tags.empty
            let uniqueName, v, locals = uniqueName state (scriptName script + "." + parameterName) locals
            let setLocal = Expressions.``setVar:to:`` state uniqueName argument
            setLocal, ((parameterName, v)::bindings, locals)
        ) ([], locals)

    let replaceParamToReadVariable = function
        | ComplexExpression(state, O.getParam, Literal(value = SString paramName)::_) as e ->
            match List.tryFind (fun (n, _) -> paramName = n) bindings with
            | None -> e
            | Some(_, { name = varName }) ->
                let state = state |> Tagged.withTags Tags.empty
                ComplexExpression(state, O.readVariable, [Expression.eString state varName])
        | e -> e

    let body = setLocals @ List.map (ComplexExpression.mapComplexExpression replaceParamToReadVariable) body
    body, locals

[<Struct; RequireQualifiedAccess>]
type private ChooseProcedureByName<'a> = { name: string } with
    interface IFunc<'a ScriptData, 'a ScriptData voption> with
        member f.Invoke s =
            match s.script with
            | Procedure(ProcedureDefinition(name = name')) when name' = f.name -> ValueSome s
            | _ -> ValueNone

let findProcedureAtBlockEnv name (BlockEnvs.Member { self = self } & BlockEnvs.Script { convertedScripts = convertedScripts }) =
    let callee =
        let mutable f = { ChooseProcedureByName.name = name }
        PooledBuffer.tryPickV &f convertedScripts

    match callee with
    | ValueSome _ as r -> r
    | _ -> findProcedure name self |> VOption.unbox

let canInlining maxInliningSize caller callee =
    if Script.atomicity caller <> Script.atomicity callee then false else
    if hasSelfCall callee then false else

    match Script.state callee |> Tagged.tryFind InliningOptionsTag with
    | ValueSome InliningOptions.NoInlining -> false
    | ValueSome InliningOptions.AggressiveInlining -> true
    | _ -> scriptSize callee <= maxInliningSize

let inlineExpansionAtExpressions maxInliningSize locals' es env =
    match es with
    | ComplexExpression(operator = O.call; operands = Literal(value = SString calleeName)::arguments)::es as ees ->
        let (BlockEnvs.Procedure { procedureScript = caller }) = env

        match findProcedureAtBlockEnv calleeName env with
        | ValueSome { script = Procedure calleeDef as callee }
            when canInlining maxInliningSize caller callee ->

            let body, locals = inlining locals'.contents calleeDef arguments env
            locals'.contents <- locals
            R.modified (body @ es)

        | _ -> R.noModify ees
    | _ -> R.noModify es

module JoinWhenGreenFlags =
    [<Struct>]
    type private ConcatBody<'a,'b> = | ConcatBody with
        interface IFunc<struct('a ComplexExpression list * struct('b * 'a ListenerDefinition)), 'a ComplexExpression list> with
            member _.Invoke struct(xs, struct(_, ListenerDefinition(body = BlockExpression(body = body)))) = xs @ body

    let makeJoinedScript joins =
        let struct(s1, ListenerDefinition(state = ls; body = BlockExpression(state = bs))) = PooledBuffer.item 0 joins
        let ls = Tagged.withTags Tags.empty ls
        let bs = Tagged.withTags Tags.empty bs
        let body = PooledBuffer.foldV (let mutable f = ConcatBody in &f) [] joins
        let script = Listener(ListenerDefinition(ls, O.whenGreenFlag, [], BlockExpression(bs, body))) |> updateScriptStatistics
        { s1 with script = script }

    let rec joinAux acc joinables = function
        | [] ->
            if 2 <= PooledBuffer.count joinables then
                PooledBuffer.add acc (makeJoinedScript joinables)
                ValueSome(PooledBuffer.toList acc)
            else
                ValueNone

        | x::xs as xxs ->

        match x.script with

        // 接続可能
        | Listener(ListenerDefinition(name = O.whenGreenFlag; arguments = []; body = body) as l) when yieldNotContanins body ->
            joinAux acc (PooledBuffer.add joinables struct(x, l); joinables) xs

        // スキップ可能
        | Procedure _ | Statements _ | Expression _ -> joinAux (PooledBuffer.add acc x; acc) joinables xs

        // スキップ不可
        | Listener _ ->

        match PooledBuffer.count joinables with

        // 接続可能はなかったので続行
        | 0 -> joinAux (PooledBuffer.add acc x; acc) joinables xs

        // 接続可能は1つなので続行
        | 1 ->
            let struct(j0, _) = PooledBuffer.item 0 joinables
            PooledBuffer.add acc j0
            PooledBuffer.add acc x
            PooledBuffer.clear joinables
            joinAux acc joinables xs

        // 接続可能を複数接続して終了
        | _ ->
            PooledBuffer.add acc (makeJoinedScript joinables)
            ValueSome(PooledBuffer.appendToList acc xxs)

    type script<'a> = 'a Tagged ScriptData
    type joinable<'a> = (struct('a script * 'a Tagged ListenerDefinition))
    type scripts<'a> = 'a script list
    type result<'a> = 'a scripts voption

    [<Struct>]
    type Join2<'a,'s> = { acc: PooledBuffer<'a script,'s>; scripts: 'a scripts } with
        interface IPooledBufferUser<'a joinable,'a result> with
            member u.Using joinablesAcc = joinAux u.acc joinablesAcc u.scripts

    [<Struct>]
    type Join1<'a> = { scripts: 'a scripts } with
        interface IPooledBufferUser<'a script,'a result> with
            member u.Using acc =
                let mutable join2 = { Join2.acc = acc; Join2.scripts = u.scripts }
                PooledBuffer.using &join2

let joinWhenGreenFlags self = 
    let mutable join1 = { JoinWhenGreenFlags.Join1.scripts = self.scripts }
    PooledBuffer.using &join1
