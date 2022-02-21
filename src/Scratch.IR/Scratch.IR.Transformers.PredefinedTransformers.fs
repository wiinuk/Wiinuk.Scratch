module Scratch.IR.Transformers.PredefinedTransformers
open Scratch
open Scratch.Primitives
open Scratch.Reflection
open Scratch.Ast
open Scratch.AstDefinitions
open Scratch.Evaluator
open Scratch.Transformers
open Scratch.IR
open Scratch.IR.Source.Operators
open Scratch.IR.Transformers
open Scratch.IR.Transformers.Transformer
open System.Runtime.CompilerServices

module R = Scratch.Transformers.TransformResult

[<AutoOpen>]
module private CostHelpers =
    let operatorCost name =
        Map.tryFind name knownAllOperatorMap
        |> VOption.map (fun op -> op.operatorCost)
        |> VOption.defaultValue Unknown

    type private MaxCostTableHolder<'a> private () =
        static let value = ConditionalWeakTable<'a Exp', Cost>()
        static member Value = value
    let maxCostTable<'a> = MaxCostTableHolder<'a>.Value

    let collectMax currentCost maxCost measureCost xs =
        let rec aux currentCost xs =
            if maxCost <= currentCost then currentCost else

            match xs with
            | [] -> currentCost
            | x::xs -> aux (max (measureCost x) currentCost) xs

        aux currentCost xs

    let maxCostRaw maxCost e =
        match e with
        | VarSet(_, e1)
        | TupleSet(_, _, e1) -> max HasSideEffect (maxCost e1)
        | Var v -> if Var.isMutable v then HasSideEffect else LiteralLike
        | Lit _ -> LiteralLike
        | Call _ -> Unknown
        
        | TupleGet(e1, _)
        | Coerce(_, e1, _)
        | Atom e1 -> maxCost e1

        | Let(_, e1, e2)
        | Seq(e1, e2) -> max (maxCost e1) (maxCost e2)

        | If(e1, e2, e3) -> max (max (maxCost e1) (maxCost e2)) (maxCost e3)

        | NewTuple es -> collectMax LiteralLike Cost.max maxCost es
        | Op(op, es) -> collectMax (operatorCost op) Cost.max maxCost es
        | ListOp(op, es) -> collectMax (operatorCost op) Cost.max (function Choice1Of2 e -> maxCost e | Choice2Of2 _ -> HasSideEffect) es
        | ExtOp(sign, es) -> collectMax sign.cost Cost.max maxCost es

    [<Sealed; AbstractClass>]
    type private MaxCostCached<'a> private () =
        static let maxCostCachedFun = MaxCostCached<_>.maxCostCached
        static let maxCostCachedDelegate = ConditionalWeakTable.CreateValueCallback(fun e -> maxCostRaw maxCostCachedFun e)
        static member maxCostCached e = maxCostTable<'a>.GetValue(e.value, maxCostCachedDelegate)

    let maxCost e = MaxCostCached<_>.maxCostCached e

[<AutoOpen>]
module private EvalHelpers =
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
        static let value: BlockState<'a,_,_,_> = newDefaultBlockState()
        static member Value = value

    let evaluateExpressionWithGlobalContext e =
        let state = BlockStateHolder.Value
        try Ok <| Scratch.Evaluator.Expressions.evaluateExpression &state e
        with e -> Error e

    let evaluateExpWithGlobalContext e =
        match IRToStageData.convertExprWith id e with
        | [e], [], [], [], [] -> evaluateExpressionWithGlobalContext e
        | _ -> Error <| Failure ""

module private Var =
    let (|IsMutable|) v = Var.isMutable v

module private Exp =
    let (|Cost|) e = maxCost e

    let variableCount var e =
        e
        |> Exp.fold (fun count e ->
            match e.value with
            | Var v
            | VarSet(v, _)
            | TupleSet(v, _, _) when v = var -> count + 1
            | _ -> count
        ) 0

    let replaceVar var newValue e =
        if not <| ExpType.isAssignable (Var.varType var) (Exp.varType newValue)
        then Exp.let' e.source var newValue |> ignore; failwith ""
        else

        let rec aux e =
            let inline rep1 make e1 =
                match aux e1 with
                | ValueSome e1 -> ValueSome(make e1 @+e.source)
                | _ -> ValueNone

            let inline rep2 make (e1, e2) =
                let changed = false

                let e1, changed =
                    match aux e1 with
                    | ValueSome e1 -> e1, true
                    | _ -> e1, changed

                let e2, changed =
                    match aux e2 with
                    | ValueSome e2 -> e2, true
                    | _ -> e2, changed

                if changed
                then ValueSome(make (e1, e2) @+e.source)
                else ValueNone

            let inline rep3 make (e1, e2, e3) =
                let changed = false

                let e1, changed =
                    match aux e1 with
                    | ValueSome e1 -> e1, true
                    | _ -> e1, changed

                let e2, changed =
                    match aux e2 with
                    | ValueSome e2 -> e2, true
                    | _ -> e2, changed

                let e3, changed =
                    match aux e3 with
                    | ValueSome e3 -> e3, true
                    | _ -> e3, changed

                if changed
                then ValueSome(make (e1, e2, e3) @+e.source)
                else ValueNone

            let inline repList' make replace es =
                let rec aux' acc changed = function
                    | [] -> if changed then ValueSome(make (List.rev acc) @+e.source) else ValueNone
                    | x::xs ->
                        match replace x with
                        | ValueSome x -> aux' (x::acc) true xs
                        | _ -> aux' (x::acc) changed xs

                aux' [] false es

            let inline repList make es = repList' make aux es
            let inline repListOpList make xs = repList' make (function Choice1Of2 e -> aux e |> VOption.map Choice1Of2 | _ -> ValueNone) xs

            match e.value with
            | Var v -> if var = v then ValueSome newValue else ValueNone

            | Lit _ -> ValueNone

            | VarSet(v, e1) -> rep1 (fun e1 -> VarSet(v, e1)) e1
            | TupleGet(e1, i) -> rep1 (fun e1 -> TupleGet(e1, i)) e1
            | TupleSet(v, i, e1) -> rep1 (fun e1 -> TupleSet(v, i, e1)) e1
            | Coerce(k, e1, t) -> rep1 (fun e1 -> Coerce(k, e1, t)) e1
            | Atom e1 -> rep1 Atom e1

            | Let(v, e1, e2) -> rep2 (fun (e1, e2) -> Let(v, e1, e2)) (e1, e2)
            | Seq(e1, e2) -> rep2 Seq (e1, e2)

            | If(e1, e2, e3) -> rep3 If (e1, e2, e3)

            | NewTuple es -> repList NewTuple es
            | Call(p, es) -> repList (fun es -> Call(p, es)) es
            | Op(op, es) -> repList (fun es -> Op(op, es)) es
            | ExtOp(spec, es) -> repList (fun es -> ExtOp(spec, es)) es

            | ListOp(op, xs) -> repListOpList (fun xs -> ListOp(op, xs)) xs

        aux e |> VOption.defaultValue e

[<AutoOpen>]
module private StatHelpers =
    module Top =
        let atomicity = function
            | Top.Procedure p -> ProcDef.atomicity p
            | Top.Listener _ -> NoAtomic

            // TODO:
            | Top.VariableInit _ -> Atomic

    module Exp =
        let equalsWithV equals e1 e2 =
            let rec exp e1 e2 =
                equals struct(e1.source, e2.source) &&
                match e1.value, e2.value with
                | Lit x1, Lit x2 -> x1 = x2
                | Let(v1, e11, e12), Let(v2, e21, e22) -> v1 = v2 && exp e11 e21 && exp e12 e22
                | VarSet(v1, e1), VarSet(v2, e2) -> v1 = v2 && exp e1 e2
                | Var v1, Var v2 -> v1 = v2
                | Seq(e11, e12), Seq(e21, e22) -> exp e11 e21 && exp e12 e22
                | If(e11, e12, e13), If(e21, e22, e23) -> exp e11 e21 && exp e12 e22 && exp e13 e23
                | NewTuple es1, NewTuple es2 -> exps es1 es2
                | TupleGet(e1, i1), TupleGet(e2, i2) -> i1 = i2 && exp e1 e2
                | TupleSet(v1, i1, e1), TupleSet(v2, i2, e2) -> i1 = i2 && v1 = v2 && exp e1 e2
                | Op(op1, es1), Op(op2, es2) -> op1 = op2 && exps es1 es2
                | ListOp(op1, xs1), ListOp(op2, xs2) -> op1 = op2 && listOps xs1 xs2
                | Call(p1, es1), Call(p2, es2) -> p1 = p2 && exps es1 es2
                | Coerce(k1, e1, t1), Coerce(k2, e2, t2) -> k1 = k2 && ExpType.equals t1 t2 && exp e1 e2
                | Atom e1, Atom e2 -> exp e1 e2
                | _ -> false

            and exps es1 es2 =
                match es1, es2 with
                | [], [] -> true
                | e1::es1, e2::es2 -> exp e1 e2 && exps es1 es2
                | _ -> false

            and listOp x1 x2 =
                match x1, x2 with
                | Choice1Of2 e1, Choice1Of2 e2 -> exp e1 e2
                | Choice2Of2 x1, Choice2Of2 x2 -> equals struct(x1.source, x2.source) && x1.value = x2.value
                | _ -> false

            and listOps xs1 xs2 =
                match xs1, xs2 with
                | [], [] -> true
                | x1::xs1, x2::xs2 -> listOp x1 x2 && listOps xs1 xs2
                | _ -> false

            exp e1 e2

        let tryPickV chooser e =
            let rec list chooser = function
                | [] -> ValueNone
                | e::es ->

                match chooser e with
                | ValueSome _ as r -> r
                | _ -> list chooser es

            let rec exp e =
                match chooser e with
                | ValueSome _ as r -> r
                | _ ->

                match e.value with
                | Lit _
                | Var _ -> ValueNone

                | Atom e1
                | Coerce(value = e1)
                | TupleGet(e1, _)
                | TupleSet(value = e1)
                | VarSet(_, e1) -> exp e1

                | Let(_, e1, e2)
                | Seq(e1, e2) ->
                    match exp e1 with
                    | ValueSome _ as r -> r
                    | _ -> exp e2

                | If(e1, e2, e3) ->
                    match exp e1 with
                    | ValueSome _ as r -> r
                    | _ ->
                    match exp e2 with
                    | ValueSome _ as r -> r
                    | _ -> exp e3

                | Call(_, es)
                | NewTuple es
                | Op(_, es)
                | ExtOp(_, es) -> list exp es

                | ListOp(_, xs) -> list (function Choice1Of2 e -> exp e | Choice2Of2 _ -> ValueNone) xs
            exp e

    let yieldNotContanins block =
        block
        |> Exp.tryPickV (fun x ->
            match x.value with

            // TODO: プロシージャの実装を見て判断することもできる
            | Call _ -> ValueSome HUnit

            | Op(operator = KnownOperatorInfo(ValueSome { control = Control.Next })) -> ValueNone
            | Op _ -> ValueSome HUnit
            | _ -> ValueNone
        )
        |> VOption.isNone

[<AutoOpen>]
module private JoinWhenGreenFlagsHelpers =
    [<Struct>]
    type private ConcatBody<'a,'b> = | ConcatBody with
        interface IFunc<struct(struct('b * 'a ListenerDef) * 'a Exp), 'a Exp> with
            member _.Invoke struct(struct(_, { value = ListenerDef(body = body) }), xs) = Exp.seq xs.source body xs

    let makeJoinedScript joins =
        let struct(s1, { source = ls }) = PooledBuffer.item 0 joins
        let ls = Tagged.withTags Tags.empty ls

        let struct(_, { Source.value = ListenerDef(body = body) }) = PooledBuffer.last joins 
        let body = PooledBuffer.foldBackIndexV (let mutable f = ConcatBody in &f) joins (PooledBuffer.count joins - 2) body

        let script = Top.Listener { source = ls; value = ListenerDef(O.whenGreenFlag, [], body) } |> ScriptStat.updateTop
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
        | Top.Listener({ value = ListenerDef(name = O.whenGreenFlag; arguments = []; body = body) } as l) when yieldNotContanins body ->
            joinAux acc (PooledBuffer.add joinables struct(x, l); joinables) xs

        // スキップ可能
        | Top.Procedure _ -> joinAux (PooledBuffer.add acc x; acc) joinables xs

        // スキップ不可
        | Top.VariableInit _
        | Top.Listener _ ->

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

    type script<'a> = 'a Tagged Scratch.IR.Transformers.IRScriptData
    type joinable<'a> = (struct('a script * 'a Tagged ListenerDef))
    type scripts<'a> = 'a script list
    type result<'a> = 'a scripts voption

    [<Struct; NoEquality; NoComparison; RequireQualifiedAccess>]
    type Join2<'a,'s> = { acc: PooledBuffer<'a script,'s>; scripts: 'a scripts } with
        interface IPooledBufferUser<'a joinable,'a result> with
            member u.Using joinablesAcc = joinAux u.acc joinablesAcc u.scripts

    [<Struct; NoEquality; NoComparison; RequireQualifiedAccess>]
    type Join1<'a> = { scripts: 'a scripts } with
        interface IPooledBufferUser<'a script,'a result> with
            member u.Using acc =
                let mutable join2 = { Join2.acc = acc; Join2.scripts = u.scripts }
                PooledBuffer.using &join2

    let joinWhenGreenFlags self = 
        let mutable join1 = { Join1.scripts = self.scripts }
        PooledBuffer.using &join1

[<AutoOpen>]
module private Helpers =
    type IRScriptData<'a> = ScriptData<Top<'a>,'a>

    [<Struct; RequireQualifiedAccess; NoEquality; NoComparison>]
    type private ChooseProcedureByName<'a> = { var: ProcedureVar } with
        interface IFunc<'a IRScriptData, 'a ProcDef voption> with
            member f.Invoke s =
                match s.script with
                | Top.Procedure proc when ProcDef.var proc = f.var -> ValueSome proc
                | _ -> ValueNone

    let findProcedure var self =
        self.scripts
        |> List.tryPick (function
            | { script = Top.Procedure p } when ProcDef.var p = var
                -> Some p

            | _ -> None
        )

    let findProcedureAtBlockEnv var (State.MemberState { self = self } & State.ScriptState { convertedScripts = convertedScripts }) =
        let mutable f = { ChooseProcedureByName.var = var }
        let callee = PooledBuffer.tryPickV &f convertedScripts

        match callee with
        | ValueSome _ as r -> r
        | _ -> findProcedure var self |> VOption.unbox

    let (|FindProcedureAtBlockEnv|) env var = findProcedureAtBlockEnv var env

    let hasSelfCall p = ScriptStat.procedureStatistics(p).hasSelfCall.Value
    let scriptSize s = ScriptStat.procedureStatistics(s).table.scriptSize

    let scriptName = function
        | Top.Procedure p -> ProcDef.var p |> Var.name
        | _ -> "λ"

    let hasExportTag tagged = Tagged.contains ExportTag tagged

    let isRootScript = function
        | Top.Listener _
        | Top.VariableInit _ -> true
        | Top.Procedure _ as s when hasExportTag (Top.source s) -> true
        | _ -> false

    let rec collectAliveProcedureReferences aliveSet self p =
        match p with
        | Top.Procedure p when Set.contains (ProcDef.var p) aliveSet -> aliveSet
        | _ ->

        let aliveSet =
            match p with
            | Top.Procedure p -> Set.add (ProcDef.var p) aliveSet
            | _ -> aliveSet

        ScriptStat.topStatistics(p).table.procedureStats
        |> Map.fold (fun aliveSet name stat ->
            if 1 <= stat.callCount then
                match findProcedure name self with
                | None -> aliveSet
                | Some s -> collectAliveProcedureReferences aliveSet self (Top.Procedure s)
            else
                aliveSet
        ) aliveSet

    let isExportVarData = function
        | { VariableDecl.isPersistent = Persistent } -> true
        | v -> hasExportTag v.state

    let hasVariableInScript var s =
        let s = ScriptStat.topStatistics s
        match Map.tryFind var s.table.variableStats with
        | ValueSome x -> x.metaCount + x.writeCount + x.readCount <> 0
        | ValueNone -> false

    let hasReferenceAtChildren var children =
        children
        |> List.exists (function
            | Choice2Of3 s ->

                // スプライトの同名の変数によって隠されていない
                s.variables |> List.forall (fun v -> v.var <> var) &&

                s.scripts |> List.exists (fun d -> hasVariableInScript var d.script)
            | _ -> false
        )

    let hasReferenceAtSelf var self =
        self.scripts
        |> List.exists (fun d -> hasVariableInScript var d.script)

[<AutoOpen>]
module private InliningHelpers =
    let inlining { Source.value = ProcDef(ProcHeader(parameters = parameters), body) } arguments env =
        let (State.ExpState { procedureScript = script }) = env

        body
        |> List.foldBack (fun (parameter, argument) scope ->
            let state = Tagged.withTags Tags.empty argument.source
            let name = scriptName script + "." + Var.name parameter.value
            let var = Var.newStorage name false (Var.varType parameter.value)
            let scope = Exp.replaceVar parameter.value (Exp.var parameter.source var) scope
            Exp.let' state var argument scope
        ) (List.zip parameters arguments)

    let canInlining maxInliningSize callsiteAtomicity callee =
        if callsiteAtomicity <> ProcDef.atomicity callee then false else
        if hasSelfCall callee then false else

        match callee.source |> Tagged.tryFind InliningOptionsTag with
        | ValueSome InliningOptions.NoInlining -> false
        | ValueSome InliningOptions.AggressiveInlining -> true
        | _ -> scriptSize callee <= maxInliningSize

    let inlineExpansionAtExpressions maxInliningSize es env =
        match es.value with
        | Call(FindProcedureAtBlockEnv env (ValueSome callee), arguments)
            when canInlining maxInliningSize (State.currentAtomicity env) callee ->
            R.modified <| inlining callee arguments env

        | _ -> R.noModify es

let constantFolding e _ =
    match e.value with
    | Op(operator, operands)
        when
            operatorCost operator = Pure &&
            operands |> List.forall (function { value = Lit _ } -> true | _ -> false) ->

        match evaluateExpWithGlobalContext e with
        | Error _ -> R.noModify e
        | Ok c -> Lit c @+e.source |> R.modified

    | _ -> R.noModify e

let constantPropagation<'a> = { new ExpTransformer<'a>() with
    member _.Invoke e _ =
        match e.value with
        | Let(Var.IsMutable false & var, Exp.Cost LiteralLike & value, scope) ->
            R.modified <| Exp.replaceVar var value scope
        | _ -> R.noModify e
}
let purePropagation<'a> = { new ExpTransformer<'a>() with
    member _.Invoke e _ =
        match e.value with
        | Let(Var.IsMutable false & var, Exp.Cost Pure & value, scope) when Exp.variableCount var scope <= 1 ->
            R.modified <| Exp.replaceVar var value scope

        | _ -> R.noModify e
}

[<Struct>]
type InlineExpansionConfig = {
    maxProcedureSize: int
}
module private InlineExpansionConfig =
    let defultValue = { maxProcedureSize = 8 }

let inlineExpansion withConfig =
    let { InlineExpansionConfig.maxProcedureSize = maxSize } = withConfig InlineExpansionConfig.defultValue
    let inliningAtExp = { new ExpTransformer<_>() with member _.Invoke e s = inlineExpansionAtExpressions maxSize e s }
    let plugins = { Plugins.empty with transformExp = inliningAtExp }

    { new EntityTransformer<_>() with
        member _.Invoke self env =
            let State.EntityState entityEnv & State.Settings config = env
            let config = { config with maxIterationCount = 1 }
            let memberEnv = { self = self }
            let env = State.newMemberState memberEnv (State.newEntityState entityEnv (State.newConfigState plugins config))

            Transformer.transformEntityScripts self.scripts env
            |> R.mapIfModified self (fun scripts -> { self with scripts = scripts })
    }

let joinWhenGreenFlags<'a> = { new EntityTransformer<'a Tagged>() with
    member _.Invoke self _ =
        match joinWhenGreenFlags self with
        | ValueNone -> R.noModify self
        | ValueSome scripts -> R.modified { self with scripts = scripts }
}
// ルート ( Listener, Expression, Statements, ExportTag が付いた Procedure ) から到達できないプロシージャを取り除く
// ルートから到達できないなら、相互再帰しているプロシージャも取り除く
let eliminateDeadProcedures<'a> = { new EntityTransformer<'a Tagged>() with
    member _.Invoke self _ =
        let aliveSet =
            self.scripts
            |> List.fold (fun aliveSet { script = s } ->
                if isRootScript s then collectAliveProcedureReferences aliveSet self s
                else aliveSet
            ) Set.empty

        self.scripts
        |> R.mapList (fun s ->
            match s.script with
            | Top.Procedure p when Set.contains (ProcDef.var p) aliveSet |> not -> R.removed s
            | _ -> R.noModify s
        )
        |> R.mapIfModified self (fun scripts -> { self with scripts = scripts })
}
let eliminateUnusedVariables<'a> = { new EntityTransformer<'a Tagged>() with
    member _.Invoke self (State.EntityState { EntityEnv.children = children }) =
        self.variables
        |> R.mapList (fun v ->
            if isExportVarData v || hasReferenceAtSelf v.var self || hasReferenceAtChildren v.var children then
                R.noModify v
            else
                R.removed v
        )
        |> R.mapIfModified self (fun variables -> { self with variables = variables })
}
let eliminateDeadLetExpression<'a> = { new ExpTransformer<'a>() with
    member _.Invoke e _ =
        match e.value with
        | Let(var, value, scope) when Exp.variableCount var scope = 0 ->
            if HasSideEffect <= maxCost value
            then R.modified <| Exp.seq e.source value scope
            else R.modified scope

        | _ -> R.noModify e
}
let eliminateDeadSeqExpression e _ =
    let isPure e = maxCost e <= Pure
    let (|OpCost|) = operatorCost

    match e.value with
    | Seq(first, last) when isPure first -> last |> R.modified
    | Seq(first, last) ->
        match first.value with
        | Lit _ -> last |> R.modified
        | Var _ -> last |> R.modified

        | VarSet _ -> e |> R.noModify

        // `(let v = e1 in e2); last` ( e2 は純粋 ) => `e1; last`
        | Let(_, e1, e2) when isPure e2 -> Exp.seq e.source e1 last |> R.modified
        | Let _ -> e |> R.noModify

        // `(e1; e2); last` ( e1 は純粋 ) => `e2; last`
        // `(e1; e2); last` ( e2 は純粋 ) => `e1; last`
        | Seq(e1, e2) when isPure e1 -> Exp.seq e.source e2 last |> R.modified
        | Seq(e1, e2) when isPure e2 -> Exp.seq e.source e1 last |> R.modified
        | Seq _ -> e |> R.noModify

        // `(if e1 then e2 else e3); last` ( e2 と e3 は純粋 ) => `e1; last`
        | If(e1, e2, e3) when isPure e2 && isPure e3 -> Exp.seq e.source e1 last |> R.modified
        | If _ -> e |> R.noModify

        // `(e1, e2, …, en); last` ( e1… のうち純粋でない式を x1… とする ) => `x1; x2; …; xn; last`
        | NewTuple es ->
            List.foldBack (fun e' last ->
                if isPure e' then last
                else Exp.seq e.source e' last
            ) es last
            |> R.modified

        // `e1.i; last` => `e1; last`
        | TupleGet(e1, _) -> Exp.seq e.source e1 last |> R.modified

        | TupleSet _ -> e |> R.noModify

        // `op e1 … en; last` ( op は純粋, e1… のうち純粋でない式を x1… とする ) => `x1; x2; …; xn; last`
        | Op(OpCost cost, es)
        | ExtOp({ cost = cost }, es) when cost <= Pure ->
            List.foldBack (fun e' last ->
                if isPure e' then last
                else Exp.seq e.source e' last
            ) es last
            |> R.modified
        | Op _
        | ExtOp _ -> e |> R.noModify

        | ListOp(OpCost cost, es) when cost <= Pure ->
            List.foldBack (fun x last ->
                match x with
                | Choice1Of2 e' when not <| isPure e' -> Exp.seq e.source e' last
                | _ -> last
            ) es last
            |> R.modified
        | ListOp _ -> e |> R.noModify

        // TODO:
        | Call _ -> e |> R.noModify

        // `(e1 :?> T); last` => `e1; last`
        | Coerce(_, e1, _) -> Exp.seq e.source e1 last |> R.modified

        // TODO:
        | Atom _ -> e |> R.noModify

    | _ -> e |> R.noModify

let eliminateSelfSetExpression e _ =
    match e.value with
    | VarSet(v, { value = Var v' }) when v = v' -> R.modified (Exp.empty e.source)
    | _ -> R.noModify e

let private eliminateDeadConditionExpression' e env =
    match e.value with
    | If({ value = Lit test }, ifTrue, ifFalse) ->
        if SValue.toBool test

        // `doIfElse true { ...ifTrue } { ... }` => `...ifTrue`
        then R.modified ifTrue

        // `doIfElse false { ... } { ...ifFalse }` => `...ifFalse`
        else R.modified ifFalse

    //| O.doWaitUntil

    //| O.doForeverIf
    //| O.doWhile

    | Op(O.doRepeat, [{ value = Lit test }; body]) ->
        match State.currentAtomicity env with
        | NoAtomic -> R.noModify e
        | Atomic ->

        let count = SValue.toNumber test

        // `doRepeat $n \{ ... }` ( atomic, $n <= 0 ) => ``
        if count <= 0. then R.modified (Exp.empty e.source)

        // `doRepeat 1 \{ ...body }` ( atomic ) => `...body`
        elif count = 1. then R.modified body

        else R.noModify e

    //| O.doReturn
    //| O.doForever

    | Op(O.doUntil, [{ value = Lit test }; ifFalse]) ->
        match State.currentAtomicity env with
        | NoAtomic -> R.noModify e
        | Atomic ->

        if SValue.toBool test

        // `doUntil { true } { ... }` ( atomic ) => ``
        then R.modified (Exp.empty e.source)

        // `doUntil { false } { ...ifFalse }` => `doForever { ...ifFalse }`
        else R.modified (Exp.Op.doForever e.source ifFalse)

    //| O.stopAll
    //| O.stopScripts
    //| O.deleteClone

    | _ -> R.noModify e

let eliminateDeadConditionExpression<'a> = { new ExpTransformer<'a>() with member _.Invoke e env = eliminateDeadConditionExpression' e env }

[<Struct>]
type AlgebraicSimplificationConfig = {
    finiteNumberOnly: bool
}
module AlgebraicSimplificationConfig =
    let defaultValue = { finiteNumberOnly = true }

[<AutoOpen>]
module private AlgebraicSimplificationHelpers =
    let removeTags x = Tagged.withTags Tags.empty x

    [<return: Struct>]
    let (|LiteralLike|_|) e = if Cost.LiteralLike <= maxCost e then ValueSome() else ValueNone

    [<return: Struct>]
    let (|BoolExp|_|) e = if ExpType.equals (Exp.varType e) ExpTypes.bool then ValueSome() else ValueNone

    [<return: Struct>]
    let (|Literal|_|) = function
        | { Source.value = Lit _ } -> ValueSome()
        | _ -> ValueNone

    [<return: Struct>]
    let (|Num|_|) = function
        | { Source.value = Lit(SNumber x) } when (* not nan *) x = x -> ValueSome x
        | _ -> ValueNone

    [<return: Struct>]
    let (|True|_|) = function
        | { Source.value = Lit(SBool true) } -> ValueSome()
        | _ -> ValueNone

    [<return: Struct>]
    let (|False|_|) = function
        | { Source.value = Lit(SBool false) } -> ValueSome()
        | _ -> ValueNone

    [<return: Struct>]
    let (|Var|_|) = function
        | { Source.value = Var v } -> ValueSome v
        | _ -> ValueNone

    [<return: Struct>]
    let (|Mul|_|) = function
        | { Source.value = Op(O.``*``, [e1; e2]) } -> ValueSome struct(e1, e2)
        | _ -> ValueNone

    [<return: Struct>]
    let (|Div|_|) = function
        | { Source.value = Op(O.``/``, [e1; e2]) } -> ValueSome struct(e1, e2)
        | _ -> ValueNone

    [<return: Struct>]
    let (|Add|_|) = function
        | { Source.value = Op(O.``+``, [e1; e2]) } -> ValueSome struct(e1, e2)
        | _ -> ValueNone

    [<return: Struct>]
    let (|Sub|_|) = function
        | { Source.value = Op(O.``-``, [e1; e2]) } -> ValueSome struct(e1, e2)
        | _ -> ValueNone

    [<return: Struct>]
    let (|Not|_|) = function
        | { Source.value = Op(O.not, [e1]) } -> ValueSome e1
        | _ -> ValueNone

    [<return: Struct>]
    let (|And|_|) = function
        | { Source.value = Op(O.``&``, [e1; e2]) } -> ValueSome struct(e1, e2)
        | _ -> ValueNone

    [<return: Struct>]
    let (|Or|_|) = function
        | { Source.value = Op(O.``|``, [e1; e2]) } -> ValueSome struct(e1, e2)
        | _ -> ValueNone

    [<return: Struct>]
    let (|Eq|_|) = function
        | { Source.value = Op(O.``=``, [e1; e2]) } -> ValueSome struct(e1, e2)
        | _ -> ValueNone

    let simpleFiniteNumberOnly e _ =
        match e with

        // `n * 1 = 1 * n = n` (n <> nan)
        | Mul(Num 1., n)
        | Mul(n, Num 1.) -> R.modified n

        // `n / 1 = n` (n <> nan)
        | Div(n, Num 1.) -> R.modified n

        // `n + 0 = 0 + n = n` (n <> nan)
        | Add(n, Num 0.)
        | Add(Num 0., n) -> R.modified n

        // `n - 0 = n` (n <> nan)
        | Sub(n, Num 0.) -> R.modified n

        | e -> R.noModify e

    let simpleIncludesFiniteNumber e _ =
        match e with

        // `n * 0 = 0 * n = 0`
        | Mul(Num 0. & z, n)
        | Mul(n, Num 0. & z) -> Exp.seq (removeTags e.source) n z |> R.modified

        // `(x - x) = 0`
        | Sub(Var x, Var x') ->
            if x = x'
            then Exp.number (removeTags e.source) 0. |> R.modified
            else R.noModify e

        // `a * (b * n)` =
        // `a * (n * b)` =
        // `(a * n) * b` =
        // `(n * a) * b` = `(a * b) * n` ( a は定数, b は定数 )
        | Mul(Literal & a, Mul(Literal & b, n))
        | Mul(Literal & a, Mul(n, Literal & b))
        | Mul(Mul(Literal & a, n), Literal & b)
        | Mul(Mul(n, Literal & a), Literal & b) ->
            let s = removeTags e.source
            match Exp.Op.``*`` s a b |> evaluateExpWithGlobalContext with
            | Error _ -> R.noModify e
            | Ok c -> Exp.Op.``*`` s (Exp.lit s c) n |> R.modified

        // `a + (b + n)` =
        // `a + (n + b)` =
        // `(a + n) + b` =
        // `(n + a) + b` = `(a + b) + n` ( a は定数, b は定数 )
        | Add(Literal & a, Add(Literal & b, n))
        | Add(Literal & a, Add(n, Literal & b))
        | Add(Add(Literal & a, n), Literal & b)
        | Add(Add(n, Literal & a), Literal & b) ->
            let s = removeTags a.source
            match Exp.Op.``+`` s a b |> evaluateExpWithGlobalContext with
            | Error _ -> R.noModify e
            | Ok c -> Exp.Op.``+`` s (Exp.lit s c) n |> R.modified

        // `(x + -y) = (x - y)`
        | Add(x, Mul(Num -1., y))
        | Add(x, Mul(y, Num -1.)) ->
            Exp.Op.``-`` (removeTags e.source) x y |> R.modified

        // `x * 2 = 2 * x = x + x` (x は定数様)
        | Mul(Num 2., LiteralLike & x)
        | Mul(LiteralLike & x, Num 2.) ->
            Exp.Op.``+`` (removeTags e.source) x x |> R.modified

        // `(x / a) = (x * (1 / a))` (a は定数)
        | Div(x, Num a) ->
            let s = removeTags e.source
            Exp.Op.``*`` s x (Exp.number s (1. / a)) |> R.modified

        | _ -> R.noModify e

    let simpleBool e _ =
        match e with

        // `!true = false`
        | Not True -> Exp.bool e.source false |> R.modified
        // `!false = true`
        | Not True -> Exp.bool e.source true |> R.modified
        // `!!b = b`
        | Not(Not b) -> b |> R.modified

        // `b && false = false && b = false`
        | And(b, False)
        | And(False, b) ->
            let s = e.source
            Exp.seq s b (Exp.bool s false) |> R.modified

        // `b && true = true && b = b`
        | And(b, True)
        | And(True, b) -> b |> R.modified

        // `b && b = b`
        | And(Var v & b, Var v') ->
            if v = v' then b |> R.modified
            else R.noModify e

        // `b || false = false || b = b`
        | Or(b, False)
        | Or(False, b) -> b |> R.modified

        // `b || true = true || b = true`
        | Or(b, True)
        | Or(True, b) ->
            Exp.seq e.source b (Exp.bool e.source true) |> R.modified

        // `b || b = b`
        | Or(Var v & b, Var v') ->
            if v = v' then b |> R.modified
            else R.noModify e

        // `b == false = false = b = !b`
        | Eq(BoolExp & b, False)
        | Eq(False, BoolExp & b) ->
            Exp.Op.not e.source b |> R.modified

        // `b == true = true == b = b`
        | Eq(BoolExp & b, True)
        | Eq(True, BoolExp & b) ->
            b |> R.modified

        | _ -> R.noModify e

let algebraicSimplification withConfig =
    let config = withConfig AlgebraicSimplificationConfig.defaultValue

    // Scratch では `NaN * x = 0`, `NaN + x = x`
    let t = R.compose simpleIncludesFiniteNumber simpleBool

    if config.finiteNumberOnly
    then R.compose simpleFiniteNumberOnly t
    else t
