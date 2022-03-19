namespace Scratch.IR
open Scratch.Ast
open Scratch.AstDefinitions
open Scratch.IR
open Scratch.Primitives
open System.Collections.Generic
module VOption = ValueOption


[<Struct>]
type Yieldability =
    | NeverYield
    | NormalYield
    | ForceYield

[<Struct>]
type Recursivity =
    | NoRecursive
    | Recursive

[<Struct>]
type WithInfinity<'T> = Finite of 'T | Infinity with
    static member inline (+) (l, r) =
        match l, r with
        | Finite l, Finite r -> Finite(l + r)
        | _ -> Infinity

[<AutoOpen>]
module private Helpers =
    let controlToYieldaility = function
        | Control.ForceYield -> ForceYield
        | Control.NormalYield -> NormalYield
        | Control.Next
        | Control.Unknown
        | Control.Return
        | Control.Stop
        | Control.Call
        | Control.DeleteClone -> NeverYield

    let operatorYieldability = function
        | Atom _
        | Call _
        | Coerce _
        | If _
        | Let _
        | Lit _
        | NewTuple _
        | Seq _
        | TupleGet _
        | TupleSet _
        | Var _
        | VarSet _ -> NeverYield

        | ExtOp(spec = spec) -> controlToYieldaility spec.control

        | Op(operator = operator)
        | ListOp(operator = operator) ->

        match operator with
        | KnownOperatorInfo ValueNone -> failwith "unknownOperator"
        | KnownOperatorInfo(ValueSome info) -> controlToYieldaility info.control

    let yieldabilityAtAtomicity yieldability atomicity =
        match yieldability, atomicity with
        | ForceYield, (NoAtomic | Atomic) -> ForceYield
        | NormalYield, NoAtomic -> NormalYield
        | NormalYield, Atomic
        | NeverYield, (NoAtomic | Atomic) -> NeverYield

    let flow rider local env state e =
        let rider = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt rider
        let local = OptimizedClosures.FSharpFunc<_,_,_>.Adapt local
        let rec aux env state e =
            let state = rider.Invoke(env, state, e)
            let env' = local.Invoke(env, e)

            match e.value with
            | Atom e -> aux env' state e
            | Call(_, es)
            | NewTuple es
            | ExtOp(_, es)
            | Op(_, es) -> List.fold (aux env') state es

            | Coerce(_, e, _)
            | TupleGet(e, _)
            | TupleSet(_, _, e)
            | VarSet(_, e) -> aux env' state e

            | If(e1, e2, e3) -> aux env' (aux env' (aux env' state e1) e2) e3

            | Let(_, e1, e2) -> aux env' (aux env state e1) e2
            | Seq(e1, e2) -> aux env (aux env state e1) e2

            | ListOp(_, xs) -> List.fold (fun maxY -> function Choice1Of2 e -> aux env' maxY e | Choice2Of2 _ -> maxY) state xs
            | Lit _
            | Var _ -> state

        aux env state e

    let yieldabilityWithoutCallOperation a e =
        let rider a maxY e = max maxY (yieldabilityAtAtomicity (operatorYieldability e.value) a)
        let local a e = match e.value with Atom _ -> Atomic | _ -> a
        flow rider local a NeverYield e

    let inline cached (r: _ byref) create =
        match r with
        | ValueSome x -> x
        | _ ->
            let v = create()
            r <- ValueSome v
            v

    module Exp =
        let getProcedureVarAndCallSiteAtomicities a e =
            let rider a result e =
                match e.value with
                | Call(p, _) -> Map.add p a result
                | _ -> result

            let local a e = match e.value with Atom _ -> Atomic | _ -> a

            flow rider local a Map.empty e

    [<Struct>]
    type CallSite<'a> = { callee: 'a ProcedureStat; atomicity: Atomicity }
    and ProcedureStat<'a> = {
        procedure: 'a ProcDef
        parent: Dictionary<ProcedureVar,'a ProcedureStat>

        mutable currentYieldability: Yieldability
        mutable yieldabilityWithoutCall: Yieldability voption
        mutable yieldability: Yieldability voption
        mutable recursivity: Recursivity voption
        mutable stackDepth: int WithInfinity voption
        mutable callees: Map<ProcedureVar, Atomicity> voption
        mutable callers: 'a CallSite seq voption
    }
    module ProcedureStat =
        let callees s = cached &s.callees (fun () ->
            Exp.getProcedureVarAndCallSiteAtomicities (ProcDef.atomicity s.procedure) (ProcDef.body s.procedure)
        )

        let yieldabilityWithoutCall s = cached &s.yieldabilityWithoutCall (fun () ->
            let p = s.procedure
            yieldabilityWithoutCallOperation (ProcDef.atomicity p) (ProcDef.body p)
        )

        let private callersRaw stat =
            let var = ProcDef.var stat.procedure

            stat.parent
            |> Seq.choose (fun kv ->
                let callees = callees kv.Value
                match Map.tryFind var callees with
                | ValueNone -> None
                | ValueSome a -> Some { callee = kv.Value; atomicity = a }
            )
            |> Seq.cache

        let callers stat = cached &stat.callers (fun () -> callersRaw stat)

[<AutoOpen>]
module internal ExpStackTypeExtensions =
    module Exp =
        let measureLocalStackSize e =
            let localSize = function

                // `let v: t = … in …` => sizeof<t>
                | Let(v, _, _) -> ExpType.size <| Var.varType v

                // `(p: … -> r)(…)` => sizeof<r>
                | Call(p, _) -> ExpType.size <| Var.trivia(p).resultType

                // `(if … then … else …): t` => sizeof<t>
                | If _ -> ExpType.size <| Exp.varType e

                | _ -> 0
        
            let rider _ count e = count + localSize e.value
            let local _ _ = ()
            flow rider local () 0 e

module internal Seq =
    let tryMaxBound maxBound (xs: #seq<_>) =
        use e = xs.GetEnumerator()
        if not <| e.MoveNext() then ValueNone else

        let mutable currentMax = e.Current
        while
            begin
                if e.MoveNext() then
                    let x = e.Current
                    if maxBound <= x then
                        currentMax <- x
                        false
                    else
                        if currentMax <= x then
                            currentMax <- x
                        true
                else
                    false
            end
            do ()

        ValueSome currentMax

type EntityStat<'a> = private {
    procedureStats: Dictionary<ProcedureVar,'a ProcedureStat>
}
module private EntityStatHelpers =
    let findProcedure stat var =
        let mutable s = Unchecked.defaultof<_>
        if stat.procedureStats.TryGetValue(var, &s) then ValueSome s else ValueNone

    let updateY proc newY =
        proc.currentYieldability < newY && (proc.currentYieldability <- newY; true)

    let rec propagateY stat (visitedVars: _ HashSet) calleeY callee =
        if visitedVars.Add(ProcDef.var callee.procedure) then
            for { callee = caller; atomicity = callSiteAtomicity } in ProcedureStat.callers callee do
                let newY = yieldabilityAtAtomicity calleeY callSiteAtomicity
                if updateY caller newY then
                    propagateY stat visitedVars newY caller

    let propagateAllY stat =
        let visitedVars = HashSet()
        for kv in stat.procedureStats do
            let callee = kv.Value

            let y = ProcedureStat.yieldabilityWithoutCall callee
            let y = yieldabilityAtAtomicity y (ProcDef.atomicity callee.procedure)
            if updateY callee y then
                propagateY stat visitedVars y callee
                visitedVars.Clear()

        for kv in stat.procedureStats do
            kv.Value.yieldability <- ValueSome kv.Value.currentYieldability

    let yieldability stat p = cached &p.yieldability (fun () ->
        propagateAllY stat
        p.currentYieldability
    )
    let recursivityRaw stat root =
        let rec hasRootCall (visitedVars: _ HashSet) caller =
            if not <| visitedVars.Add caller then false else

            match findProcedure stat caller with
            | ValueNone -> false
            | ValueSome caller ->

            let callees = ProcedureStat.callees caller
            Map.containsKey root callees || Map.exists (fun k _ -> hasRootCall visitedVars k) callees

        let visitedVars = HashSet()
        let r = if hasRootCall visitedVars root then Recursive else NoRecursive
        r

    let maxDepthOrZero xs =
        xs
        |> Seq.tryMaxBound Infinity
        |> VOption.defaultValue (Finite 0)

    let stackDepthRaw stat p =
        let rec stackDepth stat (visitedVars: _ HashSet) caller =
            if not <| visitedVars.Add caller then Infinity else

            match findProcedure stat caller with
            | ValueNone -> Infinity
            | ValueSome caller -> cached &caller.stackDepth (fun () ->
                match yieldability stat caller with
                | NeverYield -> Finite 0
                | NormalYield
                | ForceYield ->

                let (ProcDef(body = body)) = caller.procedure.value
                let size = Finite <| Exp.measureLocalStackSize body
                let maxDepth =
                    ProcedureStat.callees caller
                    |> Seq.map (fun kv -> stackDepth stat visitedVars kv.Key)
                    |> maxDepthOrZero

                size + maxDepth
            )

        let visitedVars = HashSet()
        stackDepth stat visitedVars p

module EntityStat =
    open EntityStatHelpers

    let make entity =
        let parent = Dictionary()
        for s in entity.scripts do
            match s.script with
            | Top.Procedure p ->
                let stat = {
                    procedure = p
                    parent = parent
                    currentYieldability = NeverYield

                    yieldabilityWithoutCall = ValueNone
                    yieldability = ValueNone
                    recursivity = ValueNone
                    stackDepth = ValueNone
                    callees = ValueNone
                    callers = ValueNone
                }
                parent.Add(ProcDef.var p, stat)

            | _ -> ()
        {
            procedureStats = parent
        }

    let procedureYieldability stat var =
        let mutable r = Unchecked.defaultof<_>
        if stat.procedureStats.TryGetValue(var, &r) then
            yieldability stat r
        else
            NeverYield

    let listenerYieldability stat { Source.value = ListenerDef(body = body) } =
        let y = yieldabilityWithoutCallOperation NoAtomic body
        let ys = body |> Exp.getProcedureVars |> Seq.map (procedureYieldability stat)
        if Seq.isEmpty ys then y else Seq.append (Seq.singleton y) ys |> Seq.max

    let procedureRecursivity stat var =
        let mutable r = Unchecked.defaultof<_>
        if stat.procedureStats.TryGetValue(var, &r) then
            cached &r.recursivity (fun () ->
                recursivityRaw stat (ProcDef.var r.procedure)
            )
        else
            NoRecursive

    let procedureStackDepth stat var =
        let mutable r = Unchecked.defaultof<_>
        if stat.procedureStats.TryGetValue(var, &r) then
            cached &r.stackDepth (fun () ->
                stackDepthRaw stat var
            )
        else
            Infinity

    let listenerStackDepth stat { Source.value = ListenerDef(body = body) } =
        let size = Exp.measureLocalStackSize body
        let maxDepth =
            body
            |> Exp.getProcedureVars
            |> Seq.map (procedureStackDepth stat)
            |> maxDepthOrZero
        Finite size + maxDepth
