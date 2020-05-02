namespace Scratch.IR
open Scratch
open Scratch.Ast
open Scratch.AstDefinitions
open Scratch.IR
open Scratch.IR.Source.Operators
open Scratch.Primitives
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

module Types = ExpTypes
#nowarn "0086" // The '=' operator should not normally be redefined. To define equality semantics for a type, override the 'Object.Equals' member in the definition of that type.


module private CoreExpHelpers =
    let ofTsType t =
        if t = TsType.gNumber then Types.number
        elif t = TsType.gString then Types.string
        elif t = TsType.gBoolean then Types.bool
        elif t = TsType.gUnit then Types.empty
        elif t = TsType.gUnknown then Types.any
        else

        match t with
        | TsType.StringSs _ -> Types.string
        | TsType.Or _
        | TsType.GVar _
        | TsType.Named _ -> Types.any

    let operatorResultTypeMap =
        knownAllOperatorMap
        |> Seq.map (fun kv ->
            let info = kv.Value
            let t =
                match info.kind with
                | Kind.Statement -> Types.empty
                | Kind.Expression -> ofTsType info.resultType
            kv.Key, t
        )
        |> dict
        |> Dictionary

    let operatorResultType op =
        let mutable t = Unchecked.defaultof<_>
        if operatorResultTypeMap.TryGetValue(op, &t) then
            t
        else
            failwithf "Unknown operator '%s'" (Symbol.name op)

    type IFuncO<'T1,'R> = IFuncR<'T1,'R>
    type IFuncI<'T1,'R> = IFuncR<'T1,'R>

    [<Struct; NoEquality; NoComparison>]
    type FoldFlameTag =
        | FoldExp
        | FoldOperands
        | FoldListOperands

    [<Struct; NoEquality; NoComparison>]
    type FoldFlame<'a> = {
        mutable tag: FoldFlameTag
        mutable foldExp: 'a Exp
        mutable foldOperands: 'a Exp list
        mutable foldListOperands: Choice<'a Exp, 'a ListVar> list
    }
    module FoldFlame =
        module Unchecked =
            let foldExp (f: _ inref) = f.foldExp
            let foldOperands (f: _ inref) = f.foldOperands
            let foldListOperands (f: _ inref) = f.foldListOperands

        [<Struct; NoEquality; NoComparison>]
        type InitExp<'a> = { e: 'a Exp } with
            interface IFuncO<'a FoldFlame, HUnit> with
                [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
                member f.Invoke r = r.tag <- FoldExp; r.foldExp <- f.e; HUnit

        [<Struct; NoEquality; NoComparison>]
        type InitOperands<'a> = { es: 'a Exp list } with
            interface IFuncO<'a FoldFlame, HUnit> with
                [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
                member f.Invoke r = r.tag <- FoldOperands; r.foldOperands <- f.es; HUnit

        [<Struct; NoEquality; NoComparison>]
        type InitListOperands<'a> = { xs: Choice<'a Exp, 'a ListVar> list } with
            interface IFuncO<'a FoldFlame, HUnit> with
                [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
                member f.Invoke r = r.tag <- FoldListOperands; r.foldListOperands <- f.xs; HUnit

    [<Struct; NoEquality; NoComparison>]
    type SmallStack<'a> = {
        mutable item0: 'a
        mutable item1: 'a
        mutable item2: 'a
        mutable item3: 'a
        mutable items: 'a array
        mutable size: int
    }
    module SmallStack =
        let private fixedSize = 4
        let private initialItemsCount =
#if DEBUG
                1
#else
                8
#endif
        let newEmpty() = Unchecked.defaultof<_ SmallStack>

        [<MethodImpl(MethodImplOptions.NoInlining)>]
        let private alloc (s: _ byref) =
            s.items <- Array.zeroCreate initialItemsCount

        [<MethodImpl(MethodImplOptions.NoInlining)>]
        let private ensure (s: _ byref) =
            let items = Array.zeroCreate (s.items.Length * 2)
            Array.blit s.items 0 items 0 s.items.Length
            s.items <- items

        let count (s: _ inref) = s.size

        module Unchecked =
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            let pushInit (s: _ byref) (init: 'F byref when 'F :> IFuncO<_,_> and 'F : struct) =
                let size = s.size
                let r =
                    match size with
                    | 0 -> &s.item0
                    | 1 -> &s.item1
                    | 2 -> &s.item2
                    | 3 -> &s.item3
                    | _ ->
                        if isNull s.items then alloc &s
                        elif s.items.Length <= size - fixedSize then ensure &s
                        else ()
                        &s.items.[size - fixedSize]

                HUnit.toUnit(init.Invoke &r)
                s.size <- size + 1

            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            let popUsing (s: _ byref) (using: 'F byref when 'F :> IFuncI<_,_> and 'F : struct) =
                let size = s.size
                let r =
                    match size with
                    | 0 -> failwith "empty stack"; &s.items.[-1]
                    | 1 -> &s.item0
                    | 2 -> &s.item1
                    | 3 -> &s.item2
                    | 4 -> &s.item3
                    | size -> &s.items.[size - fixedSize - 1]

                s.size <- size - 1
                using.Invoke &r

    module SmallStack = SmallStack.Unchecked

    let foldOperands' es (stack: _ byref) =
        match es with
        | [] -> ()
        | e::es ->
            let mutable f = { FoldFlame.InitOperands.es = es }
            SmallStack.pushInit &stack &f
            let mutable f = { FoldFlame.InitExp.e = e }
            SmallStack.pushInit &stack &f

    let foldListOperands' xs (stack: _ byref) =
        match xs with
        | [] -> ()
        | Choice1Of2 e::xs ->
            let mutable f = { FoldFlame.InitListOperands.xs = xs }
            SmallStack.pushInit &stack &f
            let mutable f = { FoldFlame.InitExp.e = e }
            SmallStack.pushInit &stack &f

        | Choice2Of2 _::xs ->
            let mutable f = { FoldFlame.InitListOperands.xs = xs }
            SmallStack.pushInit &stack &f

    [<Struct; NoEquality; NoComparison>]
    type FoldDispacher<'s,'a,'F> when 'F :> IFunc<struct('s * 'a Exp),'s> and 'F : struct = {
        mutable state: 's
        mutable folder: 'F
        mutable stack: 'a FoldFlame SmallStack
    }
    with
        member inline d.Loop() =
            while 0 < SmallStack.count &d.stack do
                HUnit.toUnit(SmallStack.popUsing &d.stack &d)

        interface IFuncI<'a FoldFlame, HUnit> with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member d.Invoke flame =
                match flame.tag with
                | FoldOperands -> foldOperands' (FoldFlame.Unchecked.foldOperands &flame) &d.stack; HUnit
                | FoldListOperands -> foldListOperands' (FoldFlame.Unchecked.foldListOperands &flame) &d.stack; HUnit
                | FoldExp ->

                let e = FoldFlame.Unchecked.foldExp &flame
                d.state <- d.folder.Invoke struct(d.state, e)

                match e.value with
                | Var _
                | Lit _ -> ()

                | VarSet(_, e1)
                | TupleGet(e1, _)
                | TupleSet(_, _, e1)
                | Coerce(_, e1, _)
                | Atom e1 ->
                    let mutable f = { FoldFlame.InitExp.e = e1 }
                    SmallStack.pushInit &d.stack &f

                | Let(_, e1, e2)
                | Seq(e1, e2) ->
                    let mutable f = { FoldFlame.InitExp.e = e2 }
                    SmallStack.pushInit &d.stack &f
                    let mutable f = { FoldFlame.InitExp.e = e1 }
                    SmallStack.pushInit &d.stack &f

                | If(e1, e2, e3) ->
                    let mutable f = { FoldFlame.InitExp.e = e3 }
                    SmallStack.pushInit &d.stack &f
                    let mutable f = { FoldFlame.InitExp.e = e2 }
                    SmallStack.pushInit &d.stack &f
                    let mutable f = { FoldFlame.InitExp.e = e1 }
                    SmallStack.pushInit &d.stack &f

                | Call(_, es)
                | NewTuple es
                | Op(_, es) ->
                    let mutable f = { FoldFlame.InitOperands.es = es }
                    SmallStack.pushInit &d.stack &f

                | ListOp(_, xs) ->
                    let mutable f = { FoldFlame.InitListOperands.xs = xs }
                    SmallStack.pushInit &d.stack &f

                HUnit

    let fold (folder: 'F byref when 'F :> IFunc<_,_> and 'F : struct) state e =
        let mutable d = {
            state = state
            folder = folder // NOTE: `folder = &folder` ができれば下のマーシャリングは必要ない
            stack = SmallStack.newEmpty()
        }
        let mutable f = { FoldFlame.InitExp.e = e }
        SmallStack.pushInit &d.stack &f
        d.Loop()
        folder <- d.folder // byref のマーシャリング
        d.state

    [<Struct>]
    type GetUniquesState<'a> when 'a : comparison = {
        set: 'a HashSet
        result: 'a ResizeArray
    }
    let add s p =
        let s =
            match s.set with
            | null -> { set = HashSet(); result = ResizeArray() }
            | _ -> s

        if s.set.Add p then
            s.result.Add p
        s

    let rec freeVars env vars e =
        match e.value with
        | Lit _ -> vars
        | Var v -> if Set.contains v env then vars else add vars v
        | VarSet(v, e)
        | TupleSet(v, _, e) ->
            let vars = if Set.contains v env then vars else add vars v
            freeVars env vars e

        | TupleGet(e, _)
        | Coerce(_, e, _)
        | Atom e -> freeVars env vars e

        | Seq(e1, e2) -> freeVars env (freeVars env vars e1) e2
        | If(e1, e2, e3) -> freeVars env (freeVars env (freeVars env vars e1) e2) e3
        | Let(v, value, scope) ->
            let vars = freeVars env vars value
            freeVars (Set.add v env) vars scope

        | Call(_, es)
        | Op(_, es)
        | NewTuple es -> freeVarsList env vars es
        | ListOp(_, xs) -> freeVarsListOps env vars xs

    and freeVarsList env vars = function
        | [] -> vars
        | e::es -> freeVarsList env (freeVars env vars e) es

    and freeVarsListOps env vars = function
        | [] -> vars
        | Choice1Of2 e::es -> freeVarsListOps env (freeVars env vars e) es
        | Choice2Of2 _::es -> freeVarsListOps env vars es

    [<AbstractClass; Sealed>]
    type TypeFieldTableHolder<'a> private () =
        static let value = ConditionalWeakTable<Exp'<'a>,_>()
        static member Value = value
    [<GeneralizableValue>]
    let typeFieldTable<'a> = TypeFieldTableHolder<'a>.Value

    module rec TypeRec =
        let typeRaw = function
            | Var v -> Var.varType v
            | VarSet _ -> Types.empty

            | Lit(SNumber _) -> Types.number
            | Lit(SString _) -> Types.string
            | Lit(SBool _) -> Types.bool

            | If(test, ifTrue, ifFalse) -> typeIf (test, ifTrue, ifFalse)
            | Let(scope = e)
            | Seq(last = e)
            | Atom e -> typeField e

            | Call(proc = proc) -> Var.trivia(proc).resultType
            | Op(operator = op)
            | ListOp(operator = op) -> operatorResultType op

            | NewTuple es -> typeNewTuple es
            | TupleGet(e, i) -> typeTupleGet e i
            | TupleSet _ -> Types.empty

            | Coerce(newType = t) -> t

        let typeIf (_, ifTrue, ifFalse) =
            match typeField ifTrue, typeField ifFalse with
            | UnboxedType _ as t, UnboxedType _ -> t

        let typeNewTuple = function
            | [] -> Types.empty
            | [e] -> typeField e
            | es ->

            let rec aux acc = function
                | [] ->
                    match acc with
                    | []
                    | [[]] -> Types.empty
                    | [[t]] -> ExpType.ofMemberType t
                    | _ -> List.rev acc |> List.concat |> UnboxedType

                | e::es ->

                match typeField e with
                | UnboxedType ts -> aux (ts::acc) es

            aux [] es

        let typeTupleGet e i =
            match typeField e with
            | UnboxedType ts -> ts |> List.item i |> ExpType.ofMemberType

        [<AbstractClass; Sealed>]
        type TypeRawDelegateHolder<'a> private () =
            static let value = ConditionalWeakTable<'a Exp',_>.CreateValueCallback typeRaw
            static member Value = value
        [<GeneralizableValue>]
        let typeRawDelegate<'a> = TypeRawDelegateHolder<'a>.Value

        let typeField<'a> (x: 'a Exp): ExpType = typeFieldTable.GetValue(x.value, typeRawDelegate)
open CoreExpHelpers

module Exp =
    let varType e = TypeRec.typeField e
    let getFreeVars e =
        let s = { set = null; result = null }
        let s = freeVars Set.empty s e
        match s.result with
        | null -> [] :> _ seq
        | xs -> xs :> _ seq

    [<Struct; NoComparison; NoEquality>]
    type private AddProcedureVar<'a> = | AddProcedureVar with
        interface IFunc<struct(ProcedureVar GetUniquesState * 'a Exp), ProcedureVar GetUniquesState> with
            member _.Invoke struct(s, e) =
                match e.value with
                | Call(p, _) -> add s p
                | _ -> s

    let getProcedureVars e =
        let mutable f = AddProcedureVar
        let s = { set = null; result = null }
        let s = fold &f s e
        match s.result with
        | null -> [] :> _ seq
        | xs -> xs :> _ seq

    let fold folder state e =
        let mutable f = Func.ofFun2 folder
        fold &f state e

[<AutoOpen>]
module private ExpStartConstructorHelpers =
    let showVar v = sprintf "'%s' (id = %d)" (Var.name v) (let (VarId id) = Keyed.key v in id)

    let showSType = function SType.N -> "n" | SType.S -> "s" | SType.B -> "b"
    let showVType = function Any -> "any" | Typed t -> showSType t
    let showMemberType t =
        let u = MemberType.underlyingType t
        match MemberType.memberName t with
        | null
        | "" -> showVType u
        | n -> sprintf "%s: %s" n (showVType u)

    let showType = function
        | UnboxedType t ->
        t
        |> List.toSeq
        |> Seq.map showMemberType
        |> String.concat ", "
        |> sprintf "(%s)"

    let showOperandType = function
        | OperandType.ListVariableExpression t -> sprintf "list %s" (showType <| ofTsType t)
        | OperandType.Block -> showType Types.empty
        | OperandType.Expression t -> showType <| ofTsType t
        | OperandType.Reporter
        | OperandType.Rotation
        | OperandType.Stop
        | OperandType.StopScript
            -> showType Types.string

        | OperandType.Variable -> "variable"
        | OperandType.VariadicExpressions -> "[...]"

    let showTypes ts = ts |> Seq.map showType |> String.concat " " |> sprintf "'%s'"

    [<Struct; RequireQualifiedAccess>]
    type private LocationTag =
        | Exp
        | Top

    [<Struct; StructLayout(LayoutKind.Auto)>]
    type Location<'a> = private {
        tag: LocationTag
        expOrDefault: 'a Exp
        topOrDefault: 'a Top
    }
    module Top =
        let source = function
            | Top.VariableInit(VariableInitDef(init = e)) -> e.source
            | Top.Listener x -> x.source
            | Top.Procedure x -> x.source

    module Location =
        let ofExp e = { tag = LocationTag.Exp; expOrDefault = e; topOrDefault = Unchecked.defaultof<_> }
        let ofTop t = { tag = LocationTag.Top; expOrDefault = Unchecked.defaultof<_>; topOrDefault = t }
        let pretty (s: _ inref) =
            match s.tag with
            | LocationTag.Exp -> Pretty.prettyExp s.expOrDefault
            | LocationTag.Top -> Scratch.IR.Pretty.prettyTop s.topOrDefault

        let source (s: _ inref) =
            match s.tag with
            | LocationTag.Exp -> s.expOrDefault.source
            | LocationTag.Top -> Top.source s.topOrDefault

    [<Struct>]
    type TestPair<'a> = { expected: 'a; actual: 'a }
    module TestPair =
        let inline map f x = { expected = f x.expected; actual = f x.actual }

    let locationFootter (source: _ inref) = sprintf "source: ( %s ), location: ( %A )" (Location.pretty &source) (Location.source &source)

    let raiseTypeMismatch (source: _ inref) { expected = expected; actual = actual } =
        invalidOp <| sprintf "Type mismatch, expected: %s, actual: %s, %s" expected actual (locationFootter &source)

    let raiseNotMutable (source: _ inref) v =
        invalidOp <| sprintf "Var %s is not mutable, %s" (showVar v) (locationFootter &source)

    let raiseUnknownOperator (source: _ inref) operator =
        invalidOp <| sprintf "Unknown operator, '%s', %s" (Symbol.name operator) (locationFootter &source)

    let raiseNotAllowedOperator (source: _ inref) operator =
        invalidOp <| sprintf "Operator '%s' is not allowed, Please use a alternative format, %s" (Symbol.name operator) (locationFootter &source)

    let raiseTupleIndexOutOfRange (source: _ inref) t i =
        invalidOp <| sprintf "Tuple index is out of range, type %s, index %d, %s" (showType t) i (locationFootter &source)

    let raiseTypeMismatchAnd (source: _ inref) t1 t2 =
        invalidOp <| sprintf "Type mismatch, %s and %s, %s" t1 t2 (locationFootter &source)

    let raiseTypeMismatchList (source: _ inref) operand =
        invalidOp <| sprintf "Type mismatch, expected: list, actual: %s, %s" (showType (Exp.varType operand)) (locationFootter &source)

    let raiseArityMismatch (source: _ inref) allTypes allOperands =
        invalidOp <| sprintf "Arity mismatch, expected: %s, actual: %s, %s"
            (allTypes |> String.concat " " |> sprintf "'%s'")
            allOperands
            (locationFootter &source)

    let raiseCoerceNotPossible (source: _ inref) t newType =
        invalidOp <| sprintf "%s to %s coerce is not possible, The size of the type is different, %s" (showType t) (showType newType) (locationFootter &source)

    let checkAssignType (source: _ inref) test =
        if not <| ExpType.isAssignable test.expected test.actual then
            raiseTypeMismatch &source (TestPair.map showType test)

    let checkAssignMemberType (source: _ inref) test =
        if not <| MemberType.isAssignable test.expected test.actual then
            raiseTypeMismatch &source (TestPair.map showMemberType test)

    let rec checkArgTypesAux (source: _ inref) varParameterTypes arguments = function
        | [], [] -> ()
        | t::ts, a::args ->
            checkAssignType &source { expected = t; actual = Exp.varType a }
            checkArgTypesAux &source varParameterTypes arguments (ts, args)

        | _ ->
            let argTypes = arguments |> List.map Exp.varType
            raiseTypeMismatch &source { expected = showTypes varParameterTypes; actual = showTypes argTypes }

    let checkArgTypes (source: _ inref) varParameterTypes arguments =
        checkArgTypesAux &source (List.toSeq varParameterTypes) arguments (varParameterTypes, arguments)

    let checkMutable (source: _ inref) v =
        if not <| Var.isMutable v then raiseNotMutable &source v

    let checkTupleIndex (source: _ inref) t i =
        match t with
        | UnboxedType ts ->
            if i < 0 || List.length ts <= i then
                raiseTupleIndexOutOfRange &source t i

    let inline checkTypeEq (source: _ inref) show t1 t2 =
        if not <| ExpType.equals t1 t2 then
            raiseTypeMismatchAnd &source (show t1) (show t2)

    let checkTypeEquals (source: _ inref) t1 t2 = checkTypeEq &source showType t1 t2

    let checkVarType (source: _ inref) var value =
        let varType = Var.varType var
        let valueType = Exp.varType value
        checkAssignType &source { expected = varType; actual = valueType }

    let checkPrimitiveVarType (source: _ inref) varType index value =
        match varType, Exp.varType value with

        // (v: [..]).N <- (v: [] | [_;_;..])
        | UnboxedType _, (UnboxedType([] | _::_::_) as valueType) -> raiseTypeMismatch &source { expected = showType varType; actual = showType valueType }

        // (v: [..]).N <- (v: [_])
        | UnboxedType varType, UnboxedType [t] -> checkAssignMemberType &source { expected = varType.[index]; actual = t }

    let checkOperand (source: _ inref) operand = function
        | OperandType.Block ->
            checkAssignType &source { actual = Exp.varType operand; expected = Types.empty }

        // TODO:
        | OperandType.ListVariableExpression _ -> ()
        | OperandType.Variable -> ()
        | OperandType.VariadicExpressions -> ()

        | OperandType.Expression t -> checkAssignType &source { actual = Exp.varType operand; expected = ofTsType t }
        | OperandType.Reporter
        | OperandType.Rotation
        | OperandType.Stop
        | OperandType.StopScript
            -> checkAssignType &source { actual = Exp.varType operand; expected = Types.string }

    let checkExpressionTypeInListOperand (source: _ inref) operand expectedType =
        match operand with
        | Choice1Of2 operand -> checkAssignType &source { actual = Exp.varType operand; expected = expectedType }
        | Choice2Of2 _ -> ()

    let checkListOperand (source: _ inref) operand = function
        | OperandType.Block -> checkExpressionTypeInListOperand &source operand Types.empty

        // TODO:
        | OperandType.ListVariableExpression _ ->
            match operand with
            | Choice1Of2 operand -> raiseTypeMismatchList &source operand
            | Choice2Of2 _ -> ()

        // TODO:
        | OperandType.Variable -> ()
        | OperandType.VariadicExpressions -> ()

        | OperandType.Expression t -> checkExpressionTypeInListOperand &source operand (ofTsType t)
        | OperandType.Reporter
        | OperandType.Rotation
        | OperandType.Stop
        | OperandType.StopScript
            -> checkExpressionTypeInListOperand &source operand Types.string

    let rec checkOperandsAux (source: _ inref) allOperands allTypes = function
        | [], [] -> ()
        | o::operands, t::types ->
            checkOperand &source o t
            checkOperandsAux &source allOperands allTypes (operands, types)

        | _ ->
            raiseArityMismatch &source
                (allTypes |> Seq.map showOperandType)
                (allOperands |> Seq.map Exp.varType |> showTypes)

    let checkOperands (source: _ inref) operands types =
        checkOperandsAux &source (List.toSeq operands) (List.toSeq types) (operands, types)

    let showListOperandType = function
        | Choice1Of2 t -> Exp.varType t |> showType
        | Choice2Of2 _ -> "list"

    let rec checkListOperandsAux (source: _ inref) allOperands allTypes = function
        | [], [] -> ()
        | o::operands, t::types ->
            checkListOperand &source o t
            checkListOperandsAux &source allOperands allTypes (operands, types)

        | _ ->
            raiseArityMismatch &source
                (allTypes |> Seq.map showOperandType)
                (allOperands |> Seq.map showListOperandType |> String.concat " " |> sprintf "'%s'")

    let checkListOperands (source: _ inref) operands types =
        checkListOperandsAux &source (List.toSeq operands) (List.toSeq types) (operands, types)

    let isListOperator = function
        | O.``append:toList:``
        | O.``contentsOfList:``
        | O.``deleteLine:ofList:``
        | O.``getLine:ofList:``
        | O.``hideList:``
        | O.``insert:at:ofList:``
        | O.``lineCountOfList:``
        | O.``list:contains:``
        | O.``setLine:ofList:to:``
        | O.``showList:`` -> true
        | _ -> false

    let checkCoerceTypeSize (source: _ inref) t newType =
        match t, newType with

        // (x: [..]) :> [..]
        | UnboxedType t', UnboxedType newType' ->
            if List.length t' <> List.length newType' then
                raiseCoerceNotPossible &source t newType

    let checkShallow e =
        let source = Location.ofExp e
        match e.value with
        | Var _
        | Lit _
        | NewTuple _
        | Seq _
        | Atom _
            -> ()

        | ListOp(operator, operands) ->
            if not <| isListOperator operator then
                raiseNotAllowedOperator &source operator
            else
                match operator with
                | KnownOperatorInfo ValueNone -> raiseUnknownOperator &source operator
                | KnownOperatorInfo(ValueSome info) -> checkListOperands &source operands info.operands

        | VarSet(var, value) ->
            checkMutable &source var
            checkVarType &source var value

        | If(test, ifTrue, ifFalse) ->
            let testSource = Location.ofExp test
            checkAssignType &testSource { expected = Types.bool; actual = Exp.varType test }
            checkTypeEquals &source (Exp.varType ifTrue) (Exp.varType ifFalse)

        | Call(v, args) -> checkArgTypes &source (Var.trivia(v).parameterTypes) args
        | Let(var, value, _) -> checkVarType &source var value
        | TupleGet(xs, i) -> checkTupleIndex &source (Exp.varType xs) i
        | TupleSet(v, i, value) ->
            let varType = Var.varType v
            checkMutable &source v
            checkTupleIndex &source varType i
            checkPrimitiveVarType &source varType i value

        | Coerce(_, value, newType) ->
            let t = Exp.varType value
            checkCoerceTypeSize &source t newType

        | Op(operator, operands) ->
            match operator with
            | O.call
            
            | O.doIf
            | O.doIfElse

            | O.``&``
            | O.``|``

            | O.getParam
            | O.readVariable
            | O.``changeVar:by:``
            | O.``setVar:to:`` -> raiseNotAllowedOperator &source operator

            | _ when isListOperator operator -> raiseNotAllowedOperator &source operator

            | KnownOperatorInfo ValueNone -> raiseUnknownOperator &source operator

            | KnownOperatorInfo(ValueSome info) ->
                checkOperands &source operands info.operands

    let validateShallow e = checkShallow e; e

    let listOp source operator operands =
        ListOp(operator, operands) @+source |> validateShallow

    [<Sealed; AbstractClass>]
    type private EmptyHolder<'a> private () =
        static let value = Exp'<'a>.NewTuple []
        static member Value = value

    [<GeneralizableValue>]
    let empty<'a> = EmptyHolder<'a>.Value

    let rec concat s = function
        | [] -> empty @+s
        | [x] -> x
        | x::xs -> Seq(x, concat s xs) @+s

    let primitiveTypeConvert source e newType =
        let t = Exp.varType e
        if ExpType.equals t newType then e
        else Coerce(CoerceKind.Convert, e, newType) @+source |> validateShallow

[<RequireQualifiedAccess; Struct>]
type StopScriptKind = | All | ThisScript

[<RequireQualifiedAccess; Struct>]
type MathFunctionKind =
    | Abs
    | Floor
    | Sqrt
    | Ceiling
    | Cos
    | Sin
    | Tan
    | Asin
    | Acos
    | Atan
    | Ln
    | Log
    | PowE
    | Pow10

[<RequireQualifiedAccess; Struct>]
type Filter =
    | Color
    | Fisheye
    | Whirl
    | Pixelate
    | Mosaic
    | Brightness
    | Ghost

[<AutoOpen>]
module ExpSmartConstructorExtensions =
    [<Sealed; AbstractClass>]
    type private ZeroHolder<'a> private () =
        static let value = Exp'<'a>.Lit(SNumber 0.)
        static member Value = value
    [<GeneralizableValue>]
    let private zero<'a> = ZeroHolder<'a>.Value
    
    [<Sealed; AbstractClass>]
    type private OneHolder<'a> private () =
        static let value = Exp'<'a>.Lit(SNumber 1.)
        static member Value = value
    [<GeneralizableValue>]
    let private one<'a> = OneHolder<'a>.Value

    [<Sealed; AbstractClass>]
    type private TrueHolder<'a> private () =
        static let value = Exp'<'a>.Lit(SBool true)
        static member Value = value
    [<GeneralizableValue>]
    let private true'<'a> = TrueHolder<'a>.Value

    [<Sealed; AbstractClass>]
    type private FalseHolder<'a> private () =
        static let value = Exp'<'a>.Lit(SBool false)
        static member Value = value
    [<GeneralizableValue>]
    let private false'<'a> = FalseHolder<'a>.Value

    [<Sealed; AbstractClass>]
    type private EmptyStringHolder<'a> private () =
        static let value = Exp'<'a>.Lit(SString "")
        static member Value = value
    [<GeneralizableValue>]
    let private emptyString<'a> = EmptyStringHolder<'a>.Value

    module Exp =
        [<Struct; NoComparison; NoEquality>]
        type private CheckShallow<'a> = | CheckShallow with
            interface IFunc<struct(HUnit * 'a Exp), HUnit> with
                member _.Invoke struct(_, e) = checkShallow e; HUnit

        let validate e =
            let mutable f = CheckShallow
            HUnit.toUnit <| fold &f HUnit e
            e

        let lit source x =
            let e =
                match x with

                // +0
                | SNumber n when n = 0. && 1. / n = infinity -> zero

                | SNumber 1. -> one
                | SBool true -> true'
                | SBool false -> false'
                | SString "" -> emptyString

                | x -> Lit x

            e @+source

        let number source x = lit source (SNumber x)
        let string source x = lit source (SString x)
        let bool source x = lit source (SBool x)
        let param source v = Var v.value @+source

        let var source v = Var v @+source
        let list source v: _ ListVar = v @+source
        let varSet source v value =
            VarSet(v, value) @+source |> validateShallow

        let if' source test ifTrue ifFalse =
            If(test, ifTrue, ifFalse) @+source |> validateShallow

        let call source v args =
            Call(v, args) @+source |> validateShallow

        let let' source var value scope =
            Let(var, value, scope) @+ source |> validateShallow

        let inline internal letScope' letSource varSource isMutable varNeme value scope =
            let var = Var.newStorage varNeme isMutable (Exp.varType value)
            let v = { value = Var var; source = varSource }
            let' letSource var value (scope v var)

        let inline letScope source varNeme value scope =
            let var = Var.newStorage varNeme false (Exp.varType value)
            let v = { value = Var var; source = source }
            let' source var value (scope v)

        let newTuple source xs =
            let e =
                match xs with
                | [] -> empty
                | _ -> NewTuple xs

            e @+source |> validateShallow

        let tupleGet source xs i = TupleGet(xs, i) @+source |> validateShallow
        let tupleSet source v i value = TupleSet(v, i, value) @+source |> validateShallow

        let empty source = empty @+source
        let seq source first last = Seq(first, last) @+source |> validateShallow
        let concat source es = concat source es

        let reinterpret source target newType =
            if ExpType.equals (Exp.varType target) newType then target else

            Coerce(CoerceKind.Reinterpret, target, newType) @+source
            |> validateShallow

        let atom source scope = Atom scope @+source |> validateShallow

        let toAny source e = Coerce(CoerceKind.Reinterpret, e, Types.any) @+source |> validateShallow

        let toS source e = primitiveTypeConvert source e Types.string
        let toN source e = primitiveTypeConvert source e Types.number

        let toEmpty source e =
            let t = Exp.varType e
            if ExpType.equals t Types.empty then e
            else Seq(e, empty source) @+source |> validateShallow

        let op source operator operands =
            Op(operator, operands) @+source |> validateShallow

        module Op =
            let ``deleteLine:ofList:`` source line list = listOp source O.``deleteLine:ofList:`` [Choice1Of2 line; Choice2Of2 list]
            let ``append:toList:`` source value list = listOp source O.``append:toList:`` [Choice1Of2 value; Choice2Of2 list]
            let ``setLine:ofList:to:`` source line list value = listOp source O.``setLine:ofList:to:`` [Choice1Of2 line; Choice2Of2 list; Choice1Of2 value]

            let ``getLine:ofList:`` source line list = listOp source O.``getLine:ofList:`` [Choice1Of2 line; Choice2Of2 list]
            let ``lineCountOfList:`` source list = listOp source O.``lineCountOfList:`` [Choice2Of2 list]

            let ``showList:`` source list = listOp source O.``showList:`` [Choice2Of2 list]
            let ``hideList:`` source list = listOp source O.``hideList:`` [Choice2Of2 list]

            let ``showVariable:`` source v = op source O.``showVariable:`` [var source v]
            let ``hideVariable:`` source v = op source O.``hideVariable:`` [var source v]

            let ``&`` source l r = if' source l r (bool source false)
            let ``|`` source l r = if' source l (bool source true) r

            let ``+`` source l r = op source O.``+`` [l; r]
            let ``-`` source l r = op source O.``-`` [l; r]
            let ``*`` source l r = op source O.``*`` [l; r]
            let ``/`` source l r = op source O.``/`` [l; r]
            let ``%`` source l r = op source O.``%`` [l; r]
            let ``=`` source l r = op source O.``=`` [l; r]
            let ``<`` source l r = op source O.``<`` [l; r]
            let ``>`` source l r = op source O.``>`` [l; r]
            let ``concatenate:with:`` source l r = op source O.``concatenate:with:`` [l; r]

            let not source x = op source O.not [x]
            let rounded source x = op source O.rounded [x]
            let ``stringLength:`` source x = op source O.``stringLength:`` [x]
            let ``letter:of:`` source nth target = op source O.``letter:of:`` [nth; target]

            type private M = MathFunctionKind
            let ``computeFunction:of:`` source f value =
                let n =
                    match f with
                    | M.Abs -> "abs"
                    | M.Floor -> "floor"
                    | M.Sqrt -> "sqrt"
                    | M.Ceiling -> "ceiling"
                    | M.Cos -> "cos"
                    | M.Sin -> "sin"
                    | M.Tan -> "tan"
                    | M.Asin -> "asin"
                    | M.Acos -> "acos"
                    | M.Atan -> "atan"
                    | M.Ln -> "ln"
                    | M.Log -> "log"
                    | M.PowE -> "e ^"
                    | M.Pow10 -> "10 ^"

                op source O.``computeFunction:of:`` [string source n; value]

            let doRepeat source count body = op source O.doRepeat [count; body]
            let doForever source body = op source O.doForever [body]
            let doUntil source test ifFalse = op source O.doUntil [test; ifFalse]

            let ``wait:elapsed:from:`` source seconds = op source O.``wait:elapsed:from:`` [seconds]
            let ``broadcast:`` source name = op source O.``broadcast:`` [name]
            let doBroadcastAndWait source name = op source O.doBroadcastAndWait [name]
            let doWaitUntil source isNext = op source O.doWaitUntil [isNext]
            let doAsk source question = op source O.doAsk [question]

            let stopScripts source kind =
                let kind = match kind with StopScriptKind.All -> "all" | StopScriptKind.ThisScript -> "this script"
                op source O.stopScripts [string source kind]

            let timer source = op source O.timer []
            let ``keyPressed:`` source key = op source O.``keyPressed:`` [key]
            let mousePressed source = op source O.mousePressed []
            let mouseX source = op source O.mouseX []
            let mouseY source = op source O.mouseY []
            let answer source = op source O.answer []

            let xpos source = op source O.xpos []
            let ypos source = op source O.ypos []

            /// <param name="name">"_myself" | SpriteName</param>
            let createCloneOf source name = op source O.createCloneOf [string source name]
            let deleteClone source = op source O.deleteClone []
            let ``gotoX:y:`` source x y = op source O.``gotoX:y:`` [x; y]
            let show source = op source O.show []
            let hide source = op source O.hide []
            let ``lookLike:`` source costume = op source O.``lookLike:`` [costume]
            let ``setGraphicEffect:to:`` source filter value =
                let name =
                    match filter with
                    | Filter.Color -> "color"
                    | Filter.Fisheye -> "fisheye"
                    | Filter.Whirl -> "whirl"
                    | Filter.Pixelate -> "pixelate"
                    | Filter.Mosaic -> "mosaic"
                    | Filter.Brightness -> "brightness"
                    | Filter.Ghost -> "ghost"

                op source O.``setGraphicEffect:to:`` [string source name; value]

            let ``setSizeTo:`` source size = op source O.``setSizeTo:`` [size]

[<Struct>]
type OpExpBuilder<'T> = private { defaultState: 'T } with
    member b.DefaultSource = b.defaultState
    member private b.DefaultOr s = Option.defaultValue b.DefaultSource s

    member b.``+``(left, right, ?source) = Exp.Op.``+`` (b.DefaultOr source) left right
    member b.``-``(left, right, ?source) = Exp.Op.``-`` (b.DefaultOr source) left right
    member b.``*``(left, right, ?source) = Exp.Op.``*`` (b.DefaultOr source) left right
    member b.``/``(left, right, ?source) = Exp.Op.``/`` (b.DefaultOr source) left right
    member b.``%``(left, right, ?source) = Exp.Op.``%`` (b.DefaultOr source) left right
    member b.``=``(left, right, ?source) = Exp.Op.``=`` (b.DefaultOr source) left right
    member b.``<``(left, right, ?source) = Exp.Op.``<`` (b.DefaultOr source) left right
    member b.``>``(left, right, ?source) = Exp.Op.``>`` (b.DefaultOr source) left right
    member b.``&``(left, right, ?source) = Exp.Op.``&`` (b.DefaultOr source) left right
    member b.``|``(left, right, ?source) = Exp.Op.``|`` (b.DefaultOr source) left right
    member b.``concatenate:with:``(left, right, ?source) = Exp.Op.``concatenate:with:`` (b.DefaultOr source) left right

    member b.not(value, ?source) = Exp.Op.not (b.DefaultOr source) value
    member b.rounded(value, ?source) = Exp.Op.rounded (b.DefaultOr source) value
    member b.``computeFunction:of:``(kind, value, ?source) = Exp.Op.``computeFunction:of:`` (b.DefaultOr source) kind value

    member b.``append:toList:``(value, list, ?listSource, ?source) = Exp.Op.``append:toList:`` (b.DefaultOr source) value (Exp.list (b.DefaultOr listSource) list)
    member b.``deleteLine:ofList:``(line, list, ?listSource, ?source) = Exp.Op.``deleteLine:ofList:`` (b.DefaultOr source) line (Exp.list (b.DefaultOr listSource) list)

    member b.doRepeat(count, body, ?source) = Exp.Op.doRepeat (b.DefaultOr source) count body
    member b.doForever(body, ?source) = Exp.Op.doForever (b.DefaultOr source) body
    member b.doUntil(test, ifFalse, ?source) = Exp.Op.doUntil (b.DefaultOr source) test ifFalse

    member b.stopScripts(kind, ?source) = Exp.Op.stopScripts (b.DefaultOr source) kind

[<Struct>]
type ExpBuilder<'T> = private { b: OpExpBuilder<'T> } with
    member b.DefaultSource = b.b.defaultState
    member private b.DefaultOr s = Option.defaultValue b.DefaultSource s

    member b.number(value, ?source) = Exp.number (b.DefaultOr source) value
    member b.string(value, ?source) = Exp.string (b.DefaultOr source) value
    member b.bool(value, ?source) = Exp.bool (b.DefaultOr source) value

    member b.param(param, ?source) = Exp.param (b.DefaultOr source) param

    member b.list(var, ?source) = Exp.list (b.DefaultOr source) var
    member b.var(var, ?source) = Exp.var (b.DefaultOr source) var
    member b.varSet(var, value, ?source) = Exp.varSet (b.DefaultOr source) var value

    member b.let'(var, value, scope, ?source) = Exp.let' (b.DefaultOr source) var value scope
    member b.letScope(varName, value) scope = Exp.letScope' b.DefaultSource b.DefaultSource false varName value <| fun v _ -> scope v
    member b.letMutableScope(varName, value) scope = Exp.letScope' b.DefaultSource b.DefaultSource true varName value scope

    member b.if'(test, ifTrue, ifFalse, ?source) = Exp.if' (b.DefaultOr source) test ifTrue ifFalse

    member b.empty(?source) = Exp.empty (b.DefaultOr source)
    member b.seq(first, last, ?source) = Exp.seq (b.DefaultOr source) first last
    member b.concat(expressions, ?source) = Exp.concat (b.DefaultOr source) (List.ofSeq expressions)

    member b.newTuple(expressions, ?source) = Exp.newTuple (b.DefaultOr source) expressions
    member b.tupleGet(tuple, index, ?source) = Exp.tupleGet (b.DefaultOr source) tuple index
    member b.tupleSet(var, index, value, ?source) = Exp.tupleSet (b.DefaultOr source) var index value

    member b.call(var, arguments, ?source) = Exp.call (b.DefaultOr source) var arguments

    member b.toAny(value, ?source) = Exp.toAny (b.DefaultOr source) value
    member b.toS(value, ?source) = Exp.toS (b.DefaultOr source) value

    member b.op(operator, operands, ?source) = Exp.op (b.DefaultOr source) operator operands

    member b.Op = b.b

[<AutoOpen>]
module ExpBuilderExtensions =
    module Exp =
        let builderWith defaultSource = { b = { defaultState = defaultSource } }

module Param =
    let make source name paramType = Var.newStorage name false paramType @+source

module ProcDef =
    let make source var ps atomicity body = ProcDef(ProcHeader(var, ps, atomicity), body) @+source

    let var { Source.value = ProcDef(header = ProcHeader(var = var)) } = var
    let body { Source.value = ProcDef(body = body) } = body
    let atomicity { Source.value = ProcDef(header = ProcHeader(atomicity = a)) } = a

[<AutoOpen>]
module private TopHelpers =
    let rec checkParameterTypesAux (source: _ inref) varParameterTypes parameters = function
        | [], [] -> ()
        | t::ts, p::ps ->
            let p = Var.varType p.value
            checkAssignType &source { expected = t; actual = p }
            checkParameterTypesAux &source varParameterTypes parameters (ts, ps)

        | _ ->
            let parameterTypes = parameters |> Seq.map (fun x -> Var.varType x.value)
            raiseTypeMismatch &source { expected = showTypes varParameterTypes; actual = showTypes parameterTypes }

    let checkParameterTypes (source: _ inref) varParameterTypes parameters =
        checkParameterTypesAux &source (List.toSeq varParameterTypes) (List.toSeq parameters) (varParameterTypes, parameters)

module Top =
    let proc source var atomicity ps body =
        let t = Top.Procedure(ProcDef.make source var ps atomicity body)
        let source = Location.ofTop t

        let procType = Var.trivia var
        let resultType = Exp.varType body
        checkParameterTypes &source procType.parameterTypes ps
        checkAssignType &source { expected = procType.resultType; actual = resultType }
        t

    let procAndNewVar source atomicity name ps body =
        let parameterTypes = ps |> List.map (fun x -> Var.varType x.value)
        let resultType = Exp.varType body
        let var = Var.newProc name parameterTypes resultType
        let proc = proc source var atomicity ps body
        proc, var

    let procNewVar source atomicity name ps body =
        fst <| procAndNewVar source atomicity name ps body

    let listener source symbol args body =
        let t = Top.Listener(ListenerDef(symbol, args, body) @+source)

        let bodyType = Exp.varType body
        let source = Location.ofTop t
        checkAssignType &source { expected = Types.empty; actual = bodyType }
        t

    module Listener =
        let whenGreenFlag source body = listener source O.whenGreenFlag [] body
        let whenIReceive source name body = listener source O.whenIReceive [Exp.string source name] body
        let whenCloned source body = listener source O.whenCloned [] body

[<Struct>]
type TopListenerBuilder<'a> = private { defaultState: 'a } with
    member b.DefaultSource = b.defaultState
    member private b.DefaultOr s = Option.defaultValue b.DefaultSource s
    member b.whenGreenFlag(body, ?source) = Top.Listener.whenGreenFlag (b.DefaultOr source) body

[<Struct>]
type TopBuilder<'a> = private { b: 'a TopListenerBuilder } with
    member b.DefaultSource = b.b.defaultState
    member private b.DefaultOr s = Option.defaultValue b.DefaultSource s

    member b.proc(var, parameters, body, ?atomicity, ?source) = Top.proc (b.DefaultOr source) var (Option.defaultValue Atomic atomicity) parameters body
    member b.procNewVar(name, parameters, body, ?atomicity, ?source) = Top.procNewVar (b.DefaultOr source) (Option.defaultValue Atomic atomicity) name parameters body
    member b.procAndNewVar(name, parameters, body, ?atomicity, ?source) = Top.procAndNewVar (b.DefaultOr source) (Option.defaultValue Atomic atomicity) name parameters body

    member b.Exp = Exp.builderWith b.b.defaultState
    member b.Listener = b.b

[<AutoOpen>]
module TopBuilderExtensions =
    module Top =
        let builderWith defaultSource = { b = { defaultState = defaultSource } }

module VariableDecl =
    let make state var persistance: _ VariableDecl =
        {
            state = state
            var = var
            isPersistent = persistance
            init = HUnit
            view = HUnit
        }

module ListInitData =
    let make state var init: _ ListDef = {
        state = state
        var = var
        isPersistent = NoPersistent
        init = init
        view = {
            x = 0.
            y = 0.
            width = 0.
            height = 0.
            visible = Hidden
        }
    }
