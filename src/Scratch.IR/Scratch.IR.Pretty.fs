module Scratch.IR.Pretty
open Scratch
open Scratch.Ast
open Scratch.IR
open Scratch.Primitives
open Scratch.Primitives.Document.Constructors
open Scratch.Primitives.Document.Operators
open System.Collections.Generic
open System.Text.RegularExpressions


type private Namespace<'k,'n> when 'k : comparison and 'n : comparison = {
    names: 'n HashSet
    keyToName: Dictionary<'k,'n>
    naming: struct('k * int) -> 'n
}
type State = private { ns: Namespace<SimpleVar, string> }

[<AutoOpen>]
module private Helpers =
    [<Struct>]
    type UniqueName<'a> = UniqueName of 'a
    module UniqueName =
        let name (UniqueName n) = n

    module Namespace =
        let newNamespace naming = { names = HashSet(); keyToName = Dictionary(); naming = naming }

        let private freshName key { names = set; naming = naming } =
            let rec aux i =
                let n = naming(key, i)
                if set.Add n then n else
                aux (i + 1)
            aux 0

        let uniqueName ({ keyToName = keyToName } as ns) key =
            let mutable r = Unchecked.defaultof<_>
            if keyToName.TryGetValue(key, &r) then UniqueName r else

            let uniqueName = freshName key ns
            keyToName.Add(key, uniqueName)
            UniqueName uniqueName

    [<RequireQualifiedAccess>]
    type Precedence =
        /// `(a)` `name` `10` `a.b`
        | Primitive
        /// `f a`
        | Apply
        | Mul
        | Add
        | Rel
        | Eq
        /// `a :> t`
        | TypeOp
        /// `a, b, c`
        | Comma
        /// `x <- a`
        | Assign
        /// `if a then b else c`
        | If
        | Let
        | Seq
        | Max

    module Reflection =
        open FSharp.Reflection

        let unionTag (union: 'Union) =
            let c, _ = FSharpValue.GetUnionFields(union, typeof<'Union>, allowAccessToPrivateRepresentation = true)
            c.Tag

        let makeNoFieldUnionOfTag (tag: int) =
            let cs = FSharpType.GetUnionCases(typeof<'EnumLikeUnion>, allowAccessToPrivateRepresentation = true)
            let c = cs |> Array.find (fun c -> c.Tag = tag)
            FSharpValue.MakeUnion(c, null, allowAccessToPrivateRepresentation = true) :?> 'EnumLikeUnion

    module Precedence =
        let sub (p: Precedence) n: Precedence =
            Reflection.unionTag p - n
            |> max (Reflection.unionTag Precedence.Primitive)
            |> min (Reflection.unionTag Precedence.Max)
            |> Reflection.makeNoFieldUnionOfTag

    module Document =
        /// `nest (group (ns + x))`
        let layoutBlock x = nest (group (ns ++ x))

[<AutoOpen>]
module private Printers =
    type private P = Precedence

    /// `()` -or- `group (f xs[0]) ++ ns ++ group (f xs[1]) ++ … ++ group (f xs[N])`
    let prettyListOrUnit f = function
        | [] -> text "()"
        | xs -> Document.concatMap (group << f) ns xs

    // TODO: escape
    let prettyName n = text n
    let prettySymbol symbol = prettyName (Symbol.name symbol)

    /// `opName` -or- `system opName`
    let prettyOperator _ op =
        //if Set.contains (Symbol.name op) state.procedures then
        //    struct(text "system" ++ ws ++ prettySymbol op, P.Application)
        //else
        struct(prettySymbol op, P.Primitive)

    let prettyVar s var = Namespace.uniqueName (Pretty.State.pluginState s).ns (Var.simple var) |> UniqueName.name |> prettyName
    let prettyMutability var = if Var.isMutable var then text "mutable " else empty

    let stringLiteralEscapeRegex = Regex @"\\"""
    let escape = MatchEvaluator(fun m -> @"\" + m.Value)
    let prettyStringLiteral = function
        | null -> text "<null>"
        | "" -> text "\"\""
        | x ->

        sequence [|
            text "\""
            text (stringLiteralEscapeRegex.Replace(x, escape))
            text "\""
        |]

    let prettySType = function
        | SType.N -> text "n"
        | SType.B -> text "b"
        | SType.S -> text "s"

    let prettyVType = function
        | Any -> text "any"
        | Typed t -> prettySType t

    let prettyMemberType t =
        let name = MemberType.memberName t
        let t = MemberType.underlyingType t
        let t = prettyVType t
        match name with
        | "" -> t
        | _ -> prettyName name +. " : " ++ t

    let prettyType = function
        | UnboxedType [t] -> prettyMemberType t
        | UnboxedType ts -> "(" .+ Document.concatMap prettyMemberType (text ", ") ts +. ")"

    let prettyNumberLiteral (x: double) = text (x.ToString "G17")

    let prettyLit = function
        | SBool true -> text "true"
        | SBool false -> text "false"
        | SNumber x -> prettyNumberLiteral x
        | SString x -> prettyStringLiteral x

    let rec prettyExpRaw s = function
        | Lit x -> struct(prettyLit x, P.Primitive)
        | Var v -> prettyVar s v, P.Primitive
        | VarSet(v, value) -> prettyVarSet s (v, value)

        | If(test, ifTrue, ifFalse) -> prettyIf s (test, ifTrue, ifFalse)
        | Let(var, value, scope) -> prettyLet s (var, value, scope)
        | Seq(first, last) -> prettySeq s (first, last)

        | Call(proc, args) -> prettyCall s (proc, args)

        | Op(op, ops) -> prettyOp s (op, ops)
        | ListOp(op, ops) -> prettyListOp s (op, ops)
        | ExtOp(spec, ops) -> prettyExtOp s (spec, ops)

        | NewTuple xs -> prettyNewTuple s xs
        | TupleGet(value, index) -> prettyTupleGet s (value, index)
        | TupleSet(var, index, value) -> prettyTupleSet s (var, index, value)

        | Coerce(kind, value, newType) -> prettyCoerce s (kind, value, newType)
        | Atom e -> prettyAtom s e

    and prettyExp s maxPrec e =
        let struct(e, prec) = prettyExpRaw s e.value
        if maxPrec < prec
        then "(" .+ nest (ne ++ e) ++ ne +. ")"
        else e

    and prettyVarSet s (var, value) =
        let var = prettyVar s var
        let value = prettyExp s P.Assign value

        var +. " <-" ++ Document.layoutBlock value, P.Assign

    and prettyIf s (test, ifTrue, ifFalse) =
        let test = prettyExp s P.Max test
        let ifTrue = prettyExp s P.Max ifTrue
        let ifFalse = prettyExp s P.Max ifFalse

        "if" .+ Document.layoutBlock test ++ ns +.
        "then" ++ Document.layoutBlock ifTrue ++ ns +.
        "else" ++ Document.layoutBlock ifFalse,
        P.If

    and prettyLet s (var, value, scope) =
        let m = prettyMutability var
        let var = prettyVar s var
        let value = prettyExp s P.Max value
        let scope = prettyExp s P.Max scope

        "let " .+ m ++ var +. " =" ++ Document.layoutBlock value ++ breakable " in " ++
        scope,
        P.Let

    and prettySeq s (first, last) =
        let first = prettyExp s P.If first
        let last = prettyExp s P.Max last
        first ++ breakable "; " ++ last, P.Seq

    and prettyCall s (proc, args) =
        let name = prettyVar s proc
        let args = prettyListOrUnit (prettyExp s P.Primitive) args
        name ++ Document.layoutBlock args, P.Apply

    and prettyNewTuple s = function
        | [] -> text "()", P.Primitive
        // (x1,)
        | [x1] -> "(" .+ nest (ne ++ prettyExp s P.Max x1) ++ ne +. ",)", P.Primitive
        // x1, x2, ..., xN
        | xs -> Document.concatMap (prettyExp s (Precedence.sub P.Comma 1) >> group) ("," .+ ns) xs, P.Comma

    and prettyTupleGet s (value, index) =
        let value = prettyExp s P.Primitive value
        let index = prettyNumberLiteral (double index)

        value +. "." ++ index, P.Primitive

    and prettyTupleSet s (var, index, value) =
        let var = prettyVar s var
        let index = prettyNumberLiteral (double index)
        let value = prettyExp s P.Assign value

        var +. "." ++ index +. " <-" ++ Document.layoutBlock value, P.Assign

    and prettyCoerce s (kind, value, newType) =
        let value = prettyExp s (Precedence.sub P.TypeOp 1) value
        let op = match kind with CoerceKind.Convert -> ":>" | CoerceKind.Reinterpret -> ":?>"
        let newType = prettyType newType

        group (value ++ ns) +. op ++ ws ++ newType, P.TypeOp

    and prettyAtom s e =
        let e = prettyExp s P.Primitive e
        "atomic" .+ Document.layoutBlock e, P.If

    and prettyOperands s ops = prettyListOrUnit (prettyExp s P.Primitive) ops

    and prettyOp s (op, ops) =
        match op with
        | O.``+``
        | O.``-`` -> prettyBinaryOp s (P.Add, P.Add, P.Mul) (op, ops)
        | O.``*``
        | O.``/``
        | O.``%`` -> prettyBinaryOp s (P.Mul, P.Mul, Precedence.sub P.Mul 1) (op, ops)
        | O.``<``
        | O.``>`` -> prettyBinaryOp s (P.Rel, P.Rel, Precedence.sub P.Rel 1) (op, ops)
        | O.``=`` -> prettyBinaryOp s (P.Eq, P.Eq, Precedence.sub P.Eq 1) (op, ops)
        | _ -> prettyNormalOp s (op, ops)

    and prettyExtOp s (spec, ops) =
        let op = prettyName spec.extensionId
        op ++ nest (ns ++ prettyOperands s ops), P.Apply

    and prettyNormalOp s (op, ops) =
        let struct(op, _) = prettyOperator s op
        let ops = prettyOperands s ops
        op ++ nest (ns ++ ops), P.Apply

    and prettyBinaryOp s (leftPrec, opPrec, rightPrec) (op, ops) =
        match ops with
        | [l; r] ->
            let l = prettyExp s leftPrec l
            let struct(op, _) = prettyOperator s op
            let r = prettyExp s rightPrec r
            group l ++ group (nest (ns ++ op ++ ns ++ group r)), opPrec

        | _  -> prettyNormalOp s (op, ops)

    and prettyListOperand s = function
        | Choice1Of2 e -> prettyExp s P.Primitive e
        | Choice2Of2 { Source.value = listVar } -> prettyVar s listVar

    and prettyListOp s (op, ops) =
        let struct(op, _) = prettyOperator s op
        let ops = prettyListOrUnit (prettyListOperand s) ops
        op ++ nest (ns ++ ops), P.Apply

    let prettyParameter s { Source.value = var } = prettyVar s var

    let prettyProcedure s (ProcDef(ProcHeader(var, ps, atomicity), body)) =
        let attrs = match atomicity with NoAtomic -> "@async" .+ ns | Atomic -> empty
        let name = prettyVar s var

        let parameters =
            match ps with
            | [] -> text "()"
            | _ -> Document.concatMap (prettyParameter s) ns ps

        let body = prettyExp s P.Max body
        group (attrs +. "let " ++ name ++ nest (ns ++ group (parameters ++ ns) +. "=")) ++
        Document.layoutBlock body

    let prettyListener s (ListenerDef(name, ps, body)) =
        let struct(n, _) = prettyOperator s name
        let ps = Document.concatMap (group << prettyExp s P.Primitive) ns ps
        let body = prettyExp s P.Max body
        "do" .+ group (nest (ns ++ n ++ ns ++ ps) ++ ns) ++ body

    let prettyVariableInit s (VariableInitDef(var, init)) =
        let var = prettyVar s var
        let init = prettyExp s P.Apply init
        "do " .+ var +. " :=" ++ Document.layoutBlock init

    let prettyTop state = function
        | Top.Procedure p -> prettyProcedure state p.value
        | Top.Listener l -> prettyListener state l.value
        | Top.VariableInit v -> prettyVariableInit state v

    let prettyListDef state (list: ListDef<_>) =
        let { ListView.x = x; y = y; width = w; height = h; visible = v } = list.view

        let ps = empty
        let ps =
            if x <> 0. || y <> 0.
            then ps +. "@position(" +. string x +. ", " +. string y ++ ns
            else ps

        let ps =
            if w <> 0. || h <> 0.
            then ps +. "@size(" +. string w +. ", " +. string h +. ")" ++ ns
            else ps

        let ps =
            match v with
            | Visible -> ps +. "@visible" ++ ns
            | Hidden -> ps

        let ps =
            match list.isPersistent with
            | Persistent -> ps +. "@persistent" ++ ns
            | NoPersistent -> ps

        let name = prettyVar state list.var

        let values =
            IArray.toSeqCopiable list.init
            |> Document.concatMap prettyLit ns

        group (
            group (ps +. "let " ++ name ++ ns +. "=" ++ ns +. "[") ++
            nest (ne ++ group values) ++ ne +.
            "]"
        )

    let prettyVariableDecl s (v: VariableDecl<_>) =
        let p = match v.isPersistent with Persistent -> "@persistent" .+ ns | NoPersistent -> empty
        let m = prettyMutability v.var
        group (p +. "let " ++ m ++ prettyVar s v.var)

let private varNaming struct(var, i) = if i = 0 then Var.name var else Var.name var + "@" + string i
let private newState() = { ns = Namespace.newNamespace varNaming }

let plugin = {
    makeState = newState

    procedureNames = fun struct(s, scripts) ->
        scripts
        |> Seq.choose (function
            | Top.Procedure { value = ProcDef(ProcHeader(var = var), _) } ->
                Namespace.uniqueName s.ns (Var.simple var)
                |> UniqueName.name
                |> Some
            | _ -> None
        )

    listNames = fun struct(s, xs) -> Seq.map (fun l -> l.var |> Namespace.uniqueName s.ns |> UniqueName.name) xs
    variableNames = fun struct(s, xs) -> Seq.map (fun l -> l.var |> Var.simple |> Namespace.uniqueName s.ns |> UniqueName.name) xs

    prettyScript = fun struct(s, x) -> prettyTop s x
    prettyList = fun struct(s, l) -> prettyListDef s l
    prettyVariable = fun struct(s, v) -> prettyVariableDecl s v
}

let prettyExpWith withConfig e =
    prettyExp (Pretty.State.empty (newState())) Precedence.Max e
    |> Document.renderWith (withConfig Document.Config.defaultConfig)

let prettyExp (e: _ Exp) = prettyExpWith id e

let prettyTopWith withConfig t =
    prettyTop (Pretty.State.empty (newState())) t
    |> Document.renderWith (withConfig Document.Config.defaultConfig)

let prettyTop t = prettyTopWith id t

let prettyWith withConfig e = Pretty.prettyWith (Pretty.Config.withPlugin plugin >> withConfig) e
let pretty (e: _ IRStageData) = prettyWith id e
