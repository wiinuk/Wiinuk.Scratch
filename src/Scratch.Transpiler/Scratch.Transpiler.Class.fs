module Scratch.Transpiler.Class
#nowarn "1204"
open FSharp.Quotations
open Scratch.Reflection
open Scratch.Reflection.Transformers
open System
open System.Reflection
module E = FSharp.Quotations.Patterns
module E = FSharp.Quotations.DerivedPatterns


type ReflectedClassTops = {
    beforeInit: Expr list
    afterInit: Expr list
}
type ReflectedClass = {
    constructorParameters: Var list
    baseConstructorSelf: Var option
    baseConstructorArguments: Expr list
    this: Var
    selfField: FieldInfo option
    initField: FieldInfo option
    tops: ReflectedClassTops
}

type SelfRef =
    | VarSelf of Var
    | VarOrFieldSelf of selfInBaseNewArgs: Var * selfField: FieldInfo

type SelfState =
    | NoSelf of this: Var option
    | HasSelf of selfRef: SelfRef * this: Var * thisExpr: Expr

type InitsState =
    | NoInit of noinitAcc: Expr list
    | Initialized of noinitAcc: Expr list * initField: FieldInfo * initializedAcc: Expr list

let addInit x = function
    | NoInit xs -> NoInit(x::xs)
    | Initialized(xs1, f, xs2) -> Initialized(xs1, f, x::xs2)

let fieldEq (l: FieldInfo) (r: FieldInfo) = l.DeclaringType = r.DeclaringType && l.Name = r.Name

let (|CheckThisCall|_|) = E.(|SpecificCall|_|) <@ LanguagePrimitives.IntrinsicFunctions.CheckThis @>

/// (`<@ CheckThis(%self.contents) @>` | `<@ CheckThis(%this.self.contents) @>`) => `<@ %this @>`
let transformCheckThis self thisE e =
    match self, e with
    | Choice1Of2 self, CheckThisCall(_, _, E.PropertyGet(Some(E.Var self'), _, _)::_) when self' = self -> Some thisE
    | Choice2Of2 selfField, CheckThisCall(_, _, E.PropertyGet(Some(E.FieldGet(Some thisE, selfField')), _, _)::_) when fieldEq selfField selfField' -> Some thisE
    | _ -> None

/// (`<@ CheckThis(%self.contents) @>` | `<@ CheckThis(%this.self.contents) @>`) => `<@ %this @>`
let replaceCheckThis self thisE e =
    match self with
    | VarSelf self -> transformCheckThis (Choice1Of2 self) thisE e
    | VarOrFieldSelf(selfField = selfField) -> transformCheckThis (Choice2Of2 selfField) thisE e

/// `<@ ...; () @>` | `<@ ...; this.init <- 1; ...; () @>`
let memberInit allExpr thisT allInits args self baseNewArgs body =
    let rec aux self initsAcc = function
        | E.Unit() ->
            match initsAcc, self with
            | NoInit _, HasSelf _ -> Error("requires end initialize. e.g.: <@ this.init <- 1; ... @>", allInits)
            | NoInit xs1, NoSelf _ -> Ok(self, xs1, None, [])
            | Initialized(xs1, f, xs2), (HasSelf _ | NoSelf _) -> Ok(self, xs1, Some f, xs2)

        | E.Sequential(init, inits) as e ->
            match self, init with
            | HasSelf(this = this), E.FieldSet(Some(E.Var this'), initF, E.Int32 1) when this = this' && initF.Name.StartsWith "init@" ->
                match initsAcc with
                | Initialized _ -> Error("double init", e)
                | NoInit xs1 -> aux self (Initialized(xs1, initF, [])) inits

            | _ ->
                let self =
                    match self with
                    | NoSelf None -> init.GetFreeVars() |> Seq.tryFind (fun v -> v.Type = thisT) |> NoSelf
                    | NoSelf(Some _)
                    | HasSelf _ -> self

                let init =
                    match self with
                    | HasSelf(selfRef = self; thisExpr = thisE) -> transformDeepOnce (replaceCheckThis self thisE) init |> Option.defaultValue init
                    | _ -> init

                aux self (addInit init initsAcc) inits

        // TODO: e.g. <@ type C() = let x = 10 in () @>
        // | E.Let(v, value, inits) -> ...

        | e -> Error("invalid member init. e.g.: <@ let x = ... in do ... in () @>", e)

    aux self (NoInit []) body
    |> Result.map (fun (self, xs1, initField, xs2) ->
        let this =
            match self with
            | NoSelf(Some this) -> this
            | NoSelf None -> let v = Var("this", thisT) in v
            | HasSelf(this = this) -> this

        {
            constructorParameters = args
            baseConstructorSelf =
                match self with
                | HasSelf(selfRef = VarOrFieldSelf(selfInBaseNewArgs = self)) -> Some self
                | _ -> None

            baseConstructorArguments = baseNewArgs
            this = this
            selfField =
                match self with
                | HasSelf(selfRef = VarOrFieldSelf(selfField = selfField)) -> Some selfField
                | _ -> None

            initField = initField

            tops = { beforeInit = List.rev xs1; afterInit = List.rev xs2 }
        }, allExpr
    )

/// `<@ self.contents <- %this; ... @>` | `<@ this.self.contents <- %this; ... @>`
let endBaseInitAndMemberInit allExpr thisT args selfRef (baseType, baseArgs) body =
    match selfRef, body with
    | Choice1Of2 selfRef, E.Sequential(E.PropertySet(Some(E.Var selfRef'), _, _, (E.Var this as thisE)), body)
        when selfRef = selfRef' && not (List.contains this args) && this <> selfRef && this.Type.BaseType = baseType ->
        memberInit allExpr thisT body args (HasSelf(VarSelf selfRef, this, thisE)) baseArgs body

    | Choice2Of2(this, thisE, selfField, selfInBaseNew), E.Sequential(E.PropertySet(Some(E.FieldGet(Some(E.Var this'), selfField')), _, _, E.Var this''), body)
        when not (List.contains this args) && this = this' && this = this'' && fieldEq selfField selfField' && this.Type.BaseType = baseType ->
        memberInit allExpr thisT body args (HasSelf(VarOrFieldSelf(selfInBaseNew, selfField), this, thisE)) baseArgs body

    | _, e -> Error("requires self initialize. e.g.: <@ self.contents <- this; ... @>", e)

let (|BaseNew|_|) thisT = function
    | E.NewObject(baseC, baseArgs) when (thisT: Type).BaseType = baseC.DeclaringType -> Some(baseC, baseArgs)
    | _ -> None

/// `<@ (let self = this.self in new Base(...)); ... @>` | `<@ new Base(...); ... @>`
let baseInitAndBody allExpr thisT args self body =
    match self, body with
    | Some(Choice2Of2(this, thisE, selfField)), E.Sequential(E.Let(selfInBaseNew, E.FieldGet(Some(E.Var this'), selfField'), BaseNew thisT (baseC, baseArgs)), body) when this = this' && fieldEq selfField selfField' ->
        endBaseInitAndMemberInit allExpr thisT args (Choice2Of2(this, thisE, selfField, selfInBaseNew)) (baseC.DeclaringType, baseArgs) body

    | Some(Choice1Of2 selfRef), E.Sequential(BaseNew thisT (baseC, baseArgs), body) ->
        endBaseInitAndMemberInit allExpr thisT args (Choice1Of2 selfRef) (baseC.DeclaringType, baseArgs) body

    | None, E.Sequential(BaseNew thisT (_, baseArgs), body) ->
        memberInit allExpr thisT body args (NoSelf None) baseArgs body

    | _, e -> Error("requires base new. e.g.: <@ new obj(); ... @>", e)

let (|NewThisRef|_|) thisT = function
    | E.NewRecord(refT, [E.Value(null, _)]) when refT = typedefof<_ ref>.MakeGenericType [|thisT|] -> Some()
    | _ -> None

/// `<@ let self = { contents = null } in ... @>` | `<@ this.self <- { contents = null }; ... @>` | `<@ ... @>`
let classBody allExpr thisT args = function
    | E.Let(selfRef, NewThisRef thisT (), body) -> baseInitAndBody allExpr thisT args (Some(Choice1Of2 selfRef)) body
    | E.Sequential(E.FieldSet(Some(E.Var this as thisE), selfField, NewThisRef thisT ()), body) -> baseInitAndBody allExpr thisT args (Some(Choice2Of2(this, thisE, selfField))) body
    | body -> baseInitAndBody allExpr thisT args None body

let (|ClassWithReflectedDefinition|) = function
    | E.MethodWithReflectedDefinition(Lambdas(args, body) as e) as c -> classBody e (c: ConstructorInfo).DeclaringType (List.concat args) body
    | _ -> Error("requires class with reflected definition", <@@ () @@>)
