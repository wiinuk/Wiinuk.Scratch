module internal Scratch.Evaluator.Validators
open Scratch.Ast
open Scratch
open Scratch.Primitives
open System
open System.Runtime.InteropServices


type DiagnosticsKind =
    | FootterStatementInPositionOtherThanLast
    | ParameterCountMismatch
    | CloudVariableInSprite
    | CloudVariableTooMany

[<Struct; StructLayout(LayoutKind.Auto)>]
type Diagnostics<'a> = {
    state: 'a
    kind: DiagnosticsKind
}

[<Struct; StructLayout(LayoutKind.Auto); NoComparison>]
type ValidateState<'a,'W> = {
    isLast: bool
    diagnostics: PooledBuffer<'a Diagnostics,'W>
}

let rec expression (s: _ inref) = function
    | Expression.Literal _ -> ()
    | Expression.Block x -> blockExpression &s x
    | Expression.Complex x -> complexExpression &s x

and complexExpression (s: _ inref) (ComplexExpression(state, operator, operands)) =
    match Map.tryFind operator AstDefinitions.knownAllOperatorMap with
    | ValueNone -> ()
    | ValueSome info ->
        if not s.isLast && info.isFooter then
            PooledBuffer.add s.diagnostics { state = state; kind = FootterStatementInPositionOtherThanLast }

    for x in operands do
        expression &s x

and private _complexExpressionsAux (s: _ inref) xs =
    match xs with
    | [] -> ()
    | [x] ->
        let s = { s with isLast = true }
        complexExpression &s x

    | x::xs ->
        complexExpression &s x
        _complexExpressionsAux &s xs

and complexExpressions (s: _ inref) xs =
    let s = { s with isLast = false }
    _complexExpressionsAux &s xs

and blockExpression (s: _ inref) (BlockExpression(body = body)) =
    complexExpressions &s body

let procedure (s: _ inref) (ProcedureDefinition(state = state; name = name; parameters = ps; body = x)) =
    let _, types = AstDefinitions.demangleProcedureName name
    if List.length types <> List.length ps then
        PooledBuffer.add s.diagnostics { state = state; kind = ParameterCountMismatch }

    blockExpression &s x

let scriptData diagnostics script =
    let s = { isLast = true; diagnostics = diagnostics }

    match script.script with
    | Procedure x -> procedure &s x
    | Listener(ListenerDefinition(body = x))
    | Statements x -> blockExpression &s x
    | Expression x -> complexExpression &s x

let private incrementWithState x state =
    match x with
    | ValueNone -> ValueSome struct(1, state)
    | ValueSome struct(n, s) -> ValueSome(n + 1, s)

let private mergeWithState x1 x2 =
    match x1, x2 with
    | ValueSome struct(n1, s), ValueSome struct(n2, _) -> ValueSome struct(n1 + n2, s)
    | ValueNone, x | x, ValueNone -> x

let entityData diagnostics isStage data =
    let mutable cloudVariableCount = ValueNone

    for s in data.scripts do scriptData diagnostics s
    for v in data.variables do
        match v.isPersistent with
        | NoPersistent -> ()
        | Persistent when not isStage -> PooledBuffer.add diagnostics { state = v.state; kind = CloudVariableInSprite }
        | Persistent -> cloudVariableCount <- incrementWithState cloudVariableCount v.state

    cloudVariableCount

let stageData diagnostics data =
    let mutable cloudVariableCount = entityData diagnostics true data
    for x in StageData.sprites data do
        cloudVariableCount <- mergeWithState cloudVariableCount (entityData diagnostics false x)

    match cloudVariableCount with
    | ValueSome(n, s) when 10 < n -> PooledBuffer.add diagnostics { state = s; kind = CloudVariableTooMany }
    | _ -> ()

type ValidationException<'a>(diagnostics: Diagnostics<'a> list) =
    inherit Exception()
    member _.Diagnostics = diagnostics
    override _.Message = $"ValidationException(%A{diagnostics})"
    override e.ToString() = e.Message

[<Struct>]
type private HandleDiagnosticsToException<'a> = { data: 'a StageData } with
    interface IPooledBufferUser<Diagnostics<'a>, HUnit> with
        member f.Using diagnostics =
            stageData diagnostics f.data
            if PooledBuffer.isEmpty diagnostics then HUnit else

            let diagnostics = PooledBuffer.toList diagnostics
            raise <| ValidationException diagnostics

let inline validateStageData data =
    let mutable action = { data = data }
    PooledBuffer.using &action |> ignore
