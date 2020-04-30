[<AutoOpen>] 
module internal Scratch.Ast.Transformers.Helpers
open Scratch
open Scratch.Ast
open Scratch.Primitives
open Scratch.Transformers


[<Struct>]
type VariableStatistics = {
    /// readVariable, changeVar:by:, ...
    readCount: int
    /// setVar:to:, chargeVar:by:, ...
    writeCount: int
    /// showVariable:, hideVariable:, ...
    metaCount: int
}

[<Struct>]
type ProcedureStatistics = {
    callCount: int
}

[<Struct>]
type ExpressionStatistics = {
    variableStatistics: Map<string, VariableStatistics>
    procedureStatistics: Map<string, ProcedureStatistics>
    scriptSize: int
}

[<AutoOpen>]
module ExpressionStatisticsIsTag =
    type private ExpressionStatisticsTagId = struct end
    [<Struct>]
    type ExpressionStatisticsTagShape =
        interface IIsTagIdShape with
            member _.Id = Tag.typeid the<ExpressionStatisticsTagId>

    let ExpressionStatisticsTag = IsTag.makeIsTag the<ExpressionStatisticsTagShape> (IsTagValue.reference the<Lazy<ExpressionStatistics>>)

let emptyStatistics = { readCount = 0; writeCount = 0; metaCount = 0 }

let analyzeScript script =
    let merge k s' ({ variableStatistics = map } as s) =
        let v =
            match Map.tryFind k map with
            | ValueNone -> s'
            | ValueSome v -> {
                readCount = v.readCount + s'.readCount
                writeCount = v.writeCount + s'.writeCount
                metaCount = v.metaCount + s'.metaCount
            }
        { s with variableStatistics = Map.add k v map }

    let mergeProcedureStat k s' ({ procedureStatistics = map } as s) =
        let v =
            match Map.tryFind k map with
            | ValueNone -> s'
            | ValueSome v -> { callCount = v.callCount + s'.callCount }
        { s with procedureStatistics = Map.add k v map }

    let local s (ComplexExpression(operator = operator; operands = operands)) =
        let s = { s with scriptSize = s.scriptSize + 1 }
        match operator, operands with
        | O.readVariable, [Literal(_, SString n)] -> merge n { emptyStatistics with readCount = 1 } s
        | O.``setVar:to:``, [Literal(_, SString n); _] -> merge n { emptyStatistics with writeCount = 1 } s
        | O.``changeVar:by:``, [Literal(_, SString n); _] -> merge n { emptyStatistics with readCount = 1 } s
        | O.``showVariable:``, [Literal(_, SString n)]
        | O.``hideVariable:``, [Literal(_, SString n)] -> merge n { emptyStatistics with metaCount = 1 } s
        | O.call, Literal(_, SString n)::_ -> mergeProcedureStat n { callCount = 1 } s
        | _ -> s

    let s = {
        variableStatistics = Map.empty
        procedureStatistics = Map.empty
        scriptSize = 0
    }
    match script with
    | Expression x -> ComplexExpression.fold local s x
    | Statements x -> BlockExpression.fold local s x
    | Listener(ListenerDefinition(arguments = args; body = body)) ->
        let map = List.fold (Expression.fold local) s args
        BlockExpression.fold local map body

    | Procedure(ProcedureDefinition(body = body)) ->
        BlockExpression.fold local s body

let updateScriptStatistics s =
    let tag = Script.state s
    let tag = Tagged.add ExpressionStatisticsTag (lazy analyzeScript s) tag
    Script.withState tag s

let addStageStatistics stage =
    let addEntityStatistics entity =
        let scripts =
            entity.scripts
            |> List.map (fun s -> { s with script = updateScriptStatistics s.script })

        { entity with scripts = scripts }

    stage
    |> addEntityStatistics
    |> EntityData.mapExtension (fun ex ->
        let children =
            ex.children
            |> List.map (function
                | Choice2Of3 sprite -> Choice2Of3 <| addEntityStatistics sprite
                | x -> x
            )
        { ex with StageDataExtension.children = children }
    )
