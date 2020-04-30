module Scratch.Pretty
open Scratch.Ast
open Scratch.PrettyInternal
open Scratch.Primitives


[<Struct; NoEquality; NoComparison>]
type Config<'State,'Script,'Variable,'List> = {
    document: Document.Config
    plugin: PrettyPlugin<'State,'Script,'Variable,'List>
}
module Plugin =
    let defaultValue = {
        prettyScript = fun struct(state, s) -> prettyScriptAst state s
        prettyVariable = fun struct(_, v) -> prettyVariableData v
        prettyList = fun struct(_, v) -> prettyListVariableData v
        procedureNames = fun struct(_, xs) -> xs |> Seq.choose (function Procedure(ProcedureDefinition(name = name)) -> Some name | _ -> None)
        listNames = fun struct(_, xs) -> xs |> Seq.map (fun l -> l.listName)
        variableNames = fun struct(_, xs) -> xs |> Seq.map (fun v -> v.name)
        makeState = fun () -> HUnit
    }

module Config =
    let defaultConfig = {
        document = Document.Config.defaultConfig
        plugin = Plugin.defaultValue
    }
    let withPlugin plugin config = {
        document = config.document
        plugin = plugin
    }

module State =
    let pluginState state = state.pluginState
    let empty pluginState = {
        scope = None
        parameters = Set.empty
        procedures = Set.empty
        listOrVariables = Set.empty
        pluginState = pluginState
    }

let prettyExpressionWith withConfig x =
    prettyExpression (State.empty HUnit) Precedence.Max x
    |> Document.renderWith (withConfig Document.Config.defaultConfig)

let prettyExpression x = prettyExpressionWith id x

let prettyWith withConfig x =
    let config = withConfig Config.defaultConfig
    prettyStageData config.plugin x
    |> Document.renderWith config.document

let pretty x = prettyWith id x
