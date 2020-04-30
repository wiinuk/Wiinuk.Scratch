[<AutoOpen>]
module Scratch.Detranspiler.Main
open Scratch.Primitives
open Core


let makeDefaultConfig showLocation = {
    renderConfig = Document.Config.defaultConfig
    showLocation = showLocation
    plugin = PredefinedPlugins.plugin
    ignoreInitialListValues = false
    treatStringAsNumberIfPossible = false
    treatStringAsBoolIfPossible = false
}

let prettyWith showLocation modifyConfig data =
    let config = modifyConfig <| makeDefaultConfig showLocation
    let warnings = ResizeArray()
    let env = {
        config = config
        warnings = warnings
        resolveEnv = {
            typeNameMemo = makeMemo()
            assemblyEntityMemo = makeMemo()
        }
    }
    match Context.run (prettyStage data) env () with
    | Error(DetranspileError(l, e, st)) -> raise <| DetranspileException(config.showLocation l, e, st)
    | Ok struct(x, ()) -> Document.renderWith config.renderConfig x, Seq.toList warnings

let pretty showLocation data = prettyWith showLocation id data
