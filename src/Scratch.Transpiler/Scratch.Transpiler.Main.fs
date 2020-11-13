[<AutoOpen>]
module Scratch.Transpiler.Main
open System
open Scratch
open Scratch.Ast
open Scratch.IR
open Scratch.MemoryModel
open Scratch.Primitives
open Scratch.Reflection
open Scratch.Transformers
open Scratch.Transpiler


let defaultConfig =
    let token name =
        match Expr.parseOperatorName name with
        | ValueSome "" -> "op_"
        | ValueSome n -> n
        | ValueNone -> name

    let removeArity arity (name: string) =
        match name.LastIndexOf '`' with
        | -1 -> name
        | i ->
            let mutable r = 0
            if Int32.TryParse(name.[(i+1)..(name.Length-1)], &r) && r = arity then name.[0..(i-1)]
            else name

    let typeName = Fun.memoizeFix <| fun typeName -> function
        | t when t = typeof<unit> -> "()"
        | TupleType ts ->
            ts
            |> Seq.map typeName
            |> String.concat ","
            |> sprintf "(%s)"

        | GenericType(d, ts) ->
            ts
            |> Seq.map typeName
            |> String.concat ","
            |> sprintf "%s(%s)" (removeArity (List.length ts) (token d.Name))

        | t -> token t.Name
    {
        indexedName = sprintf "%s#%d"
        resultName = "result"
        localName = "_"
        lambdaName = "λ"
        stackName = "_"
        namespaceSeparator = "."
        tupleFieldName = sprintf "%d"

        methodName = fun m ->
            if not m.IsGenericMethod then token m.Name else

            let genericArguments =
                m.GetGenericArguments()
                |> Array.map (typeName >> sprintf "@%s")
                |> String.concat ""

            token m.Name + genericArguments

        propertyName = fun m -> token m.Name
        fieldName = fun m -> token m.Name
        spriteName = fun t -> token t.Name

        typeName = typeName
        unionCaseName = fun u -> $"%s{typeName u.DeclaringType}.%s{token u.Name}"
        unionCaseFieldName = fun (UnionCaseFieldInfo(fieldIndex = i)) -> $"{i}"
        unionCaseTagName = "tag"
        unionCaseUnifiedFieldName = fun (n1, n2, ns) ->
            match List.distinct (n1::n2::ns) with
            | [] -> ""
            | [n] -> n
            | ns -> ns |> String.concat "|" |> sprintf "(%s)"

        plugin = PredefinedPlugins.predefinedPlugin

        outputLevel = OutputLevel.Error
        maxGenericInstantiateCount = 256

        unionCaseUninitField = SString "UNINIT"

        ir = IRToStageData.Config.defaultConfig
    }

let transpileStageWith modifyConfig e = transpileEntity StageData.defaultValue (modifyConfig defaultConfig) e
let transpileStage e = transpileStageWith id e
let defaultLoc: Loc = Tagged.empty None
let showLoc x = Tagged.value x |> Expr.showLocation
