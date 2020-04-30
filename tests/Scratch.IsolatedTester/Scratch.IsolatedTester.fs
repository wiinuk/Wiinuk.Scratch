module Scratch.IsolatedTester
open System.IO
open System.Text
open System.Reflection
open Argu
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open Scratch.Primitives
open Scratch.Ast
open Scratch.Transpiler
module E = FSharp.Quotations.DerivedPatterns
module E = FSharp.Quotations.Patterns


type IArrayConverter<'T>() =
    inherit JsonConverter<'T iarray>()
    static let instance = IArrayConverter<'T>()
    static member Instance = instance

    override _.WriteJson(w: JsonWriter, v: 'T iarray, s: JsonSerializer) =
        s.Serialize(w, IArray.Unchecked.unwrapArray v)

    override _.ReadJson(r, _, _, _, s) =
        IArray.Unchecked.wrapArray(s.Deserialize<'T[]> r)

type GenericIArrayConverterContractResolver() =
    inherit DefaultContractResolver()

    override _.CreateContract t =
        let contract = base.CreateContract t
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ iarray> then
            let e = t.GetGenericArguments().[0]
            let iarrayConverter =
                typedefof<IArrayConverter<_>>.MakeGenericType(e).GetProperty("Instance").GetValue(null)
                :?> JsonConverter
            contract.Converter <- iarrayConverter
        contract

let serializer =
    JsonSerializerSettings(
        ContractResolver = GenericIArrayConverterContractResolver()
    )
    |> JsonSerializer.CreateDefault

[<RequiresExplicitTypeArguments>]
let ofJsonFile<'T> path =
    use reader = new StreamReader(path = path, encoding = Encoding.UTF8)
    use reader = new JsonTextReader(reader)
    try
        serializer.Deserialize<'T> reader
    with :? JsonReaderException as e ->
        let source = File.ReadAllText path
        raise <| exn(sprintf "%s\r\nsource:\r\n%s" e.Message source, e)

let toJsonFile path value =
    use writer = new StreamWriter(path, append = false, encoding = Encoding.UTF8)
    serializer.Serialize(writer, box value)

type LoadAndTranspileResult =
    | TranspileOk of Loc StageData
    | TranspileError of message: string
    | RequireReflectedDefintitionError

type LoadAndTranspileConfig = {
    dllPath: string
    stageModuleName: string
    entryPointName: string option
    outPath: string option
}

let loadAndTranspile config =
    let { dllPath = dllPath; stageModuleName = stageModuleName } = config
    let entryPointName = defaultArg config.entryPointName "entryPoint"
    let outPath = defaultArg config.outPath (dllPath + ".json")

    let asm = Assembly.LoadFrom dllPath
    let entryPoint = asm.GetType(stageModuleName).GetMethod(entryPointName)
    let result =
        match entryPoint with
        | E.MethodWithReflectedDefinition(E.Lambda(_, body)) ->
            try TranspileOk(transpileStage body)
            with TranspileException e -> TranspileError <| buildTranspileExceptionMessage e

        | _ -> RequireReflectedDefintitionError

    toJsonFile outPath result

type TesterCLIArgs =
    | [<Unique; Mandatory>] Input of pathToJson: string
with
    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Input _ -> "*.json file path."

type Command =
    | LoadAndTranspile of LoadAndTranspileConfig

let processFromJsonFile path =
    match ofJsonFile<_> path with
    | LoadAndTranspile config -> loadAndTranspile config; 0
