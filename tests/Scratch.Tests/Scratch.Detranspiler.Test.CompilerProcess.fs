module Scratch.Detranspiler.Test.CompilerProcess
open Shell
open System
open System.IO
open System.Xml.Linq


let usingDirectory path scope = async {
    try return! scope <| Directory.CreateDirectory(path).FullName
    finally Directory.Delete(path, recursive = true)
}
let xelement name = XElement(XName.op_Implicit name)
let (+@) (x: XElement) (name, value) = x.Add(XAttribute(XName.op_Implicit name, value + "")); x
let (+<) (x: XElement) (node: #XNode) = x.Add node; x
let (+~) x value = x +< XText(value = value)

type XDocument with
    member x.AsyncSave path = async {
        use w = File.OpenWrite path
        let! c = Async.CancellationToken
        do! x.SaveAsync(w, SaveOptions.None, c) |> Async.AwaitTask
    }

let buildToBinary outDirectory source references assemblyName = async {
    let sourcePath = outDirectory/"source.fs"
    let projectPath = outDirectory/(assemblyName + ".fsproj")
    let framework =
#if NET6_0
        "net6.0"
#endif
    let outPath = outDirectory/"bin"/"Debug"/framework/(assemblyName + ".dll")

    let x = xelement
    let references =
        references
        |> Seq.map (fun path ->
            x"Reference"
            +@ ("Include", Path.GetFileNameWithoutExtension(path + ""))
            +< (x"HintPath" +~ path)
        )

    let project =
        x"Project" +@ ("Sdk", "Microsoft.NET.Sdk")
        +< (x"PropertyGroup"
            +< (x"TargetFramework" +~ framework)
        )
        +< (x"ItemGroup"
            +< (x"Compile" +@ ("Include", sourcePath))
        )
        +< Seq.fold (+<) (x"ItemGroup") references

    do! File.WriteAllTextAsync(sourcePath, source) |> Async.AwaitTask
    do! XDocument(project).AsyncSave projectPath

    do! startWithAsync (fun c -> { c with onOut = ignore }) "dotnet build --nologo \"%s\"" projectPath
    return outPath
}
let compileToAssembly references moduleName source assemblyFileScope =
    usingDirectory (Path.GetTempPath()/("test-" + Guid.NewGuid().ToString())) <| fun rootPath -> async {
        let! outFile = buildToBinary rootPath source references moduleName
        return! assemblyFileScope outFile
    }
