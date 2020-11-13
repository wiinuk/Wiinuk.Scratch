#load "Shell.fsx"
open System.IO

start "dotnet tool restore"
start "dotnet paket restore"
try
    start "dotnet test ./tests/Scratch.Transpiler.Tests/ --blame"
finally
    let parent = "./tests/Scratch.Transpiler.Tests/TestResults/"
    if Directory.Exists parent then
        for xmlPath in Directory.EnumerateFiles(parent, "*.xml", SearchOption.AllDirectories) do
            printfn ""
            printfn $"{xmlPath}"
            printfn "======================="
            for line in File.ReadLines xmlPath do
                printfn $"{line}"
            printfn "======================="
            printfn ""
