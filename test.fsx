#load "Shell.fsx"
open System.IO

start "dotnet tool restore"
start "dotnet paket restore"
start "dotnet test ./tests/Scratch.Transpiler.Tests/ --blame --filter instructTest"
