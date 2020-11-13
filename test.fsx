#load "Shell.fsx"

start "dotnet tool restore"
start "dotnet paket restore"
start "dotnet test ./tests/Scratch.Tests/ --blame --filter algebraicSimplification"
