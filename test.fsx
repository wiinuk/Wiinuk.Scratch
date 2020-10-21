#load "Shell.fsx"

start "npm install"
start "dotnet tool restore"
start "dotnet paket restore"
start "dotnet test"
