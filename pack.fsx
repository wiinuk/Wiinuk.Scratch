#load "Shell.fsx"

start "dotnet tool restore"
start "dotnet build --configuration Release src/Scratch"
start "dotnet paket pack --template src/Scratch/paket.template src/Scratch/bin/Release"
