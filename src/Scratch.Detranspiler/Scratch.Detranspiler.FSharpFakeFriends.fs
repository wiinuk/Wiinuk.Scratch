module Scratch.Detranspiler.FSharpFakeFriends
open System
open System.Collections
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Compiler.AbstractIL.ILBinaryReader
open FSharp.Compiler.Range


[<AutoOpen>]
module private InteralMemberInfos =
    type B = System.Reflection.BindingFlags
    type T = FSharp.Reflection.FSharpType
    type V = FSharp.Reflection.FSharpValue

    let allowAccessToPrivateMember = B.Instance ||| B.Static ||| B.Public ||| B.NonPublic
    let (|Method|) name (t: Type) =
        match t.GetMethod(name, allowAccessToPrivateMember) with
        | null -> failwith $"method not found: {t.FullName} :: {name}"
        | x -> x

    let (|Property|) name (t: Type) =
        match t.GetProperty(name, allowAccessToPrivateMember) with
        | null -> failwith $"property not found: {t.FullName} :: {name}"
        | x -> x

    /// FSharp.Compiler.Service.dll
    /// FSharp.Compiler
    let compilerA, compilerNS =
        let rangeT = typeof<range>
        rangeT.Assembly, rangeT.DeclaringType.Namespace

    let compilerType name = compilerA.GetType(compilerNS + "." + name, throwOnError = true)

    /// type PickledCcuInfo = { mspec: ModuleOrNamespace; ... }
    let pickledCcuInfoT
        & Property "mspec" mspecP
        = compilerType "Tast+PickledCcuInfo"

    /// type Entity = { ... }
    let _
        & Property "LogicalName" logicalNameP
        & Property "CompiledName" compiledNameP
        & Property "ModuleOrNamespaceType" moduleOrNamespaceTypeP
        = compilerType "Tast+Entity"

    /// type PickledDataWithReferences<'T> = { RawData: 'T; FixupThunks: CcuThunk list }
    /// type PickledDataWithReferences<CcuThunk> = { PickledDataWithReferences<CcuThunk>.RawData: CcuThunk }
    let _ & Property "RawData" rawDataP = compilerType("TastPickle+PickledDataWithReferences`1").MakeGenericType pickledCcuInfoT

    /// type ModuleOrNamespaceType(...) =
    let _
        & Property "AllValsAndMembers" allValsAndMembersP
        & Property "AllEntities" allEntitiesP
        & Property "ModuleAndNamespaceDefinitions" moduleAndNamespaceDefinitionsP
        = compilerType "Tast+ModuleOrNamespaceType"

    /// type Val =
    ///     member LogicalName: string
    ///     member CompiledName: compilerGlobalState: CompilerGlobalState option -> string
    //      ...
    let _
        & Method "CompiledName" valToCompiledNameM
        & Property "LogicalName" valToLogicalNameP
        = compilerType "Tast+Val"
            
    /// module internal TastPickle =
    /// val internal unpickleObjWithDanglingCcus<'T> : file:string -> viewedScope:ILScopeRef -> ilModule:ILModuleDef option -> ('T unpickler) -> byte[] ->  PickledDataWithReferences<'T>
    /// val internal unpickleCcuInfo : ReaderState -> PickledCcuInfo
    /// ...
    let _
        & Method "unpickleObjWithDanglingCcus" unpickleObjWithDanglingCcusM
        & Method "unpickleCcuInfo" unpickleCcuInfoM
        = compilerType "TastPickle"

    /// val internal unpickleObjWithDanglingCcus<PickledCcuInfo> : file:string -> viewedScope:ILScopeRef -> ilModule:ILModuleDef option -> (PickledCcuInfo unpickler) -> byte[] -> PickledDataWithReferences<PickledCcuInfo>
    let unpickleObjWithDanglingCcusOfPickledCcuInfoM = unpickleObjWithDanglingCcusM.MakeGenericMethod pickledCcuInfoT

    /// type RawFSharpAssemblyDataBackedByFileOnDisk = new (...) = ...
    let rawFSharpAssemblyDataBackedByFileOnDiskC = compilerType("CompileOps+RawFSharpAssemblyDataBackedByFileOnDisk").GetConstructors()[0]

    /// type IRawFSharpAssemblyData =
    ///     abstract GetRawFSharpSignatureData: range * ilShortAssemName: string * fileName: string -> (string * (unit -> ReadOnlyByteMemory)) list
    ///     ...
    let _
        & Method "GetRawFSharpSignatureData" getRawFSharpSignatureDataM
        = compilerType "CompileOps+IRawFSharpAssemblyData"

    /// type ReadOnlyByteMemory = ...
    let readOnlyByteMemoryT = compilerType "AbstractIL.Internal.ReadOnlyByteMemory"

    /// `ReaderState -> PickledCcuInfo`
    let unpickleCcuInfo =
        let t1 = (unpickleCcuInfoM.GetParameters()[0]).ParameterType
        let t2 = unpickleCcuInfoM.ReturnType
        V.MakeFunction(T.MakeFunctionType(t1, t2), (fun st -> unpickleCcuInfoM.Invoke(null, [|st|])))

    /// (unit -> ReadOnlyByteMemory).Invoke
    let _
        & Method "Invoke" fSharpFuncOfUnitOfReadOnlyByteMemory_InvokeM
        = T.MakeFunctionType(typeof<unit>, readOnlyByteMemoryT)

[<Struct>]
type FakeModuleOrNamespaceType = FakeModuleOrNamespaceType of obj
[<Struct>]
type FakeEntity = FakeEntity of obj
[<Struct>]
type FakePickledCcuInfo = FakePickledCcuInfo of obj
[<Struct>]
type FakeVal = FakeVal of obj

module ModuleOrNamespaceType =
    let allValsAndMembers (FakeModuleOrNamespaceType x) =
        allValsAndMembersP.GetValue x
        :?> _
        |> Seq.cast<obj>
        |> Seq.map FakeVal

    let allEntities (FakeModuleOrNamespaceType x) =
        allEntitiesP.GetValue x
        :?> _
        |> Seq.cast<obj>
        |> Seq.map FakeEntity

    let moduleAndNamespaceDefinitions (FakeModuleOrNamespaceType x) =
        moduleAndNamespaceDefinitionsP.GetValue x
        :?> _
        |> Seq.cast<obj>
        |> Seq.map FakeEntity

module Entity =
    let moduleOrNamespaceType (FakeEntity x) = moduleOrNamespaceTypeP.GetValue x |> FakeModuleOrNamespaceType
    let logicalName (FakeEntity x) = logicalNameP.GetValue x :?> string
    let compiledName (FakeEntity x) = compiledNameP.GetValue x :?> string

    let moduleAndNamespaceDefinitions x = moduleOrNamespaceType x |> ModuleOrNamespaceType.moduleAndNamespaceDefinitions

    let lookupEntityByCompiledName name x = x |> moduleAndNamespaceDefinitions |> Seq.filter (compiledName >> (=) name) |> Seq.exactlyOne
    let lookupEntityByLogicalName name x = x |> moduleAndNamespaceDefinitions |> Seq.filter (logicalName >> (=) name) |> Seq.exactlyOne

module PickledCcuInfo =
    let mspec (FakePickledCcuInfo x) = mspecP.GetValue x |> FakeEntity

module Val =
    let compiledName (FakeVal x) = valToCompiledNameM.Invoke(x, [|None|]) :?> string
    let logicalName (FakeVal x) = valToLogicalNameP.GetValue x :?> string

let makeScopeRefForILModule = function
    | { Manifest = Some m } -> ILScopeRef.Assembly(mkRefToILAssembly m)
    | ilModule -> ILScopeRef.Module(mkRefToILModule ilModule)

let GetSignatureData file ilScopeRef ilModule byteReader =
    let bytes = fSharpFuncOfUnitOfReadOnlyByteMemory_InvokeM.Invoke(byteReader, [|()|])
    unpickleObjWithDanglingCcusOfPickledCcuInfoM.Invoke(null, [|
        (file: string)
        (ilScopeRef: ILScopeRef)
        (ilModule: ILModuleDef option)
        unpickleCcuInfo
        bytes
    |])

let GetRawFSharpSignatureData ilModule m ilShortAssemName fileName =
    let rawFSharpAssemblyDataBackedByFileOnDisk =
        rawFSharpAssemblyDataBackedByFileOnDiskC.Invoke [|
            (ilModule: ILModuleDef)
            ([]: ILAssemblyRef list)
        |]

    let data =
        getRawFSharpSignatureDataM.Invoke(rawFSharpAssemblyDataBackedByFileOnDisk, [|
            (m: range)
            (ilShortAssemName: string)
            (fileName: string)
        |])
        :?> IEnumerable

    [for tuple in data -> V.GetTupleField(tuple, 0) :?> string, V.GetTupleField(tuple, 1)]

let importReferencedFSharpAssembly fileName ilModule =
    let ilScopeRef = makeScopeRefForILModule ilModule
    let sigDataReaders = GetRawFSharpSignatureData ilModule range.Zero "" fileName
    sigDataReaders
    |> List.map (fun (_ccuName, sigDataReader) ->
        GetSignatureData fileName ilScopeRef (Some ilModule) sigDataReader
        |> rawDataP.GetValue
        |> FakePickledCcuInfo
    )

let usingFSharpAssembly fileName action =
    let opts = {
        metadataOnly = MetadataOnlyFlag.Yes
        reduceMemoryUsage = ReduceMemoryFlag.Yes
        pdbDirPath = None
        tryGetMetadataSnapshot = fun _ -> None
    }
    use moduleReader = AssemblyReader.GetILModuleReader(fileName, opts)
    importReferencedFSharpAssembly fileName moduleReader.ILModuleDef |> action
