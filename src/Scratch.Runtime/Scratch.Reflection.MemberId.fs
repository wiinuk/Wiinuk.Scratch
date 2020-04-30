namespace Scratch.Reflection
open FSharp.Reflection
open Scratch.Primitives
open System
open System.Reflection


[<Struct>]
type AssemblyId = {
    assemblyName: string
}
[<Struct>]
type ModuleId = {
    moduleName: string
    assemblyId: AssemblyId
}
[<RequireQualifiedAccess>]
type TypeDefinitionId =
    | FromModule of typeToken: int * moduleId: ModuleId
    | FromDynamicAssembly of assemblyName: string * moduleName: string * typeName: string

[<Struct>]
type TypeWithoutGenericParameterId = {
    definitionId: TypeDefinitionId
    genericTypeArguments: TypeId list
}
and [<RequireQualifiedAccess>] TypeId =
    | Normal of TypeWithoutGenericParameterId
    | GenericParameter of declaringMember: Choice<TypeId, MethodId> * index: int

and MethodId = {
    methodToken: int
    declaringType: TypeWithoutGenericParameterId
    genericMethodArguments: TypeId list
}
[<Struct>]
type PropertyId = {
    declaringType: TypeId
    propertyName: string
}
[<Struct>]
type FieldId = {
    fieldToken: int
    moduleId: ModuleId
}
[<Struct>]
type UnionCaseId = {
    tag: int
    typeId: TypeId
}
module MemberId =
    let assemblyId (a: Assembly) = { assemblyName = a.FullName }
    let moduleId (m: Module) = {
        assemblyId = assemblyId m.Assembly
        moduleName = m.Name
    }
    let private typeDefinitionId (t: Type) =
        assert(not t.IsGenericType || t.IsGenericTypeDefinition)
        if t.Assembly.IsDynamic
        then TypeDefinitionId.FromDynamicAssembly(t.Assembly.FullName, t.Module.Name, t.FullName)
        else TypeDefinitionId.FromModule(t.MetadataToken, moduleId t.Module)

    let rec private typeIdWithoutGenericParameter (t: Type) =
        if t.IsGenericTypeDefinition then
            {
                definitionId = typeDefinitionId t
                genericTypeArguments = []
            }
        else
            let d = if t.IsGenericType then t.GetGenericTypeDefinition() else t
            let ts = if t.IsGenericType then t.GetGenericArguments() |> Seq.map typeId |> Seq.toList else []
            {
                definitionId = typeDefinitionId d
                genericTypeArguments = ts
            }

    and private typeIdRaw (t: Type) =
        if t.IsGenericParameter then
            let d =
                match t.DeclaringMethod with
                | null -> Choice1Of2(typeId t.DeclaringType)
                | m -> Choice2Of2(methodId m)

            TypeId.GenericParameter(d, t.GenericParameterPosition)

        else
            TypeId.Normal(typeIdWithoutGenericParameter t)

    and typeId = Fun.memoize typeIdRaw
    and methodId (m: MethodBase) = {
        methodToken = m.MetadataToken
        declaringType = typeIdWithoutGenericParameter m.DeclaringType
        genericMethodArguments =
            if m.IsGenericMethod then
                if m.IsGenericMethodDefinition then []
                else m.GetGenericArguments() |> Seq.map typeId |> Seq.toList
            else []
    }
    let propertyId (p: PropertyInfo) = {
        declaringType = typeId p.DeclaringType
        propertyName = p.Name
    }
    let fieldId (f: FieldInfo) = {
        fieldToken = f.MetadataToken
        moduleId = moduleId f.Module
    }
    let unionCaseId (u: UnionCaseInfo) = {
        tag = u.Tag
        typeId = typeId u.DeclaringType
    }

    let resolveAssembly a =
        System.AppDomain.CurrentDomain.GetAssemblies()
        |> Array.find (fun a' -> a.assemblyName = a'.FullName)

    let resolveModule m =
        resolveAssembly(m.assemblyId).Modules
        |> Seq.find (fun m' -> m.moduleName = m'.Name)

    let private resolveTypeDefinition = function
        | TypeDefinitionId.FromModule(t, m) -> resolveModule(m).ResolveType(t)
        | TypeDefinitionId.FromDynamicAssembly(assemblyName, moduleName, fullName) ->
            match Type.GetType fullName with
            | null ->
                AppDomain.CurrentDomain.GetAssemblies()
                |> Seq.filter (fun a -> a.IsDynamic && a.FullName = assemblyName)
                |> Seq.collect (fun a -> a.Modules)
                |> Seq.filter (fun m -> m.Name = moduleName)
                |> Seq.collect (fun m -> m.GetTypes())
                |> Seq.filter (fun t -> t.FullName = fullName)
                |> Seq.exactlyOne
            | t -> t

    let rec private resolveTypeWithoutGenericParameter id =
        let d = resolveTypeDefinition id.definitionId
        match id.genericTypeArguments with
        | [] -> d
        | ts ->
            let ts = ts |> Seq.map resolveType |> Seq.toArray
            d.MakeGenericType ts

    and resolveType = function
        | TypeId.Normal id -> resolveTypeWithoutGenericParameter id
        | TypeId.GenericParameter(Choice1Of2 d, i) -> resolveType(d).GetGenericArguments().[i]
        | TypeId.GenericParameter(Choice2Of2 d, i) -> resolveMethod(d).GetGenericArguments().[i]

    and resolveMethod { methodToken = t; genericMethodArguments = ts; declaringType = d }: MethodBase =
        let m = resolveTypeWithoutGenericParameter(d).Module.ResolveMethod(t)
        match ts with
        | [] -> m
        | ts ->
            let ts = ts |> List.map resolveType |> List.toArray
            (m :?> MethodInfo).MakeGenericMethod(ts) :> _

    let resolveProperty { declaringType = t; propertyName = p } =
        resolveType(t).GetProperty(p)
