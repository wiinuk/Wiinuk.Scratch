[<AutoOpen>]
module Scratch.Reflection.MemberPatterns
open System.Reflection
open System
open FSharp.Reflection
open Scratch.Primitives
type private T = FSharp.Reflection.FSharpType
module VOption = ValueOption


let (|GenericMethodDefinition|) (m: MethodInfo) =
    if m.IsGenericMethod then m.GetGenericMethodDefinition() else m

[<return: Struct>]
let (|GenericType|_|) (t: Type) =
    if t.IsGenericType then ValueSome struct(t.GetGenericTypeDefinition(), t.GetGenericArguments() |> Array.toList)
    else ValueNone

[<return: Struct>]
let (|TupleType|_|) t = if T.IsTuple t then ValueSome(T.GetTupleElements t |> Array.toList) else ValueNone
[<return: Struct>]
let (|GenericParameterType|_|) (t: Type) =
    if t.IsGenericParameter then ValueSome struct(t.GenericParameterAttributes, t.GetGenericParameterConstraints() |> Array.toList)
    else ValueNone

[<return: Struct>]
let (|StructType|_|) (t: Type) = if t.IsValueType then ValueSome() else ValueNone
[<return: Struct>]
let (|RecordType|_|) t = if T.IsRecord(t, true) then ValueSome(T.GetRecordFields(t, true)) else ValueNone
[<return: Struct>]
let (|UnionType|_|) t = if T.IsUnion(t, true) then ValueSome(T.GetUnionCases(t, true)) else ValueNone
[<return: Struct>]
let (|DeclaringType|_|) (t: #MemberInfo) = t.DeclaringType |> VOption.ofObj
[<return: Struct>]
let (|ModuleType|_|) t = if T.IsModule t then ValueSome() else ValueNone
[<return: Struct>]
let (|ExceptionRepresentationType|_|) t = if T.IsExceptionRepresentation t then ValueSome() else ValueNone

let (|CaseFields|) (c: UnionCaseInfo) = c.GetFields()

let (|GenericTypeDefinition|) = function
    | GenericType(d, _) -> d
    | t -> t
