[<AutoOpen>]
module Scratch.Reflection.MemberPatterns
open System.Reflection
open System
open FSharp.Reflection
type private T = FSharp.Reflection.FSharpType


let (|GenericMethodDefinition|) (m: MethodInfo) =
    if m.IsGenericMethod then m.GetGenericMethodDefinition() else m

let (|GenericType|_|) (t: Type) =
    if t.IsGenericType then Some(t.GetGenericTypeDefinition(), t.GetGenericArguments() |> Array.toList)
    else None

let (|TupleType|_|) t = if T.IsTuple t then Some(T.GetTupleElements t |> Array.toList) else None
let (|GenericParameterType|_|) (t: Type) =
    if t.IsGenericParameter then Some(t.GenericParameterAttributes, t.GetGenericParameterConstraints() |> Array.toList)
    else None

let (|StructType|_|) (t: Type) = if t.IsValueType then Some() else None
let (|RecordType|_|) t = if T.IsRecord(t, true) then Some(T.GetRecordFields(t, true)) else None
let (|UnionType|_|) t = if T.IsUnion(t, true) then Some(T.GetUnionCases(t, true)) else None
let (|DeclaringType|_|) (t: #MemberInfo) = t.DeclaringType |> Option.ofObj
let (|ModuleType|_|) t = if T.IsModule t then Some() else None
let (|ExceptionRepresentationType|_|) t = if T.IsExceptionRepresentation t then Some() else None

let (|CaseFields|) (c: UnionCaseInfo) = c.GetFields()

let (|GenericTypeDefinition|) = function
    | GenericType(d, _) -> d
    | t -> t
