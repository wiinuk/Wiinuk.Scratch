[<AutoOpen>]
module Scratch.Transpiler.TypeSpec
open Scratch.Ast
open Scratch.MemoryModel
open Scratch.MemoryModel.TypeSpec
open Scratch.Primitives
open Scratch.Reflection
open System
open System.Reflection
module E = FSharp.Quotations.DerivedPatterns
module E = FSharp.Quotations.Patterns
module VOption = ValueOption


let isPrimitiveType t = underlyingPrimitiveType t |> VOption.isSome

let collectableInValue t =
    match underlyingType t with
    | ValueNone -> false
    | ValueSome vs -> vs |> List.exists (function UnderlyingTypeSpec(kind = Kind.Collectable) -> true | _ -> false)

let newable t =
    match underlyingType t with
    | ValueSome [UnderlyingTypeSpec(kind = Kind.Collectable)] when not t.IsValueType -> true
    | _ -> false

let isValueType t =
    if typeof<ISyntaxInfrastructure>.IsAssignableFrom t then false else
    if typeof<IWord>.IsAssignableFrom t then false else
    isZeroSizedType t || t.IsValueType

let sBool = [UnderlyingTypeSpec(Typed SType.B, UnderlyingValue, Kind.Primitive)]

let isParameterType = function
    | UnderlyingType t ->
        t
        |> List.forall (function
            | UnderlyingTypeSpec(vType = Any) -> false
            | UnderlyingTypeSpec(vType = Typed _) -> true
        )
    | _ -> false

let isNumericBinaryType t1 t2 tr = (underlyingPrimitiveType t1 = ValueSome (Typed SType.N)) && t1 = t2 && t2 = tr
let isConcatType t1 t2 tr = (underlyingPrimitiveType t1 = ValueSome (Typed SType.S)) && t1 = t2 && t2 = tr
let isAddressAddType t1 t2 tr = t1 = typeof<Address> && (t2 = typeof<N> || t2 = typeof<int>) && tr = typeof<Address>
let isSubtractType t1 t2 tr = isNumericBinaryType t1 t2 tr || t1 = typeof<Address> && t2 = typeof<Address> && tr = typeof<int>

let underlyingParameterType = function
    | Any -> SType.S
    | Typed t -> t


let sharingParameter (typeParameter: Type) typeArgument =
    if typeof<ISyntaxInfrastructure>.IsAssignableFrom typeArgument then
        typeArgument
    elif typeof<IWord>.IsAssignableFrom typeArgument then
        let a = typeParameter.GenericParameterAttributes
        if a.HasFlag GenericParameterAttributes.NotNullableValueTypeConstraint then
            // f<'T when 'T : struct>
            // f<N> -> f<V>
            //
            // f<'T when 'T : struct and 'T :> IWord>
            // f<S> -> f<V>
            //
            // f<'T when 'T : struct and 'T :> IEquatable<'T>>
            // f<N> -> f<N>
            match typeParameter.GetGenericParameterConstraints() with
            | null
            | [||] -> typeof<V>
            | [|c|] when c = typeof<IWord> -> typeof<V>
            | _ -> typeArgument

        elif a.HasFlag GenericParameterAttributes.ReferenceTypeConstraint then
            // f<'T when 'T : not struct>
            // f<MyWord> -> f<IWord>
            //
            // f<'T when 'T : not struct and 'T :> IWord>
            // f<MyWord> -> f<IWord>
            //
            // f<'T when 'T : not struct and 'T :> IEquatable<'T>>
            // f<MyWord> -> f<MyWord>
            match typeParameter.GetGenericParameterConstraints() with
            | null
            | [||] -> typeof<IWord>
            | [|c|] when c = typeof<IWord> -> typeof<IWord>
            | _ -> typeArgument

        else typeof<V>

    elif newable typeArgument then
        match typeParameter.GetGenericParameterConstraints() with
        | null
        | [||] -> typeof<obj>
        | [|c|] when c = typeof<obj> -> typeof<obj>
        | _ -> typeArgument

    else typeArgument

let (|SharingGenericMethod|) (m: MethodInfo) =
    if not m.IsGenericMethod then m else

    let md = m.GetGenericMethodDefinition()
    let ts = Array.map2 sharingParameter (md.GetGenericArguments()) (m.GetGenericArguments())

    try md.MakeGenericMethod ts

    // 他の型制約違反
    with :? ArgumentException -> m
