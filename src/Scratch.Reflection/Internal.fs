[<AutoOpen>]
module internal Scratch.Reflection.Internal
open System
open System.Reflection
type E = FSharp.Quotations.Expr


let trySizeOf (t: Type) =
    if t.IsGenericParameter then
        // 'T

        if t.GenericParameterAttributes &&& GenericParameterAttributes.ReferenceTypeConstraint <> GenericParameterAttributes.None then
            // 'T when 'T : not struct
            Some sizeof<obj>

        elif t.GetGenericParameterConstraints() |> Array.exists (fun t -> t.IsClass) then
            // 'T when 'T :> Enum
            // 'T when 'T :> AnyClass
            Some sizeof<obj>

        else
            None

    elif t.IsPointer || t.IsArray || t.IsByRef || t.IsClass || t.IsInterface then
        Some sizeof<obj>
    else
        // incudes primitive, enum
        let m = typeof<unit>.Assembly.GetType("Microsoft.FSharp.Core.Operators").GetMethod("SizeOf")
        let size = m.MakeGenericMethod(t).Invoke(null, null) :?> int
        Some size

let genericTypeGenericMethodDefinition (m: 'm when 'm :> MethodBase): 'm =
    if
        not m.IsGenericMethod && (
            let t = m.DeclaringType
            not t.IsGenericType && isNull t.DeclaringType
        )
    then
        m
    else
        m.Module.ResolveMethod(m.MetadataToken, null, null) :?> 'm
