namespace global

#if NETSTANDARD1_0 || NETSTANDARD1_1 || NETSTANDARD1_2 || NETSTANDARD1_3 || NETSTANDARD1_4 || NETSTANDARD1_5 || NETSTANDARD1_6 || NETSTANDARD2_0
[<AutoOpen>]
module RuntimeHelpersExtensions =
    open System
    type private B = System.Reflection.BindingFlags

    let rec private isReferenceOrContainsReferences (t: Type) =
        if not t.IsValueType then true else
        if t.IsPrimitive then false else

        t.GetFields(B.NonPublic ||| B.Public ||| B.Instance)
        |> Seq.exists (fun f -> isReferenceOrContainsReferences f.FieldType)

    [<Sealed; AbstractClass>]
    type IsReferenceOrContainsReferencesHolder<'T> private () =
        static let value = isReferenceOrContainsReferences typeof<'T>
        static member Value = value

    type System.Runtime.CompilerServices.RuntimeHelpers with
        [<RequiresExplicitTypeArguments>]
        static member IsReferenceOrContainsReferences<'T>() = IsReferenceOrContainsReferencesHolder<'T>.Value
#endif
