namespace global

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =

#if NET45
    [<AbstractClass; Sealed>]
    type private EmptyArrayHolder<'T> private () =
        static let value: 'T[] = [||]
        static member Value = value

    /// singleton instance of empty array
    [<GeneralizableValue>]
    let empty<'T> = EmptyArrayHolder<'T>.Value
#else
    /// alias of System.Array.Empty
    [<GeneralizableValue>]
    let empty<'T> = System.Array.Empty<'T>()
#endif
