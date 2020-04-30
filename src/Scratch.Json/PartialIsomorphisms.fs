namespace Scratch.Json.PartialIsomorphisms


[<Struct; RequireQualifiedAccess>]
type IsoError =
    val internal _data: obj
    internal new (data: obj) = { _data = data }

module IsoError =
    let ofMessage (message: string) = IsoError message
    let ofException (``exception``: exn) = IsoError ``exception``

    let expectionOrNull (e: IsoError) =
        match e._data with
        | :? exn as x -> x
        | _ -> null

    let messageOrNull (e: IsoError) =
        match e._data with
        | :? string as x -> x
        | :? exn as x -> x.Message
        | _ -> null

[<Sealed>]
type IsoExceptionN(message, innerException, isos: IsoError list) =
    inherit exn(message, innerException = innerException)
    member _.Data1 = isos

[<Sealed>]
type IsoException2(message, innerException, iso1: IsoError, iso2: IsoError) =
    inherit exn(message, innerException = innerException)
    member _.Data1 = iso1
    member _.Data2 = iso2

type IsoR<'T1,'T2> =
    // TODO: inref
    abstract Forward: value: 'T1 byref * result: 'T2 outref -> bool
    abstract Reverse: value: 'T2 byref * result: 'T1 outref -> bool
