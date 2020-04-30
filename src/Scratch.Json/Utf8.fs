namespace Scratch.Json.Utf8
open Utf8Json
open Scratch.Json.PartialIsomorphisms


type ISyntax<'T> =
    abstract TryDeserialize: reader: JsonReader byref * result: 'T outref -> bool
    abstract Deserialize: reader: JsonReader byref -> 'T
    abstract TrySerialize: writer: JsonWriter byref * value: 'T -> bool
    abstract Serialize: writer: JsonWriter byref * value: 'T -> unit

[<AutoOpen>]
module internal InternalSyntax =
    let tryDeserialize (s: 's byref when 's :> _ ISyntax) (r: _ byref) (result: _ outref) = s.TryDeserialize(&r, &result)
    let deserialize (s: 's byref when 's :> _ ISyntax) (r: _ byref) = s.Deserialize &r
    let trySerialize (s: 's byref when 's :> _ ISyntax) (w: _ byref) x = s.TrySerialize(&w, x)
    let serialize (s: 's byref when 's :> _ ISyntax) (w: _ byref) x = s.Serialize(&w, x)

[<Struct; NoEquality; NoComparison>]
type ArrayContext = {
    mutable count: int
}

type IArraySyntax<'T> =
    abstract TryDeserialize: reader: JsonReader byref * context: ArrayContext byref * result: 'T outref -> bool
    abstract Deserialize: reader: JsonReader byref * context: ArrayContext byref -> 'T
    abstract TrySerialize: writer: JsonWriter byref * context: ArrayContext byref * value: 'T -> bool
    abstract Serialize: writer: JsonWriter byref * context: ArrayContext byref * value: 'T -> unit

[<Struct>]
type ParseError = { messageOrExn: IsoError; reader: JsonReader }

[<Struct; NoEquality; NoComparison>]
type ObjectParsingInfo = {
    mutable lastReader: JsonReader
    mutable error: ParseError
}

[<Struct; NoEquality; NoComparison>]
type ObjectParsingContext = {
    mutable propertyKey: int
    mutable currentKey: int
    mutable error: ParseError
}

type IObjectSyntax<'T,'TIntermediate> =
    abstract GetPropertyKeys: currentKey: int -> (string * int) list

    abstract InitializeFromZero: zero: 'TIntermediate outref -> unit
    abstract DeserializeAndAdd: reader: JsonReader byref * context: ObjectParsingContext byref * intermediate: 'TIntermediate byref -> bool
    abstract Complete: context: ObjectParsingInfo byref * intermediate: 'TIntermediate byref * result: 'T outref -> bool

    abstract TrySerialize: writer: JsonWriter byref * value: 'T * count: int -> bool

type JsonParsingExceptionWithInner(message, bytes, offset, limit, actual, inner) =
    inherit JsonParsingException(message, bytes, offset, limit, actual)
    member _.Inner = inner
