module Scratch.Json.Utf8.Syntax
open NonStructuralComparison
open System
open System.IO
open System.Text
open Utf8Json
open Utf8Json.Internal
open Scratch.Primitives
open Scratch.Json.PartialIsomorphisms.Iso
open Scratch.Json.Utf8.Internal


let deserialize (syntax: 's when 's :> _ ISyntax and 's : not struct) utf8Json =
    let mutable r = JsonReader utf8Json
    syntax.Deserialize &r

let deserializeString syntax json =
    deserialize syntax <| Encoding.UTF8.GetBytes(s = json)

let private ensure (items: _ byref) newSize =
    let newItems = Array.zeroCreate newSize
    Buffer.BlockCopy(items, 0, newItems, 0, items.Length)
    items <- newItems

let deserializeStream syntax (stream: Stream) = async {
    let mutable items = Array.zeroCreate 2048
    let mutable itemsSize = 0
    let mutable loop = true
    while loop do
        let! readCount = stream.AsyncRead(items, itemsSize, items.Length - itemsSize)
        if 0 < readCount then
            itemsSize <- itemsSize + readCount
            if itemsSize = items.Length then
                ensure &items (itemsSize * 2)
        else
            loop <- false

    return deserialize syntax items
}
let serialize (syntax: 's when 's :> _ ISyntax and 's : not struct) value =
    let mutable writer = JsonWriter(Array.zeroCreate 65535)
    syntax.Serialize(&writer, value)
    writer.ToUtf8ByteArray()

let serializeString (syntax: 's when 's :> _ ISyntax and 's : not struct) value =
    let mutable writer = JsonWriter(Array.zeroCreate 65535)
    syntax.Serialize(&writer, value)
    writer.ToString()

let serializeStream (syntax: 's when 's :> _ ISyntax and 's : not struct) (stream: Stream) value = async {
    let buffer = Array.zeroCreate 65535
    let mutable writer = JsonWriter buffer
    syntax.Serialize(&writer, value)
    let buffer = writer.GetBuffer()
    do! stream.AsyncWrite(buffer.Array, buffer.Offset, buffer.Count)
}
let raiseParsingException (r: _ inref) message =
    raise <| makeParsingException r message null

let box (syntax: _ ISyntax) = syntax


// syntaxes
open Scratch.Json.Utf8.Syntaxes
let jNull = NullToHUnit
let jString = StringToString
let jNumber = NumberToDouble
let jBoolean = BooleanToBool
let jTrue = True
let jFalse = False
let jNumberLiteral literal = {
    literal = literal
    raw =
        let mutable w = JsonWriter()
        w.WriteDouble literal
        w.ToUtf8ByteArray()

    errorMessage = $"expected: '%0.17g{literal}'"
}
let jStringLiteral literal =
    let quoted =
        let mutable w = JsonWriter()
        writeString &w literal
        w.ToUtf8ByteArray()
    {
        literal = literal
        raw = quoted.[1..quoted.Length-2]
        rawQuoted = quoted
        errorMessage = $"expected: '{literal}'"
    }

let (|>>) source mapping = { Syntaxes.Pipe.source = source; mapping = mapping }
let (<|>) source1 source2 = { Or.source1 = source1; source2 = source2 }

let forwardedToRef() =
    let ref = ref { new ISyntax<_> with
        member _.TryDeserialize(_,_) = failwith ""
        member _.Deserialize _ = failwith ""
        member _.TrySerialize(_,_) = failwith ""
        member _.Serialize(_,_) = failwith ""
    }
    { ref = ref }, ref

let jImmutableArray item = { ImmutableArray.syntax = { item = item; collection = IArrayOps } }
let jList item = { List.syntax = { item = item; collection = ListOps } }

let jKeyValues value = { KeyValueArray.syntax = { key = StringToString; value = value; collection = IArrayOps } }
let jMap value = { Map.syntax = { key = StringToString; value = value; collection = MapOps } }
let jOMap value = jKeyValues value |>> {
    forward = fun xs ->
        let mutable map = OMap.empty
        let mutable e = xs.GetInEnumerator()
        while e.MoveNext() do
            map <- OMap.add e.Current.Key e.Current.Value map
        Ok map

    reverse = fun map -> map |> OMap.toSeqOrdered |> IArray.ofSeq |> Ok
}
let jNullable source = { Option.source = source }

let json =
    let json = {
        array = Unchecked.defaultof<_>
        object = Unchecked.defaultof<_>
    }
    json.array <- jList json
    json.object <- jOMap json
    json


// array syntaxes
open Scratch.Json.Utf8.ArraySyntaxes

/// `[head, ...tail]`
let ( ** ) head tail = { HCons.head = head; HCons.tail = tail }
/// `[head?, ...tail]`
let ( **? ) head tail = { OptionalHCons.head = head; OptionalHCons.tail = tail }
/// `[head?, ...tail]`
let ( **! ) (head, defaultValue) tail = { DefaultHCons.head = head; DefaultHCons.tail = tail; DefaultHCons.defaultValue = defaultValue }
/// `[]`
let jEmptyArray = EmptyTuple
let jTupleToList item = { List.item = item }
let jArray source = { ToSyntax.source = source }


// object syntaxes
open Scratch.Json.Utf8.ObjectSyntaxes

/// `{}`
let jEmptyObject = EmptyObject
/// `{ [key]: value }`
let ignoreProperties = IgnoreProperties
/// `{ key: value, ...rest }`
let (@@) (key, value) rest = {
    Property.keyString = key
    keySerializer = makeKeySerializer key
    value = value
    rest = rest
}
/// `{ key?: value, ...rest }`
let (@@?) (key, value) rest = {
    OptionalProperty.keyString = key
    keySerializer = makeKeySerializer key
    value = value
    rest = rest
}
/// `{ key?: value, ...rest }`
let (@@!) (key, value, defaultValue) rest = {
    DefaultProperty.keyString = key
    keySerializer = makeKeySerializer key
    defaultValue = defaultValue
    value = value
    rest = rest
}
/// `{ ...source }`
let (|>>@) source mapping = { ObjectSyntaxes.Pipe.source = source; mapping = mapping }

/// `{ ...objectSyntax }`
let jObject (objectSyntax: #IObjectSyntax<_,_>) = {
    dictionary = objectSyntax.GetPropertyKeys 0 |> List.fold (fun (d: AutomataDictionary) (k, i) -> d.Add(k, i); d) (new _())
    source = objectSyntax
}

module Values =
    let ( ** ) head tail = { MVCons.head = head; MVCons.tail = tail }
    let jTupleToList item = { ArraySyntaxes.ListV.item = item }

    let (@@) (key, value) rest = {
        ValueProperty.keyString = key
        keySerializer = makeKeySerializer key
        value = value
        rest = rest
    }
    let (@@?) (key, value) rest = {
        ValueOptionalProperty.keyString = key
        keySerializer = makeKeySerializer key
        value = value
        rest = rest
    }
    let (@@!) (key, value, defaultValue) rest = {
        ValueDefaultProperty.keyString = key
        keySerializer = makeKeySerializer key
        defaultValue = defaultValue
        value = value
        rest = rest
    }
