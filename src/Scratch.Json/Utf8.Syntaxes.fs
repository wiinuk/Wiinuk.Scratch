module Scratch.Json.Utf8.Syntaxes
open NonStructuralComparison
open System.Collections.Generic
open Utf8Json.Internal
open Scratch.Primitives
open Scratch.Json
open Scratch.Json.Utf8
open Scratch.Json.PartialIsomorphisms


[<Struct>]
type HUnit = | NullToHUnit with
    interface hunit ISyntax with
        member _.TryDeserialize(r, _ref) = r.ReadIsNull()
        member _.Deserialize r = if r.ReadIsNull() then HUnit.HUnit else raiseParsingException &r "expected: 'null'"
        member _.TrySerialize(w, _) = w.WriteNull(); true
        member _.Serialize(w, _) = w.WriteNull()

[<Struct>]
type String = | StringToString with
    interface string ISyntax with
        member _.TryDeserialize(r, x) =
            match r.GetCurrentJsonToken() with
            | T.Null
            | T.String -> x <- r.ReadString(); true
            | _ -> false

        member _.Deserialize r = r.ReadString()
        member _.TrySerialize(w, x) = w.WriteString x; true
        member _.Serialize(w, x) = w.WriteString x

[<Struct>]
type Double = | NumberToDouble with
    interface double ISyntax with
        member _.TryDeserialize(r, result) = tryReadDouble &r &result
        member _.Deserialize r =
            let mutable result = 0.
            if tryReadDouble &r &result then result
            else raiseParsingException &r "expected: number"

        member _.TrySerialize(w, x) = writeDouble &w x; true
        member _.Serialize(w, x) = writeDouble &w x

[<Struct>]
type Bool = | BooleanToBool with
    interface bool ISyntax with
        member _.TryDeserialize(r, x) =
            match r.GetCurrentJsonToken() with
            | T.True
            | T.False -> x <- r.ReadBoolean(); true
            | _ -> false

        member _.Deserialize r = r.ReadBoolean()
        member _.TrySerialize(w, x) = w.WriteBoolean x; true
        member _.Serialize(w, x) = w.WriteBoolean x

[<Struct>]
type True = | True with
    interface hunit ISyntax with
        member _.TryDeserialize(r, _ref) = match r.GetCurrentJsonToken() with T.True -> ignore(r.ReadBoolean()); true | _ -> false
        member _.Deserialize r =
            match r.GetCurrentJsonToken() with
            | T.True -> ignore(r.ReadBoolean()); HUnit.HUnit
            | _ -> raiseParsingException &r "expected: 'true'"

        member _.TrySerialize(w, _) = w.WriteTrue(); true
        member _.Serialize(w, _) = w.WriteTrue()

[<Struct>]
type False = | False with
    interface hunit ISyntax with
        member _.TryDeserialize(r, _ref) = match r.GetCurrentJsonToken() with T.False -> ignore(r.ReadBoolean()); true | _ -> false
        member _.Deserialize r =
            match r.GetCurrentJsonToken() with
            | T.False -> ignore(r.ReadBoolean()); HUnit.HUnit
            | _ -> raiseParsingException &r "expected: 'false'"

        member _.TrySerialize(w, _) = w.WriteFalse(); true
        member _.Serialize(w, _) = w.WriteFalse()

[<Struct; NoEquality; NoComparison>]
type NumberLiteral = internal { literal: double; raw: byte array; errorMessage: string } with
    interface hunit ISyntax with
        member s.TryDeserialize(r, _ref) =
            match r.GetCurrentJsonToken() with
            | T.Number -> r.ReadDouble() = s.literal
            | _ -> false

        member s.Deserialize r =
            if r.ReadDouble() = s.literal then HUnit.HUnit
            else raiseParsingException &r s.errorMessage

        member s.TrySerialize(w, _) = w.WriteRaw s.raw; true
        member s.Serialize(w, _) = w.WriteRaw s.raw

[<Struct; NoEquality; NoComparison>]
type StringLiteral = internal {
    literal: string
    raw: byte array
    rawQuoted: byte array
    errorMessage: string
}
with
    interface hunit ISyntax with
        member s.TryDeserialize(r, _ref) =

            // TODO: null
            match r.GetCurrentJsonToken() with
            | T.String ->

                // TODO:
                // No escape compare
                let t = readStringSegmentRaw &r
                ByteArrayComparer.Equals(t.Array, t.Offset, t.Count, s.raw, 0, s.raw.Length)

            | _ -> false

        member s.Deserialize r =
            match r.GetCurrentJsonToken() with
            | T.String ->
                let t = readStringSegmentRaw &r

                // TODO:
                // No escape compare
                if ByteArrayComparer.Equals(t.Array, t.Offset, t.Count, s.raw, 0, s.raw.Length)

                then HUnit.HUnit
                else raiseParsingException &r s.errorMessage

            | _ -> raiseParsingException &r s.errorMessage

        member s.TrySerialize(w, _) = w.WriteRaw s.rawQuoted; true
        member s.Serialize(w, _) = w.WriteRaw s.rawQuoted

[<Struct; NoEquality; NoComparison>]
type Pipe<'T,'TSource,'TIso,'TResult>
    when 'TSource :> 'T ISyntax
    and 'TIso :> IsoR<'T,'TResult> = internal {
    mutable source: 'TSource
    mutable mapping: 'TIso
}
with
    interface 'TResult ISyntax with
        member s.TryDeserialize(r, result) =
            let mutable result' = Unchecked.defaultof<_>
            s.source.TryDeserialize(&r, &result') &&
            s.mapping.Forward(&result', &result)

        member s.Deserialize r =
            // TODO: inref
            let mutable x = s.source.Deserialize &r
            let mutable result = Unchecked.defaultof<_>
            if s.mapping.Forward(&x, &result) then result
            else raiseParsingException &r "pipe"

        // TODO:
        member s.TrySerialize(w, x) =
            // TODO: inref
            let mutable x = x
            let mutable x' = Unchecked.defaultof<_>
            s.mapping.Reverse(&x, &x') &&
            s.source.TrySerialize(&w, x')

        member s.Serialize(w, x) =
            // TODO: inref
            let mutable x = x
            let mutable x' = Unchecked.defaultof<_>
            if s.mapping.Reverse(&x, &x') then
                s.source.Serialize(&w, x')
            else
                failwithf "pipe %A" x

[<Struct; NoEquality; NoComparison>]
type Or<'T,'TSource1,'TSource2>
    when 'TSource1 :> 'T ISyntax
    and 'TSource2 :> 'T ISyntax = internal {
    mutable source1: 'TSource1
    mutable source2: 'TSource2
}
with
    interface 'T ISyntax with
        member s.TryDeserialize(r, x) =
            let r' = r
            if s.source1.TryDeserialize(&r, &x) then true else

            r <- r'
            s.source2.TryDeserialize(&r, &x)

        member s.Deserialize r =
            let mutable result = Unchecked.defaultof<_>
            let r' = r
            if s.source1.TryDeserialize(&r, &result) then result else

            r <- r'
            if s.source2.TryDeserialize(&r, &result) then result else

            failwith "or"

        member s.TrySerialize(w, x) =
            let w' = w
            if s.source1.TrySerialize(&w, x) then true else

            w <- w'
            s.source2.TrySerialize(&w, x)

        member s.Serialize(w, x) =
            let w' = w
            if s.source1.TrySerialize(&w, x) then () else

            w <- w'
            if s.source2.TrySerialize(&w, x) then () else

            failwith "or"

[<Struct; NoEquality; NoComparison>]
type ForwardedToRef<'T> = internal { ref: 'T ISyntax ref } with
    interface 'T ISyntax with
        member s.TryDeserialize(r, x) = s.ref.contents.TryDeserialize(&r, &x)
        member s.Deserialize r = s.ref.contents.Deserialize &r
        member s.TrySerialize(w, x) = s.ref.contents.TrySerialize(&w, x)
        member s.Serialize(w, x) = s.ref.contents.Serialize(&w, x)

[<Struct; NoEquality; NoComparison>]
type List<'T,'TSource> when 'TSource :> 'T ISyntax = internal {
    mutable syntax: CollectionSyntax<'T list,'TSource,'T ListOps,'T,'T ResizeArray,'T list>
}
with
    interface 'T list ISyntax with
        member s.TryDeserialize(r, x) = tryDeserializeMany &s.syntax &r &x
        member s.Deserialize r = deserializeMany &s.syntax &r
        member s.TrySerialize(w, xs) = trySerializeMany &s.syntax &w xs
        member s.Serialize(w, xs) = serializeMany &s.syntax &w xs

[<Struct; NoEquality; NoComparison>]
type ImmutableArray<'T,'TSource> when 'TSource :> 'T ISyntax = internal {
    mutable syntax: CollectionSyntax<'T ImmutableArray, 'TSource, 'T IArrayOps, 'T, 'T ImmutableArray.Builder, 'T ImmutableArray.Enumerator>
}
with
    interface 'T ImmutableArray ISyntax with
        member s.TryDeserialize(r, x) = tryDeserializeMany &s.syntax &r &x
        member s.Deserialize r = deserializeMany &s.syntax &r
        member s.TrySerialize(w, xs) = trySerializeMany &s.syntax &w xs
        member s.Serialize(w, xs) = serializeMany &s.syntax &w xs

[<Struct; NoEquality; NoComparison>]
type Map<'TKey,'TValue,'TKeySource,'TValueSource>
    when 'TKey : comparison
    and 'TKeySource :> 'TKey ISyntax
    and 'TValueSource :> 'TValue ISyntax = internal {
    mutable syntax: KeyValueCollectionSyntax<'TKey,'TValue,'TKeySource,'TValueSource, MapOps<'TKey,'TValue>, Map<'TKey,'TValue>, KeyValuePair<'TKey,'TValue> ResizeArray, KeyValuePair<'TKey,'TValue> IEnumerator>
}
with
    interface Map<'TKey,'TValue> ISyntax with
        member s.TryDeserialize(r, x) = tryDeserializeKvs &s.syntax &r &x
        member s.Deserialize r = deserializeKvs &s.syntax &r
        member s.TrySerialize(w, x) = trySerializeKvs &s.syntax &w x
        member s.Serialize(w, x) = serializeKvs &s.syntax &w x

[<Struct; NoEquality; NoComparison>]
type KeyValueArray<'TKey,'TValue,'TKeySource,'TValueSource>
    when 'TKey : comparison
    and 'TKeySource :> 'TKey ISyntax
    and 'TValueSource :> 'TValue ISyntax = internal {
    mutable syntax: KeyValueCollectionSyntax<'TKey,'TValue,'TKeySource,'TValueSource, IArrayOps<KeyValuePair<'TKey,'TValue>>, KeyValuePair<'TKey,'TValue> ImmutableArray, KeyValuePair<'TKey,'TValue> ImmutableArray.Builder, KeyValuePair<'TKey,'TValue> ImmutableArray.Enumerator>
}
with
    interface KeyValuePair<'TKey,'TValue> ImmutableArray ISyntax with
        member s.TryDeserialize(r, x) = tryDeserializeKvs &s.syntax &r &x
        member s.Deserialize r = deserializeKvs &s.syntax &r
        member s.TrySerialize(w, x) = trySerializeKvs &s.syntax &w x
        member s.Serialize(w, x) = serializeKvs &s.syntax &w x

[<Struct; NoEquality; NoComparison>]
type Option<'T,'TSource> when 'TSource :> 'T ISyntax = internal { mutable source: 'TSource } with
    interface 'T option ISyntax with
        member s.TryDeserialize(r, x) =
            if r.ReadIsNull() then x <- None; true else

            let mutable value = Unchecked.defaultof<_>
            if s.source.TryDeserialize(&r, &value) then
                x <- Some value
                true
            else
                false

        member s.Deserialize r =
            if r.ReadIsNull() then None else
            Some(s.source.Deserialize &r)

        member s.TrySerialize(w, x) =
            match x with
            | None -> w.WriteNull(); true
            | Some x -> s.source.TrySerialize(&w, x)

        member s.Serialize(w, x) =
            match x with
            | None -> w.WriteNull()
            | Some x -> s.source.Serialize(&w, x)

[<NoEquality; NoComparison>]
type JsonNoLocation = internal {
    mutable array: unit Json list ISyntax
    mutable object: OMap<string, unit Json> ISyntax
}
with
    interface unit Json ISyntax with
        member s.TryDeserialize(r, result) =
            match r.GetCurrentJsonToken() with
            | T.Null -> result <- Json.jnull; r.AdvanceOffset 4; true
            | T.True -> result <- Json.jtrue; r.AdvanceOffset 4; true
            | T.False -> result <- Json.jfalse; r.AdvanceOffset 5; true

            | T.String ->
                result <- Json.jstring <| r.ReadString()
                true

            | T.BeginArray ->
                let mutable x = []
                s.array.TryDeserialize(&r, &x) && (
                    result <- Json.jarray x
                    true
                )
            | T.BeginObject ->
                let mutable x = OMap.empty
                s.object.TryDeserialize(&r, &x) && (
                    result <- Json.jobject x
                    true
                )

            | _ ->
                let mutable n = 0.
                tryReadDouble &r &n && (
                    result <- Json.jnumber n
                    true
                )

        member s.Deserialize r =
            let mutable result = Unchecked.defaultof<_>
            let mutable s = s
            if tryDeserialize &s &r &result then result else
            raiseParsingException &r "expected: json"

        member s.TrySerialize(w, Json(token = x)) =
            match x with
            | JNull -> w.WriteNull(); true
            | JTrue -> w.WriteTrue(); true
            | JFalse -> w.WriteFalse(); true
            | JNumber x -> writeDouble &w x; true
            | JString x -> w.WriteString x; true
            | JArray xs -> s.array.TrySerialize(&w, xs)
            | JObject xs -> s.object.TrySerialize(&w, xs)

        member s.Serialize(w, x) =
            let mutable s = s
            if trySerialize &s &w x then ()
            else failwithf "expected: json"
