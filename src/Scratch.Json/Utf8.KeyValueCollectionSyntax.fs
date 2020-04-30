[<AutoOpen>]
module internal Scratch.Json.Utf8.KeyValueCollectionSyntax
open System.Collections.Generic


[<Struct; NoEquality; NoComparison>]
type KeyValueCollectionSyntax<'TKey,'TValue,'TKeySource,'TValueSource,'TCollectionOperations,'T,'TBuilder,'TIterator>
    when 'TKey : comparison
    and 'TKeySource :> 'TKey ISyntax
    and 'TValueSource :> 'TValue ISyntax
    and 'TCollectionOperations :> ICollectionOperations<'T, KeyValuePair<'TKey,'TValue>,'TBuilder,'TIterator> = internal {
    mutable collection: 'TCollectionOperations
    mutable key: 'TKeySource
    mutable value: 'TValueSource
}
let tryDeserializeKvs (s: _ byref) (r: _ byref) (result: _ outref) =
    if not <| readIsBeginObject &r then false else

    let mutable builder = s.collection.Create()
    let mutable count = 0
    let mutable success = false
    let mutable key = Unchecked.defaultof<_>
    let mutable value = Unchecked.defaultof<_>
    while
        if tryReadEndObjectOrValueSeparator &r &count &success then
            if not <| s.key.TryDeserialize(&r, &key) then false else
            if not <| r.ReadIsNameSeparator() then false else
            if not <| s.value.TryDeserialize(&r, &value) then false else
            s.collection.Add(&builder, KeyValuePair(key, value))
            true
        else
            false
        do ()

    result <- s.collection.Complete &builder
    success

let deserializeKvs (s: _ byref) (r: _ byref) =
    readIsBeginObjectWithVerify &r

    let mutable builder = s.collection.Create()
    let mutable count = 0
    while not (r.ReadIsEndObjectWithSkipValueSeparator &count) do
        let key = s.key.Deserialize &r
        r.ReadIsNameSeparatorWithVerify()
        let value = s.value.Deserialize &r
        s.collection.Add(&builder, KeyValuePair(key, value))
    s.collection.Complete &builder

let trySerializeKvs (s: _ byref) (w: _ byref) x =
    writeBeginObject &w

    let mutable it = s.collection.Iterator x
    let mutable kv = KeyValuePair()
    if not <| s.collection.Next(&it, &kv) then w.WriteEndObject(); true else

    if not <| s.key.TrySerialize(&w, kv.Key) then false else
    w.WriteNameSeparator()
    if not <| s.value.TrySerialize(&w, kv.Value) then false else

    let mutable success = false
    while
        begin
        if s.collection.Next(&it, &kv) then
            w.WriteValueSeparator()
            if not <| s.key.TrySerialize(&w, kv.Key) then false else
            w.WriteNameSeparator()
            s.value.TrySerialize(&w, kv.Value)
        else
            success <- true
            false
        end
        do ()

    w.WriteEndObject()
    success

let serializeKvs (s: _ byref) (w: _ byref) x =
    writeBeginObject &w

    let mutable it = s.collection.Iterator x
    let mutable kv = KeyValuePair()
    if not <| s.collection.Next(&it, &kv) then w.WriteEndObject() else

    s.key.Serialize(&w, kv.Key)
    w.WriteNameSeparator()
    s.value.Serialize(&w, kv.Value)

    while s.collection.Next(&it, &kv) do
        w.WriteValueSeparator()
        s.key.Serialize(&w, kv.Key)
        w.WriteNameSeparator()
        s.value.Serialize(&w, kv.Value)

    w.WriteEndObject()
