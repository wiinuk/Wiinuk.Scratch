[<AutoOpen>]
module internal Scratch.Json.Utf8.CollectionSyntax

[<Struct>]
type CollectionSyntax<'TCollection,'TSource,'TCollectionOperations,'TItem,'TBuilder,'TIterator>
    when 'TCollectionOperations :> ICollectionOperations<'TCollection,'TItem,'TBuilder,'TIterator>
    and 'TSource :> 'TItem ISyntax = {
    mutable collection: 'TCollectionOperations
    mutable item: 'TSource
}

let tryDeserializeMany (s: _ byref) (r: _ byref) (result: _ outref) =
    if not <| readIsBeginArray &r then false else
    let mutable state = s.collection.Create()
    let mutable context = { count = 0 }
    let mutable success = false
    let mutable item = Unchecked.defaultof<_>
    while
        if tryReadEndArrayOrValueSeparator &r &context &success then
            if not <| tryDeserialize &s.item &r &item then false else

            s.collection.Add(&state, item)
            true
        else
            false
        do ()
    result <- s.collection.Complete &state
    success

let deserializeMany (s: _ byref) (r: _ byref) =
    readIsBeginArrayWithVerify &r
    let mutable state = s.collection.Create()
    let mutable count = 0
    while not (r.ReadIsEndArrayWithSkipValueSeparator &count) do
        s.collection.Add(&state, deserialize &s.item &r)
    s.collection.Complete &state

let trySerializeMany (s: _ byref) (w: _ byref) state =
    writeBeginArray &w

    let mutable it = s.collection.Iterator state
    let mutable x = Unchecked.defaultof<_>
    if not <| s.collection.Next(&it, &x) then
        w.WriteEndArray(); true
    else

    if not <| trySerialize &s.item &w x then false else

    let mutable success = false
    while
        if not <| s.collection.Next(&it, &x) then
            success <- true
            false
        else
            w.WriteValueSeparator()
            trySerialize &s.item &w x
        do ()

    w.WriteEndArray()
    true

let serializeMany (s: _ byref) (w: _ byref) state =
    writeBeginArray &w

    let mutable it = s.collection.Iterator state
    let mutable x = Unchecked.defaultof<_>
    if not <| s.collection.Next(&it, &x) then
        w.WriteEndArray()
    else

    serialize &s.item &w x
    while
        if not <| s.collection.Next(&it, &x) then
            false
        else
            w.WriteValueSeparator()
            s.item.Serialize(&w, x)
            true
        do ()
    w.WriteEndArray()
