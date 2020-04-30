namespace global

module Map =
    let tryFind k (map: Map<_,_>) =
        let mutable v = Unchecked.defaultof<_>
        if map.TryGetValue(k, &v) then ValueSome v
        else ValueNone
