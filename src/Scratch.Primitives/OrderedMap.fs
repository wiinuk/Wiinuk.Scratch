namespace Scratch.Primitives
open System
open System.Collections
open System.Collections.Generic


module internal OMapHelpers =
    [<Struct; NoComparison; NoEquality>]
    type SortedEnumerator<'K,'V> = {
        e: KeyValuePair<'K, KeyValuePair<int,'V>> IEnumerator
    }
    with
        interface IEnumerator with
            override e.Reset() = e.e.Reset()
            override e.MoveNext() = e.e.MoveNext()
            override e.Current = (e :> _ IEnumerator).Current :> _

        interface IEnumerator<KeyValuePair<'K,'V>> with
            override e.Current =
                let mutable kv = e.e.Current
                let mutable v = kv.Value
                KeyValuePair(kv.Key, v.Value)
            override e.Dispose() = e.e.Dispose()

    [<Struct; NoComparison; NoEquality>]
    type SortedEnumerable<'K,'V> when 'K : comparison = {
        map: Map<'K, KeyValuePair<int,'V>>
    }
    with
        interface IEnumerable with
            override e.GetEnumerator() = (e :> _ seq).GetEnumerator() :> _
        interface IEnumerable<KeyValuePair<'K,'V>> with
            override e.GetEnumerator() = { e = (e.map :> _ seq).GetEnumerator() } :> _

    [<Struct; NoComparison; NoEquality>]
    type OrderedEnumerator<'K,'V> = {
        e: KeyValuePair<int, KeyValuePair<'K,'V>> IEnumerator
    }
    with
        interface IEnumerator with
            override e.MoveNext() = e.e.MoveNext()
            override e.Current = (e :> _ IEnumerator).Current :> _
            override e.Reset() = e.e.Reset()

        interface IEnumerator<KeyValuePair<'K,'V>> with
            override e.Current =
                let mutable kv = e.e.Current
                kv.Value

            override e.Dispose() = e.e.Dispose()

    [<Struct; NoComparison; NoEquality>]
    type OrderedEnumerable<'K,'V> = {
        imap: Map<int, KeyValuePair<'K,'V>>
    }
    with
        interface IEnumerable with
            override e.GetEnumerator() = (e :> _ seq).GetEnumerator() :> IEnumerator
        interface IEnumerable<KeyValuePair<'K,'V>> with
            override e.GetEnumerator() = { e = (e.imap :> _ seq).GetEnumerator() } :> _

    [<Struct>]
    type OMapView<'K,'V> = OMap of ('K * 'V) list

open OMapHelpers

[<Struct; CustomEquality; CustomComparison; StructuredFormatDisplay "{Display}">]
type OMap<[<EqualityConditionalOn; ComparisonConditionalOn>] 'K, [<EqualityConditionalOn; ComparisonConditionalOn>] 'V> when 'K : comparison = private {
    nextIndex: int
    map: Map<'K, KeyValuePair<int,'V>>
    imap: Map<int, KeyValuePair<'K,'V>>
}
with
    member x.Equals y =
        let e1, e2 = (x.map :> _ seq).GetEnumerator(), (y.map :> _ seq).GetEnumerator()
        let rec aux() =
            match e1.MoveNext(), e2.MoveNext() with
            | false, false -> true
            | true, true ->
                let kv1, kv2 = e1.Current, e2.Current
                kv1.Key = kv2.Key &&
                let v1 = kv1.Value
                let v2 = kv2.Value
                Unchecked.equals v1.Value v2.Value &&
                aux()

            | _ -> false
        aux()

    member private m.Display =
        m.imap
        |> Seq.map (fun kv -> let v = kv.Value in v.Key, v.Value)
        |> Seq.toList
        |> OMapView.OMap

    override m.ToString() = string m.Display

    override x.Equals y =
        match y with
        | :? OMap<'K,'V> as y -> x.Equals y
        | _ -> false

    override x.GetHashCode() =
        let (++) x y = (x <<< 1) + y + 631 
        let mutable v = 0
        for kv in x do
            v <- v ++ hash kv.Key
            v <- v ++ Unchecked.hash kv.Value
        abs v

    interface IEquatable<OMap<'K,'V>> with
        member x.Equals y = x.Equals y

    interface IComparable<OMap<'K,'V>> with
        member x.CompareTo y =
            Seq.compareWith (fun (kv1: KeyValuePair<_,_>) (kv2: KeyValuePair<_,_>) ->
                let c = compare kv1.Key kv2.Key
                if c <> 0 then c else Unchecked.compare kv1.Value kv2.Value
            ) x y

    interface IReadOnlyDictionary<'K,'V> with
        member m.GetEnumerator() = (m :> _ seq).GetEnumerator() :> IEnumerator
        member m.GetEnumerator() = ({ imap = m.imap } :> _ seq).GetEnumerator()
        member m.Count = Map.count m.map

        member m.ContainsKey k = Map.containsKey k m.map
        member m.Item with get key = let mutable kv = Map.find key m.map in kv.Value
        member m.TryGetValue(key, value) =
            let mutable r = KeyValuePair()
            if m.map.TryGetValue(key, &r) then
                value <- r.Value
                true
            else
                false

        member m.Keys = (m.map :> IReadOnlyDictionary<_,_>).Keys
        member m.Values = m.map |> Seq.map (fun kv -> let mutable v = kv.Value in v.Value)

module OMap =
    let empty = { nextIndex = 0; map = Map.empty; imap = Map.empty }
    let isEmpty map = Map.isEmpty map.map
    let containsKey key map = Map.containsKey key map.map

    let add key value { nextIndex = nextIndex; map = map; imap = imap } =
        let struct(index, nextIndex) =
            match Map.tryFind key map with
            | ValueNone -> struct(nextIndex, nextIndex + 1)
            | ValueSome iv -> iv.Key, nextIndex
        {
            nextIndex = nextIndex
            map = Map.add key (KeyValuePair(index, value)) map
            imap = Map.add index (KeyValuePair(key, value)) imap
        }

    let remove key omap =
        match Map.tryFind key omap.map with
        | ValueNone -> omap
        | ValueSome iv ->
            { omap with
                map = Map.remove key omap.map
                imap = Map.remove iv.Key omap.imap
            }

    /// `OMap.toListOrdered (OMap.ofList [1, "A"; 3, "C"; 1, "a"]) = [3, "C"; 1, "a"]`
    let toSeqOrdered omap = { imap = omap.imap } :> _ seq

    /// `OMap.toListSorted (OMap.ofList [1, "A"; 3, "C"; 1, "a"]) = [1, "a"; 3, "C"]`
    let toSeqSorted omap = { map = omap.map } :> _ seq

    let tryFind key omap =
        match Map.tryFind key omap.map with
        | ValueNone -> ValueNone
        | ValueSome kv -> ValueSome kv.Value

    let map mapping omap =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
        let map = omap.map |> Map.map (fun k iv -> KeyValuePair(iv.Key, mapping.Invoke(k, iv.Value)))
        let imap = omap.imap |> Map.map (fun _ kv -> KeyValuePair(kv.Key, let mutable kv = Map.find kv.Key map in kv.Value))
        { nextIndex = omap.nextIndex; map = map; imap = imap }

    let foldOrdered folder state omap =
        let folder = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt folder
        omap.imap |> Map.fold (fun state _ kv -> folder.Invoke(state, kv.Key, kv.Value)) state

    let ofList list =
        List.fold (fun map (k,v) -> add k v map) empty list

    let fromSeq source =
        Seq.fold (fun map (kv: KeyValuePair<_,_>) -> add kv.Key kv.Value map) empty source

    let ofSeq source =
        Seq.fold (fun map (k,v) -> add k v map) empty source

    /// `OMap.toListOrdered (OMap.ofList [1, "A"; 3, "C"; 1, "a"]) = [3, "C"; 1, "a"]`
    let toListOrdered map =
        if isEmpty map then [] else
        toSeqOrdered map |> Seq.map (|KeyValue|) |> Seq.toList

    /// `OMap.toListSorted (OMap.ofList [1, "A"; 3, "C"; 1, "a"]) = [1, "a"; 3, "C"]`
    let toListSorted map =
        if isEmpty map then [] else
        toSeqSorted map |> Seq.map (|KeyValue|) |> Seq.toList

    let ofMap map = Map.fold (fun map k v -> add k v map) empty map
    let toMap omap = omap.map |> Map.map (fun _ kv -> kv.Value)

type OMap<'K,'V> when 'K : comparison with
    member m.Item with get(k) =
        match OMap.tryFind k m with
        | ValueSome x -> x
        | _ -> raise <| KeyNotFoundException()
