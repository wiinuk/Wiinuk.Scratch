[<AutoOpen>]
module internal Scratch.Json.Utf8.CollectionOperations
open System.Collections.Generic
open Scratch.Primitives


type ICollectionOperations<'TCollection,'TItem,'TBuilder,'TIterator> =
    abstract Create: unit -> 'TBuilder
    abstract Add: builder: 'TBuilder byref * item: 'TItem -> unit
    abstract Complete: builder: 'TBuilder byref -> 'TCollection

    abstract Iterator: collection: 'TCollection -> 'TIterator
    abstract Next: iterator: 'TIterator byref * result: 'TItem outref -> bool

[<Struct>]
type 'T ListOps = | ListOps with
    interface ICollectionOperations<'T list,'T,'T ResizeArray,'T list> with
        member _.Create() = null
        member _.Add(xs, x) =
            match xs with
            | null -> xs <- ResizeArray 256
            | _ -> ()
            xs.Add x

        member _.Complete xs =
            match xs with
            | null -> []
            | _ ->

            let mutable list = []
            for i = xs.Count-1 downto 0 do
                list <- xs[i]::list
            list

        member _.Iterator xs = xs
        member _.Next(it, result) =
            match it with
            | [] -> false
            | x::xs -> result <- x; it <- xs; true

type SeqOps<'c,'a> = ICollectionOperations<'c,'a,'a ResizeArray,'a IEnumerator>

[<Struct>]
type MapOps<'K,'V> when 'K : comparison = | MapOps with
    interface SeqOps<Map<'K,'V>, KeyValuePair<'K,'V>> with
        member _.Create() = null
        member _.Add(b, kv) =
            match b with
            | null -> b <- ResizeArray 256
            | _ -> ()
            b.Add kv

        member _.Complete b =
            match b with
            | null -> Map.empty
            | b ->

            let mutable e = b.GetEnumerator()
            let mutable map = Map.empty
            while e.MoveNext() do
                let mutable kv = e.Current
                map <- Map.add kv.Key kv.Value map
            map

        member _.Iterator map = (map :> _ seq).GetEnumerator()
        member _.Next(it, result) = it.MoveNext() && (result <- it.Current; true)

[<Struct>]
type IArrayOps<'a> = | IArrayOps with
    interface ICollectionOperations<'a ImmutableArray, 'a, 'a ImmutableArray.Builder, 'a ImmutableArray.Enumerator> with
        member _.Create() = IArray.createBuilder()
        member _.Add(b, kv) = IArrayBuilder.add &b kv
        member _.Complete b = IArrayBuilder.toImmutable &b

        member _.Iterator kvs = kvs.GetInEnumerator()
        member _.Next(it, result) = it.MoveNext() && (result <- it.Current; true)

let tryReadItem (s: _ byref) (r: _ byref) (c: _ byref) (result: _ outref) =
    if c.count <> 0 && not <| readIsValueSeparator &r then false else // }

    // ,
    c.count <- c.count + 1
    let r' = r
    if tryDeserialize &s &r &result then true else // , item

    // item deserialize failure.
    // restore to ','.
    r <- r'
    false
