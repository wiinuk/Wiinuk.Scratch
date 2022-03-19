namespace Scratch.Primitives
open System
open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices

[<Struct>]
type Index<'T> = { index: int } with
    static member (+) (x: 'T Index, i): 'T Index = { index = x.index + i }
    static member (.+.) (x: 'T Index, y: 'T Index): 'T Index = { index = x.index + y.index }
    static member (-) (x: 'T Index, i): 'T Index = { index = x.index - i }
    static member (.-.) (x: 'T Index, y: 'T Index) = x.index - y.index

type index<'T> = 'T Index

[<Extension; AbstractClass; Sealed>]
type ArrayExtensions =
    [<Extension>]
    static member Address(xs: 'T array, i: 'T index) = &xs[i.index]

[<Extension; AbstractClass; Sealed>]
type ListExtensions =
    [<Extension>]
    static member Get(xs: 'T ResizeArray, i: 'T index) = xs[i.index]
    [<Extension>]
    static member Set(xs: 'T ResizeArray, i: 'T index, value) = xs[i.index] <- value
    [<Extension>]
    static member NextIndex(xs: 'T ResizeArray): 'T index = { index = xs.Count }

module Index =
    let isZero p = p.index = 0
    let zero = { index = 0 }
    [<RequiresExplicitTypeArguments>]
    let create<'T> index: 'T Index = { index = index }

[<Struct; RequireQualifiedAccess>]
type iarray_seq_enumerator<'T> = private { array: 'T[]; mutable i: int } with
    member x.MoveNext() =
        x.i <- x.i + 1
        uint32 x.i < uint32 x.array.Length

    member x.Current: 'T inref = &x.array[x.i]
    interface 'T IEnumerator with
        member x.MoveNext() = x.MoveNext()
        member x.Current: 'T = x.Current
        member x.Current = x.Current :> obj
        member _.Reset() = raise <| NotImplementedException()
        member _.Dispose() = ()

type IStrongReadonlyArray<'T,'TEnumerator> =
    abstract Length: int
    /// <exception cref="System.IndexOutOfRangeException"></exception>
    abstract Address: index: int -> 'T inref
    abstract UncheckedInternalArray: 'T[]
    abstract GetEnumerator: unit -> 'TEnumerator

[<Struct; RequireQualifiedAccess>]
type iarray_seq<'T> = private { source: 'T[] } with
    member x.GetEnumerator() = { iarray_seq_enumerator.array = x.source; iarray_seq_enumerator.i = -1 }
    interface 'T seq with
        member x.GetEnumerator() = x.GetEnumerator() :> _ IEnumerator
        member x.GetEnumerator() = x.GetEnumerator() :> IEnumerator

    interface IStrongReadonlyArray<'T,'T iarray_seq_enumerator> with
        member x.Length = x.source.Length
        member x.Address i: _ inref = &x.source[i]
        member x.GetEnumerator() = x.GetEnumerator()
        member x.UncheckedInternalArray = x.source

[<Struct; RequireQualifiedAccess>]
type iarray_enumerator<'T> = private { array: 'T[]; mutable i: int } with
    member x.MoveNext() =
        x.i <- x.i + 1
        uint32 x.i < uint32 x.array.Length

    member x.Current: _ inref = &x.array[x.i]

[<Struct>]
type ImmutableArray<'T> = private { items: 'T[] } with
    member x.Address(i: 'T index): 'T inref = &x.items[i.index]
    member x.GetInEnumerator() = { iarray_enumerator.array = x.items; iarray_enumerator.i = -1 }
    interface IStrongReadonlyArray<'T,'T iarray_enumerator> with
        member x.Length = x.items.Length
        member x.Address i: _ inref = &x.items[i]
        member x.GetEnumerator() = x.GetInEnumerator()
        member x.UncheckedInternalArray = x.items

type iarray<'T> = 'T ImmutableArray

[<Struct>]
type iarray_builder<'T> = private {
    mutable buffer: Buffer<'T>
}

module ImmutableArray =
    type Builder<'T> = 'T iarray_builder
    type Enumerator<'T> = 'T iarray_enumerator

module IArray =
    module Unchecked =
        let wrapArray items = { items = items }
        let unwrapArray (xs: 'A when 'A :> #IStrongReadonlyArray<_,_> and 'A : struct) = xs.UncheckedInternalArray

    let empty = { items = Array.empty }

    let create xs = { items = Array.copy xs }
    let toArray xs = Array.copy (Unchecked.unwrapArray xs)

    let toResizeArray xs = ResizeArray(Unchecked.unwrapArray xs)

    let length (xs: 'A when 'A :> #IStrongReadonlyArray<_,_> and 'A : struct) = xs.Length
    let ref index (xs: 'A when 'A :> #IStrongReadonlyArray<_,_> and 'A : struct) = &xs.Address index
    let address (index: 'T index) xs: 'T inref = let i = index.index in &(ref i xs)
    let get xs (index: 'T index when 'T : not struct) = address index xs
    let getCopiable xs index = address index xs
    let item index xs: 'T when 'T : not struct = ref index xs
    let head xs = item 0 xs

    let inline mapToArray ([<InlineIfLambda>] mapping) xs =
        let result = Array.zeroCreate (length xs)
        for i = 0 to length xs - 1 do
            result[i] <- mapping (xs.Address i)
        result

    let toSeqCopiable xs = { iarray_seq.source = Unchecked.unwrapArray xs }
    let toSeq xs: 'a iarray_seq when 'a: not struct = { iarray_seq.source = Unchecked.unwrapArray xs }

    let ofICollection (xs: #ICollection<_>) =
        let items = Array.zeroCreate xs.Count
        xs.CopyTo(items, arrayIndex = 0)
        { items = items }

    let ofSeq xs = { items = Seq.toArray xs }

    let isEmpty xs = length xs = 0
    let inline map ([<InlineIfLambda>] mapping) xs =
        if isEmpty xs then empty else

        let items = Unchecked.unwrapArray xs
        let items2 = Array.zeroCreate items.Length
        for i = 0 to items.Length do
            items2[i] <- mapping items[i]
        Unchecked.wrapArray items2
        
    let inline fold ([<InlineIfLambda>] folder) state (xs: 'A when 'A :> #IStrongReadonlyArray<_,'e> and 'A : struct and 'e :> IEnumerator<_>) =
        let mutable state = state
        use mutable e = xs.GetEnumerator()
        while e.MoveNext() do
            state <- folder state e.Current
        state

    let inline findIndex ([<InlineIfLambda>] predicate) xs =
        let mutable i, result = 0, ValueNone
        while
            if length xs < i then
                if predicate (ref i xs) then
                    result <- ValueSome i
                    false
                else
                    true
            else
                false
            do i <- i + 1
        result

    let createBuilder() = { iarray_builder.buffer = Buffer.create() }

module IArrayBuilder =
    let add (b: _ byref) x = Buffer.add &b.buffer x
    let toImmutable (b: _ byref) = { iarray.items = Buffer.toArray &b.buffer }

[<Struct>]
type private MutablePair<'K,'V> = { mutable key: 'K; mutable value: 'V }

[<Struct>]
type IndexMap<'K,'T> = private { sortedItems: MutablePair<'K index,'T>[] }

module IndexMap =
    let ofMap (xs: Map<_,_>) =
        let buffer = ResizeArray()
        for kv in xs do
            buffer.Add { key = kv.Key; value = kv.Value }
        { sortedItems = buffer.ToArray() }

    let tryGetValue (key: 'k index) ({ sortedItems = array }: IndexMap<'k,_>) (result: _ outref) =
        let mutable low = 0
        let mutable high = array.Length - 1
        let mutable success = false
        while
            if low <= high then
                let i = low + (high - low >>> 1)
                let kv = &array[i]
                let key' = kv.key
                if key.index = key'.index then
                    result <- kv.value
                    success <- true
                    false
                else

                if key'.index < key.index then
                    low <- i + 1
                else
                    high <- i - 1
                true
            else
                false
            do ()
        success

    let tryFind key map =
        let mutable result = Unchecked.defaultof<_>
        if tryGetValue key map &result then ValueSome result
        else ValueNone

    let toSeq xs =
        xs.sortedItems
        |> Seq.map (fun kv -> struct(kv.key, kv.value))
