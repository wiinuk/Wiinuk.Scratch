namespace Scratch.Primitives
open System
open System.Runtime.CompilerServices


[<Struct>]
type Buffer<'T> = private {
    /// allow null
    mutable items: 'T[]
    mutable size: int
}

/// non thread safe operations on Buffers.
module Buffer =
    let private defaultBufferSize = 4
    let private maxArrayLength = 0x7FEFFFFF

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private raiseOutOfRangeError() = raise <| IndexOutOfRangeException()

    let create() = { Buffer.items = null; size = 0 }

    let count (b: _ inref) = b.size
    let isEmpty (b: _ inref) = count &b = 0

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let item i (b: _ inref) = if b.size <= i then raiseOutOfRangeError() else b.items.[i]

    let clear (b: 'T Buffer byref) =
        if not (RuntimeHelpers.IsReferenceOrContainsReferences<'T>()) then b.size <- 0 else

        let size = b.size
        b.size <- 0
        if 0 < size then
            Array.Clear(b.items, 0, size)

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private addRare (b: _ byref) x =
        if isNull b.items then b.items <- Array.zeroCreate defaultBufferSize

        let size = b.size
        let newSize = max (b.items.Length * 2) defaultBufferSize
        let newSize = min newSize maxArrayLength
        let newBuffer = Array.zeroCreate newSize
        Array.Copy(b.items, 0, newBuffer, 0, b.size)
        b.items <- newBuffer

        b.size <- size + 1
        b.items.[size] <- x

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let add (b: _ byref) x =
        let array = b.items
        let size = b.size
        if not (isNull array) && uint32 size < uint32 array.Length then
            b.size <- size + 1
            array.[size] <- x
        else
            addRare &b x

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let toArray (b: _ inref) =
        match b.items with
        | null -> Array.empty
        | buffer ->
            let items = Array.zeroCreate b.size
            Array.Copy(buffer, 0, items, 0, items.Length)
            items

    let appendToList (b: _ inref) initialList =
        let mutable list = initialList
        let mutable i = count &b - 1
        while 0 <= i do
            list <- item i &b::list
            i <- i - 1
        list

    let toList (b: _ inref) = appendToList &b []

    let foldV (folder: _ byref) initialState (b: _ inref) =
        let mutable state = initialState
        let mutable i = 0

        // b の状態が folder によって変更される場合があるので、ループの度にチェックする必要がある
        while i < b.size do
            state <- Func.invoke &folder struct(state, b.items.[i])
            i <- i + 1
        state

    let tryPickV (chooser: _ byref) (b: _ inref) =
        let mutable result = ValueNone
        let mutable i = 0

        // b の状態が folder によって変更される場合があるので、ループの度にチェックする必要がある
        while
            if i < b.size then
                match Func.invoke &chooser b.items.[i] with
                | ValueNone -> true
                | ValueSome _ as r -> result <- r; false
            else
                false
            do i <- i + 1
        result
    let foldBackIndexV (folder: _ byref) (b: _ inref) index initialState =
        let mutable state = initialState
        let mutable i = index
        while 0 <= i do
            state <- Func.invoke &folder struct(b.items.[i], state)
            i <- i - 1
        state
