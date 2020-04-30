namespace Scratch.MemoryModel
open Scratch
open Scratch.MemoryModel
open Scratch.MemoryModel.Operators
open Scratch.Operators
open Scratch.Primitives

[<Struct>]
type 'T Vector = {
    elementSize: 'T Size
    count: N
    item0: 'T Sequence
}
module Vector =
    [<ReflectedDefinition>]
    let elementSize<'a> = Field.unsafeOfNumber<'a Vector, 'a Size> 0
    [<ReflectedDefinition>]
    let count<'a> = Field.unsafeOfNumber<'a Vector, N> 1
    [<ReflectedDefinition>]
    let item0<'a> = Field.unsafeOfNumber<'a Vector, 'a Sequence> 2
    [<ReflectedDefinition>]
    let ``$headerSize`` = 2

    [<ReflectedDefinition>]
    let reference v i =
        if i < 0 || v->%count <= i then failwithf "Vector.reference (v = %A) (i = %A): error IndexOutOfRange" v i
        Sequence.unsafeReference (Field.reference v item0) i (Memory.get v elementSize)

    [<ReflectedDefinition>]
    let blit<'a> (source: 'a Vector Reference) sourceIndex (target: 'a Vector Reference) targetIndex copyCount =
        if source->%count < sourceIndex + copyCount then failwithf "Vector.blit: error (source.count < sourceIndex + copyCount)" else
        if target->%count < targetIndex + copyCount then failwithf "Vector.blit: error (target.count < targetIndex + copyCount)" else
        if copyCount < 0 then failwithf "Vector.blit: error (copyCount < 0)" else

        if copyCount = 0 then () else
        let p1 = Reference.toAddress (reference source sourceIndex)
        let p2 = Reference.toAddress (reference target targetIndex)
        Memory.copy p1 p2 (Size.toNumber (Memory.get source elementSize) * copyCount)

    [<ReflectedDefinition>]
    let copy<'a> (v1: 'a Vector Reference) (v2: 'a Vector Reference) copyCount = blit v1 0 v2 0 copyCount

    [<ReflectedDefinition>]
    let get v i = Memory.read (reference v i)
    [<ReflectedDefinition>]
    let set v i x = Memory.write (reference v i) x

    [<ReflectedDefinition>]
    let iter (action: 'F when 'F :> IFunc<_,_> and 'F : not struct) v =
        let elementSize = Memory.get v elementSize
        for i in 0..int (Memory.get v count) - 1 do
            let (HUnit.HUnit) = action.Invoke(Sequence.unsafeReference (Field.reference v item0) i elementSize)
            ()

    [<ReflectedDefinition>]
    let iteri (action: 'F when 'F :> IFunc<_,_> and 'F : not struct) v =
        let elementSize = Memory.get v elementSize
        for i in 0..int (Memory.get v count) - 1 do
            let (HUnit.HUnit) = action.Invoke struct(Sequence.unsafeReference (Field.reference v item0) i elementSize, i)
            ()

    [<ReflectedDefinition>]
    let iterAsync (action: 'F when 'F :> IFunc<_,_> and 'F : not struct) v = tightrope {
        let elementSize = Memory.get v elementSize
        let mutable i = 0
        do! repeatAsync (int (Memory.get v count)) {
            let! (HUnit.HUnit) = action.Invoke(Sequence.unsafeReference (Field.reference v item0) i elementSize)
            i <- i + 1
        }
    }

    [<ReflectedDefinition>]
    let iteriAsync (action: 'F when 'F :> IFunc<_,_> and 'F : not struct) v = tightrope {
        let elementSize = Memory.get v elementSize
        let mutable i = 0
        do! repeatAsync (int (Memory.get v count)) {
            let! (HUnit.HUnit) = action.Invoke struct(Sequence.unsafeReference (Field.reference v item0) i elementSize, i)
            i <- i + 1
        }
    }
    [<ReflectedDefinition>]
    let foldAsync (folder: 'F when 'F :> IFunc<_,_> and 'F : not struct) state vector = tightrope {
        let mutable state = state
        let mutable i = 0
        do! repeatAsync (vector->%count) {
            let! s = folder.Invoke struct(state, reference vector i)
            state <- s
            i <- i + 1
        }
        return state
    }
    [<ReflectedDefinition>]
    let fold (folder: 'F when 'F :> IFunc<_,_> and 'F : not struct) state vector =
        let mutable state = state
        for i in 0 .. vector->%count - 1 do
            state <- folder.Invoke struct(state, reference vector i)
        state

    [<ReflectedDefinition>]
    let private median (compare: 'F when 'F :> IFunc<_,_> and 'F : not struct) a b c =
        let mutable a = a
        let mutable b = b
        let mutable c = c
        let mutable t = a
        if compare.Invoke struct(a, b) > 0 then (t <- a; a <- b; b <- t)
        if compare.Invoke struct(a, c) > 0 then (t <- a; a <- c; c <- t)
        if compare.Invoke struct(b, c) > 0 then (t <- b; b <- c; c <- t)
        b

    [<ReflectedDefinition>]
    let private insertionSort (compare: 'F when 'F :> IFunc<_,_> and 'F : not struct) v first last =
        for i = first + 1 to last do
            let mutable j = i
            repeatUntil (fun _ -> j <= first || compare.Invoke struct(reference v (j - 1), reference v j) <= 0) <| fun _ ->
                Reference.swap (reference v j) (reference v (j - 1))
                j <- j - 1

    [<ReflectedDefinition>]
    let rec private quickSort compare v first last =
        if last - first < 64 then insertionSort compare v first last else

        let pivot = median compare (reference v first) (reference v ((first + last) / 2)) (reference v last)
        let mutable l = first
        let mutable r = last
        while
            begin
                if l <= r then
                    while l < last && compare.Invoke struct(reference v l, pivot) < 0 do l <- l + 1
                    while r > first && compare.Invoke struct(reference v r, pivot) >= 0 do r <- r - 1
                    l <= r
                else
                    false
            end
            do
                Reference.swap (reference v l) (reference v r)
                l <- l + 1
                r <- r - 1

        quickSort compare v first (l - 1)
        quickSort compare v l last

    [<ReflectedDefinition>]
    let sort compare vector = quickSort compare vector 0 (vector->%count - 1)
