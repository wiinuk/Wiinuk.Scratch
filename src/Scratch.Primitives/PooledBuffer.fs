namespace Scratch.Primitives
open System
open System.Collections.Generic
open System.Runtime.CompilerServices


type private BufferBox<'T> = private { mutable buffer: 'T Buffer }
[<Struct>]
type PooledBuffer<'T,'S> = private { box: BufferBox<'T> }

type [<AbstractClass; Sealed>] private ThreadStaticBufferPoolHolder<'T,'S> private () =
    [<ThreadStatic; DefaultValue>]
    static val mutable private value: PooledBuffer<'T,'S> Stack

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    static member initialize() =
        let x = Stack()
        ThreadStaticBufferPoolHolder<'T,'S>.value <- x
        x

    static member Value with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () =
        match ThreadStaticBufferPoolHolder<'T,'S>.value with
        | null -> ThreadStaticBufferPoolHolder<'T,'S>.initialize()
        | x -> x

type IPooledBufferUser<'T,'R> =
    inherit IHigherKind
    abstract Using: PooledBuffer<'T,'S> -> 'R

module PooledBuffer =
    let add buffer x = Buffer.add &buffer.box.buffer x
    let count buffer = Buffer.count &buffer.box.buffer
    let isEmpty buffer = Buffer.isEmpty &buffer.box.buffer
    let item index buffer = Buffer.item index &buffer.box.buffer
    let clear buffer = Buffer.clear &buffer.box.buffer

    let foldV (folder: _ byref) initialState buffer = Buffer.foldV &folder initialState &buffer.box.buffer
    let tryPickV (chooser: _ byref) buffer = Buffer.tryPickV &chooser &buffer.box.buffer

    let addBuffer buffer values =
        for i in 0..count values-1 do
            add buffer (item i values)

    let appendToList buffer initialList = Buffer.appendToList &buffer.box.buffer initialList
    let toList buffer = appendToList buffer []

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private createPooledBuffer() = { box = { buffer = Buffer.create() } }
    type private PrivateType = struct end
    let using (action: 'f byref when 'f :> IPooledBufferUser<_,_> and 'f : struct) =
        let stack = ThreadStaticBufferPoolHolder<_,PrivateType>.Value
        let p =
            if stack.Count = 0
            then createPooledBuffer()
            else stack.Pop()

        try action.Using p
        finally
            Buffer.clear &p.box.buffer
            stack.Push p

    let uncheckedEscape buffer = { box = buffer.box }
    let last buffer = item (count buffer - 1) buffer
    let foldBackIndexV (folder: _ byref) buffer initialIndex initialState = Buffer.foldBackIndexV &folder &buffer.box.buffer initialIndex initialState
