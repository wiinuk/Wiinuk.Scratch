[<ReflectedDefinition>]
module Scratch.MemoryModel.Allocator
open Scratch
open Scratch.Operators
open Scratch.MemoryModel
open Scratch.MemoryModel.Operators


/// `0.30102999566398114`
let ln2_ln10s = log 2. / log 10.
let log2_int n = floor(log10 n / ln2_ln10s)

/// `32`
let freesLength = int (log2_int 4294967296. (* 0x100000000L *))

type BlockState =
    | Using
    | Free
with
    interface IWord with
        override x.Value = enumLikeUnionToV x

type Block = {
    bodySize: N
    prev: Block Pointer
    next: Block Pointer
    state: BlockState
    body: IWord Sequence
    blockSize: N
}
module Block =
    let bodySize = Field.defineRecordField <@ fun b -> b.bodySize @>
    let prev = Field.defineRecordField <@ fun b -> b.prev @>
    let next = Field.defineRecordField <@ fun b -> b.next @>
    let state = Field.defineRecordField <@ fun b -> b.state @>
    let body = Field.defineRecordField <@ fun b -> b.body @>

    let _headerSize = 4
    let _footerSize = 1

    /// impl `block + Block.$headerSize + heap[block + Block.bodySize]`
    let blockSize (block: Block Reference) = Field.unsafeOfNumber<Block, N>(_headerSize + (block->%bodySize))

let asBlock p = Reference.unsafeOfAddress<Block>(Reference.toAddress p + -Block._headerSize)

//type Heap = {
//    frees: Block Pointer Vector
//    block0: Block Sequence
//}
//
// freeList0
// |
// *
// [ free lists | blocks ]
//              *        *
//              |        |
//              block0   heapLimit

[<AutoOpen>]
module Diagnostics =
    let getHeapSize() = Memory.memoryEnd() - Memory.memoryBegin()
    let mutable totalBlockCount = 0
    let mutable totalFreeBlockSize = 0
    let mutable totalFreeBlockCount = 0

type FreeLists = Vector<Pointer<Block>>

let fillWord = Uninit
let mutable block0 = Nil

let mutable initialized = false
let initializeAllocator() =
    if not initialized then
        totalFreeBlockSize <- 0
        totalBlockCount <- 0
        totalFreeBlockCount <- 0

        Memory.clear()
        Memory.push (Word.size: Pointer<Block> Size)
        Memory.push (N (double freesLength))
        for _ in 1..freesLength do Memory.push (Nil: Pointer<Block>)
        block0 <- Pointer.unsafeOfAddress<Block> <| Memory.memoryEnd()
        initialized <- true

let initializeBlock block bodySize =
    block->*Block.bodySize <-% bodySize
    block->*Block.prev <-* Nil
    block->*Block.next <-* Nil
    block->*Block.state <-* Using
    block->*Block.blockSize(block) <-% bodySize + Block._headerSize + Block._footerSize

    totalBlockCount <- totalBlockCount + 1

let unlink node =

    // connect the prev and next nodes
    match node->.Block.next with
    | Nil -> ()
    | NonNil next -> next->*Block.prev <-* node->.Block.prev

    match node->.Block.prev with
    | Nil -> ()
    | NonNil prev -> prev->*Block.next <-* node->.Block.next

    // invalidate
    node->*Block.prev <-* Nil
    node->*Block.next <-* Nil

let unlinkFree block =
    assert (block->.Block.state = Free)

    let bodySize = block->%Block.bodySize
    match block->.Block.prev with
    | NonNil _ -> ()
    | Nil ->
        let next = block->.Block.next

        // unlink list head
        let freeLists = Reference.unsafeOfAddress<FreeLists>(Memory.memoryBegin())
        let freeIndex = int(log2_int(double bodySize))
        Vector.set freeLists freeIndex next

    unlink block
    block->*Block.state <-* Using

    totalFreeBlockSize <- totalFreeBlockSize - (bodySize + Block._headerSize + Block._footerSize)
    totalFreeBlockCount <- totalFreeBlockCount - 1

let popFree freeList =
    match Memory.read freeList with
    | Nil -> Nil
    | NonNil free ->
        unlinkFree free
        NonNil free

let getRightBlock block =
    let blockSize = block->%Block.bodySize + Block._headerSize + Block._footerSize
    let rightBlock = Reference.unsafeOfAddress<Block>(Reference.toAddress block + blockSize)
    if Reference.toAddress rightBlock < Memory.memoryEnd() then
        NonNil rightBlock
    else
        Nil

let mergeRightBlock block =
    assert (block->.Block.state = Using)

    match getRightBlock block with
    | Nil -> ()
    | NonNil rightBlock  ->
        assert (rightBlock->.Block.state = Using)

        let blockSize = block->%Block.bodySize + Block._headerSize + Block._footerSize
        let mergedBodySize = blockSize + (rightBlock->%Block.bodySize)

        rightBlock->*Block.blockSize(rightBlock) <-% (mergedBodySize + Block._headerSize + Block._footerSize)
        block->*Block.bodySize <-% mergedBodySize

        totalBlockCount <- totalBlockCount - 1

let split (block: Reference<_>) leftBodySize =
    assert (block->.Block.prev = Nil)
    assert (block->.Block.next = Nil)
    assert (block->.Block.state = Using)

    // [?,?,?,[...                ],?]
    // [?,?,?,[...],?],[?,?,?,[...],?]
    let rightBodySize = block->%Block.bodySize - leftBodySize - Block._headerSize - Block._footerSize

    if rightBodySize < 1 then
        // not splittable
        Nil
    else
        block->*Block.bodySize <-% leftBodySize
        block->*Block.blockSize block <-% (leftBodySize + Block._headerSize + Block._footerSize)

        let rightBlock = Reference.unsafeOfAddress<Block>(Reference.toAddress block + (Block._headerSize + leftBodySize + Block._footerSize))
        initializeBlock rightBlock rightBodySize
        NonNil rightBlock

let fillObject block =
    assert (block->.Block.state = Free)

    let bodySize = block->%Block.bodySize
    let mutable p = Reference.toAddress(Field.reference block Block.body)
    for _ in 1..bodySize do
        Memory.write (Reference.unsafeOfAddress<Uninit> p) fillWord
        p <- p + 1

let pushFree block =
    assert (block->.Block.state = Using)
    assert (block->.Block.prev = Nil)
    assert (block->.Block.next = Nil)

    let bodySize = block->%Block.bodySize
    let freeIndex = int(log2_int(double bodySize))
    let freeLists = Reference.unsafeOfAddress<FreeLists>(Memory.memoryBegin())
    let freeList = Vector.reference freeLists freeIndex
    let freeHead = Memory.read freeList
    match freeHead with
    | Nil -> ()
    | NonNil freeHead ->
        assert (freeHead->.Block.prev = Nil)
        freeHead->*Block.prev <-* NonNil block

    block->*Block.state <-* Free
    block->*Block.next <-* freeHead
    Memory.write freeList (NonNil block)
    fillObject block

    totalFreeBlockSize <- totalFreeBlockSize + (bodySize + Block._headerSize + Block._footerSize)
    totalFreeBlockCount <- totalFreeBlockCount + 1

let getLeftBlock block =
    if 0 < Reference.toAddress block - Pointer.toAddress block0 then
        let leftBlockSize = %(Memory.read (Reference.unsafeOfAddress<N>(Reference.toAddress block + -Block._footerSize)))
        NonNil(Reference.unsafeOfAddress<Block>(Reference.toAddress block + -leftBlockSize))
    else
        Nil

let tryMergeBlock block =
    assert (block->.Block.state = Using)

    let block =
        // ?, [using, ...], ?
        match getLeftBlock block with
        | NonNil leftBlock when leftBlock->.Block.state = Free ->
            // [free, ...], [using, ...], ?
            unlinkFree leftBlock

            // [using, ...], [using, ...], ?
            mergeRightBlock leftBlock
            // [using, ...             ], ?
            leftBlock
        | _ -> block

    // [using, ...], ?
    match getRightBlock block with
    | NonNil rightBlock when rightBlock->.Block.state = Free ->
        // [using, ...], [free, ...]
        unlinkFree rightBlock

        // [using, ...], [using, ...]
        mergeRightBlock block
        // [using, ...             ]
    | _ -> ()

    block

let free object =
    let block = asBlock object
    if block->.Block.state = Free then failwithf "double free" else

    assert (Nil = block->.Block.prev)
    assert (Nil = block->.Block.next)

    let block = tryMergeBlock block
    pushFree block

let extend minBodySize =
    let minBlockSize = minBodySize + Block._headerSize + Block._footerSize

    // allocate
    let newBlock = Reference.unsafeOfAddress<Block>(Memory.memoryEnd())
    let newBlockSize = Memory.memoryEnd() - Pointer.toAddress block0
    let newBlockSize = if newBlockSize < minBlockSize then minBlockSize else newBlockSize
    for _ in 1..newBlockSize do Memory.push fillWord

    initializeBlock newBlock (newBlockSize - Block._headerSize - Block._footerSize)

    free(Field.reference newBlock Block.body)


let allocFromFreeLargerBlock size largerBlock =
    assert (size < largerBlock->%Block.bodySize)
    assert (largerBlock->.Block.prev = Nil)
    assert (largerBlock->.Block.state = Using)

    match split largerBlock size with
    | Nil ->

        // 分けることができる最小サイズより小さかったので、大きいブロックをそのまま使う
        Field.reference largerBlock Block.body

    | NonNil rightBlock ->

        // 分けたうちの右ブロックを解放
        free(Field.reference rightBlock Block.body)

        // 左ブロックを使う
        Field.reference largerBlock Block.body

// bins
// [0]: 1      ( 0b1 )
// [1]: 2..3   ( 0b10..0b11 )
// [2]: 4..7   ( 0b100..0b111 )
// [3]: 8..15  ( 0b1000..0b1111 )
//
// good 0 = 0
// good 1 = 0
// good s = log2(s - 1) + 1
//
// good 2 = [1]: 2..3
// good 3 = [2]: 4..7
// good 4 = [2]: 4..7
// good 5 = [3]: 8..15
// good 6 = [3]: 8..15
// good 7 = [3]: 8..15
let getGoodIndex bodySize =
    if bodySize < 2 then 0
    else int(log2_int(double(bodySize - 1))) + 1

let mallocNoExtend size =
    if not initialized then initializeAllocator()

    // goodfit な未使用ブロックリストを探す
    let freeLists = Reference.unsafeOfAddress<FreeLists>(Memory.memoryBegin())
    let freeIndex = getGoodIndex size

    // 未使用ブロックリストからブロックを取得
    match popFree(Vector.reference freeLists freeIndex) with

    // ブロックが無かった
    | Nil ->

        // より大きな未使用ブロックリストからブロックを検索
        let mutable largerIndex = freeIndex + 1
        let mutable largerHead = Nil
        while not (largerHead <> Nil || freesLength <= largerIndex) do
            largerHead <- popFree(Vector.reference freeLists largerIndex)
            largerIndex <- largerIndex + 1

        match largerHead with

        // より大きなブロックも無かった
        | Nil -> Nil

        // 見つかった、より大きなブロックを分割して返す
        | NonNil largerHead ->
            NonNil(allocFromFreeLargerBlock size largerHead)

    // 未使用ブロックがあった
    | NonNil freeHead ->
        assert (size <= freeHead->%Block.bodySize)
        assert (Nil = freeHead->.Block.prev)

        NonNil(Field.reference freeHead Block.body)

let rec mallocExtend size =
    match mallocNoExtend size with
    | NonNil p -> p
    | Nil ->
        // ヒープを拡張して、再度確保
        extend size
        mallocExtend size

let mallocVectorExtend size count =
    let mutable count = count
    if count < 0 then count <- 0

    let v = mallocExtend (Vector.``$headerSize`` + Size.toNumber size * count)
    let v = Reference.unsafeOfAddress<_ Vector>(Reference.toAddress v)
    v->*Vector.elementSize <-* size
    v->*Vector.count <-% count
    v

[<ReflectedDefinition>]
let allocate() =
    Size.toNumber Size.typeSize<'a>
    |> mallocExtend
    |> Reference.toAddress
    |> Reference.unsafeOfAddress<'a>
