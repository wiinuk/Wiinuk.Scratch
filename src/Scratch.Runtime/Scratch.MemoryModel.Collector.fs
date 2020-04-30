[<ReflectedDefinition>]
module Scratch.MemoryModel.Collector
open Scratch.Operators
open Scratch.MemoryModel.Operators
module A = Allocator


type Color =
    | White
    | Gray
    | Black
    | Purple
with
    interface IWord with
        override x.Value = enumLikeUnionToV x

type RcObject = | RcObject
type RcHeader = {
    rc: N
    color: Color
    pointers: N Vector Reference
    candidatesPrev: RcHeader Pointer
    candidatesNext: RcHeader Pointer
    // mutable body: RcObject
}
module Rc =
    let rc = Field.defineRecordField <@ fun x -> x.rc @>
    let color = Field.defineRecordField <@ fun x -> x.color @>
    let pointers = Field.defineRecordField <@ fun x -> x.pointers @>
    let candidatesPrev = Field.defineRecordField <@ fun x -> x.candidatesPrev @>
    let candidatesNext = Field.defineRecordField <@ fun x -> x.candidatesNext @>
    let _headerSize = 5
    // let body = Member.defineReadWrite <@ fun x v -> x.body <- v @>

[<AutoOpen>]
module Diagnostics =
    let mutable totalObjectCount = 0
    let mutable totalReferenceCount = 0
    let mutable totalCandidatesCount = 0

let mutable candidates = Nil

let mutable initialized = false
let initializeCollector() =
    if not initialized then
        totalObjectCount <- 0
        totalReferenceCount <- 0
        totalCandidatesCount <- 0
        candidates <- Nil
        initialized <- true

let getHeader (p: Reference<RcObject>) = Reference.unsafeOfAddress<RcHeader>(Reference.toAddress p + -Rc._headerSize)

let child p i =
    let p = Reference.unsafeOfAddress<Pointer<RcObject>>(Reference.toAddress p + Rc._headerSize + %(Vector.get (p->.Rc.pointers) i))
    match Memory.read p with
    | Nil -> Nil
    | NonNil c -> NonNil(getHeader c)

let rec markGray p =
    if p->.Rc.color <> Gray then
        p->*Rc.color <-* Gray

        let mutable i = 0
        for _ in 1..p->.Rc.pointers->%Vector.count do
            match child p i with
            | Nil -> ()
            | NonNil child ->

                // 試験的削除
                child->*Rc.rc <-% child->%Rc.rc - 1

                totalReferenceCount <- totalReferenceCount-1

                markGray child
            i <- i + 1

let remove c cs =
    let prev = c->.Rc.candidatesPrev
    let next = c->.Rc.candidatesNext

    match prev with
    | NonNil prev -> prev->*Rc.candidatesNext <-* next
    | _ -> ()

    match next with
    | NonNil next -> next->*Rc.candidatesPrev <-* prev
    | _ -> ()

    c->*Rc.candidatesPrev <-* Nil
    c->*Rc.candidatesNext <-* Nil

    if cs = NonNil c then next else cs

let contains c cs =
    NonNil c = cs || c->.Rc.candidatesPrev <> Nil || c->.Rc.candidatesNext <> Nil

let isEmpty: RcHeader Pointer -> bool = function Nil -> true | _ -> false
let minElement: RcHeader Pointer -> RcHeader Reference = function
    | Nil -> failwithf "empty"
    | NonNil c -> c

let add c cs =
    if contains c cs then cs else

    match cs with
    | NonNil cs -> cs->*Rc.candidatesPrev <-* NonNil c
    | Nil -> ()

    c->*Rc.candidatesNext <-* cs
    NonNil c

let markCandidates() =
    let mutable cs = candidates
    let mutable stop = false
    while not stop do
        match cs with
        | Nil -> stop <- true
        | NonNil c ->
            cs <- c->.Rc.candidatesNext

            if c->.Rc.color = Purple then
                markGray c
            else
                candidates <- remove c candidates
                totalCandidatesCount <- totalCandidatesCount-1

                if c->.Rc.color = Black && c->%Rc.rc = 0 then
                    A.free c
                    totalObjectCount <- totalObjectCount-1

let rec scanBlack p =
    p->*Rc.color <-* Black

    let mutable i = 0
    for _ in 1..p->.Rc.pointers->%Vector.count do
        match child p i with
        | Nil -> ()
        | NonNil child ->

            // フリーリストのオブジェクトは黒
            child->*Rc.rc <-% child->%Rc.rc + 1
            totalReferenceCount <- totalReferenceCount+1

            if child->.Rc.color <> Black then
                scanBlack child
        i <- i + 1

let rec scan p =
    if p->.Rc.color = Gray then
        if 0 < p->%Rc.rc then
            // 外部からの参照がある
            scanBlack p
        else
            // ゴミのようである
            p->*Rc.color <-* White

            let mutable i = 0
            for _ in 1..p->.Rc.pointers->%Vector.count do
                // 処理を続ける
                match child p i with
                | Nil -> ()
                | NonNil child -> scan child
                i <- i + 1

let rec collectWhite p =
    if p->.Rc.color = White && not (contains p candidates) then

        // フリーリストのオブジェクトは黒
        p->*Rc.color <-* Black

        let mutable i = 0
        for _ in 1..p->.Rc.pointers->%Vector.count do
            match child p i with
            | Nil -> ()
            | NonNil child -> collectWhite child
            i <- i + 1

        totalReferenceCount <- totalReferenceCount - (p->%Rc.rc)
        A.free p
        totalObjectCount <- totalObjectCount-1

let collectCandidates() =
    while not (isEmpty candidates) do
        let p = minElement candidates
        candidates <- remove p candidates
        totalCandidatesCount <- totalCandidatesCount-1
        collectWhite p

let collect() =
    markCandidates()
    let mutable cs = candidates
    let mutable stop = false
    while not stop do
        match cs with
        | Nil -> stop <- true
        | NonNil c ->
            scan c
            cs <- c->.Rc.candidatesNext
    collectCandidates()

let newRc pointers size =
    if not initialized then initializeCollector()

    let p =
        match A.mallocNoExtend (size + Rc._headerSize) with
        | NonNil p -> p
        | Nil ->

            // 循環ゴミを集める
            collect()

            A.mallocExtend (size + Rc._headerSize)
    totalObjectCount <- totalObjectCount+1

    let p = Reference.unsafeOfAddress<RcHeader>(Reference.toAddress p)
    p->*Rc.rc <-% 0
    p->*Rc.color <-* White
    p->*Rc.pointers <-* pointers
    p->*Rc.candidatesPrev <-* Nil
    p->*Rc.candidatesNext <-* Nil

    Reference.unsafeOfAddress<RcObject>(Reference.toAddress p + Rc._headerSize)

let addReference = function
    | Nil -> ()
    | NonNil p ->
        let p = getHeader p
        p->*Rc.rc <-% p->%Rc.rc + 1

        totalReferenceCount <- totalReferenceCount+1

        // 循環ゴミの内部ではない
        p->*Rc.color <-* Black

let candidate p =
    if p->.Rc.color <> Purple then
        p->*Rc.color <-* Purple
        candidates <- add p candidates
        totalCandidatesCount <- totalCandidatesCount+1

let rec release p =
    let ps = p->.Rc.pointers
    let mutable i = 0
    for _ in 1..ps->%Vector.count do
        let p = Reference.toAddress p + Rc._headerSize + %(Vector.get ps i)
        let pointer = Memory.read (Reference.unsafeOfAddress<RcObject Pointer> p)
        deleteReference pointer
        i <- i + 1

    // フリーリスト内のオブジェクトは黒
    p->*Rc.color <-* Black

    // 候補は後で処理する
    if not (contains p candidates) then
        totalReferenceCount <- totalReferenceCount - (p->%Rc.rc)
        A.free p
        totalObjectCount <- totalObjectCount-1

and deleteReference = function
    | Nil -> ()
    | NonNil p ->
        let p = getHeader p
        p->*Rc.rc <-% p->%Rc.rc - 1
        totalReferenceCount <- totalReferenceCount-1

        if p->%Rc.rc = 0 then
            release p
        else
            // 循環ゴミが切り離されるかもしれない
            candidate p
