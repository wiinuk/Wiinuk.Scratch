namespace Scratch.Primitives

[<CompiledName "IHeteloList">] 
type HList =
    abstract Match: matcher: #HListMatcher<'Result> -> 'Result

and [<CompiledName "IHeteloListMatcher`1">] HListMatcher<'Result> =
    abstract Unit: unit -> 'Result
    abstract Node: head: 'Head inref * tail: #HList inref -> 'Result

[<CompiledName "IMutableValueHeteloList">]
type MVList =
    inherit HList
    abstract Match: matcher: #MVListMatcher<'R> -> 'R

and [<CompiledName "IMutableValueListMatcher">] MVListMatcher<'R> =
    abstract Unit: unit -> 'R
    abstract Node: head: 'Head byref * tail: #HList byref -> 'R

[<Struct>]
[<CompiledName "HeteloUnit">]
type HUnit = | HUnit with
    interface HList with
        member _.Match m = m.Unit()

    interface MVList with
        member _.Match m = m.Unit()
type hunit = HUnit
module HUnit =
    let ignore _ = HUnit
    let toUnit (_: HUnit) = ()

[<CompiledName "HeteloNode">]
type HCons<'Head,'Tail> when 'Tail :> HList = { head: 'Head; tail: 'Tail } with
    interface HList with
        member x.Match m = m.Node(&x.head, &x.tail)

type hcons<'a,'xs> when 'xs :> HList = HCons<'a,'xs>

[<Struct>]
[<CompiledName "MutableValueHeteloNode">]
type MVCons<'Head,'Tail> when 'Tail :> HList = {
    mutable vhead: 'Head
    mutable vtail: 'Tail
}
with
    interface HList with
        member c.Match m = m.Node(&c.vhead, &c.vtail)

    interface MVList with
        member c.Match m = m.Node(&c.vhead, &c.vtail)

type mvcons<'a,'xs> when 'xs :> HList = MVCons<'a,'xs>

type HTuple<'T1> = HCons<'T1, HUnit>
type HTuple<'T1,'T2> = HCons<'T1,HTuple<'T2>>
type HTuple<'T1,'T2,'T3> = HCons<'T1,HTuple<'T2,'T3>>
type HTuple<'T1,'T2,'T3,'T4> = HCons<'T1,HTuple<'T2,'T3,'T4>>
type HTuple<'T1,'T2,'T3,'T4,'T5> = HCons<'T1,HTuple<'T2,'T3,'T4,'T5>>
type HTuple<'T1,'T2,'T3,'T4,'T5,'T6> = HCons<'T1,HTuple<'T2,'T3,'T4,'T5,'T6>>
type HTuple<'T1,'T2,'T3,'T4,'T5,'T6,'T7> = HCons<'T1,HTuple<'T2,'T3,'T4,'T5,'T6,'T7>>
type HTuple<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8> = HCons<'T1,HTuple<'T2,'T3,'T4,'T5,'T6,'T7,'T8>>
type HTuple<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9> = HCons<'T1,HTuple<'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9>>
type HTuple<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9,'T10> = HCons<'T1,HTuple<'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9,'T10>>

[<AutoOpen>]
module HListOperators =
    let (^^) head tail = { head = head; tail = tail }

module HList =
    module Types =
        [<RequireQualifiedAccess; Struct; NoComparison; NoEquality>]
        type Length = { acc: int } with
            interface HListMatcher<int> with
                member m.Unit() = m.acc
                member m.Node(_ref, xs) = xs.Match { acc = m.acc + 1 }

        /// Least Upper Boundary
        type ILub<'A,'B,'Out> =
            abstract Left: 'A -> 'Out
            abstract Right: 'B -> 'Out

        [<Struct; NoComparison; NoEquality>]
        type Lub<'T> = | Lub with
            interface ILub<'T,'T,'T> with
                member _.Left x = x
                member _.Right x = x

        type IToList<'L,'Lub when 'L :> HList> =
            abstract Apply: 'L -> 'Lub list

        [<Struct; NoComparison; NoEquality>]
        type HUnitToList<'T> = | HUnitToList with
            interface IToList<HUnit,'T> with
                member _.Apply HUnit = []

        [<Struct; NoComparison; NoEquality>]
        type HSingleToList<'H1,'T,'Lub> when 'Lub :> ILub<'H1,'H1,'T> and 'Lub : struct = | HSingleToList of 'Lub with
            interface IToList<HCons<'H1,HUnit>,'T> with
                member x.Apply l =
                    let (HSingleToList u) = x
                    [u.Left l.head]

        [<Struct; NoComparison; NoEquality>]
        type HListToList<'H1,'H2,'Rest,'T,'Lub,'ToList>
            when
            'Rest :> HList and
            'Lub :> ILub<'H1,'H2,'T> and
            'Lub : struct and
            'ToList :> IToList<HCons<'H2,'Rest>,'T> and
            'ToList : struct =
            | HListToList of u: 'Lub * ttl: 'ToList
        with
            interface IToList<HCons<'H1,HCons<'H2,'Rest>>,'T> with
                member x.Apply l =
                    let (HListToList(u, ttl)) = x
                    u.Left l.head::ttl.Apply l.tail

        [<Struct>]
        [<NoComparison; NoEquality>]
        type ToListBox<'L,'Lub when 'L :> HList> = | ToListBox of IToList<'L,'Lub> with
            interface IToList<'L,'Lub> with
                member x.Apply l =
                    let (ToListBox x) = x
                    x.Apply l
    open Types

    let matchWith (m: #HListMatcher<_>) (xs: 'List when 'List :> HList) = xs.Match m
    let toList (toList: 'ToList when 'ToList :> IToList<_,_> and 'ToList : struct) (t: #HList) = toList.Apply t

    let empty = HUnit
    let singleton x = { head = x; tail = HUnit }
    let cons x xs = { head = x; tail = xs }
    let inline mapHead f xs = { head = f xs.head; tail = xs.tail }

    let length xs = matchWith { Length.acc = 0 } xs

    let inline item0 t = t.head
    let inline item1 t = t.tail.head
    let inline item2 t = item1 t.tail
    let inline item3 t = item2 t.tail
    let inline item4 t = item3 t.tail

[<AutoOpen>]
module HUnitIsHasDefault =
    module HasDefault =
        [<Struct>]
        type HUnit =
            interface IHasDefaultShape<hunit> with
                member _.GetDefault() = hunit.HUnit

        let hunit = the<HUnit>