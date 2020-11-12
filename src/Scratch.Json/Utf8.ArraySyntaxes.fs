module Scratch.Json.Utf8.ArraySyntaxes
open NonStructuralComparison
open Scratch.Primitives
open Scratch.Json.Utf8


[<Struct; NoEquality; NoComparison; RequireQualifiedAccess>]
type HCons<'T,'TTail,'TSource,'TTailSource>
    when 'TSource :> 'T ISyntax
    and 'TTailSource :> 'TTail IArraySyntax
    and 'TTail :> HList = internal {
    mutable head: 'TSource
    mutable tail: 'TTailSource
}
with
    interface HCons<'T,'TTail> IArraySyntax with
        member s.TryDeserialize(r, c, result) =
            if c.count <> 0 && not <| r.ReadIsValueSeparator() then false else
            c.count <- c.count + 1

            let mutable head = Unchecked.defaultof<_>
            let mutable tail = Unchecked.defaultof<_>
            s.head.TryDeserialize(&r, &head) &&
            s.tail.TryDeserialize(&r, &c, &tail) && (
                result <- { head = head; tail = tail }
                true
            )

        member s.Deserialize(r, c) =
            if c.count <> 0 then r.ReadIsValueSeparatorWithVerify()
            c.count <- c.count + 1

            let head = s.head.Deserialize &r
            let tail = s.tail.Deserialize(&r, &c)
            { head = head; tail = tail }

        member s.TrySerialize(w, c, x) =
            if c.count <> 0 then w.WriteValueSeparator()
            c.count <- c.count + 1

            s.head.TrySerialize(&w, x.head) &&
            s.tail.TrySerialize(&w, &c, x.tail)

        member s.Serialize(w, c, x) =
            if c.count <> 0 then w.WriteValueSeparator()
            c.count <- c.count + 1

            s.head.Serialize(&w, x.head)
            s.tail.Serialize(&w, &c, x.tail)

[<Struct; NoEquality; NoComparison; RequireQualifiedAccess>]
type DefaultHCons<'T,'TTail,'TSource,'TTailSource>
    when 'TSource :> 'T ISyntax
    and 'TTailSource :> 'TTail IArraySyntax
    and 'TTail :> HList
    and 'T : equality = internal {
    mutable head: 'TSource
    mutable tail: 'TTailSource
    defaultValue: 'T
}
with
    interface HCons<'T,'TTail> IArraySyntax with
        member s.TryDeserialize(r, c, result) =
            let mutable head = Unchecked.defaultof<_>
            if tryReadItem &s.head &r &c &head then () else head <- s.defaultValue

            let mutable tail = Unchecked.defaultof<_>
            s.tail.TryDeserialize(&r, &c, &tail) && (
                result <- { head = head; tail = tail }
                true
            )

        member s.Deserialize(r, c) =
            let mutable head = Unchecked.defaultof<_>
            if tryReadItem &s.head &r &c &head then () else head <- s.defaultValue

            let tail = s.tail.Deserialize(&r, &c)
            { head = head; tail = tail }

        member s.TrySerialize(w, c, x) =
            (
                if LanguagePrimitives.GenericEqualityER x.head s.defaultValue then true else
                if c.count <> 0 then w.WriteValueSeparator()
                c.count <- c.count + 1

                s.head.TrySerialize(&w, x.head)
            )
            &&
            s.tail.TrySerialize(&w, &c, x.tail)

        member s.Serialize(w, c, x) =
            (
                if LanguagePrimitives.GenericEqualityER x.head s.defaultValue then () else
                if c.count <> 0 then w.WriteValueSeparator()
                c.count <- c.count + 1

                s.head.Serialize(&w, x.head)
            )
            s.tail.Serialize(&w, &c, x.tail)

[<Struct; NoEquality; NoComparison; RequireQualifiedAccess>]
type OptionalHCons<'T,'TTail,'TSource,'TTailSource>
    when 'TSource :> 'T ISyntax
    and 'TTailSource :> 'TTail IArraySyntax
    and 'TTail :> HList = internal {
    mutable head: 'TSource
    mutable tail: 'TTailSource
}
with
    interface HCons<'T option,'TTail> IArraySyntax with
        member s.TryDeserialize(r, c, result) =
            let head =
                let mutable head = Unchecked.defaultof<_>
                if tryReadItem &s.head &r &c &head then Some head else None

            let mutable tail = Unchecked.defaultof<_>
            s.tail.TryDeserialize(&r, &c, &tail) && (
                result <- { head = head; tail = tail }
                true
            )

        member s.Deserialize(r, c) =
            let head =
                let mutable head = Unchecked.defaultof<_>
                if tryReadItem &s.head &r &c &head then Some head else None

            let tail = s.tail.Deserialize(&r, &c)
            { head = head; tail = tail }

        member s.TrySerialize(w, c, x) =
            match x.head with
            | None -> true
            | Some head ->
                if c.count <> 0 then w.WriteValueSeparator()
                c.count <- c.count + 1
                s.head.TrySerialize(&w, head)
            &&
            s.tail.TrySerialize(&w, &c, x.tail)

        member s.Serialize(w, c, x) =
            match x.head with
            | Some head ->
                if c.count <> 0 then w.WriteValueSeparator()
                c.count <- c.count + 1
                s.head.Serialize(&w, head)
            | _ -> ()
            s.tail.Serialize(&w, &c, x.tail)

[<Struct; NoEquality; NoComparison; RequireQualifiedAccess>]
type MVCons<'T,'TTail,'TSource,'TTailSource>
    when 'TSource :> 'T ISyntax
    and 'TTailSource :> 'TTail IArraySyntax
    and 'TTail :> HList = internal {
    mutable head: 'TSource
    mutable tail: 'TTailSource
}
with
    interface MVCons<'T,'TTail> IArraySyntax with
        member s.TryDeserialize(r, c, result) =
            if c.count <> 0 && not <| r.ReadIsValueSeparator() then false else
            c.count <- c.count + 1

            s.head.TryDeserialize(&r, &result.vhead) &&
            s.tail.TryDeserialize(&r, &c, &result.vtail)

        member s.Deserialize(r, c) =
            if c.count <> 0 then r.ReadIsValueSeparatorWithVerify()
            c.count <- c.count + 1

            let head = s.head.Deserialize &r
            let tail = s.tail.Deserialize(&r, &c)
            { vhead = head; vtail = tail }

        member s.TrySerialize(w, c, x) =
            if c.count <> 0 then w.WriteValueSeparator()
            c.count <- c.count + 1

            s.head.TrySerialize(&w, x.vhead) &&
            s.tail.TrySerialize(&w, &c, x.vtail)

        member s.Serialize(w, c, x) =
            if c.count <> 0 then w.WriteValueSeparator()
            c.count <- c.count + 1

            s.head.Serialize(&w, x.vhead)
            s.tail.Serialize(&w, &c, x.vtail)

[<Struct>]
type Nil = | EmptyTuple
with
    interface hunit IArraySyntax with
        member _.TryDeserialize(r, _ref1, _ref2) = r.ReadIsEndArray()
        member _.Deserialize(r, _ref) = r.ReadIsEndArrayWithVerify(); HUnit.HUnit
        member _.TrySerialize(_ref1, _ref2, _) = (); true
        member _.Serialize(_ref1, _ref2, _) = ()

[<Struct; NoComparison; NoEquality>]
type List<'T,'TSource> when 'TSource :> 'T ISyntax = internal {
    mutable item: 'TSource
}
with
    interface HCons<'T list, hunit> IArraySyntax with
        member s.TryDeserialize(r, c, x) =
            let mutable items = []
            let mutable success = false
            let mutable item = Unchecked.defaultof<_>
            while
                if tryReadEndArrayOrValueSeparator &r &c &success then
                    if not (s.item.TryDeserialize(&r, &item)) then false else

                    items <- item::items
                    true
                else
                    false
                do ()

            x <- { head = List.rev items; tail = HUnit.HUnit }
            success

        member s.Deserialize(r, c) =
            let mutable items = []
            while not (r.ReadIsEndArrayWithSkipValueSeparator &c.count) do
                items <- s.item.Deserialize &r::items

            { head = List.rev items; tail = HUnit.HUnit }

        // TODO:
        member s.TrySerialize(w, c, t) =
            match t.head with
            | [] -> true
            | x::xs ->

            if c.count <> 0 then w.WriteValueSeparator()
            if not <| s.item.TrySerialize(&w, x) then false else

            let mutable list = xs
            let mutable success = false
            while
                match list with
                | [] -> success <- true; false
                | x::xs ->
                    w.WriteValueSeparator()
                    if not <| s.item.TrySerialize(&w, x) then false else
                    list <- xs
                    true
                do ()
            success

        member s.Serialize(w, c, t) =
            match t.head with
            | [] -> ()
            | x::xs ->

            if c.count <> 0 then w.WriteValueSeparator()
            s.item.Serialize(&w, x)

            let mutable list = xs
            while
                match list with
                | [] -> false
                | x::xs ->
                    w.WriteValueSeparator()
                    s.item.Serialize(&w, x)
                    list <- xs
                    true
                do ()

[<Struct; NoComparison; NoEquality>]
type ListV<'T,'TSource> when 'TSource :> 'T ISyntax = internal {
    mutable item: 'TSource
}
with
    interface MVCons<'T list, hunit> IArraySyntax with
        member s.TryDeserialize(r, c, x) =
            let mutable items = []
            let mutable success = false
            let mutable item = Unchecked.defaultof<_>
            while
                if tryReadEndArrayOrValueSeparator &r &c &success then
                    if not (s.item.TryDeserialize(&r, &item)) then false else

                    items <- item::items
                    true
                else
                    false
                do ()

            x.vhead <- List.rev items
            success

        member s.Deserialize(r, c) =
            let mutable items = []
            while not (r.ReadIsEndArrayWithSkipValueSeparator &c.count) do
                items <- s.item.Deserialize &r::items

            { vhead = List.rev items; vtail = HUnit.HUnit }

        // TODO:
        member s.TrySerialize(w, c, t) =
            match t.vhead with
            | [] -> true
            | x::xs ->

            if c.count <> 0 then w.WriteValueSeparator()
            if not <| s.item.TrySerialize(&w, x) then false else

            let mutable list = xs
            let mutable success = false
            while
                match list with
                | [] -> success <- true; false
                | x::xs ->
                    w.WriteValueSeparator()
                    if not <| s.item.TrySerialize(&w, x) then false else
                    list <- xs
                    true
                do ()
            success

        member s.Serialize(w, c, t) =
            match t.vhead with
            | [] -> ()
            | x::xs ->

            if c.count <> 0 then w.WriteValueSeparator()
            s.item.Serialize(&w, x)

            let mutable list = xs
            while
                match list with
                | [] -> false
                | x::xs ->
                    w.WriteValueSeparator()
                    s.item.Serialize(&w, x)
                    list <- xs
                    true
                do ()

[<Struct; NoEquality; NoComparison>]
type ToSyntax<'T,'TSource> when 'TSource :> 'T IArraySyntax = internal {
    mutable source: 'TSource
}
with
    interface 'T ISyntax with
        member s.TryDeserialize(r, x) =
            if r.ReadIsBeginArray() then
                let mutable context = { count = 0 }
                s.source.TryDeserialize(&r, &context, &x)
            else
                false

        member s.Deserialize r =
            r.ReadIsBeginArrayWithVerify()
            let mutable context = { count = 0 }
            s.source.Deserialize(&r, &context)

        member s.TrySerialize(w, x) =
            w.WriteBeginArray()
            let mutable context = { count = 0 }
            s.source.TrySerialize(&w, &context, x) && (
                w.WriteEndArray()
                true
            )

        member s.Serialize(w, x) =
            w.WriteBeginArray()
            let mutable context = { count = 0 }
            s.source.Serialize(&w, &context, x)
            w.WriteEndArray()
