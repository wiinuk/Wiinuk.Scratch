namespace Scratch.Transformers
#nowarn "0009" // Uses of this construct may result in the generation of unverifiable .NET IL code.
#nowarn "0051" // The use of native pointers may result in unverifiable .NET IL code.
open Scratch.Primitives
open System.Runtime.InteropServices


type ModifyTag =
    | None = 0b000uy
    | Modified = 0b001uy
    | RemovedMask = 0b010uy
    | ModifyRemoved = 0b011uy
    | Skip = 0b100uy

[<Struct; StructLayout(LayoutKind.Auto)>]
type TransformResult<'T> = { tag: ModifyTag; value: 'T }
type Transform<'E,'T> = 'E -> 'T TransformResult
type Transformer<'E,'T> = 'T -> Transform<'E,'T>

[<AbstractClass; Sealed>]
type private TypeHandleValueHolder<'T> private () =
    static let value = typeof<'T>.TypeHandle.Value
    static member Value = value

[<Struct>]
type TagValue = private TagValue of value1: int64 * value2: obj

type IIsTagValueShape<'V> =
    inherit IShape
    abstract Construct: 'V -> TagValue
    abstract Deconstruct: TagValue * 'V outref -> bool

module IsTagValue =
    let inline construct (_s: The<'S> when 'S :> #IIsTagValueShape<_>) v = Unchecked.defaultof<'S>.Construct v
    let inline deconstruct (_s: The<'S> when 'S :> #IIsTagValueShape<_>) v (r: _ outref) = Unchecked.defaultof<'S>.Deconstruct(v, &r)

    [<Struct>]
    type HUnit =
        interface IIsTagValueShape<hunit> with
            member _.Construct _ = TagValue(0L, null)
            member _.Deconstruct(_, _ref) = true
    let hunit = the<HUnit>
    
    [<Struct>]
    type Reference<'t> when 't : not struct =
        interface IIsTagValueShape<'t> with
            member _.Construct x = TagValue(0L, x)
            member _.Deconstruct(TagValue(_, x), r) =
                match x with 
                | :? 't as x -> r <- x; true
                | _ -> false
    let reference (_: The<'t>) = the<Reference<'t>>

    [<Struct>]
    type Obj<'t> when 't : not struct and 't : null =
        interface IIsTagValueShape<'t> with
            member _.Construct x = TagValue(0L, x)
            member _.Deconstruct(TagValue(_, x), r) =
                match x with
                | null -> r <- null; true
                | :? 't as x -> r <- x; true
                | _ -> false
    let obj (_: The<'t>) = the<Obj<'t>>

    module P = NativeInterop.NativePtr
    let private enumToInt32 (x: 't when 't : enum<int32> and 't : unmanaged): int32 = P.read(P.ofVoidPtr<int32>(P.toVoidPtr &&x))
    let private int32ToEnum (x: int32): 't when 't : enum<int32> and 't : unmanaged = P.read(P.ofVoidPtr<'t>(P.toVoidPtr &&x))

    [<Struct>]
    type EnumInt32<'t> when 't : enum<int32> and 't : unmanaged =
        interface IIsTagValueShape<'t> with
            member _.Construct x = TagValue(int64(enumToInt32 x), null)
            member _.Deconstruct(TagValue(x, _), r) = r <- int32ToEnum(int32 x); true

    let enum (_: The<'t>) = the<EnumInt32<'t>>

[<Struct>]
type TagId = private TagId of typeHandle: nativeint

[<Struct>]
type Tag = Tag of id: TagId * value: TagValue
module Tag =
    let typeid (_: The<'t>) = TagId(TypeHandleValueHolder<'t>.Value)

[<Struct>]
type Tags = private TagList of Tag list
module Tags =
    let empty = TagList []
    let isEmpty = function TagList [] -> true | _ -> false
    let contains id (TagList xs) = List.exists (fun (Tag(id = id')) -> id = id') xs
    let add x (TagList xs) = TagList(x::xs)
    let tryPickV picker (TagList xs) =
        let mutable list = xs
        let mutable result = ValueNone
        while
            begin
                match list with
                | [] -> false
                | x::xs ->

                match picker x with
                | ValueSome _ as r -> result <- r; false
                | _ -> list <- xs; true
            end
            do ()
        result

type IIsTagIdShape =
    inherit IShape
    abstract Id: TagId

type IIsTagShape<'V> =
    inherit IIsTagValueShape<'V>
    inherit IIsTagIdShape

type ITagsOperationsShape<'V> =
    inherit IShape
    abstract Add: 'V * Tags -> Tags

module IsTag =
    let inline id (_s: The<'S> when 'S :> #IIsTagIdShape and 'S : struct) = Unchecked.defaultof<'S>.Id
    let inline add (_s: The<'S> when 'S :> #ITagsOperationsShape<_> and 'S : struct) v ts = Unchecked.defaultof<'S>.Add(v, ts)

    [<AbstractClass; Sealed>]
    type private DefaultTagHolder<'S,'V> when 'S :> IIsTagShape<'V> and 'S :> IHasDefaultShape<'V> and 'S : struct private () =
        static let value = Tag(id the<'S>, IsTagValue.construct the<'S> (HasDefault.getDefault the<'S>))
        static member Value = value
    let defaultTag (_s: The<'S>) = DefaultTagHolder<'S,_>.Value

    [<AbstractClass; Sealed>]
    type private DefaultTagsHolder<'S,'V> when 'S :> IIsTagShape<'V> and 'S :> IHasDefaultShape<'V> and 'S : struct private () =
        static let value = Tags.empty |> Tags.add (defaultTag the<'S>)
        static member Value = value
    let defaultTags (_s: The<'S>) = DefaultTagsHolder<'S,_>.Value

    [<Struct>]
    type IsTag<'IsTagId,'IsTagValue,'V> when 'IsTagId :> IIsTagIdShape and 'IsTagValue :> IIsTagValueShape<'V> and 'IsTagId : struct and 'IsTagValue : struct =
        interface IIsTagShape<'V> with
            member _.Id = id the<'IsTagId>
            member _.Construct x = IsTagValue.construct the<'IsTagValue> x
            member _.Deconstruct(x, r) = IsTagValue.deconstruct the<'IsTagValue> x &r

        interface ITagsOperationsShape<'V> with
            member _.Add(v, tags) = Tags.add (Tag(id the<'IsTagId>, IsTagValue.construct the<'IsTagValue> v)) tags

    type internal IsTagWithDefaultBox<'V> = { id: TagId; tag: Tag; tags: Tags }
    [<Struct>]
    type IsTagWithDefault<'C,'V> when 'C :> IIsTagShape<'V> and 'C :> IHasDefaultShape<'V> and 'C : struct =
        interface IIsTagShape<'V> with
            member _.Id = id the<'C>
            member _.Construct x = IsTagValue.construct the<'C> x
            member _.Deconstruct(t, r) = IsTagValue.deconstruct the<'C> t &r

        interface ITagsOperationsShape<'V> with
            member _.Add(v, tags) = if Tags.isEmpty tags then defaultTags the<'C> else Tags.add (Tag(id the<'C>, IsTagValue.construct the<'C> v)) tags

        interface IHasDefaultShape<'V> with
            member _.GetDefault() = HasDefault.getDefault the<'C>

    let makeIsTag (_: The<'IsTagId>) (_: The<'IsTag>) = the<IsTag<'IsTagId,'IsTag,_>>
    let makeIsTagWithDefault (_: The<'C>) = the<IsTagWithDefault<'C,_>>

[<Struct>]
type Tagged<'a> = Tagged of value: 'a * tags: Tags
module Tagged =
    let empty e = Tagged(e, Tags.empty)
    let value (Tagged(e, _)) = e
    let withTags tags (Tagged(e, _)) = Tagged(e, tags)
    
    let add _s v (Tagged(e, ts)) = Tagged(e, IsTag.add _s v ts)
    let addDefault _s (Tagged(e, ts)) = Tagged(e, IsTag.add _s (HasDefault.getDefault _s) ts)
    let contains _s (Tagged(_, xs)) = Tags.contains (IsTag.id _s) xs
    let tryFind _s (Tagged(_, xs)) =
        xs
        |> Tags.tryPickV (fun (Tag(id, t)) ->
            let mutable r = Unchecked.defaultof<_>
            if IsTag.id _s = id && IsTagValue.deconstruct _s t &r then ValueSome r
            else ValueNone
        )

[<AutoOpen>]
module ExportTagIsTag =
    type private ExportTagId = struct end
    [<Struct>]
    type ExportTagShape =
        interface IIsTagShape<HUnit> with
            member _.Id = Tag.typeid the<ExportTagId>

        interface IIsTagValueShape<HUnit> with
            member _.Construct x = IsTagValue.construct IsTagValue.hunit x
            member _.Deconstruct(x, r) = IsTagValue.deconstruct IsTagValue.hunit x &r

        interface IHasDefaultShape<HUnit> with
            member _.GetDefault() = HUnit
        
    let ExportTag = IsTag.makeIsTagWithDefault the<ExportTagShape>

[<RequireQualifiedAccess>]
type InliningOptions =
    | NoInlining
    | AggressiveInlining

[<AutoOpen>]
module InliningOptionsIsTag =
    type private InliningOptionsTagId = struct end

    [<Struct>]
    type InliningOptionsTagShape =
        interface IIsTagIdShape with
            member _.Id = Tag.typeid the<InliningOptionsTagId>

    let InliningOptionsTag = IsTag.makeIsTag the<InliningOptionsTagShape> (IsTagValue.reference the<InliningOptions>)
