module Scratch.Json.Utf8.ObjectSyntaxes
open NonStructuralComparison
open Utf8Json
open Utf8Json.Internal
open Scratch.Primitives
open Scratch.Json.Utf8
open Scratch.Json.PartialIsomorphisms


[<Struct>]
type Nil = | EmptyObject with
    interface IObjectSyntax<hunit, hunit> with
        member _.GetPropertyKeys _ = []
        member _.InitializeFromZero _ref = ()
        member _.DeserializeAndAdd(r, c, _ref) =
            match r.GetCurrentJsonToken() with
            | T.EndObject -> true
            | _ ->
                c.error <- { messageOrExn = IsoError.ofMessage "expected: '}'"; reader = r }
                false

        member _.Complete(_ref1, _ref2, _ref3) = true
        member _.TrySerialize(_ref, _, _) = true

[<Struct>]
type Ignore = | IgnoreProperties with
    interface IObjectSyntax<hunit, hunit> with
        member _.GetPropertyKeys _ = []
        member _.InitializeFromZero _ref = ()
        member _.DeserializeAndAdd(r, _ref2, _ref3) =

            // ReadNextBlock is nothrow
            r.ReadNextBlock()
            true

        member _.Complete(_ref1, _ref2, _ref3) = true
        member _.TrySerialize(_ref1, _, _) = true

[<Struct; NoEquality; NoComparison>] 
type Property<'T,'TRest,'TSource,'TRestSource,'TRestIntermediate>
    when 'TSource :> 'T ISyntax
    and 'TRest :> HList
    and 'TRestSource :> IObjectSyntax<'TRest,'TRestIntermediate>
    and 'TRestIntermediate :> HList = internal {

    keyString: string
    keySerializer: KeySerializer

    mutable value: 'TSource
    mutable rest: 'TRestSource
}
with
    interface IObjectSyntax<HCons<'T,'TRest>, mvcons<'T voption, 'TRestIntermediate>> with
        member s.GetPropertyKeys currentKey = (s.keyString, currentKey)::s.rest.GetPropertyKeys(currentKey + 1)
        member s.InitializeFromZero result =
            // default s.vhead = ValueNone
            s.rest.InitializeFromZero &result.vtail

        member s.DeserializeAndAdd(r, c, intermediate) =
            if c.propertyKey = c.currentKey then
                let mutable value = Unchecked.defaultof<_>
                if s.value.TryDeserialize(&r, &value) then
                    intermediate.vhead <- ValueSome value
                    true
                else
                    c.error <- { messageOrExn = s.keySerializer.errorMessage; reader = r }
                    false
            else
                c.currentKey <- c.currentKey + 1
                s.rest.DeserializeAndAdd(&r, &c, &intermediate.vtail)

        member s.Complete(c, intermediate, result) =
            match intermediate.vhead with
            | ValueNone ->
                c.error <- { messageOrExn = s.keySerializer.errorMessage; reader = c.lastReader }
                false

            | ValueSome head ->

            let mutable tail = Unchecked.defaultof<_>
            if s.rest.Complete(&c, &intermediate.vtail, &tail) then
                result <- { head = head; tail = tail }
                true
            else
                false

        member s.TrySerialize(w, x, n) =
            serializeKey s.keySerializer &w n

            s.value.TrySerialize(&w, x.head) &&
            s.rest.TrySerialize(&w, x.tail, n + 1)

[<Struct; NoEquality; NoComparison>] 
type ValueProperty<'T,'TRest,'TSource,'TRestSource,'TRestIntermediate>
    when 'TSource :> 'T ISyntax
    and 'TRestSource :> IObjectSyntax<'TRest,'TRestIntermediate>
    and 'TRestIntermediate :> HList 
    and 'TRest :> HList = internal {

    keyString: string
    keySerializer: KeySerializer

    mutable value: 'TSource
    mutable rest: 'TRestSource
}
with
    interface IObjectSyntax<MVCons<'T,'TRest>, mvcons<'T voption, 'TRestIntermediate>> with
        member s.GetPropertyKeys currentKey = (s.keyString, currentKey)::s.rest.GetPropertyKeys(currentKey + 1)
        member s.InitializeFromZero result = s.rest.InitializeFromZero &result.vtail
        member s.DeserializeAndAdd(r, c, intermediate) =
            if c.propertyKey = c.currentKey then
                let mutable value = Unchecked.defaultof<_>
                if s.value.TryDeserialize(&r, &value) then
                    intermediate.vhead <- ValueSome value
                    true
                else
                    c.error <- { messageOrExn = s.keySerializer.errorMessage; reader = r }
                    false
            else
                c.currentKey <- c.currentKey + 1
                s.rest.DeserializeAndAdd(&r, &c, &intermediate.vtail)

        member s.Complete(c, intermediate, result) =
            match intermediate.vhead with
            | ValueNone ->
                c.error <- { messageOrExn = s.keySerializer.errorMessage; reader = c.lastReader }
                false

            | ValueSome head ->
                if s.rest.Complete(&c, &intermediate.vtail, &result.vtail) then
                    result.vhead <- head
                    true
                else
                    false

        member s.TrySerialize(w, x, n) =
            serializeKey s.keySerializer &w n

            s.value.TrySerialize(&w, x.vhead) &&
            s.rest.TrySerialize(&w, x.vtail, n + 1)

[<Struct; NoEquality; NoComparison>] 
type OptionalProperty<'T,'TRest,'TSource,'TRestSource,'TRestIntermediate>
    when 'TSource :> 'T ISyntax
    and 'TRest :> HList
    and 'TRestSource :> IObjectSyntax<'TRest,'TRestIntermediate>
    and 'TRestIntermediate :> HList = internal {

    keyString: string
    keySerializer: KeySerializer

    mutable value: 'TSource
    mutable rest: 'TRestSource
}
with
    interface IObjectSyntax<HCons<'T option,'TRest>, mvcons<'T voption,'TRestIntermediate>> with
        member s.GetPropertyKeys currentKey = (s.keyString, currentKey)::s.rest.GetPropertyKeys(currentKey + 1)
        member s.InitializeFromZero result = s.rest.InitializeFromZero &result.vtail
        member s.DeserializeAndAdd(r, c, intermediate) =
            if c.propertyKey = c.currentKey then
                let mutable value = Unchecked.defaultof<_>
                if s.value.TryDeserialize(&r, &value) then
                    intermediate.vhead <- ValueSome value
                    true
                else
                    c.error <- { messageOrExn = s.keySerializer.errorMessage; reader = r }
                    false
            else
                c.currentKey <- c.currentKey + 1
                s.rest.DeserializeAndAdd(&r, &c, &intermediate.vtail)

        member s.Complete(c, intermediate, result) =
            let mutable tail = Unchecked.defaultof<_>
            if s.rest.Complete(&c, &intermediate.vtail, &tail) then
                result <- {
                    head = VOption.box intermediate.vhead
                    tail = tail
                }
                true
            else
                false

        member s.TrySerialize(w, x, n) =
            match x.head with
            | Some head ->
                serializeKey s.keySerializer &w n
                s.value.TrySerialize(&w, head) &&
                s.rest.TrySerialize(&w, x.tail, n + 1)

            | _ ->
                s.rest.TrySerialize(&w, x.tail, n)

[<Struct; NoEquality; NoComparison>] 
type ValueOptionalProperty<'T,'TRest,'TSource,'TRestSource,'TRestIntermediate>
    when 'TSource :> 'T ISyntax
    and 'TRest :> HList
    and 'TRestSource :> IObjectSyntax<'TRest,'TRestIntermediate>
    and 'TRestIntermediate :> HList = internal {

    keyString: string
    keySerializer: KeySerializer

    mutable value: 'TSource
    mutable rest: 'TRestSource
}
with
    interface IObjectSyntax<MVCons<'T option,'TRest>, MVCons<'T voption,'TRestIntermediate>> with
        member s.GetPropertyKeys currentKey = (s.keyString, currentKey)::s.rest.GetPropertyKeys(currentKey + 1)
        member s.InitializeFromZero result = s.rest.InitializeFromZero &result.vtail
        member s.DeserializeAndAdd(r, c, intermediate) =
            if c.propertyKey = c.currentKey then
                let mutable value = Unchecked.defaultof<_>
                if s.value.TryDeserialize(&r, &value) then
                    intermediate.vhead <- ValueSome value
                    true
                else
                    c.error <- { messageOrExn = s.keySerializer.errorMessage; reader = r }
                    false
            else
                c.currentKey <- c.currentKey + 1
                s.rest.DeserializeAndAdd(&r, &c, &intermediate.vtail)

        member s.Complete(c, intermediate, result) =
            if s.rest.Complete(&c, &intermediate.vtail, &result.vtail) then
                result.vhead <- VOption.box intermediate.vhead
                true
            else
                false

        member s.TrySerialize(w, x, n) =
            match x.vhead with
            | Some head ->
                serializeKey s.keySerializer &w n
                s.value.TrySerialize(&w, head) &&
                s.rest.TrySerialize(&w, x.vtail, n + 1)

            | _ ->
                s.rest.TrySerialize(&w, x.vtail, n)

[<Struct; NoEquality; NoComparison>] 
type DefaultProperty<'T,'TRest,'TSource,'TRestSource,'TRestIntermediate>
    when 'TSource :> 'T ISyntax
    and 'TRest :> HList
    and 'TRestSource :> IObjectSyntax<'TRest,'TRestIntermediate>
    and 'TRestIntermediate :> HList = internal {

    keyString: string
    keySerializer: KeySerializer
    defaultValue: 'T
    mutable value: 'TSource
    mutable rest: 'TRestSource
}
with
    interface IObjectSyntax<HCons<'T,'TRest>, mvcons<'T,'TRestIntermediate>> with
        member s.GetPropertyKeys currentKey = (s.keyString, currentKey)::s.rest.GetPropertyKeys(currentKey + 1)
        member s.InitializeFromZero result =
            result.vhead <- s.defaultValue
            s.rest.InitializeFromZero &result.vtail

        member s.DeserializeAndAdd(r, c, intermediate) =
            if c.propertyKey = c.currentKey then
                let mutable value = Unchecked.defaultof<_>
                if s.value.TryDeserialize(&r, &value) then
                    intermediate.vhead <- value
                    true
                else
                    c.error <- { messageOrExn = s.keySerializer.errorMessage; reader = r }
                    false
            else
                c.currentKey <- c.currentKey + 1
                s.rest.DeserializeAndAdd(&r, &c, &intermediate.vtail)

        member s.Complete(c, intermediate, result) =
            let mutable tail = Unchecked.defaultof<_>
            if s.rest.Complete(&c, &intermediate.vtail, &tail) then
                result <- {
                    head = intermediate.vhead
                    tail = tail
                }
                true
            else
                false

        member s.TrySerialize(w, x, n) =
            serializeKey s.keySerializer &w n
            s.value.TrySerialize(&w, x.head) &&
            s.rest.TrySerialize(&w, x.tail, n + 1)

[<Struct; NoEquality; NoComparison>] 
type ValueDefaultProperty<'T,'TRest,'TSource,'TRestSource,'TRestIntermediate>
    when 'TSource :> 'T ISyntax
    and 'TRest :> HList
    and 'TRestSource :> IObjectSyntax<'TRest,'TRestIntermediate>
    and 'TRestIntermediate :> HList = internal {

    keyString: string
    keySerializer: KeySerializer
    defaultValue: 'T
    mutable value: 'TSource
    mutable rest: 'TRestSource
}
with
    interface IObjectSyntax<MVCons<'T,'TRest>, MVCons<'T,'TRestIntermediate>> with
        member s.GetPropertyKeys currentKey = (s.keyString, currentKey)::s.rest.GetPropertyKeys(currentKey + 1)
        member s.InitializeFromZero result =
            result.vhead <- s.defaultValue
            s.rest.InitializeFromZero &result.vtail 

        member s.DeserializeAndAdd(r, c, intermediate) =
            if c.propertyKey = c.currentKey then
                let mutable value = Unchecked.defaultof<_>
                if s.value.TryDeserialize(&r, &value) then
                    intermediate.vhead <- value
                    true
                else
                    c.error <- { messageOrExn = s.keySerializer.errorMessage; reader = r }
                    false
            else
                c.currentKey <- c.currentKey + 1
                s.rest.DeserializeAndAdd(&r, &c, &intermediate.vtail)

        member s.Complete(c, intermediate, result) =
            if s.rest.Complete(&c, &intermediate.vtail, &result.vtail) then
                result.vhead <- intermediate.vhead
                true
            else
                false

        member s.TrySerialize(w, x, n) =
            serializeKey s.keySerializer &w n
            s.value.TrySerialize(&w, x.vhead) &&
            s.rest.TrySerialize(&w, x.vtail, n + 1)

[<Struct; NoEquality; NoComparison>]
type ToSyntax<'T,'TSource,'TIntermediate>
    when 'TSource :> IObjectSyntax<'T,'TIntermediate> = internal {
    mutable source: 'TSource
    dictionary: AutomataDictionary
}
with
    interface 'T ISyntax with
        member s.TryDeserialize(r, result) =
            let mutable count = 0
            if not <| r.ReadIsBeginObject() then false else

            let dictionaty = s.dictionary
            let mutable success = false
            let mutable context = { propertyKey = 0; currentKey = 0; error = { messageOrExn = IsoError(); reader = JsonReader() } }
            let mutable intermediate = Unchecked.defaultof<_>
            s.source.InitializeFromZero &intermediate
            while
                if tryReadEndObjectOrValueSeparator &r &count &success then
                    let t = r.GetCurrentJsonToken()
                    match t with
                    | T.String ->
                        let keyString = readStringSegmentRaw &r
                        if not <| r.ReadIsNameSeparator() then false else

                        // TODO:
                        ignore (dictionaty.TryGetValue(keyString, &context.propertyKey))

                        context.currentKey <- 0
                        s.source.DeserializeAndAdd(&r, &context, &intermediate)

                    | _ -> false
                else
                    false
                do ()

            if not success then
                false
            else

            let mutable context = { lastReader = r; error = context.error }
            s.source.Complete(&context, &intermediate, &result)

            // TODO: error handling

        member s.Deserialize r =
            let mutable count = 0
            r.ReadIsBeginObjectWithVerify()

            let dictionary = s.dictionary
            let mutable context = { propertyKey = 0; currentKey = 0; error = { messageOrExn = IsoError(); reader = JsonReader() } }
            let mutable intermediate = Unchecked.defaultof<_>
            s.source.InitializeFromZero &intermediate
            while not (r.ReadIsEndObjectWithSkipValueSeparator &count) do
                let keyString = r.ReadPropertyNameSegmentRaw()

                // TODO:
                ignore (dictionary.TryGetValue(keyString, &context.propertyKey))

                context.currentKey <- 0
                if s.source.DeserializeAndAdd(&r, &context, &intermediate) then () else

                raiseParsingExceptionOfParseError context.error

            let mutable context = { lastReader = r; error = context.error }
            let mutable x = Unchecked.defaultof<_>
            if s.source.Complete(&context, &intermediate, &x) then x else

            raiseParsingExceptionOfParseError context.error

        // TODO:
        member s.TrySerialize(w, x) =
            w.WriteBeginObject()
            if s.source.TrySerialize(&w, x, 0) then
                w.WriteEndObject()
                true
            else
                false

        member s.Serialize(w, x) =
            w.WriteBeginObject()
            if s.source.TrySerialize(&w, x, 0) then
                w.WriteEndObject()
            else
                failwithf "jObject"

[<Struct; NoEquality; NoComparison>]
type Pipe<'T,'TSource,'TIso,'TResult,'TIntermediate>
    when 'TSource :> IObjectSyntax<'T,'TIntermediate>
    and 'TIso :> IsoR<'T,'TResult> = internal {
    mutable source: 'TSource
    mutable mapping: 'TIso
}
with
    interface IObjectSyntax<'TResult,'TIntermediate> with
        member s.InitializeFromZero result = s.source.InitializeFromZero &result
        member s.GetPropertyKeys k = s.source.GetPropertyKeys k
        member s.DeserializeAndAdd(r, c, x) = s.source.DeserializeAndAdd(&r, &c, &x)
        member s.Complete(c, x, result) =
            let mutable value = Unchecked.defaultof<_>
            if s.source.Complete(&c, &x, &value) then
                if s.mapping.Forward(&value, &result) then true else
                c.error <- { messageOrExn = IsoError.ofMessage "pipe"; reader = c.lastReader }
                false
            else
                false

        member s.TrySerialize(w, v, n) =
            let mutable x = Unchecked.defaultof<_>
            // TODO: inref
            let mutable v = v
            s.mapping.Reverse(&v, &x) &&
            s.source.TrySerialize(&w, x, n)
