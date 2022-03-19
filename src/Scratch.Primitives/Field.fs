namespace Scratch.Primitives

type IFieldShape<'Record,'Field> =
    inherit IShape
    abstract Get: 'Record -> 'Field
    abstract With: 'Record * 'Field -> 'Record

module Field =
    /// like `x.f`
    let get (s: 's when 's :> IFieldShape<_,_> and 's : struct) x = s.Get x
    /// like `{ x with f = v }`
    let wiz (s: 's when 's :> IFieldShape<_,_> and 's : struct) v x = s.With(x, v)
    /// like `{ x with f = mapping x.f }`
    let inline map s ([<InlineIfLambda>] mapping) x = wiz s (mapping (get s x)) x
    /// like `scope { x with f = mapping x.f }`
    let inline local s x ([<InlineIfLambda>] mapping) ([<InlineIfLambda>] scope) = scope (map s mapping x)
    let (|Get|) s x = get s x

    [<Struct; RequireQualifiedAccess>]
    type Compose<'S1,'S2,'R1,'R2,'F> when 'S1 :> IFieldShape<'R1,'R2> and 'S2 :> IFieldShape<'R2,'F> and 'S1 : struct and 'S2 : struct = | Compose with
        interface IFieldShape<'R1,'F> with
            member s.Get x = Unchecked.defaultof<'S2>.Get(Unchecked.defaultof<'S1>.Get x)
            member s.With(x, v) = Unchecked.defaultof<'S1>.With(x, Unchecked.defaultof<'S2>.With(Unchecked.defaultof<'S1>.Get x, v))

    let compose (_s1: 's1) (_s2: 's2): Compose<'s1,'s2,_,_,_> = Compose.Compose

    let inline runRef s ref ([<InlineIfLambda>] f) =
        let states = ref.contents
        let r, state = f (get s states)
        ref.contents <- wiz s state states
        r
    
    let inline modifyRef s ref ([<InlineIfLambda>] f) =
        let states = ref.contents
        ref.contents <- wiz s (f (get s states)) states
