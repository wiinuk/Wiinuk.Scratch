namespace Scratch.Primitives

type IHigherKind = interface end
type IShape = interface end

type IHasDefaultShape<'T> =
    inherit IShape
    abstract GetDefault: unit -> 'T

module HasDefault =
    let inline getDefault (_s: '_s The when '_s :> IHasDefaultShape<_>) = Unchecked.defaultof<'_s>.GetDefault()

    [<Struct; NoEquality; NoComparison>]
    type Option<'T> =
        interface IHasDefaultShape<'T option> with
            member _.GetDefault() = None

    [<Struct; NoEquality; NoComparison>]
    type UncheckedDefault<'T> =
        interface IHasDefaultShape<'T> with
            member _.GetDefault() = Unchecked.defaultof<_>

    let option: 't Option The = The
    /// Unchecked.defaultof<_>
    let unchecked: 't UncheckedDefault The = The
    let unit: unit UncheckedDefault The = The
