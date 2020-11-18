namespace Scratch.MemoryModel
open Scratch
open Scratch.Ast


type IWord =
    abstract Value: SValue

[<Struct>]
type V = V of SValue with
    interface IWord with
        override x.Value = let (V x) = x in x

[<Struct>]
type S = S of string with
    interface IWord with
        override x.Value = let (S x) = x in SString x
    static member op_Implicit x = S x
    static member op_Implicit(S x) = x
    [<Block(Symbol.``concatenate:with:``)>]
    static member (+) (S x, S y) = S(x + y)

type N = NWithUnit<1>
and N<[<Measure>]'u> = NWithUnit<'u>
and [<Struct>] NWithUnit<[<Measure>]'u> = N of float<'u> with
    interface IWord with
        override x.Value = let (N x) = x in SNumber(double<float<'u>> x)
    static member op_Implicit< >(x: float<'u>) = N x
    static member op_Implicit< >(x: int<'u>) = N(LanguagePrimitives.FloatWithMeasure<'u>(float<int<'u>> x))
    static member op_Implicit< >(N x: N<'u>) = x
    static member op_Implicit< >(N x: N<'u>) = LanguagePrimitives.Int32WithMeasure<'u>(Checked.int<float<'u>> x)

    [<Block(Symbol.``+``)>]
    static member (+) (N x, N y) = N(x + y)
    [<Block(Symbol.``-``)>]
    static member (-) (N x, N y) = N(x - y)
    [<Block(Symbol.``*``)>]
    static member (*) (N x, N y) = N(x * y)
    [<Block(Symbol.``/``)>]
    static member (/) (N x, N y) = N(x / y)
    static member (~-) (N x) = N -x
    static member Floor (N x) = N (LanguagePrimitives.FloatWithMeasure<'u0>(floor(double<float<'u0>> x)))
    static member WithMeasure(N x) = N(LanguagePrimitives.FloatWithMeasure(double x))

[<Struct>]
type B = B of bool with
    interface IWord with
        override x.Value = let (B n) = x in SBool n
    static member op_Implicit x = B x
    static member op_Implicit x = let (B x) = x in x

[<AutoOpen>]
module WordOperations =
    let toV (x: #IWord) = x.Value

    let enumLikeUnionToV (x: 'T when 'T : not struct) =
        let u, _ = FSharp.Reflection.FSharpValue.GetUnionFields(x, x.GetType())
        SString u.Name

    /// impl (n + 0)
    let toN<'a when 'a :> IWord> (w: 'a) = N(SValue.toNumber(toV w))

    /// impl (combine n "")
    let toS<'a when 'a :> IWord> (w: 'a) = S(SValue.toString(toV w))

[<Struct>]
type 'T Size = Size of int
with
    interface IWord with
        override x.Value = let (Size x) = x in SNumber(double x)

type Uninit = | Uninit
with
    interface IWord with
        override _.Value = SString "uninit value"
