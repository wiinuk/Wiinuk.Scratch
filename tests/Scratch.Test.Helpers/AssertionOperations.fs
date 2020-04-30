namespace global
open FsCheck
open System
open Xunit


[<Struct; CustomEquality; CustomComparison; StructuredFormatDisplay("{Display}")>]
type ([<EqualityConditionalOn; ComparisonConditionalOn>] 'a) CustomToString = {
    value: 'a
    show: 'a -> string
}
    with
    member x.Display = x.show x.value
    override x.ToString() = x.show x.value
    override x.Equals y =
        match y with
        | :? ('a CustomToString) as y -> Unchecked.equals x.value y.value
        | _ -> false
    override x.GetHashCode() = Unchecked.hash x.value
    interface 'a CustomToString IComparable with
        override x.CompareTo y = Unchecked.compare x.value y.value

[<AutoOpen>]
module private AssertionOperationHelpers =
    let qcheckConfig = { Config.QuickThrowOnFailure with QuietOnSuccess = true }

[<AutoOpen>]
module AssertionOperations =
    let (=?) l r =
        if not <| LanguagePrimitives.GenericEqualityER l r then
            Assert.True(false, sprintf "%A =? %A" l r)

    let (<>?) l r =
        if LanguagePrimitives.GenericEqualityER l r then
            Assert.True(false, sprintf "%A <>? %A" l r)

    let throws x =
        let r = try let (Lazy x) = x in Ok x with e -> Error e
        match r with
        | Ok x -> failwithf "throws %A" x
        | _ -> ()

    let qcheckWith withConfig property =
        Check.One(withConfig qcheckConfig, property)

    let qcheck property = qcheckWith id property
