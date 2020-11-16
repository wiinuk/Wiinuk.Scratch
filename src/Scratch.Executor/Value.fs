namespace Scratch
open NonStructuralComparison
open System
open System.Globalization
open Scratch.Primitives
open Scratch.Ast
open System.Text.RegularExpressions


[<AutoOpen>]
module private ValueInternal =
    let trueObj = box true
    let falseObj = box false
    let nullObj = box DBNull.Value

[<Struct>]
type private DebugValue =
    | Number of doubleValue: double
    | Bool of boolValue: bool
    | String of stringValue: string

[<Struct; CustomEquality; CustomComparison; StructuredFormatDisplay("{Display}")>]
[<NoGcAllocation(AllocatableTypes = [|typeof<PlatformNotSupportedException>|])>]
type Value private (_number: double, _obj: obj) =
    new (numberValue) = Value(numberValue, null)
    new (boolValue) = Value(0., if boolValue then trueObj else falseObj)
    new (stringValue) = Value(0., if isNull stringValue then nullObj else upcast stringValue)

    member x.Tag =
        if isNull _obj then 0 else
        if x.IsBool then 1 else
        2

    member x.Type =
        if isNull _obj then SType.N else
        if x.IsBool then SType.B else
        SType.S

    member _.IsNumber = isNull _obj
    member _.IsBool = obj.ReferenceEquals(_obj, trueObj) || obj.ReferenceEquals(_obj, falseObj)
    member x.IsString = not x.IsNumber && not x.IsBool

    member _.IsTrue = obj.ReferenceEquals(_obj, trueObj)
    member _.IsFalse = obj.ReferenceEquals(_obj, falseObj)
    
    member _.TryGetNumber(result: _ outref) =
        result <- _number
        isNull _obj

    member _.NumberOrNone =
        if isNull _obj then ValueSome _number
        else ValueNone

    member x.TryGetBool(result: _ outref) =
        result <- obj.ReferenceEquals(_obj, trueObj)
        x.IsBool

    member _.BoolOrNone =
        if obj.ReferenceEquals(_obj, trueObj) then ValueSome true
        elif obj.ReferenceEquals(_obj, falseObj) then ValueSome false
        else ValueNone

    member _.TryGetString(result: _ outref) =
        match _obj with
        | :? string as s -> result <- s; true
        | v when obj.ReferenceEquals(v, nullObj) -> result <- null; true
        | _ -> false

    member _.StringOrNone =
        match _obj with
        | :? string as s -> ValueSome s
        | v when obj.ReferenceEquals(v, nullObj) -> ValueSome null
        | _ -> ValueNone

    member _.NumberOrDefault = _number
    member _.BoolOrDefault = obj.ReferenceEquals(_obj, trueObj)
    member _.StringOrDefault = match _obj with :? string as x -> x | _ -> null

    member v1.Equals(v2: Value) =
        let t1 = v1.Tag
        if t1 = v2.Tag then
            match t1 with
            | 0 -> v1.NumberOrDefault = v2.NumberOrDefault
            | 1 -> v1.IsTrue = v2.IsTrue
            | _ -> String.Equals(v1.StringOrDefault, v2.StringOrDefault)
        else
            false

    member v1.CompareTo(v2: Value) =
        let t1 = v1.Tag
        let t2 = v2.Tag
        if t1 = t2 then
            match t1 with
            | 0 ->
                let v1 = v1.NumberOrDefault
                let v2 = v2.NumberOrDefault
                if v1 < v2 then -1
                elif v1 > v2 then 1
                elif v1 = v2 then 0
                elif Double.IsNaN v1 then
                    if Double.IsNaN v2 then 0
                    else -1
                else 1

            | 1 ->
                let v1 = v1.BoolOrDefault
                let v2 = v2.BoolOrDefault
                if (if v1 then 1 else 0) < (if v2 then 1 else 0) then -1
                else if ((if v1 then 1 else 0) > (if v2 then 1 else 0)) then 1 else 0

            | _ ->
                String.CompareOrdinal(v1.StringOrDefault, v2.StringOrDefault)
        else
            t1 - t2

    override v.GetHashCode() =
        match v.Tag with
        | 0 -> v.NumberOrDefault.GetHashCode() - 1640531527
        | 1 -> 64 + (if v.IsTrue then 1 else 0) - 1640531527
        | _ ->
            let v = v.StringOrDefault
            128 + (if isNull v then 0 else v.GetHashCode()) - 1640531527

    override v1.Equals v2 =
        match v2 with
        | :? Value as v2 -> v1.Equals v2
        | _ -> false

    interface IEquatable<Value> with
        override v1.Equals v2 = v1.Equals v2
        
    interface IComparable<Value> with
        override v1.CompareTo v2 = v1.CompareTo v2

    interface IComparable with
        override v1.CompareTo v2 = v1.CompareTo(v2 :?> Value)

    member private v.Display =
        match v.Tag with
        | 0 -> DebugValue.Number v.NumberOrDefault
        | 1 -> DebugValue.Bool v.BoolOrDefault
        | _ -> DebugValue.String v.StringOrDefault

    [<NoGcAllocation(TreatAsNoAllocation = true)>]
    override v.ToString() =
        if v.IsNumber then $"SNumber(%0.17f{v.NumberOrDefault})"
        elif v.IsTrue then "SBool(true)"
        elif v.IsFalse then "SBool(false)"
        else $"SString({v.StringOrDefault})"

module Value =
    module Tags =
        let [<Literal>] Number = 0
        let [<Literal>] Bool = 1
        let [<Literal>] String = 2

    let True = Value true
    let False = Value false
    let Bool x = if x then Value true else Value false
    let EmptyString = Value(stringValue = "")
    let String x = Value(stringValue = x)
    let Number x = Value(numberValue = x)

    let tag (x: Value) = x.Tag
    let isNumber (x: Value) = x.IsNumber
    let isString (x: Value) = x.IsString
    let isBool (x: Value) = x.IsBool
    let isTrue (x: Value) = x.IsTrue
    let isFalse (x: Value) = x.IsFalse
    let stringOrDefault (x: Value) = x.StringOrDefault

    let toNumber x =
        match tag x with
        | 0 -> x.NumberOrDefault
        | 1 -> if x.BoolOrDefault then 1. else 0.
        | _ -> SValue.stringToNumber x.StringOrDefault

    let toNumberZeroIfNaN x =
        let x = toNumber x
        if Double.IsNaN x then 0. else x

    let toBool x =
        if isNumber x then x.NumberOrDefault <> 0. else
        if x.IsTrue then true else
        if x.IsFalse then false else
        match x.StringOrDefault with
        | ""
        | "false" -> false
        | x -> SValue.stringToNumber x <> 0.

    let toString x =
        if isNumber x then
            let x = x.NumberOrDefault

            // `0` -or- `-0`
            if x = 0. then "0"

            elif Double.IsNaN x then "NaN"
            elif Double.IsPositiveInfinity x then "Infinity"
            elif Double.IsNegativeInfinity x then "-Infinity"

            // round trip
            else x.ToString "G17"

        elif x.IsTrue then "true"
        elif x.IsFalse then "false"
        else x.StringOrDefault

    let ofSValue = function
        | SBool x -> Bool x
        | SNumber x -> Number x
        | SString x -> String x

    let toSValue v =
        if isNumber v then SNumber v.NumberOrDefault
        elif v.IsTrue then SBool true
        elif v.IsFalse then SBool false
        else SString v.StringOrDefault

    let private digitRegex = Regex @"^\d+$"
    let private tryParseDigits x (result: _ outref) =
        let x = toString x
        if digitRegex.IsMatch x then
            result <- double x
            true
        else
            false

    let tryToNumberWithoutNaN x (result: _ outref) =
        if isNumber x then
            result <- x.NumberOrDefault
            not (Double.IsNaN result)
        else
            tryParseDigits x &result

    let equals x1 x2 =
        let mutable n1 = 0.
        let mutable n2 = 0.
        if tryToNumberWithoutNaN x1 &n1 && tryToNumberWithoutNaN x2 &n2 then
            n1 = n2
        else
            let x1 = toString x1
            let x2 = toString x2
            System.String.Equals(x1, x2, StringComparison.OrdinalIgnoreCase)

    let equalsToNumber x1 n2 =
        let mutable n1 = 0.
        if tryToNumberWithoutNaN x1 &n1 then
            n1 = n2
        else
            let x1 = toString x1
            let x2 = toString(Number n2)
            System.String.Equals(x1, x2, StringComparison.OrdinalIgnoreCase)

    let compare x1 x2 =
        let mutable n1, n2 = 0., 0.
        if tryToNumberWithoutNaN x1 &n1 && tryToNumberWithoutNaN x2 &n2 then
            if n1 < n2 then -1 elif n1 = n2 then 0 else 1
        else
            let x1 = toString x1
            let x2 = toString x2
            System.String.Compare(x1, x2, StringComparison.OrdinalIgnoreCase)
