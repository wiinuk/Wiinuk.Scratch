[<AutoOpen>]
module internal Scratch.Json.Utf8.Internal
open Utf8Json
open Scratch.Json.PartialIsomorphisms
type T = Utf8Json.JsonToken


[<AutoOpen>]
module ObjectParsingInfo =
    let setIsoError (c: ObjectParsingInfo byref) error =
        c.error <- { messageOrExn = error; reader = c.lastReader }
        false

type KeySerializer = {
    /// `"pname":`
    propertyName: byte array
    /// `,"pname":`
    propertyNameWithPrefixValueSeparator: byte array
    errorMessage: IsoError
}
let serializeKey s (w: _ byref) n =
    if n = 0 then writeRaw &w s.propertyName
    else w.WriteRaw s.propertyNameWithPrefixValueSeparator

let makeKeySerializer key = {
    propertyName = JsonWriter.GetEncodedPropertyName key
    propertyNameWithPrefixValueSeparator = JsonWriter.GetEncodedPropertyNameWithPrefixValueSeparator key
    errorMessage = IsoError.ofMessage $"property: %A{key}"
}
