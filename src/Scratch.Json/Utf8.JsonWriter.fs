[<AutoOpen>]
module internal Scratch.Json.Utf8.JsonWriter
open Utf8Json

let writeBeginArray (w: JsonWriter byref) = w.WriteBeginArray()
let writeBeginObject (w: JsonWriter byref) = w.WriteBeginObject()
let writeRaw (w: JsonWriter byref) (rawValue: byte[]) = w.WriteRaw rawValue
