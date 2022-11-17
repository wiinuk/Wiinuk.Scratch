module Scratch.Json.Parser.Test.Helpers
open FsCheck
open Scratch.Primitives
open Scratch.Json
module Map = OMap


let primitiveToken() = Gen.frequency [
    1, gen { return JNull }
    2, gen {
        match! Arb.generate<bool> with
        | true -> return JTrue
        | false -> return JFalse
    }
    5, gen {
        let! NormalFloat x = Arb.generate
        return JNumber x
    }
    5, gen {
        let! NonNull x = Arb.generate
        return JString x
    }
]
let toJson token = gen {
    let! l = Arb.generate
    let! t = token
    return Json(l, t)
}
let rec tokenSized = function
    | 0 -> primitiveToken()
    | n ->

    let jsonGen = toJson <| tokenSized (n / 2)

    let jArrayToken = gen {
        let! xs = Gen.listOf jsonGen
        return JArray xs
    }
    let jProperty = gen {
        let! NonNull k = Arb.generate
        let! v = jsonGen
        return k, v
    }
    let jObjectToken = gen {
        let! xs = Gen.listOf jProperty
        return JObject(Map.ofList xs)
    }
    Gen.frequency [
        2, primitiveToken()
        1, jArrayToken
        1, jObjectToken
    ]
let genJsonToken() = Gen.sized tokenSized
let genJson() = toJson <| genJsonToken()

let rec shrinkList = function
    | [] -> Seq.empty
    | x::xs -> seq {
        xs
        for xs in shrinkList xs do x::xs
        for x in Arb.shrink x do x::xs
    }

let shrinkJsonToken t = seq {
    match t with
    | JNull -> ()
    | JFalse -> JNull
    | JTrue -> JFalse
    | JNumber 0.0 -> JTrue
    | JNumber n ->
        JNumber 0.
        for n in Arb.shrink n do
        JNumber n

    | JString null -> ()
    | JString "" -> JNumber 0.
    | JString s ->
        JString ""
        for s in Arb.shrink s do
        JString s

    | JArray [] -> JString ""
    | JArray xs ->
        for xs in shrinkList xs do
        JArray xs

    | JObject map ->
        if Map.isEmpty map then JArray [] else

        for xs in map |> Map.toListOrdered |> shrinkList do
        JObject <| Map.ofList xs
}

let shrinkJson (Json(l, t)) = Arb.shrink (l, t) |> Seq.map Json

type Arbs =
    static member JsonToken() = Arb.fromGenShrink(genJsonToken(), shrinkJsonToken)
    static member Json() = Arb.fromGenShrink(genJson(), shrinkJson)

let rec unLocation (Json(_, t)) =
    match t with
    | JNull
    | JTrue
    | JFalse
    | JNumber _
    | JString _ -> Json(None, t)
    | JArray [] -> Json.emptyArray
    | JArray xs -> Json(None, JArray(List.map unLocation xs))
    | JObject map when Map.isEmpty map -> Json.emptyObject
    | JObject map -> Json(None, JObject(Map.map (fun _ x -> unLocation x) map))

let quickcheck test = qcheckWith (fun c -> { c with Arbitrary = typeof<Arbs> :: c.Arbitrary }) test

let json t = Json(None, t)
