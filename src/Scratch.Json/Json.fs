namespace Scratch.Json
open Scratch.Primitives

[<Struct>]
type Json<'T> = Json of location: 'T option * token: 'T JsonToken
and JsonToken<'T> =
    | JNull
    | JTrue
    | JFalse
    | JNumber of double
    | JString of string
    | JArray of 'T Json list
    | JObject of OMap<string, 'T Json>

module JsonToken =
    [<AbstractClass; Sealed>]
    type private EmptyArrayHolder<'T> private () =
        static let value: 'T JsonToken = JArray []
        static member inline Value = value

    [<AbstractClass; Sealed>]
    type private EmptyObjectHolder<'T> private () =
        static let value: 'T JsonToken = JObject OMap.empty
        static member inline Value = value

    [<GeneralizableValue>]
    let emptyArray<'T> = EmptyArrayHolder<'T>.Value
    [<GeneralizableValue>]
    let emptyObject<'T> = EmptyObjectHolder<'T>.Value

module Json =
    let jnull = Json(None, JNull)
    let jtrue = Json(None, JTrue)
    let jfalse = Json(None, JFalse)
    let jbool x = if x then jtrue else jfalse
    let jstring x = Json(None, JString x)
    let jnumber x = Json(None, JNumber x)
    let jobject map = Json(None, JObject map)
    let jarray xs = Json(None, JArray xs)

    let emptyArray = Json(None, JsonToken.emptyArray)
    let emptyObject = Json(None, JsonToken.emptyObject)

    let equalsWith locationEquals x1 x2 =
        let rec equalsWith (eq: OptimizedClosures.FSharpFunc<_,_,_>) (Json(l1, t1)) (Json(l2, t2)) =
            if not (eq.Invoke(l1, l2)) then false else

            match t1, t2 with
            | JNull, JNull
            | JTrue, JTrue
            | JFalse, JFalse -> true
            | JNumber x1, JNumber x2 -> x1 = x2
            | JString x1, JString x2 -> x1 = x2
            | JArray xs1, JArray xs2 ->
                let rec aux = function
                    | [], [] -> true
                    | x1::xs1, x2::xs2 when equalsWith eq x1 x2 -> aux (xs1, xs2)
                    | _ -> false
                aux (xs1, xs2)

            | JObject map1, JObject map2 ->
                match OMap.isEmpty map1, OMap.isEmpty map2 with
                | true, true -> true
                | false, false ->
                    use e1 = OMap.toSeqSorted(map1).GetEnumerator()
                    use e2 = OMap.toSeqSorted(map2).GetEnumerator()
                    let rec aux() =
                        match e1.MoveNext(), e2.MoveNext() with
                        | false, false -> true
                        | true, true ->
                            let kv1 = e1.Current
                            let kv2 = e2.Current
                            if kv1.Key = kv2.Key && equalsWith eq kv1.Value kv2.Value then
                                aux()
                            else
                                false
                        | _ -> false
                    aux()

                | _ -> false
            | _ -> false

        let locationEquals = OptimizedClosures.FSharpFunc<_,_,_>.Adapt locationEquals
        equalsWith locationEquals x1 x2

    let tokenEquals x1 x2 = equalsWith (fun _ _ -> true) x1 x2

[<Struct>]
type JsonObject<'T> = JsonObject of location: 'T option * token: OMap<string,'T Json>
[<Struct>]
type JsonArray<'T> = JsonArray of location: 'T option * tyoken: 'T Json list


[<Struct>]
type Member =
    | Index of index: int
    | Property of propertyName: string

[<Struct>]
type Position = { 
    line: int
    column: int
}
[<Struct>]
type Location = {
    name: string
    position1: Position
    position2: Position
}
type TokenInfo = {
    location: Location
    path: Member list
}
