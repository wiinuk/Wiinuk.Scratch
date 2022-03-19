namespace Scratch.Primitives

type ResultBuilder = | ResultBuilder with
    member inline _.Return x = Ok x
    member inline _.ReturnFrom x = x
    member inline _.Bind(x, [<InlineIfLambda>] f) = match x with Ok x -> f x | Error x -> Error x
    member inline _.Zero() = Ok()
    member inline _.Combine(m1, [<InlineIfLambda>] m2) = match m1 with Ok() -> m2() | Error x -> Error x
    member inline _.Delay([<InlineIfLambda>] f: _ -> _) = f
    member inline _.Run([<InlineIfLambda>] f) = f()

[<AutoOpen>]
module ResultOperators =
    let result = ResultBuilder

module Result =
    let isOk = function Ok _ -> true | _ -> false
    let isError = function Error _ -> true | _ -> false
    let defaultValue value = function Error() -> value | Ok x -> x
    let mapSeq mapping (xs: #seq<_>) =
        use e = xs.GetEnumerator()
        if not <| e.MoveNext() then Ok([] :> _ seq) else

        match mapping e.Current with
        | Error e -> Error e
        | Ok x ->

        let xs = ResizeArray()
        let mutable result = Ok(xs :> _ seq)
        xs.Add x
        while
            begin
                if e.MoveNext() then
                    match mapping e.Current with
                    | Error e -> result <- Error e; false
                    | Ok x -> xs.Add x; true
                else
                    false
            end
            do ()
        result

    let mapList f x =
        let rec tryMap f acc = function
            | [] -> List.rev acc |> Ok
            | x::xs ->

            match f x with
            | Error e -> Error e
            | Ok x -> tryMap f (x::acc) xs

        tryMap f [] x
