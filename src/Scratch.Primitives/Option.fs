module Scratch.Primitives.Option

let mapSeq mapping (xs: #seq<_>) =
    use e = xs.GetEnumerator()
    if not <| e.MoveNext() then Some([] :> _ seq) else
    match mapping e.Current with
    | None -> None
    | Some x ->

    let mutable xs = ResizeArray()
    xs.Add x
    while
        begin
            if e.MoveNext() then
                match mapping e.Current with
                | None -> xs <- null; false
                | Some x -> xs.Add x; true
            else
                false
        end
        do ()
    match xs with
    | null -> None
    | _ -> Some(xs :> _ seq)
