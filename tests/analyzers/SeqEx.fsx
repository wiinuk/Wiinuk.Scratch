module Seq
open System
open System.Collections
open System.Collections.Generic


let trySkip count (source: _ seq) =
    let enumerator() =
        let mutable e = source.GetEnumerator()
        let mutable state = 0
        {
        new IEnumerator<_> with
            member _.Current = e.Current
            member _.Dispose() = e.Dispose()

        interface IEnumerator with
            member _.MoveNext() =
                match state with
                | 0 ->
                    let mutable i = 0
                    while count <> i && e.MoveNext() do
                        i <- i + 1

                    state <- 1
                    i = count && e.MoveNext()

                | _ -> e.MoveNext()

            member _.Current = (e :> IEnumerator).Current
            member _.Reset() = raise <| NotImplementedException()
        }
    {
    new seq<_> with
        member _.GetEnumerator() = enumerator()
    interface IEnumerable with
        member _.GetEnumerator() = upcast enumerator()
    }

let splitAt index xs =
    Seq.truncate index xs, trySkip index xs
