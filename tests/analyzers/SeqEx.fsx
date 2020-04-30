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
            member __.Current = e.Current
            member __.Dispose() = e.Dispose()

        interface IEnumerator with
            member __.MoveNext() =
                match state with
                | 0 ->
                    let mutable i = 0
                    while count <> i && e.MoveNext() do
                        i <- i + 1

                    state <- 1
                    i = count && e.MoveNext()

                | _ -> e.MoveNext()

            member __.Current = (e :> IEnumerator).Current
            member __.Reset() = raise <| NotImplementedException()
        }
    {
    new seq<_> with
        member __.GetEnumerator() = enumerator()
    interface IEnumerable with
        member __.GetEnumerator() = upcast enumerator()
    }

let splitAt index xs =
    Seq.truncate index xs, trySkip index xs
