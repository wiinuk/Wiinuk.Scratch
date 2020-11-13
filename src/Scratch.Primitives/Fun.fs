namespace Scratch.Primitives

[<AutoOpen>]
module FunOperators =
    let constant x = fun _ -> x

[<AutoOpen>]
module private Hash =
    let getOrAdd x (f: System.Func<_,_>) (m: System.Collections.Concurrent.ConcurrentDictionary<'a,'b>) = m.GetOrAdd(x, f)

module Fun =
    let tryAdapt2 f =
        match box (f: 'a -> 'b -> 'c) with
        | :? OptimizedClosures.FSharpFunc<'a,'b,'c> as f -> ValueSome f
        | _ -> ValueNone

    let memoizeFix f =
        let m = new _()
        let mutable factory = null
        let rec f' x = m |> getOrAdd x factory
        factory <-
            match tryAdapt2 f with
            | ValueSome f -> new _(fun x -> f.Invoke(f', x))
            | _ -> new _(f f')
        f'

    let memoize f = memoizeFix <| fun _ -> f
