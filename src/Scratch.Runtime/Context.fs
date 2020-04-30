namespace Scratch.Primitives

type Context<'Env,'State1,'State2,'Error,'T> = (struct('Env * 'State1) -> Result<struct('T * 'State2),'Error>)
type Context<'Env,'State,'Error,'T> = Context<'Env,'State,'State,'Error,'T>
type ContextBuilder = | ContextBuilder

[<AutoOpen>]
module ContextOperators =
    let context = ContextBuilder

module Context =
    let inline bind f m = fun struct(env, state) ->
        match m struct(env, state) with
        | Error e -> Error e
        | Ok struct(x, state) -> f x struct(env, state)
    let inline private (>>=) m f = bind f m
    let inline return' x = fun struct(_, state) -> Ok struct(x, state)

    let inline mapError mapping context = fun envState ->
        match context envState with
        | Error e -> Error(mapping e)
        | Ok xState -> Ok xState

    let inline combine c1 c2 = bind (fun () -> c2) c1
    let inline error e = fun _ -> Error e
    let inline environment struct(env, state) = Ok struct(env, state)
    let inline local mapping localContext = fun struct(env, state) -> localContext struct(mapping env, state)
    let inline state struct(_, state) = Ok struct(state, state)
    let inline setState state = fun _ -> Ok struct((), state)
    let inline mapState f = fun struct(_, state) -> Ok struct((), f state)

    let inline run context env state = context struct(env, state)
    let inline delay f = fun s -> f () s

    let mapList f xs = fun struct(env, state) ->
        match xs with
        | [] -> Ok struct([], state)
        | xs ->
            let rec aux state acc = function
                | [] -> Ok struct(List.rev acc, state)
                | x::xs ->
                    match f x struct(env, state) with
                    | Ok struct(x, state) -> aux state (x::acc) xs
                    | Error e -> Error e
            aux state [] xs

    let mapSeq f (xs: _ seq) = fun struct(env, state) ->
        match xs with
        | :? (_ list) as xs ->
            match mapList f xs struct(env, state) with
            | Error e -> Error e
            | Ok struct(xs, state) -> Ok struct(xs :> _ seq, state)
            
        | _ ->

        use e = xs.GetEnumerator()
        let results = ResizeArray()
        let mutable state = state
        let mutable result = Unchecked.defaultof<_>
        while
            begin
                if e.MoveNext() then
                    let x = e.Current
                    match f x struct(env, state) with
                    | Ok struct(x, state') ->
                        results.Add x
                        state <- state'
                        true
                    | Error e ->
                        result <- Error e
                        false
                else
                    result <- Ok struct(results :> _ seq, state)
                    false
            end do ()
        result

    let inline apply1 f x1 =
        environment >>= fun env ->
        state >>= fun state ->
        let x, state = f env state x1
        setState state >>= fun () -> return' x

    let inline apply4 f x1 x2 x3 x4 =
        environment >>= fun env ->
        state >>= fun state ->
        let x, state = f env state x1 x2 x3 x4
        setState state >>= fun () ->
        return' x

type ContextBuilder with
    member inline _.Bind(m, f) = Context.bind f m
    member inline _.Return x = Context.return' x
    member inline _.ReturnFrom x = x
    member inline _.Zero() = Context.return' ()
    member inline _.Combine(m1, m2) = Context.combine m1 m2
    member inline _.Delay f = Context.delay f
