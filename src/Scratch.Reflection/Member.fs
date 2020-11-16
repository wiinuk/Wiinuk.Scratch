module Scratch.Reflection.Member
open FSharp.Reflection
open Scratch.Reflection.Expr
module E = FSharp.Quotations.Patterns
type private B = System.Reflection.BindingFlags


let tryFindProperty e =
    e
    |> tryPick (function
        | E.PropertyGet(_, p, _)
        | E.PropertySet(_, p, _, _) -> Some p
        | _ -> None
    )
let findProperty e =
    tryFindProperty e
    |> Option.defaultWith (fun _ ->
        invalidArg "e" "e.g. <@ fun x -> x.field @>"
    )

let tryFindMethod e =
    e
    |> tryPick (function
        | E.Call(_, GenericMethodDefinition m' & m, _) ->
            Some(m', Array.toList <| m.GetGenericArguments())
        | _ -> None
    )

let findMethod e =
    tryFindMethod e
    |> Option.defaultWith (fun _ ->
        invalidArg "e" "e.g. <@ f @>"
    )

let findField field =
    field
    |> tryPick (function
        | E.FieldGet(Some _, f) -> Some f
        | _ -> None
    )
    |> Option.orElseWith(fun _ ->
        tryFindProperty field
        |> Option.bind (fun p ->
            let t = p.DeclaringType
            if FSharpType.IsRecord(t, allowAccessToPrivateRepresentation = true) then
                t.GetField(p.Name + "@", B.Instance ||| B.Public ||| B.NonPublic)
                |> Option.ofObj
            else
                None
        )
    )
    |> Option.defaultWith (fun () -> failwith $"field not found: %A{field}")

let genericTypeGenericMethodDefinition m = genericTypeGenericMethodDefinition m
