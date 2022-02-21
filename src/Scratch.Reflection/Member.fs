module Scratch.Reflection.Member
open FSharp.Reflection
open Scratch.Reflection.Expr
module E = FSharp.Quotations.Patterns
module VOption = ValueOption
type private B = System.Reflection.BindingFlags


let tryFindProperty e =
    e
    |> tryPick (function
        | E.PropertyGet(_, p, _)
        | E.PropertySet(_, p, _, _) -> ValueSome p
        | _ -> ValueNone
    )
let findProperty e =
    tryFindProperty e
    |> VOption.defaultWith (fun _ ->
        invalidArg "e" "e.g. <@ fun x -> x.field @>"
    )

let tryFindMethod e =
    e
    |> tryPick (function
        | E.Call(_, GenericMethodDefinition m' & m, _) ->
            ValueSome(m', Array.toList <| m.GetGenericArguments())
        | _ -> ValueNone
    )

let findMethod e =
    tryFindMethod e
    |> VOption.defaultWith (fun _ ->
        invalidArg "e" "e.g. <@ f @>"
    )

let findField field =
    field
    |> tryPick (function
        | E.FieldGet(Some _, f) -> ValueSome f
        | _ -> ValueNone
    )
    |> VOption.orElseWith(fun _ ->
        tryFindProperty field
        |> VOption.bind (fun p ->
            let t = p.DeclaringType
            if FSharpType.IsRecord(t, allowAccessToPrivateRepresentation = true) then
                t.GetField(p.Name + "@", B.Instance ||| B.Public ||| B.NonPublic)
                |> VOption.ofObj
            else
                ValueNone
        )
    )
    |> VOption.defaultWith (fun () -> failwith $"field not found: %A{field}")

let genericTypeGenericMethodDefinition m = genericTypeGenericMethodDefinition m
