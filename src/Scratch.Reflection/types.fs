namespace Scratch.Reflection

[<Struct>]
type Position = {
    line: int
    column: int
}

[<Struct>]
type Location = {
    path: string
    position1: Position
    position2: Position
}
module Location =
    let merge l1 l2 =
        match l1, l2 with
        | None, None -> None
        | (Some _ as l), None
        | None, (Some _ as l) -> l
        | Some l1 as l, Some l2 when l1.path <> l2.path -> l
        | Some l1, Some l2 -> Some {
            path = l1.path
            position1 = min l1.position1 l2.position1
            position2 = max l1.position2 l2.position2
        }

type Cost =
    | LiteralLike
    | Pure
    | HasSideEffect
    | Unknown

module Cost =
    let min = LiteralLike
    let max = Unknown