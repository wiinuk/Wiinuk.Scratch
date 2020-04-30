module List

let rec revAppend revList1 list2 =
    match revList1 with
    | [] -> list2
    | x::xs -> revAppend xs (x::list2)
