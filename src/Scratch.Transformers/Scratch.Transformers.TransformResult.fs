module Scratch.Transformers.TransformResult
open Scratch.Primitives
open System
open System.Collections.Generic
type private T = Scratch.Transformers.ModifyTag


/// not (isNoModify r)
let isModified r = r.tag &&& T.Modified <> T.None
/// not (isModified r) && not (isRemoved r)
let isNoModify r = r.tag &&& T.Modified = T.None
let isSkip r = r.tag &&& T.Skip <> T.None
/// isModified r && not (isNoModify r)
let isRemoved r = r.tag &&& T.RemovedMask <> T.None
let valueOrOld oldValue r = if isNoModify r then oldValue else r.value

let inline private map mapping r = { tag = r.tag; value = mapping r.value }
/// if (isNoModify x || isRemoved x) then oldValue else f x
let inline mapIfModified oldValue f x =
    if isNoModify x || isRemoved x then { tag = x.tag; value = oldValue }
    else { tag = x.tag; value = f x.value }

/// not (isSkip result) && isNoMoify result && not (isModified result) && not (isRemoved result)
let noModify x = { tag = T.None; value = x }
/// not (isSkip result) && not (isNoModify result) && isModified result && not (isRemoved result)
let modified x = { tag = T.Modified; value = x }
/// isSkip result && isNoModify result && not (isModified result) && not (isRemoved result)
let skip x = { tag = T.Skip; value = x }
/// not (isSkip result) && isModified result && not (isModified result) && isRemoved result
let removed x = { tag = T.ModifyRemoved; value = x }

/// isSkip result = v
let withIsSkip v x = {
    tag = if v then x.tag ||| T.Skip else x.tag &&& ~~~T.Skip
    value = x.value
}
/// isModified result = v && isNoModify result = not v
let private withIsModified v x = {
    tag = if v then x.tag ||| T.Modified else x.tag &&& ~~~T.Modified
    value = x.value
}

let unsetRemoved t = t &&& ~~~T.RemovedMask
let mergeTag t1 t2 = (t1 ||| t2) &&& ~~~T.RemovedMask

/// not (isRemoved result)
let mergeState destination target = {
    tag = mergeTag destination.tag target.tag
    value = target.value
}

let inline sequence2 f1 f2 = fun struct(x1, x2) ->
    let r1 = f1 x1
    let x1 = if isNoModify r1 then x1 else r1.value
    if isSkip r1 then
        { tag = unsetRemoved r1.tag; value = struct(x1, x2) }
    else
    
    let r2 = f2 x2
    let x2 = if isNoModify r2 then x2 else r2.value
    {
        tag = mergeTag r1.tag r2.tag
        value = struct(x1, x2)
    }

let inline parallel2 f1 f2 = fun struct(x1, x2) ->
    let r1 = f1 x1
    let r2 = f2 x2
    {
        tag = mergeTag r1.tag r2.tag
        value =
            if isNoModify r1 && isNoModify r2 then struct(x1, x2)
            else struct(r1.value, r2.value)
    }

type IMapFoldListMapping<'T,'State> =
    abstract Invoke<'S> : PooledBuffer<'T,'S> * 'State * 'T -> struct('T TransformResult * 'State)

let rec private mapFoldListVAux (mapping: 'M byref when 'M :> IMapFoldListMapping<_,_>) list acc state = function
    | [] ->
        if isNoModify acc then
            struct(noModify list, state)
        else
            struct({ tag = acc.tag; value = PooledBuffer.toList acc.value }, state)

    | head::tail as headtail ->

    let struct(head', state) = mapping.Invoke(acc.value, state, head)

    // 要素の変換が中断されたので、蓄積と残りを結合して中断を伝える
    if isSkip head' then
        let list =
            if isModified head' then PooledBuffer.appendToList acc.value (head'.value::tail)
            elif isModified acc then PooledBuffer.appendToList acc.value headtail
            else list

        struct({ tag = mergeTag acc.tag head'.tag; value = list }, state)

    // 要素の変更はなかったので、残りの変換を続ける
    elif isNoModify head' then
        PooledBuffer.add acc.value head
        mapFoldListVAux &mapping list acc state tail

    // 要素が削除されたのでリストは変更された
    elif isRemoved head' then
        mapFoldListVAux &mapping list (withIsModified true acc) state tail

    // 他の変更
    else
        PooledBuffer.add acc.value head'.value
        let acc = acc |> withIsModified true
        mapFoldListVAux &mapping list acc state tail

[<Struct>]
type private MapFoldListV<'T,'S,'F> when 'F :> IMapFoldListMapping<'T,'S> and 'F : struct = { mutable mapping: 'F; state: 'S; list: 'T list } with
    interface IPooledBufferUser<'T, struct('T list TransformResult * 'S)> with
        member u.Using buffer = mapFoldListVAux &u.mapping u.list (noModify buffer) u.state u.list

/// not (isRemoved result)
let mapFoldListV mapping state list =
    let mutable f = { mapping = mapping; state = state; list = list }
    PooledBuffer.using &f

[<Struct>]
type private MapListMapping<'a> = { f: 'a -> 'a TransformResult } with
    interface IMapFoldListMapping<'a, HUnit> with
        member f.Invoke(_, _, x) = struct(f.f x, HUnit)

/// not (isRemoved result)
let mapList f xs =
    let struct(xs, _) = mapFoldListV { MapListMapping.f = f } HUnit xs
    xs

let inline compose t1 t2 x env =
    let x' = t1 x env
    let x'' = t2 x'.value env
    { tag = x'.tag ||| x''.tag; value = x''.value }
