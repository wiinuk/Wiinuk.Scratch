namespace Scratch.Primitives

type IFunc<'T,'R> =
    abstract Invoke: x: 'T -> 'R

type IFuncR<'T,'R> =
    abstract Invoke: x: 'T byref -> 'R
    
type IFuncR2<'T1,'T2,'R> =
    abstract Invoke: x1: 'T1 byref * x2: 'T2 byref -> 'R
    
type IFuncR3<'T1,'T2,'T3,'R> =
    abstract Invoke: x1: 'T1 byref * x2: 'T2 byref * x3: 'T3 byref -> 'R

type IFuncI3<'T1,'T2,'T3,'R> =
    abstract Invoke: x1: 'T1 inref * x2: 'T2 inref * x3: 'T3 inref -> 'R

type func<'T,'R> = IFunc<'T,'R>
module Func =
    let invoke (f: #IFunc<_,_> byref) x = f.Invoke x

    [<Struct; NoEquality; NoComparison; AutoSerializable false>]
    type Id<'T> = | Id with
        interface IFunc<'T,'T> with
            member _.Invoke x = x
    let id = Id

    [<Struct; NoEquality; NoComparison; AutoSerializable false>]
    type Constant<'T,'R> = internal { x: 'R } with
        interface IFunc<'T,'R> with
            member f.Invoke _ = f.x

    let constant x = { Constant.x = x }

    let boxFun (f: #IFunc<_,_> inref) = f.Invoke

    [<Struct; NoEquality; NoComparison; AutoSerializable false>]
    type OfFun<'T,'R> = internal { f: 'T -> 'R } with
        interface IFunc<'T,'R> with
            member f.Invoke x = f.f x

    let ofFun f = { OfFun.f = f }

    [<Struct; NoEquality; NoComparison; RequireQualifiedAccess; AutoSerializable false>]
    type OfFun2<'T1,'T2,'R> = private { f: OptimizedClosures.FSharpFunc<'T1,'T2,'R> } with
        interface IFunc<struct('T1 * 'T2),'R> with
            member f.Invoke struct(x1, x2) = f.f.Invoke(x1, x2)

    let ofFun2 f = { OfFun2.f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f }

    [<Struct; NoEquality; NoComparison; AutoSerializable false>]
    type ToFuncR<'T,'R,'F> when 'F :> IFunc<'T,'R> = private { mutable f: 'F } with
        interface IFuncR<'T,'R> with
            member f.Invoke x = f.f.Invoke x

    let toFuncR (f: _ inref) = { ToFuncR.f = f }

    [<Struct; NoEquality; NoComparison; AutoSerializable false>]
    type ToFuncR2<'T1,'T2,'R,'F> when 'F :> IFunc<struct('T1 * 'T2),'R> = private { mutable f: 'F } with
        interface IFuncR2<'T1,'T2,'R> with
            member f.Invoke(x1, x2) = f.f.Invoke struct(x1, x2)

    let toFuncR2 (f: _ inref) = { ToFuncR2.f = f }

module FuncR =
    let invoke (f: #IFuncR<_,_> byref) (x: _ byref) = f.Invoke &x

module FuncR2 =
    let invoke (f: #IFuncR2<_,_,_> byref) (x1: _ byref) (x2: _ byref) = f.Invoke(&x1, &x2)

module FuncR3 =
    let invoke (f: #IFuncR3<_,_,_,_> byref) (x1: _ byref) (x2: _ byref) (x3: _ byref) = f.Invoke(&x1, &x2, &x3)

module FuncI3 =
    let invoke (f: #IFuncI3<_,_,_,_> byref) (x1: _ inref) (x2: _ inref) (x3: _ inref) = f.Invoke(&x1, &x2, &x3)
