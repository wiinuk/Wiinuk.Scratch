namespace Scratch.Primitives

type IGeneratorBuilder = inherit ISyntaxInfrastructure

[<NoComparison; NoEquality>]
type FiberBuilder = | FiberBuilder
with
    interface IGeneratorBuilder

type FiberBuilder with
    member inline _.Bind(x, f) = Fiber.bind (let f = Func.ofFun f in &f) &x
    member inline _.Return x = Fiber.result x
    member inline _.ReturnFrom x = x
    member inline _.Zero() = Fiber.zero
    member inline _.Yield x = Fiber.singleton x
    member inline _.YieldFrom x = x

module FiberBuilderDelay =
    type FiberBuilder with
        member inline _.Delay x = x
        member inline _.Combine(x1, cont) = Fiber.combine &x1 (let cont = Func.ofFun cont in &cont)
        member inline _.For(xs, action) = Fiber.for' action xs
        // <@ while %e1 do %e2 @> => <@ $b.While((fun () -> %e1), $b.Delay(fun () -> %(c e2))) @>
        member inline _.While(test, body) = Fiber.while' (let test = Func.ofFun test in &test) (let body = Func.ofFun body in &body)
        // <@ try %e1 with | .. | $pi -> %ci @> => <@ $b.TryWith($b.Delay(fun () -> %(c e1)), function | .. | $pi -> %(c ci)) @>
        member inline _.TryWith(body, handler) = Fiber.tryWith (let body = Func.ofFun body in &body) (let handler = Func.ofFun handler in &handler)
        member inline _.Run f = Fiber.delay (let f = Func.ofFun f in &f)

type FiberBuilderPoly = | FiberBuilderPoly
with
    interface IGeneratorBuilder

    member inline _.Bind(x: #fiber<_,_,_>, f): fiber<_,_,_> = upcast Fiber.bind (let f = Func.ofFun f in &f) &x
    member inline _.Return x: fiber<_,_,_> = upcast Fiber.result x
    member inline _.ReturnFrom x = x
    member inline _.Zero(): fiber<_,_,_> = upcast Fiber.zero
    member inline _.Yield x: fiber<_,_,_> = upcast Fiber.singleton x
    member inline _.YieldFrom x = x

    member inline _.Delay x = x
    member inline _.Combine(x1: #fiber<_,_,_>, cont): fiber<_,_,_> = upcast Fiber.combine &x1 (let cont = Func.ofFun cont in &cont)
    member inline _.For(xs, action): fiber<_,_,_> = upcast Fiber.for' action xs
    member inline _.While(test, body): fiber<_,_,_> = upcast Fiber.while' (let test = Func.ofFun test in &test) (let body = Func.ofFun body in &body)
    member inline _.TryWith(body, handler): fiber<_,_,_> = upcast Fiber.tryWith (let body = Func.ofFun body in &body) (let handler = Func.ofFun handler in &handler)
    member inline _.Run f = Fiber.delay (let f = Func.ofFun f in &f)

[<Struct>]
[<NoComparison; NoEquality>]
type FiberBuilderPolyWith<'T,'E,'R,'C> = FiberBuilderPolyWith of continuation: (fiber<'T,'E,'R> -> 'C)
with
    interface IGeneratorBuilder

type FiberBuilderPolyWith<'T,'E,'R,'C> with
    member inline _.Bind(x: #fiber<_,_,_>, f): fiber<_,_,_> = upcast Fiber.bind (let f = Func.ofFun f in &f) &x
    member inline _.Return x: fiber<_,_,_> = upcast Fiber.result x
    member inline _.ReturnFrom x = x
    member inline _.Zero(): fiber<_,_,_> = upcast Fiber.zero
    member inline _.Yield x: fiber<_,_,_> = upcast Fiber.singleton x
    member inline _.YieldFrom x = x

    member inline _.Delay x = x
    member inline _.Combine(x1: #fiber<_,_,_>, cont): fiber<_,_,_> = upcast Fiber.combine &x1 (let cont = Func.ofFun cont in &cont)
    member inline _.For(xs, action): fiber<_,_,_> = upcast Fiber.for' action xs
    member inline _.While(test, body): fiber<_,_,_> = upcast Fiber.while' (let test = Func.ofFun test in &test) (let body = Func.ofFun body in &body)
    member inline _.TryWith(body, handler): fiber<_,_,_> = upcast Fiber.tryWith (let body = Func.ofFun body in &body) (let handler = Func.ofFun handler in &handler)

    member inline b.Run f =
        let f = Fiber.delay (let f = Func.ofFun f in &f)
        let (FiberBuilderPolyWith b) = b in b f

[<Struct; NoComparison; NoEquality>]
type GeneratorPolyWith<'F,'T,'E,'R,'C> when 'F :> fiber<'T,'E,'R> =
    | GeneratorPolyWith of continuation: (Generator.Delay<'F,'T,'E,'R> -> 'C)
with
    interface IGeneratorBuilder

type GeneratorPolyWith<'F,'T,'E,'R,'C> when 'F :> fiber<'T,'E,'R> with
    member inline _.Bind(x: #fiber<_,_,_>, f): fiber<_,_,_> = upcast Fiber.bind (let f = Func.ofFun f in &f) &x
    member inline _.Return x: fiber<_,_,_> = upcast Fiber.result x
    member inline _.ReturnFrom x = x
    member inline _.Zero(): fiber<_,_,_> = upcast Fiber.zero
    member inline _.Yield x: fiber<_,_,_> = upcast Fiber.singleton x
    member inline _.YieldFrom x = x

    member inline _.Delay x = x
    member inline _.Combine(x1: #fiber<_,_,_>, cont): fiber<_,_,_> = upcast Fiber.combine &x1 (let cont = Func.ofFun cont in &cont)
    member inline _.For(xs, action): fiber<_,_,_> = upcast Fiber.for' action xs
    member inline _.While(test, body): fiber<_,_,_> = upcast Fiber.while' (let test = Func.ofFun test in &test) (let body = Func.ofFun body in &body)
    member inline _.TryWith(body, handler): fiber<_,_,_> = upcast Fiber.tryWith (let body = Func.ofFun body in &body) (let handler = Func.ofFun handler in &handler)

    member inline b.Run f = let (GeneratorPolyWith b) = b in b (Generator.delay f)

[<AutoOpen>]
module FiberBuilderOperations =
    let fiber = FiberBuilder
    let fiberPoly = FiberBuilderPoly
    let fiberPolyWith cont = FiberBuilderPolyWith cont
    let generatorPolyWith cont = GeneratorPolyWith cont
