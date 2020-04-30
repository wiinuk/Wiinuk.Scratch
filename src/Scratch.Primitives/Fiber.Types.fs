namespace Scratch.Primitives

[<Struct>]
type FiberResult<'Y,'R> =
    | Yield of yieldValue: 'Y
    | Return of returnValue: 'R

type ISyntaxInfrastructure = interface end

type IFiber<'Y,'E,'R> =
    inherit ISyntaxInfrastructure
    abstract Next: environment: 'E byref -> FiberResult<'Y,'R>

type fiber<'Y,'E,'R> = IFiber<'Y,'E,'R>

type IGenerator<'F,'T,'E,'R> when 'F :> fiber<'T,'E,'R> =
    inherit ISyntaxInfrastructure
    abstract GetFiber: unit -> 'F

type generator<'F,'T,'E,'R> when 'F :> fiber<'T,'E,'R> = IGenerator<'F,'T,'E,'R>
type generator<'T,'E,'R> = generator<fiber<'T,'E,'R>,'T,'E,'R>
