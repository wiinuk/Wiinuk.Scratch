namespace Scratch.Primitives
open System

// TODO: inref
type FuncIOV<'T1,'T2,'TResult> = delegate of arg1: 'T1 byref * arg2: 'T2 outref -> 'TResult

[<AttributeUsage(AttributeTargets.Constructor ||| AttributeTargets.Method ||| AttributeTargets.Class, Inherited = false)>]
type NoGcAllocationAttribute() =
    inherit Attribute()
    member val AllocatableTypes: Type[] = [||] with get, set
    member val TreatAsNoAllocation = false with get, set
