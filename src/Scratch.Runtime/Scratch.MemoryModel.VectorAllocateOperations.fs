[<AutoOpen>]
module Scratch.MemoryModel.VectorAllocateOperations

module Vector =
    [<ReflectedDefinition>]
    let newVector count initializer =
        let v = Allocator.mallocVectorExtend Size.typeSize<'T> count
        Vector.iteri initializer v
        v

    [<ReflectedDefinition>]
    let newFold count state initializer =
        let v = Allocator.mallocVectorExtend Size.typeSize<'T> count
        struct(v, Vector.fold initializer state v)
