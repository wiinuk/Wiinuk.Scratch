namespace Scratch.Executor
open System
open Scratch
open Scratch.Primitives


type NameWithVersion = NameWithVersion of name: string * version: int
type LabelBuilder = {
    name: NameWithVersion
    emittedPlaceholderIndexes: Instruction index ResizeArray
    markedIndex: Instruction index voption
}
type LabelId = LabelBuilder index

[<Struct>]
type LocalBuilder = {
    using: bool
}

[<ReferenceEquality; NoComparison>]
type CodeBuilder = {
    buffer: Instruction ResizeArray

    /// add only
    definedLabels: LabelBuilder ResizeArray

    mutable localsVersion: int
    /// add only
    definedLocals: LocalBuilder ResizeArray

    mutable stringLiteralToIndex: Map<string, string index>
    stringLiterals: string ResizeArray
}

and [<Struct; NoEquality; NoComparison>] Label = { declaringBuilder: CodeBuilder; labelIndex: LabelId }
and [<Struct; NoEquality; NoComparison>] Local = {
    parent: CodeBuilder
    version: int
    localIndex: LocalBuilder index
}
with
    member x.Dispose() =
        if x.version <> x.parent.localsVersion then () else

        let ls = x.parent.definedLocals
        let l = ls.Get(x.localIndex)
        ls.Set(x.localIndex, { l with using = false })

    interface IDisposable with
        member x.Dispose() = x.Dispose()

module CodeBuilder =
    let create() = {
        buffer =
            let x = ResizeArray()
            // index 0 indicates invalid
            x.Add { code = Code.Nop; operand1 = 0L }
            x

        definedLabels = ResizeArray()

        localsVersion = 0
        definedLocals = ResizeArray()

        stringLiteralToIndex = Map.empty
        stringLiterals = ResizeArray()
    }
    let offset b = b.buffer.NextIndex()

    let defineLabel name ({ definedLabels = labels } as b) =
        let index = labels.NextIndex()
        labels.Add {
            name = NameWithVersion(name, index.index)
            emittedPlaceholderIndexes = ResizeArray()
            markedIndex = ValueNone
        }
        { declaringBuilder = b; labelIndex = index }

    let resetLocals b =
        b.localsVersion <- Checked.(+) b.localsVersion 1
        b.definedLabels.Clear()

    let nameWithVersion (NameWithVersion(name, v)) =
        if v = 0 then name else sprintf "%s#%d" name v

    let checkParent b label =
        let info = label.declaringBuilder.definedLabels.Get(label.labelIndex)
        if label.declaringBuilder <> b then
            failwithf "labels defined with a different builder can not be emitted: %A" (nameWithVersion info.name)
        info

    let markLabel b label =
        let info = checkParent b label
        match info.markedIndex with
        | ValueSome _ -> failwithf "Duplicate labels: %A" (nameWithVersion info.name)
        | _ ->
            let buffer = b.buffer
            let markIndex = buffer.NextIndex()
            for index in info.emittedPlaceholderIndexes do
                let offset = markIndex .-. (index + 1)
                let instruction = buffer.Get(index)
                buffer.Set(index, { code = instruction.code; operand1 = int64 offset })

            let info = {
                name = info.name
                emittedPlaceholderIndexes = null
                markedIndex = ValueSome markIndex
            }
            b.definedLabels.Set(label.labelIndex, info)

    let checkMarked b =
        for l in b.definedLabels do
            match l.markedIndex with
            | ValueNone -> failwithf "label %A is not marked" (nameWithVersion l.name)
            | _ -> ()

    let defineLocal ({ definedLocals = locals } as b) =
        let localIndex = locals.FindIndex(fun l -> not l.using)
        let localIndex =
            if localIndex < 0 then
                let localIndex = locals.NextIndex()
                locals.Add { using = true }
                localIndex
            else
                Index.create<LocalBuilder> localIndex

        { parent = b; version = b.localsVersion; localIndex = localIndex }

    let localCount b = b.definedLocals.Count

    let build b =
        checkMarked b
        IArray.ofICollection b.buffer, IArray.ofICollection b.stringLiterals

    let buildToList b (result: _ ResizeArray) =
        checkMarked b
        result.AddRange b.buffer

    [<Struct>]
    type BuilderWithOperand<'T> = {
        operand: 'T
        withoutOperand: CodeBuilder
    }

    let emit (def: 'D when 'D : struct) b code operand =
        let mutable operandDef = def
        b.buffer.Add { code = code; operand1 = Func.invoke &operandDef { operand = operand; withoutOperand = b } }

    [<RequiresExplicitTypeArguments>]
    let private indexToInt64<'T> (x: 'T index) = int64 x.index

    type private def<'T> = IFunc<BuilderWithOperand<'T>, int64>
    type private idef<'T> = 'T index def

    [<Struct>]
    type DLabel = | DLabel with
        interface Label def with
            member _.Invoke { withoutOperand = b; operand = label } =
                let info = checkParent b label
                let index = b.buffer.NextIndex()
                match info.markedIndex with
                | ValueSome markedIndex ->
                    let offset = markedIndex .-. (index + 1)
                    int64 offset

                | _ ->
                    info.emittedPlaceholderIndexes.Add index
                    Int64.MinValue

    [<Struct>]
    type DLocal = | DLocal with interface Local def with member _.Invoke x = int64 x.operand.localIndex.index

    [<Struct>]
    type DVoid = | DVoid with interface unit def with member _.Invoke _ = 0L
    [<Struct>]
    type DDouble = | DDouble with interface double def with member _.Invoke x = BitConverter.DoubleToInt64Bits x.operand
    [<Struct>]
    type DBool = | DBool with interface bool def with member _.Invoke x = if x.operand then 1L else 0L
    [<Struct>]
    type DInt32 = | DInt32 with interface int32 def with member _.Invoke x = int64<int> x.operand
    [<Struct>]
    type DString = | DString with
        interface string def with
            member _.Invoke { operand = operand; withoutOperand = b } =
                let index =
                    match Map.tryFind operand b.stringLiteralToIndex with
                    | ValueSome index -> index
                    | _ ->
                        let index = b.stringLiterals.NextIndex()
                        b.stringLiteralToIndex <- Map.add operand index b.stringLiteralToIndex
                        b.stringLiterals.Add operand
                        index
                int64 index.index

    [<Struct>]
    type DArgument = | DArgument with interface Value idef with member _.Invoke x = indexToInt64<_> x.operand
    [<Struct>]
    type DKeyCode = | DKeyCode with interface Scratch.KeyCode def with member _.Invoke x = int64 x.operand
    [<Struct>]
    type DFilter = | DFilter with interface Executor.Filter def with member _.Invoke x = int64 x.operand
    [<Struct>]
    type DProcedure = | DProcedure with interface Procedure idef with member _.Invoke x = indexToInt64<_> x.operand
    [<Struct>]
    type DScalar = | DScalar with interface Value idef with member _.Invoke x = indexToInt64<_> x.operand
    [<Struct>]
    type DList = | DList with interface Value ResizeArray idef with member _.Invoke x = indexToInt64<_> x.operand
    [<Struct>]
    type DBroadcast = | DBroadcast with interface Broadcast idef with member _.Invoke x = indexToInt64<_> x.operand
