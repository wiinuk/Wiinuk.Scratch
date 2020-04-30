module Scratch.Executor.Diagnostics.Printer
open System
open Scratch.Primitives
open Scratch.Executor


[<AutoOpen>]
module private Privates =
    let scaler { EntityImage.scalarVariables = vs } (i: _ inref) = vs.Address(Index.create<_>(int32 i.operand1))
    let list { EntityImage.listVariables = vs } (i: _ inref) = vs.Address(Index.create<_>(int i.operand1))

    let sprintName = function
        | "" -> "''"
        | s when String.forall Char.IsLetterOrDigit s -> s
        | s ->
            let s = String.collect (function '\'' -> "\\'" | '\\' -> "\\\\" | c -> string c) s
            "'" + s + "'"

let printOperand w ({ Image.instructions = is } as image) entity procedure index =
    let i = &is.Address(index)
    let p = &image.procedures.Address(procedure)

    let m = CodeMetadata.metadata i.code
    match m.operandTypeCode with
    | OperandTypeCode.Void -> ()

    | OperandTypeCode.BroadcastIndex ->
        image.broadcasts.Address(Index.create<_>(int32 i.operand1)).broadcastName
        |> LowerName.toString
        |> sprintName
        |> fprintf w " %s"

    | OperandTypeCode.Double -> fprintf w " %0.17g" <| BitConverter.Int64BitsToDouble i.operand1
    | OperandTypeCode.StringIndex -> fprintf w " \"%s\"" <| image.stringLiterals.Address(Index.create<_>(int32 i.operand1))
    | OperandTypeCode.Bool -> fprintf w " %b" (i.operand1 <> 0L)
    | OperandTypeCode.Int32 -> fprintf w " %d" (int32 i.operand1) 
    | OperandTypeCode.KeyCode -> fprintf w " %A" <| enum<Scratch.KeyCode> (int32 i.operand1)
    | OperandTypeCode.Filter -> fprintf w " %A" <| enum<Filter> (int32 i.operand1)

    | OperandTypeCode.ArgumentIndex ->
        p.parameters.Address(Index.create<_>(int32 i.operand1)).parameterName
        |> sprintName
        |> fprintf w " %s"

    | OperandTypeCode.LocalIndex -> fprintf w " %d" (Index.create<_>(int32 i.operand1)).index
    | OperandTypeCode.Offset -> fprintf w " @%04X" (Index.(+)(index, 1) + (int32 i.operand1)).index

    | OperandTypeCode.StageListIndex -> fprintf w " %s" <| sprintName (list image.stageImage &i).listName
    | OperandTypeCode.SpriteListIndex -> fprintf w " %s" <| sprintName (list entity &i).listName

    | OperandTypeCode.ProcedureIndex ->
        let proc = &image.procedures.Address(Index.create<_>(int32 i.operand1))
        fprintf w " %s @%04X" (sprintName proc.name) proc.startAddress.index

    | OperandTypeCode.SpriteVariableIndex -> fprintf w " %s" (sprintName (scaler entity &i).name)
    | OperandTypeCode.StageVariableIndex -> fprintf w " %s" (sprintName (scaler image.stageImage &i).name)

let printInstruction w ({ Image.instructions = is } as image) entity procedure index =
    let i = &is.Address(index)
    fprintf w "@%04X %A" index.index i.code
    printOperand w image entity procedure index

let printProcedure w image entity procedure =
    let p = &image.procedures.Address(procedure)
    fprintfn w "procedure %s::%s @%04X" (sprintName entity.entityName) (sprintName p.name) p.startAddress.index
    fprintfn w "    parameterCount %d" p.parameterCount
    fprintfn w "    parameters %A" (p.parameters |> IArray.toSeqCopiable |> Seq.map (fun p -> p.parameterName) |> Seq.toList)
    fprintfn w "    localCount %d" p.localCount
    for i in p.startAddress..1..(p.startAddress + p.codeLength - 1) do
        fprintf w "    "
        printInstruction w image entity procedure i
        fprintfn w ""

let printListeners w image entity listeners listenerName =
    for p in IArray.toSeqCopiable listeners do
        fprintf w "on %s " listenerName
        printProcedure w image entity p
        fprintfn w ""

let printEntityImage w image entity =
    fprintfn w "entity %s" (sprintName entity.entityName)
    fprintfn w ""

    for v in IArray.toSeq entity.scalarVariables do
        fprintfn w "scaler %s" (sprintName v.name)
    fprintfn w ""

    for v in IArray.toSeqCopiable entity.listVariables do
        fprintfn w "list %s" (sprintName v.listName)
    fprintfn w ""

    for p in IArray.toSeqCopiable entity.definedProcedures do
        printProcedure w image entity p
        fprintfn w ""

    for struct(k, ls) in IndexMap.toSeq entity.broadcastListeners do
        let name = image.broadcasts.Address(k).broadcastName |> LowerName.toString |> sprintName
        printListeners w image entity ls name

    for p in IArray.toSeqCopiable entity.whenGreenFlags do
        fprintf w "on whenGreenFlag "
        printProcedure w image entity p
        fprintfn w ""

    for p in IArray.toSeqCopiable entity.whenClicked do
        fprintf w "on whenClicked "
        printProcedure w image entity p
        fprintfn w ""

    for p in IArray.toSeqCopiable entity.whenCloned do
        fprintf w "on whenCloned "
        printProcedure w image entity p
        fprintfn w ""

let printImage w image =
    printEntityImage w image image.stageImage
    for sprite in IArray.toSeq image.spriteImages do
        printEntityImage w image sprite

let printFlame w showLocation image (flame: _ inref) =
    match IndexMap.tryFind flame.returnAddress image.locationMap with
    | ValueNone -> ()
    | ValueSome l -> fprintf w "%s " (showLocation l)

    let p = &image.procedures.Address(flame.procedure)
    fprintf w "%s @%04X" p.name p.startAddress.index
