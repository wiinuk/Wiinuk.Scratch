module internal Scratch.Transpiler.PredefinedPlugin.AnnotatedMethods
open Scratch
open Scratch.Ast
open Scratch.AstDefinitions
open Scratch.IR
open Scratch.MemoryModel
open Scratch.Primitives
open Scratch.Reflection
open Scratch.Transpiler
open Scratch.Transpiler.PredefinedPlugin.Helpers
open Scratch.IR.Source.Operators
open System.Collections.Concurrent
open System.Reflection
open System

module E = FSharp.Quotations.Patterns


[<AutoOpen>]
module private Helpers =
    let inline toResult f = function ValueNone -> f() | ValueSome x -> Ok x
    module TsType =
        let ofType t =
            TypeSpec.underlyingPrimitiveType t
            |> VOption.map (function
                | Any -> TsType.gUnknown
                | Typed SType.B -> TsType.gBoolean
                | Typed SType.N -> TsType.gNumber
                | Typed SType.S -> TsType.gString
            )

let extensionSpecFromMethodWithBlockAttribute (m: MethodInfo) (b: BlockAttribute) = result {
    let! returnType, kind = result {
        let returnType = m.ReturnType
        if returnType = typeof<Void> || returnType = typeof<unit> then return struct([], Kind.Statement) else

        let! returnType = TypeSpec.underlyingPrimitiveType m.ReturnType |> toResult (fun () ->
            raiseError (SourceCode.ofMethod m) <| InvalidProcedureResultType m.ReturnType
        )
        return ExpType.ofVType returnType, Kind.Expression
    }
    let! operands =
        m.GetParameters()
        |> Result.mapSeq (fun p -> result {
            let! operandType = TsType.ofType p.ParameterType |> toResult (fun () ->
                raiseError (SourceCode.ofMethod m) <| InvalidExpressionType p.ParameterType
            )
            return {
                operandType = OperandType.Expression operandType
                literalOperandType = LiteralOperandTypeInfo.ForceInherit
            }
        })

    let opCode =
        b.ExtensionOpCode
        |> Option.ofObj
        |> Option.defaultWith (fun _ -> m.Name)

    return {
        extensionId = $"{b.ExtensionId}_{opCode}"
        resultType = returnType
        control = Control.Unknown
        operands = Seq.toList operands
        kind = kind
        cost = HasSideEffect
    }
}
let blockAttributeToIRConstructor (m: MethodInfo) =
    m.GetCustomAttributes<BlockAttribute>(``inherit`` = true)
    |> Seq.tryHead
    |> Option.map (fun b ->
        let (GenericMethodDefinition m) = m

        match b.Operator with
        | Symbol.Extension ->
            match extensionSpecFromMethodWithBlockAttribute m b with
            | Error e -> fun _ -> Error e
            | Ok spec -> fun struct(args, source) -> Ok <| ExtOp(spec, args) @+ source

        | operator ->
            fun struct(args, source) -> Ok <| Op(operator, args) @+ source
    )

let private methodToIRFactoryCache = ConcurrentDictionary()
let (|CallMethodWithBlockAttribute|_|) senv = function
    | E.Call(None, m, args) as e ->
        methodToIRFactoryCache.GetOrAdd(methodId m, valueFactory = fun _ ->
            blockAttributeToIRConstructor m
        )
        |> Option.map (fun makeIR ->
            let source = SourceCode.ofExpr e |> SourceCode.tag
            let args = List.map (Core.transpileExpression senv) args
            makeIR(args, source)
        )
    | _ -> None
