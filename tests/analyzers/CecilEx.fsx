#r "nuget: Mono.Cecil"
#load "Print.fsx"
open System
open System.IO
open System.Text
open Printf
open Mono.Cecil
open Mono.Cecil.Cil
open Print
type T = Mono.Cecil.MetadataType
type S = Cil.StackBehaviour
type C = Cil.Code


type StackType =
    | I4
    | I
    | I8
    | F
    | O
    | Ref

type AssemblyReadConfig = {
    searchDirs: string list
    inMemory: bool
    readingMode: ReadingMode
}
let readAssembly withConfig path =
    let config = withConfig { inMemory = true; searchDirs = []; readingMode = ReadingMode.Deferred }

    let config =
        let resolver = new DefaultAssemblyResolver()
        Path.GetDirectoryName(path: string) |> resolver.AddSearchDirectory
        Path.GetDirectoryName typeof<int>.Assembly.Location |> resolver.AddSearchDirectory // mscorlib
        for d in config.searchDirs do resolver.AddSearchDirectory d

        ReaderParameters(
            ReadingMode = config.readingMode,
            InMemory = config.inMemory,
            AssemblyResolver = resolver,
            ReadSymbols = true,
            ThrowIfSymbolsAreNotMatching = false,
            SymbolReaderProvider = DefaultSymbolReaderProvider(throwIfNoSymbol = false)
        )
    AssemblyDefinition.ReadAssembly(path, config)

/// e.g. `C:/dir/file.ext(1,2-3,4)`
let writeSourceLocation w (p: SequencePoint) =
    match p with
    | null -> fprintf w "?"; false
    | _ ->
        match p.Document with
        | null -> w.Write "???"
        | d ->
            let url = d.Url
            if url.Contains " " then
                fprintf w "\"%s\"" d.Url
            else
                w.Write d.Url

        if p.IsHidden then
            w.Write "(???)"
        else
            fprintf w "(%d,%d)" p.StartLine p.StartColumn
        true

let writeShortMethodSignature w (m: MethodReference) =
    fprintf w "%s::%s" m.DeclaringType.Name m.Name

let writeShortInstruction w (i: Instruction) =
    fprintf w "%s" i.OpCode.Name
    match i.Operand with
    | null -> ()
    | :? MethodReference as m ->
        fprintf w " "
        writeShortMethodSignature w m

    | x ->
        fprintf w " %O" x

let rec baseClasses = function
    | null -> [] :> _ seq
    | (t: TypeReference) -> seq {
        yield t
        yield! baseClasses <| t.Resolve().BaseType
    }

let isSubclassOf (child: TypeDefinition) (parent: TypeReference) = 
    child.MetadataToken <> parent.MetadataToken &&
    baseClasses child |> Seq.exists (fun b -> b.MetadataToken = parent.MetadataToken)

let rec doesSpecificInterfaceImplementInterface (it0: TypeDefinition) (it1: TypeDefinition) =
    assert it1.IsInterface
    assert it0.IsInterface

    it0.MetadataToken = it1.MetadataToken || doesAnySubTypeImplementInterface it0 it1

and doesSpecificTypeImplementInterface (child: TypeDefinition) (it: TypeDefinition) =
    assert it.IsInterface

    child.Interfaces
    |> Seq.exists (fun ifaceDef ->
        doesSpecificInterfaceImplementInterface (ifaceDef.InterfaceType.Resolve()) it
    )

and doesAnySubTypeImplementInterface (child: TypeDefinition) (it: TypeDefinition) =
    assert it.IsInterface
    child
    |> baseClasses
    |> Seq.exists (fun b -> doesSpecificTypeImplementInterface (b.Resolve()) it)

let isAssignableFrom (sink: TypeDefinition) (source: TypeDefinition) =
    sink = source ||
    sink.MetadataToken = source.MetadataToken ||
    isSubclassOf source sink ||
    sink.IsInterface && doesAnySubTypeImplementInterface source sink

let rec typeEq (t1: TypeReference) (t2: TypeReference) =
    t1.MetadataToken = t2.MetadataToken &&

    t1.GenericParameters.Count = t2.GenericParameters.Count &&
    Seq.forall2 typeEq t1.GenericParameters t2.GenericParameters

let findOverridedMethod (derivedType: TypeDefinition) (parentMethod: MethodReference) =
    derivedType.Methods
    |> Seq.filter (fun m ->
        m.IsVirtual &&
        m.IsHideBySig &&
        m.Name = parentMethod.Name &&

        match m.HasParameters, parentMethod.HasParameters with
        | false, false -> true
        | true, false
        | false, true -> false
        | _ ->

        m.Parameters.Count = parentMethod.Parameters.Count &&
        Seq.zip m.Parameters parentMethod.Parameters
        |> Seq.forall (fun (p, parentP) -> typeEq p.ParameterType parentP.ParameterType) &&

        typeEq m.MethodReturnType.ReturnType parentMethod.MethodReturnType.ReturnType
    )
    |> Seq.toList

type OperationEnvironment<'T> = {
    body: MethodBody
    data: 'T
}

[<Struct>]
type Operation<'T> = {
    instruction: Instruction
    env: 'T OperationEnvironment
}

type ConsoleWithLocationConfig = {
    color: ConsoleColor
    backgroundColor: ConsoleColor
    debugColor: ConsoleColor

    /// allow null
    site: MethodReference
    /// allow null
    point: SequencePoint
    /// allow null
    operation: Instruction
    writer: TextWriter

    colorModifer: ColorModifer
}

let nearestSeqiencePoint (debug: MethodDebugInformation) (i: Instruction) =
    if not debug.HasSequencePoints then null else
    let pointAndDistances =
        debug.SequencePoints
        |> Seq.map (fun p -> struct(p, p.Offset - i.Offset))
        |> Seq.sortBy (fun struct(_, r) -> abs r)

    let prevNearest =
        pointAndDistances
        |> Seq.tryFind (fun struct(_, r) -> r <= 0)
    match prevNearest with
    | Some struct(prevPoint, _) -> prevPoint
    | _ ->

    let nextNearest =
        pointAndDistances
        |> Seq.tryFind (fun struct(_, r) -> 0 < r)
    match nextNearest with
    | Some struct(nextPoint, _) -> nextPoint
    | _ -> null

let sequencePointOrNull config =
    match config.point with
    | null ->
        match config.site with
        | null -> null
        | m ->
            
        let debug = m.Resolve().DebugInformation
        match config.operation with
        | null ->
            let ps = debug.SequencePoints
            if ps.Count <> 0 then ps.[0]
            else null

        | i -> nearestSeqiencePoint debug i

    | p -> p

let writeLocation config =
    match sequencePointOrNull config with
    | null -> false
    | p ->
        withConsoleConfig (fun c -> { c with fore = config.debugColor; colorModifer = config.colorModifer }) <| fun _ ->
        writeSourceLocation config.writer p

let writeMethodSignature short config =
    match config.site with
    | null -> ()
    | m ->
        withConsoleConfig (fun c -> { c with fore = config.debugColor; colorModifer = config.colorModifer }) <| fun _ ->

        if short then ()
        else
            config.writer.Write " at "
            config.writer.Write m

let writeInstruction short config =
    match config.operation with
    | null -> ()
    | i ->
        withConsoleConfig (fun c -> { c with fore = config.debugColor; colorModifer = config.colorModifer }) <| fun _ ->

        config.writer.Write " @ "
        if short then
            writeShortInstruction config.writer i
        else
            config.writer.Write i

/// `C:/dir/file.ext(0, 2):... @ opname operand`
let lprintfn withConfig format =
    kprintf (fun message ->
        let m = consoleColorModier
        let config = withConfig {
            color = m.getColor()
            backgroundColor = m.getBackgroundColor()
            debugColor = ConsoleColor.DarkGray

            site = null
            point = null
            operation = null
            writer = stdout
            colorModifer = consoleColorModier
        }
        let isWriteFullLocation = writeLocation config
        withConsoleConfig (fun c -> { c with fore = config.debugColor; colorModifer = config.colorModifer }) <| fun _ ->
            config.writer.Write ": "

        withConsoleConfig (fun c -> { c with fore = config.color; back = config.backgroundColor; colorModifer = config.colorModifer }) <| fun _ ->
            config.writer.Write message

        writeMethodSignature isWriteFullLocation config
        writeInstruction isWriteFullLocation config

        config.writer.WriteLine()
    ) format

let operationError operation format =
    ksprintf (fun message ->
        use w = new StringWriter()
        lprintfn (fun c -> {
            c with
                writer = w
                site = operation.env.body.Method
                operation = operation.instruction
        }) "%s" message
        failwith <| w.ToString()
    ) format

let opcodes filter =
    typeof<OpCodes>.GetFields()
    |> Seq.choose (fun f ->
        match f.GetValue null with
        | :? OpCode as x -> Some x
        | _ -> None
    )
    |> Seq.filter filter
    |> Seq.map (fun c -> sprintf "| C.%O" c.Code)
    |> String.concat "\n"

let rec stackType (t: TypeReference) =
    match t.MetadataType with
    | T.Boolean
    | T.Byte
    | T.Char
    | T.Int16
    | T.Int32
    | T.SByte
    | T.UInt16
    | T.UInt32 -> I4

    | T.Int64
    | T.UInt64 -> I8

    | T.Double
    | T.Single -> F

    | T.IntPtr
    | T.UIntPtr
    | T.Pointer
    | T.FunctionPointer -> I

    | T.ByReference
    | T.Pinned -> Ref

    | T.Array
    | T.Class
    | T.Object
    | T.String
    | T.TypedByReference
    | T.ValueType -> O

    | T.GenericInstance ->
        let t = t :?> GenericInstanceType
        stackType t.ElementType

    | T.Var
    | T.MVar
    | T.Void
    | T.OptionalModifier
    | T.RequiredModifier
    | T.Sentinel
    | _ as x -> failwithf "unexpected element type: %A" x
    
let operandOrRaise operation =
    match operation.instruction.Operand with
    | :? _ as x -> x
    | _ -> operationError operation "unexpected operand"

module VietualEval =    
    type OpInferEnv = {
        locals: StackType array
        parameters: StackType array
    }

    let pop0Push1 operation =
        let ldargN operation index =
            try operation.env.data.parameters.[index]
            with :? IndexOutOfRangeException ->
                operationError operation "parameter out of range: %d" index

        let ldlocN operation index =
            try operation.env.data.locals.[index]
            with :? IndexOutOfRangeException ->
                operationError operation "local out of range: %d" index

        match operation.instruction.OpCode.Code with
        | C.Ldarg_0 -> ldargN operation 0
        | C.Ldarg_1 -> ldargN operation 1
        | C.Ldarg_2 -> ldargN operation 2
        | C.Ldarg_3 -> ldargN operation 3
        | C.Ldarg_S
        | C.Ldarg ->
            let p: ParameterDefinition = operandOrRaise operation
            ldargN operation p.Index

        | C.Ldloc_0 -> ldlocN operation 0
        | C.Ldloc_1 -> ldlocN operation 1
        | C.Ldloc_2 -> ldlocN operation 2
        | C.Ldloc_3 -> ldlocN operation 3
        | C.Ldloc_S
        | C.Ldloc ->
            let v: VariableDefinition = operandOrRaise operation
            ldlocN operation v.Index

        | C.Ldsfld ->
            let f: FieldReference = operandOrRaise operation
            stackType f.FieldType

        | _ -> operationError operation "unexpected pop0Push1 operator"

    let binaryNumericType operation (t1, t2) =
        let error operation t1 t2 = operationError operation "invalid binary numeric operator type: %A, %A" t1 t2

        match t1, t2 with
        | I4, I4
        | I8, I8
        | F, F
        | I, I -> t1
        | I, I4
        | I4, I -> I
        | s1, s2 ->

        match operation.instruction.OpCode.Code with
        | Code.Add ->
            match s1, s2 with
            | Ref, I4
            | Ref, I -> t1
            | I4, Ref
            | I, Ref -> t2
            | _ -> error operation t1 t2

        | Code.Sub ->
            match s1, s2 with
            | Ref, I4
            | Ref, I -> t1
            | Ref, Ref -> I
            | _ -> error operation t1 t2

        | _ -> error operation t1 t2

    let overflowArithmeticType operation (t1, t2) =
        let error operation t1 t2 = operationError operation "invalid overflow arithmetic operator type: %A, %A" t1 t2

        match t1, t2 with
        | I4, I4
        | I8, I8
        | I, I -> t1
        | I4, I
        | I, I4 -> I
        | s1, s2 ->

        match operation.instruction.OpCode.Code with
        | C.Add_Ovf_Un ->
            match s1, s2 with
            | Ref, I4
            | Ref, I -> t1
            | I4, Ref
            | I, Ref -> t2
            | _ -> error operation t1 t2

        | C.Sub_Ovf_Un ->
            match s1, s2 with
            | Ref, I4
            | Ref, I -> t1
            | Ref, Ref -> I
            | _ -> error operation t1 t2
        | _ -> error operation t1 t2

    let integerOperationType operation (t1, t2) =
        match t1, t2 with
        | I4, I4
        | I8, I8
        | I, I -> t1
        | I4, I
        | I, I4 -> I
        | _ -> operationError operation "invalid integer operation operator type: %A, %A" t1 t2

    let shiftOperationType operation (t1, t2) =
        match t1, t2 with
        | I4, (I4 | I)
        | I8, (I4 | I)
        | I, (I4 | I) -> t1
        | _ -> operationError operation "invalid shift operation operator type: %A, %A" t1 t2

    let pop2Push1 operation x2 x1 =
        match operation.instruction.OpCode.Code with
        | C.Add
        | C.Sub
        | C.Mul
        | C.Div
        | C.Rem -> binaryNumericType operation (x1, x2)

        | C.Div_Un
        | C.Rem_Un
        | C.And
        | C.Or
        | C.Xor -> integerOperationType operation (x1, x2)

        | C.Shl
        | C.Shr
        | C.Shr_Un -> shiftOperationType operation (x1, x2)
        | C.Ldelem_Any ->
            let t: TypeReference = operandOrRaise operation
            stackType t

        | C.Add_Ovf
        | C.Add_Ovf_Un
        | C.Mul_Ovf
        | C.Mul_Ovf_Un
        | C.Sub_Ovf
        | C.Sub_Ovf_Un -> overflowArithmeticType operation (x1, x2)
        | _ -> operationError operation "unexpected pop2Push1 code"

    let pushC operation stack =
        match operation.instruction.OpCode.StackBehaviourPush with
        | S.Push0 -> ValueSome stack
        | S.Pushi -> ValueSome(I::stack)
        | S.Pushi8 -> ValueSome(I8::stack)
        | S.Pushr4
        | S.Pushr8 -> ValueSome(F::stack)
        | S.Pushref -> ValueSome(Ref::stack)
        | _ -> ValueNone

    let pop0PushN operation stack =
        match pushC operation stack with
        | ValueSome stack -> stack
        | _ ->

        match operation.instruction.OpCode.StackBehaviourPush with
        | S.Push1 -> pop0Push1 operation::stack
        | S.Push1_push1
        | S.Varpush
        | _ -> operationError operation "unexpected pop0 operation"

    let pop1Push1 operation x1 =
        match operation.instruction.OpCode.Code with
        | C.Neg
        | C.Not -> x1
        | C.Ldobj
        | C.Unbox_Any ->
            match operation.instruction.Operand with
            | :? TypeReference as t -> stackType t
            | _ -> operationError operation "unexpected operator"

        | C.Ldfld ->
            match operation.instruction.Operand with
            | :? FieldReference as f -> stackType f.FieldType
            | _ -> operationError operation "unexpected operator"

        | C.Mkrefany -> stackType operation.env.body.Method.Module.TypeSystem.TypedReference
        | _ -> operationError operation "unexpected pop1 push1 operation"

    let pop1Push2 operation x1 stack =
        match operation.instruction.OpCode.Code with
        | C.Dup -> x1::x1::stack
        | _ -> operationError operation "unexpected pop1 push2 operation"

    let pop1PushN operation x1 stack =
        match pushC operation stack with
        | ValueSome stack -> stack
        | _ ->

        match operation.instruction.OpCode.StackBehaviourPush with
        | S.Push1 -> pop1Push1 operation x1::stack
        | S.Push1_push1 -> pop1Push2 operation x1 stack
        | S.Varpush
        | _ -> operationError operation "unexpected pop1 operation"

    let pop2PushN operation x2 x1 stack =
        match pushC operation stack with
        | ValueSome stack -> stack
        | _ ->

        match operation.instruction.OpCode.StackBehaviourPush with
        | S.Push1 -> pop2Push1 operation x1 x2::stack
        | S.Push1_push1
        | S.Varpush
        | _ -> operationError operation "unexpected pop2 operation"

    let pop3PushN operation x3 x2 x1 stack =
        match pushC operation stack with
        | ValueSome stack -> stack
        | _ ->

        match operation.instruction.OpCode.StackBehaviourPush with
        | S.Push1
        | S.Push1_push1
        | S.Varpush
        | _ -> operationError operation "unexpected pop3 operation"

    let popCall operation (m: #IMethodSignature) stack =
        let count = m.Parameters.Count
        let count = if m.HasThis then 1 + count else count
        try List.skip count stack
        with :? ArgumentException ->
            operationError operation "expected stack depth => %d. actual: %A" count stack

    let evalCall operation m stack =
        let stack = popCall operation m stack
        if m.ReturnType.MetadataType = T.Void then stack
        else stackType m.ReturnType::stack

    let stackEval operation stack =
        let operator = operation.instruction.OpCode
        match operator.StackBehaviourPop with
        | S.Pop0 -> pop0PushN operation stack
        | S.Pop1
        | S.Popi
        | S.Popref ->
            match stack with
            | x1::stack -> pop1PushN operation x1 stack
            | _ -> operationError operation "expected stack depth => 1. actual: %A" stack

        | S.Pop1_pop1
        | S.Popi_pop1
        | S.Popi_popi
        | S.Popi_popi8
        | S.Popi_popr4
        | S.Popi_popr8
        | S.Popref_pop1
        | S.Popref_popi ->
            match stack with
            | x2::x1::stack -> pop2PushN operation x2 x1 stack
            | _ -> operationError operation "expected stack depth => 2. actual: %A" stack

        | S.Popi_popi_popi
        | S.Popref_popi_popi
        | S.Popref_popi_popi8
        | S.Popref_popi_popr4
        | S.Popref_popi_popr8
        | S.Popref_popi_popref ->
            match stack with
            | x3::x2::x1::stack -> pop3PushN operation x3 x2 x1 stack
            | _ -> operationError operation "expected stack depth => 3. actual: %A" stack

        | S.Varpop ->
            match operator.Code with
            | Code.Call
            | Code.Callvirt ->
                let m: MethodReference = operandOrRaise operation
                evalCall operation m stack

            | Code.Newobj ->
                let m: MethodReference = operandOrRaise operation
                let stack = popCall operation m stack
                stackType m.DeclaringType::stack

            | Code.Calli ->
                let m: CallSite = operandOrRaise operation
                evalCall operation m stack

            | Code.Ret ->
                match operation.env.body.Method.ReturnType.MetadataType, stack with
                | T.Void, [] -> []
                | T.Void, _ -> operationError operation "expected stack depth = 0. actual: %A" stack
                | _, [_] -> []
                | _ -> operationError operation "expected stack depth = 1. actual: %A" stack

            | _ -> operationError operation "unexpected varpop instruction"

        | S.PopAll -> []
        | b -> operationError operation "unexpected StackBehaviourPop: %A" b

    let inferStackTypes (body: MethodBody) =
        let parameters = [|
            if body.Method.HasThis then
                yield stackType body.ThisParameter.ParameterType

            for p in body.Method.Parameters ->
                stackType p.ParameterType
        |]
        let locals = [|
            for v in body.Variables ->
                stackType v.VariableType
        |]
        let env = { body = body; data = { parameters = parameters; locals = locals; } }

        let types = Array.create body.CodeSize ValueNone
        let rec loop stack = function
        | null as x -> operationError { env = env; instruction = x } "null instruction"
        | i ->
            let operation = { env = env; instruction = i }
            let offset = operation.instruction.Offset
            if offset < 0 || types.Length <= offset then
                operationError operation "offset out of range"
            else

            match Array.item offset types with
            | ValueSome _ -> printfn "merged %A" operation
            | _ ->

            let stack = stack |> stackEval operation

            types.[offset] <- ValueSome stack

            match operation.instruction.OpCode.FlowControl with
            | FlowControl.Next
            | FlowControl.Break
            | FlowControl.Call
            | FlowControl.Meta -> loop stack operation.instruction.Next
            | FlowControl.Branch ->
                let t: Instruction = operandOrRaise operation
                loop stack t

            | FlowControl.Cond_Branch ->
                loop stack operation.instruction.Next
                match operation.instruction.Operand with
                | :? Instruction as t -> loop stack t
                | :? array<Instruction> as ts -> for t in ts do loop stack t
                | x -> operationError operation "unexpected operand: %O" x

            | FlowControl.Return
            | FlowControl.Throw -> ()
            | FlowControl.Phi
            | _ as x -> operationError operation "unknown flow control: %A" x

        loop [] body.Instructions.[0]
        types

type AssemblyDefinition with
    member a.Methods =
        a.Modules
        |> Seq.collect (fun m -> m.Types)
        |> Seq.collect (fun t -> t.Methods)

    member a.GetType name =
        a.Modules
        |> Seq.collect (fun m -> m.Types)
        |> Seq.filter (fun t -> t.FullName = name)
        |> Seq.exactlyOne

type TypeDefinition with
    member t.GetMethod name =
        t.Methods
        |> Seq.filter (fun m -> m.Name = name)
        |> Seq.exactlyOne

let (=~) l r = String.Equals(l, r, StringComparison.InvariantCultureIgnoreCase)

type Custom = {
    attributeType: TypeReference
    arguments: (ParameterDefinition * CustomAttributeArgument) list Lazy
    properties: CustomAttributeNamedArgument list Lazy
    fields: CustomAttributeNamedArgument list Lazy
}

let (|Name|_|) name (c: CustomAttribute) =
    let n = c.AttributeType.Name
    if name =~ n || name + "Attribute" =~ n then
        let t = c.AttributeType
        Some {
            attributeType = t
            arguments =
                lazy
                    Seq.zip c.Constructor.Parameters c.ConstructorArguments
                    |> Seq.toList

            properties = lazy Seq.toList c.Properties
            fields = lazy Seq.toList c.Fields
        }
    else
        None

let take1 f xs = List.partition f xs |> function [x], xs -> Some x, xs | _ -> None, xs

let (|ParameterOptional|) name c =
    let p, ps = c.arguments.Value |> take1 (fun (p, _) -> name =~ p.Name)
    p, { c with arguments = lazy ps }

let (|PropertyOptional|) name c =
    let p, ps = c.properties.Value |> take1 (fun p -> name =~ p.Name)
    p, { c with properties = lazy ps }

let (|FieldOptional|) name c =
    let f, fs = c.fields.Value |> take1 (fun p -> name =~ p.Name)
    f, { c with fields = lazy fs }

let (|NamedValueOptional|) name = function
    | ParameterOptional name (Some(p, v), c) -> Some(p.Name, v), c
    | PropertyOptional name (Some x, c) -> Some(x.Name, x.Argument), c
    | FieldOptional name (Some x, c) -> Some(x.Name, x.Argument), c
    | c -> None, c

let (|StringArgument|_|) name (v: CustomAttributeArgument) = 
    match v.Value with
    | :? string as s when name =~ s -> Some name
    | _ -> None

let (|EnumArgument|_|) name (v: CustomAttributeArgument) =
    if not v.Type.IsValueType then None else

    let t = v.Type.Resolve()
    if not t.IsEnum then None else
    t.Fields
    |> Seq.tryFind (fun f -> f.HasConstant && f.Constant.Equals v.Value && f.Name =~ name)

let (|StringOrEnumArgument|_|) name = function
    | StringArgument name x -> Some x
    | EnumArgument name x -> Some x.Name
    | _ -> None
