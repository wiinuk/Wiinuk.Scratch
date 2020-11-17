namespace Scratch.IR
open System
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Threading
open Scratch
open Scratch.Ast
open Scratch.Primitives


module private Unchecked =
    [<AbstractClass; Sealed>]
    type private EqualityComparerHolder<'Shape,'T> when 'Shape : struct and 'Shape :> IShape private () =
        static let value =
            match Unchecked.defaultof<'Shape> |> box with
            | :? IEqualityComparer<'T> as x -> x
            | _ ->
                { new IEqualityComparer<_> with
                    member _.Equals(x, y) = Unchecked.equals x y
                    member _.GetHashCode x = Unchecked.hash x
                }

        static member Value = value

    let equalsBy (_: 'Shape The) x y = EqualityComparerHolder<'Shape,_>.Value.Equals(x, y)
    let hashBy (_: 'Shape The) x = EqualityComparerHolder<'Shape,_>.Value.GetHashCode x

    [<AbstractClass; Sealed>]
    type private ComparerHolder<'Shape,'T> when 'Shape : struct and 'Shape :> IShape private () =
        static let value =
            match Unchecked.defaultof<'Shape> |> box with
            | :? IComparer<'T> as s -> s
            | _ -> { new IComparer<_> with member _.Compare(x, y) = Unchecked.compare x y }

        static member Value = value

    let compareBy (_: 'Shape The) x y = ComparerHolder<'Shape,_>.Value.Compare(x, y)

[<Struct; StructLayout(LayoutKind.Auto)>]
[<CustomEquality; CustomComparison>]
type Keyed<'Shape, [<EqualityConditionalOn; ComparisonConditionalOn>] 'Key,'Trivia> when 'Shape : struct and 'Shape :> IShape = private {
    key: 'Key
    trivia: 'Trivia
}
with
    member private x.equals y = Unchecked.equalsBy the<'Shape> x.key y.key
    member x.Equals y = x.equals y
    override x.Equals y =
        match y with
        | :? Keyed<'Shape,'Key,'Trivia> as y -> x.equals y
        | _ -> false

    override x.GetHashCode() = Unchecked.hashBy the<'Shape> x.key

    interface IEquatable<Keyed<'Shape,'Key,'Trivia>> with
        member x.Equals y = x.equals y

    member x.CompareTo y = Unchecked.compareBy the<'Shape> x.key y.key

    interface IComparable with
        member x.CompareTo y = x.CompareTo(y :?> Keyed<'Shape,'Key,'Trivia>)

    interface IComparable<Keyed<'Shape,'Key,'Trivia>> with
        member x.CompareTo y = x.CompareTo y

module Keyed =
    let make (_: 'Shape The) key trivia: Keyed<'Shape,_,_> = { key = key; trivia = trivia }
    let key k = k.key
    let trivia k = k.trivia

    type Equatable<'Shape,'Key,'Trivia>
        when 'Shape : struct
        and 'Shape :> IShape
        and 'Shape :> IEqualityComparer<'Key>
        = Keyed<'Shape,'Key,'Trivia>

    type Comparable<'Shape,'Key,'Trivia>
        when 'Shape : struct
        and 'Shape :> IShape
        and 'Shape :> IEqualityComparer<'Key>
        and 'Shape :> IComparer<'Key>
        = Equatable<'Shape,'Key,'Trivia>

    [<Struct>]
    type LanguageComparableShape<'Key when 'Key : comparison> =
        interface IShape
        interface IEqualityComparer<'Key> with
            member _.Equals(x, y) = x = y
            member _.GetHashCode x = hash x

        interface IComparer<'Key> with
            member _.Compare(x, y) = compare x y

    type LanguageComparable<'Key,'Trivia>
        when 'Key : comparison
        = Comparable<LanguageComparableShape<'Key>,'Key,'Trivia>

[<Struct>]
type VarId = private VarId of int
module VarId =
    let mutable private nextId = 0
    let newId() = VarId <| Interlocked.Increment &nextId

[<Struct>]
type VarIdShape =
    interface IShape
    interface IEqualityComparer<VarId> with
        member _.Equals(VarId x, VarId y) = x = y
        member _.GetHashCode(VarId x) = x.GetHashCode()

    interface IComparer<VarId> with
        member _.Compare(VarId x, VarId y) = compare x y

[<Struct; StructLayout(LayoutKind.Auto)>]
type VarTrivia<'a> = private { name: string; varTrivia: 'a }
type Var<'a> = Keyed.Comparable<VarIdShape, VarId,'a VarTrivia>

type SimpleVar = HUnit Var
module Var =
    let newVar name trivia: _ Var =
        let id = VarId.newId()
        Keyed.make the<VarIdShape> id { name = name; varTrivia = trivia }

    let newSimple name = newVar name HUnit
    let name v = Keyed.trivia(v).name
    let trivia v = Keyed.trivia(v).varTrivia
    let simple v = Keyed.make the<_> (Keyed.key v) (let x = Keyed.trivia v in { name = x.name; varTrivia = HUnit })

type MemberType = Keyed.LanguageComparable<(* underlyingType: *) VType, (* memberName: *) string>

module MemberType =
    let make n t: MemberType = Keyed.make the<_> t n
    let underlyingType (t: MemberType) = Keyed.key t
    let memberName (t: MemberType) = Keyed.trivia t

    module private VType =
        let isAssignable place value =
            match place, value with
            | Any, _ -> true
            | expected, actual -> expected = actual

    let isAssignable place value = VType.isAssignable (underlyingType place) (underlyingType value)
    let equals t1 t2 = underlyingType t1 = underlyingType t2

type ExpType = MemberType list
[<AutoOpen>]
module ExpTypeCaseExtensions =
    [<AutoOpen>]
    module ExpType =
        let UnboxedType(xs: MemberType list): ExpType = xs
        let (|UnboxedType|) (t: ExpType): MemberType list = t

module MemberTypes =
    let litAny = MemberType.make "" Any
    let litNumber = MemberType.make "" (Typed SType.N)
    let litString = MemberType.make "" (Typed SType.S)
    let litBool = MemberType.make "" (Typed SType.B)

module ExpTypes =
    open MemberTypes

    let number = UnboxedType [litNumber]
    let string = UnboxedType [litString]
    let bool = UnboxedType [litBool]
    let any = UnboxedType [MemberType.make "" Any]
    let empty = UnboxedType []

module ExpType =
    let ofMemberType t =
        match MemberType.memberName t, MemberType.underlyingType t with
        | "", Typed SType.N -> ExpTypes.number
        | "", Typed SType.S -> ExpTypes.string
        | "", Typed SType.B -> ExpTypes.bool
        | "", Any -> ExpTypes.any
        | _ -> UnboxedType [t]

    let ofVType t = ofMemberType <| MemberType.make "" t

    let equals (UnboxedType ts1) (UnboxedType ts2) =
        let rec aux = function
            | [], [] -> true
            | p::ps, v::vs -> if MemberType.equals p v then aux (ps, vs) else false
            | _ -> false

        aux (ts1, ts2)

    let isAssignable place value =
        let rec aux = function
            | [], [] -> true
            | p::ps, v::vs -> if MemberType.isAssignable p v then aux (ps, vs) else false
            | _ -> false

        match place, value with
        | UnboxedType p, UnboxedType v -> aux (p, v)

    let size (UnboxedType ts) = List.length ts

type Storage = { isMutable: bool; varType: ExpType }
module Storage =
    let make isMutable varType = {
        isMutable = isMutable
        varType = varType
    }

    let varType x = x.varType

type Var = Storage Var
[<AutoOpen>]
module StorageVarExtensions =
    module Var =
        let newStorage name isMutable varType =
            Var.newVar name <| Storage.make isMutable varType

        let varType v = Storage.varType (Var.trivia v)
        let isMutable v = Var.trivia(v).isMutable

type ProcedureType = { parameterTypes: ExpType list; resultType: ExpType }
type ProcedureVar = ProcedureType Var

[<AutoOpen>]
module ProcedureVarExtensions =
    module Var =
        let newProc name parameterTypes resultType =
            Var.newVar name { parameterTypes = parameterTypes; resultType = resultType }

[<Struct>]
type Source<'a,'b> = { value: 'b; source: 'a }
module Source =
    let withSource newSource x = { value = x.value; source = newSource }
    module Operators =
        let (@+) value source = { value = value; source = source }

type ListVar<'a> = Source<'a, SimpleVar>

[<RequireQualifiedAccess; Struct>]
type CoerceKind =
    | Reinterpret
    | Convert

type ExtensionSpec = {
    extensionId: string
    resultType: ExpType
    operands: AstDefinitions.OperandInfo list
    control: AstDefinitions.Control
    kind: AstDefinitions.Kind
    cost: Scratch.Reflection.Cost
}

type Exp'<'a> =
    | Lit of SValue
    | Let of Var * value: 'a Exp * scope: 'a Exp
    | VarSet of Var * 'a Exp
    | Var of Var
    | Seq of first: 'a Exp * last: 'a Exp
    | If of test: 'a Exp * ifTrue: 'a Exp * ifFalse: 'a Exp

    | NewTuple of 'a Exp list
    /// 0-base index
    | TupleGet of 'a Exp * index: int
    /// 0-base index
    | TupleSet of Var * index: int * value: 'a Exp

    | Op of operator: Symbol * operands: 'a Exp list
    | ListOp of operator: Symbol * operands: Choice<'a Exp, 'a ListVar> list
    | ExtOp of spec: ExtensionSpec * operands: 'a Exp list

    /// `divmod(10, 20)`
    | Call of proc: ProcedureVar * args: 'a Exp list

    | Coerce of kind: CoerceKind * value: 'a Exp * newType: ExpType

    | Atom of 'a Exp
and Exp<'a> = Source<'a, 'a Exp'>

type Param' = Var
and Param<'a> = Source<'a, Param'>

type ProcHeader<'a> = ProcHeader of var: ProcedureVar * parameters: 'a Param list * atomicity: Atomicity 

type ProcDef<'a> = Source<'a,'a ProcDef'>
and [<Struct>] ProcDef'<'a> =
    | ProcDef of header: 'a ProcHeader * body: 'a Exp

type ListenerDef<'a> = Source<'a,'a ListenerDef'>
and ListenerDef'<'a> =
    | ListenerDef of name: Symbol * arguments: 'a Exp list * body: 'a Exp

type VariableInitDef<'a> = VariableInitDef of Var * init: 'a Exp

[<RequireQualifiedAccess>]
type Top<'a> =
    | Listener of 'a ListenerDef
    | Procedure of 'a ProcDef
    | VariableInit of 'a VariableInitDef

type VariableDef<'a,'var,'init,'view> = {
    state: 'a
    isPersistent: Persistence
    var: 'var
    init: 'init
    view: 'view
}

type VariableDecl<'a> = VariableDef<'a, Var, hunit, hunit>

[<Struct>]
type ListView = {
    x: double
    y: double
    width: double
    height: double
    visible: Visibility
}
type ListDef<'a> = VariableDef<'a, SimpleVar, SValue iarray, ListView>

type IREntityData<'Ex,'a> = EntityData<'Ex,Top<'a>, VariableDecl<'a>, ListDef<'a>,'a>
type IRStageData<'a> = StageData<Top<'a>, VariableDecl<'a>, ListDef<'a>,'a>

type RuntimeProcedures =
    | EnsureStack
    | RentStack
    | InitDefaultStack

type RuntimeVariables =
    | DefaultStack

type RuntimeLists =
    | StackMemory
    | StackPool

type Naming = {
    indexedName: struct {| baseName: string; index: int |} -> string
    resultName: string
    localName: string
    anonName: string
    listenerWrapperName: string -> string
    atomicWrapperName: string
    stackName: string
    runtimeVariableName: RuntimeVariables -> string
    runtimeListName: RuntimeLists -> string
    runtimeProcedureName: RuntimeProcedures -> string
    combineNamespaceItem: struct(string * string) -> string
}

module IRToStageData =
    type Config = {
        maxStackCapacity: int
        uninitValue: SValue
        naming: Naming
    }
    module Config =
        let defaultConfig = {
            maxStackCapacity = 1024
            uninitValue = SString "UNINIT"
            naming = {
                indexedName = fun n -> $"{n.baseName}#{n.index}"
                resultName = "result"
                localName = "_"
                stackName = "_stack"
                anonName = "λ"
                listenerWrapperName = fun listenerName -> "_" + listenerName
                atomicWrapperName = "atomic"

                runtimeProcedureName = function
                    | EnsureStack -> "ensureStack"
                    | RentStack -> "rentStack"
                    | InitDefaultStack -> "initDefaultStack"

                runtimeListName = function
                    | StackMemory -> "stackMemory"
                    | StackPool -> "stackPool"

                runtimeVariableName = function
                    | DefaultStack -> "defaultStack"

                combineNamespaceItem = fun struct(x, y) -> x + "." + y
            }
        }
