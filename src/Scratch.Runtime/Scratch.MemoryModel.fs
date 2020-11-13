namespace rec Scratch.MemoryModel
open FSharp.Reflection
open Scratch
open Scratch.Ast
open Scratch.MemoryModel
open Scratch.Primitives
open Scratch.Reflection
open System.Reflection


[<NoComparison>]
type UnionRepresentation =
    | UnitLikeUnion of UnionCaseInfo
    | EnumLikeUnion of UnionCaseInfo * UnionCaseInfo * UnionCaseInfo list
    | RecordLikeUnion of UnionCaseInfo
    | OptionLikeUnion of noneCase: UnionCaseInfo * someCase: UnionCaseInfo
    | ComplexUnion of UnionCaseInfo * UnionCaseInfo * UnionCaseInfo list

type UnionCaseFieldInfo = UnionCaseFieldInfo of case: UnionCaseId * fieldIndex: int * field: PropertyId

type Member =
    | TupleField of index: int
    | RecordField of PropertyId
    | UnionCaseField of UnionCaseFieldInfo

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type MemberPath =
    | UnderlyingValue
    | UnionCaseTagField of unionDeclaringType: TypeId
    | MemberPath of Member * MemberPath
    | UnionCaseUnifiedField of MemberPath * MemberPath * MemberPath list

[<Struct; RequireQualifiedAccess>]
type Kind =
    | Primitive
    | Collectable

[<Struct>]
type UnderlyingTypeSpec = UnderlyingTypeSpec of vType: VType * path: MemberPath * kind: Kind

type ICustomLayoutShape =
    inherit IShape
    abstract ValueLayout: UnderlyingTypeSpec list option

type ICustomLayout<'Shape> when 'Shape :> ICustomLayoutShape and 'Shape : struct = interface end

module internal TypeSpecHelpers =
    open Scratch.Threading
    open System
    open System.Collections.Generic
    module E = FSharp.Quotations.Patterns
    module E = FSharp.Quotations.DerivedPatterns

    let inMember m ts = List.map (fun (UnderlyingTypeSpec(t, path, k)) -> UnderlyingTypeSpec(t, MemberPath(m, path), k)) ts

    let underlyingPrimitiveTypeTable =
        [|
            typeof<int>, Typed SType.N
            typeof<double>, Typed SType.N
            typeof<bool>, Typed SType.B
            typeof<string>, Typed SType.S

            typeof<Scratch.MemoryModel.S>, Typed SType.S
            typeof<Scratch.MemoryModel.N>, Typed SType.N
            typeof<Scratch.MemoryModel.B>, Typed SType.B

            typeof<Scratch.MemoryModel.Address>, Typed SType.N
            typedefof<Scratch.MemoryModel.Field<_,_>>, Typed SType.N
            typedefof<Scratch.MemoryModel.Pointer<_>>, Typed SType.N
            typedefof<Scratch.MemoryModel.Reference<_>>, Typed SType.N
            typedefof<Scratch.MemoryModel.Size<_>>, Typed SType.N

            typeof<SValue>, Any
            typeof<IWord>, Any
        |]
        |> dict
        |> Dictionary
        :> IReadOnlyDictionary<_,_>

    [<Struct>]
    type private DummyCustomLayoutShape =
        interface ICustomLayoutShape with
            member _.ValueLayout = None

    let (|CustomLayoutShape|_|) (t: Type) =
        t.GetInterfaces()
        |> Seq.tryPick (function

            // type t = interface ICustomLayout<shapeT>
            // new shapeT().ValueLayout
            | GenericType(d, [shapeT]) when d = typedefof<ICustomLayout<DummyCustomLayoutShape>> ->
                let shape = Activator.CreateInstance shapeT :?> ICustomLayoutShape
                Some shape.ValueLayout

            | _ -> None
        )

    let rec isZeroSizedType = function
        | t when t = typeof<unit> -> true

        // e.g. `(unit * unit)`
        | TupleType es -> es |> List.forall isZeroSizedType

        // e.g. `type Record = { f1: unit; f2: unit }`
        | RecordType fs -> fs |> Array.forall (fun f -> isZeroSizedType f.PropertyType)

        | UnionType _ as t ->
            TypeSpec.unionRepresentation t
            |> Option.map (function
                
                // e.g. `type Union = | Case`
                | UnitLikeUnion _ -> true

                // e.g. `type Union = Case of unit * unit`
                | RecordLikeUnion u -> u.GetFields() |> Array.forall (fun f -> isZeroSizedType f.PropertyType)

                | EnumLikeUnion _
                | OptionLikeUnion _
                | ComplexUnion _ -> false
            )
            |> Option.defaultValue false

        | _ -> false

    let rec valueLayout = function
        | t when typeof<ISyntaxInfrastructure>.IsAssignableFrom t -> None
        | t ->

        match TypeSpec.underlyingPrimitiveType t with
        | Some t -> Some [UnderlyingTypeSpec(t, UnderlyingValue, Kind.Primitive)]
        | None ->

        match t with

        // type T = interface ICustomLayout
        | CustomLayoutShape x -> x
        
        | t when isZeroSizedType t -> Some []

        | t when t = typeof<obj> -> Some TypeSpec.collectableType

        // 'T when 'T : not struct
        | GenericParameterType(GenericParameterAttributes.ReferenceTypeConstraint, []) ->
            Some TypeSpec.collectableType

        // 'T when 'T :> IWord
        | GenericParameterType(GenericParameterAttributes.None, [c0]) when typeof<IWord>.IsAssignableFrom c0 ->
            Some TypeSpec.iwordType

        | TupleType es as t -> 
            if t.IsValueType then valueTupleValueLayout es
            else Some TypeSpec.collectableType

        | RecordType fs as t ->
            if t.IsValueType then valueRecordValueLayout fs
            else Some TypeSpec.collectableType

        | UnionType _ as t ->
            TypeSpec.unionRepresentation t
            |> Option.bind unionValueLayout

        | _ -> None

    and valueTupleValueLayout es =
        es
        |> Option.mapSeq TypeSpec.valueLayoutCached
        |> Option.map (fun tupleFields ->
            tupleFields
            |> Seq.mapi (fun i fields -> inMember (TupleField(i + 1)) fields)
            |> List.concat
        )

    and valueRecordValueLayout fs =
        fs
        |> Option.mapSeq (fun f ->
            TypeSpec.valueLayoutCached f.PropertyType
            |> Option.map (inMember (RecordField(MemberId.propertyId f)))
        )
        |> Option.map List.concat

    and unionValueLayout = function
        | UnitLikeUnion _ -> Some []
        | EnumLikeUnion _ -> Some TypeSpec.enumLikeCaseType
        | RecordLikeUnion case ->
            if case.DeclaringType.IsClass then Some TypeSpec.collectableType else
            recordLikeValueUnionLayout case

        | OptionLikeUnion(noneCase, someCase) ->
            if noneCase.DeclaringType.IsClass then Some TypeSpec.collectableType else
            optionLikeValueUnionLayout noneCase someCase

        | ComplexUnion(case1, case2, cases) ->
            if case1.DeclaringType.IsClass then Some TypeSpec.collectableType else
            complexValueUnionLayout case1 case2 cases

    and complexValueUnionLayout case1 case2 cases =
        let cases = case1::case2::cases
        // [<Struct>]
        // type U =
        //     | C0
        //     | C1
        //     | C2 of Class
        //     | C3 of int * Class * Class
        //     | C4 of Class * double * double
        //
        // {
        //     tag: Primitive
        //     1: Class // C2.1 | C3.2 | C4.1
        //     2: Class // C3.3
        //     3: Primitive
        //     4: Primitive
        // }
        cases
        |> Option.mapSeq (fun c ->
            c.GetFields()
            |> Seq.indexed
            |> Option.mapSeq (fun (i, f) ->
                TypeSpec.valueLayoutCached f.PropertyType
                |> Option.map (inMember (UnionCaseField <| UnionCaseFieldInfo(MemberId.unionCaseId c, i, MemberId.propertyId f)))
            )
            |> Option.map List.concat
        )
        |> Option.map (fun caseLayouts ->
            let rec sort (collectables, primitives) = function
                | [] -> List.rev collectables, List.rev primitives
                | UnderlyingTypeSpec(t, path, Kind.Collectable)::ts -> sort ((t, path)::collectables, primitives) ts
                | UnderlyingTypeSpec(t, path, Kind.Primitive)::ts -> sort (collectables, (t, path)::primitives) ts

            let caseKinds = Seq.map (sort ([], [])) caseLayouts |> Seq.toArray

            let max select = Seq.map (select >> List.length) >> Seq.max
            let maxCollectableCount = max fst caseKinds
            let maxPrimitiveCount = max snd caseKinds

            let caseKinds =
                caseKinds
                |> Seq.map (fun (collectables, primitives) ->
                    let pad maxCount xs =
                        List.map Some xs @
                        List.replicate (maxCount - List.length xs) None

                    pad maxCollectableCount collectables, pad maxPrimitiveCount primitives
                )

            let merge xs1 xs2 =
                List.zip xs1 xs2
                |> List.map (fun ((paths, xs1), xs2) ->
                    match xs1, xs2 with
                    | xs1, None -> paths, xs1
                    | None, Some(t2, p2) -> p2::paths, Some t2
                    | Some t1, Some(t2, path2) ->
                        path2::paths, Some(if t1 = t2 then t1 else Any)

                )

            let initial = List.replicate maxCollectableCount ([], None), List.replicate maxPrimitiveCount ([], None)
            let collectables, primitives =
                caseKinds
                |> Seq.fold (fun (collectables1, primitives1) (collectables2, primitives2) ->
                    merge collectables1 collectables2, merge primitives1 primitives2
                ) initial

            let build kind xs =
                xs
                |> List.map (fun (paths, t) ->
                    assert (not <| List.isEmpty paths)
                    assert (Option.isSome t)

                    let path =
                        match paths with
                        | [] -> UnderlyingValue
                        | [path] -> path
                        | p1::p2::ps -> UnionCaseUnifiedField(p1, p2, ps)

                    UnderlyingTypeSpec(Option.defaultValue Any t, path, kind)
                )
            let collectables = build Kind.Collectable collectables
            let primitives = build Kind.Primitive primitives
            TypeSpec.caseTagType case1.DeclaringType::collectables@primitives
        )

    and recordLikeValueUnionLayout case =
        case.GetFields()
        |> Seq.indexed
        |> Option.mapSeq (fun (i, f) ->
            TypeSpec.valueLayoutCached f.PropertyType
            |> Option.map (inMember (UnionCaseField <| UnionCaseFieldInfo(MemberId.unionCaseId case, i, MemberId.propertyId f)))
        )
        |> Option.map List.concat

    and optionLikeValueUnionLayout noneCase someCase =
        let u = if noneCase.Tag < someCase.Tag then ComplexUnion(noneCase, someCase, []) else ComplexUnion(someCase, noneCase, [])
        unionValueLayout u

    let unionMemoryLayout = function
        | UnitLikeUnion _ -> Some []
        | EnumLikeUnion _ -> Some TypeSpec.enumLikeCaseType
        | RecordLikeUnion case -> recordLikeValueUnionLayout case

        | OptionLikeUnion(noneCase, someCase) ->
            if noneCase.DeclaringType.IsValueType
            then optionLikeValueUnionLayout noneCase someCase
            else None

        | ComplexUnion(case1, case2, cases) ->
            if case1.DeclaringType.IsValueType
            then complexValueUnionLayout case1 case2 cases
            else None

    let unionCaseMemoryLayout (c: UnionCaseInfo) =
        TypeSpec.unionRepresentation c.DeclaringType
        |> Option.bind (function
            | UnitLikeUnion _
            | EnumLikeUnion _
            | RecordLikeUnion _ as u -> unionMemoryLayout u
            | OptionLikeUnion(noneCase, someCase) ->
                if c.DeclaringType.IsValueType then optionLikeValueUnionLayout noneCase someCase else

                if c.Tag = noneCase.Tag then Some []
                else recordLikeValueUnionLayout someCase

            | ComplexUnion(case1, case2, cases) ->
                if c.DeclaringType.IsValueType then complexValueUnionLayout case1 case2 cases else

                recordLikeValueUnionLayout c
                |> Option.map (fun c' -> TypeSpec.caseTagType c.DeclaringType::c')
        )

module TypeSpec =
    open System
    open TypeSpecHelpers
    type private T = Reflection.FSharpType


    let collectableType = [UnderlyingTypeSpec(Typed SType.N, UnderlyingValue, Kind.Collectable)]
    let enumLikeCaseType = [UnderlyingTypeSpec(Typed SType.S, UnderlyingValue, Kind.Primitive)]
    let iwordType = [UnderlyingTypeSpec(Any, UnderlyingValue, Kind.Primitive)]
    let caseTagType unionDeclaringType = UnderlyingTypeSpec(Typed SType.S, UnionCaseTagField(MemberId.typeId unionDeclaringType), Kind.Primitive)

    let unionRepresentation = Fun.memoize <| fun t ->
        if
            typeof<IWord>.IsAssignableFrom t ||
            typeof<ISyntaxInfrastructure>.IsAssignableFrom t ||
            not (T.IsUnion(t, allowAccessToPrivateRepresentation = true))
        then None
        else

        let cases =
            T.GetUnionCases(t, allowAccessToPrivateRepresentation = true)
            |> Seq.map (fun c -> c, c.GetFields() |> Array.toList)
            |> Seq.toList

        match cases with
        | [] -> None

        | [case, []] -> UnitLikeUnion case |> Some

        | (enum1, [])::(enum2, [])::cases ->
            let enums = cases |> Seq.map fst |> Seq.toList
            if List.forall (snd >> List.isEmpty) cases
            then EnumLikeUnion(enum1, enum2, enums) |> Some
            else ComplexUnion(enum1, enum2, enums) |> Some

        | [noneCase, []; someCase, _::_]
        | [someCase, _::_; noneCase, []] ->
            OptionLikeUnion(noneCase, someCase) |> Some

        | [case, _::_] -> RecordLikeUnion case |> Some

        | (case1, _)::(case2, _)::cases ->
            let cases = cases |> Seq.map fst |> Seq.toList
            ComplexUnion(case1, case2, cases) |> Some

    // t が class なら t の値のメモリ上のレイアウトを返す
    // t が class でないなら t の値をメモリ上にボックス化した時のレイアウトを返す
    let memoryLayout = function
        | Choice1Of2 t ->
            match t with
            | t when typeof<ISyntaxInfrastructure>.IsAssignableFrom t -> None
            | t when t = typeof<unit> -> Some []
            | GenericParameterType(GenericParameterAttributes.None, [c0]) when typeof<IWord>.IsAssignableFrom c0 ->
                Some iwordType

            | TupleType es -> valueTupleValueLayout es
            | RecordType fs -> valueRecordValueLayout fs

            | UnionType _ as t ->
                unionRepresentation t
                |> Option.bind unionMemoryLayout

            | t ->

            match underlyingPrimitiveType t with
            | Some t -> Some [UnderlyingTypeSpec(t, UnderlyingValue, Kind.Primitive)]
            | None -> None

        | Choice2Of2 c -> unionCaseMemoryLayout c

    let valueLayout = function
        | GeneratorType t
        | AtomicType t
        | t -> TypeSpecHelpers.valueLayout t

    let internal valueLayoutCached: Type -> _ = Fun.memoize valueLayout

    let underlyingPrimitiveType (GenericTypeDefinition t) =
        match underlyingPrimitiveTypeTable.TryGetValue t with
        | true, t -> Some t
        | _ ->
            if typeof<IWord>.IsAssignableFrom t then Some Any
            else None

    /// `unit` -> `[]`
    /// `struct(struct(int * IWord * #IWord) * unit * string)` -> `[N, [TupleField 1; TupleField 1]; Any, [TupleField 1; TupleField 2]; Any, [TupleField 1; TupleField 3]; S, [TupleField 2]]`
    let underlyingType t = valueLayoutCached t
    let isZeroSizedType t = match underlyingType t with Some [] -> true | _ -> false

    let internal sizeRaw = function
        | UnderlyingType t -> List.length t
        | t -> failwithf $"size is undefined: {t}"

    [<AbstractClass; Sealed>]
    type private SizeHolder<'T> private () =
        static let value: 'T Size = sizeRaw typeof<'T> |> Size
        static member Value = value

    [<RequiresExplicitTypeArguments>]
    let size<'T> = SizeHolder<'T>.Value

[<AutoOpen>]
module TypeSpecOperators =
    open Scratch.Threading
    open System

    let (|GeneratorType|_|) t =
        let rec inheritTypes (t: Type) = seq {
            t
            for i in t.GetInterfaces() do
                i
                yield! inheritTypes i

            match t.BaseType with
            | null -> ()
            | b ->
                b
                yield! inheritTypes b
        }

        inheritTypes t
        |> Seq.tryPick (function
            | GenericType(d, [_;_;t]) when d = typedefof<IFiber<_,_,_>> -> Some t
            | _ -> None
        )

    let (|AtomicType|_|) = function
        | GenericType(d, [_;_;t]) when d = typedefof<Atomic<_,_,_>> -> Some t
        | _ -> None

    let (|UnderlyingType|_|) t = TypeSpec.underlyingType t
    let (|UnderlyingPrimitiveType|_|) t = TypeSpec.underlyingPrimitiveType t

module Size =
    let toNumber (Size x) = x

    [<RequiresExplicitTypeArguments>]
    let defineSize<'T> size: 'T Size = Size size

    [<RequiresExplicitTypeArguments>]
    let unsafeOfNumber<'T> size: 'T Size = Size size

    let typeSizeRaw t = TypeSpec.sizeRaw t

    [<RequiresExplicitTypeArguments>]
    let typeSize<'T> = TypeSpec.size<'T>

[<AutoOpen>]
module SizeOperations =
    [<RequiresExplicitTypeArguments>]
    let defineTypeSize<'T> = Size.typeSize<'T>

type 'T Reference = Reference of int
with
    interface IWord with
        override x.Value = let (Reference x) = x in SNumber(double x)

type 'T Pointer = Nil | NonNil of 'T Reference
with
    interface IWord with
        override x.Value =
            match x with
            | Nil -> SNumber 0.
            | NonNil x -> toV x

[<AutoOpen>]
module PointerOperations =
    let nil = Nil

[<Struct>]
type SequenceLayoutShape =
    interface ICustomLayoutShape with
        member _.ValueLayout = None

type 'T Sequence = | Sequence with
    interface ICustomLayout<SequenceLayoutShape>

module Sequence =
    let unsafeReference (Reference p: 'T Sequence Reference) index (Size size: 'T Size): 'T Reference = Reference <| p + index * size

[<Struct>]
type ('T, 'M) Field = Field of int
module Field =
    open FSharp.Quotations
    type private T = FSharp.Reflection.FSharpType

    let toNumber (Field n) = n

    [<RequiresExplicitTypeArguments>]
    let unsafeOfNumber<'T,'M> offset: Field<'T,'M> = Field offset

    let private recordFieldRaw' (recordField: PropertyInfo) =
        let fs = T.GetRecordFields(recordField.DeclaringType, allowAccessToPrivateRepresentation = true)
        let i = Array.findIndex ((=) recordField) fs
        Seq.take i fs
        |> Seq.sumBy (fun f -> Size.typeSizeRaw f.PropertyType)

    let recordFieldRaw = Fun.memoize recordFieldRaw'

    let defineRecordField (e: ('T -> 'M) Expr): Field<'T,'M> =
        let f =
            e
            |> Expr.tryPick (function
                | Patterns.PropertyGet(_, m, _) -> Some m
                | _ -> None
            )
            |> Option.defaultWith (fun _ ->
                failwithf $"invalid record field expr: {e}"
            )
        recordFieldRaw f |> Field

    let reference (Reference p: 'T Reference) (Field o: Field<'T,'F>): 'F Reference = Reference(p + o)

[<Struct>]
type Address = Address of IWord Pointer with
    interface IWord with
        override x.Value = let (Address x) = x in toV x
    static member (+) (Address x, n) =
        match x with
        | Nil -> Address(NonNil(Reference n))
        | NonNil(Reference x) -> Address(NonNil(Reference(x + n)))

    static member (-) (Address a1, Address a2) = int (SValue.toNumber (toV a1) - SValue.toNumber (toV a2))
module Address =
    let ofNumber n = Address(NonNil(Reference n))
    let toNumber (Address a) = match a with Nil -> 0 | NonNil(Reference n) -> n

[<StructuredFormatDisplay "{Display}">]
[<NoComparison; NoEquality>]
type Memory = Memory of IWord SList with
    member x.Display =
        let (Memory(SList xs)) = x
        let sb = System.Text.StringBuilder()
        sb.AppendLine "Memory [" |> ignore
        let mutable i = 1
        for x in xs do
            sb
                .Append(i)
                .Append(": ")
                .Append(x)
                .AppendLine "," |> ignore
            i <- i + 1
        sb.AppendLine "]" |> ignore
        string sb
    override x.ToString() = x.Display

module Pointer =
    let toAddress = function Nil -> Address Nil | NonNil(Reference x) -> Address(NonNil(Reference x))

    [<RequiresExplicitTypeArguments>]
    let unsafeOfAddress<'a> : _ -> 'a Pointer = function 
        | Address Nil -> Nil
        | Address(NonNil(Reference x)) -> NonNil(Reference x)

module Reference =
    let toPointer (Reference x: 'T Reference): 'T Pointer = NonNil(Reference x)
    let toAddress (Reference x) = Address(NonNil(Reference x))
    
    [<RequiresExplicitTypeArguments>]
    let unsafeOfAddress<'a> : Address -> 'a Reference = function
        | Address Nil -> failwithf "nil address"
        | Address(NonNil(Reference x)) -> Reference x

    let unsafeOfPointer (p: 'a Pointer) = Pointer.toAddress p |> unsafeOfAddress<'a>

    [<ReflectedDefinition>]
    let swap (x1: 'T Reference) (x2: 'T Reference) =
        let mutable x1 = toAddress x1
        let mutable x2 = toAddress x2
        repeat (Size.toNumber Size.typeSize<'T>) <| fun _ ->
            let x = Memory.read (unsafeOfAddress<_> x1)
            Memory.write (unsafeOfAddress<_> x1) (Memory.read (unsafeOfAddress<_> x2))
            Memory.write (unsafeOfAddress<_> x2) x
            x1 <- Address.(+)(x1, 1)
            x2 <- Address.(+)(x2, 1)

module Memory =
    let memory = Memory <| defineList []
    let memoryBegin() = 1 |> Reference |> NonNil |> Address
    let memoryEnd() = let (Memory x) = memory in SList.length x + 1 |> Reference |> NonNil |> Address
    let clear() = let (Memory m) = memory in SList.removeAll m
    let push x = let (Memory m) = memory in SList.push m x
    let read (Reference p: 'T Reference when 'T :> IWord) = let (Memory m) = memory in SList.get m p :?> 'T
    let write (Reference p: 'T Reference when 'T :> IWord) (x: 'T) = let (Memory m) = memory in SList.set m p (x :> IWord)

    [<ReflectedDefinition>]
    let get p f = read (Field.reference p f)
    [<ReflectedDefinition>]
    let set p f x = write (Field.reference p f) x

    [<ReflectedDefinition>]
    let copy fromp top count =
        let mutable i = 0
        repeat count <| fun _ ->
            write (Reference.unsafeOfAddress<IWord> (Address.(+)(top, i))) (read (Reference.unsafeOfAddress<IWord> (Address.(+)(fromp, i))))
            i <- i + 1

module Operators =
    open Scratch.Operators

    /// alias of `Field.reference p f`
    let inline (->*) p f = Field.reference p f
    /// alias of `Memory.get p f`
    let inline (->.) p f = Memory.get p f
    /// alias of `%(Memory.get p f)`
    let inline (->%) p f = %(p->.f)
    /// alias of `Memory.write p v`
    let inline (<-*) p v = Memory.write p v
    /// alias of `Memory.write p (%v)`
    let inline (<-%) p v = p <-* %v

    let (.<) (x1: 'a when 'a :> IWord) (x2: 'a) = SValue.compareSValue (toV x1) (toV x2) < 0
    let (.>) (x1: 'a when 'a :> IWord) (x2: 'a) = SValue.compareSValue (toV x1) (toV x2) > 0
    let (.=) (x1: 'a when 'a :> IWord) (x2: 'a) = SValue.compareSValue (toV x1) (toV x2) = 0

module Word =
    open Scratch.MemoryModel.Operators

    [<ReflectedDefinition>]
    let size<'a when 'a :> IWord> : 'a Size = Size 1
    let toString (w: #IWord) = SValue.toString <| toV w
    let toNumber (w: #IWord) = toV w |> SValue.toNumber
    let toBool (w: #IWord) = toV w |> SValue.toBool

    [<ReflectedDefinition>]
    type Compare<'T> when 'T :> IWord = | Compare with
        interface IFunc<struct('T Reference * 'T Reference), int> with
            member _.Invoke struct(x1, x2) =
                if Memory.read x1 .< Memory.read x2 then -1 elif x1 = x2 then 0 else 1
