module internal Scratch.Transpiler.PredefinedPlugin.CollectorPlugin
open FSharp.Quotations
open FSharp.Reflection
open Scratch
open Scratch.Ast
open Scratch.IR
open Scratch.MemoryModel
open Scratch.MemoryModel.TypeSpec
open Scratch.Primitives
open Scratch.Primitives.Field
open Scratch.Reflection
open Scratch.Reflection.Expr
open Scratch.Reflection.Member
open Scratch.Transformers
open Scratch.Transpiler
open Scratch.Transpiler.PredefinedPlugin.Helpers

module A = Scratch.Ast.Expression
module A = Scratch.Ast.Expressions
module Field = Scratch.MemoryModel.Field
module E = Quotations.Patterns
module E = Quotations.DerivedPatterns
module E = FSharp.Quotations.ExprShape
module Exp = Exp.Op
type private T = FSharp.Reflection.FSharpType
type private E = FSharp.Quotations.Expr
type private QVar = FSharp.Quotations.Var


let deleteReferenceMark (_: 'T) = ()
let addReferenceMark (_: 'T) = ()

[<AutoOpen>]
module private Helpers =
    let underlyingTypeOrError e t = context {
        match underlyingType t with
        | None -> return! InvalidExpressionType t |> raiseError e
        | Some vType -> return vType
    }

    let deleteReferenceMarkMD, _ = findMethod <@ deleteReferenceMark @>
    let addReferenceMarkMD, _ = findMethod <@ addReferenceMark @>

    /// deleteReferenceMark %e
    let deleteReference' (ExprType et & e) l =
        E.Call(deleteReferenceMarkMD.MakeGenericMethod et, [e])@+l

    /// addReferenceMark %e
    let addReference' (ExprType et & e) l =
        E.Call(addReferenceMarkMD.MakeGenericMethod et, [e])@+l

    // (fun o -> %e) => (fun o -> let r = %e in --o; r)
    // (let o = %e1 in %e2) => (let o = %e1 in let r = %e2 in --o; r)
    // (let rec o1 = %e1 and o2 = %e2 and ... in $en) => (let rec o1 = %e1 and o2 = %e2 and ... in let r = %en in --o1; --o2; ...; r)
    // (o) => (++o; o) // Var, static FieldGet, static PropertyGet
    // (o <- %e) => (--o; o <- %e) // Var, static FieldGet, static PropertyGet
    // (%o.o) => (let r = %o in let r2 = r.o in ++r2 --r; r2) // instance FieldGet, instance PropertyGet, TupleGet
    // (%x.o) => (let r = %x in let r2 = r.o in ++r2; r2) // instance FieldGet, instance PropertyGet, TupleGet
    // (%o.x) => (let r = %o in let r2 = r.x in --r; r2) // instance FieldGet, instance PropertyGet, TupleGet
    // (%o.o <- %ov) => (let r = %o in --r.o; r.o <- %ov; --r) // instance FieldSet, instance PropertySet
    // (%x.o <- %ov) => (let r = %x in --r.o; r.o <- %ov) // instance FieldSet, instance PropertySet
    // (%o.x <- %xv) => (let r = %o in r.x <- %xv; --r) // instance FieldSet, instance PropertySet
    // (%o?) => (let r = %o in let r2 = r? in r--; r2)
    // (%o; %e) => (--%o; %e)
    // (for v in %e1..%e2 do %o) => (for v in %e1..%e2 do --%o)
    // (while %e1 do %o) => (while %e1 do --%o)
    // ({ O.f1 = %e1; %fs... }) => (let r = { O.f1 = %e1; %fs... }; addReference r; r)
    let rec tryMark config e =
        match e with
        | DefineListCall _ -> None
        | E.Lambda(o, e1) when collectableInValue o.Type -> markLambda config (o, e1) e
        | E.Let(o, e1, e2) when collectableInValue o.Type -> markLet config (o, e1, e2) e
        | E.LetRecursive(binds, e2) when binds |> List.exists (fun (v,_) -> collectableInValue v.Type) -> markLetRecursive config (binds, e2) e

        | E.Var v when collectableInValue v.Type -> markStaticGet config e
        | E.FieldGet(None, f) when collectableInValue f.FieldType -> markStaticGet config e
        | E.PropertyGet(None, p, []) when collectableInValue p.PropertyType -> markStaticGet config e

        | E.VarSet(v, o) when collectableInValue v.Type -> markStaticSet config (fun () -> E.Var v) (fun x -> E.VarSet(v, x)) o e
        | E.FieldSet(None, f, o) when collectableInValue f.FieldType -> markStaticSet config (fun () -> E.FieldGet f) (fun x -> E.FieldSet(f, x)) o e
        | E.PropertySet(None, p, [], o) when collectableInValue p.PropertyType -> markStaticSet config (fun () -> E.PropertyGet p) (fun x -> E.PropertySet(p, x)) o e

        | E.FieldGet(Some this, f) when collectableInValue f.DeclaringType || collectableInValue f.FieldType -> markInstanceGet config (fun e -> E.FieldGet(e, f)) this e
        | E.PropertyGet(Some this, p, []) when collectableInValue p.DeclaringType || collectableInValue p.PropertyType -> markInstanceGet config (fun e -> E.PropertyGet(e, p)) this e
        | E.TupleGet(this, i) when collectableInValue this.Type || collectableInValue e.Type -> markInstanceGet config (fun e -> E.TupleGet(e, i)) this e

        // (%o?) => (let r = %o in let r2 = r? in r--; r2)
        | E.UnionCaseTest(o, u) when collectableInValue u.DeclaringType -> markUnionCaseTest config (o, u) e

        | E.FieldSet(Some this, f, e2) when collectableInValue f.DeclaringType || collectableInValue f.FieldType ->
            markInstanceSet config (fun x v -> E.FieldSet(x, f, v)) (fun x -> E.FieldGet(x, f)) this e2 e
        | E.PropertySet(Some this, p, [], e2) when collectableInValue p.DeclaringType || collectableInValue p.PropertyType ->
            markInstanceSet config (fun x v -> E.PropertySet(x, p, v)) (fun x -> E.PropertyGet(x, p)) this e2 e

        | E.Sequential(o, e2) when collectableInValue o.Type -> markSequential config (o, e2) e

        | E.ForIntegerRangeLoop(v, e1, e2, o) when collectableInValue o.Type -> markForIntegerRangeLoop config (v, e1, e2, o) e

        | E.WhileLoop(e1, o) when collectableInValue o.Type -> markWhileLoop config (e1, o) e

        | E.NewRecord(t, es) when newable t -> markNew config (fun () -> E.NewRecord(t, List.map (mark config) es)) e
        | E.NewTuple es when newable e.Type -> markNew config (fun () -> E.NewTuple(List.map (mark config) es)) e
        | E.NewUnionCase(c, es) when newable c.DeclaringType -> markNew config (fun () -> E.NewUnionCase(c, List.map (mark config) es)) e

        | e ->

        match e with
        | E.ShapeVar _ -> None
        | E.ShapeLambda(x, e1) ->
            tryMark config e1
            |> Option.map (fun e1 ->
                let l = getLocation e
                E.Lambda(x, e1)@+l
            )

        | E.ShapeCombination(op, es) ->
            let rec tryMarks = function
                | [] -> None
                | e::es ->

                match tryMark config e with
                | None ->
                    match tryMarks es with
                    | None -> None
                    | Some es -> Some(e::es)
                | Some e ->
                    match tryMarks es with
                    | None -> Some(e::es)
                    | Some es -> Some(e::es)

            tryMarks es
            |> Option.map (fun es ->
                let l = getLocation e
                E.RebuildShapeCombination(op, es)@+l
            )

    and mark config e = tryMark config e |> Option.defaultValue e

    and markLambda config (o, e1) e =
        let l = getLocation e
        let l1 = getLocation e1
        let temp = QVar(config.localName, e1.Type)
        E.Lambda(o, E.Let(temp, mark config e1, E.Sequential(deleteReference' (E.Var o@+l1) l1, E.Var temp@+l1)@+l1)@+l1)@+l |> Some

    and markLet config (o, e1, e2) e =
        let l = getLocation e
        let l2 = getLocation e2
        let temp = QVar(config.localName, e2.Type)
        E.Let(o, mark config e1, E.Let(temp, mark config e2, E.Sequential(deleteReference' (E.Var o@+l2) l2, E.Var temp@+l2)@+l2)@+l2)@+l |> Some

    and markLetRecursive config (binds, e2) e =
        let l = getLocation e
        let l2 = getLocation e2
        let temp = QVar(config.localName, e2.Type)
        let rest =
            binds
            |> Seq.filter (fun (v,_) -> collectableInValue v.Type)
            |> Seq.map (fun (o,_) -> deleteReference' (E.Var o) l2)
            |> Seq.fold (fun last e -> E.Sequential(e, last)) (E.Var temp)

        let binds = List.map (fun (v, e1) -> v, mark config e1) binds

        E.LetRecursive(binds, E.Let(temp, mark config e2, rest)@+l2)@+l |> Some

    and markUnionCaseTest config (o, u) e =
        let l = getLocation e
        let temp1V = QVar(config.localName, o.Type)
        let temp1 = E.Var temp1V
        let temp2V = QVar(config.localName, e.Type)
        let temp2 = E.Var temp2V
        E.Let(temp1V, mark config o, E.Let(temp2V, E.UnionCaseTest(temp1@+l, u)@+l, E.Sequential(deleteReference' (temp1@+l) l, temp2@+l)@+l)@+l)@+l |> Some

    and markSequential config (o, e2) e =
        let l = getLocation e
        let ol = getLocation o
        E.Sequential(deleteReference' (mark config o) ol, (mark config e2))@+l |> Some

    and markForIntegerRangeLoop config (v, e1, e2, o) e =
        let l = getLocation e
        let ol = getLocation o
        E.ForIntegerRangeLoop(v, mark config e1, mark config e2, deleteReference' (mark config o) ol)@+l |> Some

    and markWhileLoop config (e1, o) e =
        let l = getLocation e
        let ol = getLocation o
        E.WhileLoop(mark config e1, deleteReference' (mark config o) ol)@+l |> Some


    and markStaticGet config e =
        let l = getLocation e
        let tempV = QVar(config.localName, e.Type)
        let temp = E.Var tempV@+l
        E.Let(tempV, e, E.Sequential(addReference' temp l, temp)@+l)@+l |> Some

    and markStaticSet config makeGet makeSet value e =
        let l = getLocation e
        E.Sequential(deleteReference' (makeGet()@+l) l, makeSet(mark config value)@+l)@+l |> Some

    and markInstanceGet config makeGet this e =
        let l = getLocation e
        let thisL = getLocation this

        let thisTempV = QVar(config.localName, this.Type)
        let thisTemp = E.Var thisTempV@+thisL
        let fieldTempV = QVar(config.localName, e.Type)
        let fieldTemp = E.Var fieldTempV@+l

        let rest = fieldTemp
        let rest = if collectableInValue thisTemp.Type then E.Sequential(deleteReference' thisTemp l, rest)@+l else rest
        let rest = if collectableInValue fieldTemp.Type then E.Sequential(addReference' fieldTemp l, rest)@+l else rest

        E.Let(thisTempV, mark config this, E.Let(fieldTempV, makeGet thisTemp@+l, rest)@+l)@+l |> Some

    and markInstanceSet config makeSet makeGet this (ExprType valueT & value) e =
        let l = getLocation e
        let thisL = getLocation this

        let tempV = QVar(config.localName, this.Type)
        let temp = E.Var tempV@+l

        let rest = E.Value(())@+l
        let rest = if collectableInValue tempV.Type then E.Sequential(deleteReference' temp thisL, rest)@+l else rest
        let rest = E.Sequential(makeSet temp (mark config value)@+l, rest)
        let rest = if collectableInValue valueT then E.Sequential(deleteReference' (makeGet temp@+l) l, rest) else rest

        E.Let(tempV, mark config this, rest)@+l |> Some

    and markNew config makeNew e =
        let l = getLocation e
        let tempV = QVar(config.localName, e.Type)
        let temp = E.Var tempV@+l
        E.Let(tempV, makeNew(), E.Sequential(addReference' temp l, temp@+l)@+l)@+l |> Some

    let addReferenceE = <@@ Scratch.MemoryModel.Collector.addReference @@>
    let addReferenceM, _ = findMethod addReferenceE
    let deleteReferenceE = <@@ Scratch.MemoryModel.Collector.deleteReference @@>
    let deleteReferenceM, _ = findMethod deleteReferenceE
    let newRcE = <@@ Scratch.MemoryModel.Collector.newRc @@>
    let newRcM, _ = findMethod newRcE

    let pointerOffsets layout =
        layout
        |> Seq.indexed
        |> Seq.choose (function
            | offset, UnderlyingTypeSpec(kind = Kind.Collectable) -> Some offset
            | _ -> None
        )
        |> Seq.toList

    let collectables t1 source = context {
        let! ts = underlyingTypeOrError source t1
        return pointerOffsets ts
    }
    let memoryCaseFieldGet senv this p hasTagMember (case: UnionCaseInfo) source = context {
        let thisT = case.DeclaringType
        let fields = case.GetFields()
        let index = fields |> Seq.findIndex ((=) p)
        let! fieldTypes =
            fields
            |> Seq.take index
            |> Result.mapSeq (fun p -> underlyingTypeOrError source p.PropertyType)
        let! UnboxedType thisT = underlyingMemberType senv.e source thisT
        let offset = (if hasTagMember then 1 else 0) + Seq.sumBy List.length fieldTypes

        let! memory = lookupOrRaiseMemory senv source
        let! this = transpilePrimitiveExpression senv this
        let a = SourceCode.location source |> Tagged.empty

        // memory.[this + offset]
        return
            thisT
            |> List.mapi (fun index fieldType ->
                let address = Exp.``+`` a this (Exp.number a (double (offset + index)))
                Exp.reinterpret a (Exp.``getLine:ofList:`` a address (Exp.list a memory.listVar)) (ExpType.ofMemberType fieldType)
            )
            |> Exp.newTuple a

    }
    // let temp = newRc $someCaseVar $memorySize
    // memory.[temp + 0] <- %(tag)
    // memory.[temp + %offsetOf(f1)] <- %field1
    // memory.[temp + %offsetOf(f2)] <- %field2
    // ...
    // temp
    let memoryNew senv runtimeTypeStorageId tagValue fields source memoryLayout = context {
        let l = SourceCode.location source
        let a = Tagged.empty l
        let newRc = lookupOrRaiseProcedure source (methodId newRcM) senv.e
        let! memory = lookupOrRaiseMemory senv source
        let memory = Exp.list a memory.listVar

        let typeInfo =
            match lookupSpec runtimeTypeStorageId senv.e with
            | ValueSome(VarSpec v) -> v
            | _ -> failwith $"internal error: type info variable not found: %A{runtimeTypeStorageId}"

        let memorySize = Exp.number a (double(List.length memoryLayout)) // tagValue
        let callNewRc = Exp.call a newRc.procedureVar [Exp.var a typeInfo.var; memorySize]
        let! fields = fields |> Result.mapSeq (transpileExpression senv)

        return
            Exp.letScope a (get FConfig senv.e).localName callNewRc <| fun temp ->

            let initialOffset, assignTagValue =
                match tagValue with
                | None -> 0, None
                | Some tagValue ->
                    let address = Exp.``+`` a temp (Exp.number a 0.)
                    let assign = Exp.``setLine:ofList:to:`` a address memory tagValue
                    1, Some assign

            let assigns = temp
            let struct(_, assigns) =
                fields
                |> Seq.fold (fun struct(offset, assigns) field ->
                    match Exp.varType field with
                    | UnboxedType t ->

                    t
                    |> Seq.indexed
                    |> Seq.fold (fun struct(offset, assigns) (i, _) ->
                        let address = Exp.``+`` a temp (Exp.number a (double offset))
                        let assign = Exp.``setLine:ofList:to:`` a address memory (Exp.tupleGet a field i)
                        offset + 1, Exp.seq a assign assigns
                    ) (offset, assigns)

                ) (initialOffset, assigns)

            match assignTagValue with
            | None -> assigns
            | Some assign -> Exp.seq a assign assigns
    }

    let newUnionCase senv (c: UnionCaseInfo, es) source = context {
        match unionRepresentation c.DeclaringType with
        | None -> return! skip()
        | Some r ->

        let l = SourceCode.tag source
        match r with
        | UnitLikeUnion _ -> return Exp.empty l

        // `$(c.Name)`
        | EnumLikeUnion _ -> return Exp.string l c.Name
        | OptionLikeUnion(noneCase, _) ->

            if noneCase.Name = c.Name then
                // `0` ( nil )
                return Exp.number l 0.
            else

            // new es
            match memoryLayout <| Choice2Of2 c with
            | None -> return! skip()
            | Some layout -> return! memoryNew senv (unionCaseId c) None es source layout

        // new es
        | RecordLikeUnion _ ->
            match memoryLayout <| Choice2Of2 c with
            | None -> return! skip()
            | Some layout -> return! memoryNew senv (unionCaseId c) None es source layout

        // new tag es
        | ComplexUnion _ ->
            match memoryLayout <| Choice2Of2 c with
            | None -> return! skip()
            | Some layout -> return! memoryNew senv (unionCaseId c) (Some (Exp.string l c.Name)) es source layout
    }
    let expressionPlugin' senv e = context {
        let source = SourceCode.ofExpr e
        match e with
        | E.NewTuple es & ExprType t when newable t ->
            match memoryLayout (Choice1Of2 t) with
            | None -> return! skip()
            | Some layout -> return! memoryNew senv (typeId t) None es source layout

        | E.NewRecord(t, es) when newable t ->
            match memoryLayout (Choice1Of2 t) with
            | None -> return! skip()
            | Some recordLayout -> return! memoryNew senv (typeId t) None es source recordLayout

        | E.NewUnionCase(c, es) when newable c.DeclaringType -> return! newUnionCase senv (c, es) source
        | E.NewRecord(UnderlyingType _ & t, es) when isValueType t ->
            let! xs = Result.mapSeq (transpileExpression senv) es
            return Exp.newTuple (SourceCode.tag source) <| List.ofSeq xs

        | E.PropertyGet(Some this, DeclaringType thisT & p, []) ->
            // { x = x }
            if newable thisT && T.IsRecord(thisT, allowAccessToPrivateRepresentation = true) then
                // memory.[this + %offsetOf(p)] :> 'fieldType

                let! fieldValueLayout = underlyingMemberType senv.e source p.PropertyType
                let l = getLoc e
                let! this = transpilePrimitiveExpression senv this
                let! memory = lookupOrRaiseMemory senv source
                let memory = Exp.list l memory.listVar
                let fieldOffset = Field.recordFieldRaw p
                return
                    fieldValueLayout
                    |> List.mapi (fun offset fieldType ->
                        let address = Exp.``+`` l this (Exp.number l (double (fieldOffset + offset)))
                        Exp.reinterpret l (Exp.``getLine:ofList:`` l address memory) (ExpType.ofMemberType fieldType)
                    )
                    |> Exp.newTuple (SourceCode.tag source)

            // { x = x }
            elif isValueType thisT && T.IsRecord(thisT, allowAccessToPrivateRepresentation = true) then
                let! t = underlyingTypeOrError source thisT
                let! this = transpileExpression senv this
                let a = SourceCode.tag source
                let p = MemberId.propertyId p
                return
                    Seq.indexed t
                    |> Seq.choose (function
                        | i, UnderlyingTypeSpec(path = MemberPath(Member.RecordField p', _)) when p = p' -> Some (Exp.tupleGet a this i)
                        | _ -> None
                    )
                    |> Seq.toList
                    |> Exp.newTuple a

            // Case(item1 = x)
            elif newable thisT && T.IsUnion(thisT, allowAccessToPrivateRepresentation = true) then
                let repr = unionRepresentation thisT
                match repr with
                | None
                | Some(UnitLikeUnion _)
                | Some(EnumLikeUnion _) -> return failwithf "internal error"
                | Some(OptionLikeUnion(_, case))
                | Some(RecordLikeUnion case) ->
                    return! memoryCaseFieldGet senv this p false case source

                | Some(ComplexUnion(case1, case2, cases)) ->
                    let cases = case1::case2::cases
                    let case = cases |> List.find (fun c -> c.GetFields() |> Seq.contains p)
                    return! memoryCaseFieldGet senv this p true case source
            else
                return! raiseError source <| InvalidPropertyType p

        // let thisTemp = $this
        // let valueTemp = $value
        // memory.[thisTemp + %offsetOf(p)] <- valueTemp
        | E.PropertySet(Some this, DeclaringType thisT & p, [], value) ->
            if not (newable thisT && T.IsRecord thisT) then return! raiseError source <| InvalidPropertyType p else

            match TypeSpec.underlyingType p.PropertyType with
            | None -> return! raiseError source <| InvalidPropertyType p
            | Some _ ->

            let l = getLoc e
            let! this = transpilePrimitiveExpression senv this
            let! value = transpileExpression senv value

            let! memory = lookupOrRaiseMemory senv source
            let memory = Exp.list l memory.listVar

            let fieldOffset = Field.recordFieldRaw p
            return
                Exp.letScope l (get FConfig senv.e).localName this <| fun thisTemp ->
                Exp.letScope l (get FConfig senv.e).localName value <| fun valueTemp ->
                let xs =
                    match Exp.varType value with
                    | UnboxedType t ->

                    t
                    |> List.mapi (fun offset _ ->
                        let address = Exp.``+`` l thisTemp (Exp.number l (double (fieldOffset + offset)))
                        Exp.``setLine:ofList:to:`` l address memory (Exp.tupleGet l valueTemp offset)
                    )

                Exp.concat l xs

        | E.UnionCaseTest(this, c) when newable c.DeclaringType ->
            match unionRepresentation c.DeclaringType with
            | None -> return! raiseError source <| InvalidUnionType c
            | Some repr ->

            let! this = transpilePrimitiveExpression senv this
            let l = getLoc e

            match repr with
            // `true`
            | UnitLikeUnion _
            | RecordLikeUnion _ -> return Exp.bool l true

            // `%this = $(c.Name)`
            | EnumLikeUnion _ -> return Exp.``=`` l this (Exp.string l c.Name)

            // `%this = 0` || `not (%this = 0)`
            // nil か nil でないか
            | OptionLikeUnion(noneCase, _) ->
                return
                    if c.Name = noneCase.Name then Exp.``=`` l this (Exp.number l 0.)
                    else Exp.not l (Exp.``=`` l this (Exp.number l 0.))

            // `memory.[%this + tag] :> s = $(c.Name)`
            | ComplexUnion _ ->
                let! memory = lookupOrRaiseMemory senv source
                return Exp.``=`` l (Exp.reinterpret l (Exp.``getLine:ofList:`` l this (Exp.list l memory.listVar)) ExpTypes.string) (Exp.string l c.Name)

        | _ -> return! skip()
    }

    module X = TypedExpr

    /// `Pointer<_>`
    let typeType = ExpTypes.number

    let (@+) e l = withLocationTyped l e
    let addTypeAsVariable senv location id varName pointerOffsets =
        if lookupSpec id (get FExternalItemState senv.s.contents).externalEnv |> VOption.isSome then () else

        let l = location
        let state = ref <| States.make((get FExternalItemState senv.s.contents).externalState)
        let spec = declareVariable NoExport NoPersistent false (varName()) typeType

        // let offsets = mallocVectorExtend Word.size %(pointers.Length)
        // Vector.set offsets 0 %(pointers.[0])
        // Vector.set offsets 1 %(pointers.[1])
        // ...
        // offsets
        let init =
            let offsetsV = TypedVar<N Vector Reference> "offsets"
            let pointers = pointerOffsets |> Seq.map (double >> N) |> Seq.cache
            let offsets = X.Var offsetsV@+l
            let setOffsets = pointers |> Seq.mapi (fun i p -> <@ Vector.set %offsets %(X.Value i@+l) %(X.Value p@+l) @> @+l)
            let setOffsets = Seq.foldBack (fun set offsets -> <@ %set; %offsets @> @+l) setOffsets offsets
            let size = <@ Word.size<N> @> @+l
            let length = X.Value (Seq.length pointers)
            X.Let(offsetsV, <@ MemoryModel.Allocator.mallocVectorExtend %size %length @> @+l, setOffsets)@+l

        modifyRef FExternalItemState senv.s <| fun s -> { s with externalState = get FState state.contents }
        addExternalSpec senv id <| VarSpec spec
        pushExternalAcc senv <| ExternalVariable(spec, init)

    let mallocVectorExtendE = <@@ MemoryModel.Allocator.mallocVectorExtend: N Size -> _ -> _ @@>
    let externalItemsPlugin' senv e = context {
        match e with
        | (E.NewRecord _ | E.NewTuple _) when newable e.Type ->
            let t = e.Type
            match memoryLayout (Choice1Of2 t) with
            | None
            | Some [] -> ()
            | Some layout ->

            let varName() = (get FConfig (get FExternalItemState senv.s.contents).externalEnv).typeName t
            do transpileExternalItemSpecs senv newRcE
            do transpileExternalItemSpecs senv mallocVectorExtendE
            do addTypeAsVariable senv (getLocation e) (typeId t) varName (pointerOffsets layout)

        | E.NewUnionCase(u, _) when newable u.DeclaringType ->
            match memoryLayout (Choice2Of2 u) with
            | None
            | Some [] -> ()
            | Some layout ->

            let varName() = (get FConfig (get FExternalItemState senv.s.contents).externalEnv).unionCaseName u
            do transpileExternalItemSpecs senv newRcE
            do transpileExternalItemSpecs senv mallocVectorExtendE
            do addTypeAsVariable senv (getLocation e) (unionCaseId u) varName (pointerOffsets layout)

        | _ -> return! skip()
    }

let callExpressions() =
    [

    // <@ addReferenceMark $e1 @> => `let temp = $e1 in addReference temp.$N1; addReference temp.$N2; ...`
    [ <@@ addReferenceMark @@> ], fun senv struct(args, e) -> context {
        match args with
        | _, [t1], [e1] ->
            let addReference = lookupOrRaiseProcedure (SourceCode.ofExpr e) (methodId addReferenceM) senv.e
            let! x1 = transpileExpression senv e1
            let! cs = collectables t1 (SourceCode.ofExpr e1)
            let l = getLoc e
            return
                Exp.letScope l (get FConfig senv.e).localName x1 <| fun temp ->
                cs
                |> Seq.map (fun i -> Exp.call l addReference.procedureVar [Exp.tupleGet l temp i])
                |> Seq.toList
                |> Exp.concat l

        | _ -> return! skip()
    }
    // <@ deleteReferenceMark $e1 @> => `let temp = $e1 in deleteReference temp.$N1; deleteReference temp.$N2; ...`
    [ <@@ deleteReferenceMark @@> ], fun senv struct(args, e) -> context {
        match args with
        | _, [t1], [e1] ->
            let deleteReference = lookupOrRaiseProcedure (SourceCode.ofExpr e) (methodId deleteReferenceM) senv.e
            let! x1 = transpileExpression senv e1
            let! cs = collectables t1 (SourceCode.ofExpr e1)
            let l = getLoc e
            return
                Exp.letScope l (get FConfig senv.e).localName x1 <| fun temp ->
                cs
                |> Seq.map (fun i -> Exp.call l deleteReference.procedureVar [Exp.tupleGet l temp i])
                |> Seq.toList
                |> Exp.concat l

        | _ -> return! skip()
    }
    ]

let preTransformPlugin = tryMark

let externalCallItems() =
    [
    [
        <@@ addReferenceMark @@>
    ], fun senv _ ->
        do transpileExternalItemSpecs senv addReferenceE
        Ok()
    [
        <@@ deleteReferenceMark @@>
    ], fun senv _ ->
        do transpileExternalItemSpecs senv deleteReferenceE
        Ok()
    ]

let expressionPlugin() = { new ExpressionPluginProcess() with member _.Invoke senv e = expressionPlugin' senv e }
let externalItemsPlugin() = { new ExternalItemsPluginProcess() with member _.Invoke senv e = externalItemsPlugin' senv e }
