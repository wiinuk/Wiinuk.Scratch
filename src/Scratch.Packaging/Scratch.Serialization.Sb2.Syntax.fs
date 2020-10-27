module Scratch.Serialization.Sb2.Syntax
open Scratch.Primitives
open Scratch.Ast
open Scratch.Json.PartialIsomorphisms
open Scratch.Json.PartialIsomorphisms.Iso
open Scratch.Json.PartialIsomorphisms.Iso.Values
open Scratch.Json.PartialIsomorphisms.Reflected.Iso.Values
open Scratch
open Scratch.Json.Utf8
open Scratch.Json.Utf8.Syntax
open Scratch.Json.Utf8.Syntax.Values


[<Struct; NoEquality; NoComparison>]
type InSet<'a> when 'a : comparison = private { set: 'a Set } with
    interface IsoR<'a,'a> with
        member f.Forward(x, result) =
            if Set.contains x f.set then result <- x; true
            else false

        member f.Reverse(x, result) =
            if Set.contains x f.set then result <- x; true
            else false
let set set = { InSet.set = set }
let (^^) x xs = { vhead = x; vtail = xs }

[<Struct>]
type ValueToTuple1<'T,'R,'Iso> when 'Iso :> IsoR<mvcons<'T, HUnit>,'R> = internal { mutable f: 'Iso } with
    interface IsoR<'T,'R> with
        member f.Forward(x, result) =
            let mutable x = { vhead = x; vtail = HUnit }
            f.f.Forward(&x, &result)

        member f.Reverse(x, result) =
            let mutable x' = { vhead = result; vtail = HUnit }
            let r = f.f.Reverse(&x, &x')
            result <- x'.vhead
            r

let isoFromUnion1 e = { ValueToTuple1.f = isoFromUnion1 e }
let knownListenerHeaders = Scratch.AstDefinitions.knownListenerHeaders |> Seq.map fst |> Set.ofSeq

let sstring' = isoFromUnion1 <@ SString @>
let snumber' = isoFromUnion1 <@ SNumber @>
let sbool' = isoFromUnion1 <@ SBool @>

let blockEq a b =
    match a, b with
    | Block(BlockExpression(_, [])), Block(BlockExpression(_, [])) -> true
    | _ -> false

/// `number | string | boolean` :> SValue
let sValue =
    (jString |>> sstring') <|>
    (jNumber |>> snumber') <|>
    (jBoolean |>> sbool')

[<Struct>]
type ConsDefault<'_s,'t,'a> when 't :> HList and '_s : struct and '_s :> IHasDefaultShape<'a> = | ConsDef with
    interface IsoR<'t, mvcons<'a,'t>> with
        member _.Forward(x, result) =
            result.vhead <- HasDefault.getDefault the<'_s>
            result.vtail <- x
            true

        member _.Reverse(x, result) = result <- x.vtail; true
let consDefault (_s: The<'_s>) = ConsDefault<'_s,_,_>.ConsDef

let sValues = jList sValue
let jStrings = jList jString

[<Struct>]
type ToSymbol = | ToSymbol with
    interface IsoR<string, Symbol> with
        member _.Forward(x, result) = result <- Symbol.ofName x; true
        member _.Reverse(x, result) = result <- Symbol.name x; true

let jSymbol = jString |>> ToSymbol

let makeScriptData (_s: The<'_s>) =

    let expression, _expression = forwardedToRef()

    let complexExpression' = consDefault _s >><< isoFromUnion3 <@ ComplexExpression @>
    /// `string, ...expression[]]` :> ComplexExpression
    let complexExpression =
        jSymbol ** jTupleToList expression
        |> jArray
        |>> complexExpression'

    let literal' = valueToTuple1 >><< consDefault _s >><< isoFromUnion2 <@ Literal @>
    let complex' = isoFromUnion1 <@ Complex @>
    let block' = isoFromUnion1 <@ Block @>

    let blockExpression' = valueToTuple1 >><< consDefault _s >><< isoFromUnion2 <@ BlockExpression @>
    let operand = expression

    let statement = complexExpression
    let statements = jTupleToList statement
    let block = jList statement |>> blockExpression'

    let None = HasDefault.getDefault _s
    _expression := upcast (
        (jNull |>> singletonWith blockEq (Block(BlockExpression(None, [])))) <|>
        (sValue |>> literal') <|>
        (complexExpression |>> complex') <|>
        (block |>> block')
    )

    let listenerHeader =
        (jSymbol |>> set knownListenerHeaders) ** jTupleToList operand
        |> jArray

    let makeListenerDefinition = {
        forward = fun t ->
            let { vhead = name; vtail = { vhead = args } } = t.vhead
            let xs = t.vtail.vhead
            Ok <| ListenerDefinition(None, name, args, BlockExpression(None, xs))

        reverse = fun (ListenerDefinition(_, name, args, BlockExpression(_, xs))) ->
            Ok <| (name^^args^^HUnit)^^xs^^HUnit
    }
    let listenerDefinition =
        listenerHeader ** statements
        |> jArray
        |>> makeListenerDefinition

    let procedureHeader =
        jStringLiteral "procDef" ** jString ** jStrings ** sValues ** jBoolean ** jEmptyArray
        |> jArray

    let makeProcedureDefinition = {
        forward = fun t ->
            let h = &t.vhead
            let name = h.vtail.vhead
            let ns = h.vtail.vtail.vhead
            let vs = h.vtail.vtail.vtail.vhead
            let isAtomic = h.vtail.vtail.vtail.vtail.vhead
            let xs = t.vtail.vhead

            // TODO: name check
            let ps = Seq.zip ns vs
            let ps = ps |> Seq.map (fun (n, v) -> ParameterDefinition(None, n, v)) |> Seq.toList
            let isAtomic = if isAtomic then Atomic else NoAtomic
            Ok <| ProcedureDefinition(None, name, ps, isAtomic, BlockExpression(None, xs))

        reverse = fun (ProcedureDefinition(_, name, ps, isAtomic, BlockExpression(_, xs))) ->
            let (ns: string list), vs = ps |> List.map (fun (ParameterDefinition(_, n, v)) -> n, v) |> List.unzip
            let isAtomic = match isAtomic with Atomic -> true | NoAtomic -> false
            Ok <| (HUnit^^name^^ns^^vs^^isAtomic^^HUnit)^^xs^^HUnit
    }

    let procedureDefinition =
        procedureHeader ** statements
        |> jArray
        |>> makeProcedureDefinition

    let listener' = isoFromUnion1 <@ Listener @>
    let procedure' = isoFromUnion1 <@ Procedure @>
    let expression' = isoFromUnion1 <@ Expression @>

    let expression =
        complexExpression ** jEmptyArray |> jArray |>> {
            forward = fun x ->
                match x.vhead with
                | ComplexExpression(operator = AstDefinitions.KnownOperatorInfo(ValueSome { AstDefinitions.kind = AstDefinitions.Kind.Expression })) as e ->
                    e |> Expression |> Ok

                | _ -> Iso.error "expression"

            reverse = function
                | Expression(ComplexExpression(operator = AstDefinitions.KnownOperatorInfo(ValueSome { AstDefinitions.kind = AstDefinitions.Kind.Expression })) as e) ->
                    Ok { vhead = e; vtail = HUnit }

                | _ -> Iso.error "expression"
        }
    let statements = {
        forward = fun x -> Ok <| Statements x
        reverse = function
            | Statements x -> Ok x
            | _ -> Iso.error "expressionOrStatements"
    }
    // type ComplexExpression = [operator: string, ...operands: Expression[]]
    // type Statement = ComplexExpression /* kind = Statement */
    // type Expression = ComplexExpression /* kind = Expression */ 
    // type Script =
    //     | [["procDef", ...unknown[]]]            // ⇔ Script.Procedure
    //     | [[KnownListenerName, ...unknown[]]]    // ⇔ Script.Listener
    //     | [Expression]                           // ⇔ Script.Expression ( kind = Expression )
    //     | Statement[]                            // ⇔ Script.Statements
    let script =
        (listenerDefinition |>> listener') <|>
        (procedureDefinition |>> procedure') <|>
        expression <|>
        (block |>> statements)

    let scriptData' = isoFromRecord <@ fun (r: _ ScriptData) -> r.x^^r.y^^r.script^^HUnit @>
    let scriptData =
        (jNumber ** jNumber ** script ** jEmptyArray)
        |> jArray
        |>> scriptData'

    Syntax.box scriptData, Syntax.box script, Syntax.box statement, Syntax.box complexExpression

[<AbstractClass; Sealed>]
type private ScriptDataHolder<'_s,'a> when '_s : struct and '_s :> IHasDefaultShape<'a> private () =
    static let value1, value2, value3, value4:
        'a ScriptData ISyntax *
        'a Script ISyntax *
        'a ComplexExpression ISyntax *
        'a ComplexExpression ISyntax = makeScriptData the<'_s>
    static member Value1 = value1
    static member Value2 = value2
    static member Value3 = value3
    static member Value4 = value4

let statement (_: The<'_s>) = ScriptDataHolder<'_s,_>.Value3
let script (_: The<'_s>) = ScriptDataHolder<'_s,_>.Value2
let scriptData (_: The<'_s>) = ScriptDataHolder<'_s,_>.Value1
let complexExpression (_: The<'_s>) = ScriptDataHolder<'_s,_>.Value4

let costumeData' = isoFromRecord <@ fun r ->
    r.baseLayerMD5^^
    r.baseLayerID^^
    r.textLayerMD5^^
    r.textLayerID^^

    r.bitmapResolution^^
    r.costumeName^^
    r.rotationCenterX^^
    r.rotationCenterY^^
    HUnit
@>

let costumeData =
    ("baseLayerMD5", jString) @@
    ("baseLayerID", jNumber) @@
    ("textLayerMD5", jString) @@?
    ("textLayerID", jNumber) @@?

    ("bitmapResolution", jNumber) @@?
    ("costumeName", jString) @@
    ("rotationCenterX", jNumber) @@
    ("rotationCenterY", jNumber) @@
    jEmptyObject
    |> jObject
    |>> costumeData'

let soundRate =
    (jNumberLiteral 11025. |>> singleton R11025) <|>
    (jNumberLiteral 22050. |>> singleton R22050) <|>
    (jNumberLiteral 48000. |>> singleton R48000)

let soundFormat =
    (jStringLiteral "" |>> singleton EmptyFormat) <|>
    (jStringLiteral "adpcm" |>> singleton Adpcm)

let soundData' = isoFromRecord <@ fun r -> r.md5^^r.soundID^^r.soundName^^r.sampleCount^^r.rate^^r.format^^HUnit @>
let soundData =
    ("md5", jString) @@
    ("soundID", jNumber) @@
    ("soundName", jString) @@
    ("sampleCount", jNumber) @@?
    ("rate", soundRate) @@?
    ("format", soundFormat) @@?
    jEmptyObject
    |> jObject
    |>> soundData'

let persistence =
    (jTrue |>> singleton Persistent) <|>
    (jFalse |>> singleton NoPersistent)

let visibility =
    (jTrue |>> singleton Visible) <|>
    (jFalse |>> singleton Hidden)

let objectData (_s: The<'_s>) restToExtensionData =
    let variableData' = consDefault _s >><< isoFromRecord <@ fun (r: _ VariableData) -> r.state^^r.isPersistent^^r.name^^r.value^^HUnit @>
    let variableData =
        ("isPersistent", persistence, NoPersistent) @@!
        ("name", jString) @@
        ("value", sValue) @@
        jEmptyObject
        |> jObject
        |>> variableData'

    let listVariableData' = consDefault _s >><< isoFromRecord <@ fun (r: _ ListVariableData) -> 
        r.state^^r.isPersistent^^r.listName^^r.contents'^^r.x^^r.y^^r.width^^r.height^^r.visible^^HUnit
    @>

    let listVariableData =
        ("isPersistent", persistence, NoPersistent) @@!
        ("listName", jString) @@
        ("contents", jImmutableArray sValue) @@
        ("x", jNumber) @@
        ("y", jNumber) @@
        ("width", jNumber) @@
        ("height", jNumber) @@
        ("visible", visibility) @@
        jEmptyObject
        |> jObject
        |>> listVariableData'

    let objectData' = isoFromRecord <@ fun r -> 
        r.objName^^
        r.scripts^^
        r.costumes^^
        r.sounds^^
        r.variables^^
        r.lists^^
        r.currentCostumeIndex^^
        r.ObjectDataExtension^^
        HUnit
    @>

    let objectData =
        ("objName", jString) @@
        ("scripts", jList (scriptData _s), []) @@!
        ("costumes", jList costumeData, []) @@!
        ("sounds", jList soundData, []) @@!
        ("variables", jList variableData, []) @@!
        ("lists", jList listVariableData, []) @@!
        ("currentCostumeIndex", jNumber) @@?
        restToExtensionData
        |> jObject
        |>> objectData'

    objectData, listVariableData

let watcherData' = isoFromRecord <@ fun r ->
    r.cmd^^r.param^^r.target^^r.color^^r.isDiscrete^^r.label^^r.mode^^r.sliderMax^^r.sliderMin^^r.visible^^r.x^^r.y^^HUnit
@>
let watcherData =
    ("cmd", jString) @@
    ("param", jNullable jString) @@
    ("target", jString) @@
    ("color", jNumber) @@?
    ("isDiscrete", jNullable jBoolean) @@?
    ("label", jString) @@?
    ("mode", jNumber) @@?
    ("sliderMax", jNullable jNumber) @@?
    ("sliderMin", jNullable jNumber) @@?
    ("visible", jNullable visibility) @@?
    ("x", jNullable jNumber) @@?
    ("y", jNullable jNumber) @@?
    jEmptyObject
    |> jObject
    |>> watcherData'

let makeStageData _s =
    let rotationStyle =
        (jStringLiteral "none" |>> singleton RotationStyle.None) <|>
        (jStringLiteral "normal" |>> singleton RotationStyle.Normal) <|>
        (jStringLiteral "leftRight" |>> singleton RotationStyle.LeftRight)

    let info = jMap sValue

    let spriteDataExtension' = isoFromRecord <@ fun (r: SpriteDataExtension) ->
        r.direction^^r.indexInLibrary^^r.isDraggable^^r.rotationStyle^^
        r.scale^^r.scratchX^^r.scratchY^^r.spriteInfo^^r.visible^^HUnit
    @>

    let spriteDataExtension =
        ("direction", jNumber) @@
        ("indexInLibrary", jNumber) @@
        ("isDraggable", jBoolean) @@
        ("rotationStyle", rotationStyle) @@
        ("scale", jNumber) @@
        ("scratchX", jNumber) @@
        ("scratchY", jNumber) @@
        ("spriteInfo", info) @@
        ("visible", visibility) @@

        // ignore "scriptComments" property
        ignoreProperties

        |>>@ spriteDataExtension'
        |>>@ valueToTuple1

    let spriteData, listVariableData = objectData _s spriteDataExtension
    let spriteData = Syntax.box spriteData

    let stageChild =
        (watcherData |>> isoFromUnion1 <@ Choice1Of3 @>) <|>
        (spriteData |>> isoFromUnion1 <@ Choice2Of3 @>) <|>
        (listVariableData |>> isoFromUnion1 <@ Choice3Of3 @>)

    let stageDataExtension' = isoFromRecord <@ fun r ->
        r.children^^r.penLayerMD5^^r.penLayerID^^r.tempoBPM^^r.videoAlpha^^r.info^^HUnit
    @>
    let stageDataExtension =
        // < 2.x: { children?: ReadonlyArray<WatcherData | SpriteData> }
        // 3.x: { children: ReadonlyArray<WatcherData | SpriteData> }
        ("children", jList stageChild, []) @@!
        ("penLayerMD5", jString) @@?
        ("penLayerID", jNumber) @@?
        ("tempoBPM", jNumber) @@?
        ("videoAlpha", jNumber) @@?
        ("info", info, Map.empty) @@!

        // ignore scriptComments property
        ignoreProperties

        |>>@ stageDataExtension'
        |>>@ valueToTuple1

    let stageData, _ = objectData _s stageDataExtension
    stageData :> _ ISyntax, spriteData :> _ ISyntax

[<Sealed; AbstractClass>]
type private SyntaxHolder<'_s,'a> when '_s : struct and '_s :> IHasDefaultShape<'a> private () =
    static let value1, value2: 'a StageData ISyntax * 'a SpriteData ISyntax = makeStageData the<'_s>
    static member Value1 = value1
    static member Value2 = value2

let spriteData (_s: The<'_s>) = SyntaxHolder<'_s,_>.Value2
let stageData (_s: The<'_s>) = SyntaxHolder<'_s,_>.Value1
