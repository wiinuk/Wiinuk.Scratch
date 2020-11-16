[<AutoOpen>]
module Scratch.Runtime.Test.Helpers
open FsCheck
open Scratch
open Scratch.Ast
open Scratch.AstDefinitions


[<Struct>]
type KnownFooterStatementName = KnownFooterStatementName of Symbol
[<Struct>]
type KnownListenerHeaderName = KnownListenerHeaderName of Symbol
[<Struct>]
type KnownComplexExpressionName = KnownComplexExpressionName of Symbol
[<Struct>]
type KnownValueComplexExpressionName = KnownValueComplexExpressionName of Symbol
[<Struct>]
type KnownUnitComplexExpressionName = KnownUnitComplexExpressionName of Symbol

/// 0. .. 1.
[<Struct>]
type VideoAlphaValue = VideoAlphaValue of double

let nameElements ofString toString names =
    let gen = names |> Seq.map ofString |> Gen.elements
    let shrink x = names |> Seq.filter ((>) (toString x)) |> Seq.map ofString
    Arb.fromGenShrink(gen, shrink)

let (|OptionMap|) f x = Option.map f x
let (|NullableOptionMap|) f x = Option.map (Option.map f) x
let getNonNull = function NonNull x -> x

[<Struct>]
type FooterStatement<'a> = FooterStatement of 'a ComplexExpression
let knownListenerHeaderMap = Map knownListenerHeaders

[<Struct>]
type KnownValueComplexExpression<'a> = KnownValueComplexExpression of 'a ComplexExpression

[<Struct>]
type KnownUnitComplexExpression<'a> = KnownUnitComplexExpression of 'a ComplexExpression

[<Struct>]
type HashWithImageExtension = HashWithImageExtension of string
[<Struct>]
type HashWithSoundExtension = HashWithSoundExtension of string

let complexExpressionArb wrapSymbol (|UnwrapSymbol|) =
    let anyValueGen = Arb.generate<SValue>
    let boolValueGen = Arb.generate<_> |> Gen.map SBool
    let numberValueGen = Arb.generate<_> |> Gen.map (fun (NormalFloat x) -> SNumber x)
    let colorValueGen = Gen.choose (minColorCode, maxColorCode) |> Gen.map (fun x -> SNumber(double x))
    let stringValueGen = Arb.generate<_> |> Gen.map (fun (NonNull x) -> SString x)

    let rec valueGenFromTsType = function
        | TsType.StringSs ss -> Gen.elements ss |> Gen.map SString
        | TsType.GVar _ -> anyValueGen
        | TsType.Or _ as t ->
            let rec flattenOrs acc = function
                | TsType.StringSs _
                | TsType.GVar _
                | TsType.Named _ as t -> t::acc
                | TsType.Or(t1, t2) -> flattenOrs (flattenOrs acc t1) t2

            flattenOrs [] t
            |> List.map valueGenFromTsType
            |> Gen.oneof

        | TsType.Named _ as t ->
            if t = TsType.gBoolean then boolValueGen
            elif t = TsType.gNumber then numberValueGen
            elif t = TsType.gString then stringValueGen
            elif t = TsType.gColor then colorValueGen
            else anyValueGen

    let valueGen = Option.map valueGenFromTsType >> Option.defaultValue anyValueGen
    let literalGen t = gen {
        let! state = Arb.generate<_>
        let! x = valueGen t
        return Literal(state, x)
    }
    let literalOrValueComplexGen t = Gen.sized <| fun size ->
        if size <= 0 then literalGen t else
        Gen.oneof [
            literalGen t
            Arb.generate<_> |> Gen.scaleSize (fun x -> x / 2) |> Gen.map (fun (KnownValueComplexExpression e) -> Complex e)
        ]

    let stringsGen xs = gen {
        let! state = Arb.generate<_>
        let! x = Gen.elements xs
        return [Expression.eString state x]
    }
    let blockGen = gen {
        let! x = Arb.generate<_> |> Gen.scaleSize (fun x -> x / 2)
        return [Block x]
    }
    let stringLiteralGen = gen {
        let! state = Arb.generate<_>
        let! NonNull x = Arb.generate<_>
        return [Expression.eString state x]
    }
    let procedureSignGen =
        Gen.map3 (fun x (NonNull y) z ->
            let z = z |> List.map (fun struct(t, NonNull s) -> struct(t, s))
            ProcedureSign(x, y, z)
        ) Arb.generate<_> Arb.generate<_> Arb.generate<_>

    let procedureNameAndValueExpressionsGen = gen {
        let! state = Arb.generate<_>
        let! sign = procedureSignGen
        let name = ProcedureSign.toEscapedName sign
        let! arguments =
            ProcedureSign.paramTypes sign
            |> Gen.collect (fun p ->
                let t =
                    match p with
                    | SType.S
                    | SType.N -> TsType.gNumberOrString
                    | SType.B -> TsType.gBoolean
                literalOrValueComplexGen (Some t)
            )

        return Expression.eString state name::arguments
    }

    let operandGen info =
        match info.operandType with
        | OperandType.Block -> blockGen
        | OperandType.Expression t ->
            let t =
                match info.literalOperandType with
                | LiteralOperandTypeInfo.Any -> None
                | LiteralOperandTypeInfo.ForceInherit -> Some t
                | LiteralOperandTypeInfo.Force t -> Some t
            t
            |> literalOrValueComplexGen
            |> Gen.map List.singleton

        | OperandType.ListVariableExpression _
        | OperandType.Variable
        | OperandType.ParameterName -> stringLiteralGen
        | OperandType.StringLiterals ss -> stringsGen ss
        | OperandType.ProcedureNameAndExpressions -> procedureNameAndValueExpressionsGen
        | OperandType.ExtensionNameAndExpressions as t -> failwith $"invalid operand type: %A{t}"

    let g = gen {
        let! state = Arb.generate<_>
        let! UnwrapSymbol operator = Arb.generate<_>

        let! operands = knownAllOperatorMap.[operator].operands |> Gen.collectToSeq operandGen
        let operands = Seq.concat operands |> Seq.toList
        return ComplexExpression(state, operator, operands)
    }

    let isValidComplexExpressionOperands operand operands =
        match Map.tryFind operand knownAllOperatorMap with
        | ValueNone -> false
        | ValueSome { operands = specs } ->

        let (|ExpressionKind|_|) = function
            | Literal _ -> Some Kind.Expression
            | Block _ -> Some Kind.Statement
            | Complex(ComplexExpression(operator = KnownOperatorInfo(ValueSome { kind = kind }))) -> Some kind
            | _ -> None

        let rec includes = function
            | TsType.StringSs ss, SString s when List.contains s ss -> true
            | TsType.GVar _, _ -> true
            | TsType.Or(t1, t2), v -> includes (t1, v) || includes (t2, v)
            | TsType.Named _ as t, v ->
                match v with
                | SBool _ -> t = TsType.gBoolean
                | SNumber n -> t = TsType.gNumber || (t = TsType.gColor && isColorCode n)
                | SString _ -> t = TsType.gString

            | _ -> false

        let validateAndTakeOperands (info, operands) =
            match info.operandType, operands with
            | OperandType.Expression t, (ExpressionKind Kind.Expression as operand)::operands ->
                match operand, info.literalOperandType with
                | Literal(_, v), (LiteralOperandTypeInfo.Force _ | LiteralOperandTypeInfo.ForceInherit) when not (includes (t, v)) -> None
                | _ -> Some operands

            | OperandType.Block, Block _::operands
            | (OperandType.Variable | OperandType.ListVariableExpression _ | OperandType.ParameterName), EString _::operands ->
                Some operands

            | OperandType.StringLiterals ss, EString(_, s)::operands when Set.contains s ss ->
                Some operands

            | OperandType.ProcedureNameAndExpressions, EString(_, name)::operands ->
                match ProcedureSign.parse name with
                | ValueNone -> None
                | ValueSome(ProcedureSign(t0, _, tail)) ->

                match t0, operands with
                | Some _, []
                | Some _, ExpressionKind Kind.Statement::_ -> None
                | Some _, _::operands
                | None, operands ->

                let rec validateArguments = function
                    | [], operands -> Some operands
                    | _::signTail, ExpressionKind Kind.Expression::operands -> validateArguments (signTail, operands)
                    | _ -> None

                validateArguments (tail, operands)

            | OperandType.Expression _, _
            | OperandType.Block, _
            | OperandType.Variable, _
            | OperandType.ProcedureNameAndExpressions, _
            | OperandType.ExtensionNameAndExpressions, _
            | OperandType.ParameterName, _
            | OperandType.StringLiterals _, _
            | OperandType.ListVariableExpression _, _
                -> None

        let rec isValidOperands operands specs =
            match operands, specs with
            | [], [] -> true
            | _::_, [] -> false
            | operands, s::specs ->

            match validateAndTakeOperands (s, operands) with
            | Some operands -> isValidOperands operands specs
            | _ -> false

        isValidOperands operands specs

    let s (ComplexExpression(state, operator, operands)) = seq {
        for state, UnwrapSymbol operator, operands in Arb.shrink(state, wrapSymbol operator, operands) do
            if isValidComplexExpressionOperands operator operands then
                ComplexExpression(state, operator, operands)
    }
    Arb.fromGenShrink(g, s)

let hashWithExtensionArb (wrap, extensions) =
    let md5HashGen af =
        Gen.oneof [
            Gen.elements ['0'..'9']
            Gen.elements af
        ]
        |> Gen.arrayOfLength 32

    let md5HashGen = Gen.oneof [md5HashGen ['a'..'f']; md5HashGen ['A'..'F']]
    let extensionGen = Gen.elements extensions

    let g = gen {
        let! hexChars = md5HashGen
        let! ext = extensionGen
        return wrap <| sprintf "%s.%s" (System.String hexChars) ext
    }
    Arb.fromGen g

type Arbs =
    static member KnownFooterStatementName() =
        knownAllOperatorMap
        |> Map.toSeq
        |> Seq.filter (fun (_, v) -> v.isFooter)
        |> Seq.map fst
        |> nameElements KnownFooterStatementName (fun (KnownFooterStatementName x) -> x)

    static member KnownListenerHeaderName() =
        knownListenerHeaders
        |> Seq.map fst
        |> nameElements KnownListenerHeaderName (fun (KnownListenerHeaderName x) -> x)

    static member KnownComplexExpressionName() =
        knownAllOperatorMap
        |> Map.toSeq
        |> Seq.map fst
        |> nameElements KnownComplexExpressionName (fun (KnownComplexExpressionName x) -> x)

    static member KnownValueComplexExpressionName() =
        knownAllOperatorMap
        |> Map.toSeq
        |> Seq.filter (fun (_, v) -> v.kind = Kind.Expression)
        |> Seq.map fst
        |> nameElements KnownValueComplexExpressionName (fun (KnownValueComplexExpressionName x) -> x)

    static member KnownUnitComplexExpressionName() =
        knownAllOperatorMap
        |> Map.toSeq
        |> Seq.filter (fun (_, v) -> v.kind = Kind.Statement)
        |> Seq.map fst
        |> nameElements KnownUnitComplexExpressionName (fun (KnownUnitComplexExpressionName x) -> x)

    static member VideoAlphaValue() =
        Arb.from<_>
        |> Arb.convert
            (fun (NormalFloat x) -> VideoAlphaValue(x % 1.))
            (fun (VideoAlphaValue x) -> NormalFloat x)

    static member HashWithImageExtension() =
        hashWithExtensionArb (
            HashWithImageExtension,
            ["png"; "svg"; "jpeg"; "jpg"; "bmp"; "gif"]
        )

    static member HashWithSoundExtension() =
        hashWithExtensionArb (
            HashWithSoundExtension,
            ["wav"; "wave"; "mp3"]
        )

    static member SValue() =
        let sString = gen {
            let! NonNull s = Arb.generate<_>
            return SString s
        }
        let sNumber = gen {
            let! NormalFloat x = Arb.generate<_>
            return SNumber x
        }
        let sBool = gen {
            let! x = Arb.generate<_>
            return SBool x
        }
        let g = Gen.oneof [sString; sNumber; sBool]
        let s x = seq {
            match x with
            | SString x -> for NonNull x in Arb.shrink(NonNull x) -> SString x
            | SNumber x -> for NormalFloat x in Arb.shrink(NormalFloat x) -> SNumber x
            | SBool x -> for x in Arb.shrink x -> SBool x
        }
        Arb.fromGenShrink(g, s)

    static member KnownValueComplexExpression() =
        complexExpressionArb KnownValueComplexExpressionName (fun (KnownValueComplexExpressionName x) -> x)
        |> Arb.convert KnownValueComplexExpression (fun (KnownValueComplexExpression x) -> x)

    static member KnownUnitComplexExpression() =
        complexExpressionArb KnownUnitComplexExpressionName (fun (KnownUnitComplexExpressionName x) -> x)
        |> Arb.convert KnownUnitComplexExpression (fun (KnownUnitComplexExpression x) -> x)

    static member ComplexExpression() =
        complexExpressionArb KnownComplexExpressionName (fun (KnownComplexExpressionName x) -> x)

    static member FooterStatement() =
        let g = gen {
            let! state = Arb.generate<_>
            let! KnownFooterStatementName operator = Arb.generate<_>
            let! operands = Arb.generate<_>
            return FooterStatement(ComplexExpression(state, operator, operands))
        }
        let s (FooterStatement(ComplexExpression(state, operator, operands))) = seq {
            for state, KnownFooterStatementName operator, operands in Arb.shrink(state, KnownFooterStatementName operator, operands) ->
                FooterStatement(ComplexExpression(state, operator, operands))
        }
        Arb.fromGenShrink(g, s)

    static member BlockExpression() =
        Arb.from<_>
        |> Arb.convert
            (fun (state, body) -> BlockExpression(state, body |> List.map (fun (KnownUnitComplexExpression x) -> x)))
            (fun (BlockExpression(state, body)) -> state, body |> List.map KnownUnitComplexExpression)

    static member ListenerDefinition() =
        let argumentGen spec = gen {
            let! state = Arb.generate<_>
            match spec with
            | ListenerHeaderType.AnyOrKeyName
            | ListenerHeaderType.EventName
            | ListenerHeaderType.String ->
                let! NonNull x = Arb.generate<_>
                return Literal(state, SString x)

            | ListenerHeaderType.Bool ->
                let! x = Arb.generate<_>
                return Literal(state, SBool x)

            | ListenerHeaderType.Null ->
                return Block(BlockExpression(state, []))
        }
        let isValidListenerArguments name arguments =
            match Map.tryFind name knownListenerHeaderMap with
            | ValueNone -> false
            | ValueSome specs ->

            if List.length arguments <> List.length specs then false else

            // TODO:
            true

        let g = gen {
            let! state = Arb.generate<_>
            let! KnownListenerHeaderName name = Arb.generate<_>
            let! arguments = knownListenerHeaderMap.[name] |> Gen.collect argumentGen
            let! body = Arb.generate<_>
            return ListenerDefinition(state, name, arguments, body)
        }
        let s (ListenerDefinition(state, name, arguments, body)) = seq {
            for state, KnownListenerHeaderName name, arguments, body in Arb.shrink (state, KnownListenerHeaderName name, arguments, body) do
                if isValidListenerArguments name arguments then
                    ListenerDefinition(state, name, arguments, body)
        }
        Arb.fromGenShrink(g, s)

    static member ListData() =
        Arb.from
        |> Arb.convert
            (fun (state, isPersistent, NonNull listName, contents, NormalFloat x, NormalFloat y, NormalFloat width, NormalFloat height, visible) ->
                {
                    state = state
                    isPersistent = isPersistent
                    listName = listName
                    contents' = contents

                    x = x
                    y = y
                    width = width
                    height = height
                    visible = visible
                }
            )
            (fun x -> (x.state, x.isPersistent, NonNull x.listName, x.contents', NormalFloat x.x, NormalFloat x.y, NormalFloat x.width, NormalFloat x.height, x.visible))

    static member ProcedureDefinition() =
        let parameterType (ParameterDefinition(defaultValue = v)) =
            match v with
            | SString _ -> SType.S
            | SNumber _ -> SType.N
            | SBool _ -> SType.B

        let generator = gen {
            let! state = Arb.generate<_>

            let! parameter0 = Arb.generate<_>
            let! NonNull head = Arb.generate<_>
            let! tailAndParameters = Arb.generate<_>

            let type0 = parameter0 |> Option.map parameterType
            let tail =
                tailAndParameters
                |> List.map (fun (NonNull text, p) ->
                    struct(parameterType p, text)
                )

            let name = ProcedureSign.toEscapedName (ProcedureSign(type0, head, tail))
            let parameters = Option.toList parameter0 @ List.map snd tailAndParameters

            let! isAtomic = Arb.generate<_>
            let! body = Arb.generate<_>
            return ProcedureDefinition(state, name, parameters, isAtomic, body)
        }
        let shrinker (ProcedureDefinition(state, name, parameters, isAtomic, body)) = seq {
            for state, NonNull name, parameters, isAtomic, body in Arb.shrink (state, NonNull name, parameters, isAtomic, body) do
                match ProcedureSign.parse name with
                | ValueSome sign when ProcedureSign.paramCount sign = List.length parameters ->
                    ProcedureDefinition(state, name, parameters, isAtomic, body)

                | _ -> ()
        }
        Arb.fromGenShrink(generator, shrinker)

    static member Script() =
        Arb.from
        |> Arb.convert
            (function
                | Choice1Of4 x -> Listener x
                | Choice2Of4 x -> Procedure x
                | Choice3Of4(state, NonEmptyArray xs) ->
                    Statements <| BlockExpression(state, [ for KnownUnitComplexExpression x in xs -> x ])

                | Choice4Of4(KnownValueComplexExpression x) -> Expression x
            )
            (function
                | Listener x -> Choice1Of4 x
                | Procedure x -> Choice2Of4 x
                | Statements(BlockExpression(state, xs)) ->
                    let xs = NonEmptyArray [| for x in xs -> KnownUnitComplexExpression x |]
                    Choice3Of4(state, xs)

                | Expression x -> Choice4Of4(KnownValueComplexExpression x)
            )
        |> Arb.filter (function

            // (print -> parse) のとき等しくないパターン
            | Statements(BlockExpression(body = [ComplexExpression(operator = KnownOperatorInfo(ValueSome { kind = Kind.Expression }))])) -> false

            | _ -> true
        )

    static member ScriptData() =
        Arb.from
        |> Arb.convert
            (fun (NormalFloat x, NormalFloat y, script) ->
                { ScriptData.x = x; y = y; script = script }
            )
            (fun x -> (NormalFloat x.x, NormalFloat x.y, x.script))

    static member CostumeData() =
        Arb.from
        |> Arb.convert
            (fun
                (
                    HashWithImageExtension baseLayerMD5,
                    NormalFloat baseLayerID,
                    textLayerMD5,
                    OptionMap NormalFloat.op_Explicit textLayerID,
                    OptionMap NormalFloat.op_Explicit bitmapResolution,
                    NonNull costumeName,
                    NormalFloat rotationCenterX,
                    NormalFloat rotationCenterY
                ) ->
                {
                    baseLayerMD5 = baseLayerMD5
                    baseLayerID = baseLayerID
                    textLayerMD5 = textLayerMD5
                    textLayerID = textLayerID
                    bitmapResolution = bitmapResolution
                    costumeName = costumeName
                    rotationCenterX = rotationCenterX
                    rotationCenterY = rotationCenterY
                }
            )
            (fun x ->
                (
                    HashWithImageExtension x.baseLayerMD5,
                    NormalFloat x.baseLayerID,
                    x.textLayerMD5,
                    Option.map NormalFloat x.textLayerID,
                    Option.map NormalFloat x.bitmapResolution,
                    NonNull x.costumeName,
                    NormalFloat x.rotationCenterX,
                    NormalFloat x.rotationCenterY
                )
            )

    static member SoundData() =
        Arb.from
        |> Arb.convert
            (fun
                (
                    HashWithSoundExtension md5,
                    NormalFloat soundID,
                    NonNull soundName,
                    OptionMap NormalFloat.op_Explicit sampleCount,
                    rate,
                    format
                ) ->
                {
                    md5 = md5
                    soundID = soundID
                    soundName = soundName
                    sampleCount = sampleCount
                    rate = rate
                    format = format
                }
            )
            (fun x ->
                (
                    HashWithSoundExtension x.md5,
                    NormalFloat x.soundID,
                    NonNull x.soundName,
                    Option.map NormalFloat x.sampleCount,
                    x.rate,
                    x.format
                )
            )

    static member WatcherData() =
        Arb.from
        |> Arb.convert
            (fun
                (
                    NonNull cmd,
                    OptionMap getNonNull param,
                    NonNull target,
                    OptionMap NormalFloat.op_Explicit color,
                    isDiscrete,
                    OptionMap getNonNull label,
                    OptionMap NormalFloat.op_Explicit mode,
                    NullableOptionMap NormalFloat.op_Explicit sliderMax,
                    NullableOptionMap NormalFloat.op_Explicit sliderMin,
                    visible,
                    NullableOptionMap NormalFloat.op_Explicit x,
                    NullableOptionMap NormalFloat.op_Explicit y
                ) ->
                {
                    cmd = cmd
                    param = param
                    target = target
                    color = color
                    isDiscrete = isDiscrete
                    label = label
                    mode = mode
                    sliderMax = sliderMax
                    sliderMin = sliderMin
                    visible = visible
                    x = x
                    y = y
                }
            )
            (fun x ->
                (
                    NonNull x.cmd,
                    Option.map NonNull x.param,
                    NonNull x.target,
                    Option.map NormalFloat x.color,
                    x.isDiscrete,
                    Option.map NonNull x.label,
                    Option.map NormalFloat x.mode,
                    (|NullableOptionMap|) NormalFloat x.sliderMax,
                    (|NullableOptionMap|) NormalFloat x.sliderMin,
                    x.visible,
                    (|NullableOptionMap|) NormalFloat x.x,
                    (|NullableOptionMap|) NormalFloat x.y
                )
            )
    static member SpriteDataExtension() =
        Arb.from
        |> Arb.convert
            (fun
                (
                    NormalFloat direction,
                    NormalFloat indexInLibrary,
                    isDraggable,
                    rotationStyle,
                    NormalFloat scale,
                    NormalFloat scratchX,
                    NormalFloat scratchY,
                    spriteInfo,
                    visible
                ) ->
                {
                    direction = direction
                    indexInLibrary = indexInLibrary
                    isDraggable = isDraggable
                    rotationStyle = rotationStyle
                    scale = scale
                    scratchX = scratchX
                    scratchY = scratchY
                    spriteInfo = spriteInfo
                    visible = visible
                }
            )
            (fun x ->
                (
                    NormalFloat x.direction,
                    NormalFloat x.indexInLibrary,
                    x.isDraggable,
                    x.rotationStyle,
                    NormalFloat x.scale,
                    NormalFloat x.scratchX,
                    NormalFloat x.scratchY,
                    x.spriteInfo,
                    x.visible
                )
            )

    static member VariableData() =
        Arb.from
        |> Arb.convert
            (fun (state, isPersistent, NonNull name, value) ->
                {
                    state = state
                    isPersistent = isPersistent
                    name = name
                    value = value
                }
            )
            (fun x ->
                (
                    x.state,
                    x.isPersistent,
                    x.name |> NonNull,
                    x.value
                )
            )

    static member StageDataExtension() =
        Arb.from
        |> Arb.convert
            (fun (children, penLayerMD5, penLayerID, tempoBPM, videoAlpha, info) ->
                {
                    children = children
                    penLayerMD5 = penLayerMD5 |> Option.map (fun (NonNull x) -> x)
                    penLayerID = penLayerID |> Option.map (fun (NormalFloat x) -> x)
                    tempoBPM = tempoBPM |> NormalFloat.op_Explicit
                    videoAlpha = videoAlpha |> Option.map (fun (VideoAlphaValue x) -> x)
                    info = info |> Map.toSeq |> Seq.map (fun (NonNull k, v) -> k, v) |> Map.ofSeq
                }
            )
            (fun x ->
                (
                    x.children,
                    x.penLayerMD5 |> Option.map NonNull,
                    x.penLayerID |> Option.map NormalFloat,
                    x.tempoBPM |> NormalFloat,
                    x.videoAlpha |> Option.map VideoAlphaValue,
                    x.info |> Map.toSeq |> Seq.map (fun (k, v) -> NonNull k, v) |> Map.ofSeq
                )
            )

    static member ObjectData() =
        Arb.from
        |> Arb.convert
            (fun
                (
                    NonNull objName,
                    scripts,
                    costumes,
                    sounds,
                    variables,
                    lists,
                    NormalFloat currentCostumeIndex,
                    objectDataExtension
                ) ->
                {
                    objName = objName
                    scripts = scripts
                    costumes = costumes
                    sounds = sounds
                    variables = variables
                    lists = lists
                    currentCostumeIndex = abs currentCostumeIndex
                    ObjectDataExtension = objectDataExtension
                }
            )
            (fun x ->
                (
                NonNull x.objName,
                x.scripts,
                x.costumes,
                x.sounds,
                x.variables,
                x.lists,
                NormalFloat x.currentCostumeIndex,
                x.ObjectDataExtension
                )
            )

let quickcheck test = qcheckWith (fun c -> { c with Arbitrary = typeof<Arbs> :: c.Arbitrary }) test
