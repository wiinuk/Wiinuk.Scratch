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

let complexExpressionArb wrapSymbol (|UnwrapSymbol|) =
    let literalGen = Arb.generate<_> |> Gen.map Literal
    let literalOrValueComplexGen = Gen.sized <| fun size ->
        if size <= 0 then literalGen else
        Gen.oneof [
            literalGen
            Arb.generate<_> |> Gen.scaleSize (fun x -> x / 2) |> Gen.map (fun (KnownValueComplexExpression e) -> Complex e)
        ]
    let stringsGen strings = gen {
        let! state = Arb.generate<_>
        let! x = Gen.elements strings
        return [Expression.eString state x]
    }
    let blockGen = gen {
        let! x = Arb.generate<_> |> Gen.scaleSize (fun x -> x / 2)
        return [Block x]
    }
    let valueExpressionGen = gen {
        let! x = literalOrValueComplexGen
        return [x]
    }
    let listVarGen = gen {
        let! state = Arb.generate<_>
        let! NonNull x = Arb.generate<_>
        return [Expression.eString state x]
    }
    let reporterGen = stringsGen ["r"]
    let rotationGen = stringsGen ["left-right"; "don't rotate"; "normal"]
    let stopGen = stringsGen ["other scripts in sprite"; "other scripts in stage"]
    let stopScriptGen = stringsGen ["all"; "this script"]
    let varGen = gen {
        let! state = Arb.generate<_>
        let! NonNull x = Arb.generate<_>
        return [Expression.eString state x]
    }
    let valueExpressionsGen = Gen.listOf literalOrValueComplexGen
    let operandGen = function
        | OperandType.Block -> blockGen
        | OperandType.Expression t ->
            match t with
            | TsType.StringSs strings -> stringsGen strings

            // TODO:
            | _ -> valueExpressionGen

        | OperandType.ListVariableExpression _ -> listVarGen
        | OperandType.Reporter -> reporterGen
        | OperandType.Rotation -> rotationGen
        | OperandType.Stop -> stopGen
        | OperandType.StopScript -> stopScriptGen
        | OperandType.Variable -> varGen
        | OperandType.VariadicExpressions -> valueExpressionsGen

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
            
        let validateAndTakeOperands = function
            | OperandType.Expression _, ExpressionKind Kind.Expression::operands
            | OperandType.Block, Block _::operands
            | OperandType.ListVariableExpression _, EString _::operands
            | OperandType.Reporter, EString _::operands
            | OperandType.Rotation, EString _::operands
            | OperandType.Stop, EString _::operands
            | OperandType.StopScript, EString _::operands
            | OperandType.Variable, EString _::operands ->
                Some operands

            | OperandType.VariadicExpressions, operands ->
                operands |> List.takeWhile (function ExpressionKind Kind.Expression -> true | _ -> false) |> Some

            | _ -> None

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

    static member Script() =
        Arb.from
        |> Arb.convert
            (function
                | Choice1Of4 x -> Listener x
                | Choice2Of4 x -> Procedure x
                | Choice3Of4 x -> Statements x
                | Choice4Of4(KnownValueComplexExpression x) -> Expression x
            )
            (function
                | Listener x -> Choice1Of4 x
                | Procedure x -> Choice2Of4 x
                | Statements x -> Choice3Of4 x
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
                    NonNull baseLayerMD5,
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
                    NonNull x.baseLayerMD5,
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
                    NonNull md5,
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
                    NonNull x.md5,
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
                    OptionMap NormalFloat.op_Explicit currentCostumeIndex,
                    objectDataExtension
                ) ->
                {
                    objName = objName
                    scripts = scripts
                    costumes = costumes
                    sounds = sounds
                    variables = variables
                    lists = lists
                    currentCostumeIndex = currentCostumeIndex
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
                Option.map NormalFloat x.currentCostumeIndex,
                x.ObjectDataExtension
                )
            )

let quickcheck test = qcheckWith (fun c -> { c with Arbitrary = typeof<Arbs> :: c.Arbitrary }) test
