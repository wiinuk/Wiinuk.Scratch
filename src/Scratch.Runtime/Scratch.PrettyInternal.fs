module internal Scratch.PrettyInternal
open System.Text.RegularExpressions
open Scratch
open Scratch.Ast
open Scratch.Primitives
open Scratch.Primitives.Document.Constructors
open Scratch.Primitives.Document.Operators

(*
@special = [""'@`\(\)\[\]\{\};,]
@quotedChar(@@q) = [^@@q\\] | \\[@@q\\]
*)
(*
@reservedName =
    | lazy | let | do | true | false | null
    | system
    | stage | sprite | watcher
    | <-

@simpleName =
    @@head = \w.:!#$%&=\-~|+*/<>?_
    @@tail = @@head \d'\@
    ( [@@head][@@tail]* ) - @reservedName
*)
(*
@quotedName = ` @quotedChar(`)* `
@name = @simpleName | @quotedName
*)
(*
@stringLiteral = " @quotedChar("")* "

@uint = [0-9]+
@int = [+-]? @uint
@numberLiteral = @int (\. @uint)? ([Ee] @int)?

@literal = true | false | @stringLiteral | @numberLiteral
*)
(*
literals = ( @literal (/,/ @literal)* /,/? )?

attribute = /@/ @name ( /\(/ @literals /\)/ )?
attributes = attribute*

variable =
    attributes
    /let/ @name /=/ @literal /;/

list =
    attributes
    /let/ @name /=/ /\[/ @literals /\]/ /;/
*)
(*
constantExpression =
    | @literal

    (?# readVariable | system operation )
    | @name

    | /\(/ expression /\)/

complexExpression =
    | /system/ @name constantExpression*

    (?# readVariable )
    | /stage|sprite/ @name

expression =
    | complexExpression
    | constantExpression


statement =
    | /system/? @name (blockLiteral | expression)* /;/
footerStatement = statement
statements = statement* footerStatement?
blockLiteral = /\{/ statements /\}/

parameter = @name | /\(/ @name /=/ @literal /\)/
procedure = /let/ @name parameter* /=/ blockLiteral

script =
    | procedure
    | /lazy/ blockLiteral
    | /lazy/ complexExpression /;/
    | /do/ complexExpression blockLiteral

item =
    | variable
    | list
    | attributes script

sprite = item*
*)

let reservedNameRegex = Regex @"^(lazy|let|do|true|false|null|system|stage|sprite|watcher|<-)$"
let simpleNameRegex =
    let head = @"\w.:!#$%&=\-~|+*/<>?_"
    let tail = sprintf @"%s\d'@" head
    Regex <| sprintf @"^[%s][%s]*$" head tail

let quotedNameEscapeRegex = Regex @"\\`"
let escape = MatchEvaluator(fun m -> @"\" + m.Value)

let prettyName n =
    if simpleNameRegex.IsMatch n && not (reservedNameRegex.IsMatch n) then
        text n
    else
        sequence [|
            text "`"
            text (quotedNameEscapeRegex.Replace(n, escape))
            text "`"
        |]

let stringLiteralEscapeRegex = Regex @"\\"""
let prettyStringLiteral = function
    | null -> text "<null>"
    | "" -> text "\"\""
    | x ->
        sequence [|
            text "\""
            text (stringLiteralEscapeRegex.Replace(x, escape))
            text "\""
        |]

let prettyBoolLiteral = function true -> text "true" | false -> text "false"
let prettyNumberLiteral (x: double) = text (x.ToString "G17")

let prettyLiteral = function
    | SBool x -> prettyBoolLiteral x
    | SNumber x -> prettyNumberLiteral x
    | SString x -> prettyStringLiteral x

let prettyAttribute name pretty x =
    text "@" ++ text name ++ group (text "(" ++ nest (ne ++ pretty x) + ne ++ text ")")

/// `None -> "@%name()" | Some v -> "@%name(%(pretty v))"`
let prettyOptionalAttribute name pretty = function
    | None -> text "@" ++ text name ++ text "()"
    | Some x -> prettyAttribute name pretty x

let prettyVariableData { isPersistent = p; name = n; value = v } =
    let p = match p with Persistent -> text "@persistent" ++ ns | NoPersistent -> empty
    group (p ++ text "let " ++ prettyName n ++ ns ++ text "=" ++ ns ++ prettyLiteral v)

let prettyVariable state v =
    state.plugins.prettyVariable(state.state, v)

/// `f xs[0] ++ sep ++ f xs[1] ++ ... ++ f xs[N]`
let concatMap f sep xs = Document.concat sep <| Seq.map f xs
/// `f xs[0] ++ ns ++ f xs[1] ++ ... ++ f ns[N]`
let concatNS f xs = concatMap f ns xs
/// `f xs[0] ++ nl ++ f xs[1] ++ ... ++ f ns[N]`
let concatNL f xs = concatMap f nl xs

let prettyListVariableData { isPersistent = isP; listName = n; contents' = vs; x = x; y = y; width = width; height = height; visible = visible } =
    let ps = empty
    let ps =
        if x <> 0. || y <> 0.
        then ps ++ text "@position(" ++ text (string x) ++ text ", " ++ text (string y) ++ text ")" ++ ns
        else ps

    let ps =
        match isP with
        | Persistent -> ps ++ text "@persistent" ++ ns
        | NoPersistent -> ps

    let ps =
        if width <> 0. || height <> 0.
        then ps ++ text "@size(" ++ text (string width) ++ text ", " ++ text (string height) ++ text ")" ++ ns
        else ps

    let ps =
        match visible with
        | Visible -> ps ++ text "@visible" ++ ns
        | Hidden -> ps

    group (
        group (ps ++ text "let " ++ prettyName n ++ ns ++ text "=" ++ ns ++ text "[") ++
        nest (ne ++
            group (concatNS prettyLiteral (IArray.toSeqCopiable vs))
        ) ++ ne ++
        text "]"
    )

let prettyList state l = state.plugins.prettyList(state.state, l)

let prettyParameter (ParameterDefinition(_, n, dv)) =
    let dvt =
        match dv with
        | SNumber _ -> SType.N
        | SBool _ -> SType.B
        | SString _ -> SType.S
    
    if SType.parameterDefaultValue dvt = dv then
        prettyName n
    else
        prettyAttribute "default" prettyLiteral dv ++ prettyName n

[<RequireQualifiedAccess>]
type Precedence =
    | Min

    /// e.g. `1` `"text"` `()` `name` `list[i]`
    | Primitive
    /// e.g. `call a b c`
    | Application
    /// e.g. `x * y`
    | Mul
    /// e.g. `x + y`
    | Add
    /// e.g. `x < y`
    | Rel
    /// e.g. `x = y`
    | Eq
    /// e.g. `x & y`
    | And
    /// e.g. `x | y`
    | Or
    /// e.g. `v <- x`
    | Assign

    | Max

let prettySymbol symbol = prettyName (Symbol.name symbol)

/// `opName` -or- `system.opName`
let prettyOperator state op =
    if Set.contains (Symbol.name op) state.procedures then
        struct(text "system" ++ ws ++ prettySymbol op, Precedence.Application)
    else
        prettySymbol op, Precedence.Primitive

let prettyVarOrListName state name =
    if Set.contains name state.parameters then
        let self =
            match state.scope with
            | None -> "stage"
            | Some p when Set.contains name p.listOrVariables -> "stage"
            | _ -> "sprite"

        struct(text self ++ ws ++ prettyName name, Precedence.Application)
    else
        prettyName name, Precedence.Primitive

let prettyListAccess name line =
    prettyName name ++ group (text "[" + nest (ne ++ line) ++ ne ++ text "]")

let prettyExpressionOrWrap maxPrec struct(e, exprPrec) =
    if maxPrec < exprPrec then
        group (text "(" ++ nest (ne ++ group e) ++ ne ++ text ")")
    else
        e

let rec prettyComplexExpression state maxPrec op ops =
    let eWithPrec =
        match op with
        | O.getParam -> prettyGetParam state op ops
        | O.readVariable -> prettyReadVariable state op ops
        | O.``%``
        | O.``*``
        | O.``/`` -> prettyBinaryOp state (Precedence.Mul, Precedence.Mul, Precedence.Application) op ops
        | O.``+``
        | O.``-`` -> prettyBinaryOp state (Precedence.Add, Precedence.Add, Precedence.Mul) op ops
        | O.``<``
        | O.``>`` -> prettyBinaryOp state (Precedence.Rel, Precedence.Rel, Precedence.Add) op ops
        | O.``=`` -> prettyBinaryOp state (Precedence.Eq, Precedence.Eq, Precedence.Rel) op ops
        | O.``&`` -> prettyBinaryOp state (Precedence.And, Precedence.And, Precedence.Eq) op ops
        | O.``|`` -> prettyBinaryOp state (Precedence.Or, Precedence.Or, Precedence.And) op ops
        | O.``getLine:ofList:`` -> prettyGetLine_OfList_ state op ops
        | _ -> prettyNormalComplexExpression state op ops

    prettyExpressionOrWrap maxPrec eWithPrec

and prettyExpressions state ops =
    concatNS (group << prettyExpression state Precedence.Primitive) ops

and prettyOperand state operand { AstDefinitions.operandType = operandType } =
    match operand, operandType with
    | Literal(_, SString name), (OperandType.ListVariableExpression _ | OperandType.Variable _) ->
        prettyExpressionOrWrap Precedence.Primitive (prettyVarOrListName state name)

    | _ -> prettyExpression state Precedence.Primitive operand

and prettyOperands state op ops =
    match ops with
    | [] -> text "()"
    | _ ->

    match Map.tryFind op AstDefinitions.knownAllOperatorMap with
    | ValueSome info when List.length ops = List.length info.operands ->
        concatNS group (Seq.map2 (prettyOperand state) ops info.operands)

    | _ -> prettyExpressions state ops

and prettyNormalComplexExpression state op ops =
    let ops = prettyOperands state op ops
    let struct(op, _) = prettyOperator state op
    op ++ nest (ns ++ ops), Precedence.Application

and prettyGetParam state op = function
    | [Literal(_, SString name); Literal(_, SString "r")] -> prettyName name, Precedence.Primitive
    | ops -> prettyNormalComplexExpression state op ops

and prettyReadVariable state op = function
    | [Literal(_, SString name)] -> prettyVarOrListName state name
    | ops -> prettyNormalComplexExpression state op ops

and prettyBinaryOp state (leftPrec, opPrec, rightPrec) op = function
    | [value1; value2] ->
        let v1 = prettyExpression state leftPrec value1
        let v2 = prettyExpression state rightPrec value2
        group v1 ++ group (nest (ns ++ prettySymbol op ++ ns ++ group v2)), opPrec

    | ops -> prettyNormalComplexExpression state op ops

and prettyGetLine_OfList_ state op = function
    | [line; Literal(_, SString name)] ->
        let line = prettyExpression state Precedence.Max line
        prettyListAccess name line, Precedence.Primitive

    | ops -> prettyNormalComplexExpression state op ops

and prettyExpression state prec = function
    | Literal(_, x) -> prettyLiteral x
    | Complex(ComplexExpression(_, n, xs)) -> prettyComplexExpression state prec n xs
    | Block x -> prettyBlockLiteral state x

and prettyBlockLiteral state x =
    text "\\{" ++ nest (ns ++ group (prettyBlockExpression state x)) ++ ns ++ text "}"

and prettyStatement state (ComplexExpression(_, op, ops)) =
    match op with
    | O.``setVar:to:`` -> prettySetVar_To_ state op ops
    | O.``setLine:ofList:to:`` -> prettySetLine_OfList_To_ state op ops
    | O.call -> prettyCall state op ops
    | _ -> prettyNormalStatement state op ops

and prettyNormalStatement state op ops =
    let struct(op', _) = prettyOperator state op
    let ops = prettyOperands state op ops
    group (op' ++ nest (ns ++ group ops))

and prettySetVar_To_ state op = function
    | [Literal(_, SString name); value] ->
        let struct(name, _) = prettyVarOrListName state name
        let value = prettyExpression state Precedence.Assign value
        group (name ++ nest (ns ++ text "<-")) ++ nest (ns ++ group value)

    | ops -> prettyNormalStatement state op ops

and prettySetLine_OfList_To_ state op = function
    | [line; Literal(_, SString name); value] ->
        let access = prettyListAccess name (prettyExpression state Precedence.Max line)
        let value = prettyExpression state Precedence.Assign value
        group (access ++ nest (ns ++ text "<-")) ++ nest (ns ++ group value)

    | ops -> prettyNormalStatement state op ops

and prettyCall state op = function
    | Literal(_, SString name)::args ->
        let name = prettyName name
        let args =
            match args with
            | [] -> text "()"
            | _ -> prettyExpressions state args

        name ++ nest (ns ++ args)

    | ops -> prettyNormalStatement state op ops

and prettyBlockExpression state (BlockExpression(_, ss)) =
    match ss with
    | [] -> text "()"
    | _ -> concatNL (group << prettyStatement state) ss

let prettyProcedure state (ProcedureDefinition(_, name, ps, isAtomic, body)) =
    let state = { state with parameters = ps |> Seq.map (fun (ParameterDefinition(name = n)) -> n) |> Set.ofSeq }

    let attrs = match isAtomic with NoAtomic -> text "@async" ++ ns | Atomic -> empty
    let name = prettyName name
    let parameters =
        match ps with
        | [] -> text "()"
        | _ -> concatMap prettyParameter ns ps

    let body = prettyBlockExpression state body
    group (attrs ++ text "let " ++ name ++ nest (ns ++ group (parameters ++ ns) ++ text "=")) ++ nest (ns ++ group body)

let prettyListener state (ListenerDefinition(_, n, xs, b)) =
    let struct(n, _) = prettyOperator state n
    let ps = prettyExpressions state xs
    let body = prettyBlockLiteral state b
    text "do" ++ group (nest (ns ++ n ++ ns ++ ps) ++ ns) ++ body

let prettyScriptAst state = function
    | Procedure p -> prettyProcedure state p
    | Statements x -> text "lazy" ++ ns ++ prettyBlockLiteral state x
    | Expression(ComplexExpression(_, n, xs)) -> text "lazy" ++ ns ++ prettyComplexExpression state Precedence.Max n xs
    | Listener l -> prettyListener state l

let prettyScript state script =
    state.plugins.prettyScript(state.state, script)

let prettyScriptData state { x = x; y = y; script = s } =
    let p = if x <> 0. || y <> 0. then text "@position(" ++ text (string x) ++ text ", " ++ text (string y) ++ text ")" ++ ns else empty
    p ++ prettyScript state s

let prettyAdd pretty x xs = pretty x::xs
let prettyAddIf ok pretty x xs = if ok then prettyAdd pretty x xs else xs
let prettyAddNoDefault x x' pretty xs = prettyAddIf (x <> x') pretty x xs
let prettyAddAttribute x x' name pretty xs = prettyAddNoDefault x x' (prettyAttribute name pretty) xs
let prettyAddOptional pretty x xs = match x with None -> xs | Some x -> prettyAdd pretty x xs

let prettyRevAppend pretty xs attrs = List.fold (fun attrs x -> pretty x::attrs) attrs xs

let prettyProperty name pretty x = text name ++ text " = " ++ pretty x
let prettyTuple = function [] -> empty | x::xs -> x ++ sequence (Seq.map (fun x -> text "," ++ ns ++ x) xs)

let prettyAddProperty name pretty x = prettyAdd (prettyProperty name pretty) x

let prettyCostumeDataProperties x =
    []
    |> prettyAddProperty "costumeName" prettyStringLiteral x.costumeName
    |> prettyAddProperty "baseLayerMD5" prettyStringLiteral x.baseLayerMD5
    |> prettyAddProperty "baseLayerID" prettyNumberLiteral x.baseLayerID
    |> prettyAddOptional (prettyProperty "textLayerMD5" prettyStringLiteral) x.textLayerMD5
    |> prettyAddOptional (prettyProperty "textLayerID" prettyNumberLiteral) x.textLayerID
    |> prettyAddOptional (prettyProperty "bitmapResolution" prettyNumberLiteral) x.bitmapResolution
    |> prettyAddProperty "rotationCenterX" prettyNumberLiteral x.rotationCenterX
    |> prettyAddProperty "rotationCenterY" prettyNumberLiteral x.rotationCenterY
    |> List.rev
    |> prettyTuple

let prettyCostumeData x = prettyAttribute "costumeData" prettyCostumeDataProperties x

let prettySoundRate s = SoundRate.toNumber s |> prettyNumberLiteral

let prettySoundFormat s = SoundFormat.toString s |> prettyStringLiteral

let prettySoundDataProperties x =
    []
    |> prettyAddProperty "soundName" prettyStringLiteral x.soundName
    |> prettyAddProperty "md5" prettyStringLiteral x.md5
    |> prettyAddProperty "soundID" prettyNumberLiteral x.soundID
    |> prettyAddOptional (prettyProperty "sampleCount" prettyNumberLiteral) x.sampleCount
    |> prettyAddOptional (prettyProperty "rate" prettySoundRate) x.rate
    |> prettyAddOptional (prettyProperty "format" prettySoundFormat) x.format
    |> List.rev
    |> prettyTuple

let prettySoundData x = prettyAttribute "soundData" prettySoundDataProperties x

let prettyObjectDatas state x =
    List.map (fun v -> prettyVariable state v) x.variables @
    List.map (fun l -> prettyList state l) x.lists @
    List.map (fun s -> prettyScriptData state s) x.scripts

let prettyRotationStyle = function
    | RotationStyle.LeftRight -> prettyStringLiteral "leftRight"
    | RotationStyle.None -> prettyStringLiteral "none"
    | RotationStyle.Normal -> prettyStringLiteral "normal"

let prettyInfo x =
    Map.toSeq x
    |> Seq.map (fun (k, v) -> prettyName k ++ text " = " ++ prettyLiteral v)
    |> Seq.toList
    |> prettyTuple

let prettyVisibility = function
    | Visible -> prettyBoolLiteral true
    | Hidden -> prettyBoolLiteral false

let procedureNames plugins state x =
    let xs = x.scripts |> Seq.map (fun s -> s.script)
    plugins.procedureNames(state, xs) |> Set.ofSeq

let variableOrListNames plugins state x =
    let ls = plugins.listNames(state, List.toSeq x.lists)
    let vs = plugins.variableNames(state, List.toSeq x.variables)
    Seq.append ls vs |> Set.ofSeq

let stageState plugins x =
    let state = plugins.makeState()
    {
        state = {
            scope = None
            parameters = Set.empty
            procedures = procedureNames plugins state x
            listOrVariables = variableOrListNames plugins state x
            pluginState = state
        }
        plugins = plugins
    }
let spriteState { plugins = plugins; state = parent } x =
    let state = parent.pluginState
    {
        state = {
            scope = Some parent
            parameters = Set.empty
            procedures = procedureNames plugins state x
            listOrVariables = parent.listOrVariables + variableOrListNames plugins state x
            pluginState = state
        }
        plugins = plugins
    }
let prettySpriteData state x =
    let state = spriteState state x
    let e = x.ObjectDataExtension
    let x' = SpriteData.defaultValue
    let e' = x'.ObjectDataExtension

    let attributes =
        []
        |> prettyRevAppend prettyCostumeData x.costumes
        |> prettyRevAppend prettySoundData x.sounds
        |> prettyAddNoDefault x.currentCostumeIndex x'.currentCostumeIndex (prettyOptionalAttribute "currentCostumeIndex" prettyNumberLiteral)
        |> prettyAddAttribute e.direction e'.direction "direction" prettyNumberLiteral
        |> prettyAddAttribute e.indexInLibrary e'.indexInLibrary "indexInLibrary" prettyNumberLiteral
        |> prettyAddAttribute e.isDraggable e'.isDraggable "isDraggable" prettyBoolLiteral
        |> prettyAddAttribute e.rotationStyle e'.rotationStyle "rotationStyle" prettyRotationStyle
        |> prettyAddAttribute e.scale e'.scale "scale" prettyNumberLiteral
        |> prettyAddIf (e.scratchX <> e'.scratchX || e.scratchY <> e'.scratchY) (prettyAttribute "position" (List.map prettyNumberLiteral >> prettyTuple)) [e.scratchX; e.scratchY]
        |> prettyAddAttribute e.spriteInfo e'.spriteInfo "spriteInfo" prettyInfo
        |> prettyAddAttribute e.visible e'.visible "visible" prettyVisibility
        |> List.rev

    let attributes =
        match attributes with
        | [] -> empty
        | x::xs -> x ++ sequence (Seq.map (fun x -> ns ++ group x) xs) ++ nl

    attributes
    ++ text "sprite " ++ prettyName x.objName ++ nl
    ++ (prettyObjectDatas state x |> concatNL id) ++ nl

let prettyOptionalNullableAttribute name pretty = function
    | None -> prettyAttribute name (fun () -> empty) ()
    | Some None -> prettyAttribute name (fun () -> text "null") ()
    | Some(Some x) -> prettyAttribute name pretty x

let prettyWatcherMode() = text "1"

let prettyWatcherData x =
    let attributes =
        []
        |> prettyAdd (prettyOptionalAttribute "color" prettyNumberLiteral) x.color
        |> prettyAdd (prettyOptionalNullableAttribute "isDiscrete" prettyBoolLiteral) x.isDiscrete
        |> prettyAdd (prettyOptionalAttribute "label" prettyStringLiteral) x.label
        |> prettyAdd (prettyOptionalAttribute "mode" prettyNumberLiteral) x.mode
        |> prettyAdd (prettyOptionalNullableAttribute "sliderMax" prettyNumberLiteral) x.sliderMax
        |> prettyAdd (prettyOptionalNullableAttribute "sliderMin" prettyNumberLiteral) x.sliderMin
        |> prettyAdd (prettyOptionalNullableAttribute "visible" prettyVisibility) x.visible
        |> prettyAdd (prettyOptionalNullableAttribute "x" prettyNumberLiteral) x.x
        |> prettyAdd (prettyOptionalNullableAttribute "y" prettyNumberLiteral) x.y

    let attributes =
        match attributes with
        | [] -> empty
        | x::xs -> x ++ sequence (Seq.map (fun x -> ns ++ group x) xs) ++ nl

    let name = match x.param with None -> text "null" | Some x -> prettyName x

    attributes ++
    text "watcher " ++ prettyName x.cmd ++ ws ++ name ++ prettyName x.target ++ nl

let prettyStageChildren state = function
    | Choice1Of3 x -> prettyWatcherData x
    | Choice2Of3 x -> prettySpriteData state x

    // children の list は Evaluator で認識されない
    | Choice3Of3 x -> prettyListVariableData x

let prettyStageData plugins x =
    let e = x.ObjectDataExtension
    let x' = StageData.defaultValue
    let e' = x'.ObjectDataExtension

    let attributes =
        []
        |> prettyRevAppend prettyCostumeData x.costumes
        |> prettyRevAppend prettySoundData x.sounds
        |> prettyAddNoDefault x.currentCostumeIndex x'.currentCostumeIndex (prettyOptionalAttribute "currentCostumeIndex" prettyNumberLiteral)
        |> prettyAddNoDefault e.penLayerID e'.penLayerID (prettyOptionalAttribute "penLayerID" prettyNumberLiteral)
        |> prettyAddNoDefault e.penLayerMD5 e'.penLayerMD5 (prettyOptionalAttribute "penLayerMD5" prettyStringLiteral)
        |> prettyAddNoDefault e.tempoBPM e'.tempoBPM (prettyOptionalAttribute "tempoBPM" prettyNumberLiteral)
        |> prettyAddAttribute e.info e'.info "info" prettyInfo
        |> List.rev

    let attributes =
        match attributes with
        | [] -> empty
        | x::xs -> x ++ sequence (Seq.map (fun x -> ns ++ group x) xs) ++ nl

    let state = stageState plugins x
    let stage =
        group attributes
        ++ text "stage " ++ prettyName x.objName ++ nl
        ++ (prettyObjectDatas state x |> concatNL id) ++ nl

    stage::List.map (prettyStageChildren state) e.children
    |> concatNL id
