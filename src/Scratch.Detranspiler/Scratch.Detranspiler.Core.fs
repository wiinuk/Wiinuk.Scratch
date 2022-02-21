module Scratch.Detranspiler.Core
open Typing
open Environments
open Scratch
open Scratch.Primitives
open Scratch.Primitives.Document.Constructors
open Scratch.Primitives.Document.Operators
open Scratch.Ast
open Scratch.AstDefinitions
open System.Text.RegularExpressions
module A = Scratch.Ast.ExpressionPatterns
module E = Quotations.Patterns


let reservedNames =
    [
        "abstract"; "and"; "as"; "assert"; "base"; "begin"; "class"; "default"; "delegate"; "do"; "done";
        "downcast"; "downto"; "elif"; "else"; "end"; "exception"; "extern"; "false"; "finally"; "for";
        "fun"; "function"; "global"; "if"; "in"; "inherit"; "inline"; "interface"; "internal"; "lazy"; "let";
        "match"; "member"; "module"; "mutable"; "namespace"; "new"; "null"; "of"; "open"; "or";
        "override"; "private"; "public"; "rec"; "return"; "sig"; "static"; "struct"; "then"; "to";
        "true"; "try"; "type"; "upcast"; "use"; "val"; "void"; "when"; "while"; "with"; "yield";
        
        "const"; "fixed"
    ]
    |> Seq.map (fun x -> x, ())
    |> Map.ofSeq

let simpleNameRegex =
    let cs = @"\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}_"
    Regex <| sprintf @"^[%s_][%s0-9\p{Pc}\p{Mn}\p{Mc}\p{Cf}']*$" cs cs

let exQuotedNameInnerRegex =
    // `\` をエスケープ文字として追加
    // @ は warning FS1104 が出るのでエスケープする
    let cs = @"[^`\n\r\t\\@]"
    Regex <| sprintf @"^(%s|`%s)+$" cs cs

let prettyName n =
    if simpleNameRegex.IsMatch n && not (Map.containsKey n reservedNames) then text n else
    if exQuotedNameInnerRegex.IsMatch n then "``" .+. n +. "``" else
    let n = Regex.Replace(n, @"[`\n\r\t\\@]", (fun m ->
        match m.Value with
        | "`" -> "\\u0060"
        | "@" -> "\\u0040"
        | "\n" -> "\\n"
        | "\r" -> "\\r"
        | "\t" -> "\\t"
        | "\\" -> "\\\\"
        | x -> x
    ))
    "``" .+. n +. "``"

let prettyLongNameRaw kind (LongName(ns, n)) =
    let path = sequence (Seq.map (fun n -> prettyName n +. ".") ns) ++ prettyName n
    match kind with
    | Global -> "global." .+ path
    | Local -> path

let invalidStringLiteralCharRegex = Regex @"[\n\t\r\b\a\f\v\\""]"
let prettyStringLiteral s =
    let s =
        if invalidStringLiteralCharRegex.IsMatch s then
            invalidStringLiteralCharRegex.Replace(s, (fun m ->
                match m.Value with
                | "\n" -> "\\n"
                | "\t" -> "\\t"
                | "\r" -> "\\r"
                | "\b" -> "\\b"
                | "\a" -> "\\a"
                | "\f" -> "\\f"
                | "\v" -> "\\v"
                | "\\" -> "\\\\"
                | "\"" -> "\\\""
                | x -> "\\" + x
            ))
        else
            s
    "\"" .+. s +. "\""

// TODO: nan, infinity, -infinity
let private invariantCulture = System.Globalization.CultureInfo.InvariantCulture
let prettyDoubleLiteral x =
    if double (int x) = x then
        match int x with
        | -1 -> text "-1.0"
        | 0 -> text "0.0"
        | 1 -> text "1.0"
        | 2 -> text "2.0"
        | 3 -> text "3.0"
        | 4 -> text "4.0"
        | 10 -> text "10.0"
        | _ -> text (x.ToString("F01", invariantCulture))
    else
        let t = (x + 0.).ToString("G17", invariantCulture)
        if 0 <= t.IndexOf '.' || 0 <= t.IndexOf 'E' then
            text t
        else
            text (x.ToString("F01", invariantCulture))

let prettyIntLiteral = function
    | -1 -> text "-1"
    | 0 -> text "0"
    | 1 -> text "1"
    | 2 -> text "2"
    | 3 -> text "3"
    | 4 -> text "4"
    | 10 -> text "10"
    | x -> text (string x)

let prettyBoolLiteral = function
    | true -> text "true"
    | _ -> text "false"

let prettyAttributePropertySet n v = prettyName n +. " =" ++ nest (ns ++ v)

let prettyTuple = function
    | [] -> empty
    | x::xs -> group x ++ sequence (Seq.map (fun x -> "," .+ ns ++ group x) xs)

let lookupKnownShortestId name = context {
    let! env = Context.environment
    match Map.tryFind (LongName.ofGlobalName name) env.specs.fsNamespace with
    | ValueSome(MemberSpec { alternativeNames = names }) ->
        return
            if Set.isEmpty names then struct(Global, LongName.ofGlobalName name)
            else struct(Local, LongName.ofLocalName (Set.minElement names))

    | ValueSome(OpenedMemberSpec _)
    | ValueSome(ValueSpec _)
    | ValueSome(ProcedureSpec _)
    | ValueSome(SpriteSpec _)
    | ValueNone -> return failwith $"internal error: EntityNotFound(%A{name})"
}
let prettyKnownGlobalName name = context {
    let! struct(k, id) = lookupKnownShortestId name
    return prettyLongNameRaw k id
}
let prettyKnownGlobalAttributeName name = context {
    let! struct(k, id) = lookupKnownShortestId name
    let! env = Context.environment
    let id =
        let (LongName(ns, n)) = id
        let attributeS = "Attribute"
        if n.EndsWith attributeS then
            let shortName = LongName(ns, n[0..n.Length-1-attributeS.Length])
            if Map.containsKey shortName env.specs.fsNamespace then id
            else shortName
        else id
    return prettyLongNameRaw k id
}

let inline localExpressionEnv prec atomicity = Context.local (fun env -> { expressionEnv = env; precedence = prec; atomicity = atomicity })
let inline inExpressionEnv c = Context.local (fun e -> e.expressionEnv) c
let prettyKnownEntity (entity: #Quotations.Expr) = context {
    let! env = Context.environment
    return! prettyKnownGlobalName (findName env.prettyEnv.resolveEnv entity)
}
let prettyKnownAttribute typeName args = context {
    let! name = prettyKnownGlobalAttributeName typeName
    let args =
        match args with
        | [] -> empty
        | props -> group ("(" .+ nest (ne ++ prettyTuple props) ++ ne +. ")")
    return "[<" .+ name ++ args +. ">]"
}
let add x xs = x::xs
let addOption x xs = match x with None -> xs | Some x -> x::xs
let prettyAppendIfNoDefault defaultValue pretty x xs =
    if x = defaultValue then xs
    else List.fold (fun xs x -> pretty x::xs) xs x
let prettyAddIfNoDefault defaultValue pretty x xs =
    if x = defaultValue then xs
    else pretty x::xs

let prettyAddAttributeProperty pretty (name, x) = add (prettyAttributePropertySet name (pretty x))
let prettyAddAttributeOptionalProperty pretty (name, x) = addOption (x |> Option.map (fun x -> prettyAttributePropertySet name (pretty x)))

let nameOf (_: 'T The) = context {
    let! env = Context.environment
    return nameOf<'T> env.prettyEnv.resolveEnv
}

let prettyCostumeAttribute ({ costumeName = costumeName; baseLayerMD5 = baseLayerMD5 } as x) = context {
    let props =
        []
        |> prettyAddAttributeProperty prettyStringLiteral ("CostumeName", costumeName)
        |> prettyAddAttributeProperty prettyStringLiteral ("BaseLayerMD5", baseLayerMD5)
        |> prettyAddAttributeProperty prettyDoubleLiteral ("BaseLayerID", x.baseLayerID)
        |> prettyAddAttributeOptionalProperty prettyStringLiteral ("TextLayerMD5", x.textLayerMD5)
        |> prettyAddAttributeOptionalProperty prettyDoubleLiteral ("TextLayerID", x.textLayerID)
        |> prettyAddAttributeOptionalProperty prettyDoubleLiteral ("BitmapResolution", x.bitmapResolution)
        |> prettyAddAttributeProperty prettyDoubleLiteral ("RotationCenterX", x.rotationCenterX)
        |> prettyAddAttributeProperty prettyDoubleLiteral ("RotationCenterY", x.rotationCenterY)
        |> List.rev

    let! name = nameOf the<CostumeAttribute>
    return! prettyKnownAttribute name props
}

let prettySoundRate x = SoundRate.toNumber x |> int |> prettyIntLiteral

let prettySoundFormat x = SoundFormat.toString x |> prettyStringLiteral

let prettySoundAttribute x = context {
    let props =
        []
        |> prettyAddAttributeProperty prettyStringLiteral ("SoundName", x.soundName)
        |> prettyAddAttributeProperty prettyDoubleLiteral ("SoundID", x.soundID)
        |> prettyAddAttributeProperty prettyStringLiteral ("Md5", x.md5)
        |> prettyAddAttributeOptionalProperty prettyDoubleLiteral ("SampleCount", x.sampleCount)
        |> prettyAddAttributeOptionalProperty prettySoundRate ("Rate", x.rate)
        |> prettyAddAttributeOptionalProperty prettySoundFormat ("Format", x.format)
        |> List.rev

    let! name = nameOf the<SoundAttribute>
    return! prettyKnownAttribute name props
}
let prettyCurrentCostumeIndexAttribute x = context {
    let props = [prettyDoubleLiteral x]
    let! name = nameOf the<CurrentCostumeIndexAttribute>
    return! prettyKnownAttribute name props
}
let prettyStageAttribute x = context {
    let props =
        []
        |> prettyAddAttributeOptionalProperty prettyStringLiteral ("PenLayerMD5", x.penLayerMD5)
        |> prettyAddAttributeOptionalProperty prettyDoubleLiteral ("PenLayerID", x.penLayerID)
        |> prettyAddAttributeProperty prettyDoubleLiteral ("TempoBPM", x.tempoBPM)
        |> prettyAddAttributeOptionalProperty prettyDoubleLiteral ("VideoAlpha", x.videoAlpha)
        |> List.rev

    let! name = nameOf the<StageAttribute>
    return! prettyKnownAttribute name props
}
let prettyRotationStyle x =
    match x with
    | RotationStyle.LeftRight -> "leftRight"
    | RotationStyle.None -> "none"
    | RotationStyle.Normal -> "normal"
    |> prettyStringLiteral

let prettyVisiblity x =
    match x with
    | Visible -> true
    | Hidden -> false
    |> prettyBoolLiteral

let prettySpriteAttribute x = context {
    let props =
        []
        |> prettyAddAttributeProperty prettyDoubleLiteral ("Direction", x.direction)
        |> prettyAddAttributeProperty prettyDoubleLiteral ("IndexInLibrary", x.indexInLibrary)
        |> prettyAddAttributeProperty prettyBoolLiteral ("IsDraggable", x.isDraggable)
        |> prettyAddAttributeProperty prettyRotationStyle ("RotationStyle", x.rotationStyle)
        |> prettyAddAttributeProperty prettyDoubleLiteral ("Scale", x.scale)
        |> prettyAddAttributeProperty prettyVisiblity ("Visible", x.visible)
        |> List.rev

    let! name = nameOf the<SpriteAttribute>
    return! prettyKnownAttribute name props
}
let prettyPositionAttribute (x, y) = context {
    let props =
        []
        |> prettyAddAttributeProperty prettyDoubleLiteral ("X", x)
        |> prettyAddAttributeProperty prettyDoubleLiteral ("Y", y)
        |> List.rev

    let! name = nameOf the<PositionAttribute>
    return! prettyKnownAttribute name props
}
let prettySValue env = function
    | SString x ->
        match env.config, x with
        | { treatStringAsNumberIfPossible = true }, TryParseSNumber x -> prettyDoubleLiteral x
        | { treatStringAsBoolIfPossible = true }, TryParseSBool x -> prettyBoolLiteral x
        | _ -> prettyStringLiteral x

    | SBool x -> prettyBoolLiteral x
    | SNumber x -> prettyDoubleLiteral x

let prettyAnnotationAttributes info =
    info
    |> Map.toSeq
    |> Seq.map (fun (k, v) -> context {
        let! { prettyEnv = env } = Context.environment
        let! name = nameOf the<AnnotationAttribute>
        return! prettyKnownAttribute name [prettyStringLiteral k; prettySValue env v]
    })

let prettyDefaultAttribute x = context {
    let! name = nameOf the<DefaultAttribute>
    let! env = Context.environment
    return! prettyKnownAttribute name [prettySValue env.prettyEnv x]
}
/// `newlines ["A"; "B"; "C"] = sequence ["A"; nl; "B"; nl; "C"; nl]`
let newlines xs =
    xs
    |> Seq.map (fun x -> x ++ nl)
    |> sequence

let prettyVariableOrListId location varOrListName = context {
    let! spec = lookupVariableOrList location varOrListName
    return prettyName spec.uniqueName
}
let prettyParameterId location paramName = context {
    let! spec = lookupParameter location paramName
    return prettyName spec.uniqueName
}
let prettyPersistentAttributeOrEmpty isPersistent = context {
    match isPersistent with
    | Persistent ->
        let! name = nameOf the<PersistentAttribute>
        let! a = prettyKnownAttribute name []
        return a ++ nl
    | NoPersistent ->
        return empty
}
// let mutable x = ...
let prettyLetVariable { state = struct(location, _); isPersistent = isPersistent; name = name; value = value } = context {
    let! attributes = prettyPersistentAttributeOrEmpty isPersistent
    let! name = prettyVariableOrListId location name
    let! env = Context.environment
    let value = prettySValue env.prettyEnv value
    return
        attributes +.
        "let mutable " ++ name +. " = " ++ value
}
let prettyLetList { state = struct(location, itemType); listName = name; isPersistent = isPersistent; contents' = contents } = context {
    let! attributes = prettyPersistentAttributeOrEmpty isPersistent
    let! name = prettyVariableOrListId location name
    let! defineList = prettyKnownEntity <@ defineList @>
    let! defineList = context {
        match itemType with
        | TypeScheme([], _) -> return defineList
        | TypeScheme _ ->
            let! name = nameOf the<System.String>
            let! string = prettyKnownGlobalName name
            return defineList +. "<" ++ string +. ">"
    }
    let! env = Context.environment
    let contents =
        if env.prettyEnv.config.ignoreInitialListValues then
            empty
        else
            let contents = contents |> IArray.map (prettySValue env.prettyEnv)
            if IArray.isEmpty contents then empty else

            let x = IArray.ref 0 contents
            let xs = Seq.truncate 1 <| IArray.toSeqCopiable contents
            x ++ sequence (Seq.map (fun x -> nsc ++ x) xs)

    return
        attributes ++
        group ("let " .+ name +. " = " ++ defineList +. " [" ++ nest (ne ++ contents) ++ ne +. "]")
}
let prettyProcedureParameters ps = context {
    match ps with
    | [] -> return text "()"
    | parameters ->

    let! ps =
        parameters
        |> Context.mapSeq (fun (ParameterDefinition(struct(l, _), n, v)) -> context {
            let v' =
                match v with
                | SBool _ -> SType.B
                | SNumber _ -> SType.N
                | SString _ -> SType.S
                |> SType.parameterDefaultValue

            let! attrs =
                []
                |> prettyAddIfNoDefault v' prettyDefaultAttribute v
                |> List.rev
                |> Context.mapList id

            let! n = prettyParameterId l n

            return
                match attrs with
                | [] -> ws ++ n
                | attrs -> "(" .+ sequence attrs ++ ws ++ n +. ")" ++ ws
        })
    return sequence ps
}
let inline localPrecedence prec = Context.local (fun env -> { env with precedence = prec })
let wrap x = "(" .+ nest (ne ++ x) ++ ne +. ")"

let startPlugin getPlugin xMapping error getLocation x = context {
    let! env = Context.environment
    return!
        getPlugin env.expressionEnv.prettyEnv.config.plugin x
        |> Context.mapError (function
            | Some e -> e
            | None ->
                let l = getLocation x |> ValueTuple.fst
                let x = xMapping (ValueTuple.fst >> env.expressionEnv.prettyEnv.config.showLocation) x |> error
                DetranspileError(l, x, null)
        )
}

let rec prettyExpressionAndPrecedence x = context {
    match x with
    | Literal(_, x) ->
        let! env = Context.environment
        return struct(prettySValue env.expressionEnv.prettyEnv x, Precedence.Primitive)
    | Complex e -> return! startPlugin (fun p -> p.expression) ComplexExpression.map NotImplementedExpression ComplexExpression.state e
    | Block x -> return! prettyBlock x
    }
and prettyExpression x = context {
    let! { precedence = prec } = Context.environment
    let! struct(x, prec') = prettyExpressionAndPrecedence x
    return if prec < prec' then wrap x else x
    }
and prettyOperands operands = Context.mapList prettyExpression operands
and prettyStatement s = context {
    let prec = Precedence.Sequence
    let! struct(x, prec') = localPrecedence prec (startPlugin (fun p -> p.statement) ComplexExpression.map NotImplementedStatement ComplexExpression.state s)
    return if prec < prec' then wrap x else x
    }
and prettyBlock (BlockExpression(_, ss)) = context {
    let! ss = Context.mapList (fun x -> context { let! x = prettyStatement x in return x }) ss
    match ss with
    | [] -> return struct(text "()", Precedence.Primitive)
    | ss -> return struct(Document.concat nl ss, Precedence.Sequence)
    }

// let print x y = atomic {
//     ...
// }
// let receiveMessage() = generator {
//     ...
// }
let prettyProcedure headKeyword (x, y, ProcedureDefinition(struct(location, _), name, parameters, isAtomic, body)) =
    Context.local (fun env -> { env with specs = registerParameters parameters env.specs }) <| context {
        let! attributes =
            []
            |> prettyAddIfNoDefault (0., 0.) prettyPositionAttribute (x, y)
            |> List.rev
            |> Context.mapSeq id
        let attributes =
            attributes
            |> newlines
            
        let! { uniqueName = name } = lookupProcedure location name
        let! parameters = prettyProcedureParameters parameters
        let! context =
            match isAtomic with
            | Atomic -> prettyKnownEntity <@ atomic @>
            | NoAtomic -> prettyKnownEntity <@ tightrope @>

        let! struct(body, _) = localExpressionEnv Precedence.Sequence isAtomic (prettyBlock body)
        return
            attributes ++
            headKeyword ++ prettyName name ++ parameters +. " = " ++ context +. " {" ++
            group (nest (nl ++ body) ++ nl) +.
            "}"
    }

let rec referencesStatement rs = function
    | ComplexExpression(operator = O.call; operands = A.EString(_, name)::_exprs) -> name::rs
    | ComplexExpression(operands = bs) -> List.fold referencesOperand rs bs

and referencesOperand rs = function
    | Block b -> referencesBlock rs b
    | _ -> rs

and referencesBlock rs (BlockExpression(_, ss)) =
    List.fold referencesStatement rs ss

let prettyProcedureBindGroup procedures = context {
    let ps = procedures |> Seq.rev |> Seq.toList
    match ps with
    | [] ->
        return []

    | [n,(p,rs)] ->
        let isSelfRec = List.contains n rs
        let! p = prettyProcedure (if isSelfRec then text "let rec " else text "let ") p
        return [p]

    | (_,(p,_))::ps ->
        let! p = prettyProcedure (text "let rec ") p
        let! ps = Context.mapList (snd >> fst >> prettyProcedure (text "and ")) ps
        return p::ps
}

let prettyProcedureBindGroups procedures =
    procedures
    |> Seq.map (fun (_, _, ProcedureDefinition(name = name; body = body) as p) ->
        let rs = referencesBlock [] body |> List.distinct |> List.rev
        name, (p, rs)
    )
    |> Graph.stronglyConnectedComponents
    |> Context.mapSeq prettyProcedureBindGroup

let prettyListener l = context {
    let! { precedence = prec } = Context.environment
    let! struct(l, prec') = startPlugin (fun p -> p.listener) ListenerDefinition.map NotImplementedListener ListenerDefinition.state l
    return if prec < prec' then wrap l else l
}
let prettyLazyExpression e = context {
    let! e = localPrecedence Precedence.Lazy <| prettyExpression (Complex e)
    return "lazy" .+ nest (ns ++ e)
}
let prettyLazyBlock b = context {
    let! struct(b, _) = localPrecedence Precedence.Lazy <| prettyBlock b
    return "lazy" .+ nest (ns ++ b)
}

let prettyKnownNewObject name args = context {
    let args =
        match args with
        | [] -> text "()"
        | props -> group ("(" .+ nest (ne ++ prettyTuple props) ++ ne +. ")")

    let! name = prettyKnownGlobalName name
    return name ++ args
}

// attributes (lazy [PositionAttribute(1.0, 2.0)]) (...)
let inline prettyWithPosition x y a = context {
    if x = 0. && y = 0. then return! a else

    let! attributesP = inExpressionEnv (prettyKnownEntity <@ Scratch.PseudoAttributes.attributes @>)
    let! posisionA = inExpressionEnv (context {
        let! name = nameOf the<PositionAttribute>
        return! prettyKnownNewObject name [prettyDoubleLiteral x; prettyDoubleLiteral y]
    })
    let! l = localPrecedence Precedence.Primitive a
    return attributesP +. " (lazy [" ++ posisionA +. "]) " ++ l
}

let prettyListenerWithPosition x y l = prettyWithPosition x y <| prettyListener l
let prettyLazyExpressionWithPosition x y e = prettyWithPosition x y <| prettyLazyExpression e
let prettyLazyStatementsWithPosition x y s = prettyWithPosition x y <| prettyLazyBlock s

let prettyEntityAttributes data data' =
    []
    |> prettyAppendIfNoDefault data'.costumes prettyCostumeAttribute data.costumes
    |> prettyAppendIfNoDefault data'.sounds prettySoundAttribute data.sounds
    |> prettyAddIfNoDefault data'.currentCostumeIndex prettyCurrentCostumeIndexAttribute data.currentCostumeIndex
    |> List.rev
    |> Context.mapSeq id

let prettyStageAttributes data = context {
    let data' = StageData.defaultValue
    let! entityAttributes = prettyEntityAttributes data data'

    let! stageAttributes =
        let edata' = { data'.ObjectDataExtension with children = [] }
        let edata = { data.ObjectDataExtension with StageDataExtension.children = [] }
        []
        |> prettyAddIfNoDefault edata' prettyStageAttribute (StageDataExtension.map ignore edata)
        |> (fun xs -> prettyAnnotationAttributes edata.info |> Seq.fold (fun xs x -> x::xs) xs)
        |> List.rev
        |> Context.mapSeq id

    let! name = nameOf the<ReflectedDefinitionAttribute>
    let! reflectedDefinitionAttribute = prettyKnownAttribute name []

    return
        Seq.concat [
            entityAttributes
            stageAttributes
            Seq.singleton reflectedDefinitionAttribute
        ]
}
let prettySpriteAttributes data = context {
    let data' = SpriteData.defaultValue
    let! entityAttributes = prettyEntityAttributes data data'

    let! spriteAttributes =
        let edata' = data'.ObjectDataExtension
        let edata = data.ObjectDataExtension
        []
        |> prettyAddIfNoDefault edata' prettySpriteAttribute edata
        |> prettyAddIfNoDefault (edata'.scratchX, edata'.scratchY) prettyPositionAttribute (edata.scratchX, edata.scratchY)
        |> (fun xs -> prettyAnnotationAttributes edata.spriteInfo |> Seq.fold (fun xs x -> x::xs) xs)
        |> List.rev
        |> Context.mapSeq id

    let! name = nameOf the<SealedAttribute>
    let! sealedAttribute = prettyKnownAttribute name []

    return Seq.concat [
        entityAttributes
        spriteAttributes
        Seq.singleton sealedAttribute
    ]
}
let prettyProcedures scripts = context {
    let! procedureBindGroups =
        scripts
        |> Seq.choose (function
            | { x = x; y = y; script = Procedure p } -> Some(x, y, p)
            | _ -> None
        )
        |> prettyProcedureBindGroups

    return
        procedureBindGroups
        |> Seq.concat
}
let prettyListenerAndExpressionAndStatements scripts = context {
    let! scripts =
        scripts
        |> Context.mapSeq (fun { x = x; y = y; script = s } -> context {
            match s with
            | Listener l ->
                let! x = localExpressionEnv Precedence.Sequence NoAtomic <| prettyListenerWithPosition x y l
                return Some x

            | Expression e ->
                let! x = localExpressionEnv Precedence.Sequence NoAtomic <| prettyLazyExpressionWithPosition x y e
                return Some x

            | Statements s ->
                let! x = localExpressionEnv Precedence.Sequence NoAtomic <| prettyLazyStatementsWithPosition x y s
                return Some x

            | Procedure _ -> return None
        })
    return scripts |> Seq.choose id
}
let prettyEntryPointBody scripts = context {
    let! scripts = prettyListenerAndExpressionAndStatements scripts
    let! startMainLoop = prettyKnownEntity <@ Control.startMainLoop @>
    let callStartMainLoop = startMainLoop +. "()"

    return
        Seq.append scripts [callStartMainLoop]
        |> Seq.toList
        |> function
            | [] -> text "()"
            | xs -> newlines xs
}
let registerStageSpecs data env =
    let specs = {
        fsNamespace = Map.empty
        variableOrListSpecs = Map.empty
        procedureSpecs = Map.empty
        parameterSpecs = Map.empty
        spriteSpecs = Map.empty
    }
    let specs =
        registerCoreGlobalNames env.resolveEnv specs
        |> registerGlobalNames env.config.plugin.globalNames

    let entryPoint, specs =
        specs
        |> registerVarOrList "entryPoint"

    let specs =
        specs
        |> registerDataSpecs data

    let specs =
        data
        |> StageData.sprites
        |> Seq.fold (fun specs s -> registerSpriteSpec s specs) specs
    {
        prettyEnv = env
        specs = specs
        self = None
        primTypes = TsType.types
        entryPoint = prettyName entryPoint.uniqueName
    }

// [<Costume(...)>]
// [<Costume(...)>]
// [<Sealed>]
// type %(data.objName) () as self =
//     inherit Sprite()
//
//     let mutable x = 0
//     let list = defineList()
//     let f1 x y = ...
//     let rec f2 x ... = ...
//     and f3 x ... = ...
//     do ...
//     do ...
let prettySprite data = context {
    let registerSpriteSpecs self data env =
        let spriteName, specs = env.specs |> registerVarOrList data.objName
        let self, specs = specs |> registerVarOrList self
        let specs = specs |> registerDataSpecs data

        let spriteName = prettyName spriteName.uniqueName
        let self = prettyName self.uniqueName
        let env =
            { env with
                specs = specs
                self = Some { selfName = self }
            }
        spriteName, self, env

    let! attributes = prettySpriteAttributes data
    let! env = Context.environment
    let spriteName, self, env = registerSpriteSpecs "self" data env

    let! spriteTypeName = nameOf the<Sprite>
    let! sprite = prettyKnownGlobalName spriteTypeName
    let spriteHeader =
        newlines attributes +.
        "type " ++ spriteName +. " () as " ++ self +. " =" ++ nl

    return! Context.local (fun _ -> env) (context {
        let! variables = data.variables |> Context.mapList prettyLetVariable
        let! lists = data.lists |> Context.mapList prettyLetList
        let! procedures = prettyProcedures data.scripts
        let! listeners = prettyListenerAndExpressionAndStatements data.scripts

        return
            spriteHeader ++ nest (nl +.
                "inherit " ++ sprite +. "()" ++ nl ++
                newlines variables ++
                newlines lists ++
                newlines procedures ++
                newlines listeners
            )
            ++ nl
    })
}

let prettyStageChild x =
    match x with
    | Choice2Of3 x -> prettySprite x

    | Choice1Of3 _ -> context.Return empty
    | Choice3Of3 _ -> context.Return empty

// [<Costume(CostumeName = ..., BaseLayerMD5 = ...)>]
// [<Costume(CostumeName = ..., BaseLayerMD5 = ...)>]
// module %(data.objName)
// open Scratch
// open ...
//
// let mutable x = 0
// let list = defineList()
// let f1 x y = ...
// let rec f2 x ... = ...
// and f3 x ... = ...
//
// let entryPoint() =
//     whenGreenFlag ...
//     whenIReceive ...
let prettyStage data = context {
    let! env = Context.environment
    let data = infer env data

    return! Context.local (registerStageSpecs data) <| context {

        let! attributes = prettyStageAttributes data

        let opens =
            coreOpenNames env.resolveEnv
            |> Seq.map (fun name -> "open " .+ prettyLongNameRaw Global (LongName.ofGlobalName name))

        let moduleHeader =
            newlines attributes +.
            "module " ++ prettyName data.objName ++ nl ++
            newlines opens ++ nl

        return! Context.local (fun env -> { env with specs = openNames (coreOpenNames env.prettyEnv.resolveEnv) env.specs }) (context {
            let! variables = data.variables |> Context.mapList prettyLetVariable
            let! lists = data.lists |> Context.mapList prettyLetList
            let! procedures = prettyProcedures data.scripts
            let! entryPointBody = prettyEntryPointBody data.scripts
            let! sprites = data.ObjectDataExtension.children |> Context.mapList prettyStageChild
            let! env = Context.environment

            return
                moduleHeader ++
                newlines variables ++
                newlines lists ++
                newlines procedures ++
                newlines sprites +.
                "let " ++ env.entryPoint +. "() =" ++ nest (ns ++ entryPointBody)
        })
    }
}
