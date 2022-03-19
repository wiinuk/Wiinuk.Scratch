module Scratch.Detranspiler.PredefinedPlugins
open Scratch.Detranspiler.Core
open Scratch
open Scratch.Primitives
open Scratch.Primitives.Document.Constructors
open Scratch.Primitives.Document.Operators
open Scratch.Reflection
open Scratch.Ast
module A = Scratch.Ast.ExpressionPatterns
module A = Scratch.Ast.Expressions


let inline prettyMethodName m =
    let m, _ = Member.findMethod m
    prettyName m.Name

let inline prettyVariableOrListId l varName = inExpressionEnv (prettyVariableOrListId l varName) |> Context.mapError Some
let inline prettyParameterId l paramName = inExpressionEnv (prettyParameterId l paramName) |> Context.mapError Some
let inline prettyExpression prec e = localPrecedence prec (prettyExpression e) |> Context.mapError Some
let inline prettyKnownEntity e = inExpressionEnv (prettyKnownEntity e)
let binaryOp (prec1, op, prec2) prec s = context {
    match s with
    | ComplexExpression(operands = [e1; e2]) ->
        let! e1 = prettyExpression prec1 e1
        let! e2 = prettyExpression prec2 e2
        return struct(e1 +. op ++ e2, prec)
    | _ -> return! skip()
}
let inline prettyBlock prec body = context {
    let! struct(x, _) = localPrecedence prec (prettyBlock body) |> Context.mapError Some
    return x
}
let inline prettyOperands prec operands = localPrecedence prec (prettyOperands operands) |> Context.mapError Some
let withPrec prec d = context {
    let! d = d
    return struct(d, prec)
}

let binary f = function
    | ComplexExpression(operands = [x1; x2]) -> f x1 x2
    | _ -> skip()

let binaryStringValue prec f = function
    | ComplexExpression(operands = [A.EString name; value]) -> withPrec prec (f name value)
    | _ -> skip()

let binaryValueString prec f = function
    | ComplexExpression(operands = [value; A.EString name]) -> withPrec prec (f value name)
    | _ -> skip()

let binaryValueBlock prec f = function
    | ComplexExpression(operands = [test; Block body]) -> withPrec prec (f test body)
    | _ -> skip()

let unary prec f = function
    | ComplexExpression(operands = [value]) -> withPrec prec (f value)
    | _ -> skip()

let unaryString prec f = function
    | ComplexExpression(operands = [A.EString name]) -> withPrec prec (f name)
    | _ -> skip()

let unaryBlock prec f = function
    | ComplexExpression(operands = [Block body]) -> withPrec prec (f body)
    | _ -> skip()

let nullary prec f = function
    | ComplexExpression(operands = []) -> withPrec prec f
    | _ -> skip()

let inline spriteOnly x ([<InlineIfLambda>] f) = context {
    let! env = Context.environment
    match env.expressionEnv.self with
    | None -> return! skip()
    | Some self -> return! f struct(self, x)
}
let prettySpritePropertyGet (spriteProperty: Quotations.Expr<Sprite -> 'a>) sprite = context {
    let p = Member.findProperty spriteProperty
    return sprite.selfName +. "." ++ prettyName p.Name
}
let prettySpriteMethodCall (spriteMethod: Quotations.Expr<Sprite -> 'a>) sprite operands = context {
    let args =
        match Seq.toList operands with
        | [] -> text "()"
        | arg::args -> "(" .+ nest (ne ++ arg ++ sequence (List.map (fun x -> "," .+ ns ++ x) args)) ++ ne +. ")"

    return sprite.selfName +. "." ++ prettyMethodName spriteMethod ++ args
}
let inline spriteMethodOnly spriteMethod x = spriteOnly x <| fun struct(sprite, ComplexExpression(operands = operands)) -> context {
    let! operands = prettyOperands Precedence.Lazy operands
    let! call = prettySpriteMethodCall spriteMethod sprite operands
    return struct(call, Precedence.Call)
}
let inline spriteCall1 x ([<InlineIfLambda>] f) = spriteOnly x <| function
    | struct(sprite, ComplexExpression(operands = [e])) ->
        withPrec Precedence.Call <| f struct(sprite, e)
    | _ -> skip()

let prettyApplications f xs = f ++ group (nest (sequence <| Seq.map (fun x -> ns ++ group x) xs))
let prettyExpressionApplications f es = context {
    let! es = Context.mapSeq (prettyExpression Precedence.Primitive) es
    return struct(prettyApplications f es, Precedence.Application)
}
let prettyIntExpression prec e = context {
    match e with
    | Literal(_, SNumber x) when double (int x) = x -> return prettyIntLiteral <| int x
    | _ ->
        let! int = prettyKnownEntity <@ Operators.int @>
        let! e = prettyExpression prec e
        return "(" .+ int ++ ws ++ e +. ")"
}

let knownCallExpressions() = [
    O.getParam, fun e -> context { // ([], ([O.Expression gString; O.Reporter], tValue))
        match e with
        | ComplexExpression(operands = [A.EString(struct(location, _), paramName); _]) ->
            let! p = prettyParameterId location paramName
            return struct(p, Precedence.Primitive)
        | _ -> return! skip()
    }
    //"costumeName", g0e0 gString
    //"sceneName", g0e0 gString
    O.readVariable, unaryString Precedence.Primitive <| fun struct(struct(l, _), varName) ->
        prettyVariableOrListId l varName

    //"contentsOfList:", g1 "T" (fun t -> [O.ListVariableExpression t], gString)
    O.``getLine:ofList:``, fun e -> context {
        match e with
        | ComplexExpression(operands = [nth; A.EString(struct(l, _), listName)]) ->
            let enum f list = context {
                let! f = prettyKnownEntity f
                return struct(f ++ ws ++ list, Precedence.Application)
            }
            let simple nth list = context {
                let! get = prettyKnownEntity <@@ SList.get @@>
                let! nth = prettyIntExpression Precedence.Primitive nth
                return struct(get ++ ws ++ list ++ ws ++ nth, Precedence.Application)
            }
            let! list = prettyVariableOrListId l listName
            match nth with
            | A.EString(_, e) ->
                match e with
                | "random" | "any" -> return! enum <@@ SList.getRandom @@> list
                | "last" -> return! enum <@@ SList.getLast @@> list
                | _ -> return! simple nth list
            | _ -> return! simple nth list
        | _ -> return! skip()
    }
    O.``concatenate:with:``, fun e -> context {
        match e with

        // ToString
        | ComplexExpression(operands = [Literal(_, SString ""); x] | [x; Literal(_, SString "")]) ->

            // <@ string %x @>
            let! string = prettyKnownEntity <@ Operators.string @>
            return! prettyExpressionApplications string [x]

        // +
        | e -> return! binaryOp (Precedence.Add, " + ", Precedence.Mul) Precedence.Add e
    }
    //"letter:of:", g0e2 gString gNumber gString 
    //"answer", g0e0 gString
    //"getAttribute:of:", g0e2 tAttributeName gString tValue
    //"getUserId", g0e0 gNumber
    //"getUserName", g0e0 gString
    ]

let rem = fun e -> binaryOp (Precedence.Mul, " % ", Precedence.Pown) Precedence.Mul e
let knownNumberExpressions() = [
    //"xpos", g0e0 gNumber
    //"ypos", g0e0 gNumber
    //"heading", g0e0 gNumber
    //"costumeIndex", g0e0 gNumber
    //"backgroundIndex", g0e0 gNumber
    //"scale", g0e0 gNumber
    //"volume", g0e0 gNumber
    //"tempo", g0e0 gNumber
    O.``lineCountOfList:``, unaryString Precedence.Application <| fun struct(struct(l, _), listName) -> context {
        let! length = prettyKnownEntity <@ SList.length @>
        let! list = prettyVariableOrListId l listName
        let! double = prettyKnownEntity <@ ExtraTopLevelOperators.double @>
        return prettyApplications double ["(" .+ prettyApplications length [list] +. ")"]
    }
    O.``+``, fun e -> context {
        match e with

        // ToNumber
        | ComplexExpression(operands = [Literal(_, SNumber 0.); x] | [x; Literal(_, SNumber 0.)]) ->
            let! double = prettyKnownEntity <@ ExtraTopLevelOperators.double @>
            return! prettyExpressionApplications double [x]

        // +
        | e -> return! binaryOp (Precedence.Add, " + ", Precedence.Mul) Precedence.Add e
    }
    O.``-``, binaryOp (Precedence.Add, " - ", Precedence.Mul) Precedence.Add
    O.``*``, binaryOp (Precedence.Mul, " * ", Precedence.Pown) Precedence.Mul
    O.``/``, binaryOp (Precedence.Mul, " / ", Precedence.Pown) Precedence.Mul
    O.``randomFrom:to:``, binary <| fun x1 x2 -> context {
        let! randomFromTo =  prettyKnownEntity <@ randomFromTo @>
        return! prettyExpressionApplications randomFromTo [x1; x2]
    }
        // g0e2 gNumber gNumber gNumber
    //"abs", g0e1 gNumber gNumber
    //"sqrt", g0e1 gNumber gNumber
    //"stringLength:", g0e1 gString gNumber
    O.``%``, rem
    O.``\\``, rem
    O.rounded, unary Precedence.Application <| fun value -> context {
        let! round = prettyKnownEntity <@ Operators.round @>
        let! value = prettyExpression Precedence.Primitive value
        return prettyApplications round [value]
    }
    O.``computeFunction:of:``, binaryStringValue Precedence.Application <| fun struct(_, mathFunctionName) value -> context {
        let! func =
            match mathFunctionName with
            | "abs" -> prettyKnownEntity <@ Operators.abs @>
            | "floor" -> prettyKnownEntity <@ Operators.floor @>
            | "sqrt" -> prettyKnownEntity <@ Operators.sqrt @>
            | "ceiling" -> prettyKnownEntity <@ Operators.ceil @>
            | "cos"
            | "sin"
            | "tan"
            | "asin"
            | "acos"
            | "atan" -> skip()
            | "ln" -> prettyKnownEntity <@ Operators.log @>
            | "log" -> prettyKnownEntity <@ Operators.log10 @>
            | "e ^"
            | "10 ^" -> skip()
            | _ -> skip()

        let! value = prettyExpression Precedence.Primitive value
        return prettyApplications func [value]
    }

    //"mouseX", g0e0 gNumber
    //"mouseY", g0e0 gNumber
    O.timer, nullary Precedence.Primitive <| prettyKnownEntity <@ PrimitiveOperations.timer @>
    //"distanceTo:", g0e1 (gString .|. gStringL "_mouse_") gNumber
    //"timestamp", g0e0 gNumber
    //"timeAndDate", g0e1 tTimeAndDateFormat gNumber
    ]

let knownBooleanExpressions() = [
    //"list:contains:", g1 "T" (fun t -> [O.ListVariableExpression t; O.Expression t], gBoolean)
    O.``<``, binaryOp (Precedence.Relation, " < ", Precedence.Add) Precedence.VarSet
    O.``>``, binaryOp (Precedence.Relation, " > ", Precedence.Add) Precedence.VarSet
    O.``=``, binaryOp (Precedence.Relation, " = ", Precedence.Add) Precedence.VarSet
    O.``&``, binaryOp (Precedence.Relation, " && ", Precedence.Add) Precedence.VarSet
    O.``|``, binaryOp (Precedence.Relation, " || ", Precedence.Add) Precedence.VarSet
    O.not, unary Precedence.Application <| fun x1 -> context {
        // <@ not %x1 @>
        let! not = prettyKnownEntity <@ Operators.not @>
        let! x1 = prettyExpression Precedence.Primitive x1
        return prettyApplications not [x1]
    }
    O.mousePressed, nullary Precedence.Call <| prettyKnownEntity <@ SensingOperations.mousePressed @>
    //"touching:", g0e1 (gString .|. G.StringSs ["_mouse_"; "_edge_"]) gBoolean
    //"touchingColor:", g0e1 tRgb gBoolean
    //"color:sees:", g0e2 tRgb tRgb gBoolean
    O.``keyPressed:``, unary Precedence.Application <| fun x1 -> context {
        let struct(_, TypeScheme(_, t)) = Expression.state x1
        let! env = Context.environment
        let! x1 = prettyExpression Precedence.Primitive x1
        let! keyPressed =
            if t = env.expressionEnv.primTypes.tNumber then prettyKnownEntity <@ SensingOperations.keyPressed @>
            else prettyKnownEntity <@ SensingOperations.keyPressedFrom @>
        return prettyApplications keyPressed [x1]
    }
    ]

[<AbstractClass; Sealed>]
type private KnownExpressionsMapHolder<'a> private () =
    static let value: Map<_, PluginProcess<'a,_>> =
        [
            knownCallExpressions()
            knownBooleanExpressions()
            knownNumberExpressions()
        ]
        |> Seq.concat
        |> Map.ofSeq

    static member Value = value
[<GeneralizableValue>]
let knownExpressionsMap<'a> = KnownExpressionsMapHolder<'a>.Value

let expressionPlugin (ComplexExpression(_, operator, _) as e) =
    match Map.tryFind operator knownExpressionsMap with
    | ValueNone -> skip()
    | ValueSome p -> p e

let knownStatements() = [
    // module A
    // let f1 a b = atomic { () }
    // let f2() = atomic { do! awaitAtomic(f1 a b) }
    // 
    // ...
    // type S(...) =
    //     inherit Sprite(...)
    //     let f1 a b = atomic { () }
    //     let f2() = atomic { do! awaitAtomic(f2 a b) }
    O.call, fun s -> context {
        match s with
        | ComplexExpression(operands = A.EString(struct(location, _), name)::args) ->
            // do! awaitAtomic (%name %args...)
            // do! %name %args...

            let! { uniqueName = name; procedure = ProcedureDefinition(isAtomic = atomicity) } = inExpressionEnv (lookupProcedure location name) |> Context.mapError Some
            
            let name = prettyName name
            let! args = prettyOperands Precedence.Primitive args
            let call =
                match args with
                | [] -> name +. "()"
                | args -> prettyApplications name args

            match atomicity with
            | Atomic ->
                let! awaitAtomic = prettyKnownEntity <@ awaitAtomic @>
                return struct("do! " .+ awaitAtomic +. " (" ++ call +. ")", Precedence.Do)

            | NoAtomic ->
                return struct("do! " .+ call, Precedence.Do)

        | _ -> return! skip()
    }
    // "forward:", None, spriteMethod <@ fun s -> s.Forward @>
    // "turnRight:", None, spriteMethod <@ fun s -> s.TurnRight @>
    // "turnLeft:", None, spriteMethod <@ fun s -> s.TurnLeft @>

    // ...
    // type S(...) as self =
    //     inherit Sprite(...)
    //     let f() = self.Heading(...)
    O.``heading:``, spriteMethodOnly <@ fun s -> s.Heading @>
    // "pointTowards:", None, spriteMethod <@ fun s -> s.PointTowards @>
    O.``gotoX:y:``, spriteMethodOnly <@ fun s -> s.GotoXY @>
    // "gotoSpriteOrMouse:", None, spriteMethod <@ fun s -> s.GotoSpriteOrMouse @>
    // "changeXposBy:", None, spriteMethod <@ fun s -> s.ChangeXPosBy @>
    // "xpos:", None, spriteProperty <@ fun s -> s.X @>
    // "changeYposBy:", None, spriteMethod <@ fun s -> s.ChangeYPosBy @>
    // "ypos:", None, spriteProperty <@ fun s -> s.Y @>
    // "bounceOffEdge", None, spriteMethod <@ fun s -> s.BounceOffEdge @>
    // "setRotationStyle", None, spriteMethod <@ fun s -> s.SetRotationStyle @>
    O.``lookLike:``, fun s -> spriteCall1 s <| fun struct(self, e) -> context {
        let struct(_, TypeScheme(_, t)) = Expression.state e
        let! env = Context.environment
        let! e = prettyExpression Precedence.Lazy e

        // TODO:
        if t = env.expressionEnv.primTypes.tNumber then
            return! prettySpriteMethodCall <@ fun s -> s.LookLike @> self [e]
        else
            return! prettySpriteMethodCall <@ fun s -> s.LookLikeFrom @> self [e]
    }
    // "nextCostume", stageMethod <@ nextCostume @>, spriteMethod <@ fun s -> s.NextCostume @>
    // "showBackground:", stageMethod <@ startScene @>, spriteMethod <@ fun s -> s.StartScene @>
    // "startScene", stageMethod <@ startScene @>, spriteMethod <@ fun s -> s.StartScene @>
    // "nextBackground", stageMethod <@ nextScene @>, spriteMethod <@ fun s -> s.NextScene @>
    // "nextScene", stageMethod <@ nextScene @>, spriteMethod <@ fun s -> s.NextScene @>
    // "startSceneAndWait", stageMethod <@ startSceneAndWait @>, spriteMethod <@ fun s -> s.StartSceneAndWait @>
    // "say:duration:elapsed:from:", None, spriteMethod <@ fun s -> s.SayDurationElapsedFrom @>
    // "say:", None, spriteMethod <@ fun s -> s.Say @>
    // "think:duration:elapsed:from:", None, spriteMethod <@ fun s -> s.ThinkDurationElapsedFrom @>
    // "think:", None, spriteMethod <@ fun s -> s.Think @>
    // "changeGraphicEffect:by:", stageMethod <@ changeGraphicEffectBy @>, spriteMethods [T.StringSs["color"], <@ fun s -> s.ChangeColorEffect @>]
    //"setGraphicEffect:to:", g0s2 tFilterName gNumber
    //"filterReset", g0s0
    //"changeSizeBy:", g0s1 gNumber
    //"setSizeTo:", g0s1 gNumber
    //"show", g0s0
    //"hide", g0s0
    //"comeToFront", g0s0
    //"goBackByLayers:", g0s1 gNumber
    O.``playSound:``, unaryString Precedence.Call <| fun struct(_, soundName) -> context {
        let! env = Context.environment
        let name = prettyStringLiteral soundName
        match env.expressionEnv.self with
        | None ->
            let! playSound = prettyKnownEntity <@ SoundOperations.playSound @>
            return prettyApplications playSound [name]

        | Some self ->
            return! prettySpriteMethodCall <@ fun s -> s.PlaySound @> self [name]
    }
    //"doPlaySoundAndWait", g0s1 gString
    //"stopAllSounds", g0s0
    //"playDrum", g0s2 gNumber gNumber
    //"rest:elapsed:from:", g0s1 gNumber
    //"noteOn:duration:elapsed:from:", g0s2 gNumber gNumber
    //"instrument:", g0s1 gNumber
    //"changeVolumeBy:", g0s1 gNumber
    //"setVolumeTo:", g0s1 gNumber
    //"changeTempoBy:", g0s1 gNumber
    //"setTempoTo:", g0s1 gNumber
    O.clearPenTrails, nullary Precedence.Application <| context {
        let! clearPenTrails = prettyKnownEntity <@ PenOperations.clearPenTrails @>
        return clearPenTrails +. "()"
    }
    //"putPenDown", g0s0
    //"putPenUp", g0s0
    //"penColor:", g0s1 tRgb
    //"setPenHueTo:", g0s1 gNumber
    //"changePenHueBy:", g0s1 gNumber
    //"setPenShadeTo:", g0s1 gNumber
    //"changePenShadeBy:", g0s1 gNumber
    //"penSize:", g0s1 gNumber
    //"changePenSizeBy:", g0s1 gNumber
    //"stampCostume", g0s0

    O.``setVar:to:``, binaryStringValue Precedence.VarSet <| fun struct(struct(l, _), varName) value -> context {
        let! var = prettyVariableOrListId l varName
        let! value = prettyExpression Precedence.Lazy value
        return var +. " <- " ++ value
    }
    O.``changeVar:by:``, binaryStringValue Precedence.VarSet <| fun struct(struct(l, _), varName) value -> context {
        // <@ var <- var + %value @>
        let! var = prettyVariableOrListId l varName
        let! value = prettyExpression Precedence.Mul value
        return var +. " <- " ++ var +. " + " ++ value
    }
    O.``append:toList:``, binaryValueString Precedence.Application <| fun value struct(struct(l, _), listName) -> context {
        // <@ SList.push %listName %value @>
        let! push = prettyKnownEntity <@ SList.push @>
        let! list = prettyVariableOrListId l listName
        let! value = prettyExpression Precedence.Primitive value
        return prettyApplications push [list; value]
    }
    O.``deleteLine:ofList:``, binaryValueString Precedence.Application <| fun nth struct(struct(l, _), listName) -> context {
        let simple nth list = context {
            // <@ SList.remove %listName %nth @>
            let! remove = prettyKnownEntity <@ SList.remove @>
            let! nth = prettyIntExpression Precedence.Primitive nth
            return prettyApplications remove [list; nth]
        }
        let enum f list = context {
            let! f = prettyKnownEntity f
            return prettyApplications f [list]
        }
        let! list = prettyVariableOrListId l listName
        match nth with
        | A.EString(_, e) ->
            match e with
            | "all" -> return! enum <@ SList.removeAll @> list
            | "random" | "any" -> return! enum <@ SList.removeRandom @> list
            | "last" -> return! enum <@ SList.removeLast @> list
            | _ -> return! simple nth list
        | _ -> return! simple nth list
    }
    //"insert:at:ofList:", g1 "T" (fun t -> [O.ListVariableExpression t; O.Expression(gNumber .|. G.StringSs ["random"; "last"]); O.Expression t], gUnit)
    O.``setLine:ofList:to:``, fun x -> context {
        // g1 "T" (fun t -> [O.ListVariableExpression t; O.Expression(gNumber .|. G.StringSs ["random"; "last"]); O.Expression t], gUnit)
        match x with
        | ComplexExpression(operands = [nth; A.EString(struct(l, _), listName); value]) ->
            let enum f list value = context {
                let! f = prettyKnownEntity f
                return struct(prettyApplications f [list; value], Precedence.Application)
            }
            let simple nth list value = context {
                let! set = prettyKnownEntity <@ SList.set @>
                let! nth = prettyIntExpression Precedence.Primitive nth
                return struct(prettyApplications set [list; nth; value], Precedence.Application)
            }
            let! list = prettyVariableOrListId l listName
            let! value = prettyExpression Precedence.Primitive value
            match nth with
            | A.EString(_, e) ->
                match e with
                | "random" | "any" -> return! enum <@@ SList.setRandom @@> list value
                | "last" -> return! enum <@@ SList.setLast @@> list value
                | _ -> return! simple nth list value
            | _ -> return! simple nth list value
        | _ -> return! skip()
    }
    O.``showVariable:``, unaryString Precedence.Application <| fun struct(struct(l, _), varName) -> context {
        let! show = prettyKnownEntity <@ PrimitiveOperations.showVariable @>
        let! var = prettyVariableOrListId l varName
        return prettyApplications show [var]
    }
    O.``hideVariable:``, unaryString Precedence.Application <| fun struct(struct(l, _), varName) -> context {
        let! hide = prettyKnownEntity <@ PrimitiveOperations.hideVariable @>
        let! var = prettyVariableOrListId l varName
        return prettyApplications hide [var]
    }
    O.``broadcast:``, unary Precedence.Application <| fun name -> context {
        let! env = Context.environment
        match env.expressionEnv.self with
        | None ->
            // <@ broadcast %name @>
            let! broadcast = prettyKnownEntity <@ Control.broadcast @>
            let! name = prettyExpression Precedence.Primitive name
            return prettyApplications broadcast [name]

        | Some self ->
            // <@ do self.Broadcast %name @>
            let! name = prettyExpression Precedence.Lazy name
            return! prettySpriteMethodCall <@ fun s -> s.Broadcast @> self [name]
    }
    //"doBroadcastAndWait", g0s1 gString
    //"doForeverIf", g0 (ebs gBoolean)
    O.doIf, binaryValueBlock Precedence.If <| fun test ifTrue -> context {
        // if %test then %ifTrue
        let! test = prettyExpression Precedence.Expression test
        let! ifTrue = prettyBlock Precedence.Sequence ifTrue
        return "if" .+ group (nest (ns ++ test) ++ ns) +. "then" ++ group (nest (ns ++ group ifTrue))
    }
    O.doIfElse, fun x -> context {
        match x with
        | ComplexExpression(operands = [test; Block ifTrue; Block ifFalse]) ->
            let! test = prettyExpression Precedence.Expression test
            let! ifTrue = prettyBlock Precedence.Sequence ifTrue
            let! ifFalse = prettyBlock Precedence.Sequence ifFalse
            let d = "if" .+ group (nest (ns ++ test) ++ ns) +. "then" ++ group (nest (ns ++ group ifTrue) ++ ns +. "else" ++ nest (ns ++ group ifFalse))
            return struct(d, Precedence.If)
        | _ -> return! skip()
    }
    O.doRepeat, binaryValueBlock Precedence.Do <| fun count body -> context {
        // do! repeatAsync (int %count) { %body }
        let! repeatAsync = prettyKnownEntity <@ PrimitiveOperations.repeatAsync @>
        let! count = prettyIntExpression Precedence.Primitive count
        let! body = prettyBlock Precedence.Sequence body
        return "do! " .+ repeatAsync ++ group (nest (ns ++ count) ++ ns) +. " {" ++ nest (ns ++ body) ++ ns +. "}"
    }
    //"doReturn", g0s0
    O.doUntil, binaryValueBlock Precedence.Do <| fun test body -> context {
        // do! repeatUntilAsync (fun () -> %test) { %body }
        let! repeatUntilAsync = prettyKnownEntity <@ PrimitiveOperations.repeatUntilAsync @>
        let! test = prettyExpression Precedence.Do test
        let! body = prettyBlock Precedence.Sequence body
        return "do! " .+ repeatUntilAsync +. " (fun () -> " ++ test +. ") {" ++ nest (nl ++ body) ++ nl +. "}"
    }
    //"doWhile", g0 (ebs gBoolean)
    O.doWaitUntil, unary Precedence.Do <| fun test -> context {
        // do! waitUntilAsync (fun () -> %test)
        let! waitUntilAsync = prettyKnownEntity <@ PrimitiveOperations.waitUntilAsync @>
        let! test = prettyExpression Precedence.Do test
        return "do! " .+ waitUntilAsync +. " (fun () -> " ++ test +. ")"
    }
    //"glideSecs:toX:y:elapsed:from:", g0 (s3 gNumber gNumber gNumber)
    //"stopScripts", g0 ([OperandType.Stop], gUnit)
    O.``wait:elapsed:from:``, unary Precedence.Do <| fun seconds -> context {
        // do! waitElapsedFrom %seconds
        let! waitElapsedFrom = prettyKnownEntity <@ Control.waitElapsedFrom @>
        let! seconds = prettyExpression Precedence.Primitive seconds
        return "do! " .+ waitElapsedFrom +. " " ++ seconds
    }
    //"warpSpeed", g0 ([OperandType.Block], gUnit)

    // g0s1 (gString .|. gStringL "_myself_")
    O.createCloneOf, unary Precedence.Call <| fun enum -> context {
        let! env = Context.environment
        // spriteMethodOnly <@ fun s -> s.Heading @>
        match env.expressionEnv.self, enum with
        | Some sprite, Literal(_, SString "_myself_") -> return! prettySpriteMethodCall <@ fun s -> s.Clone() @> sprite []
        | Some sprite, Literal(struct(location, _), SString spriteName) ->
            let! { uniqueName = name } = inExpressionEnv (lookupSprite location spriteName) |> Context.mapError Some
            return! prettySpriteMethodCall <@ fun s -> s.CloneSprite("") @> sprite [prettyName name]
        | _ -> return! skip()
    }

    //"doAsk", g0s1 gString
    //"timerReset", g0s0
]
// type KnownFooterStatement<M> =
//     | Case1<"doForever", M>
//     | Case0<"stopAll">
//     | Case1<"stopScripts", "all" | "this script">
//     | Case0<"deleteClone">
let knownFooterStatements() = [
    O.doForever, unaryBlock Precedence.Do <| fun body -> context {
        // <@
        // do! foreverAsync (%context {
        //     %body
        // })
        // @>
        let! foreverAsync = prettyKnownEntity <@ PrimitiveOperations.foreverAsync @>
        let! env = Context.environment
        let! context =
            match env.atomicity with
            | Atomic -> prettyKnownEntity <@ AtomicGeneratorBuilderOperations.atomic @>
            | NoAtomic -> prettyKnownEntity <@ GeneratorBuilderOperations.tightrope @>

        let! body = prettyBlock Precedence.Sequence body
        return "do! " .+ foreverAsync +. "(" ++ context +. " {" ++ group (nest (ns ++ body) ++ ns) +. "})"
    }
    //"stopAll", g0s0
    O.stopScripts, unaryString Precedence.Application <| fun struct(_, enum) -> context {
        match enum with
        | "all" ->
            let! stopAllScripts = prettyKnownEntity <@ PrimitiveOperations.stopAllScripts @>
            return stopAllScripts +. "()"

        | "this script" ->
            // do! stopThisScript()
            let! stopThisScript = prettyKnownEntity <@ PrimitiveOperations.stopThisScript @>
            return "do! " .+ stopThisScript +. "()"

        | _ -> return! skip()
    }
    //"deleteClone", g0s0
    ]

[<AbstractClass; Sealed>]
type private KnownStatementMapHolder<'a> private () =
    static let value: Map<_, PluginProcess<'a,_>> =
        [
            knownStatements()
            knownFooterStatements()
        ]
        |> Seq.concat
        |> Map.ofSeq

    static member Value = value
[<GeneralizableValue>]
let knownStatementMap<'a> = KnownStatementMapHolder<'a>.Value

let statementPlugin (ComplexExpression(operator = operator) as s) =
    match Map.tryFind operator knownStatementMap with
    | ValueNone -> skip()
    | ValueSome f -> f s

let knownListeners = [
    //"whenClicked", []
    O.whenGreenFlag, fun l -> context {
        match l with
        | ListenerDefinition(_, _, [], body) ->
            let! env = Context.environment
            match env.expressionEnv.self with
            | None ->
                // whenGreenFlag {
                //     %body
                // }

                let! whenGreenFlag = prettyKnownEntity <@ whenGreenFlag @>
                let! body = prettyBlock Precedence.Sequence body
                let d = whenGreenFlag +. " {" ++ group (nest (nl ++ body) ++ nl) +. "}"
                return struct(d, Precedence.Application)

            | Some self ->
                // do self.WhenGreenFlag {
                //     %body
                // }

                let! whenGreenFlag = prettySpritePropertyGet <@ fun s -> s.WhenGreenFlag @> self
                let! body = prettyBlock Precedence.Sequence body
                let d =
                    "do " .+ self.selfName ++ whenGreenFlag +. " {" ++
                    group (nest (nl ++ body) ++ nl) +.
                    "}"

                return struct(d, Precedence.Call)
        | _ -> return! skip()
    }
    //"whenCloned", []
    O.whenIReceive, fun l -> context {
        match l with
        | ListenerDefinition(_, _, [Literal(_, SString name)], body) ->
            let! env = Context.environment
            match env.expressionEnv.self with
            | None ->
                // whenIReceive %name {
                //     %body
                // }

                let! whenIReceive = prettyKnownEntity <@ whenIReceive @>
                let! body = prettyBlock Precedence.Sequence body
                let d = whenIReceive +. " " ++ prettyStringLiteral name +. " {" ++ group (nest (nl ++ body) ++ nl) +. "}"
                return struct(d, Precedence.Application)

            | Some self ->
                //do self.WhenIReceive %name {
                //    %body
                //}

                let! whenIReceive = prettySpriteMethodCall <@ fun s -> s.WhenIReceive @> self [prettyStringLiteral name]
                let! body = prettyBlock Precedence.Sequence body
                let d =
                    "do " .+ self.selfName ++ whenIReceive +. " {" ++
                    group (nest (nl ++ body) ++ nl) +.
                    "}"
                return struct(d, Precedence.Call)
        | _ -> return! skip()

    } // [ListenerHeaderType.EventName]
    //"whenKeyPressed", [ListenerHeaderType.AnyOrKeyName]
    //"whenSceneStarts", [ListenerHeaderType.String; ListenerHeaderType.Null; ListenerHeaderType.Null; ListenerHeaderType.Bool]
    ]

[<AbstractClass; Sealed>]
type private KnownListenerMapHolder<'a> private () =
    static let value: Map<_, PluginProcess<'a,_>> = Map knownListeners
    static member Value = value
[<GeneralizableValue>]
let knownListenerMap<'a> = KnownListenerMapHolder<'a>.Value

let listenerPlugin (ListenerDefinition(name = name) as l) =
    match Map.tryFind name knownListenerMap with
    | ValueNone -> skip()
    | ValueSome f -> f l

let plugin = {
    expression = expressionPlugin
    statement = statementPlugin
    listener = listenerPlugin
    globalNames = Seq.empty
}
