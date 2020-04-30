namespace Scratch.Ast
#nowarn "0086" // The '=' operator should not normally be redefined.
open Scratch.Ast


[<AutoOpen>]
module private Helpers =
    let block s xs = BlockExpression(s, xs)
    let sString p x = Expression.eString p x
    let unary p op x = Complex(ComplexExpression(p, op, [x]))
    let binary p op l r = Complex(ComplexExpression(p, op, [l; r]))

    let mapChoice3 f1 f2 f3 = function
        | Choice1Of3 x -> Choice1Of3(f1 x)
        | Choice2Of3 x -> Choice2Of3(f2 x)
        | Choice3Of3 x -> Choice3Of3(f3 x)

module ListenerDefinition =
    let map f (ListenerDefinition(state, name, args, body)) =
        ListenerDefinition(f state, name, List.map (Expression.map f) args, BlockExpression.map f body)

    let state (ListenerDefinition(state = x)) = x
    let withState state (ListenerDefinition(_, name, args, body)) = ListenerDefinition(state, name, args, body)

module ParameterDefinition =
    let map f (ParameterDefinition(x, n, v)) = ParameterDefinition(f x, n, v)
    let make s n t = ParameterDefinition(s, n, SType.parameterDefaultValue t)

module ProcedureDefinition =
    let map f (ProcedureDefinition(state, name, ps, isAtomic, body)) =
        ProcedureDefinition(f state, name, List.map (ParameterDefinition.map f) ps, isAtomic, BlockExpression.map f body)
    let state (ProcedureDefinition(state = state)) = state
    let withState state (ProcedureDefinition(_, name, ps, isAtomic, body)) = ProcedureDefinition(state, name, ps, isAtomic, body)

module Script =
    let map f = function
        | Statements x -> Statements(BlockExpression.map f x)
        | Expression x -> Expression(ComplexExpression.map f x)
        | Listener x -> Listener(ListenerDefinition.map f x)
        | Procedure x -> Procedure(ProcedureDefinition.map f x)

    let fold folder state = function
        | Expression e -> ComplexExpression.fold folder state e
        | Statements e -> BlockExpression.fold folder state e
        | Procedure(ProcedureDefinition(body = body)) -> BlockExpression.fold folder state body
        | Listener(ListenerDefinition(arguments = arguments; body = body)) ->
            let state = List.fold (Expression.fold folder) state arguments
            BlockExpression.fold folder state body

    let tryPick chooser = function
        | Expression e -> ComplexExpression.tryPick chooser e
        | Statements e -> BlockExpression.tryPick chooser e
        | Procedure(ProcedureDefinition(body = body)) -> BlockExpression.tryPick chooser body
        | Listener(ListenerDefinition(arguments = arguments; body = body)) ->
            match List.tryPick (Expression.tryPick chooser) arguments with
            | Some _ as r -> r
            | _ -> BlockExpression.tryPick chooser body

    let variableCount varName x = fold (fun n c -> n + if ComplexExpression.isVariableAccess varName c then 1 else 0) 0 x
    let hasVariable varName x = tryPick (fun c -> if ComplexExpression.isVariableAccess varName c then Some() else None) x |> Option.isSome

    let state = function
        | Statements x -> BlockExpression.state x
        | Expression x -> ComplexExpression.state x
        | Procedure x -> ProcedureDefinition.state x
        | Listener x -> ListenerDefinition.state x

    let withState s = function
        | Statements x -> BlockExpression.withState s x |> Statements
        | Expression x -> ComplexExpression.withState s x |> Expression
        | Procedure x -> ProcedureDefinition.withState s x |> Procedure
        | Listener x -> ListenerDefinition.withState s x |> Listener

    let atomicity = function
        | Procedure(ProcedureDefinition(isAtomic = a)) -> a
        | Statements _
        | Expression _
        | Listener _ -> NoAtomic

module VariableData =
    let map f x = {
        name = x.name
        state = f x.state
        isPersistent = x.isPersistent
        value = x.value
    }
    let make p n t = {
        state = p
        isPersistent = NoPersistent
        name = n
        value = SType.parameterDefaultValue t
    }

module ListData =
    let map f ({ ListVariableData.state = state } as x) = {
        state = f state
        isPersistent = x.isPersistent
        listName = x.listName
        contents' = x.contents'

        x = x.x
        y = x.y
        width = x.width
        height = x.height
        visible = x.visible
    }
    let make p n contents = {
        state = p
        listName = n
        isPersistent = NoPersistent
        contents' = Scratch.Primitives.IArray.ofSeq contents
        x = 0.
        y = 0.
        width = 100.
        height = 100.
        visible = Hidden
    }

module ScriptData =
    let map f { ScriptData.x = x; y = y; script = script } = { x = x; y = y; script = Script.map f script }
    let procedure p name ps isAtomic stats = {
        x = 0.
        y = 0.
        script = Procedure(ProcedureDefinition(p, name, ps, isAtomic, block p stats))
    }

module EntityData =
    let defaultValue objName extension = {
        objName = objName
        variables = []
        lists = []
        scripts = []
        costumes = []
        sounds = []
        currentCostumeIndex = Some 0.

        ObjectDataExtension = extension
    }
    let map extensionMapping stateMapping x = {
        objName = x.objName
        variables = List.map (VariableData.map stateMapping) x.variables
        lists = List.map (ListData.map stateMapping) x.lists
        scripts = List.map (ScriptData.map stateMapping) x.scripts
        costumes = x.costumes
        sounds = x.sounds
        currentCostumeIndex = x.currentCostumeIndex

        ObjectDataExtension = extensionMapping x.ObjectDataExtension
    }
    let mapExtension extensionMapping x = {
        objName = x.objName
        scripts = x.scripts
        costumes = x.costumes
        sounds = x.sounds
        variables = x.variables
        lists = x.lists
        currentCostumeIndex = x.currentCostumeIndex

        ObjectDataExtension = extensionMapping x.ObjectDataExtension
    }
    let unState x = map id ignore x

module SpriteData =
    let defaultValue = {
        objName = "Sprite1"
        variables = []
        lists = []
        scripts = []
        costumes = []
        sounds = []
        currentCostumeIndex = Some 0.

        ObjectDataExtension = {
            direction = 90.
            indexInLibrary = 100000.
            isDraggable = false
            rotationStyle = RotationStyle.Normal
            scale = 1.
            scratchX = 0.
            scratchY = 0.
            spriteInfo = Map.empty
            visible = Visible
        }
    }
    let map f x = EntityData.map id f x

module StageDataExtension =
    let map f { children = children; penLayerMD5 = penLayerMD5; penLayerID = penLayerID; tempoBPM = tempoBPM; videoAlpha = videoAlpha; info = info } = {
        children = List.map (fun x -> mapChoice3 id (SpriteData.map f) (ListData.map f) x) children
        penLayerMD5 = penLayerMD5
        penLayerID = penLayerID
        tempoBPM = tempoBPM
        videoAlpha = videoAlpha
        info = info
    }

module StageData =
    let map f x = EntityData.map (StageDataExtension.map f) f x
    let defaultValue = {
        objName = "Stage"
        variables = []
        lists = []
        scripts = []
        costumes = []
        sounds = []
        currentCostumeIndex = Some 0.
    
        ObjectDataExtension = {
            children = []
            penLayerID = None // 0
            penLayerMD5 = None // "5c81a336fab8be57adc039a8a2b33ca9.png"
            tempoBPM = Some 60.
            videoAlpha = Some 0.5
            info = Map.empty
            (*
            info: {
                "scriptCount": 0,
                "spriteCount": 1,
                "userAgent": "Mozilla\/5.0 (Windows NT 10.0) AppleWebKit\/537.36 (KHTML, like Gecko) Chrome\/69.0.3497.81 Safari\/537.36",
                "swfVersion": "v461",
                "hasCloudData": false,
                "videoOn": false,
                "flashVersion": "WIN 31,0,0,108",
                "projectID": "?????????"
                }
            *)
        }
    }
    let sprites x = seq {
        for c in x.ObjectDataExtension.children do
            match c with
            | Choice2Of3 c -> yield c
            | _ -> ()
    }

    let [<Literal>] private ModuleSpriteName = "__Module__"
    let applyScratch3ExecutionOrderTrick stage =
        let children = stage.ObjectDataExtension.children
        match children, stage.scripts with
        | [], _
        | _, [] -> stage
        | _, scripts ->
    
        // TODO: スプライトの名前が衝突しないようにする
        let moduleSprite =
            let d = SpriteData.defaultValue
            { d with
                objName = ModuleSpriteName
                scripts = scripts
                ObjectDataExtension =
                { d.ObjectDataExtension with
                    visible = Hidden
                }
            }
    
        { stage with
            scripts = []
            ObjectDataExtension =
            { stage.ObjectDataExtension with
                children = children @ [Choice2Of3 moduleSprite]
            }
        }

    let unapplyScratch3ExecutionOrderTrick stage =
        match stage.ObjectDataExtension.children, stage.scripts with
        | [], _
        | _, _::_ -> stage
        | children, [] ->

        match List.last children with
        | Choice2Of3 { objName = name; scripts = scripts } when name = ModuleSpriteName -> 
            { stage with
                scripts = scripts
                ObjectDataExtension =
                { stage.ObjectDataExtension with
                    children = List.take (List.length children - 1) children
                }
            }
        | _ -> stage

module Expressions =
    open Expression

    let getParam p name = binary p O.getParam (eString p name) (eString p "r")
    let ``+`` p l r = binary p O.``+`` l r
    let ``-`` p l r = binary p O.``-`` l r
    let ``*`` p l r = binary p O.``*`` l r
    let ``/`` p l r = binary p O.``/`` l r
    let ``%`` p l r = binary p O.``%`` l r
    let ``computeFunction:of:`` p name x = binary p O.``computeFunction:of:`` name x
    let ``computeFunction"floor"of:`` p x = ``computeFunction:of:`` p (eString p "floor") x
    let ``computeFunction"ceiling"of:`` p x = ``computeFunction:of:`` p (eString p "ceiling") x
    let ``computeFunction"ln"of:`` p x = ``computeFunction:of:`` p (eString p "ln") x
    let ``computeFunction"log"of:`` p x = ``computeFunction:of:`` p (eString p "log") x
    let ``computeFunction"abs"of:`` p x = ``computeFunction:of:`` p (eString p "abs") x
    let ``computeFunction"sqrt"of:`` p x = ``computeFunction:of:`` p (eString p "sqrt") x
    let rounded p x = unary p O.rounded x
    let ``concatenate:with:`` p l r = binary p O.``concatenate:with:`` l r
    let ``=`` p l r = binary p O.``=`` l r
    let not p x = unary p O.not x
    let ``<`` p l r = binary p O.``<`` l r
    let ``>`` p l r = binary p O.``>`` l r
    let ``|`` p l r = binary p O.``|`` l r
    let ``&`` p l r = binary p O.``&`` l r
    let readVariable p n = unary p O.readVariable (eString p n)
    let ``showVariable:`` p n = ComplexExpression(p, O.``showVariable:``, [eString p n])
    let ``hideVariable:`` p n = ComplexExpression(p, O.``hideVariable:``, [eString p n])
    let ``showList:`` p n = ComplexExpression(p, O.``showList:``, [eString p n])
    let ``hideList:`` p n = ComplexExpression(p, O.``hideList:``, [eString p n])
    let hide p = ComplexExpression(p, O.hide, [])
    let show p = ComplexExpression(p, O.show, [])
    let ``getLine:ofList:`` p name line = binary p O.``getLine:ofList:`` line (eString p name)
    let ``lineCountOfList:`` p name = unary p O.``lineCountOfList:`` (eString p name)
    let ``stringLength:`` p x = unary p O.``stringLength:`` x
    let ``letter:of:`` p nth target = binary p O.``letter:of:`` nth target
    let timer p = Complex(ComplexExpression(p, O.timer, []))
    let xpos p = Complex(ComplexExpression(p, O.xpos, []))
    let ypos p = Complex(ComplexExpression(p, O.ypos, []))
    /// <param name="key">KeyName | KeyCode</param>
    let ``keyPressed:`` p key = Complex(ComplexExpression(p, O.``keyPressed:``, [key]))
    let mousePressed p = Complex(ComplexExpression(p, O.mousePressed, []))
    let mouseX p = Complex(ComplexExpression(p, O.mouseX, []))
    let mouseY p = Complex(ComplexExpression(p, O.mouseY, []))

    let ``setVar:to:`` p name value = ComplexExpression(p, O.``setVar:to:``, [sString p name; value])
    let ``changeVar:by:`` p name value = ComplexExpression(p, O.``changeVar:by:``, [sString p name; value])
    let call p name args = ComplexExpression(p, O.call, sString p name::args)
    let ``append:toList:`` p name value = ComplexExpression(p, O.``append:toList:``, [value; sString p name])
    let doIfElse p test p2 ifTrue p3 ifFalse = ComplexExpression(p, O.doIfElse, [test; Block(block p2 ifTrue); Block(block p3 ifFalse)])
    let doIf p test p2 ifTrue = ComplexExpression(p, O.doIf, [test; Block(block p2 ifTrue)])
    let ``deleteLine:ofList:`` p name line = ComplexExpression(p, O.``deleteLine:ofList:``, [line; sString p name])
    let ``setLine:ofList:to:`` p name line value = ComplexExpression(p, O.``setLine:ofList:to:``, [line; sString p name; value])
    let ``insert:at:ofList:`` p name line value = ComplexExpression(p, O.``insert:at:ofList:``, [value; line; sString p name])

    let doForever p body = ComplexExpression(p, O.doForever, [Block body])
    let doRepeat p count body = ComplexExpression(p, O.doRepeat, [count; Block body])
    let doUntil p test ifFalse = ComplexExpression(p, O.doUntil, [test; Block ifFalse])
    let doWaitUntil p test = ComplexExpression(p, O.doWaitUntil, [test])
    let ``wait:elapsed:from:`` p seconds = ComplexExpression(p, O.``wait:elapsed:from:``, [seconds])
    let broadcast p name = ComplexExpression(p, O.``broadcast:``, [sString p name])
    let doBroadcastAndWait p name = ComplexExpression(p, O.doBroadcastAndWait, [sString p name])

    /// <param name="kind">"all" | "this script"</param>
    let stopScripts p kind = ComplexExpression(p, O.stopScripts, [sString p kind])
    /// <param name="name">"_myself" | SpriteName</param>
    let createCloneOf p name = ComplexExpression(p, O.createCloneOf, [sString p name])
    let deleteClone p = ComplexExpression(p, O.deleteClone, [])
    let ``gotoX:y:`` p x y = ComplexExpression(p, O.``gotoX:y:``, [x; y])
    let ``lookLike:`` p costume = ComplexExpression(p, O.``lookLike:``, [costume])
    /// <param name="filterName">"color" | "fisheye" | "whirl" | "pixelate" | "mosaic" | "brightness" | "ghost"</param>
    let ``setGraphicEffect:to:`` p filterName value = ComplexExpression(p, O.``setGraphicEffect:to:``, [sString p filterName; value])
    let ``changeSizeBy:`` p value = ComplexExpression(p, O.``changeSizeBy:``, [value])
    let ``setSizeTo:`` p value = ComplexExpression(p, O.``setSizeTo:``, [value])

    let listenerScript p name args stats = { x = 0.; y = 0.; script = Listener <| ListenerDefinition(p, name, args, block p stats) }
    let whenGreenFlag p stats = listenerScript p O.whenGreenFlag [] stats
    let whenIReceive p name stats = listenerScript p O.whenIReceive [eString p name] stats
    let whenCloned p stats = listenerScript p O.whenCloned [] stats
