module Scratch.Symbol
open Scratch.Ast
open System.Collections.Generic

let private gate = obj()
let mutable max = LanguagePrimitives.EnumToValue O.KNOWN_MAX
let mutable private nameToSymbol = null
let mutable private symbolToName = null

let ofName = function
    | "" -> O.Empty
    | "%" -> O.``%``
    | "&" -> O.``&``
    | "*" -> O.``*``
    | "+" -> O.``+``
    | "-" -> O.``-``
    | "/" -> O.``/``
    | "<" -> O.``<``
    | "=" -> O.``=``
    | ">" -> O.``>``
    | "\\" -> O.``\\``
    | "abs" -> O.abs
    | "answer" -> O.answer
    | "append:toList:" -> O.``append:toList:``
    | "backgroundIndex" -> O.backgroundIndex
    | "bounceOffEdge" -> O.bounceOffEdge
    | "broadcast:" -> O.``broadcast:``
    | "call" -> O.call
    | "changeGraphicEffect:by:" -> O.``changeGraphicEffect:by:``
    | "changePenHueBy:" -> O.``changePenHueBy:``
    | "changePenShadeBy:" -> O.``changePenShadeBy:``
    | "changePenSizeBy:" -> O.``changePenSizeBy:``
    | "changeSizeBy:" -> O.``changeSizeBy:``
    | "changeTempoBy:" -> O.``changeTempoBy:``
    | "changeVar:by:" -> O.``changeVar:by:``
    | "changeVolumeBy:" -> O.``changeVolumeBy:``
    | "changeXposBy:" -> O.``changeXposBy:``
    | "changeYposBy:" -> O.``changeYposBy:``
    | "clearPenTrails" -> O.clearPenTrails
    | "color:sees:" -> O.``color:sees:``
    | "comeToFront" -> O.comeToFront
    | "computeFunction:of:" -> O.``computeFunction:of:``
    | "concatenate:with:" -> O.``concatenate:with:``
    | "contentsOfList:" -> O.``contentsOfList:``
    | "costumeIndex" -> O.costumeIndex
    | "costumeName" -> O.costumeName
    | "createCloneOf" -> O.createCloneOf
    | "deleteClone" -> O.deleteClone
    | "deleteLine:ofList:" -> O.``deleteLine:ofList:``
    | "distanceTo:" -> O.``distanceTo:``
    | "doAsk" -> O.doAsk
    | "doBroadcastAndWait" -> O.doBroadcastAndWait
    | "doForever" -> O.doForever
    | "doForeverIf" -> O.doForeverIf
    | "doIf" -> O.doIf
    | "doIfElse" -> O.doIfElse
    | "doPlaySoundAndWait" -> O.doPlaySoundAndWait
    | "doRepeat" -> O.doRepeat
    | "doReturn" -> O.doReturn
    | "doUntil" -> O.doUntil
    | "doWaitUntil" -> O.doWaitUntil
    | "doWhile" -> O.doWhile
    | "filterReset" -> O.filterReset
    | "forward:" -> O.``forward:``
    | "getAttribute:of:" -> O.``getAttribute:of:``
    | "getLine:ofList:" -> O.``getLine:ofList:``
    | "getParam" -> O.getParam
    | "getUserId" -> O.getUserId
    | "getUserName" -> O.getUserName
    | "glideSecs:toX:y:elapsed:from:" -> O.``glideSecs:toX:y:elapsed:from:``
    | "goBackByLayers:" -> O.``goBackByLayers:``
    | "gotoSpriteOrMouse:" -> O.``gotoSpriteOrMouse:``
    | "gotoX:y:" -> O.``gotoX:y:``
    | "heading" -> O.heading
    | "heading:" -> O.``heading:``
    | "hide" -> O.hide
    | "hideList:" -> O.``hideList:``
    | "hideVariable:" -> O.``hideVariable:``
    | "insert:at:ofList:" -> O.``insert:at:ofList:``
    | "instrument:" -> O.``instrument:``
    | "keyPressed:" -> O.``keyPressed:``
    | "letter:of:" -> O.``letter:of:``
    | "lineCountOfList:" -> O.``lineCountOfList:``
    | "list:contains:" -> O.``list:contains:``
    | "lookLike:" -> O.``lookLike:``
    | "mousePressed" -> O.mousePressed
    | "mouseX" -> O.mouseX
    | "mouseY" -> O.mouseY
    | "nextBackground" -> O.nextBackground
    | "nextCostume" -> O.nextCostume
    | "nextScene" -> O.nextScene
    | "not" -> O.not
    | "noteOn:duration:elapsed:from:" -> O.``noteOn:duration:elapsed:from:``
    | "penColor:" -> O.``penColor:``
    | "penSize:" -> O.``penSize:``
    | "playDrum" -> O.playDrum
    | "playSound:" -> O.``playSound:``
    | "pointTowards:" -> O.``pointTowards:``
    | "putPenDown" -> O.putPenDown
    | "putPenUp" -> O.putPenUp
    | "randomFrom:to:" -> O.``randomFrom:to:``
    | "readVariable" -> O.readVariable
    | "rest:elapsed:from:" -> O.``rest:elapsed:from:``
    | "rounded" -> O.rounded
    | "say:" -> O.``say:``
    | "say:duration:elapsed:from:" -> O.``say:duration:elapsed:from:``
    | "scale" -> O.scale
    | "sceneName" -> O.sceneName
    | "setGraphicEffect:to:" -> O.``setGraphicEffect:to:``
    | "setLine:ofList:to:" -> O.``setLine:ofList:to:``
    | "setPenHueTo:" -> O.``setPenHueTo:``
    | "setPenShadeTo:" -> O.``setPenShadeTo:``
    | "setRotationStyle" -> O.setRotationStyle
    | "setSizeTo:" -> O.``setSizeTo:``
    | "setTempoTo:" -> O.``setTempoTo:``
    | "setVar:to:" -> O.``setVar:to:``
    | "setVolumeTo:" -> O.``setVolumeTo:``
    | "show" -> O.show
    | "showBackground:" -> O.``showBackground:``
    | "showList:" -> O.``showList:``
    | "showVariable:" -> O.``showVariable:``
    | "sqrt" -> O.sqrt
    | "stampCostume" -> O.stampCostume
    | "startScene" -> O.startScene
    | "startSceneAndWait" -> O.startSceneAndWait
    | "stopAll" -> O.stopAll
    | "stopAllSounds" -> O.stopAllSounds
    | "stopScripts" -> O.stopScripts
    | "stringLength:" -> O.``stringLength:``
    | "tempo" -> O.tempo
    | "think:" -> O.``think:``
    | "think:duration:elapsed:from:" -> O.``think:duration:elapsed:from:``
    | "timeAndDate" -> O.timeAndDate
    | "timer" -> O.timer
    | "timerReset" -> O.timerReset
    | "timestamp" -> O.timestamp
    | "touching:" -> O.``touching:``
    | "touchingColor:" -> O.``touchingColor:``
    | "turnLeft:" -> O.``turnLeft:``
    | "turnRight:" -> O.``turnRight:``
    | "volume" -> O.volume
    | "wait:elapsed:from:" -> O.``wait:elapsed:from:``
    | "warpSpeed" -> O.warpSpeed
    | "xpos" -> O.xpos
    | "xpos:" -> O.``xpos:``
    | "ypos" -> O.ypos
    | "ypos:" -> O.``ypos:``
    | "|" -> O.``|``

    | "whenClicked" -> O.whenClicked
    | "whenGreenFlag" -> O.whenGreenFlag
    | "whenCloned" -> O.whenCloned
    | "whenIReceive" -> O.whenIReceive
    | "whenKeyPressed" -> O.whenKeyPressed
    | "whenSceneStarts" -> O.whenSceneStarts

    | name -> lock gate <| fun _ ->

    if isNull nameToSymbol then
        nameToSymbol <- Dictionary()

    let mutable r = Unchecked.defaultof<_>
    if nameToSymbol.TryGetValue(name, &r) then LanguagePrimitives.EnumOfValue r else

    if isNull symbolToName then
        symbolToName <- Dictionary()

    max <- Checked.(+) max 1uy
    nameToSymbol.Add(name, max)
    symbolToName.Add(max, name)
    LanguagePrimitives.EnumOfValue max

let name = function
    | O.Empty -> ""
    | O.``%`` -> "%"
    | O.``&`` -> "&"
    | O.``*`` -> "*"
    | O.``+`` -> "+"
    | O.``-`` -> "-"
    | O.``/`` -> "/"
    | O.``<`` -> "<"
    | O.``=`` -> "="
    | O.``>`` -> ">"
    | O.``\\`` -> "\\"
    | O.abs -> "abs"
    | O.answer -> "answer"
    | O.``append:toList:`` -> "append:toList:"
    | O.backgroundIndex -> "backgroundIndex"
    | O.bounceOffEdge -> "bounceOffEdge"
    | O.``broadcast:`` -> "broadcast:"
    | O.call -> "call"
    | O.``changeGraphicEffect:by:`` -> "changeGraphicEffect:by:"
    | O.``changePenHueBy:`` -> "changePenHueBy:"
    | O.``changePenShadeBy:`` -> "changePenShadeBy:"
    | O.``changePenSizeBy:`` -> "changePenSizeBy:"
    | O.``changeSizeBy:`` -> "changeSizeBy:"
    | O.``changeTempoBy:`` -> "changeTempoBy:"
    | O.``changeVar:by:`` -> "changeVar:by:"
    | O.``changeVolumeBy:`` -> "changeVolumeBy:"
    | O.``changeXposBy:`` -> "changeXposBy:"
    | O.``changeYposBy:`` -> "changeYposBy:"
    | O.clearPenTrails -> "clearPenTrails"
    | O.``color:sees:`` -> "color:sees:"
    | O.comeToFront -> "comeToFront"
    | O.``computeFunction:of:`` -> "computeFunction:of:"
    | O.``concatenate:with:`` -> "concatenate:with:"
    | O.``contentsOfList:`` -> "contentsOfList:"
    | O.costumeIndex -> "costumeIndex"
    | O.costumeName -> "costumeName"
    | O.createCloneOf -> "createCloneOf"
    | O.deleteClone -> "deleteClone"
    | O.``deleteLine:ofList:`` -> "deleteLine:ofList:"
    | O.``distanceTo:`` -> "distanceTo:"
    | O.doAsk -> "doAsk"
    | O.doBroadcastAndWait -> "doBroadcastAndWait"
    | O.doForever -> "doForever"
    | O.doForeverIf -> "doForeverIf"
    | O.doIf -> "doIf"
    | O.doIfElse -> "doIfElse"
    | O.doPlaySoundAndWait -> "doPlaySoundAndWait"
    | O.doRepeat -> "doRepeat"
    | O.doReturn -> "doReturn"
    | O.doUntil -> "doUntil"
    | O.doWaitUntil -> "doWaitUntil"
    | O.doWhile -> "doWhile"
    | O.filterReset -> "filterReset"
    | O.``forward:`` -> "forward:"
    | O.``getAttribute:of:`` -> "getAttribute:of:"
    | O.``getLine:ofList:`` -> "getLine:ofList:"
    | O.getParam -> "getParam"
    | O.getUserId -> "getUserId"
    | O.getUserName -> "getUserName"
    | O.``glideSecs:toX:y:elapsed:from:`` -> "glideSecs:toX:y:elapsed:from:"
    | O.``goBackByLayers:`` -> "goBackByLayers:"
    | O.``gotoSpriteOrMouse:`` -> "gotoSpriteOrMouse:"
    | O.``gotoX:y:`` -> "gotoX:y:"
    | O.heading -> "heading"
    | O.``heading:`` -> "heading:"
    | O.hide -> "hide"
    | O.``hideList:`` -> "hideList:"
    | O.``hideVariable:`` -> "hideVariable:"
    | O.``insert:at:ofList:`` -> "insert:at:ofList:"
    | O.``instrument:`` -> "instrument:"
    | O.``keyPressed:`` -> "keyPressed:"
    | O.``letter:of:`` -> "letter:of:"
    | O.``lineCountOfList:`` -> "lineCountOfList:"
    | O.``list:contains:`` -> "list:contains:"
    | O.``lookLike:`` -> "lookLike:"
    | O.mousePressed -> "mousePressed"
    | O.mouseX -> "mouseX"
    | O.mouseY -> "mouseY"
    | O.nextBackground -> "nextBackground"
    | O.nextCostume -> "nextCostume"
    | O.nextScene -> "nextScene"
    | O.not -> "not"
    | O.``noteOn:duration:elapsed:from:`` -> "noteOn:duration:elapsed:from:"
    | O.``penColor:`` -> "penColor:"
    | O.``penSize:`` -> "penSize:"
    | O.playDrum -> "playDrum"
    | O.``playSound:`` -> "playSound:"
    | O.``pointTowards:`` -> "pointTowards:"
    | O.putPenDown -> "putPenDown"
    | O.putPenUp -> "putPenUp"
    | O.``randomFrom:to:`` -> "randomFrom:to:"
    | O.readVariable -> "readVariable"
    | O.``rest:elapsed:from:`` -> "rest:elapsed:from:"
    | O.rounded -> "rounded"
    | O.``say:`` -> "say:"
    | O.``say:duration:elapsed:from:`` -> "say:duration:elapsed:from:"
    | O.scale -> "scale"
    | O.sceneName -> "sceneName"
    | O.``setGraphicEffect:to:`` -> "setGraphicEffect:to:"
    | O.``setLine:ofList:to:`` -> "setLine:ofList:to:"
    | O.``setPenHueTo:`` -> "setPenHueTo:"
    | O.``setPenShadeTo:`` -> "setPenShadeTo:"
    | O.setRotationStyle -> "setRotationStyle"
    | O.``setSizeTo:`` -> "setSizeTo:"
    | O.``setTempoTo:`` -> "setTempoTo:"
    | O.``setVar:to:`` -> "setVar:to:"
    | O.``setVolumeTo:`` -> "setVolumeTo:"
    | O.show -> "show"
    | O.``showBackground:`` -> "showBackground:"
    | O.``showList:`` -> "showList:"
    | O.``showVariable:`` -> "showVariable:"
    | O.sqrt -> "sqrt"
    | O.stampCostume -> "stampCostume"
    | O.startScene -> "startScene"
    | O.startSceneAndWait -> "startSceneAndWait"
    | O.stopAll -> "stopAll"
    | O.stopAllSounds -> "stopAllSounds"
    | O.stopScripts -> "stopScripts"
    | O.``stringLength:`` -> "stringLength:"
    | O.tempo -> "tempo"
    | O.``think:`` -> "think:"
    | O.``think:duration:elapsed:from:`` -> "think:duration:elapsed:from:"
    | O.timeAndDate -> "timeAndDate"
    | O.timer -> "timer"
    | O.timerReset -> "timerReset"
    | O.timestamp -> "timestamp"
    | O.``touching:`` -> "touching:"
    | O.``touchingColor:`` -> "touchingColor:"
    | O.``turnLeft:`` -> "turnLeft:"
    | O.``turnRight:`` -> "turnRight:"
    | O.volume -> "volume"
    | O.``wait:elapsed:from:`` -> "wait:elapsed:from:"
    | O.warpSpeed -> "warpSpeed"
    | O.xpos -> "xpos"
    | O.``xpos:`` -> "xpos:"
    | O.ypos -> "ypos"
    | O.``ypos:`` -> "ypos:"
    | O.``|`` -> "|"

    | O.whenClicked -> "whenClicked"
    | O.whenGreenFlag -> "whenGreenFlag"
    | O.whenCloned -> "whenCloned"
    | O.whenIReceive -> "whenIReceive"
    | O.whenKeyPressed -> "whenKeyPressed"
    | O.whenSceneStarts -> "whenSceneStarts"

    | x ->
        lock gate <| fun () ->
        if isNull symbolToName then
            symbolToName <- Dictionary()

        let mutable r = null
        if symbolToName.TryGetValue(LanguagePrimitives.EnumToValue x, &r) then r
        else failwithf "symbol %A is not registed" x
