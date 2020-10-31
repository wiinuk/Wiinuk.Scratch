module Scratch.Serialization.Sb3.OpCodeSpecs
type O = Scratch.Ast.Symbol

[<Struct; RequireQualifiedAccess>]
type VariableType =
    | Scalar
    | List
    | BroadcastMessage

type ArgInfo =
    | EmptyArg
    | InputArg of inputOp: string * inputName: string * variableType: VariableType option
    | FieldArg of fieldName: string * variableType: VariableType option

type BlockInfo = {
    opcode: string
    category: string
    argMap: ArgInfo list
}
let sb2ExpressionSpecs = Map [|
    O.``forward:``, {
        opcode = "motion_movesteps" 
        category = "motion"
        argMap = [
            InputArg("math_number", "STEPS", None)
        ]
    }
    O.``turnRight:``, {
        opcode = "motion_turnright" 
        category = "motion"
        argMap = [
            InputArg("math_number", "DEGREES", None)
        ]
    }
    O.``turnLeft:``, {
        opcode = "motion_turnleft" 
        category = "motion"
        argMap = [
            InputArg("math_number", "DEGREES", None)
        ]
    }
    O.``heading:``, {
        opcode = "motion_pointindirection" 
        category = "motion"
        argMap = [
            InputArg("math_angle", "DIRECTION", None)
        ]
    }
    O.``pointTowards:``, {
        opcode = "motion_pointtowards" 
        category = "motion"
        argMap = [
            InputArg("motion_pointtowards_menu", "TOWARDS", None)
        ]
    }
    O.``gotoX:y:``, {
        opcode = "motion_gotoxy" 
        category = "motion"
        argMap = [
            InputArg("math_number", "X", None)
            InputArg("math_number", "Y", None)
        ]
    }
    O.``gotoSpriteOrMouse:``, {
        opcode = "motion_goto" 
        category = "motion"
        argMap = [
            InputArg("motion_goto_menu", "TO", None)
        ]
    }
    O.``glideSecs:toX:y:elapsed:from:``, {
        opcode = "motion_glidesecstoxy" 
        category = "motion"
        argMap = [
            InputArg("math_number", "SECS", None)
            InputArg("math_number", "X", None)
            InputArg("math_number", "Y", None)
        ]
    }
    O.``changeXposBy:``, {
        opcode = "motion_changexby" 
        category = "motion"
        argMap = [
            InputArg("math_number", "DX", None)
        ]
    }
    O.``xpos:``, {
        opcode = "motion_setx" 
        category = "motion"
        argMap = [
            InputArg("math_number", "X", None)
        ]
    }
    O.``changeYposBy:``, {
        opcode = "motion_changeyby" 
        category = "motion"
        argMap = [
            InputArg("math_number", "DY", None)
        ]
    }
    O.``ypos:``, {
        opcode = "motion_sety" 
        category = "motion"
        argMap = [
            InputArg("math_number", "Y", None)
        ]
    }
    O.bounceOffEdge, {
        opcode = "motion_ifonedgebounce" 
        category = "motion"
        argMap = [
        ]
    }
    O.setRotationStyle, {
        opcode = "motion_setrotationstyle" 
        category = "motion"
        argMap = [
            FieldArg("STYLE", None)
        ]
    }
    O.xpos, {
        opcode = "motion_xposition" 
        category = "motion"
        argMap = [
        ]
    }
    O.ypos, {
        opcode = "motion_yposition" 
        category = "motion"
        argMap = [
        ]
    }
    O.heading, {
        opcode = "motion_direction" 
        category = "motion"
        argMap = [
        ]
    }
    (*
    O.scrollRight, {
        opcode = "motion_scroll_right" 
        category = "motion"
        argMap = [
            InputArg("math_number", "DISTANCE", None)
        ]
    }
    *)
    (*
    O.scrollUp, {
        opcode = "motion_scroll_up" 
        category = "motion"
        argMap = [
            InputArg("math_number", "DISTANCE", None)
        ]
    }
    *)
    (*
    O.scrollAlign, {
        opcode = "motion_align_scene" 
        category = "motion"
        argMap = [
            FieldArg("ALIGNMENT", None)
        ]
    }
    *)
    (*
    O.xScroll, {
        opcode = "motion_xscroll" 
        category = "motion"
        argMap = [
        ]
    }
    *)
    (*
    O.yScroll, {
        opcode = "motion_yscroll" 
        category = "motion"
        argMap = [
        ]
    }
    *)
    O.``say:duration:elapsed:from:``, {
        opcode = "looks_sayforsecs" 
        category = "looks"
        argMap = [
            InputArg("text", "MESSAGE", None)
            InputArg("math_number", "SECS", None)
        ]
    }
    O.``say:``, {
        opcode = "looks_say" 
        category = "looks"
        argMap = [
            InputArg("text", "MESSAGE", None)
        ]
    }
    O.``think:duration:elapsed:from:``, {
        opcode = "looks_thinkforsecs" 
        category = "looks"
        argMap = [
            InputArg("text", "MESSAGE", None)
            InputArg("math_number", "SECS", None)
        ]
    }
    O.``think:``, {
        opcode = "looks_think" 
        category = "looks"
        argMap = [
            InputArg("text", "MESSAGE", None)
        ]
    }
    O.show, {
        opcode = "looks_show" 
        category = "looks"
        argMap = [
        ]
    }
    O.hide, {
        opcode = "looks_hide" 
        category = "looks"
        argMap = [
        ]
    }
    (*
    O.hideAll, {
        opcode = "looks_hideallsprites" 
        category = "looks"
        argMap = [
        ]
    }
    *)
    O.``lookLike:``, {
        opcode = "looks_switchcostumeto" 
        category = "looks"
        argMap = [
            InputArg("looks_costume", "COSTUME", None)
        ]
    }
    O.nextCostume, {
        opcode = "looks_nextcostume" 
        category = "looks"
        argMap = [
        ]
    }
    O.startScene, {
        opcode = "looks_switchbackdropto" 
        category = "looks"
        argMap = [
            InputArg("looks_backdrops", "BACKDROP", None)
        ]
    }
    O.``changeGraphicEffect:by:``, {
        opcode = "looks_changeeffectby" 
        category = "looks"
        argMap = [
            FieldArg("EFFECT", None)
            InputArg("math_number", "CHANGE", None)
        ]
    }
    O.``setGraphicEffect:to:``, {
        opcode = "looks_seteffectto" 
        category = "looks"
        argMap = [
            FieldArg("EFFECT", None)
            InputArg("math_number", "VALUE", None)
        ]
    }
    O.filterReset, {
        opcode = "looks_cleargraphiceffects" 
        category = "looks"
        argMap = [
        ]
    }
    O.``changeSizeBy:``, {
        opcode = "looks_changesizeby" 
        category = "looks"
        argMap = [
            InputArg("math_number", "CHANGE", None)
        ]
    }
    O.``setSizeTo:``, {
        opcode = "looks_setsizeto" 
        category = "looks"
        argMap = [
            InputArg("math_number", "SIZE", None)
        ]
    }
    (*
    O.``changeStretchBy:``, {
        opcode = "looks_changestretchby" 
        category = "looks"
        argMap = [
            InputArg("math_number", "CHANGE", None)
        ]
    }
    *)
    (*
    O.``setStretchTo:``, {
        opcode = "looks_setstretchto" 
        category = "looks"
        argMap = [
            InputArg("math_number", "STRETCH", None)
        ]
    }
    *)
    O.comeToFront, {
        opcode = "looks_gotofrontback" 
        category = "looks"
        argMap = [
        ]
    }
    O.``goBackByLayers:``, {
        opcode = "looks_goforwardbackwardlayers" 
        category = "looks"
        argMap = [
            InputArg("math_integer", "NUM", None)
        ]
    }
    O.costumeIndex, {
        opcode = "looks_costumenumbername" 
        category = "looks"
        argMap = [
        ]
    }
    O.costumeName, {
        opcode = "looks_costumenumbername" 
        category = "looks"
        argMap = [
        ]
    }
    O.sceneName, {
        opcode = "looks_backdropnumbername" 
        category = "looks"
        argMap = [
        ]
    }
    O.scale, {
        opcode = "looks_size" 
        category = "looks"
        argMap = [
        ]
    }
    O.startSceneAndWait, {
        opcode = "looks_switchbackdroptoandwait" 
        category = "looks"
        argMap = [
            InputArg("looks_backdrops", "BACKDROP", None)
        ]
    }
    O.nextScene, {
        opcode = "looks_nextbackdrop" 
        category = "looks"
        argMap = [
        ]
    }
    O.backgroundIndex, {
        opcode = "looks_backdropnumbername" 
        category = "looks"
        argMap = [
        ]
    }
    O.``playSound:``, {
        opcode = "sound_play" 
        category = "sound"
        argMap = [
            InputArg("sound_sounds_menu", "SOUND_MENU", None)
        ]
    }
    O.doPlaySoundAndWait, {
        opcode = "sound_playuntildone" 
        category = "sound"
        argMap = [
            InputArg("sound_sounds_menu", "SOUND_MENU", None)
        ]
    }
    O.stopAllSounds, {
        opcode = "sound_stopallsounds" 
        category = "sound"
        argMap = [
        ]
    }
    O.playDrum, {
        opcode = "music_playDrumForBeats" 
        category = "music"
        argMap = [
            InputArg("music_menu_DRUM", "DRUM", None)
            InputArg("math_number", "BEATS", None)
        ]
    }
    (*
    O.``drum:duration:elapsed:from:``, {
        opcode = "music_midiPlayDrumForBeats" 
        category = "music"
        argMap = [
            InputArg("math_number", "DRUM", None)
            InputArg("math_number", "BEATS", None)
        ]
    }
    *)
    O.``rest:elapsed:from:``, {
        opcode = "music_restForBeats" 
        category = "music"
        argMap = [
            InputArg("math_number", "BEATS", None)
        ]
    }
    O.``noteOn:duration:elapsed:from:``, {
        opcode = "music_playNoteForBeats" 
        category = "music"
        argMap = [
            InputArg("note", "NOTE", None)
            InputArg("math_number", "BEATS", None)
        ]
    }
    O.``instrument:``, {
        opcode = "music_setInstrument" 
        category = "music"
        argMap = [
            InputArg("music_menu_INSTRUMENT", "INSTRUMENT", None)
        ]
    }
    (*
    O.``midiInstrument:``, {
        opcode = "music_midiSetInstrument" 
        category = "music"
        argMap = [
            InputArg("math_number", "INSTRUMENT", None)
        ]
    }
    *)
    O.``changeVolumeBy:``, {
        opcode = "sound_changevolumeby" 
        category = "sound"
        argMap = [
            InputArg("math_number", "VOLUME", None)
        ]
    }
    O.``setVolumeTo:``, {
        opcode = "sound_setvolumeto" 
        category = "sound"
        argMap = [
            InputArg("math_number", "VOLUME", None)
        ]
    }
    O.volume, {
        opcode = "sound_volume" 
        category = "sound"
        argMap = [
        ]
    }
    O.``changeTempoBy:``, {
        opcode = "music_changeTempo" 
        category = "music"
        argMap = [
            InputArg("math_number", "TEMPO", None)
        ]
    }
    O.``setTempoTo:``, {
        opcode = "music_setTempo" 
        category = "music"
        argMap = [
            InputArg("math_number", "TEMPO", None)
        ]
    }
    O.tempo, {
        opcode = "music_getTempo" 
        category = "music"
        argMap = [
        ]
    }
    O.clearPenTrails, {
        opcode = "pen_clear" 
        category = "pen"
        argMap = [
        ]
    }
    O.stampCostume, {
        opcode = "pen_stamp" 
        category = "pen"
        argMap = [
        ]
    }
    O.putPenDown, {
        opcode = "pen_penDown" 
        category = "pen"
        argMap = [
        ]
    }
    O.putPenUp, {
        opcode = "pen_penUp" 
        category = "pen"
        argMap = [
        ]
    }
    O.``penColor:``, {
        opcode = "pen_setPenColorToColor" 
        category = "pen"
        argMap = [
            InputArg("colour_picker", "COLOR", None)
        ]
    }
    O.``changePenHueBy:``, {
        opcode = "pen_changePenHueBy" 
        category = "pen"
        argMap = [
            InputArg("math_number", "HUE", None)
        ]
    }
    O.``setPenHueTo:``, {
        opcode = "pen_setPenHueToNumber" 
        category = "pen"
        argMap = [
            InputArg("math_number", "HUE", None)
        ]
    }
    O.``changePenShadeBy:``, {
        opcode = "pen_changePenShadeBy" 
        category = "pen"
        argMap = [
            InputArg("math_number", "SHADE", None)
        ]
    }
    O.``setPenShadeTo:``, {
        opcode = "pen_setPenShadeToNumber" 
        category = "pen"
        argMap = [
            InputArg("math_number", "SHADE", None)
        ]
    }
    O.``changePenSizeBy:``, {
        opcode = "pen_changePenSizeBy" 
        category = "pen"
        argMap = [
            InputArg("math_number", "SIZE", None)
        ]
    }
    O.``penSize:``, {
        opcode = "pen_setPenSizeTo" 
        category = "pen"
        argMap = [
            InputArg("math_number", "SIZE", None)
        ]
    }
    (*
    O.senseVideoMotion, {
        opcode = "videoSensing_videoOn" 
        category = "videoSensing"
        argMap = [
            InputArg("videoSensing_menu_ATTRIBUTE", "ATTRIBUTE", None)
            InputArg("videoSensing_menu_SUBJECT", "SUBJECT", None)
        ]
    }
    *)
    O.whenGreenFlag, {
        opcode = "event_whenflagclicked" 
        category = "event"
        argMap = [
        ]
    }
    O.whenKeyPressed, {
        opcode = "event_whenkeypressed" 
        category = "event"
        argMap = [
            FieldArg("KEY_OPTION", None)
        ]
    }
    O.whenClicked, {
        opcode = "event_whenthisspriteclicked" 
        category = "event"
        argMap = [
        ]
    }
    O.whenSceneStarts, {
        opcode = "event_whenbackdropswitchesto" 
        category = "event"
        argMap = [
            FieldArg("BACKDROP", None)
        ]
    }
    (*
    O.whenSensorGreaterThan, // function
    *)
    O.whenIReceive, {
        opcode = "event_whenbroadcastreceived" 
        category = "event"
        argMap = [
            FieldArg("BROADCAST_OPTION", Some VariableType.BroadcastMessage)
        ]
    }
    O.``broadcast:``, {
        opcode = "event_broadcast" 
        category = "event"
        argMap = [
            InputArg("event_broadcast_menu", "BROADCAST_INPUT", Some VariableType.BroadcastMessage)
        ]
    }
    O.doBroadcastAndWait, {
        opcode = "event_broadcastandwait" 
        category = "event"
        argMap = [
            InputArg("event_broadcast_menu", "BROADCAST_INPUT", Some VariableType.BroadcastMessage)
        ]
    }
    O.``wait:elapsed:from:``, {
        opcode = "control_wait" 
        category = "control"
        argMap = [
            InputArg("math_positive_number", "DURATION", None)
        ]
    }
    O.doRepeat, {
        opcode = "control_repeat" 
        category = "control"
        argMap = [
            InputArg("math_whole_number", "TIMES", None)
            InputArg("substack", "SUBSTACK", None)
        ]
    }
    O.doForever, {
        opcode = "control_forever" 
        category = "control"
        argMap = [
            InputArg("substack", "SUBSTACK", None)
        ]
    }
    O.doIf, {
        opcode = "control_if" 
        category = "control"
        argMap = [
            InputArg("boolean", "CONDITION", None)
            InputArg("substack", "SUBSTACK", None)
        ]
    }
    O.doIfElse, {
        opcode = "control_if_else" 
        category = "control"
        argMap = [
            InputArg("boolean", "CONDITION", None)
            InputArg("substack", "SUBSTACK", None)
            InputArg("substack", "SUBSTACK2", None)
        ]
    }
    O.doWaitUntil, {
        opcode = "control_wait_until" 
        category = "control"
        argMap = [
            InputArg("boolean", "CONDITION", None)
        ]
    }
    O.doUntil, {
        opcode = "control_repeat_until" 
        category = "control"
        argMap = [
            InputArg("boolean", "CONDITION", None)
            InputArg("substack", "SUBSTACK", None)
        ]
    }
    O.doWhile, {
        opcode = "control_while" 
        category = "control"
        argMap = [
            InputArg("boolean", "CONDITION", None)
            InputArg("substack", "SUBSTACK", None)
        ]
    }
    (*
    O.doForLoop, {
        opcode = "control_for_each" 
        category = "control"
        argMap = [
            FieldArg("VARIABLE", None)
            InputArg("text", "VALUE", None)
            InputArg("substack", "SUBSTACK", None)
        ]
    }
    *)
    O.stopScripts, {
        opcode = "control_stop" 
        category = "control"
        argMap = [
            FieldArg("STOP_OPTION", None)
        ]
    }
    O.whenCloned, {
        opcode = "control_start_as_clone" 
        category = "control"
        argMap = [
        ]
    }
    O.createCloneOf, {
        opcode = "control_create_clone_of" 
        category = "control"
        argMap = [
            InputArg("control_create_clone_of_menu", "CLONE_OPTION", None)
        ]
    }
    O.deleteClone, {
        opcode = "control_delete_this_clone" 
        category = "control"
        argMap = [
        ]
    }
    (*
    O.COUNT, {
        opcode = "control_get_counter" 
        category = "control"
        argMap = [
        ]
    }
    *)
    (*
    O.INCR_COUNT, {
        opcode = "control_incr_counter" 
        category = "control"
        argMap = [
        ]
    }
    *)
    (*
    O.CLR_COUNT, {
        opcode = "control_clear_counter" 
        category = "control"
        argMap = [
        ]
    }
    *)
    O.warpSpeed, {
        opcode = "control_all_at_once" 
        category = "control"
        argMap = [
            InputArg("substack", "SUBSTACK", None)
        ]
    }
    O.``touching:``, {
        opcode = "sensing_touchingobject" 
        category = "sensing"
        argMap = [
            InputArg("sensing_touchingobjectmenu", "TOUCHINGOBJECTMENU", None)
        ]
    }
    O.``touchingColor:``, {
        opcode = "sensing_touchingcolor" 
        category = "sensing"
        argMap = [
            InputArg("colour_picker", "COLOR", None)
        ]
    }
    O.``color:sees:``, {
        opcode = "sensing_coloristouchingcolor" 
        category = "sensing"
        argMap = [
            InputArg("colour_picker", "COLOR", None)
            InputArg("colour_picker", "COLOR2", None)
        ]
    }
    O.``distanceTo:``, {
        opcode = "sensing_distanceto" 
        category = "sensing"
        argMap = [
            InputArg("sensing_distancetomenu", "DISTANCETOMENU", None)
        ]
    }
    O.doAsk, {
        opcode = "sensing_askandwait" 
        category = "sensing"
        argMap = [
            InputArg("text", "QUESTION", None)
        ]
    }
    O.answer, {
        opcode = "sensing_answer" 
        category = "sensing"
        argMap = [
        ]
    }
    O.``keyPressed:``, {
        opcode = "sensing_keypressed" 
        category = "sensing"
        argMap = [
            InputArg("sensing_keyoptions", "KEY_OPTION", None)
        ]
    }
    O.mousePressed, {
        opcode = "sensing_mousedown" 
        category = "sensing"
        argMap = [
        ]
    }
    O.mouseX, {
        opcode = "sensing_mousex" 
        category = "sensing"
        argMap = [
        ]
    }
    O.mouseY, {
        opcode = "sensing_mousey" 
        category = "sensing"
        argMap = [
        ]
    }
    (*
    O.soundLevel, {
        opcode = "sensing_loudness" 
        category = "sensing"
        argMap = [
        ]
    }
    *)
    (*
    O.isLoud, {
        opcode = "sensing_loud" 
        category = "sensing"
        argMap = [
        ]
    }
    *)
    (*
    O.setVideoState, {
        opcode = "videoSensing_videoToggle" 
        category = "videoSensing"
        argMap = [
            InputArg("videoSensing_menu_VIDEO_STATE", "VIDEO_STATE", None)
        ]
    }
    *)
    (*
    O.setVideoTransparency, {
        opcode = "videoSensing_setVideoTransparency" 
        category = "videoSensing"
        argMap = [
            InputArg("math_number", "TRANSPARENCY", None)
        ]
    }
    *)
    O.timer, {
        opcode = "sensing_timer" 
        category = "sensing"
        argMap = [
        ]
    }
    O.timerReset, {
        opcode = "sensing_resettimer" 
        category = "sensing"
        argMap = [
        ]
    }
    O.``getAttribute:of:``, {
        opcode = "sensing_of" 
        category = "sensing"
        argMap = [
            FieldArg("PROPERTY", None)
            InputArg("sensing_of_object_menu", "OBJECT", None)
        ]
    }
    O.timeAndDate, {
        opcode = "sensing_current" 
        category = "sensing"
        argMap = [
            FieldArg("CURRENTMENU", None)
        ]
    }
    O.timestamp, {
        opcode = "sensing_dayssince2000" 
        category = "sensing"
        argMap = [
        ]
    }
    O.getUserName, {
        opcode = "sensing_username" 
        category = "sensing"
        argMap = [
        ]
    }
    O.getUserId, {
        opcode = "sensing_userid" 
        category = "sensing"
        argMap = [
        ]
    }
    O.``+``, {
        opcode = "operator_add" 
        category = "operator"
        argMap = [
            InputArg("math_number", "NUM1", None)
            InputArg("math_number", "NUM2", None)
        ]
    }
    O.``-``, {
        opcode = "operator_subtract" 
        category = "operator"
        argMap = [
            InputArg("math_number", "NUM1", None)
            InputArg("math_number", "NUM2", None)
        ]
    }
    O.``*``, {
        opcode = "operator_multiply" 
        category = "operator"
        argMap = [
            InputArg("math_number", "NUM1", None)
            InputArg("math_number", "NUM2", None)
        ]
    }
    O.``/``, {
        opcode = "operator_divide" 
        category = "operator"
        argMap = [
            InputArg("math_number", "NUM1", None)
            InputArg("math_number", "NUM2", None)
        ]
    }
    O.``randomFrom:to:``, {
        opcode = "operator_random" 
        category = "operator"
        argMap = [
            InputArg("math_number", "FROM", None)
            InputArg("math_number", "TO", None)
        ]
    }
    O.``<``, {
        opcode = "operator_lt" 
        category = "operator"
        argMap = [
            InputArg("text", "OPERAND1", None)
            InputArg("text", "OPERAND2", None)
        ]
    }
    O.``=``, {
        opcode = "operator_equals" 
        category = "operator"
        argMap = [
            InputArg("text", "OPERAND1", None)
            InputArg("text", "OPERAND2", None)
        ]
    }
    O.``>``, {
        opcode = "operator_gt" 
        category = "operator"
        argMap = [
            InputArg("text", "OPERAND1", None)
            InputArg("text", "OPERAND2", None)
        ]
    }
    O.``&``, {
        opcode = "operator_and" 
        category = "operator"
        argMap = [
            InputArg("boolean", "OPERAND1", None)
            InputArg("boolean", "OPERAND2", None)
        ]
    }
    O.``|``, {
        opcode = "operator_or" 
        category = "operator"
        argMap = [
            InputArg("boolean", "OPERAND1", None)
            InputArg("boolean", "OPERAND2", None)
        ]
    }
    O.not, {
        opcode = "operator_not" 
        category = "operator"
        argMap = [
            InputArg("boolean", "OPERAND", None)
        ]
    }
    O.``concatenate:with:``, {
        opcode = "operator_join" 
        category = "operator"
        argMap = [
            InputArg("text", "STRING1", None)
            InputArg("text", "STRING2", None)
        ]
    }
    O.``letter:of:``, {
        opcode = "operator_letter_of" 
        category = "operator"
        argMap = [
            InputArg("math_whole_number", "LETTER", None)
            InputArg("text", "STRING", None)
        ]
    }
    O.``stringLength:``, {
        opcode = "operator_length" 
        category = "operator"
        argMap = [
            InputArg("text", "STRING", None)
        ]
    }
    O.``%``, {
        opcode = "operator_mod" 
        category = "operator"
        argMap = [
            InputArg("math_number", "NUM1", None)
            InputArg("math_number", "NUM2", None)
        ]
    }
    O.rounded, {
        opcode = "operator_round" 
        category = "operator"
        argMap = [
            InputArg("math_number", "NUM", None)
        ]
    }
    O.``computeFunction:of:``, {
        opcode = "operator_mathop" 
        category = "operator"
        argMap = [
            FieldArg("OPERATOR", None)
            InputArg("math_number", "NUM", None)
        ]
    }
    O.readVariable, {
        opcode = "data_variable" 
        category = "data"
        argMap = [
            FieldArg("VARIABLE", Some VariableType.Scalar)
        ]
    }
    (*
    O.``getVar:``, {
        opcode = "data_variable" 
        category = "data"
        argMap = [
            FieldArg("VARIABLE", Some VariableType.Scalar)
        ]
    }
    *)
    O.``setVar:to:``, {
        opcode = "data_setvariableto" 
        category = "data"
        argMap = [
            FieldArg("VARIABLE", Some VariableType.Scalar)
            InputArg("text", "VALUE", None)
        ]
    }
    O.``changeVar:by:``, {
        opcode = "data_changevariableby" 
        category = "data"
        argMap = [
            FieldArg("VARIABLE", Some VariableType.Scalar)
            InputArg("math_number", "VALUE", None)
        ]
    }
    O.``showVariable:``, {
        opcode = "data_showvariable" 
        category = "data"
        argMap = [
            FieldArg("VARIABLE", Some VariableType.Scalar)
        ]
    }
    O.``hideVariable:``, {
        opcode = "data_hidevariable" 
        category = "data"
        argMap = [
            FieldArg("VARIABLE", Some VariableType.Scalar)
        ]
    }
    O.``contentsOfList:``, {
        opcode = "data_listcontents" 
        category = "data"
        argMap = [
            FieldArg("LIST", Some VariableType.List)
        ]
    }
    O.``append:toList:``, {
        opcode = "data_addtolist" 
        category = "data"
        argMap = [
            InputArg("text", "ITEM", None)
            FieldArg("LIST", Some VariableType.List)
        ]
    }
    O.``deleteLine:ofList:``, {
        opcode = "data_deleteoflist" 
        category = "data"
        argMap = [
            InputArg("math_integer", "INDEX", None)
            FieldArg("LIST", Some VariableType.List)
        ]
    }
    O.``insert:at:ofList:``, {
        opcode = "data_insertatlist" 
        category = "data"
        argMap = [
            InputArg("text", "ITEM", None)
            InputArg("math_integer", "INDEX", None)
            FieldArg("LIST", Some VariableType.List)
        ]
    }
    O.``setLine:ofList:to:``, {
        opcode = "data_replaceitemoflist" 
        category = "data"
        argMap = [
            InputArg("math_integer", "INDEX", None)
            FieldArg("LIST", Some VariableType.List)
            InputArg("text", "ITEM", None)
        ]
    }
    O.``getLine:ofList:``, {
        opcode = "data_itemoflist" 
        category = "data"
        argMap = [
            InputArg("math_integer", "INDEX", None)
            FieldArg("LIST", Some VariableType.List)
        ]
    }
    O.``lineCountOfList:``, {
        opcode = "data_lengthoflist" 
        category = "data"
        argMap = [
            FieldArg("LIST", Some VariableType.List)
        ]
    }
    O.``list:contains:``, {
        opcode = "data_listcontainsitem" 
        category = "data"
        argMap = [
            FieldArg("LIST", Some VariableType.List)
            InputArg("text", "ITEM", None)
        ]
    }
    O.``showList:``, {
        opcode = "data_showlist" 
        category = "data"
        argMap = [
            FieldArg("LIST", Some VariableType.List)
        ]
    }
    O.``hideList:``, {
        opcode = "data_hidelist" 
        category = "data"
        argMap = [
            FieldArg("LIST", Some VariableType.List)
        ]
    }
    (*
    O.procDef, {
        opcode = "procedures_definition" 
        category = "procedures"
        argMap = [
        ]
    }
    *)
    O.getParam, {
        opcode = "argument_reporter_string_number" 
        category = "argument"
        argMap = [
            FieldArg("VALUE", None)
        ]
    }
    O.call, {
        opcode = "procedures_call" 
        category = "procedures"
        argMap = [
        ]
    }
    (*
    O.``LEGO WeDo 2.0motorOnFor``, {
        opcode = "wedo2_motorOnFor" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_MOTOR_ID", "MOTOR_ID", None)
            InputArg("math_number", "DURATION", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0.motorOnFor``, {
        opcode = "wedo2_motorOnFor" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_MOTOR_ID", "MOTOR_ID", None)
            InputArg("math_number", "DURATION", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0motorOn``, {
        opcode = "wedo2_motorOn" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_MOTOR_ID", "MOTOR_ID", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0.motorOn``, {
        opcode = "wedo2_motorOn" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_MOTOR_ID", "MOTOR_ID", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0motorOff``, {
        opcode = "wedo2_motorOff" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_MOTOR_ID", "MOTOR_ID", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0.motorOff``, {
        opcode = "wedo2_motorOff" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_MOTOR_ID", "MOTOR_ID", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0startMotorPower``, {
        opcode = "wedo2_startMotorPower" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_MOTOR_ID", "MOTOR_ID", None)
            InputArg("math_number", "POWER", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0.startMotorPower``, {
        opcode = "wedo2_startMotorPower" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_MOTOR_ID", "MOTOR_ID", None)
            InputArg("math_number", "POWER", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0setMotorDirection``, {
        opcode = "wedo2_setMotorDirection" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_MOTOR_ID", "MOTOR_ID", None)
            InputArg("wedo2_menu_MOTOR_DIRECTION", "MOTOR_DIRECTION", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0.setMotorDirection``, {
        opcode = "wedo2_setMotorDirection" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_MOTOR_ID", "MOTOR_ID", None)
            InputArg("wedo2_menu_MOTOR_DIRECTION", "MOTOR_DIRECTION", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0setLED``, {
        opcode = "wedo2_setLightHue" 
        category = "wedo2"
        argMap = [
            InputArg("math_number", "HUE", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0.setLED``, {
        opcode = "wedo2_setLightHue" 
        category = "wedo2"
        argMap = [
            InputArg("math_number", "HUE", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0playNote``, {
        opcode = "wedo2_playNoteFor" 
        category = "wedo2"
        argMap = [
            InputArg("math_number", "NOTE", None)
            InputArg("math_number", "DURATION", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0.playNote``, {
        opcode = "wedo2_playNoteFor" 
        category = "wedo2"
        argMap = [
            InputArg("math_number", "NOTE", None)
            InputArg("math_number", "DURATION", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0whenDistance``, {
        opcode = "wedo2_whenDistance" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_OP", "OP", None)
            InputArg("math_number", "REFERENCE", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0.whenDistance``, {
        opcode = "wedo2_whenDistance" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_OP", "OP", None)
            InputArg("math_number", "REFERENCE", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0whenTilted``, {
        opcode = "wedo2_whenTilted" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_TILT_DIRECTION_ANY", "TILT_DIRECTION_ANY", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0.whenTilted``, {
        opcode = "wedo2_whenTilted" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_TILT_DIRECTION_ANY", "TILT_DIRECTION_ANY", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0getDistance``, {
        opcode = "wedo2_getDistance" 
        category = "wedo2"
        argMap = [
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0.getDistance``, {
        opcode = "wedo2_getDistance" 
        category = "wedo2"
        argMap = [
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0isTilted``, {
        opcode = "wedo2_isTilted" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_TILT_DIRECTION_ANY", "TILT_DIRECTION_ANY", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0.isTilted``, {
        opcode = "wedo2_isTilted" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_TILT_DIRECTION_ANY", "TILT_DIRECTION_ANY", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0getTilt``, {
        opcode = "wedo2_getTiltAngle" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_TILT_DIRECTION", "TILT_DIRECTION", None)
        ]
    }
    *)
    (*
    O.``LEGO WeDo 2.0.getTilt``, {
        opcode = "wedo2_getTiltAngle" 
        category = "wedo2"
        argMap = [
            InputArg("wedo2_menu_TILT_DIRECTION", "TILT_DIRECTION", None)
        ]
    }
    *)
|]
