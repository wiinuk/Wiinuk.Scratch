module Scratch.Serialization.Sb3.KnownExtensionSpecMap
open Scratch.Serialization.Sb3.OpCodeSpecs

let knownBlockSpecs = Map [|
    "pen_clear", {
        opcode = "pen_clear"
        category = "pen"
        argMap = [
            EmptyArg
        ]
    }
    "pen_stamp", {
        opcode = "pen_stamp"
        category = "pen"
        argMap = [
            EmptyArg
        ]
    }
    "pen_penDown", {
        opcode = "pen_penDown"
        category = "pen"
        argMap = [
            EmptyArg
        ]
    }
    "pen_penUp", {
        opcode = "pen_penUp"
        category = "pen"
        argMap = [
            EmptyArg
        ]
    }
    "pen_setPenColorToColor", {
        opcode = "pen_setPenColorToColor"
        category = "pen"
        argMap = [
            EmptyArg
            InputArg ("colour_picker", "COLOR", None)
        ]
    }
    "pen_changePenColorParamBy", {
        opcode = "pen_changePenColorParamBy"
        category = "pen"
        argMap = [
            EmptyArg
            InputArg ("pen_menu_colorParam", "COLOR_PARAM", None)
            InputArg ("math_number", "VALUE", None)
        ]
    }
    "pen_setPenColorParamTo", {
        opcode = "pen_setPenColorParamTo"
        category = "pen"
        argMap = [
            EmptyArg
            InputArg ("pen_menu_colorParam", "COLOR_PARAM", None)
            InputArg ("math_number", "VALUE", None)
        ]
    }
    "pen_changePenSizeBy", {
        opcode = "pen_changePenSizeBy"
        category = "pen"
        argMap = [
            EmptyArg
            InputArg ("math_number", "SIZE", None)
        ]
    }
    "pen_setPenSizeTo", {
        opcode = "pen_setPenSizeTo"
        category = "pen"
        argMap = [
            EmptyArg
            InputArg ("math_number", "SIZE", None)
        ]
    }
    "pen_setPenShadeToNumber", {
        opcode = "pen_setPenShadeToNumber"
        category = "pen"
        argMap = [
            EmptyArg
            InputArg ("math_number", "SHADE", None)
        ]
    }
    "pen_changePenShadeBy", {
        opcode = "pen_changePenShadeBy"
        category = "pen"
        argMap = [
            EmptyArg
            InputArg ("math_number", "SHADE", None)
        ]
    }
    "pen_setPenHueToNumber", {
        opcode = "pen_setPenHueToNumber"
        category = "pen"
        argMap = [
            EmptyArg
            InputArg ("math_number", "HUE", None)
        ]
    }
    "pen_changePenHueBy", {
        opcode = "pen_changePenHueBy"
        category = "pen"
        argMap = [
            EmptyArg
            InputArg ("math_number", "HUE", None)
        ]
    }
    "pen_menu_colorParam", {
        opcode = "pen_menu_colorParam"
        category = "pen"
        argMap = [
            FieldArg ("colorParam", None)
        ]
    }
    "wedo2_motorOnFor", {
        opcode = "wedo2_motorOnFor"
        category = "wedo2"
        argMap = [
            EmptyArg
            InputArg ("wedo2_menu_MOTOR_ID", "MOTOR_ID", None)
            InputArg ("math_number", "DURATION", None)
        ]
    }
    "wedo2_motorOn", {
        opcode = "wedo2_motorOn"
        category = "wedo2"
        argMap = [
            EmptyArg
            InputArg ("wedo2_menu_MOTOR_ID", "MOTOR_ID", None)
        ]
    }
    "wedo2_motorOff", {
        opcode = "wedo2_motorOff"
        category = "wedo2"
        argMap = [
            EmptyArg
            InputArg ("wedo2_menu_MOTOR_ID", "MOTOR_ID", None)
        ]
    }
    "wedo2_startMotorPower", {
        opcode = "wedo2_startMotorPower"
        category = "wedo2"
        argMap = [
            EmptyArg
            InputArg ("wedo2_menu_MOTOR_ID", "MOTOR_ID", None)
            InputArg ("math_number", "POWER", None)
        ]
    }
    "wedo2_setMotorDirection", {
        opcode = "wedo2_setMotorDirection"
        category = "wedo2"
        argMap = [
            EmptyArg
            InputArg ("wedo2_menu_MOTOR_ID", "MOTOR_ID", None)
            InputArg ("wedo2_menu_MOTOR_DIRECTION", "MOTOR_DIRECTION", None)
        ]
    }
    "wedo2_setLightHue", {
        opcode = "wedo2_setLightHue"
        category = "wedo2"
        argMap = [
            EmptyArg
            InputArg ("math_number", "HUE", None)
        ]
    }
    "wedo2_playNoteFor", {
        opcode = "wedo2_playNoteFor"
        category = "wedo2"
        argMap = [
            EmptyArg
            InputArg ("math_number", "NOTE", None)
            InputArg ("math_number", "DURATION", None)
        ]
    }
    "wedo2_whenDistance", {
        opcode = "wedo2_whenDistance"
        category = "wedo2"
        argMap = [
            EmptyArg
            InputArg ("wedo2_menu_OP", "OP", None)
            InputArg ("math_number", "REFERENCE", None)
        ]
    }
    "wedo2_whenTilted", {
        opcode = "wedo2_whenTilted"
        category = "wedo2"
        argMap = [
            EmptyArg
            InputArg ("wedo2_menu_TILT_DIRECTION_ANY", "TILT_DIRECTION_ANY", None)
        ]
    }
    "wedo2_getDistance", {
        opcode = "wedo2_getDistance"
        category = "wedo2"
        argMap = [
            EmptyArg
        ]
    }
    "wedo2_isTilted", {
        opcode = "wedo2_isTilted"
        category = "wedo2"
        argMap = [
            EmptyArg
            InputArg ("wedo2_menu_TILT_DIRECTION_ANY", "TILT_DIRECTION_ANY", None)
        ]
    }
    "wedo2_getTiltAngle", {
        opcode = "wedo2_getTiltAngle"
        category = "wedo2"
        argMap = [
            EmptyArg
            InputArg ("wedo2_menu_TILT_DIRECTION", "TILT_DIRECTION", None)
        ]
    }
    "wedo2_menu_MOTOR_ID", {
        opcode = "wedo2_menu_MOTOR_ID"
        category = "wedo2"
        argMap = [
            FieldArg ("MOTOR_ID", None)
        ]
    }
    "wedo2_menu_MOTOR_DIRECTION", {
        opcode = "wedo2_menu_MOTOR_DIRECTION"
        category = "wedo2"
        argMap = [
            FieldArg ("MOTOR_DIRECTION", None)
        ]
    }
    "wedo2_menu_TILT_DIRECTION", {
        opcode = "wedo2_menu_TILT_DIRECTION"
        category = "wedo2"
        argMap = [
            FieldArg ("TILT_DIRECTION", None)
        ]
    }
    "wedo2_menu_TILT_DIRECTION_ANY", {
        opcode = "wedo2_menu_TILT_DIRECTION_ANY"
        category = "wedo2"
        argMap = [
            FieldArg ("TILT_DIRECTION_ANY", None)
        ]
    }
    "wedo2_menu_OP", {
        opcode = "wedo2_menu_OP"
        category = "wedo2"
        argMap = [
            FieldArg ("OP", None)
        ]
    }
    "music_playDrumForBeats", {
        opcode = "music_playDrumForBeats"
        category = "music"
        argMap = [
            EmptyArg
            InputArg ("music_menu_DRUM", "DRUM", None)
            InputArg ("math_number", "BEATS", None)
        ]
    }
    "music_midiPlayDrumForBeats", {
        opcode = "music_midiPlayDrumForBeats"
        category = "music"
        argMap = [
            EmptyArg
            InputArg ("music_menu_DRUM", "DRUM", None)
            InputArg ("math_number", "BEATS", None)
        ]
    }
    "music_restForBeats", {
        opcode = "music_restForBeats"
        category = "music"
        argMap = [
            EmptyArg
            InputArg ("math_number", "BEATS", None)
        ]
    }
    "music_playNoteForBeats", {
        opcode = "music_playNoteForBeats"
        category = "music"
        argMap = [
            EmptyArg
            InputArg ("note", "NOTE", None)
            InputArg ("math_number", "BEATS", None)
        ]
    }
    "music_setInstrument", {
        opcode = "music_setInstrument"
        category = "music"
        argMap = [
            EmptyArg
            InputArg ("music_menu_INSTRUMENT", "INSTRUMENT", None)
        ]
    }
    "music_midiSetInstrument", {
        opcode = "music_midiSetInstrument"
        category = "music"
        argMap = [
            EmptyArg
            InputArg ("math_number", "INSTRUMENT", None)
        ]
    }
    "music_setTempo", {
        opcode = "music_setTempo"
        category = "music"
        argMap = [
            EmptyArg
            InputArg ("math_number", "TEMPO", None)
        ]
    }
    "music_changeTempo", {
        opcode = "music_changeTempo"
        category = "music"
        argMap = [
            EmptyArg
            InputArg ("math_number", "TEMPO", None)
        ]
    }
    "music_getTempo", {
        opcode = "music_getTempo"
        category = "music"
        argMap = [
            EmptyArg
        ]
    }
    "music_menu_DRUM", {
        opcode = "music_menu_DRUM"
        category = "music"
        argMap = [
            FieldArg ("DRUM", None)
        ]
    }
    "music_menu_INSTRUMENT", {
        opcode = "music_menu_INSTRUMENT"
        category = "music"
        argMap = [
            FieldArg ("INSTRUMENT", None)
        ]
    }
    "microbit_whenButtonPressed", {
        opcode = "microbit_whenButtonPressed"
        category = "microbit"
        argMap = [
            EmptyArg
            InputArg ("microbit_menu_buttons", "BTN", None)
        ]
    }
    "microbit_isButtonPressed", {
        opcode = "microbit_isButtonPressed"
        category = "microbit"
        argMap = [
            EmptyArg
            InputArg ("microbit_menu_buttons", "BTN", None)
        ]
    }
    "microbit_whenGesture", {
        opcode = "microbit_whenGesture"
        category = "microbit"
        argMap = [
            EmptyArg
            InputArg ("microbit_menu_gestures", "GESTURE", None)
        ]
    }
    "microbit_displaySymbol", {
        opcode = "microbit_displaySymbol"
        category = "microbit"
        argMap = [
            EmptyArg
            InputArg ("matrix", "MATRIX", None)
        ]
    }
    "microbit_displayText", {
        opcode = "microbit_displayText"
        category = "microbit"
        argMap = [
            EmptyArg
            InputArg ("text", "TEXT", None)
        ]
    }
    "microbit_displayClear", {
        opcode = "microbit_displayClear"
        category = "microbit"
        argMap = [
            EmptyArg
        ]
    }
    "microbit_whenTilted", {
        opcode = "microbit_whenTilted"
        category = "microbit"
        argMap = [
            EmptyArg
            InputArg ("microbit_menu_tiltDirectionAny", "DIRECTION", None)
        ]
    }
    "microbit_isTilted", {
        opcode = "microbit_isTilted"
        category = "microbit"
        argMap = [
            EmptyArg
            InputArg ("microbit_menu_tiltDirectionAny", "DIRECTION", None)
        ]
    }
    "microbit_getTiltAngle", {
        opcode = "microbit_getTiltAngle"
        category = "microbit"
        argMap = [
            EmptyArg
            InputArg ("microbit_menu_tiltDirection", "DIRECTION", None)
        ]
    }
    "microbit_whenPinConnected", {
        opcode = "microbit_whenPinConnected"
        category = "microbit"
        argMap = [
            EmptyArg
            InputArg ("microbit_menu_touchPins", "PIN", None)
        ]
    }
    "microbit_menu_buttons", {
        opcode = "microbit_menu_buttons"
        category = "microbit"
        argMap = [
            FieldArg ("buttons", None)
        ]
    }
    "microbit_menu_gestures", {
        opcode = "microbit_menu_gestures"
        category = "microbit"
        argMap = [
            FieldArg ("gestures", None)
        ]
    }
    "microbit_menu_pinState", {
        opcode = "microbit_menu_pinState"
        category = "microbit"
        argMap = [
            FieldArg ("pinState", None)
        ]
    }
    "microbit_menu_tiltDirection", {
        opcode = "microbit_menu_tiltDirection"
        category = "microbit"
        argMap = [
            FieldArg ("tiltDirection", None)
        ]
    }
    "microbit_menu_tiltDirectionAny", {
        opcode = "microbit_menu_tiltDirectionAny"
        category = "microbit"
        argMap = [
            FieldArg ("tiltDirectionAny", None)
        ]
    }
    "microbit_menu_touchPins", {
        opcode = "microbit_menu_touchPins"
        category = "microbit"
        argMap = [
            FieldArg ("touchPins", None)
        ]
    }
    "text2speech_speakAndWait", {
        opcode = "text2speech_speakAndWait"
        category = "text2speech"
        argMap = [
            EmptyArg
            InputArg ("text", "WORDS", None)
        ]
    }
    "text2speech_setVoice", {
        opcode = "text2speech_setVoice"
        category = "text2speech"
        argMap = [
            EmptyArg
            InputArg ("text2speech_menu_voices", "VOICE", None)
        ]
    }
    "text2speech_setLanguage", {
        opcode = "text2speech_setLanguage"
        category = "text2speech"
        argMap = [
            EmptyArg
            InputArg ("text2speech_menu_languages", "LANGUAGE", None)
        ]
    }
    "text2speech_menu_voices", {
        opcode = "text2speech_menu_voices"
        category = "text2speech"
        argMap = [
            FieldArg ("voices", None)
        ]
    }
    "text2speech_menu_languages", {
        opcode = "text2speech_menu_languages"
        category = "text2speech"
        argMap = [
            FieldArg ("languages", None)
        ]
    }
    "translate_getTranslate", {
        opcode = "translate_getTranslate"
        category = "translate"
        argMap = [
            EmptyArg
            InputArg ("text", "WORDS", None)
            InputArg ("translate_menu_languages", "LANGUAGE", None)
        ]
    }
    "translate_getViewerLanguage", {
        opcode = "translate_getViewerLanguage"
        category = "translate"
        argMap = [
            EmptyArg
        ]
    }
    "translate_menu_languages", {
        opcode = "translate_menu_languages"
        category = "translate"
        argMap = [
            FieldArg ("languages", None)
        ]
    }
    "videoSensing_whenMotionGreaterThan", {
        opcode = "videoSensing_whenMotionGreaterThan"
        category = "videoSensing"
        argMap = [
            EmptyArg
            InputArg ("math_number", "REFERENCE", None)
        ]
    }
    "videoSensing_videoOn", {
        opcode = "videoSensing_videoOn"
        category = "videoSensing"
        argMap = [
            EmptyArg
            InputArg ("videoSensing_menu_ATTRIBUTE", "ATTRIBUTE", None)
            InputArg ("videoSensing_menu_SUBJECT", "SUBJECT", None)
        ]
    }
    "videoSensing_videoToggle", {
        opcode = "videoSensing_videoToggle"
        category = "videoSensing"
        argMap = [
            EmptyArg
            InputArg ("videoSensing_menu_VIDEO_STATE", "VIDEO_STATE", None)
        ]
    }
    "videoSensing_setVideoTransparency", {
        opcode = "videoSensing_setVideoTransparency"
        category = "videoSensing"
        argMap = [
            EmptyArg
            InputArg ("math_number", "TRANSPARENCY", None)
        ]
    }
    "videoSensing_menu_ATTRIBUTE", {
        opcode = "videoSensing_menu_ATTRIBUTE"
        category = "videoSensing"
        argMap = [
            FieldArg ("ATTRIBUTE", None)
        ]
    }
    "videoSensing_menu_SUBJECT", {
        opcode = "videoSensing_menu_SUBJECT"
        category = "videoSensing"
        argMap = [
            FieldArg ("SUBJECT", None)
        ]
    }
    "videoSensing_menu_VIDEO_STATE", {
        opcode = "videoSensing_menu_VIDEO_STATE"
        category = "videoSensing"
        argMap = [
            FieldArg ("VIDEO_STATE", None)
        ]
    }
    "ev3_motorTurnClockwise", {
        opcode = "ev3_motorTurnClockwise"
        category = "ev3"
        argMap = [
            EmptyArg
            InputArg ("ev3_menu_motorPorts", "PORT", None)
            InputArg ("math_number", "TIME", None)
        ]
    }
    "ev3_motorTurnCounterClockwise", {
        opcode = "ev3_motorTurnCounterClockwise"
        category = "ev3"
        argMap = [
            EmptyArg
            InputArg ("ev3_menu_motorPorts", "PORT", None)
            InputArg ("math_number", "TIME", None)
        ]
    }
    "ev3_motorSetPower", {
        opcode = "ev3_motorSetPower"
        category = "ev3"
        argMap = [
            EmptyArg
            InputArg ("ev3_menu_motorPorts", "PORT", None)
            InputArg ("math_number", "POWER", None)
        ]
    }
    "ev3_getMotorPosition", {
        opcode = "ev3_getMotorPosition"
        category = "ev3"
        argMap = [
            EmptyArg
            InputArg ("ev3_menu_motorPorts", "PORT", None)
        ]
    }
    "ev3_whenButtonPressed", {
        opcode = "ev3_whenButtonPressed"
        category = "ev3"
        argMap = [
            EmptyArg
            InputArg ("ev3_menu_sensorPorts", "PORT", None)
        ]
    }
    "ev3_whenDistanceLessThan", {
        opcode = "ev3_whenDistanceLessThan"
        category = "ev3"
        argMap = [
            EmptyArg
            InputArg ("math_number", "DISTANCE", None)
        ]
    }
    "ev3_whenBrightnessLessThan", {
        opcode = "ev3_whenBrightnessLessThan"
        category = "ev3"
        argMap = [
            EmptyArg
            InputArg ("math_number", "DISTANCE", None)
        ]
    }
    "ev3_buttonPressed", {
        opcode = "ev3_buttonPressed"
        category = "ev3"
        argMap = [
            EmptyArg
            InputArg ("ev3_menu_sensorPorts", "PORT", None)
        ]
    }
    "ev3_getDistance", {
        opcode = "ev3_getDistance"
        category = "ev3"
        argMap = [
            EmptyArg
        ]
    }
    "ev3_getBrightness", {
        opcode = "ev3_getBrightness"
        category = "ev3"
        argMap = [
            EmptyArg
        ]
    }
    "ev3_beep", {
        opcode = "ev3_beep"
        category = "ev3"
        argMap = [
            EmptyArg
            InputArg ("note", "NOTE", None)
            InputArg ("math_number", "TIME", None)
        ]
    }
    "ev3_menu_motorPorts", {
        opcode = "ev3_menu_motorPorts"
        category = "ev3"
        argMap = [
            FieldArg ("motorPorts", None)
        ]
    }
    "ev3_menu_sensorPorts", {
        opcode = "ev3_menu_sensorPorts"
        category = "ev3"
        argMap = [
            FieldArg ("sensorPorts", None)
        ]
    }
    "makeymakey_whenMakeyKeyPressed", {
        opcode = "makeymakey_whenMakeyKeyPressed"
        category = "makeymakey"
        argMap = [
            EmptyArg
            InputArg ("makeymakey_menu_KEY", "KEY", None)
        ]
    }
    "makeymakey_whenCodePressed", {
        opcode = "makeymakey_whenCodePressed"
        category = "makeymakey"
        argMap = [
            EmptyArg
            InputArg ("makeymakey_menu_SEQUENCE", "SEQUENCE", None)
        ]
    }
    "makeymakey_menu_KEY", {
        opcode = "makeymakey_menu_KEY"
        category = "makeymakey"
        argMap = [
            FieldArg ("KEY", None)
        ]
    }
    "makeymakey_menu_SEQUENCE", {
        opcode = "makeymakey_menu_SEQUENCE"
        category = "makeymakey"
        argMap = [
            FieldArg ("SEQUENCE", None)
        ]
    }
    "boost_motorOnFor", {
        opcode = "boost_motorOnFor"
        category = "boost"
        argMap = [
            EmptyArg
            InputArg ("boost_menu_MOTOR_ID", "MOTOR_ID", None)
            InputArg ("math_number", "DURATION", None)
        ]
    }
    "boost_motorOnForRotation", {
        opcode = "boost_motorOnForRotation"
        category = "boost"
        argMap = [
            EmptyArg
            InputArg ("boost_menu_MOTOR_ID", "MOTOR_ID", None)
            InputArg ("math_number", "ROTATION", None)
        ]
    }
    "boost_motorOn", {
        opcode = "boost_motorOn"
        category = "boost"
        argMap = [
            EmptyArg
            InputArg ("boost_menu_MOTOR_ID", "MOTOR_ID", None)
        ]
    }
    "boost_motorOff", {
        opcode = "boost_motorOff"
        category = "boost"
        argMap = [
            EmptyArg
            InputArg ("boost_menu_MOTOR_ID", "MOTOR_ID", None)
        ]
    }
    "boost_setMotorPower", {
        opcode = "boost_setMotorPower"
        category = "boost"
        argMap = [
            EmptyArg
            InputArg ("boost_menu_MOTOR_ID", "MOTOR_ID", None)
            InputArg ("math_number", "POWER", None)
        ]
    }
    "boost_setMotorDirection", {
        opcode = "boost_setMotorDirection"
        category = "boost"
        argMap = [
            EmptyArg
            InputArg ("boost_menu_MOTOR_ID", "MOTOR_ID", None)
            InputArg ("boost_menu_MOTOR_DIRECTION", "MOTOR_DIRECTION", None)
        ]
    }
    "boost_getMotorPosition", {
        opcode = "boost_getMotorPosition"
        category = "boost"
        argMap = [
            EmptyArg
            InputArg ("boost_menu_MOTOR_REPORTER_ID", "MOTOR_REPORTER_ID", None)
        ]
    }
    "boost_whenColor", {
        opcode = "boost_whenColor"
        category = "boost"
        argMap = [
            EmptyArg
            InputArg ("boost_menu_COLOR", "COLOR", None)
        ]
    }
    "boost_seeingColor", {
        opcode = "boost_seeingColor"
        category = "boost"
        argMap = [
            EmptyArg
            InputArg ("boost_menu_COLOR", "COLOR", None)
        ]
    }
    "boost_whenTilted", {
        opcode = "boost_whenTilted"
        category = "boost"
        argMap = [
            EmptyArg
            InputArg ("boost_menu_TILT_DIRECTION_ANY", "TILT_DIRECTION_ANY", None)
        ]
    }
    "boost_getTiltAngle", {
        opcode = "boost_getTiltAngle"
        category = "boost"
        argMap = [
            EmptyArg
            InputArg ("boost_menu_TILT_DIRECTION", "TILT_DIRECTION", None)
        ]
    }
    "boost_setLightHue", {
        opcode = "boost_setLightHue"
        category = "boost"
        argMap = [
            EmptyArg
            InputArg ("math_number", "HUE", None)
        ]
    }
    "boost_menu_MOTOR_ID", {
        opcode = "boost_menu_MOTOR_ID"
        category = "boost"
        argMap = [
            FieldArg ("MOTOR_ID", None)
        ]
    }
    "boost_menu_MOTOR_REPORTER_ID", {
        opcode = "boost_menu_MOTOR_REPORTER_ID"
        category = "boost"
        argMap = [
            FieldArg ("MOTOR_REPORTER_ID", None)
        ]
    }
    "boost_menu_MOTOR_DIRECTION", {
        opcode = "boost_menu_MOTOR_DIRECTION"
        category = "boost"
        argMap = [
            FieldArg ("MOTOR_DIRECTION", None)
        ]
    }
    "boost_menu_TILT_DIRECTION", {
        opcode = "boost_menu_TILT_DIRECTION"
        category = "boost"
        argMap = [
            FieldArg ("TILT_DIRECTION", None)
        ]
    }
    "boost_menu_TILT_DIRECTION_ANY", {
        opcode = "boost_menu_TILT_DIRECTION_ANY"
        category = "boost"
        argMap = [
            FieldArg ("TILT_DIRECTION_ANY", None)
        ]
    }
    "boost_menu_COLOR", {
        opcode = "boost_menu_COLOR"
        category = "boost"
        argMap = [
            FieldArg ("COLOR", None)
        ]
    }
    "gdxfor_whenGesture", {
        opcode = "gdxfor_whenGesture"
        category = "gdxfor"
        argMap = [
            EmptyArg
            InputArg ("gdxfor_menu_gestureOptions", "GESTURE", None)
        ]
    }
    "gdxfor_whenForcePushedOrPulled", {
        opcode = "gdxfor_whenForcePushedOrPulled"
        category = "gdxfor"
        argMap = [
            EmptyArg
            InputArg ("gdxfor_menu_pushPullOptions", "PUSH_PULL", None)
        ]
    }
    "gdxfor_getForce", {
        opcode = "gdxfor_getForce"
        category = "gdxfor"
        argMap = [
            EmptyArg
        ]
    }
    "gdxfor_whenTilted", {
        opcode = "gdxfor_whenTilted"
        category = "gdxfor"
        argMap = [
            EmptyArg
            InputArg ("gdxfor_menu_tiltAnyOptions", "TILT", None)
        ]
    }
    "gdxfor_isTilted", {
        opcode = "gdxfor_isTilted"
        category = "gdxfor"
        argMap = [
            EmptyArg
            InputArg ("gdxfor_menu_tiltAnyOptions", "TILT", None)
        ]
    }
    "gdxfor_getTilt", {
        opcode = "gdxfor_getTilt"
        category = "gdxfor"
        argMap = [
            EmptyArg
            InputArg ("gdxfor_menu_tiltOptions", "TILT", None)
        ]
    }
    "gdxfor_isFreeFalling", {
        opcode = "gdxfor_isFreeFalling"
        category = "gdxfor"
        argMap = [
            EmptyArg
        ]
    }
    "gdxfor_getSpinSpeed", {
        opcode = "gdxfor_getSpinSpeed"
        category = "gdxfor"
        argMap = [
            EmptyArg
            InputArg ("gdxfor_menu_axisOptions", "DIRECTION", None)
        ]
    }
    "gdxfor_getAcceleration", {
        opcode = "gdxfor_getAcceleration"
        category = "gdxfor"
        argMap = [
            EmptyArg
            InputArg ("gdxfor_menu_axisOptions", "DIRECTION", None)
        ]
    }
    "gdxfor_menu_pushPullOptions", {
        opcode = "gdxfor_menu_pushPullOptions"
        category = "gdxfor"
        argMap = [
            FieldArg ("pushPullOptions", None)
        ]
    }
    "gdxfor_menu_gestureOptions", {
        opcode = "gdxfor_menu_gestureOptions"
        category = "gdxfor"
        argMap = [
            FieldArg ("gestureOptions", None)
        ]
    }
    "gdxfor_menu_axisOptions", {
        opcode = "gdxfor_menu_axisOptions"
        category = "gdxfor"
        argMap = [
            FieldArg ("axisOptions", None)
        ]
    }
    "gdxfor_menu_tiltOptions", {
        opcode = "gdxfor_menu_tiltOptions"
        category = "gdxfor"
        argMap = [
            FieldArg ("tiltOptions", None)
        ]
    }
    "gdxfor_menu_tiltAnyOptions", {
        opcode = "gdxfor_menu_tiltAnyOptions"
        category = "gdxfor"
        argMap = [
            FieldArg ("tiltAnyOptions", None)
        ]
    }
|]
