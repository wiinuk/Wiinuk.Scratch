module Scratch.Evaluator.ObjectState
open Scratch
open Scratch.Ast
open Scratch.Primitives
open System


// type KnownListenerHeader<S, B> =
//     | Case0<"whenClicked">
//     | Case1<"whenKeyPressed", "any" | S>
//     | Case4<"whenSceneStarts", S, null, null, B>

let ofEntityData makeSpriteState data =
    let values =
        data.variables
        |> List.fold (fun xs v ->
            match v.isPersistent with
            | Persistent -> xs
            | NoPersistent ->
                if Map.containsKey v.name xs then failwith $"duplicated variable name '{v.name}'"
                Map.add v.name (ref v.value) xs
        ) Map.empty

    let lists =
        data.lists
        |> List.fold (fun xs ({ contents' = contents' } & v) ->
            match v.isPersistent with Persistent -> failwith $"persistent list '{v.listName}'" | NoPersistent -> ()
            if Map.containsKey v.listName values || Map.containsKey v.listName xs then failwith $"duplicated list name '{v.listName}'"
            Map.add v.listName (IArray.toResizeArray contents') xs
        ) Map.empty

    let procs =
        data.scripts
        |> List.fold (fun xs s ->
            match s.script with
            | Procedure(ProcedureDefinition(name = name) as x) ->
                if Map.containsKey name xs then failwith $"duplicated procedure name '{name}'"
                Map.add name x xs
            | _ -> xs
        ) Map.empty

    let whenGreenFlag =
        data.scripts
        |> Seq.choose (fun s ->
            match s.script with
            | Listener(ListenerDefinition(name = O.whenGreenFlag) as x) -> Some x
            | _ -> None
        )
        |> Seq.toList

    let whenIReceive =
        data.scripts
        |> Seq.choose (fun s ->
            match s.script with
            | Listener(ListenerDefinition(name = O.whenIReceive; arguments = Literal(_, SString s)::_) as x) -> Some(s.ToLowerInvariant(), x)
            | _ -> None
        )
        |> Seq.groupBy fst
        |> Seq.map (fun (k, vs) -> k, vs |> Seq.map snd |> Seq.toList)
        |> Map.ofSeq

    let costumes = IArray.ofSeq data.costumes

    let costumeNameToIndex =
        data.costumes
        |> Seq.mapi (fun index c -> c.costumeName, index)
        |> Map.ofSeq

    let sounds = IArray.ofSeq data.sounds

    let soundNameToIndex =
        data.sounds
        |> Seq.mapi (fun index c -> c.soundName, index)
        |> Map.ofSeq

    let shared = {
        objectName = data.objName

        procs = procs
        whenGreenFlag = whenGreenFlag
        whenIReceive = whenIReceive

        costumes = costumes
        costumeNameToIndex = costumeNameToIndex

        sounds = sounds
        soundNameToIndex = soundNameToIndex
    }
    {
        isClone = false
        values = values
        lists = lists
        sprite = makeSpriteState data
        drawingData = {
            currentCostumeIndex = int data.currentCostumeIndex
            filters = {
                whirl = 0.
                fisheye = 0.
                brightness = 0.
                pixelate = 0.
                mosaic = 0.
                color = 0.
                ghost = 0.
                others = Map.empty
            }
            volume = 100.
        }
        shared = shared
    }

let findObject name state =
    match Map.tryFind name state.originalSpriteMap with
    | ValueSome _ as r -> r
    | ValueNone ->

    let stage = state.stage
    match name with
    | "_stage_" | _ when name = stage.shared.objectName -> ValueSome stage
    | _ -> ValueNone

let cloneSprite sprite parent = {
    drawingData = parent.drawingData

    sprite = ValueSome {
        whenCloned = sprite.whenCloned
        spriteDrawingData = sprite.spriteDrawingData
        sayVersion = Version.Version 0
    }

    isClone = true

    values = parent.values |> Map.map (fun _ x -> { contents = x.contents })
    lists = parent.lists |> Map.map (fun _ xs -> ResizeArray xs)

    shared = parent.shared

    //s2.costumes = s1.costumes;
    //s2.currentCostumeIndex = s1.currentCostumeIndex;
    //s2.soundRefs = s1.soundRefs;
    //s2.sounds = s1.sounds;
    //s2.listeners = s1.listeners;
    //s2.fns = s1.fns;
    //s2.scripts = s1.scripts;

    //const fs1 = s1.filters
    //const fs2 = s2.filters
    //fs2.color = fs1.color
    //fs2.fisheye = fs1.fisheye
    //fs2.whirl = fs1.whirl
    //fs2.pixelate = fs1.pixelate
    //fs2.mosaic = fs1.mosaic
    //fs2.brightness = fs1.brightness
    //fs2.ghost = fs1.ghost

    //s2.direction = s1.direction;
    //s2.indexInLibrary = s1.indexInLibrary;
    //s2.isDraggable = s1.isDraggable;
    //s2.rotationStyle = s1.rotationStyle;
    //s2.scale = s1.scale;
    //s2.volume = s1.volume;
    //s2.visible = s1.visible;
    //s2.penColor = s1.penColor;

    //// Pen color in RGB mode?
    //s2.penRGBA = s1.penRGBA;
    //// Pen color in RGBA
    //s2.penRed = s1.penRed;
    //s2.penGreen = s1.penGreen;
    //s2.penBlue = s1.penBlue;
    //s2.penAlpha = s1.penAlpha;
    ////Pen color in HSL
    //s2.penHue = s1.penHue;
    //s2.penSaturation = s1.penSaturation;
    //s2.penLightness = s1.penLightness;

    //s2.penSize = s1.penSize;
    //s2.isPenDown = s1.isPenDown;
}

let moveTo sprite (x, y) =
    let sprite = &sprite.spriteDrawingData

    let ox = sprite.x
    let oy = sprite.y
    if ox = x && oy = y (* && not sprite.isPenDown *) then () else

    sprite.x <- x
    sprite.y <- y
    //if sprite.isPenDown && not sprite.isDragging then
    //    _movePen sprite (ox, oy) (x, y)

    //if sprite.saying then
    //    updateBubble sprite

let setVisibility self visible =
    match self.sprite with
    | ValueNone -> ()
    | ValueSome sprite -> sprite.spriteDrawingData.visible <- visible

let showNextCostume self =
    self.drawingData.currentCostumeIndex <- (self.drawingData.currentCostumeIndex + 1) % IArray.length self.shared.costumes

let showPreviousCostume self =
    let length = IArray.length self.shared.costumes
    self.drawingData.currentCostumeIndex <- (self.drawingData.currentCostumeIndex + length - 1) % length

let setCostumeFromIndex self costume =
    if Double.IsNaN costume then () else

    let count = IArray.length self.shared.costumes
    let i = (int costume - 1) % count
    let i = if i < 0 then i + count else i
    self.drawingData.currentCostumeIndex <- i

let setCostume self costume =
    match costume with
    | SBool _ -> ()
    | SNumber costume -> setCostumeFromIndex self costume
    | SString costume ->

    match Map.tryFind costume self.shared.costumeNameToIndex with
    | ValueSome i -> self.drawingData.currentCostumeIndex <- i
    | _ ->

    let isSprite = VOption.isSome self.sprite
    match costume, isSprite with
    | "next costume", true
    | "next backdrop", false -> showNextCostume self
    | "previous costume", true
    | "previous backdrop", false -> showPreviousCostume self
    | _ ->

    match SValue.tryParseSNumber costume with
    | ValueSome costume -> setCostumeFromIndex self costume
    | _ -> ()

let setDirection sprite degrees =
    let d = degrees % 360.
    let d = if d > 180. then d - 360. else d
    let d = if d <= -180. then d + 360. else d
    sprite.spriteDrawingData.direction <- d
