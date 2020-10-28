﻿namespace Scratch
open System
open Scratch.Primitives
open Scratch.Ast


[<Struct>]
type Version<'Phantom> = Version of int

[<Sealed; AbstractClass>]
type SayPhantom = class end
[<Sealed; AbstractClass>]
type AskPhantom = class end
type SayVersion = SayPhantom Version
type AskVersion = AskPhantom Version

type IStageView<'State> =
    abstract Added: sprite: 'State -> unit
    abstract Cloned: prototypeSprite: 'State * clonedSprite: 'State * insertedIndex: int -> unit
    abstract Moved: sprite: 'State * oldIndex: int * newIndex: int -> unit
    abstract Removed: cloneSprite: 'State * index: int -> unit

    abstract ClearCanvas: unit -> unit
    abstract StampToCanvas: entity: 'State -> unit

    abstract PenDown: sprite: 'State * isDown: bool -> unit
    abstract SetPenArgb: sprite: 'State * argb: struct(byte * byte * byte * byte) -> unit
    abstract PenSize: sprite: 'State * size: double -> unit

    abstract HideStageQuestion: unit -> unit
    abstract ShowStageQuestion: question: string -> unit
    abstract ShowInputBox: initialValue: string * onSubmit: #IFunc<string, HUnit> byref -> unit

    abstract PlaySound: sprite: 'State * index: int -> unit

    abstract VariableMonitorChanged: selfSprite: 'State * variableName: string * visibility: Visibility -> unit

    abstract QueryTouchingColor: selfSprite: 'State * argb: struct(byte * byte * byte * byte) -> bool

module StageView =
    let moved (v: 'V byref when 'V :> IStageView<_> and 'V : struct) sprite oldIndex newIndex = v.Moved(sprite, oldIndex, newIndex)
    let showInputBox (v: 'V byref when 'V :> IStageView<_> and 'V : struct) initialValue (onSubmit: _ byref) = v.ShowInputBox(initialValue, &onSubmit)
    let showStageQuestion (v: 'V byref when 'V :> IStageView<_> and 'V : struct) question = v.ShowStageQuestion question
    let hideStageQuestion (v: 'V byref when 'V :> IStageView<_> and 'V : struct) = v.HideStageQuestion()
    let setPenArgb (v: 'V byref when 'V :> IStageView<_> and 'V : struct) sprite argb = v.SetPenArgb(sprite, argb)
    let penSize (v: 'V byref when 'V :> IStageView<_> and 'V : struct) sprite size = v.PenSize(sprite, size)
    let removed (v: 'V byref when 'V :> IStageView<_> and 'V : struct) cloneSprite index = v.Removed(cloneSprite, index)
    let cloned (v: 'V byref when 'V :> IStageView<_> and 'V : struct) prototypeSprite clonedSprite inseredIndex = v.Cloned(prototypeSprite, clonedSprite, inseredIndex)
    let added (v: 'V byref when 'V :> IStageView<_> and 'V : struct) sprite = v.Added sprite
    let playSound (v: 'V byref when 'V :> IStageView<_> and 'V : struct) sprite index = v.PlaySound(sprite, index)
    let variableMonitorChanged (v: 'V byref when 'V :> IStageView<_> and 'V : struct) selfSprite variableName visibility = v.VariableMonitorChanged(selfSprite, variableName, visibility)
    let queryTouchingColor (v: 'V byref when 'V :> IStageView<_> and 'V : struct) self argb = v.QueryTouchingColor(self, argb)
    let clearCanvas (v: 'V byref when 'V :> IStageView<_> and 'V : struct) = v.ClearCanvas()
    let stampToCanvas (v: 'V byref when 'V :> IStageView<_> and 'V : struct) entity = v.StampToCanvas entity
    let penDown (v: 'V byref when 'V :> IStageView<_> and 'V : struct) sprite isDown = v.PenDown(sprite, isDown)

    [<Struct; NoEquality; NoComparison>]
    type Ignore<'a> = | Ignore
    with
        interface IStageView<'a> with
            override _.Added(_) = ()
            override _.Cloned(_,_,_) = ()
            override _.Moved(_,_,_) = ()
            override _.Removed(_,_) = ()
            override _.ClearCanvas() = ()
            override _.StampToCanvas(_) = ()
            override _.PenDown(_,_) = ()
            override _.SetPenArgb(_,_) = ()
            override _.PenSize(_,_) = ()
            override _.ShowInputBox(initialValue, onSubmit) = let (HUnit) = onSubmit.Invoke initialValue in ()
            override _.HideStageQuestion() = ()
            override _.ShowStageQuestion(_) = ()
            override _.PlaySound(_,_) = ()
            override _.VariableMonitorChanged(_, _, _) = ()
            override _.QueryTouchingColor(_,_) = false

    let ignore = Ignore

    [<AbstractClass>]
    type IgnoreBase<'State>() =
        let mutable view = Ignore

        abstract Added: sprite: 'State -> unit
        default _.Added sprite = added &view sprite
        abstract Cloned: prototypeSprite: 'State * clonedSprite: 'State * insertedIndex: int -> unit
        default _.Cloned(prototypeSprite, clonedSprite, insertedIndex) = cloned &view prototypeSprite clonedSprite insertedIndex
        abstract Moved: sprite: 'State * oldIndex: int * newIndex: int -> unit
        default _.Moved(sprite, oldIndex, newIndex) = moved &view sprite oldIndex newIndex
        abstract Removed: cloneSprite: 'State * index: int -> unit
        default _.Removed(cloneSprite, index) = removed &view cloneSprite index

        abstract ClearCanvas: unit -> unit
        default _.ClearCanvas() = clearCanvas &view
        abstract StampToCanvas: entity: 'State -> unit
        default _.StampToCanvas entity = stampToCanvas &view entity

        abstract PenDown: sprite: 'State * isDown: bool -> unit
        default _.PenDown(sprite, isDown) = penDown &view sprite isDown
        abstract SetPenArgb: sprite: 'State * argb: struct(byte * byte * byte * byte) -> unit
        default _.SetPenArgb(sprite, argb) = setPenArgb &view sprite argb
        abstract PenSize: sprite: 'State * size: double -> unit
        default _.PenSize(sprite, size) = penSize &view sprite size

        abstract HideStageQuestion: unit -> unit
        default _.HideStageQuestion() = hideStageQuestion &view
        abstract ShowStageQuestion: question: string -> unit
        default _.ShowStageQuestion question = showStageQuestion &view question
        abstract ShowInputBox: initialValue: string * onSubmit: #IFunc<string, HUnit> byref -> unit
        default _.ShowInputBox(initialValue, onSubmit) = showInputBox &view initialValue &onSubmit

        abstract PlaySound: sprite: 'State * index: int -> unit
        default _.PlaySound(sprite, index) = playSound &view sprite index

        abstract VariableMonitorChanged: selfSprite: 'State * variableName: string * visibility: Visibility -> unit
        default _.VariableMonitorChanged(selfSprite, variableName, visibility) = variableMonitorChanged &view selfSprite variableName visibility

        abstract QueryTouchingColor: selfSprite: 'State * argb: struct(byte * byte * byte * byte) -> bool
        default _.QueryTouchingColor(selfSprite, argb) = queryTouchingColor &view selfSprite argb

        interface IStageView<'State> with
            member v.Added sprite = v.Added sprite
            member v.ClearCanvas() = v.ClearCanvas()
            member v.Cloned(prototypeSprite, clonedSprite, insertedIndex) = v.Cloned(prototypeSprite, clonedSprite, insertedIndex)
            member v.HideStageQuestion() = v.HideStageQuestion()
            member v.Moved(sprite, oldIndex, newIndex) = v.Moved(sprite, oldIndex, newIndex)
            member v.PenDown(sprite, isDown) = v.PenDown(sprite, isDown)
            member v.PenSize(sprite, size) = v.PenSize(sprite, size)
            member v.PlaySound(sprite, index) = v.PlaySound(sprite, index)
            member v.QueryTouchingColor(selfSprite, argb) = v.QueryTouchingColor(selfSprite, argb)
            member v.Removed(cloneSprite, index) = v.Removed(cloneSprite, index)
            member v.SetPenArgb(sprite, argb) = v.SetPenArgb(sprite, argb)
            member v.ShowInputBox(initialValue, onSubmit) = v.ShowInputBox(initialValue, &onSubmit)
            member v.ShowStageQuestion question = v.ShowStageQuestion question
            member v.StampToCanvas entity = v.StampToCanvas entity
            member v.VariableMonitorChanged(selfSprite, variableName, visibility) = v.VariableMonitorChanged(selfSprite, variableName, visibility)

    [<Struct; NoEquality; NoComparison>]
    type Poly<'a,'O> when 'O :> IStageView<'a> and 'O : not struct = private { view: 'O }
    with
        interface IStageView<'a> with
            override o.Added(s) = o.view.Added(s)
            override o.Cloned(s1, s2, i) = o.view.Cloned(s1, s2, i)
            override o.Moved(s, i1, i2) = o.view.Moved(s, i1, i2)
            override o.Removed(s, i) = o.view.Removed(s, i)
            override o.ClearCanvas() = o.view.ClearCanvas()
            override o.StampToCanvas(s) = o.view.StampToCanvas(s)

            override o.PenDown(s, d) = o.view.PenDown(s, d)
            override o.PenSize(s, x) = o.view.PenSize(s, x)
            override o.SetPenArgb(s, c) = o.view.SetPenArgb(s, c)
            override o.ShowInputBox(v, s) = o.view.ShowInputBox(v, &s)
            override o.ShowStageQuestion(q) = o.view.ShowStageQuestion(q)
            override o.HideStageQuestion() = o.view.HideStageQuestion()
            override o.PlaySound(s, i) = o.view.PlaySound(s, i)

            override o.VariableMonitorChanged(s, n, v) = o.view.VariableMonitorChanged(s, n, v)

            override o.QueryTouchingColor(s, argb) = o.view.QueryTouchingColor(s, argb)
    let poly view = { Poly.view = view }

type KeyCode =
    | None = 0

    /// ' '
    | Space = 32
    /// left arrow
    | Left = 37
    /// up arrow
    | Up = 38
    /// right arrow
    | Right = 39
    /// down arrow
    | Down = 40

    /// '-'
    | Minus = 45

    /// '0'
    | Key0 = 48
    /// '1'
    | Key1 = 49
    /// '2'
    | Key2 = 50
    /// '3'
    | Key3 = 51
    /// '4'
    | Key4 = 52
    /// '5'
    | Key5 = 53
    /// '9'
    | Key9 = 57

    /// ':'
    | Colon = 58
    /// '@'
    | At = 64

    /// 'A'
    | A = 65
    /// 'F'
    | F = 70
    /// 'I'
    | I = 73
    /// 'N'
    | N = 78
    /// 'S'
    | S = 83
    /// 'T'
    | T = 84
    /// 'Y'
    | Y = 89
    /// 'Z'
    | Z = 90

module KeyCode =
    let getKeyCodeFromNumber = function
        // char 48 = '0'
        // ...
        // char 57 = '9'
        //
        // char 58 = ':'
        // char 59 = ';'
        // char 60 = '<'
        // char 61 = '='
        // char 62 = '>'
        // char 63 = '?'
        // char 64 = '@'
        //
        // char 65 = 'A'
        // ...
        // char 90 = 'Z'
        | key when 48. <= key && key <= 90. -> int key

        // char 32 = ' ' => Key.Space
        // char 37 = '%' => Key.Left
        // char 38 = '&' => Key.Up
        // char 39 = '\'' => Key.Right
        // char 40 = '(' => Key.Down
        | 32.
        | 37.
        | 38.
        | 39.
        | 40. as key -> int key

        // string(keyCode).[0]
        | key when 0. <= key && key < 10. -> int '0' + int key 
        | key when key < 0. -> int '-'
        | key when Double.IsPositiveInfinity key -> int 'I'
        | key when Double.IsNaN key -> int 'N'
        | key ->

        // TODO:
        let rec first n = if n < 10. then n else first (n / 10.)
        int '0' + int (first (abs key))

    let getKeyCode = function
        | SBool true -> enum (int 'T')
        | SBool false -> enum (int 'F')
        | SNumber key -> enum (getKeyCodeFromNumber key)
        | SString key ->

        match key with
        | "space" -> KeyCode.Space
        | "left arrow" -> KeyCode.Left
        | "up arrow" -> KeyCode.Up
        | "right arrow" -> KeyCode.Right
        | "down arrow" -> KeyCode.Down
        | "" -> KeyCode.None
        | key ->

        match key.[0] with
        | ' ' -> KeyCode.Space
        | key -> enum (int (Char.ToUpperInvariant key))


type IInput =
    abstract IsAnyKeyDown: unit -> bool
    abstract IsKeyDown: code: KeyCode -> bool
    /// relative position,
    /// -240 <= x <= 240,
    /// -180 <= y <= 180
    abstract GetMousePosition: unit -> struct(double * double)
    abstract IsAnyMouseButtonPressed: unit -> bool
    abstract AddMouseClicked: listener: #IFunc<int voption, HUnit> inref -> unit

module Input =
    let isAnyKeyDown (i: 'T byref when 'T :> IInput and 'T : struct) = i.IsAnyKeyDown()
    let isKeyDown (i: 'T byref when 'T :> IInput and 'T : struct) code = i.IsKeyDown code
    let getMousePosition (i: 'T byref when 'T :> IInput and 'T : struct) = i.GetMousePosition()
    let isAnyMouseButtonPressed (i: 'T byref when 'T :> IInput and 'T : struct) = i.IsAnyMouseButtonPressed()
    let addMouseClicked (v: 'T byref when 'T :> IInput and 'T : struct) (listener: _ inref) = v.AddMouseClicked &listener

    [<Struct; NoEquality; NoComparison>]
    type Nil = | Nil
    with
        interface IInput with
            override _.IsAnyKeyDown() = false
            override _.IsKeyDown _ = false
            override _.GetMousePosition() = struct(0., 0.)
            override _.IsAnyMouseButtonPressed() = false
            override _.AddMouseClicked _ref = ()

    let nil = Nil

    [<Struct; NoEquality; NoComparison>]
    type Poly<'I> when 'I :> IInput and 'I : not struct = private { input: 'I }
    with
        interface IInput with
            override i.GetMousePosition() = i.input.GetMousePosition()
            override i.IsAnyKeyDown() = i.input.IsAnyKeyDown()
            override i.IsAnyMouseButtonPressed() = i.input.IsAnyMouseButtonPressed()
            override i.IsKeyDown(code) = i.input.IsKeyDown(code)
            override i.AddMouseClicked(listener) = i.input.AddMouseClicked(&listener)

    let poly input = { input = input }

type ICloud =
    abstract Set: name: string * value: SValue -> unit
    abstract Get: name: string -> SValue

module Cloud =
    open System.Collections.Generic

    let set (x: 'T byref when 'T :> ICloud and 'T : struct) name value = x.Set(name, value)
    let get (x: 'T byref when 'T :> ICloud and 'T : struct) name = x.Get name

    [<Struct; NoEquality; NoComparison>]
    type Offline = private | Offline of Dictionary<string, SValue>
    with
        interface ICloud with
            override _.Set(_, _) = failwith "offline"
            override _.Get _ = failwith "offline"

    let offline() = Offline <| Dictionary()

    [<Struct; NoEquality; NoComparison>]
    type Poly<'C> when 'C :> ICloud and 'C : not struct = private { cloud: 'C }
    with
        interface ICloud with
            override c.Set(n, v) = c.cloud.Set(n, v)
            override c.Get n = c.cloud.Get n

    let poly cloud = { Poly.cloud = cloud }
