module rec Scratch.Test.AnsiEscape
open System.Text


[<RequireQualifiedAccess>]
type Color8 =
    /// 0
    | Black
    /// 1
    | Red
    /// 2
    | Green
    /// 3
    | Yellow
    /// 4
    | Blue
    /// 5
    | Magenta
    /// 6
    | Cyan
    /// 7
    | White

type Pallet255 = byte
type Color =
    /// 5;\k<byte>
    | Pallet255 of Pallet255
    /// 2;\k<byte>;\k<byte>;\k<byte>
    | Rgb24Bit of r: byte * g: byte * b: byte

type GraphicRendition =
    /// 0
    | Reset
    /// 1 - Bold or increased intensity
    | Bold
    /// 9
    | CrossedOut
    /// 3[0-7]
    | Color8 of Color8
    /// 38;\k<Color>
    | Color of Color
    /// 39
    | ResetColor
    /// 4[0-7]
    | BackgroundColor8 of Color8
    /// 48;\k<Color>
    | BackgroundColor of Color
    /// 49
    | ResetBackgroundColor
    /// 9[0-7]
    | Color8Bright of Color8
    /// 10[0-7]
    | BackgroundColor8Bright of Color8

type Esc =
    /// \x0b\[ \k<GraphicRendition>* m
    | Graphic of GraphicRendition list
with
    override x.ToString() = Esc.toString x

[<AutoOpen>]
module private Privates =
    type StringBuilder with
        member s.AppendColor c =
            match c with
            | Pallet255 c -> s.Append("5;").Append(c)
            | Rgb24Bit(r, g, b) -> s.Append("2;").Append(r).Append(';').Append(g).Append(';').Append(b)

        member s.AppendColor8(baseNumber, c) =
            let code = function
                | Color8.Black -> 0
                | Color8.Red -> 1
                | Color8.Green -> 2
                | Color8.Yellow -> 3
                | Color8.Blue -> 4
                | Color8.Magenta -> 5
                | Color8.Cyan -> 6
                | Color8.White -> 7

            s.Append(baseNumber + code c)

        member s.AppendGraphicRendition x =
            match x with
            | Reset -> s.Append '0'
            | Bold -> s.Append '1'
            | CrossedOut -> s.Append '9'
            | Color8 c -> s.AppendColor8(30, c)
            | Color c -> s.Append("38;").AppendColor c
            | ResetColor -> s.Append "39"
            | BackgroundColor8 c -> s.AppendColor8(40, c)
            | BackgroundColor c -> s.Append("48;").AppendColor c
            | ResetBackgroundColor -> s.Append "49"
            | Color8Bright c -> s.AppendColor8(90, c)
            | BackgroundColor8Bright c -> s.AppendColor8(100, c)

        member s.AppendEsc x =
            match x with
            | Graphic xs ->
                s.Append "\x1B[" |> ignore
                match xs with
                | [] -> ()
                | x::xs ->
                    s.AppendGraphicRendition x |> ignore
                    for x in xs do s.Append(';').AppendGraphicRendition x |> ignore
                s.Append 'm'

module Esc =
    let append x (s: StringBuilder) = s.AppendEsc x
    let appendGraphicRendition x (s: StringBuilder) = s.AppendGraphicRendition x
    let toString x = StringBuilder().AppendEsc(x).ToString()
    let resetGraphic = Graphic [Reset]

type [<Struct>] Styling = private Styling of GraphicRendition list * string

type ColorLevel =
    | NoColor = 0
    | Basic = 1
    | Pallet256 = 2
    | Rgb24 = 3

type StylingConfig = {
    maxColorLevel: ColorLevel
}
module StylingConfig =
    let defaultValue = {
        maxColorLevel = ColorLevel.Rgb24
    }

module Styling =
    module Constructors =
        let div gs text = Styling(gs, text)
        let text text = div [] text
        let br = text "\n"

    let render withConfig xs =
        let config = withConfig StylingConfig.defaultValue
        let s = StringBuilder()
        for Styling(gs, x) in xs do
            match gs, config.maxColorLevel with
            | [], _
            | _, ColorLevel.NoColor -> s.Append x |> ignore
            | g::gs, _ ->
                let s = s.Append "\x1b[" |> Esc.appendGraphicRendition g
                for g in gs do s.Append ';' |> Esc.appendGraphicRendition g |> ignore
                s.Append('m').Append(x) |> Esc.append Esc.resetGraphic |> ignore

        string s
