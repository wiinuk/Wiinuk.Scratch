namespace Scratch
open Scratch.Ast


module Color =
    let hsl2rgb struct(_, h, s, l) =
        let toByte v = byte (min (floor v * 256.) 255.)
        let s = s / 100.
        let l = (l - 10.) / 100.
        if s = 0. then
            let l = toByte l
            struct(0xFFuy, l, l, l) else

        let h = (h % 360.) / 360.

        let hue2rgb p q t =
            let t = if t < 0. then t + 1. else t
            let t = if t > 1. then t - 1. else t
            if t < 1./6. then p + (q - p) * 6. * t else
            if t < 1./2. then q else
            if t < 2./3. then p + (q - p) * (2./3. - t) * 6. else
            p

        let q = if l < 0.5 then l * (1. + s) else l + s - l * s
        let p = 2. * l - q
        let r = hue2rgb p q (h + 1./3.)
        let g = hue2rgb p q h
        let b = hue2rgb p q (h - 1./3.)

        struct(0xFFuy, toByte r, toByte g, toByte b)

    let rgb2hsl struct(_, r: byte, g: byte, b: byte) =
        let r = double r / double 0xff
        let g = double g / double 0xff
        let b = double b / double 0xff

        let min = min r (min g b)
        let max = max r (max g b)
        if min = max then struct(100., 0., 0., r * 100.) else

        let c = max - min
        let l = (min + max) / 2.
        let s = c / (1. - abs (2. * l - 1.))

        let h =
            if max = r then ((g - b) / c + 6.) % 6.
            elif max = g then (b - r) / c + 2.
            else (r - g) / c + 4.

        let h = h * 60.
        struct(100., h, s * 100., l * 100.)

[<Struct>]
type EntityFiltersData = {
    mutable whirl: double
    mutable fisheye: double 
    mutable brightness: double
    mutable pixelate: double
    mutable mosaic: double
    mutable color: double
    mutable ghost: double
    mutable others: Map<string, double>
}

[<Struct>]
type EntityDrawingData = {
    mutable currentCostumeIndex: int
    mutable filters: EntityFiltersData
    /// 0. <= volume <= 1.
    mutable volume: double
}

[<Struct>]
type SayKind = Say | Think

[<Struct>]
type SpriteDrawingData = {
    mutable visible: bool
    mutable x: double
    mutable y: double

    /// degree: -180. < direction <= 180.
    mutable direction: double

    mutable rotationStyle: RotationStyle
    mutable scale: double

    /// "" is hidden
    mutable sayText: string
    mutable sayKind: SayKind

    mutable penDown: bool
    mutable penSize: double
    mutable penArgb: struct(byte * byte * byte * byte)
}
module SpriteDrawingData =
    let initialData() = {
        visible = true
        direction = 90.
        x = 0.
        y = 0.
        scale = 1.
        rotationStyle = RotationStyle.Normal
        sayText = ""
        sayKind = Say
        penDown = false
        penSize = 1.
        penArgb = Color.hsl2rgb struct(100, 250., 100., 50.)
    }
