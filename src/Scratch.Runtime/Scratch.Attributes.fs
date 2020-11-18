namespace Scratch
open System
open Scratch.Ast
open Scratch.Ast.PartialData
open System.Runtime.InteropServices
#nowarn "0049" // no warning for uppercase variable names


[<AbstractClass>]
type DataAttribute<'T>() =
    inherit Attribute()
    abstract GetData: unit -> 'T

[<AttributeUsage(AttributeTargets.All, AllowMultiple = true)>]
type CostumeAttribute private (data: PartialCostumeData) =
    inherit DataAttribute<PartialCostumeData>()
    
    let mutable data = data

    new (CostumeName) = CostumeAttribute {
        costumeName = Some CostumeName
        baseLayerMD5 = None
        baseLayerID = None
        rotationCenterX = None
        rotationCenterY = None
        textLayerMD5 = None
        textLayerID = None
        bitmapResolution = None
    }
    new (BaseLayerID) = CostumeAttribute {
        costumeName = None
        baseLayerMD5 = None
        baseLayerID = Some <| double BaseLayerID
        rotationCenterX = None
        rotationCenterY = None
        textLayerMD5 = None
        textLayerID = None
        bitmapResolution = None
    }
    member _.CostumeName with set v = data <- { data with costumeName = Some v }
    member _.RotationCenterX with set v = data <- { data with rotationCenterX = Some v }
    member _.RotationCenterY with set v = data <- { data with rotationCenterY = Some v }
    member _.TextLayerMD5 with set v = data <- { data with textLayerMD5 = Some v }
    member _.TextLayerID with set v = data <- { data with textLayerID = Some v }
    member _.BitmapResolution with set v = data <- { data with bitmapResolution = Some v }

    override _.GetData() = data

[<AttributeUsage(AttributeTargets.All, AllowMultiple = true)>]
type SoundAttribute(SoundName, SoundID, Md5) =
    inherit DataAttribute<SoundData>()

    let mutable data =
        {
            soundName = SoundName
            soundID = SoundID
            md5 = Md5
            sampleCount = None
            rate = None
            format = None
        }
    member _.SampleCount with set v = data <- { data with sampleCount = Some v }
    member _.Rate 
        with set v =
            let rate =
                match v with
                | 11025 -> Some R11025
                | 22050 -> Some R22050
                | _ -> None
            data <- { data with rate = rate }

    member _.Format
        with set v =
            let format =
                match v with
                | "" -> Some EmptyFormat
                | "adpcm" -> Some Adpcm
                | _ -> None
            data <- { data with format = format }

    override _.GetData() = data

[<AttributeUsage(AttributeTargets.All)>]
type CurrentCostumeIndexAttribute(Value) =
    inherit DataAttribute<int>()
    override _.GetData() = Value

[<AttributeUsage(AttributeTargets.All)>]
type StageAttribute() =
    inherit DataAttribute<StageDataExtension<unit>>()

    let mutable data = {
        penLayerMD5 = None
        penLayerID = None
        tempoBPM = AstDefinitions.defaultTempoBPM
        videoAlpha = None

        children = []
        info = Map.empty
    }
    member _.PenLayerMD5 with set v = data <- { data with penLayerMD5 = Some v }
    member _.PenLayerID with set v = data <- { data with penLayerID = Some v }
    member _.TempoBPM with set v = data <- { data with tempoBPM = v }
    member _.VideoAlpha with set v = data <- { data with videoAlpha = Some v }

    override _.GetData() = data

type private R = Ast.RotationStyle

[<AttributeUsage(AttributeTargets.All)>]
type SpriteAttribute(Direction, IndexInLibrary, IsDraggable, RotationStyle, Scale, Visible) =
    inherit DataAttribute<SpriteDataExtension>()
    let data = {
        direction = Direction
        indexInLibrary = IndexInLibrary
        isDraggable = IsDraggable
        rotationStyle =
            match RotationStyle with
            | "normal" -> R.Normal
            | "leftRight" -> R.LeftRight
            | "none" -> R.None
            | _ -> R.Normal

        scratchX = 0.
        scratchY = 0.
        scale = Scale
        spriteInfo = Map.empty
        visible = if Visible then Visibility.Visible else Hidden
    }
    override _.GetData() = data

[<AttributeUsage(AttributeTargets.All, AllowMultiple = true)>]
type AnnotationAttribute private (key, value) =
    inherit DataAttribute<string * SValue>()
    override _.GetData() = key, value
    new (key: string, value) = AnnotationAttribute(key, SNumber value)
    new (key: string, value) = AnnotationAttribute(key, SString value)
    new (key: string, value) = AnnotationAttribute(key, SBool value)
    new (key: string, value) = AnnotationAttribute(key, SNumber(double<int> value))

[<AttributeUsage(AttributeTargets.All)>]
type PersistentAttribute() =
    inherit Attribute()

[<AttributeUsage(AttributeTargets.All)>]
type PositionAttribute(X, Y) =
    inherit DataAttribute<double * double>()
    override _.GetData() = X, Y

[<AttributeUsage(AttributeTargets.All)>]
type DefaultAttribute private (value) =
    inherit DataAttribute<SValue>()
    override _.GetData() = value
    new (value) = DefaultAttribute(SNumber value)
    new (value) = DefaultAttribute(SString value)
    new (value) = DefaultAttribute(SBool value)

[<AttributeUsage(AttributeTargets.All)>]
type ExportAttribute() = inherit Attribute()

[<AutoOpen>]
module PseudoAttributes =
    let attributes (_attributes: Attribute list Lazy) x = x

[<AttributeUsage(AttributeTargets.Method)>]
type BlockAttribute private (operator, extensionId, extensionOpCode) =
    inherit Attribute()
    new (Operator: Symbol) = BlockAttribute(Operator, null, null)
    new (ExtensionId: string, [<Optional; DefaultParameterValue(null: string)>] OpCode: string) = BlockAttribute(Symbol.Extension, ExtensionId, OpCode)

    member _.Operator = operator
    member _.ExtensionId = extensionId
    member _.ExtensionOpCode = extensionOpCode
