[<AutoOpen>]
module internal Scratch.Packaging.Internal
open System
open System.IO
open System.IO.Compression
open Scratch.Ast
open Scratch.Json.Utf8
open Scratch.Packaging
open Scratch.Primitives


let soundExtensions = Set [".wav"]
let imageExtensions = Set [".gif"; ".png"; ".jpg"; ".svg"]

let inline raiseError e = PackageException e |> raise

type CheckState = {
    usingFiles: FileStream ResizeArray
    imageArray: ResourceDescriptor array
    soundArray: ResourceDescriptor array
}
with
    interface IDisposable with
        override s.Dispose() =
            for f in s.usingFiles do f.Dispose()

let hashWithExtension resource = async {
    match resource with
    | ResourceBase64(base64, ext) -> return HashWithExtension.ofBytes (Base64.toBytes base64) ext
    | ResourcePath path ->
        let ext = Path.GetExtension path
        let image = File.Open(path, FileMode.Open, FileAccess.Read, FileShare.Read)
        return! HashWithExtension.ofStream image ext

    | ResourceInZip(path, fullPath, ext) ->
        return! usingReadZipEntry path fullPath <| fun entry ->
            HashWithExtension.ofStream entry ext
}
let checkResourceFile { usingFiles = usingFiles } resourceName id md5 paths resourceExtensions =
    match Array.tryItem id paths with
    | None -> IdNotFound(resourceName, id) |> raiseError
    | Some resource -> async {

    let! hash = hashWithExtension resource
    if md5 <> hash then HashMismatch(md5, hash) |> raiseError

    match resource with
    | ResourceBase64(_, ext) ->
        if not <| Set.contains ext resourceExtensions then InvalidExtension(resource, resourceExtensions) |> raiseError
        return ext

    | ResourcePath path ->
        let ext = Path.GetExtension path
        if not <| Set.contains ext resourceExtensions then InvalidExtension(resource, resourceExtensions) |> raiseError
        let image = File.Open(path, FileMode.Open, FileAccess.Read, FileShare.Read)
        usingFiles.Add image
        return ext

    | ResourceInZip(_, _, ext) ->
        return ext
}

let checkHashWithExtension name x =
    match HashWithExtension.ofString x with
    | None -> raiseError <| InvlidHashWithExtensionFormat(name, x)
    | Some x -> x

let checkCostume ({ imageArray = imagePaths } as state) c =
    let baseLayerMD5 = checkHashWithExtension c.costumeName c.baseLayerMD5
    checkResourceFile state c.costumeName (int c.baseLayerID) baseLayerMD5 imagePaths imageExtensions |> ignore
    match c.textLayerID, c.textLayerMD5 with
    | None, None -> ()
    | Some id, Some md5 ->
        let md5 = checkHashWithExtension c.costumeName md5
        checkResourceFile state c.costumeName (int id) md5 imagePaths imageExtensions |> ignore

    | _ -> TextLayerConstraintViolation c.costumeName |> raiseError

let checkSound ({ soundArray = soundPaths } as state) s =
    let md5 = checkHashWithExtension s.soundName s.md5
    checkResourceFile state s.soundName (int s.soundID) md5 soundPaths soundExtensions |> ignore

let checkEntity state data =
    for c in data.costumes do
        checkCostume state c
    for s in data.sounds do
        checkSound state s

let checkStage state stageData = async {
    let i = int stageData.currentCostumeIndex
    match List.tryItem i stageData.costumes with
    | None -> IdNotFound(stageData.objName, int i) |> raiseError
    | Some _ -> ()

    match stageData.ObjectDataExtension.penLayerID, stageData.ObjectDataExtension.penLayerMD5 with
    | None, None -> ()
    | Some id, Some md5 ->
        let md5 = checkHashWithExtension stageData.objName md5
        let! ext = checkResourceFile state stageData.objName (int id) md5 state.imageArray imageExtensions
        if ext = ".svg" then PenLayerConstraintViolation stageData.objName |> raiseError

    | _ -> PenLayerConstraintViolation stageData.objName |> raiseError

    checkEntity state stageData
    for c in stageData.ObjectDataExtension.children do
        match c with
        | Choice1Of3 _
        | Choice3Of3 _ -> ()
        | Choice2Of3 s -> checkEntity state s
}
let checkOrRaisePackage package action = async {
    use state = {
        usingFiles = ResizeArray()
        imageArray = List.toArray package.images
        soundArray = List.toArray package.sounds
    }
    do! checkStage state package.project
    do! action state.usingFiles
}

let compressionLevelFromExtension = function
    | ".svg"
    | ".wav" -> CompressionLevel.Fastest
    | _ -> CompressionLevel.NoCompression

let sb2ResourceNaming struct (index, resource) =
    let ext =
        match resource with
        | ResourcePath path -> Path.GetExtension path
        | ResourceBase64(_, ext)
        | ResourceInZip(_, _, ext) -> ext

    async.Return <| $"{index}%s{ext}"

let sb3ResourceNaming struct(_, resource) = async {
    let! name = hashWithExtension resource
    return HashWithExtension.toString name
}
let writeResourceFile zip resourceNaming resource index = async {
    let! entryName = resourceNaming struct (index, resource)

    match resource with
    | ResourcePath path ->
        let ext = Path.GetExtension path
        let level = compressionLevelFromExtension ext
        ZipFileExtensions.CreateEntryFromFile(zip, path, entryName, level) |> ignore

    | ResourceBase64(base64, ext) ->
        let level = compressionLevelFromExtension ext
        let entry = zip.CreateEntry(entryName, level)
        use entry = entry.Open()
        let binary = Base64.toBytes base64
        do! entry.AsyncWrite(binary, 0, binary.Length)

    | ResourceInZip(path, entryFullName, ext) ->
        do! usingReadZipEntry path entryFullName <| fun source -> async {
            let level = compressionLevelFromExtension ext
            let destination = zip.CreateEntry(entryName, level)
            use destination = destination.Open()

            do! source.CopyToAsync destination |> Async.AwaitTask
        }
}
let white2x2PngBase64 = Base64 "iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAYAAABytg0kAAAAEUlEQVQYV2P8DwQMQMAIYwAAV9UH+3zjkgoAAAAASUVORK5CYII="
let defaultPenImage = ResourceBase64(white2x2PngBase64, ".png")
let emptySvgBase64 =
    "PHN2ZyB2ZXJzaW9uPSIxLjEiIHdpZHRoPSIyIiBoZWlnaHQ9IjIiIHZpZXdCb3g"
    + "9Ii0xIC0xIDIgMiIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIi"
    + "B4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayI+CiAgP"
    + "CEtLSBFeHBvcnRlZCBieSBTY3JhdGNoIC0gaHR0cDovL3NjcmF0Y2gubWl0LmVk"
    + "dS8gLS0+Cjwvc3ZnPg=="
    |> Base64

let defaultCostumeImage = ResourceBase64(emptySvgBase64, ".svg")

let costumeDataFromResource costumeName bitmapResolution rotationCenterX rotationCenterY baseResource index = async {
    let! md5 = ResourceDescriptor.md5 baseResource
    return {
        costumeName = costumeName
        baseLayerID = double index
        baseLayerMD5 = HashWithExtension.toString md5
        textLayerID = None
        textLayerMD5 = None
        bitmapResolution = Some bitmapResolution
        rotationCenterX = rotationCenterX
        rotationCenterY = rotationCenterY
    }
}

let inline fixField get update fix x = async {
    match! fix (get x) with
    | None -> return None
    | Some v -> return Some (update x v)
}

let inline (>>?) f1 f2 x = async {
    match! f1 x with
    | None -> return! f2 x
    | Some x ->

    match! f2 x with
    | None -> return Some x
    | x -> return x
}

let rec fixList fix xs = async {
    match xs with
    | [] -> return None
    | x::xs ->

    let! xf = fix x
    let! xsf = fixList fix xs

    return
        match xf, xsf with
        | None, None -> None
        | None, Some xs -> Some(x::xs)
        | Some x, xs' -> Some(x::Option.defaultValue xs xs')
}
let fixCostumesField fix = fixField (fun x -> x.costumes) (fun x v -> { x with costumes = v }) fix
let fixChildrenField fix = fixField (fun x -> x.ObjectDataExtension.children) (fun x v -> { x with ObjectDataExtension = { x.ObjectDataExtension with children = v } }) fix

let fixChoice3 fix1 fix2 fix3 x = async {
    match x with
    | Choice1Of3 x ->
        let! x = fix1 x
        return Option.map Choice1Of3 x

    | Choice2Of3 x ->
        let! x = fix2 x
        return Option.map Choice2Of3 x

    | Choice3Of3 x ->
        let! x = fix3 x
        return Option.map Choice3Of3 x
}

let nofix _ = async.Return None

let fixPackage package = async {
    let defaultImageAndIndex defaultImage index stores =
        let store = Seq.item index stores
        match !store with
        | Some x -> x
        | None ->
            let x = defaultImage, List.length package.images + Seq.sumBy ((!) >> Option.count) stores
            store := Some x
            x

    let defaultImageAndIndexes = List.init 2 <| fun _ -> ref None
    let getDefaultCostumeImageAndIndex() = defaultImageAndIndex defaultCostumeImage 0 defaultImageAndIndexes
    let getDefaultPenImageAndIndex() = defaultImageAndIndex defaultPenImage 1 defaultImageAndIndexes

    let (|InvalidBaseLayerMD5|) x = x = ""
    let (|InvalidCostumeName|) x = x = ""
    let (|InvalidBaseLayerID|) x = (x < 0.) || Double.IsNaN x

    let fixCostume_NameToBaseLayerId c =
        match c.baseLayerID, c.costumeName with
        | InvalidBaseLayerID true, InvalidCostumeName true -> nofix()
        | InvalidBaseLayerID true, name ->
            async {
                let index =
                    package.images
                    |> List.tryFindIndex (fun i ->
                        match ResourceDescriptor.fileName i with
                        | Some n -> name = Path.GetFileNameWithoutExtension n
                        | _ -> false
                    )

                match index with
                | None -> return None
                | Some index -> return Some { c with baseLayerID = double index }
            }
        | _ -> nofix()

    let fixCostume_BaseLayerIdToName c =
        match c.baseLayerID, c.costumeName with
        | InvalidBaseLayerID true, InvalidCostumeName true -> nofix()
        | baseLayerID, InvalidCostumeName true ->
            async {
                match List.tryItem (int baseLayerID) package.images with
                | None -> return None
                | Some image ->

                match ResourceDescriptor.fileName image with
                | None -> return None
                | Some n -> return Some { c with costumeName = Path.GetFileNameWithoutExtension n }
            }
        | _ -> nofix()

    let fixCostume_BaseLayerIdToMd5 c =
        match c.baseLayerMD5 with
        | InvalidBaseLayerMD5 true ->
            async {
                match List.tryItem (int c.baseLayerID) package.images with
                | None -> return None
                | Some image ->
                    let! md5 = ResourceDescriptor.md5 image
                    return { c with baseLayerMD5 = md5 |> HashWithExtension.toString } |> Some
            }
        | _ -> nofix()

    let fixCostume =
        fixCostume_NameToBaseLayerId >>?
        fixCostume_BaseLayerIdToName >>?
        fixCostume_BaseLayerIdToMd5

    let fixCostumeList = function
        | (_::_) as cs -> fixList fixCostume cs
        | [] -> async {
            let image, index = getDefaultCostumeImageAndIndex()
            let! c = costumeDataFromResource "costume1" 1. 0. 0. image index
            return Some [c]
        }

    let fixEntity x = fixCostumesField fixCostumeList x

    let fixSprite = fixEntity

    let fixPenLayer stage =
        match stage.ObjectDataExtension.penLayerMD5, stage.ObjectDataExtension.penLayerID with
        | Some _, Some _ -> nofix()
        | _ -> async {
            let image, index = getDefaultPenImageAndIndex()
            let! md5 = ResourceDescriptor.md5 image
            return Some {
                stage with
                    ObjectDataExtension = {
                        stage.ObjectDataExtension with
                            penLayerID = Some(double index)
                            penLayerMD5 = md5 |> HashWithExtension.toString |> Some
                    }
            }
        }

    let fixStage =
        fixEntity
        >>? fixChildrenField (fixList (fixChoice3 nofix fixSprite nofix))
        >>? fixPenLayer

    match! fixStage package.project with
    | None -> return package
    | Some stage ->
        let package = { package with project = stage }
        return
            defaultImageAndIndexes
            |> Seq.fold (fun package store ->
                match !store with
                | None -> package
                | Some(image, _) -> { package with images = package.images @ [image] }
            ) package
}
let writeResources zip resourceNaming packageData = async {
    for i, image in packageData.images |> Seq.indexed do
        do! writeResourceFile zip resourceNaming image i

    for i, sound in packageData.sounds |> Seq.indexed do
        do! writeResourceFile zip resourceNaming sound i
}
let writeProjectJson (zip: ZipArchive) syntax packageData = async {
    let project = zip.CreateEntry "project.json"
    use stream = project.Open()
    do! Syntax.serializeStream syntax stream packageData.project
}
let writePackage zip resourceNaming packageData projectJsonSyntax = async {
    do! writeResources zip resourceNaming packageData
    do! writeProjectJson zip projectJsonSyntax packageData
}
let fixAndWritePackageToStream stream (packageData: 'a StageData PackageData) write = async {
    let! packageData = fixPackage packageData
    return! checkOrRaisePackage packageData <| fun _ -> async {
        use zip = new ZipArchive(stream, ZipArchiveMode.Create, leaveOpen = true)
        return! write zip packageData
    }
}
