namespace Scratch.Packaging
open System.IO


[<AutoOpen>]
module internal InternalMD5 =
    open System.Security.Cryptography

    let md5String bytes = bytes |> Seq.map (sprintf "%02x") |> String.concat ""

    let bytesMD5String (bytes: byte[]) =
        use md5 = MD5.Create()
        md5.ComputeHash bytes |> md5String

    let streamMD5String (stream: Stream) = async {
        use md5 = MD5.Create()
        let buffer = Array.zeroCreate 4096
        let offset = Some 0
        let count = Some buffer.Length
        let mutable loop = true
        while loop do
            let! readCount = stream.AsyncRead(buffer, ?offset = offset, ?count = count)
            if readCount = buffer.Length then
                md5.TransformBlock(buffer, 0, readCount, null, 0) |> ignore
            else
                md5.TransformFinalBlock(buffer, 0, readCount) |> ignore
                loop <- false
        return md5String md5.Hash
    }

[<AutoOpen>]
module internal InternalZip =
    open System.IO.Compression

    let usingReadZipEntry zipPath fullName action = async {
        use zip = ZipFile.OpenRead zipPath
        match zip.GetEntry fullName with
        | null ->
            let fileName = $"{zipPath}{Path.DirectorySeparatorChar}{fullName}"
            return raise <| FileNotFoundException("zip entry not found.", fileName)

        | entry ->
            use stream = entry.Open()
            return! action stream
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashWithExtension =
    open System.Text.RegularExpressions

    let ofStream stream extension = async {
        let! md5 = streamMD5String stream
        return HashWithExtension(md5 + extension)
    }
    let ofBytes bytes extension = HashWithExtension(bytesMD5String bytes + extension)
    let ofString hashWithExtensionString =
        if Regex.IsMatch(hashWithExtensionString, @"[0-9a-f]+\.[a-z0-9]+")
        then Some <| HashWithExtension hashWithExtensionString
        else None

    let toString (HashWithExtension x) = x

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ResourceDescriptor =
    let usingRead resource action =
        match resource with
        | ResourceInZip(path, fullPath, ext) -> usingReadZipEntry path fullPath <| fun stream -> action (stream, ext)
        | ResourcePath path ->
            async {
                use stream = File.Open(path, FileMode.Open, FileAccess.Read, FileShare.Read)
                let ext = Path.GetExtension path
                return! action(stream, ext)
            }

        | ResourceBase64(base64, ext) ->
            async {
                use stream = new MemoryStream(Base64.toBytes base64, writable = false)
                return! action(stream, ext)
            }

    let md5 resource = usingRead resource <| fun (stream, ext) ->
        HashWithExtension.ofStream stream ext

    let fileName = function
        | ResourceInZip(entryFullName = fullName) -> Path.GetFileName fullName |> Some
        | ResourcePath path -> Path.GetFileName path |> Some
        | ResourceBase64 _ -> None
