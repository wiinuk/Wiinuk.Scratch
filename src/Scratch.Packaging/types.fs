namespace Scratch.Packaging
open Scratch.Ast

/// Wrapper for base64 format string.
[<Struct>]
type Base64 = private Base64 of string
module Base64 =
    open System

    let ofBytes bytes = Convert.ToBase64String bytes |> Base64
    let toBytes (Base64 x) = Convert.FromBase64String x
    let toString (Base64 x) = x

/// Wrapper for (md5 string + extension) format string.
[<Struct>]
type HashWithExtension = private HashWithExtension of string

type ResourceDescriptor =
    /// e.g. `ResourceInZip("X:/Dir/ZipFile.zip", "DirInZip/FileInZip.ext", ".ext")`
    | ResourceInZip of path: string * entryFullName: string * extension: string
    | ResourcePath of path: string
    | ResourceBase64 of base64: Base64 * extension: string

type PackageData<'Project> = {
    images: ResourceDescriptor list
    sounds: ResourceDescriptor list
    project: 'Project
}
module PackageData =
    let withProject value package = {
        images = package.images
        sounds = package.sounds
        project = value
    }
type ResourceType =
    | Image
    | Sound

type PackageErrorInfo =
    | IdNotFound of resourceName: string * id: int
    | InvalidExtension of resource: ResourceDescriptor * validExtensions: string Set
    | FileOpenFailure of path: string * exn
    | HashMismatch of projectHash: HashWithExtension * actualHash: HashWithExtension
    | TextLayerConstraintViolation of name: string
    | PenLayerConstraintViolation of name: string
    | InvlidHashWithExtensionFormat of name: string * value: string

exception PackageException of error: PackageErrorInfo with
    override e.Message = $"{e.error}"
