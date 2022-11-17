module Scratch.Serialization.Sb3.Tests
open Xunit
open Utf8Json
open Scratch
open Scratch.Primitives
open Scratch.Json.PartialIsomorphisms
open Scratch.Json.Utf8
open Scratch.Serialization.Sb3.Ast
module Sb3 = Scratch.Serialization.Sb3.Syntax
module Map = OMap


let forward x = Iso.forward (Iso.box x)
let parse syntax s =
    try Syntax.deserializeString (Syntax.box syntax) s |> Ok
    with :? JsonParsingException as e -> Error(e.GetUnderlyingStringUnsafe(), e.ActualChar, e.Offset, e)

[<Fact>]
let sb3ParseTest() =
    """{
        "opcode": "procedures_prototype",
        "next": null,
        "inputs": {},
        "fields": {},
        "shadow": true,
        "topLevel": false,
        "mutation": {
            "tagName": "mutation",
            "proccode": "procName",
            "argumentnames": "[]",
            "argumentids": "[]",
            "argumentdefaults": "[]",
            "warp": true,
            "children": []
        }
    }"""
        |> parse Sb3.jBlock
        =? Ok(Complex {
            opcode = Some "procedures_prototype"
            next = None
            parent = None
            inputs = Map.empty
            fields = Map.empty
            shadow = true
            topLevel = false
            x = None
            y = None
            comment = None
            mutation = Some {
                tagName = Some "mutation"
                children = Some HUnit
                proccode = Some "procName"
                argumentids = Some "[]"
                argumentdefaults = Some "[]"
                argumentnames = Some "[]"
                warp = Some true
                hasnext = None
            }
        })

    """{
        "opcode": "procedures_call",
        "next": null,
        "parent": null,
        "inputs": {
            "input0": [3, ".9gsg{9b;gw{LPXb/]Xd", [4, 10]],
            "input1": [3, "p@h=]fBku?a|H}:Q4]~G", [4, 10]]
        },
        "fields": {},
        "shadow": false,
        "topLevel": false,
        "mutation": {
            "tagName": "mutation",
            "children": [],
            "proccode": "procName %n , %n",
            "argumentids": "[\"input0\",\"input1\"]"
        }
    }"""
        |> parse Sb3.jBlock
        =? Ok(Complex {
            opcode = Some "procedures_call"
            next = None
            parent = Some None
            inputs = Map.ofSeq [
                Id "input0", DiffBlockShadow(BlockReference(Id ".9gsg{9b;gw{LPXb/]Xd"), MathNumber(SNumber 10.))
                Id "input1", DiffBlockShadow(BlockReference(Id "p@h=]fBku?a|H}:Q4]~G"), MathNumber(SNumber 10.))
            ]
            fields = Map.ofSeq []
            shadow = false
            topLevel = false
            x = None
            y = None
            comment = None
            mutation = Some {
                tagName = Some "mutation"
                children = Some HUnit
                proccode = Some "procName %n , %n"
                argumentids = Some """["input0","input1"]"""
                argumentdefaults = None
                argumentnames = None
                warp = None
                hasnext = None
            }
        })

    """[12, "varName", "Z.4Wj|XTRL+Xet*z}.}c-varName-", 123, 456]"""
        |> parse Sb3.jBlock
        =? Ok(Simple(DataVariable("varName", Id "Z.4Wj|XTRL+Xet*z}.}c-varName-", Some(Position(123, 456)))))

[<Fact>]
let jSimpleBlockParseTest() =
    """[7, "all"]"""
        |> parse Sb3.jSimpleBlock
        =? Ok(MathInteger(SString "all"))

    """[3, [12, "varName", "varId"], [10, "textValue"]]"""
        |> parse Sb3.jSimpleBlock
        =? Ok(DiffBlockShadow(DataVariable("varName", Id "varId", None), Text(SString "textValue")))

[<Fact>]
let jFieldParseTest() =
    """["X"]"""
        |> parse Sb3.jField
        =? Ok { value = SString "X"; name = None }

[<Fact>]
let jInputParseTest() =
    "[ 1, null ]"
        |> parse Sb3.jInput
        =? Ok(SameBlockShadow EmptyBlock)

[<Fact>]
let costumeParseTest() =
    """
    {
        "assetId": "cd21514d0531fdffb22204e0ec5ed84a",
        "name": "a",
        "dataFormat": "svg"
    }
    """
    |> parse Sb3.jCostume
    =? Ok {
        assetId = "cd21514d0531fdffb22204e0ec5ed84a"
        name = "a"
        dataFormat = "svg"

        bitmapResolution = None
        md5ext = None
        rotationCenterX = None
        rotationCenterY = None
    }

[<Fact>]
let soundParseTest() =
    """
    {
        "assetId": "83a9787d4cb6f3b7632b4ddfebf74367",
        "name": "a",
        "dataFormat": "wav"
    }
    """
    |> parse Sb3.jSound
    =? Ok {
        assetId = "83a9787d4cb6f3b7632b4ddfebf74367"
        name = "a"
        dataFormat = "wav"

        format = None
        rate = None
        sampleCount = None
        md5ext = None
    }
