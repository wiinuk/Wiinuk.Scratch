namespace Scratch
open System
open Scratch
open Scratch.Primitives
open Scratch.Threading
open Scratch.AtomicGeneratorBuilderOperations
open FiberBuilderDelay
#nowarn "0062" // mlcompatibility


type Stop =
    | StopAll

exception StopException of Stop * message: string

[<AutoOpen>]
module SListOperations =
    let defineList<'a> initialValues = SList <| ResizeArray<'a>(List.toSeq initialValues)

[<AutoOpen>]
module MathOperators =
    let internal random = Random 0xCAFEBABE
    let randomFromTo x1 x2 =
        let x1 = if Double.IsNaN x1 then 0. else x1
        let x2 = if Double.IsNaN x2 then 0. else x2
        let x1, x2 = if x2 < x1 then x2, x1 else x1, x2
        let r =
            if x1 = x2 then x1
            elif x1 % 1. = 0. && x2 % 1. = 0.
            then floor (random.NextDouble() * (x2 - x1 + 1.)) + x1
            else random.NextDouble() * (x2 - x1) + x1
        r

    [<ReflectedDefinition>]
    let rounded x = LanguagePrimitives.FloatWithMeasure<'u>(Math.Round(float<float<'u>> x, MidpointRounding.AwayFromZero))

module SList =
    let length (SList xs) = xs.Count
    let remove (SList xs) nth = xs.RemoveAt(nth - 1) |> ignore
    let removeAll (SList xs) = xs.Clear()
    let removeRandom xs = remove xs (MathOperators.random.Next(1, length xs + 1))
    let removeLast xs = remove xs (length xs)
    let push (SList xs) x = xs.Add x
    let set (SList xs) nth x = xs.[nth - 1] <- x
    let setRandom xs = set xs (MathOperators.random.Next(1, length xs + 1))
    let setLast xs = set xs (length xs)
    let get (SList xs) nth = xs.[nth - 1]
    let getRandom xs = get xs (MathOperators.random.Next(1, length xs + 1))
    let getLast xs = get xs (length xs)
    let toList (SList xs) = List.ofSeq xs

[<AutoOpen>]
module PrimitiveOperations =
    open AtomicGeneratorFull

    let repeat count action =
        for _ in 1 .. count do action()

    let repeatAsync count = generatorPolyWith <| fun action -> fiber {
        for _ in 1 .. count do
            do! Generator.getFiber &action
            ThreadYield
    }
    let repeatUntil predicate action =
        while predicate() |> not do action()

    let repeatUntilAsync predicate = generatorPolyWith <| fun action -> fiber {
        while not (predicate()) do
            do! Generator.getFiber &action
            ThreadYield
    }
    let waitUntilAsync predicate = fiber {
        while not (predicate()) do
            ThreadYieldForce
    }
    let forever action =
        let rec aux() =
            action()
            aux()
        aux()

    let foreverAsync<'a,'b> : GeneratorPolyWith<fiber<_,'b,_>,_,_,_,_> = generatorPolyWith <| fun action -> fiber {
        while true do
            do! Generator.getFiber &action
            ThreadYield
        return (failwith "": 'a)
    }

    let length s = String.length s
    let getChar s n = let i = n - 1 in (s: string).[i..i]
    let combine (x: string) y = x + y
    
    let timer<'a> =
        let scheduler = Runtime.globalRuntime.scheduler
        let d = Scheduler.now scheduler - scheduler.timerStart
        d.TotalSeconds

    let showVariable x = ignore x
    let hideVariable x = ignore x

    let stopAllScripts() = raise <| StopException(StopAll, "")
    let stopThisScript() = atomic { Abort }

[<AutoOpen>]
module SoundOperations =
    // TODO:
    let playSound soundName =
        Runtime.Logger.logPlaySound Runtime.globalRuntime soundName

[<AutoOpen>]
module PenOperations =
    // TODO:
    let clearPenTrails() = ()

[<AutoOpen>]
module SensingOperations =
    // TODO:
    let keyPressed (_: int) = false
    // TODO:
    let keyPressedFrom (_: string) = false
    // TODO:
    let mousePressed<'a> = false
    // TODO:
    let mouseX<'a> = 0.
    // TODO:
    let mouseY<'a> = 0.

    // TODO:
    let answer<'a> = ""
    // TODO:
    let doAsk (_: string) = fiber { ThreadYieldForce }

[<AutoOpen>]
module SpriteOperations =
    let defineSprite sprite =
        let sprite = sprite { prototype = None; runtime = Runtime.globalRuntime }
        Runtime.globalRuntime.children.Add sprite
