namespace rec Scratch
open System
open Scratch.Ast
open Scratch.Primitives
open Scratch.Reflection
open Scratch.Threading
open Scratch.Runtime


type tightrope<'R> = fiber<Flow, struct(ThreadInfo * Entity),'R>

[<NoComparison; NoEquality>]
type TightropeBuilder = | TightropeBuilder
with
    interface IGeneratorBuilder

type TightropeBuilder with
    member inline _.Bind(x: #tightrope<_>, f: _ -> #tightrope<_>): tightrope<_> = upcast Fiber.bind (let f = Func.ofFun f in &f) &x
    member inline _.Return x: tightrope<_> = upcast Fiber.result x
    member inline _.ReturnFrom (x: #tightrope<_>) = x
    member inline _.Combine(x1: #tightrope<_>, x2: #tightrope<_>): tightrope<_> = upcast Fiber.combine &x2 &x1
    member inline _.Delay(f: _ -> #tightrope<_>) = f
    member inline _.Zero(): tightrope<_> = upcast Fiber.zero
    member inline _.Run (f: unit -> #tightrope<_>) = Fiber.delay (let f = Func.ofFun f in &f)

[<AutoOpen>]
module GeneratorBuilderOperations =
    let tightrope = TightropeBuilder

type AtomicGeneratorBuilder = | AtomicGeneratorBuilder with
    interface IGeneratorBuilder

[<AutoOpen>]
module AtomicGeneratorBuilderOperations =
    let atomic = AtomicGeneratorBuilder

type AtomicGeneratorBuilder with
    member inline _.Bind(x: #tightrope<_>, f: _ -> #tightrope<_>): tightrope<_> = upcast Fiber.bind (let f = Func.ofFun f in &f) &x
    member inline _.Return x: tightrope<_> = upcast Fiber.result x
    member inline _.ReturnFrom (x: #tightrope<_>) = x
    member inline _.Combine(x1: #tightrope<_>, x2: #tightrope<_>): tightrope<_> = upcast Fiber.combine &x2 &x1
    member inline _.Delay(f: _ -> #tightrope<_>) = f
    member inline _.Zero(): tightrope<_> = upcast Fiber.zero
    member inline _.Run(f: _ -> #tightrope<_>) =
        let f = Fiber.delay (let f = Func.ofFun f in &f)
        Atomic.doAtomic &f

module AtomicGeneratorFull =
    type AtomicGeneratorBuilder with
        member inline _.Yield x = Fiber.singleton x
        member inline _.YieldFrom x = x
        member inline _.For(xs, action) = Fiber.for' action xs
        member inline _.While(test, body) = Fiber.while' (let test = Func.ofFun test in &test) (let body = Func.ofFun body in &body)
        member inline _.TryWith(body, handler) = Fiber.tryWith &body &handler

[<AutoOpen>]
module private SpriteHelpers =
    open System.Reflection
    type B = System.Reflection.BindingFlags

    let copySList (SList xs) = SList(ResizeArray xs)
    let copyInplace (SList source) (SList destination) =
        destination.Clear()
        destination.AddRange source

    let objectNameOfType (t: Type) =
        t.Name

    let registerTaskWithSelf name (task: _ inref) self scheduler =
        Scheduler.registerFiberWithSelf self name DateTime.MinValue &task scheduler

[<Struct>]
[<NoComparison; NoEquality>]
type EntityInitializeOptions = private {
    runtime: Runtime
    prototype: Entity option
}

[<AutoOpen>]
module CloneCompiler =
    type private E = System.Linq.Expressions.Expression

    let private buildCloneLambda (selfT: Type) =
        if selfT.BaseType <> typeof<Sprite> then failwithf $"require: type %s{selfT.Name}(...) = inherit Sprite(...) ..."

        // Func<_,_,_>(fun (original: Sprite) (options: EntityInitializeOptions) ->
        //     let clone = new 'Self(options)
        //     let source = original :?> 'Self
        //     clone.intField <- source.intField
        //     clone.stringField <- source.stringField
        //     clone.sListField <- copySList source.sListField
        //     copySList source.%xsIF clone.%xsIF
        //     ...
        //     clone :> Sprite
        // )

        let originalP = E.Parameter(typeof<Sprite>, "original")
        let optionsP = E.Parameter(typeof<EntityInitializeOptions>, "options")
        let body =
            let cloneV = E.Parameter(selfT, "clone")
            let sourceV = E.Parameter(selfT, "source")
            E.Block(
                variables = [cloneV; sourceV],
                expressions = seq {
                //     let clone = new 'Self(options)
                let ctor = selfT.GetConstructor [| typeof<EntityInitializeOptions> |]
                E.Assign(cloneV, E.New(ctor, optionsP)) :> E
                //     let source = original :?> 'Self
                E.Assign(sourceV, E.TypeAs(originalP, selfT)) :> E

                let copyInplaceMD, _ = Member.findMethod <@ copyInplace @>
                let copySListMD, _ = Member.findMethod <@ copySList @>
                for f in selfT.GetFields(B.Instance ||| B.NonPublic ||| B.Public ||| B.DeclaredOnly) do
                    if f.IsNotSerialized then () else
                    
                    let t = f.FieldType
                    if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<SList<_>> then
                        if f.IsInitOnly || f.IsLiteral then
                            // copySList source.%xsF clone.%xsF
                            let copyInplaceOfTM = copyInplaceMD.MakeGenericMethod(t.GetGenericArguments())
                            E.Call(copyInplaceOfTM, E.Field(sourceV, f), E.Field(cloneV, f)) :> E
                        else
                            // clone.%xsF <- copySList source.%xsF
                            let copySListOfTM = copySListMD.MakeGenericMethod(t.GetGenericArguments())
                            E.Assign(E.Field(cloneV, f), E.Call(copySListOfTM, E.Field(sourceV, f))) :> E
                    else
                        if f.IsInitOnly || f.IsLiteral then () else

                        // clone.%f <- source.%f
                        E.Assign(E.Field(cloneV, f), E.Field(sourceV, f)) :> E
                // ...

                //     upcast clone
                cloneV :> E
            })
        E.Lambda<Func<Sprite, EntityInitializeOptions, Sprite>>(
            name = "Clone" + selfT.Name,
            parameters = [originalP; optionsP],
            body = body
        )

    let compileCloneFunc selfType =
        buildCloneLambda(selfType).Compile()

[<AbstractClass>]
type Entity(options) as self =
    let mutable data =
        match options.prototype with
        | Some proto -> proto.InternalEntityDrawingData
        | None -> {
            currentCostumeIndex = 0
            filters = {
                whirl = 0.
                color = 0.
                fisheye = 0.
                brightness = 0.
                pixelate = 0.
                mosaic = 0.
                ghost = 0.
                others = Map.empty
            }
            volume = 100.
        }
    let objectName =
        match options.prototype with
        | Some proto -> proto.InternalObjectName
        | None -> objectNameOfType <| self.GetType()

    let broadcastEvents = ref Map.empty

    member internal __.InternalObjectName = objectName

    member _.WhenIReceive name = generatorPolyWith <| fun listener ->
        let listener = listener :> generator<Flow, struct(ThreadInfo * Entity), unit>
        let name = (name + "").ToLowerInvariant()

        let listeners =
            match Map.tryFind name !broadcastEvents with
            | ValueNone -> [listener]
            | ValueSome listeners -> listener::listeners

        broadcastEvents := Map.add name listeners !broadcastEvents

    member internal __.InternalEntityDrawingData: _ byref = &data

    member internal __.InternalBroadcastListeners name =
        let name = LowerName.toString name
        match Map.tryFind name !broadcastEvents with
        | ValueNone -> []
        | ValueSome listeners -> listeners

[<Sealed>]
type Stage internal (options) =
    inherit Entity(options)

[<AbstractClass>]
type Sprite(options) as self =
    inherit Entity(options) 

    let isClone = Option.isSome options.prototype

    [<NonSerialized>]
    let costumes = ResizeArray()

    let data, cloneFunc, objectName =
        match options.prototype with
        | Some(:? Sprite as p) -> p.PrivateSpriteDrawingData, p.PrivateCloneFunc, p.PrivateObjectName
        | _ ->
            let data = SpriteDrawingData.initialData()
            let cloneFunc = compileCloneFunc <| self.GetType()
            let objectName = objectNameOfType <| self.GetType()
            data, cloneFunc, objectName

    let mutable data = data

    static let findObject name ({ Runtime.children = children } as r) =
        let child =
            children
            |> Seq.tryFind (fun c -> c.PrivateObjectName = name && not c.PrivateIsClone)

        match child with
        | Some c -> ValueSome(c :> Entity)
        | None ->

        if name = "_stage_" || name = r.stage.InternalObjectName then ValueSome(upcast r.stage)
        else ValueNone

    //    for (let i = 0, l = children.length; i < l; i++) {
    //        const c = children[i];
    //        if (c.objName === name && !c.isClone) {
    //            return c as Sprite;
    //        }
    //    }
    //    if (name === '_stage_' || name === this.objName) {
    //        return this;
    //    }
    //    return
    //}

    member private __.PrivateIsClone = isClone
    member private __.PrivateCloneFunc = cloneFunc
    member private __.PrivateObjectName = objectName

    member private __.PrivateSpriteDrawingData: _ byref = &data

    member self.Clone() =
        let prototype = {
            runtime = options.runtime
            prototype = Some(upcast self)
        }
        let clone = cloneFunc.Invoke(self, prototype)
        let children = options.runtime.children
        children.Insert(children.IndexOf self, clone)

    member self.CloneSprite name =
        let parent =
            match name with
            | "_myself_" -> ValueSome self
            | _ ->
                match findObject name options.runtime with
                | ValueSome(:? Sprite as parent) -> ValueSome parent
                | _ -> ValueNone

        match parent with
        | ValueNone -> ()
        | ValueSome parent -> parent.Clone()

    //const clone = (name: "_myself_" | string) => {
    //    const parent = (name === '_myself_') ? S : self.getObject(name);
    //    if (parent == null || parent.isSprite !== true) { return }

    //    const c = parent.clone();
    //    const { children } = self
    //    children.splice(children.indexOf(parent), 0, c);
    //    self.triggerFor(c, 'whenCloned');
    //}

    member _.Hide() = data.visible <- false
    member _.Show() = data.visible <- true
    member s.ComeToFront() =
        let children = options.runtime.children
        let i = children.IndexOf s

        if 0 < i then
            children.RemoveAt i
            children.Insert(0, s)

    member s.GoBackByLayers layer =
        let children = options.runtime.children
        let i = children.IndexOf s
        if 0 <= i then
            children.RemoveAt i
            children.Insert(min children.Count (max 0 (i + layer)), s)

    member self.WhenCloned = fiberPolyWith <| fun action ->
        if isClone then
            registerTaskWithSelf "" &action (self :> Entity) options.runtime.scheduler
            |> ignore

    member _.LookLikeFrom costume =
        for i in 0..costumes.Count-1 do
            if costumes.[i].costumeName = costume then
                base.InternalEntityDrawingData.currentCostumeIndex <- i

        // TODO:
        //if (costume === (this.isSprite ? 'next costume' : 'next backdrop')) { this.showNextCostume(); return; }
        //if (costume === (this.isSprite ? 'previous costume' : 'previous backdrop')) { this.showPreviousCostume(); return; }

    member _.LookLike costume =
        let i = (costume - 1) % costumes.Count
        let i = if i < 0 then i + costumes.Count else i
        base.InternalEntityDrawingData.currentCostumeIndex <- i

    member _.Heading degrees =
        let d = degrees % 360.
        let d = if d > 180. then d - 360. else d
        let d = if d <= -180. then  d + 360. else d
        data.direction <- d

    member _.X = data.x
    member _.Y = data.y
    member _.GotoXY(x, y) =
        let ox = data.x
        let oy = data.y
        if ox = x && oy = y then () else

        data.x <- x
        data.y <- y

    member _.SetColorEffect value =
        let v = value % 200.
        let v = if v < 0. then v + 200. else v
        base.InternalEntityDrawingData.filters.color <- clamp (0., 200.) v

    member _.SetGhostEffect value =
        base.InternalEntityDrawingData.filters.ghost <- clamp (0., 10.) value

    member _.Size
        with set size =
            data.scale <- size / 100.
            if data.scale < 0. then data.scale <- 0.

    member self.DeleteClone() =
        if isClone then
            options.runtime.children.Remove self |> ignore
            options.runtime.scheduler.threads
            |> Colony.ignoreWith (fun thread -> LanguagePrimitives.PhysicalEquality thread.self (self :> Entity))
            Fiber.choice1 (Fiber.singleton Abort)
        else
            Fiber.choice2 Fiber.zero

    member _.WhenGreenFlag = fiberPolyWith <| fun action ->
        if not isClone then
            Runtime.whenGreenFlag action options.runtime

    member _.Broadcast name =
        Runtime.broadcast name options.runtime

    member _.PlaySound(soundName: string) =
        Runtime.Logger.logPlaySound options.runtime soundName
        // TODO:
        ()

[<RequireQualifiedAccess>]
type EventTag =
    | PlaySound

[<Struct>]
[<NoComparison; NoEquality>]
type RuntimeEvent = RuntimeEvent of tag: EventTag * data1: uint64 * data2: obj

[<RequireQualifiedAccess>]
[<NoComparison; NoEquality>]
type Runtime = {
    scheduler: Entity Scheduler

    stage: Stage
    children: Sprite ResizeArray

    log: struct(DateTime * RuntimeEvent) ResizeArray option
}
module Runtime =
    open FiberBuilderDelay

    module Logger =
        let log ({ Runtime.log = log } as runtime) event =
            match log with
            | None -> ()
            | Some log ->
                let time = Scheduler.now runtime.scheduler
                log.Add(struct(time, event))

        let logPlaySound runtime soundName =
            log runtime (RuntimeEvent(EventTag.PlaySound, 0UL, box soundName))

    let make config =
        let rec runtime =
            {
                Runtime.scheduler = Scheduler.make config
                Runtime.stage = stage
                Runtime.children = ResizeArray()
                Runtime.log = Some(ResizeArray())
            }
        and stage = Stage { runtime = runtime; prototype = None }
        runtime

    let run { Runtime.scheduler = s } = Scheduler.run s
    let broadcast name ({ scheduler = scheduler } as runtime) =
        let register (self: Entity) name scheduler =
            for l in self.InternalBroadcastListeners name do
                let task = Generator.getFiber &l
                registerTaskWithSelf "" &task self scheduler |> ignore

        let name = LowerName.ofString name
        register runtime.stage name scheduler
        for c in runtime.children do register c name scheduler

    let broadcastAndWait name ({ Runtime.stage = stage; Runtime.scheduler = scheduler } as runtime) =
        let register (self: Entity) name scheduler runningThreadIds =
            let mutable runningThreadIds = runningThreadIds
            for l in self.InternalBroadcastListeners name do
                let task = Generator.getFiber &l
                let thread = registerTaskWithSelf "" &task self scheduler
                runningThreadIds <- Set.add thread.id runningThreadIds
            runningThreadIds

        let mutable runningThreadIds = Set.empty
        let name = LowerName.ofString name
        runningThreadIds <- register stage name scheduler runningThreadIds
        for c in runtime.children do
            runningThreadIds <- register c name scheduler runningThreadIds

        let runningThreadIds = runningThreadIds
        fiber {
            while scheduler.threads |> Colony.exists (fun t -> Set.contains t.id runningThreadIds) do
                ThreadYield
        }

    let globalRuntime = make {
        startTime = Some <| DateTime(2000, 1, 1)
        flame = Limited <| TimeSpan.FromSeconds (1. / 30.)
        deterministic = DeterministicTime {
            processSpan = TimeSpan.FromTicks 1L
            flameSpan = TimeSpan.FromSeconds (1. / 30.)
        }
    }
    let whenGreenFlag action { stage = self; scheduler = s } =
        let mutable fiber = action
        let mutable threadInfo = struct({ id = -1; name = null }, self :> Entity)
        while
            match Fiber.next &fiber &threadInfo with
            | Yield ThreadYield
            | Yield ThreadYieldForce ->
                registerTaskWithSelf "" &fiber (self :> Entity) s |> ignore
                false

            | Yield ThreadYieldImmediate -> true
            | Yield(Sleep span) ->
                Scheduler.registerFiberWithSelf (self :> Entity) "" (Scheduler.now s + span) &fiber s |> ignore
                false

            | Yield Abort -> failwith "abort"
            | Return() -> false
            do ()

[<AutoOpen>]
module Control =
    let startMainLoop() = Runtime.run Runtime.globalRuntime
    let awaitAtomic (AsAtomic x) = x

    let whenGreenFlag<'a> =
        let f = Runtime.whenGreenFlag
        let r = Runtime.globalRuntime
        fiberPolyWith <| fun action -> f action r

    let whenIReceive name =
        Runtime.globalRuntime.stage.WhenIReceive name

    let broadcast name =
        Runtime.broadcast name Runtime.globalRuntime

    let broadcastAndWait name =
        Runtime.broadcastAndWait name Runtime.globalRuntime

    let waitElapsedFrom seconds = TimeSpan.FromSeconds seconds |> Sleep |> Fiber.singleton
