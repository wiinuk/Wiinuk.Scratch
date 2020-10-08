namespace Scratch.Executor
open Scratch
open Executions

module RuntimeVersion =
    [<Struct>]
    type Sb2 = private | Sb2 with
        interface IRuntimeVersion with
            member _.DeleteListLine(_, line, list) = Blocks.deleteLineSb2 line list
            member _.LetterOf(_, target, nth) = Blocks.letterOfSb2 target nth

    [<Struct>]
    type Sb3 = private | Sb3 with
        interface IRuntimeVersion with
            member _.DeleteListLine(env, line, list) = Blocks.deleteLineSb3 &env.environment.custom line list
            member _.LetterOf(_, target, nth) = Blocks.letterOfSb3 target nth

    let sb2 = Sb2
    let sb3 = Sb3


type ExecutionConfig<'a,'Random,'Observer,'Input,'View,'Cloud,'Version> = {
    random: 'Random
    observer: 'Observer
    input: 'Input
    view: 'View
    cloud: 'Cloud
    version: 'Version
    showLocation: 'a -> string

    userId: int
    userName: string
}
module ExecutionConfig =
    let withRandom value config = {
        showLocation = config.showLocation
        random = value
        observer = config.observer
        input = config.input
        version = config.version
        view = config.view
        cloud = config.cloud

        userId = config.userId
        userName = config.userName
    }
    let withObserver value config = {
        showLocation = config.showLocation
        random = config.random
        observer = value
        input = config.input
        version = config.version
        view = config.view
        cloud = config.cloud

        userId = config.userId
        userName = config.userName
    }
    let withInput value config = {
        showLocation = config.showLocation
        random = config.random
        observer = config.observer
        input = value
        version = config.version
        view = config.view
        cloud = config.cloud

        userId = config.userId
        userName = config.userName
    }
    let withView value config = {
        showLocation = config.showLocation
        random = config.random
        observer = config.observer
        input = config.input
        version = config.version
        view = value
        cloud = config.cloud

        userId = config.userId
        userName = config.userName
    }
    let withCloud value config = {
        showLocation = config.showLocation
        random = config.random
        observer = config.observer
        input = config.input
        version = config.version
        view = config.view
        cloud = value

        userId = config.userId
        userName = config.userName
    }
    let withVersion value config = {
        showLocation = config.showLocation
        random = config.random
        observer = config.observer
        input = config.input
        version = value
        view = config.view
        cloud = config.cloud

        userId = config.userId
        userName = config.userName
    }
    let makeDefaultConfig seed = {
        random = Random.system seed
        observer = ExecutionObserver.ignore
        input = Input.nil
        view = StageView.ignore
        cloud = Cloud.offline()
        version = RuntimeVersion.sb2
        showLocation = sprintf "%A"

        userId = 0
        userName = ""
    }

