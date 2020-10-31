// @ts-check
const Runtime = require("scratch-vm/src/engine/runtime")
const Sb3 = require("scratch-vm/src/serialization/sb3")
const Sb2 = require("scratch-vm/src/serialization/sb2")
const ScratchStorage = require("scratch-storage")
const Sb2SpecMap = require("scratch-vm/src/serialization/sb2_specmap")
const Yargs = require("yargs/yargs")
const Util = require("util")
const Path = require("path")
const Ipc = require("node-ipc")
const _Fs = require("fs")
const { clearTimeout } = require("timers")
const Fs = {
    readFile: Util.promisify(_Fs.readFile),
    writeFile: Util.promisify(_Fs.writeFile)
}

const newDummyStorage = () => {
    /** @type {any} */
    const storage = new ScratchStorage()
    const AssetType = storage.AssetType
    storage.addWebSource([AssetType.Project], () => { throw new Error("ProjectUrl") })
    storage.addWebSource([AssetType.ImageVector, AssetType.ImageBitmap, AssetType.Sound], () => { throw new Error("AssetUrl") })
    return storage
}

/**
 * @param {string} sb3ProjectJson
 */
const sb3ToSb3Json = async sb3ProjectJson => {
    try {
        const runtime = new Runtime()
        // runtime.attachStorage(newDummyStorage())

        const { targets } = await Sb3.deserialize(JSON.parse(sb3ProjectJson), runtime, undefined, undefined)
        runtime.targets = targets

        return JSON.stringify(Sb3.serialize(runtime))
    }
    catch (e) { e.source = sb3ProjectJson; throw e }
}
/**
 * @param {string} sb2ProjectJson
 */
const sb2ToSb3Json = async sb2ProjectJson => {
    try {
        const runtime = new Runtime()
        // runtime.attachStorage(newDummyStorage())

        const { targets } = await Sb2.deserialize(JSON.parse(sb2ProjectJson), runtime, undefined, undefined)
        runtime.targets = targets
        return JSON.stringify(Sb3.serialize(runtime))
    }
    catch (e) { e.source = sb2ProjectJson; throw e }
}

/**
 * @param {string} path
 */
const fileNameWithoutExtension = path => {
    return path.split(".").slice(0, -1).join(".")
}

/**
 * @typedef {object} FileConverterParams
 * @property {string} jsonPath
 * @property {string} [encoding]
 * @property {string} [outPath]
 */
const makeFileConverter = (/** @type {(input: string) => Promise<string>} */ convertAsync) => async (/** @type {Readonly<FileConverterParams>} */ args) => {
    const { jsonPath, encoding, outPath } = args

    let jsonContents = await Fs.readFile(jsonPath, encoding)
    jsonContents = await convertAsync(jsonContents)

    const resultJsonPath = outPath || `${fileNameWithoutExtension(jsonPath)}.2${Path.extname(jsonPath)}`
    await Fs.writeFile(resultJsonPath, jsonContents)

    console.log(`wrote: ${Path.resolve(resultJsonPath)}`)
}

const sb3ToSb3JsonFile = makeFileConverter(sb3ToSb3Json)
const sb2ToSb3JsonFile = makeFileConverter(sb2ToSb3Json)

const writeSb2Spec = async ({ path }) => {
    const json = JSON.stringify(Sb2SpecMap, (_, v) => (typeof v === "function") ? { $type: "function", value: String(v) } : v)
    await Fs.writeFile(path, json)
    console.log(`wrote: ${Path.resolve(path)}`)
}

let createInnerArgv = () => Yargs()
    .command(
        ["roundtrip-json <jsonPath>"],
        "Read and save the sb3 project.json file",
        p => p
            .positional("jsonPath", { type: "string", demandOption: true })
            .option("encoding", { type: "string", default: "utf8" })
            .option("outPath", { type: "string" }),

        sb3ToSb3JsonFile
    )
    .command(
        ["import-sb2-json <jsonPath>"],
        "Read sb2 project.json and save the sb3 project.json file",
        p => p
            .positional("jsonPath", { type: "string", demandOption: true })
            .option("encoding", { type: "string", default: "utf8" })
            .option("outPath", { type: "string" }),

        sb2ToSb3JsonFile
    )
    .command(
        ["write-specs <path>"],
        "Write Sb2 spec map json file",
        p => p.positional("path", { type: "string", demandOption: true }),
        async ({ path }) => writeSb2Spec({ path })
    )
    .demandCommand(1)
    .help()

let innerArgv = createInnerArgv()

const errorToJsonable = (/** @type {Error} */ error) => {
    /**
     * @type {{ [K in keyof Error]: Error[K] }}
     */
    const result = Object.create(null)
    function copyProps(error) {
        if (error == null) { return }

        copyProps(Object.getPrototypeOf(error))

        Object.getOwnPropertyNames(error).forEach(key =>
            result[key] = error[key]
        )
        Object.getOwnPropertySymbols(error).forEach(key =>
            result[key] = error[key]
        )
    }
    copyProps(error)
    return result
}

/**
 * @template T
 * @typedef {{ tag: "Ok", value: T } | { tag: "Error", value: unknown }} Result
 */
/**
 * @template T
 * @param {() => Promise<T>} asyncAction
 * @returns {Promise<Result<T>>}
 */
const protectedCallAsync = async asyncAction => {
    try {
        return { tag: "Ok", value: await asyncAction() }
    }
    catch (e) {
        if (e instanceof Error) { e = errorToJsonable(e) }
        return { tag: "Error", value: e }
    }
}

/**
 * @template {string} TName
 * @template TArgs
 * @typedef {{ name: TName, args: TArgs }} CommandArg
 */
/**
 * @typedef {
    | CommandArg<"roundtrip-json", { projectJson: string }>
    | CommandArg<"import-sb2-json", { projectJson: string }>
    } CommandArgs
 */
const startServer = (/** @type {{ id: string, silent: boolean, timeout: number }} */ { id, silent, timeout }) => {
    const ipc = new Ipc.IPC()
    ipc.config.id = id
    ipc.config.retry = 1500
    ipc.config.silent = silent

    /**  @type {ReturnType<typeof setTimeout> | null} */
    let serverTimeoutId = null
    const resetServerTimeout = () => {
        if (serverTimeoutId) { clearTimeout(serverTimeoutId) }

        serverTimeoutId = setTimeout(() => {
            ipc.server.stop()
            throw new Error(`timeout ${timeout}ms`)
        }, timeout)
    }

    ipc.serve(() => {
        resetServerTimeout()

        ipc.server.on("echo", async (data, socket) => {
            resetServerTimeout()
            ipc.server.emit(socket, "echo", data)
        })
        ipc.server.on("exec", async (/** @type {CommandArgs} */ data, /** @type {import("net").Socket} */ socket) => {
            resetServerTimeout()
            const result = await protectedCallAsync(async () => {
                switch (data.name) {
                    case "roundtrip-json": return { projectJson: await sb3ToSb3Json(data.args.projectJson) }
                    case "import-sb2-json": return { projectJson: await sb2ToSb3Json(data.args.projectJson) }
                }
            })
            ipc.server.emit(socket, "exec", result)
        })

        ipc.server.on("stop", () => {
            clearTimeout(serverTimeoutId)
            ipc.server.stop()
        })
    })
    ipc.server.start()
}

createInnerArgv()
    .command(
        ["start-server"],
        "Start ipc server",
        p => p
            .option("id", { type: "string", default: "adaptorjs" })
            .option("silent", { type: "boolean", default: false })
            .option("timeout", { type: "number", default: 1 * 60 * 1000, description: "ms" }),
        startServer
    )
    .recommendCommands()
    .strict()
    .parse(process.argv.slice(2), {})
