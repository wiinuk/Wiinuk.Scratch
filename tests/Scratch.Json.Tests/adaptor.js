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
    const runtime = new Runtime()
    // runtime.attachStorage(newDummyStorage())

    const { targets } = await Sb3.deserialize(JSON.parse(sb3ProjectJson), runtime, undefined, undefined)
    runtime.targets = targets

    return JSON.stringify(Sb3.serialize(runtime))
}
/**
 * @param {string} sb2ProjectJson
 */
const sb2ToSb3Json = async sb2ProjectJson => {
    const runtime = new Runtime()
    // runtime.attachStorage(newDummyStorage())

    const { targets } = await Sb2.deserialize(JSON.parse(sb2ProjectJson), runtime, undefined, undefined)
    runtime.targets = targets

    return JSON.stringify(Sb3.serialize(runtime))
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


/*
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
        */
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
const startServer = (/** @type {{ id: string }} */ { id }) => {
    const ipc = new Ipc.IPC()
    ipc.config.id = id
    ipc.config.retry = 1500
    ipc.serve(() => {
        // ipc.config.silent = true
        ipc.server.on("echo", async (data, socket) => {
            ipc.server.emit(socket, "echo", data)
        })
        ipc.server.on("exec", async (/** @type {CommandArgs} */ data, /** @type {import("net").Socket} */ socket) => {
            let result = null
            switch (data.name) {
                case "roundtrip-json": result = await sb3ToSb3Json(data.args.projectJson); break
                case "import-sb2-json": result = await sb2ToSb3Json(data.args.projectJson); break
            }
            ipc.server.emit(socket, "exec", result)
        })
        ipc.server.on("stop", () => {
            ipc.server.stop()
        })
    })
    ipc.server.start()
}

// ipc.connectTo("world", function() {
//   ipc.of.world.on("connect", function() {
//     ipc.log("## connected to world ##", ipc.config.delay);
//     ipc.of.world.emit("app.message", "ping");
//   });
//   ipc.of.world.on("disconnect", function() {
//     ipc.log("disconnected from world");
//   });
//   ipc.of.world.on("app.message", function(data) {
//     ipc.log("got a message from world : ", data);
//   });
//   console.log(ipc.of.world.destroy);
// });

createInnerArgv()
    .command(
        ["start-server"],
        "Start ipc server",
        p => p
            .option("id", { type: "string", default: "adaptorjs" }),
        startServer
    )
    .parse(process.argv.slice(2), {}, (error, _reply, output) => { if (error) { throw new Error(output) } })
