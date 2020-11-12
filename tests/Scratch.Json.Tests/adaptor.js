// @ts-check
"use strict";

globalThis.Blob = /** @type {any} */ (require("fetch-blob"))
const Vm = require("scratch-vm/src/virtual-machine.js")
const Runtime = require("scratch-vm/src/engine/runtime")
const Sb3 = require("scratch-vm/src/serialization/sb3")
const Sb2 = require("scratch-vm/src/serialization/sb2")
const ScratchStorage = require("scratch-storage")
const Sb2SpecMap = require("scratch-vm/src/serialization/sb2_specmap")
const validate = require("scratch-parser/lib/validate")
const Yargs = require("yargs/yargs")
const Util = require("util")
const Ws = require("ws")
const Path = require("path")
const { promises: Fs } = require("fs")

const newDummyStorage = () => new ScratchStorage()

/**
 * @param {object} sb2OrSb3ProjectJsonObject
 * @returns {Promise<void>}
 */
const validateProject = sb2OrSb3ProjectJsonObject => new Promise((onSuccess, onError) =>
    validate(false, sb2OrSb3ProjectJsonObject, validationErrors => {
        if (validationErrors == null) { onSuccess() }
        else {
            onError(new (class ValidationError extends Error {
                validationErrors = validationErrors
            }))
        }
    })
)

/**
 * @template {string} TName
 * @template TValue
 * @param {Error | unknown} error
 * @param {TName extends keyof Error ? never : TName} propertyName
 * @param {TValue} propertyValue
 */
const assignErrorInfo = (error, propertyName, propertyValue) => {
    if (error instanceof Error) {
        error[/** @type {string} */ (propertyName)] = propertyValue
        return error
    }
    else {
        class ErrorWithInfo extends Error {
            underlyingError = error
            [propertyName] = propertyValue
        }
        return new ErrorWithInfo()
    }
}

/**
 * @param {string} sb3ProjectJson
 */
const sb3ToSb3Json = async sb3ProjectJson => {
    try {
        const runtime = new Runtime()
        // runtime.attachStorage(newDummyStorage())

        const project = JSON.parse(sb3ProjectJson)
        await validateProject(project)
        const { targets } = await Sb3.deserialize(project, runtime, undefined, undefined)
        runtime.targets = targets

        return JSON.stringify(Sb3.serialize(runtime))
    }
    catch (e) { throw assignErrorInfo(e, "source", sb3ProjectJson) }
}
/**
 * @param {string} sb2ProjectJson
 */
const sb2ToSb3Json = async sb2ProjectJson => {
    try {
        const runtime = new Runtime()
        // runtime.attachStorage(newDummyStorage())

        const project = JSON.parse(sb2ProjectJson)
        await validateProject(project)
        const { targets } = await Sb2.deserialize(project, runtime, undefined, undefined)
        runtime.targets = targets
        return JSON.stringify(Sb3.serialize(runtime))
    }
    catch (e) { throw assignErrorInfo(e, "source", sb2ProjectJson) }
}
/**
 * @param {string} sb3BinaryBase64
 */
const sb3ToSb3Binary = async sb3BinaryBase64 => {
    try {
        const vm = new Vm()
        vm.attachStorage(newDummyStorage())

        const sb3Buffer = Buffer.from(sb3BinaryBase64, "base64")
        await vm.loadProject(sb3Buffer)

        const result = await /** @type {Promise<Blob>} */ (/** @type {unknown} */ (vm.saveProjectSb3()))
        return Buffer.from(await result.arrayBuffer()).toString("base64")
    }
    catch (e) {
        const maxLength = 1000
        const source =
            (maxLength < sb3BinaryBase64.length) ? (sb3BinaryBase64.substr(0, maxLength) + "...") :
            sb3BinaryBase64

        throw assignErrorInfo(e, "source", source)
    }
}

/**
 * @param {string} path
 */
const fileNameWithoutExtension = path => {
    return path.split(".").slice(0, -1).join(".")
}

const defaultFilePathNaming = (/** @type {string} */ basePath, /** @type {number} */ index) =>
    `${fileNameWithoutExtension(basePath)}.${index}${Path.extname(basePath)}`

/**
 * @param {string} basePath
 * @param {Parameters<typeof Fs.writeFile>[1]} contents
 * @param {{ naming?: typeof defaultFilePathNaming, maxRetry?: number }} [options]
 */
const writeFileWithFleshName = async (basePath, contents, options = {}) => {
    const { naming = defaultFilePathNaming, maxRetry = 50 } = options
    const writeNewFile = async (/** @type {string} */ path) => {
        await Fs.writeFile(path, contents, { flag: "wx" })
        return path
    }
    try {
        return await writeNewFile(basePath)
    }
    catch (_) {
        let count = 2
        while (true) {
            try {
                return await writeNewFile(naming(basePath, count))
            }
            catch (e) {
                if (maxRetry < count) { throw e }
                count++
            }
        }
    }
}

/**
 * @typedef {object} FileConverterParams
 * @property {string} jsonPath
 * @property {string} encoding
 * @property {string} [outPath]
 */
const makeFileConverter = (/** @type {(input: string) => Promise<string>} */ convertAsync) => async (/** @type {Readonly<FileConverterParams>} */ args) => {
    const { jsonPath, encoding, outPath } = args

    switch (encoding) {
        case "ascii":
        case "utf8":
        case "utf-8":
        case "utf16le":
        case "ucs2":
        case "ucs-2":
        case "base64":
        case "latin1":
        case "binary":
        case "hex": break
        default: throw new Error(`unknown file encoding: ${encoding}`)
    }

    let jsonContents = await Fs.readFile(jsonPath, encoding)
    jsonContents = await convertAsync(jsonContents)

    const resultJsonPath =
        outPath ? (await Fs.writeFile(outPath, jsonContents, encoding), outPath) :
        await writeFileWithFleshName(jsonPath, jsonContents)

    console.log(`wrote: ${Path.resolve(resultJsonPath)}`)
}

const sb3ToSb3JsonFile = makeFileConverter(sb3ToSb3Json)
const sb2ToSb3JsonFile = makeFileConverter(sb2ToSb3Json)
const packageToSb3File = async (/** @type {{ path: string, outPath?: string }} */ args) => {
    const { path, outPath } = args

    const contents = await Fs.readFile(path)
    const resultBase64 = await sb3ToSb3Binary(contents.toString("base64"))
    const result = Buffer.from(resultBase64, "base64")

    const resultJsonPath =
        outPath ? (await Fs.writeFile(outPath, result), outPath) :
        await writeFileWithFleshName(path, result)

    console.log(`wrote: ${Path.resolve(resultJsonPath)}`)
}

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
    .command(
        ["roundtrip-package <path>"],
        "Read and save the project file",
        p => p
            .positional("packagePath", { type: "string", demandOption: true })
            .option("outPath", { type: "string" }),
        packageToSb3File
    )
    .demandCommand(1)
    .help()

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
 * @param {number} timeout
 * @param {() => void} timeoutAction
 */
const startAliveTimer = (timeout, timeoutAction) => {

    /**  @type {ReturnType<typeof setTimeout>} */
    let serverTimeoutId = setTimeout(timeoutAction, timeout)

    return {
        reset() {
            clearTimeout(serverTimeoutId)
            serverTimeoutId = setTimeout(timeoutAction, timeout)
        },
        clear() {
            clearTimeout(serverTimeoutId)
        }
    }
}

/**
 * @typedef {boolean | number | string | bigint} LiteralOrSuperType
 * @typedef {null | undefined | LiteralOrSuperType} Literal
 */
/**
 * @template T
 * @typedef {T extends LiteralOrSuperType ? LiteralOrSuperType extends T ? never : T : never} LiteralOrNever
 */
/**
 * @typedef {object} StringSchema
 * @property {"string"} kind
 */
/**
 * @typedef {object} LiteralSchema
 * @property {"literal"} kind
 * @property {Literal} value
 */
/**
 * @typedef {object} UnknownSchema
 * @property {"unknown"} kind
 */
/**
 * @typedef {object} PropertySchema
 * @property {ValueSchema} valueSchema
 */
/**
 * @typedef {object} InterfaceSchema
 * @property {"interface"} kind
 * @property {Map<string, PropertySchema>} propertySchemas
 */
/**
 * @typedef {object} UnionSchema
 * @property {"union"} kind
 * @property {Array<ValueSchema>} schemas
 */
/**
 * @typedef {
    | UnknownSchema
    | LiteralSchema
    | StringSchema
    | InterfaceSchema
    | UnionSchema
   } ValueSchema
 */
/**
 * @typedef {object} ValidationResult
 * @property {Array<string | number>} path
 * @property {ValueSchema} schema
 * @property {unknown} actualValue
 */

class ValidationError extends Error {
    /**
     * @param {readonly (string | number)[]} path
     * @param {Readonly<ValueSchema>} schema
     * @param {unknown} actualValue
     */
    constructor (path, schema, actualValue) {
        /**
         * @param {readonly (string | number)[]} path
         */
        const showPath = path => {
            const result = ["$"]
            path.forEach(x => {
                switch (typeof x) {
                    case "string": result.push(".", x); break
                    case "number": result.push("[", String(x), "]"); break
                }
            })
            return result.join("")
        }
        super(`expected type: \`${new Schema(schema).showType()}\`\nactual value: ${actualValue}\nat: ${showPath(path)}`)
        this.path = path
        this.schema = schema
        this.actualValue = actualValue
    }
}

/**
 * @template T
 * @typedef {T extends Readonly<Schema<infer X>> ? X : never} SchemaTarget
 */

/** @template T */
class Schema {
    /**
     * @param {Readonly<ValueSchema>} schema
     */
    constructor(schema) { this._schema = schema }

    /** @private @type {Readonly<Schema<{}>>} */
    static _empty = new Schema({
        kind: "interface",
        propertySchemas: new Map(),
    })
    static get empty() { return Schema._empty }

    /** @private @type {Readonly<Schema<string>>} */
    static _string = new Schema({
        kind: "string",
    })
    static get string() { return Schema._string }

    /** @private @type {Readonly<Schema<unknown>>} */
    static _unknown = new Schema({
        kind: "unknown",
    })
    static get unknown() { return Schema._unknown }

    /** @private @type {Readonly<Schema<undefined>>} */
    static _undefined = /** @type {Schema<undefined>} */ (Schema.literal(/** @type {0} */(undefined)))
    static get undefined() { return Schema._undefined }

    /** @private @type {Readonly<Schema<null>>} */
    static _null = /** @type {Schema<null>} */ (Schema.literal(/** @type {0} */(null)))
    static get null() { return Schema._null }

    /** @type {<TLiteral = unknown>(value: LiteralOrNever<TLiteral>) => Schema<typeof value>} */
    static literal(value) {
        return new Schema({
            kind: "literal",
            value
        })
    }
    /**
     * @template {{ readonly [K in keyof TObject]: Readonly<Schema<unknown>> }} TObject
     * @param {{ [K in keyof TObject]: TObject[K] }} keyAndValueSchemas
     * @returns {Schema<{ [K in keyof TObject]: SchemaTarget<TObject[K]> }>}
     */
    static properties(keyAndValueSchemas) {
        /** @type {Map<string, PropertySchema>} */
        const propertySchemas = new Map()
        Object.keys(keyAndValueSchemas).forEach(k =>
            propertySchemas.set(k, { valueSchema: keyAndValueSchemas[/** @type {keyof TObject} */ (k)]._schema })
        )
        return new Schema({
            kind: "interface",
            propertySchemas: propertySchemas,
        })
    }
    /** @type {<TSchemas extends Readonly<Schema<unknown>>[]>(...schemas: TSchemas) => Schema<SchemaTarget<TSchemas[number]>>} */
    static union(...schemas) {
        return new Schema({
            kind: "union",
            schemas: schemas.map(s => s._schema)
        })
    }

    showType() {
        const appendType = (/** @type {ValueSchema} */ schema, /** @type {unknown[]} */ result) => {
            switch (schema.kind) {
                case "unknown": return result.push("unknown")
                case "string": return result.push("string")
                case "literal":
                    const { value } = schema
                    switch (value) {
                        case null: return result.push("null")
                        case undefined: return result.push("undefined")
                        default:
                            switch (typeof value) {
                                case "bigint": return result.push(value, "n")
                                case "boolean":
                                case "number": return result.push(value)
                                case "string": return result.push(JSON.stringify(value))
                            }
                    }

                case "interface":
                    const { propertySchemas } = schema
                    result.push("{ ")
                    for (const k in propertySchemas.keys()) {
                        result.push(k, ": ")
                        appendType(propertySchemas.get(k).valueSchema, result)
                        result.push("; ")
                    }
                    result.push(" }")
                    return

                case "union":
                    const { schemas } = schema
                    if (schemas.length === 0) { return result.push("never") }

                    appendType(schemas[0], result)
                    for (let i = 1; i < schemas.length; i++) {
                        result.push(" | ")
                        appendType(schemas[i], result)
                    }
                    return
            }
        }
        const result = []
        appendType(this._schema, result)
        return result.join("")
    }
    /**
     * @param {unknown} x
     */
    validate(x) {
        /** @type {(schema: Readonly<ValueSchema>, path: (string | number)[], actualValue: unknown) => ValidationResult | null} */
        const validateBy = (schema, path, actualValue) => {
            switch (schema.kind) {
                case "unknown": return null
                case "literal":
                    if (actualValue === schema.value) { return null }
                    return { path, schema: schema, actualValue }

                case "string":
                    if (typeof actualValue === "string") { return null }
                    return { path, schema: schema, actualValue }

                case "interface":
                    const { propertySchemas } = schema
                    for (const k of propertySchemas.keys()) {
                        let v = undefined
                        try { v = actualValue[k] } catch(_) {}
                        const valueSchema = propertySchemas.get(k).valueSchema
                        const r = validateBy(valueSchema, path.concat(k), v)
                        if (r) { return r }
                    }
                    return null

                case "union":
                    const { schemas } = schema
                    for (const s of schemas) {
                        const r = validateBy(s, path, actualValue)
                        if (r == null) { return null }
                    }
                    return { path, schema, actualValue }
            }
        }
        return validateBy(this._schema, [], x)
    }

    /**
     * @param {unknown} x
     * @returns {x is T}
     */
    isAssignableFrom(x) { return !this.validate(x) }

    /**
     * @param {unknown} x
     * @returns {asserts x is T}
     */
    assert(x) {
        const r = this.validate(x)
        if (r) { throw new ValidationError(r.path, r.schema, r.actualValue) }
    }
}
const { properties, literal, union, string, unknown } = Schema
const messageSchema = union(
    properties({ type: literal("echo"), data: unknown }),
    properties({ type: literal("stop"), data: unknown }),
    properties({
        type: literal("exec"),
        data: union(
            properties({
                name: literal("roundtrip-json"),
                args: properties({
                    projectJson: string
                })
            }),
            properties({
                name: literal("import-sb2-json"),
                args: properties({
                    projectJson: string
                })
            }),
            properties({
                name: literal("roundtrip-package"),
                args: properties({
                    binary: string
                })
            })
        )
    }),
)
/** @type {typeof messageSchema} */
const messageType = messageSchema

const startServer = (/** @type {{ port: number, silent: boolean, timeout: number }} */ { port, silent, timeout }) => {
    if (silent) { require("minilog").disable() }

    const server = new Ws.Server({ port: port })
    const timer = startAliveTimer(timeout, () => {
        server.clients.forEach(c => c.close())
        server.close()
        console.error(new Error(`timeout ${timeout}ms`))
        process.exit(-1)
    })
    const log = (...args) => {
        if (!silent) { console.log(...args) }
    }

    log(`[server] starting server on port ${port}`)

    const utf8 = new Util.TextDecoder("utf-8")
    server.on('connection', socket => {
        timer.reset()

        log(`[server] connected`)

        socket.on('error', e => { throw e })
        socket.on('message', async m => {
            timer.reset()
            log("[server] received", m)

            const json =
                typeof m === "string" ? m :
                Array.isArray(m) ? m.map(b => utf8.decode(b)).join("") :
                utf8.decode(m)

            const message = JSON.parse(json)
            messageType.assert(message)

            switch (message.type) {
                case "echo": {
                    const result = JSON.stringify(message.data)
                    log("[server] send", result)
                    socket.send(JSON.stringify(result))
                    break
                }
                case "exec": {
                    const result = await protectedCallAsync(async () => {
                        const { data } = message
                        switch (data.name) {
                            case "roundtrip-json": return { projectJson: await sb3ToSb3Json(data.args.projectJson) }
                            case "import-sb2-json": return { projectJson: await sb2ToSb3Json(data.args.projectJson) }
                            case "roundtrip-package": return { binary: await sb3ToSb3Binary(data.args.binary) }
                        }
                    })
                    log("[server] send", result)
                    socket.send(JSON.stringify(result))
                    break
                }
                case "stop": {
                    timer.clear()
                    server.clients.forEach(c => c.close())
                    server.close()
                    log(`[server] terminated`)
                    process.exit()
                }
            }
        })
        socket.on('close', () => {
            timer.reset()
            log(`[server] closed '${socket.url}'`)
        })
    })
    server.on('error', e => { throw e })
}

createInnerArgv()
    .command(
        ["start-server"],
        "Start ipc server",
        p => p
            .option("port", { type: "number", default: 443 })
            .option("silent", { type: "boolean", default: false })
            .option("timeout", { type: "number", default: 1 * 60 * 1000, description: "ms" }),
        startServer
    )
    .recommendCommands()
    .strict()
    .parse(process.argv.slice(2), {})
