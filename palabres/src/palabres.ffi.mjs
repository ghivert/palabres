import * as converter from './palabres/internals/converter.mjs'
import * as gleam from './gleam.mjs'
import * as levels from './palabres_level.ffi.mjs'

let logger = null

export function formatIso8601() {
  return new Date().toISOString()
}

export function uuid() {
  return crypto.randomUUID()
}

export function configure(options) {
  logger = new Logger({
    color: options.color,
    json: options.json,
    level: options.level,
    output: options.output,
  })
}

export function log(lvl, message, text, at) {
  logger?.log(lvl, message, text, at)
}

export function destroy() {
  logger?.destroy()
  logger = null
}

class Logger {
  constructor(options) {
    this.buffer = []
    this.level = levels.asInt(options.level)
    this.color = options.color[0] ?? this.#readColor()
    this.json = options.json
    this.output = options.output
    if (this.output.flush_interval) {
      this.interval = setInterval(() => {
        this.#flush()
      }, this.output.flush_interval)
    }
  }

  log(lvl, message, text, at) {
    if (levels.asInt(lvl) < this.level) return
    const msg = this.#formatMessage(lvl, message, text, at)
    if (this.output.filename) return this.buffer.push(msg)
    else console.log(msg)
  }

  destroy() {
    clearInterval(this.interval)
  }

  #formatMessage(lvl, message, text, at) {
    const txt = converter.format_message(text, this.#isColor)
    const id = ['id', gleam.List.fromArray([uuid()])]
    const when = ['when', gleam.List.fromArray([formatIso8601()])]
    const fields = gleam.prepend(when, gleam.prepend(id, message))
    const fields_ = this.#addAtField(fields, at)
    if (this.#isJson) return this.#formatJSON(lvl, fields_, txt)
    const fields__ = converter.to_spaced_query_string(fields_, this.#isColor)
    return `${levels.format(lvl, this.color)} ${fields__} ${txt}`
  }

  #formatJSON(lvl, fields, text) {
    const text_ = converter.format_message(text, this.#isColor)
    const data = converter
      .to_json(fields, text)
      .entries()
      .map(([key, value]) => [key, value.toArray?.() ?? value])
    const data_ = { ...Object.fromEntries(data), level: levels.rawFormat(lvl) }
    return JSON.stringify(data_)
  }

  get #isJson() {
    return this.json
  }

  get #isColor() {
    const isJson = this.#isJson
    const isColored = this.color ?? false
    return !isJson && isColored
  }

  #addAtField(fields, at) {
    if (at[0] === undefined) return fields
    const [mod, fun] = at[0]
    const at_ = converter.format_at(mod, fun, this.#isColor, this.#isJson)
    return gleam.prepend(['at', at_])
  }

  #readColor() {
    const noColor = process.env.NO_COLOUR || process.env.NO_COLOR
    return noColor === undefined
  }

  async #flush() {
    const filename = this.output.filename
    const fs = await import('node:fs')
    const path = await import('node:path')
    const dirname = path.dirname(filename)
    await fs.promises.mkdir(dirname, { recursive: true })
    const message = this.buffer.join('\n')
    this.buffer = []
    await fs.promises.appendFile(path, message)
  }
}
