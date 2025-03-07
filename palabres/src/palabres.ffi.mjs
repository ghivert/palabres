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

export function isJson() {
  return logger?.json ?? false
}

export function isColor() {
  return !isJson() && (logger?.color ?? false)
}

export function configure(options) {
  logger = new Logger({
    color: options.color,
    json: options.json,
    level: options.level,
    output: options.output,
  })
}

export function log(lvl, message, text) {
  logger?.log(lvl, message, text)
}

export function destroy() {
  logger?.destroy()
  logger = null
}

export function encodeJson(value) {
  return JSON.parse(value)
}

function readColor() {
  const noColor = process.env.NO_COLOUR || process.env.NO_COLOR
  return noColor === undefined
}

class Logger {
  constructor(options) {
    this.buffer = []
    this.level = levels.asInt(options.level)
    this.color = options.color[0] ?? readColor()
    this.json = options.json
    this.output = options.output
    if (this.output.flush_interval) {
      this.interval = setInterval(() => {
        this.#flush()
      }, this.output.flush_interval)
    }
  }

  log(lvl, message, text) {
    if (levels.asInt(lvl) < this.level) return
    const msg = this.#formatMessage(lvl, message, text)
    if (this.output.filename) return this.buffer.push(msg)
    else console.log(msg)
  }

  destroy() {
    clearInterval(this.interval)
  }

  #formatMessage(lvl, message, text) {
    const id = ['id', gleam.List.fromArray([uuid()])]
    const when = ['when', gleam.List.fromArray([formatIso8601()])]
    const fields = gleam.prepend(when, gleam.prepend(id, message))
    if (isJson()) return this.#formatJSON(lvl, fields, text)
    const fields_ = converter.to_spaced_query_string(fields)
    return `${levels.format(lvl, this.color)} ${fields_} ${text}`
  }

  #formatJSON(lvl, fields, text) {
    const data = converter
      .to_json(fields, text)
      .entries()
      .map(([key, value]) => [key, value.toArray?.() ?? value])
    const data_ = { ...Object.fromEntries(data), level: levels.rawFormat(lvl) }
    return JSON.stringify(data_)
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
