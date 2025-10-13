import * as $converter from './palabres/internals/converter.mjs'
import * as $field from './palabres/internals/field.mjs'
import * as levels from './palabres/level.ffi.mjs'
import * as $list from '../gleam_stdlib/gleam/list.mjs'

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
    defaultFields: options.default_fields,
  })
}

export function log(lvl, fields, message, at) {
  logger?.log(lvl, fields, message, at)
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
    this.defaultFields = options.defaultFields
    const timer = this.output.flush_interval[0]
    if (timer) this.interval = setInterval(() => this.#flush(), timer)
  }

  log(lvl, fields, message, at) {
    if (levels.asInt(lvl) < this.level) return
    const msg = this.#formatMessage(lvl, fields, message, at)
    if (this.output.filename) return this.buffer.push(msg)
    else console.log(msg)
  }

  destroy() {
    clearInterval(this.interval)
    this.#flush()
  }

  #formatMessage(lvl, fields, message, at) {
    const when = $field.string(formatIso8601())
    const f1 = $field.append(fields, 'when', when)
    const f2 = $list.append(f1, this.defaultFields)
    const f3 = $converter.append_at(f2, at, this.#isColor, this.#isJson)
    if (this.#isJson) return this.#formatJSON(lvl, f3, message)
    const msg = $converter.format_message(message, this.#isColor)
    const f4 = $converter.to_spaced_query_string(f3, this.#isColor)
    return `${levels.format(lvl, this.color)} ${f4} ${msg}`
  }

  #formatJSON(lvl, fields, message) {
    const message_ = $converter.format_message(message, this.#isColor)
    const data = $converter
      .to_json(fields, message_)
      .entries()
      .map(([key, value]) => [key, value?.toArray?.() ?? value ?? null])
    const data_ = { ...Object.fromEntries(data), level: levels.rawFormat(lvl) }
    return JSON.stringify(data_)
  }

  get #isJson() {
    return this.json
  }

  get #isColor() {
    const isJson = this.json
    const isColored = this.color ?? false
    return !isJson && isColored
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
    await fs.promises.appendFile(filename, message)
  }
}
