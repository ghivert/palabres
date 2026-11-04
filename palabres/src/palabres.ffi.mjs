import * as fs from 'node:fs'
import * as path from 'node:path'
import * as $converter from './palabres/internals/converter.mjs'
import * as $field from './palabres/field.mjs'
import * as $internalField from './palabres/internals/field.mjs'
import { Location } from './palabres/internals/location.mjs'
import * as $dict from '../gleam_stdlib/gleam/dict.mjs'
import * as $list from '../gleam_stdlib/gleam/list.mjs'
import { Some, None } from '../gleam_stdlib/gleam/option.mjs'

let logger = null

function formatIso8601() {
  return new Date().toISOString()
}

export function configure(options) {
  // When `configure` is called a second time, the previous logger must
  // release its file before being replaced.
  logger?.destroy()
  logger = new Logger({
    autoAt: options.auto_at,
    autoWhen: options.auto_when,
    color: options.color,
    json: options.json,
    level: options.level,
    output: options.output,
    fields: options.fields,
  })
}

// Files holding the Palabres frames above the user function: this FFI, and
// the compiled `palabres` module. In a Gleam build they identify Palabres
// frames whatever their number. Counting frames would not do: JavaScriptCore
// (Bun) eliminates tail calls, so `info` calling `init` leaves no frame.
const PALABRES_FILE = /\/palabres\/palabres(\.ffi)?\.mjs$/

// In a bundle, every frame shares the bundle's file name, and the Palabres
// frames can only be counted: `locate`, `init`, and the level constructor
// (`info`, `debug`, `log_info`…) on V8. Best effort only.
const PALABRES_FRAMES = 3

// Locate the function creating the log, only when the log is going to be
// printed, to avoid paying for a stack inspection otherwise. The stack is
// read as structured V8 call sites through `Error.prepareStackTrace`, which
// avoids formatting and parsing the stack string, and is unaffected by source
// maps. Supported on Node, Deno and Bun. Anything else gets no location.
export function locate(level) {
  if (!logger?.autoAt || $converter.level_to_int(level) < logger.level) return new None()
  const { prepareStackTrace, stackTraceLimit } = Error
  Error.stackTraceLimit = PALABRES_FRAMES + 1
  Error.prepareStackTrace = (_, callSites) => callSites
  const callSites = new Error().stack
  Error.prepareStackTrace = prepareStackTrace
  Error.stackTraceLimit = stackTraceLimit
  if (!Array.isArray(callSites)) return new None()
  const site = callSites[userFrame(callSites)]
  if (typeof site?.getFileName !== 'function') return new None()
  const module = moduleName(site.getFileName())
  if (module === null) return new None()
  const fn = site.getFunctionName()
  return new Some(new Location(module, fn ? new Some(fn) : new None()))
}

// Index of the first frame outside Palabres. The first frame is `locate`
// itself: when its file is not recognised, the application is bundled, and
// frames are counted instead.
function userFrame(callSites) {
  const isPalabres = site => PALABRES_FILE.test(site?.getFileName?.() ?? '')
  if (!isPalabres(callSites[0])) return PALABRES_FRAMES
  const index = callSites.findIndex(site => !isPalabres(site))
  return index === -1 ? callSites.length : index
}

// Gleam compiles `my_app/web/router` to `build/<profile>/javascript/my_app/
// my_app/web/router.mjs`. Recover the import path from the file name. When
// the application is bundled, the path is unknown, and the file name is used.
function moduleName(file) {
  if (!file) return null
  const gleam = file.match(/\/javascript\/[^/]+\/(.+)\.mjs$/)
  if (gleam) return gleam[1]
  const bundle = file.match(/([^/\\]+)\.[cm]?js$/)
  return bundle ? bundle[1] : null
}

export function log(lvl, fields, message, at) {
  logger?.log(lvl, fields, message, at)
}

// Whether a log at that level would be printed. Without a configured logger,
// nothing is printed, so nothing deferred is worth computing.
export function isAllowed(lvl) {
  return logger !== null && $converter.level_to_int(lvl) >= logger.level
}

export function destroy() {
  logger?.destroy()
  logger = null
}

class Logger {
  constructor(options) {
    this.autoAt = options.autoAt
    this.autoWhen = options.autoWhen
    this.level = $converter.level_to_int(options.level)
    this.color = options.color[0] ?? this.#readColor()
    this.json = options.json
    this.output = options.output
    this.fields = options.fields
    this.formatting = false
    // A file output goes through a single stream, opened in append mode for
    // the whole life of the logger. The stream queues the lines and writes
    // them as fast as the file accepts them, and pending writes keep the
    // process alive until they are done, so nothing is lost at exit.
    // `flush_interval` has thus no effect on JavaScript.
    if (this.output.filename) this.stream = this.#openStream(this.output.filename)
  }

  log(lvl, fields, message, at) {
    if ($converter.level_to_int(lvl) < this.level) return
    // A lazy global field can itself log, which formats a new log while this
    // one is being formatted. The nested log is formatted without global
    // fields, otherwise the lazy field would be evaluated again, forever.
    const nested = this.formatting
    this.formatting = true
    try {
      const msg = this.#formatMessage(lvl, fields, message, at, nested)
      if (this.stream) this.stream.write(msg + '\n')
      else console.log(msg)
    } finally {
      this.formatting = nested
    }
  }

  destroy() {
    this.stream?.end()
    this.stream = null
  }

  #openStream(filename) {
    fs.mkdirSync(path.dirname(filename), { recursive: true })
    const stream = fs.createWriteStream(filename, { flags: 'a' })
    // An unwritable file must not crash the application: it is reported
    // once on stderr, and the logs are dropped.
    let reported = false
    stream.on('error', error => {
      if (reported) return
      reported = true
      console.error(`palabres: unable to write logs in ${filename}: ${error.message}`)
    })
    return stream
  }

  #formatMessage(lvl, fields, message, at, nested) {
    // Global fields are resolved on every log, as they can be lazy. A field
    // set on the log wins over a global field with the same key.
    const f1 = nested ? fields : $internalField.merge(fields, $field.resolve(this.fields))
    const f2 = this.autoWhen ? $converter.prepend_when(f1, formatIso8601()) : f1
    const f3 = $converter.prepend_at(f2, at, this.#isColor, this.#isJson)
    if (this.#isJson) return this.#formatJSON(lvl, f3, message)
    const msg = $converter.format_message(message, this.#isColor)
    const f4 = $converter.to_spaced_query_string(f3, this.#isColor)
    // Both the fields and the message can be empty: skip them.
    const level = $converter.format_level(lvl, this.color)
    return [level, f4, msg].filter(Boolean).join(' ')
  }

  #formatJSON(lvl, fields, message) {
    const message_ = $converter.format_message(message, this.#isColor)
    const data = $dict
      .to_list($converter.to_json(fields, message_))
      .toArray()
      .map(([key, value]) => [key, value?.toArray?.() ?? value ?? null])
      // Keys are sorted to get a deterministic output, consistent with Erlang.
      .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))
    const level = $converter.level_to_string(lvl)
    const data_ = { ...Object.fromEntries(data), level }
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

  // Follow the NO_COLOR convention (https://no-color.org): colour is
  // deactivated when the variable is defined with any non-empty value.
  #readColor() {
    const flags = [process.env.NO_COLOUR, process.env.NO_COLOR]
    return !flags.some(flag => flag !== undefined && flag !== '')
  }
}
