//// Converters for fields messages. Separated in order to use it in FFI to ease
//// working with external data structures.
//// Should never be leaked.

import gleam/bool
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import palabres/internals/field.{type Field, type Fields}
import palabres/internals/location.{type Location}
import palabres/level.{type Level}

/// Converts a list of fields, to a query string separated by spaces instead
/// of ampersand (`&`). Handles coloring output, or not, depending on the state
/// of palabres logger.
pub fn to_spaced_query_string(fields: Fields, colored: Bool) -> String {
  fields
  |> list.map(fn(field) {
    let #(key, values) = field
    to_query_part(key, values, colored)
  })
  |> string.join(with: " ")
}

/// Converts a list of fields in a JSON string. Color is never applied on JSON.
pub fn to_json(
  fields: Fields,
  message: String,
) -> dict.Dict(String, dynamic.Dynamic) {
  let json_dict = init_json_dict(message)
  use json_dict, #(key, values) <- list.fold(fields, json_dict)
  dict.insert(json_dict, key, {
    case values {
      [value] -> field.to_dynamic(value)
      values -> {
        // Values are stored newest first: reverse them to get the order they
        // were added in, like the query string output does.
        values
        |> list.reverse
        |> list.map(field.to_dynamic)
        |> dynamic.list
      }
    }
  })
}

fn init_json_dict(message: String) {
  case string.is_empty(message) {
    True -> dict.new()
    False -> dict.from_list([#("__message", dynamic.string(message))])
  }
}

fn to_query_part(key: String, values: List(Field), colored: Bool) -> String {
  let values = list.reverse(values)
  let values = list.map(values, field.to_string)
  let uncolored = fn() { key <> "=" <> string.join(values, with: ",") }
  use <- bool.lazy_guard(when: !colored, return: uncolored)
  let values = string.join(values, "\u{1b}[33m,\u{1b}[0m")
  let key = "\u{1b}[32m" <> key
  let equal = "\u{1b}[33m" <> "="
  let values = "\u{1b}[0m" <> values
  key <> equal <> values
}

/// Prepend the `when` field, holding the timestamp of the log. Prepended to
/// guarantee the timestamp is always displayed first, right after the level.
pub fn prepend_when(fields: Fields, when: String) -> Fields {
  [#("when", [field.string(when)]), ..fields]
}

/// Prepend the `at` field, holding the location of the log. Prepended to
/// guarantee the location is always displayed first, right after the level.
/// When no location is known, fields are returned untouched.
pub fn prepend_at(
  fields: Fields,
  at: Option(Location),
  is_color: Bool,
  is_json: Bool,
) -> Fields {
  case at {
    None -> fields
    Some(location) -> {
      let at = format_location(location, is_color, is_json)
      [#("at", [at]), ..fields]
    }
  }
}

fn format_location(location: Location, is_color: Bool, is_json: Bool) -> Field {
  let is_color = is_color && !is_json
  let module = case is_color {
    True -> "\u{1b}[35m" <> location.module <> "\u{1b}[0m"
    False -> location.module
  }
  case location.function, is_color {
    None, _ -> field.string(module)
    Some(function), False -> field.string(module <> "." <> function)
    Some(function), True ->
      field.string(module <> "." <> "\u{1b}[34m" <> function <> "\u{1b}[0m")
  }
}

pub fn format_message(message: String, is_color: Bool) -> String {
  use <- bool.guard(when: !is_color || message == "", return: message)
  "\u{1b}[1m" <> message <> "\u{1b}[0m"
}

/// Convert a report coming from the outside world, like an Erlang `logger`
/// report, to fields. Values are typed when they are primitives, to be
/// rendered as proper strings and numbers in JSON, and are inspected
/// otherwise.
pub fn from_report(report: List(#(String, Dynamic))) -> Fields {
  use #(key, value) <- list.map(report)
  #(key, [report_field(value)])
}

fn report_field(value: Dynamic) -> Field {
  let decoder =
    decode.one_of(decode.map(decode.int, field.int), or: [
      decode.map(decode.float, field.float),
      decode.map(decode.bool, field.bool),
      decode.map(decode.string, field.string),
    ])
  case decode.run(value, decoder) {
    Ok(field) -> field
    Error(_) -> field.string(string.inspect(value))
  }
}

/// Severity of a level as an integer, to compare levels: `Emergency` is the
/// highest, `Debug` the lowest.
pub fn level_to_int(level: Level) -> Int {
  case level {
    level.Emergency -> 7
    level.Alert -> 6
    level.Critical -> 5
    level.Error -> 4
    level.Warning -> 3
    level.Notice -> 2
    level.Info -> 1
    level.Debug -> 0
  }
}

/// Name of a level, as displayed in the `level` field of JSON logs.
pub fn level_to_string(level: Level) -> String {
  case level {
    level.Emergency -> "emergency"
    level.Alert -> "alert"
    level.Critical -> "critical"
    level.Error -> "error"
    level.Warning -> "warning"
    level.Notice -> "notice"
    level.Info -> "info"
    level.Debug -> "debug"
  }
}

/// Format the level indicator of text logs, `level=info`, with an appropriate
/// colour when colour is activated: red for errors and above, yellow for
/// warnings, green for notices, blue for info and cyan for debug. Every
/// combination is spelled out as a literal on purpose: the indicator is built
/// on every log, and a literal costs no allocation on either target, where a
/// concatenation would.
pub fn format_level(level: Level, colored: Bool) -> String {
  case level, colored {
    level.Emergency, True ->
      "\u{1b}[32mlevel\u{1b}[31m=\u{1b}[1;31memrg\u{1b}[0m"
    level.Alert, True -> "\u{1b}[32mlevel\u{1b}[31m=\u{1b}[1;31malrt\u{1b}[0m"
    level.Critical, True ->
      "\u{1b}[32mlevel\u{1b}[31m=\u{1b}[1;31mcrit\u{1b}[0m"
    level.Error, True -> "\u{1b}[32mlevel\u{1b}[31m=\u{1b}[1;31meror\u{1b}[0m"
    level.Warning, True -> "\u{1b}[32mlevel\u{1b}[31m=\u{1b}[1;33mwarn\u{1b}[0m"
    level.Notice, True -> "\u{1b}[32mlevel\u{1b}[31m=\u{1b}[1;32mntce\u{1b}[0m"
    level.Info, True -> "\u{1b}[32mlevel\u{1b}[31m=\u{1b}[1;34minfo\u{1b}[0m"
    level.Debug, True -> "\u{1b}[32mlevel\u{1b}[31m=\u{1b}[1;36mdebg\u{1b}[0m"
    level.Emergency, False -> "level=emrg"
    level.Alert, False -> "level=alrt"
    level.Critical, False -> "level=crit"
    level.Error, False -> "level=eror"
    level.Warning, False -> "level=warn"
    level.Notice, False -> "level=ntce"
    level.Info, False -> "level=info"
    level.Debug, False -> "level=debg"
  }
}
