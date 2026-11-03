//// Converters for fields messages. Separated in order to use it in FFI to ease
//// working with external data structures.
//// Should never be leaked.

import gleam/bool
import gleam/dict
import gleam/dynamic
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import palabres/internals/field.{type Field, type Fields}
import palabres/internals/location.{type Location}

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
        values
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
  use <- bool.guard(when: !is_color, return: message)
  "\u{1b}[1m" <> message <> "\u{1b}[0m"
}
