//// Converters for fields messages. Separated in order to use it in FFI to ease
//// working with external data structures.
//// Should never be leaked.

import gleam/bool
import gleam/dict
import gleam/dynamic
import gleam/list
import gleam/string

/// Converts a list of fields, to a query string separated by spaces instead
/// of ampersand (`&`). Handles coloring output, or not, depending on the state
/// of palabres logger.
pub fn to_spaced_query_string(
  fields: List(#(String, List(String))),
  colored: Bool,
) -> String {
  fields
  |> list.map(to_query_part(_, colored))
  |> string.join(" ")
}

/// Converts a list of fields in a JSON string. Color is never applied on JSON.
pub fn to_json(
  fields: List(#(String, List(String))),
  text: String,
) -> dict.Dict(String, dynamic.Dynamic) {
  let init = case text {
    "" -> dict.new()
    _ -> dict.from_list([#("__message", dynamic.string(text))])
  }
  use acc, #(key, values) <- list.fold(fields, init)
  case values {
    [value] -> dict.insert(acc, key, dynamic.string(value))
    values ->
      dict.insert(acc, key, dynamic.list(list.map(values, dynamic.string)))
  }
}

fn to_query_part(field: #(String, List(String)), colored: Bool) -> String {
  let #(key, vals) = field
  let vals = list.reverse(vals)
  let uncolored = fn() { key <> "=" <> string.join(vals, ",") }
  use <- bool.lazy_guard(when: !colored, return: uncolored)
  let vals = string.join(vals, "\u{1b}[33m,\u{1b}[0m")
  let key = "\u{1b}[32m" <> key
  let equal = "\u{1b}[33m" <> "="
  let vals = "\u{1b}[0m" <> vals
  key <> equal <> vals
}

pub fn format_at(
  module: String,
  function: String,
  is_color: Bool,
  is_json: Bool,
) -> String {
  let #(module, separator, function, reset) = case is_json, is_color {
    False, False | True, _ -> #(module, ".", function, "")
    False, True -> #(
      "\u{1b}[35m" <> module,
      "\u{1b}[0m.",
      "\u{1b}[34m" <> function,
      "\u{1b}[0m",
    )
  }
  module <> separator <> function <> reset
}

pub fn format_message(message: String, is_color: Bool) {
  case is_color {
    True -> "\u{1b}[1m" <> message <> "\u{1b}[0m"
    False -> message
  }
}
