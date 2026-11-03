//// Fields are the structured data attached to logs. They are built by
//// [`palabres.string`](./palabres.html#string), [`palabres.int`](./palabres.html#int)
//// and friends when writing a log, and by this module when declaring fields
//// once, in the configuration, to attach them to every log.
////
//// ```gleam
//// import palabres/field
//// import palabres/options
////
//// pub fn configure_logger() {
////   options.defaults()
////   |> options.fields([
////     field.string("service", "my_service"),
////     field.int("version", 3),
////     field.lazy(fn() { field.string("hostname", read_hostname()) }),
////   ])
////   |> palabres.configure
//// }
//// ```

import envoy
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import palabres/internals/field as internal

/// A field is a key with one or more values. A field is added to a log with
/// its key, and can be declared once in [`options.fields`](./palabres/options.html#fields)
/// to be added to every log. Use the functions of this module to build one.
pub opaque type Field {
  Field(key: String, values: List(internal.Field))
  Lazy(fn() -> Field)
}

/// A string field.
///
/// ```gleam
/// field.string("service", "my_service")
/// // Turns in: service=my_service
/// ```
pub fn string(key: String, value: String) -> Field {
  Field(key:, values: [internal.string(value)])
}

/// An int field.
///
/// ```gleam
/// field.int("version", 3)
/// // Turns in: version=3
/// ```
pub fn int(key: String, value: Int) -> Field {
  Field(key:, values: [internal.int(value)])
}

/// A float field.
///
/// ```gleam
/// field.float("rate", 0.2)
/// // Turns in: rate=0.2
/// ```
pub fn float(key: String, value: Float) -> Field {
  Field(key:, values: [internal.float(value)])
}

/// A boolean field.
///
/// ```gleam
/// field.bool("activated", True)
/// // Turns in: activated=true
/// ```
pub fn bool(key: String, value: Bool) -> Field {
  Field(key:, values: [internal.bool(value)])
}

/// A `null` field.
///
/// ```gleam
/// field.null("user_id")
/// // Turns in: user_id=null
/// ```
pub fn null(key: String) -> Field {
  Field(key:, values: [internal.null])
}

/// A potentially null field. `None` is converted to `null`, while `Some(x)`
/// is converted to `x` with the given function.
///
/// ```gleam
/// field.nullable("user_id", option.Some("1234"), field.string)
/// // Turns in: user_id=1234
/// ```
pub fn nullable(
  key: String,
  value: Option(a),
  add: fn(String, a) -> Field,
) -> Field {
  case value {
    Some(value) -> add(key, value)
    None -> null(key)
  }
}

/// A field holding a list of values, converted one by one with the given
/// function.
///
/// ```gleam
/// field.list("services", ["my_service", "other_service"], field.string)
/// ```
pub fn list(key: String, value: List(a), add: fn(String, a) -> Field) -> Field {
  let values = list.flat_map(value, fn(value) { force(add(key, value)) })
  Field(key:, values:)
}

/// A field computed only when the log is printed, instead of once when the
/// field is declared. Use it for a value changing over time, or expensive to
/// compute, that should not be paid for at configuration.
///
/// ```gleam
/// field.lazy(fn() { field.string("hostname", read_hostname()) })
/// ```
pub fn lazy(value: fn() -> Field) -> Field {
  Lazy(value)
}

/// Read fields from the environment variables starting with `prefix`. The
/// prefix is dropped and the remaining of the name is lowercased to get the
/// key. Every value is a string. Variables are sorted by name to keep the
/// fields order deterministic.
///
/// ```gleam
/// // With PALABRES_SERVICE=my_service and PALABRES_VERSION=3 set:
/// field.from_environment(prefix: "PALABRES_")
/// // Gives: [field.string("service", "my_service"), field.string("version", "3")]
/// ```
pub fn from_environment(prefix prefix: String) -> List(Field) {
  envoy.all()
  |> dict.to_list
  |> list.filter(fn(variable) { string.starts_with(variable.0, prefix) })
  |> list.sort(by: fn(a, b) { string.compare(a.0, b.0) })
  |> list.map(fn(variable) {
    let #(name, value) = variable
    let key = string.lowercase(string.drop_start(name, string.length(prefix)))
    string(key, value)
  })
}

/// Add a field to internal fields, resolving it if needed.
@internal
pub fn add(fields: internal.Fields, field: Field) -> internal.Fields {
  case field {
    Lazy(field) -> add(fields, field())
    Field(key:, values:) -> {
      // Values are stored newest first, like `palabres.list` does.
      use fields, value <- list.fold(list.reverse(values), fields)
      internal.append(fields, key, value)
    }
  }
}

/// Resolve a list of fields to internal fields, computing lazy fields. Called
/// by the formatters every time a log is printed.
@internal
pub fn resolve(fields: List(Field)) -> internal.Fields {
  list.fold(fields, [], add)
}

fn force(field: Field) -> List(internal.Field) {
  case field {
    Lazy(field) -> force(field())
    Field(values:, ..) -> values
  }
}
