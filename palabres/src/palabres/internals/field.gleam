import gleam/dynamic.{type Dynamic}
import gleam/float
import gleam/int
import gleam/list
import gleam/result

/// Manage the different field values.
pub opaque type Field {
  StringField(String)
  BoolField(Bool)
  IntField(Int)
  FloatField(Float)
  LazyField(fn() -> Field)
  NullField
}

pub type Fields =
  List(#(String, List(Field)))

/// Convert a string to a `Field`.
pub const string = StringField

/// Convert a bool to a `Field`.
pub const bool = BoolField

/// Convert an int to a `Field`.
pub const int = IntField

/// Convert a float to a `Field`.
pub const float = FloatField

/// Converst `null` to a `Field`.
pub const null = NullField

/// Lazify a `Field`.
pub const lazy = LazyField

pub fn default_fields_to_dynamic(
  fields: List(#(String, List(Field))),
) -> Dynamic {
  dynamic.properties({
    use #(key, values) <- list.map(fields)
    #(dynamic.string(key), {
      dynamic.list({
        use value <- list.map(values)
        to_dynamic(value)
      })
    })
  })
}

/// Convert a `Field` to a `Dynamic` value.
pub fn to_dynamic(value: Field) -> Dynamic {
  case value {
    BoolField(bool) -> dynamic.bool(bool)
    FloatField(float) -> dynamic.float(float)
    IntField(int) -> dynamic.int(int)
    StringField(string) -> dynamic.string(string)
    NullField -> do_null()
    LazyField(fun) ->
      case fun() {
        LazyField(_) -> panic as "Impossible state"
        result -> to_dynamic(result)
      }
  }
}

/// Convert a `Field` to a string.
pub fn to_string(value: Field) -> String {
  case value {
    BoolField(True) -> "true"
    BoolField(False) -> "false"
    FloatField(float) -> float.to_string(float)
    IntField(int) -> int.to_string(int)
    StringField(string) -> string
    NullField -> "null"
    LazyField(fun) ->
      case fun() {
        LazyField(_) -> panic as "Impossible state"
        result -> to_string(result)
      }
  }
}

pub fn append(fields: Fields, key: String, value: Field) -> Fields {
  fields
  |> list.key_find(key)
  |> result.unwrap([])
  |> list.prepend(value)
  |> list.key_set(fields, key, _)
}

@external(erlang, "palabres_utils_ffi", "null")
fn do_null() -> Dynamic {
  dynamic.nil()
}
