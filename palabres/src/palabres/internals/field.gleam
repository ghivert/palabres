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

/// Convert a float to `null`.
pub const null = NullField

/// Convert a `Field` to a `Dynamic` value.
pub fn to_dynamic(value: Field) -> Dynamic {
  case value {
    BoolField(bool) -> dynamic.bool(bool)
    FloatField(float) -> dynamic.float(float)
    IntField(int) -> dynamic.int(int)
    StringField(string) -> dynamic.string(string)
    NullField -> do_null()
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
