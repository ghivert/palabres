import gleam/float
import gleam/http
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import palabre/internals/utils
import palabre/level
import palabre/options.{type Options}
import wisp.{type Request, type Response}

@external(erlang, "palabre_ffi", "configure")
pub fn configure(options: Options) -> Nil

pub opaque type Log {
  Log(
    level: level.Level,
    fields: List(#(String, List(String))),
    message: String,
  )
}

pub fn emergency(message: String) -> Log {
  init(level.Emergency, message)
}

pub fn alert(message: String) -> Log {
  init(level.Alert, message)
}

pub fn critical(message: String) -> Log {
  init(level.Critical, message)
}

pub fn error(message: String) -> Log {
  init(level.Error, message)
}

pub fn warning(message: String) -> Log {
  init(level.Warning, message)
}

pub fn notice(message: String) -> Log {
  init(level.Notice, message)
}

pub fn info(message: String) -> Log {
  init(level.Info, message)
}

pub fn debug(message: String) -> Log {
  init(level.Debug, message)
}

pub fn at(log: Log, module module: String, function function: String) -> Log {
  let key = "at"
  let is_json = utils.is_json()
  let is_color = utils.is_color()
  let #(module, separator, function, reset) = case is_json, is_color {
    False, False | True, _ -> #(module, ".", function, "")
    False, True -> #(
      "\u{1b}[35m" <> module,
      "\u{1b}[0m.",
      "\u{1b}[34m" <> function,
      "\u{1b}[0m",
    )
  }
  log.fields
  |> append_field(key, module <> separator <> function <> reset)
  |> set_fields(log)
}

pub fn string(log: Log, key: String, value: String) -> Log {
  log.fields
  |> append_field(key, value)
  |> set_fields(log)
}

pub fn int(log: Log, key: String, value: Int) -> Log {
  log.fields
  |> append_field(key, int.to_string(value))
  |> set_fields(log)
}

pub fn float(log: Log, key: String, value: Float) -> Log {
  log.fields
  |> append_field(key, float.to_string(value))
  |> set_fields(log)
}

pub fn dump(log_: Log) -> Nil {
  let text = case utils.is_color() {
    True -> "\u{1b}[1m" <> log_.message <> "\u{1b}[0m"
    False -> log_.message
  }
  case log_.level {
    level.Emergency -> log(log_.level, log_.fields, text)
    level.Alert -> log(log_.level, log_.fields, text)
    level.Critical -> log(log_.level, log_.fields, text)
    level.Error -> log(log_.level, log_.fields, text)
    level.Warning -> log(log_.level, log_.fields, text)
    level.Notice -> log(log_.level, log_.fields, text)
    level.Info -> log(log_.level, log_.fields, text)
    level.Debug -> log(log_.level, log_.fields, text)
  }
}

@external(erlang, "palabre_ffi", "log")
fn log(level: level.Level, message: a, text: String) -> Nil

fn init(level: level.Level, message: String) {
  Log(level:, fields: [], message:)
}

fn append_field(
  fields: List(#(String, List(String))),
  key: String,
  value: String,
) -> List(#(String, List(String))) {
  fields
  |> list.key_find(key)
  |> result.unwrap([])
  |> list.prepend(value)
  |> list.key_set(fields, key, _)
}

pub fn log_request(req: Request, handler: fn() -> Response) -> Response {
  let response = handler()
  info("")
  |> int("status", response.status)
  |> string("method", string.uppercase(http.method_to_string(req.method)))
  |> string("where", req.path)
  |> dump
  response
}

fn set_fields(fields: List(#(String, List(String))), log: Log) -> Log {
  Log(..log, fields:)
}
