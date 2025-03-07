//// ### Overview
////
//// Palabres is an opinionated logger, thought out to be compatible
//// out-of-the-box with BEAM and every JavaScript runtimes, whether it's in
//// browser or not, in Node or Deno, etc. In it's simplest form, Palabres will
//// simply creates a logger configured according to your needs, and will take
//// care of the rest. Palabres considers logs as structured logs, to help you
//// add context to your logs, and help you in future debugging sessions.
////
//// #### Structured logs
////
//// Logs can simply be strings output to your standard output, and have various
//// roles. Most of the time, it is used to help debugging, and to see what's
//// happening in real time in production servers. However, you can also use
//// logs to perform some analytics. Or maybe you want to track some user
//// activities. That's where structured logs shine: instead of outputting
//// simple strings, it's possible to output complex, rich data. To simplify that
//// usage, Palabres helps by providing constructors functions. You can use
//// `palabres.string`, `palabres.int` or `palabres.float` to add a field, with
//// its name and a value. They will automatically be part of the resulting log.
//// In Palabres, every log is not only a text message, but also context data
//// that you're free to add to help in further usages. Those additionnal data
//// are free, so it's up to you to determine how to use them efficiently!
////
//// #### Handling logs
////
//// Because logs are handled in multiple manners nowadays, Palabres provides two
//// output formats: as string and as JSON. In the former format, every string
//// will be formatted like query strings, but in a human readable way: no weird
//// ampersand, no escaping. You can enjoy structured format while keeping it
//// short, simple, and still easy to parse if needed. However, for processing
//// logs, Palabres provides an easier JSON format. JSON formatting in logs allows
//// to simply read and process them, and is widly implemented. DataDog, AWS
//// CloudWatch, PaperTrail or NewRelic, all of them handle JSON logs without
//// further configuration. Doing so also allows you to build on top of Palabres,
//// and add specific structured fields required in your log processor.
////
//// #### Default values
////
//// Palabres imposes 4 standard data in every log: the level of the log, its
//// timestamp, an ID in UUID format, and an arbitrary message string. You don't
//// have anything to do to add timestamp, id and level, they will automatically
//// be added to every log you create. They're all here to help you in future
//// log processing sessions. In the worst case, they can be safely ignored. In
//// the best case, they help you figure out what's happening, when an where.
////
//// Arbitrary text messages are parts of log, and instead of putting them in a
//// specific field, they're simply dumped as is in the string format, or under
//// the `message` field in JSON.
////
//// ### Usage Example
////
//// ```gleam
//// import palabres
//// import palabres/options
////
//// pub fn configure_logger() {
////   use json_output <- result.try(is_json_output())
////   options.defaults()
////   |> options.json(json_output)
////   |> palabres.configure
//// }
////
//// pub fn log_message(message, user) {
////   palabres.info(message)
////   |> palabres.string("node", get_node_info())
////   |> palabres.string("user_id", user.id)
////   |> palabres.int("user_age", user.age)
////   |> palabres.log
//// }
////
//// fn is_json_output() {
////   use output <- result.try(envoy.get("JSON_OUTPUT"))
////   use <- bool.guard(when: json_output != "true", return: False)
////   True
//// }
//// ```
////
//// ### How does it work?
////
//// Behind the scenes, Palabres has two different behaviours, depending on your
//// target. When targetting Erlang, Palabres will act as a `logger` layer, and
//// will take care of the formatting, filtering, and helping to deal with
//// `logger` for you. You don't need to dive in its interface, Palabres got you
//// covered. You can use Palabres to create your logs, but nothing stops you to
//// use something like `wisp.log_info` in your code. In that case, Palabres
//// can still intervene and apply styling. That allow you to write simple
//// logs functions, while still leveraging structured logs. Because Palabres
//// relies on `logger`, you can rather easily interact with Palabres in Erlang,
//// provided you know how to use `logger`.
////
//// On JavaScript though, Palabres instanciate a full-fledged, custom logger.
//// Because JavaScript does not have any logger concept in its root, Palabres
//// provides a logger with similar abilities than `logger` on BEAM. As such,
//// every logs _should go_ through the Palabres package, otherwise the logger
//// won't be able to format them accordingly.

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result
import palabres/level
import palabres/options.{type Options}

/// Configure Palabres logger. Because logger is a singleton, it only needs to be
/// configured once at startup. Select your options, and run configurations, to
/// get Palabres logger running. Each logger call will then go through Palabres
/// on BEAM. On JavaScript, calling `palabres` functions are still required.
@external(erlang, "palabres_ffi", "configure")
@external(javascript, "./palabres.ffi.mjs", "configure")
pub fn configure(options: Options) -> Nil

/// Log instance, holding the structured data. Can be instanciated with
/// corresponding "level" functions: [`emergency`](#emergency),
/// [`alert`](#alert), [`critical`](#critical), [`error`](#error),
/// [`warning`](#warning), [`notice`](#notice), [`info`](#info),
/// [`debug`](#debug).
///
/// ```gleam
/// pub fn log() {
///   palabres.info("This is a log")
///   |> palabres.log
/// }
/// ```
pub opaque type Log {
  Log(
    level: level.Level,
    fields: List(#(String, List(String))),
    message: String,
    at: Option(#(String, String)),
  )
}

/// Creates an `Emergency` `Log` instance. Should be used as a starting point
/// to create a log.
///
/// ```gleam
/// pub fn log() {
///   palabres.emergency("Example message")
///   |> palabres.log
/// }
/// ```
pub fn emergency(message: String) -> Log {
  init(level.Emergency, message)
}

/// Creates an `Alert` `Log` instance. Should be used as a starting point
/// to create a log.
///
/// ```gleam
/// pub fn log() {
///   palabres.alert("Example message")
///   |> palabres.log
/// }
/// ```
pub fn alert(message: String) -> Log {
  init(level.Alert, message)
}

/// Creates a `Critical` `Log` instance. Should be used as a starting point
/// to create a log.
///
/// ```gleam
/// pub fn log() {
///   palabres.critical("Example message")
///   |> palabres.log
/// }
/// ```
pub fn critical(message: String) -> Log {
  init(level.Critical, message)
}

/// Creates an `Error` `Log` instance. Should be used as a starting point
/// to create a log.
///
/// ```gleam
/// pub fn log() {
///   palabres.error("Example message")
///   |> palabres.log
/// }
/// ```
pub fn error(message: String) -> Log {
  init(level.Error, message)
}

/// Creates a `Warning` `Log` instance. Should be used as a starting point
/// to create a log.
///
/// ```gleam
/// pub fn log() {
///   palabres.warning("Example message")
///   |> palabres.log
/// }
/// ```
pub fn warning(message: String) -> Log {
  init(level.Warning, message)
}

/// Creates a `Notice` `Log` instance. Should be used as a starting point
/// to create a log.
///
/// ```gleam
/// pub fn log() {
///   palabres.notice("Example message")
///   |> palabres.log
/// }
/// ```
pub fn notice(message: String) -> Log {
  init(level.Notice, message)
}

/// Creates an `Info` `Log` instance. Should be used as a starting point
/// to create a log.
///
/// ```gleam
/// pub fn log() {
///   palabres.info("Example message")
///   |> palabres.log
/// }
/// ```
pub fn info(message: String) -> Log {
  init(level.Info, message)
}

/// Creates a `Debug` `Log` instance. Should be used as a starting point
/// to create a log.
///
/// ```gleam
/// pub fn log() {
///   palabres.debug("Example message")
///   |> palabres.log
/// }
/// ```
pub fn debug(message: String) -> Log {
  init(level.Debug, message)
}

/// Log with message at level `Emergency` directly. \
/// Avoid to write the full log pipeline when you just need to write a
/// single message.
///
/// ```gleam
/// palabres.log_emergency("Example message")
/// ```
pub fn log_emergency(message: String) -> Nil {
  log(init(level.Emergency, message))
}

/// Log with message at level `Alert` directly. \
/// Avoid to write the full log pipeline when you just need to write a
/// single message.
///
/// ```gleam
/// palabres.log_alert("Example message")
/// ```
pub fn log_alert(message: String) -> Nil {
  log(init(level.Alert, message))
}

/// Log with message at level `Critical` directly. \
/// Avoid to write the full log pipeline when you just need to write a
/// single message.
///
/// ```gleam
/// palabres.log_critical("Example message")
/// ```
pub fn log_critical(message: String) -> Nil {
  log(init(level.Critical, message))
}

/// Log with message at level `Error` directly. \
/// Avoid to write the full log pipeline when you just need to write a
/// single message.
///
/// ```gleam
/// palabres.log_error("Example message")
/// ```
pub fn log_error(message: String) -> Nil {
  log(init(level.Error, message))
}

/// Log with message at level `Warning` directly. \
/// Avoid to write the full log pipeline when you just need to write a
/// single message.
///
/// ```gleam
/// palabres.log_warning("Example message")
/// ```
pub fn log_warning(message: String) -> Nil {
  log(init(level.Warning, message))
}

/// Log with message at level `Notice` directly. \
/// Avoid to write the full log pipeline when you just need to write a
/// single message.
///
/// ```gleam
/// palabres.log_notice("Example message")
/// ```
pub fn log_notice(message: String) -> Nil {
  log(init(level.Notice, message))
}

/// Log with message at level `Info` directly. \
/// Avoid to write the full log pipeline when you just need to write a
/// single message.
///
/// ```gleam
/// palabres.log_info("Example message")
/// ```
pub fn log_info(message: String) -> Nil {
  log(init(level.Info, message))
}

/// Log with message at level `Debug` directly. \
/// Avoid to write the full log pipeline when you just need to write a
/// single message.
///
/// ```gleam
/// palabres.log_debug("Example message")
/// ```
pub fn log_debug(message: String) -> Nil {
  log(init(level.Debug, message))
}

/// Debug information, used to easily pinpoint a location in your code. `at`
/// should point to a module and a function, and will display in your logs
/// a data looking like `at=module.function` with proper colored output if
/// activated.
///
/// ```gleam
/// pub fn log() {
///   palabres.info("Example message")
///   |> palabres.at("my_logger", "log")
///   |> palabres.log
///   // Turns in:
///   //   level=info when=2024-12-15T15:59:17Z id=3a5c0fc2-5f74-4796-84df-cbfe4000eef6 at=my_logger.log Example message
/// }
/// ```
pub fn at(log: Log, module module: String, function function: String) -> Log {
  Log(..log, at: option.Some(#(module, function)))
}

/// Add a string field to your structured data.
///
/// ```gleam
/// let field1 = 1.0
/// palabres.info("Example")
/// |> palabres.string("field1", field1)
/// |> palabres.log
/// ```
pub fn string(log: Log, key: String, value: String) -> Log {
  log.fields
  |> append_field(key, value)
  |> set_fields(log)
}

/// Add an int field to your structured data.
///
/// ```gleam
/// let field1 = 1
/// palabres.info("Example")
/// |> palabres.int("field1", field1)
/// |> palabres.log
/// ```
pub fn int(log: Log, key: String, value: Int) -> Log {
  log.fields
  |> append_field(key, int.to_string(value))
  |> set_fields(log)
}

/// Add a float field to your structured data.
///
/// ```gleam
/// let field1 = 1.0
/// palabres.info("Example")
/// |> palabres.float("field1", field1)
/// |> palabres.log
/// ```
pub fn float(log: Log, key: String, value: Float) -> Log {
  log.fields
  |> append_field(key, float.to_string(value))
  |> set_fields(log)
}

/// Add a potentially null value to your structured data.
/// `None` will be converted to `null`, while `Some(x)` will be converted to `x`.
///
/// ```gleam
/// let field1 = option.Some(1)
/// palabres.info("Example")
/// |> palabres.nullable("field1", field1, palabres.int)
/// |> palabres.log
/// ```
pub fn nullable(
  log: Log,
  key: String,
  value: option.Option(a),
  add: fn(Log, String, a) -> Log,
) -> Log {
  case value {
    option.None -> string(log, key, "null")
    option.Some(value) -> add(log, key, value)
  }
}

/// Run your log and make it display in your log output.
///
/// ```gleam
/// palabres.info("Example")
/// |> palabres.log
/// ```
pub fn log(log_: Log) -> Nil {
  case log_.level {
    level.Emergency -> do_log(log_.level, log_.fields, log_.message, log_.at)
    level.Alert -> do_log(log_.level, log_.fields, log_.message, log_.at)
    level.Critical -> do_log(log_.level, log_.fields, log_.message, log_.at)
    level.Error -> do_log(log_.level, log_.fields, log_.message, log_.at)
    level.Warning -> do_log(log_.level, log_.fields, log_.message, log_.at)
    level.Notice -> do_log(log_.level, log_.fields, log_.message, log_.at)
    level.Info -> do_log(log_.level, log_.fields, log_.message, log_.at)
    level.Debug -> do_log(log_.level, log_.fields, log_.message, log_.at)
  }
}

@external(erlang, "palabres_ffi", "log")
@external(javascript, "./palabres.ffi.mjs", "log")
fn do_log(
  level: level.Level,
  message: List(#(String, List(String))),
  text: String,
  at: Option(#(String, String)),
) -> Nil

fn init(level: level.Level, message: String) {
  Log(level:, fields: [], message:, at: option.None)
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

fn set_fields(fields: List(#(String, List(String))), log: Log) -> Log {
  Log(..log, fields:)
}
