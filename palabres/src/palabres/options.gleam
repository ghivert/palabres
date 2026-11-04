//// Options to configure Palabres at start.
////
//// ```gleam
//// import palabres
//// import palabres/field
//// import palabres/level
//// import palabres/options
////
//// pub fn configure_logger() {
////   options.defaults()
////   // Enables or disable colored output.
////   |> options.color(True)
////   // Enables or disable JSON output. JSON output will never be colored.
////   |> options.json(False)
////   // Set the minimum log level.
////   |> options.level(level.Info)
////   // Output to a file instead of stdout.
////   |> options.output(to: {
////     options.file(test_utils.log_file)
////     |> options.flush(every: 5000)
////   })
////   // Applies or not palabres styling to default logger (on stdout).
////   |> options.style_default_logger(True)
////   // Add fields to every log.
////   |> options.fields([field.string("service", "my_service")])
////   // Configure the logger.
////   |> palabres.configure
//// }
//// ```

import gleam/list
import gleam/option.{type Option, None, Some}
import palabres/field.{type Field}
import palabres/level

/// Options to configure the logger. To create with [`defaults`](#defaults).
/// Below are the configurable options, usable with the corresponding functions.
///
/// - [`color`](#color) Defaults to `True`. \
///   Allow to choose if the output is colored or not.\
///   Can be deactivated globally with the `NO_COLOR` (or `NO_COLOUR`)
///   environment variable. \
///   Once set, `color` defaults to `False`.
/// - [`json`](#json) Defaults to `False`. \
///   Activate the JSON mode, forcing the logger to output JSON
///   instead of strings. \
///   JSON output will automatically disable colored logging.
/// - [`level`](#level) Defaults to `level.Info`. \
///   Set the minimum required log level. \
///   Every level below will be ignored.
/// - [`output`](#output) Defaults to `stdout`. \
///   Allow to choose writing to `stdout` or a file.
/// - [`style_default_logger`](#style_default_logger) Defaults to `True`. \
///   Apply the palabres styling to the default logger.
/// - [`auto_at`](#auto_at) Defaults to `True`. \
///   Automatically fill the `at` field with the module and function creating
///   the log.
/// - [`auto_when`](#auto_when) Defaults to `True`. \
///   Automatically fill the `when` field with the timestamp of the log.
/// - [`fields`](#fields) Defaults to `[]`. \
///   Fields added to every log.
pub opaque type Options {
  Options(
    auto_at: Bool,
    auto_when: Bool,
    color: Option(Bool),
    fields: List(Field),
    json: Bool,
    level: level.Level,
    output: Output,
    style_default: Bool,
  )
}

/// Configurable output for the logger. Can be the standard output, or a
/// selected file. \
/// Standard output can be simply selected with [`stdout`](#stdout), while file
/// logging is selected with [`file`](#file). \
/// A file logger will dump the log contents at regular interval. This interval
/// is configurable by using [`flush`](#flush).
///
/// ```gleam
/// pub fn configure_logger() {
///   options.defaults()
///   // Choose to output to stdout.
///   |> options.output(to: options.stdout())
///   // Choose to output to a file.
///   |> options.output(to: {
///     options.file("/var/log/my_app.log")
///     // Flush interval can be seldected in milliseconds.
///     |> options.flush(every: 2500)
///   })
/// }
/// ```
pub opaque type Output {
  File(filename: String, flush_interval: Option(Int))
  Stdout
}

/// Initialise the options with defaults.
/// - `color: True` if environment variable `NO_COLOR` is unset, `False` otherwise.
/// - `json: False`
/// - `level: level.Info`
/// - `output: stdout`
/// - `style_default_logger: True`
/// - `auto_at: True`
/// - `auto_when: True`
/// - `fields: []`
pub fn defaults() -> Options {
  Options(
    auto_at: True,
    auto_when: True,
    color: None,
    fields: [],
    json: False,
    level: level.Info,
    output: Stdout,
    style_default: True,
  )
}

/// Activate or deactivate colored output. \
/// Defaults to `True` if environment variable `NO_COLOR` is unset. \
/// Defaults to `False` otherwise. \
/// If `color` is called, it takes precedence on environment variable.
pub fn color(options: Options, color: Bool) -> Options {
  Options(..options, color: Some(color))
}

/// Defaults to `False`. \
/// Activate or deactive JSON ouput. \
/// If activated, colored output is automatically deactivated.
pub fn json(options: Options, json: Bool) -> Options {
  Options(..options, json:)
}

/// Defaults to `level.Info`. Set the minimum desired log level.
pub fn level(options: Options, level: level.Level) -> Options {
  Options(..options, level:)
}

/// Defaults to `stdout`. Choose the desired output.
pub fn output(options: Options, to output: Output) -> Options {
  Options(..options, output:)
}

/// Defaults to `True`. \
/// Activate or deactivate styling from Palabres on default logger. \
/// Has no effect on JavaScript, since there's no default logger.
pub fn style_default_logger(options: Options, style_default: Bool) -> Options {
  Options(..options, style_default:)
}

/// Defaults to `True`. \
/// Activate or deactivate the automatic `at` field. When activated, every log
/// is tagged with the module and the function it has been created in, unless
/// [`palabres.at`](./palabres.html#at) overrides it. \
/// Locating a log requires inspecting the call stack, which is cheap but not
/// free. Deactivate it on hot paths if every microsecond matters.
pub fn auto_at(options: Options, enabled: Bool) -> Options {
  Options(..options, auto_at: enabled)
}

/// Defaults to `True`. \
/// Activate or deactivate the automatic `when` field, holding the timestamp of
/// the log in ISO 8601 format. Deactivate it when your log collector adds its
/// own timestamp, like `journald` or most cloud log processors, to avoid
/// paying twice for the same information.
pub fn auto_when(options: Options, enabled: Bool) -> Options {
  Options(..options, auto_when: enabled)
}

/// Creates the stdout output.
pub fn stdout() -> Output {
  Stdout
}

/// Creates a file output, to use with `output`. Flush interval can be configured
/// with `flush`.
pub fn file(filename: String) -> Output {
  File(filename:, flush_interval: None)
}

/// When outputting to a file on Erlang, logger will sync the file every 5
/// seconds by default. `flush` allows to choose how long to wait between two
/// syncs. Be careful, setting a too small interval could lead to performance
/// issues, because BEAM will spend too much time in file writing. \
/// Has no effect on JavaScript, where logs are written to the file as they
/// come, through a single stream kept open by the logger.
pub fn flush(file: Output, every interval: Int) -> Output {
  case file {
    File(..) -> File(..file, flush_interval: Some(interval))
    Stdout -> Stdout
  }
}

/// Defaults to `[]`. \
/// Add fields to every log, whether it is created by Palabres or, on Erlang,
/// by any other library logging through `logger`. Use it for what is true for
/// the whole process: service name, version, hostname, environment… \
/// Fields are built with the [`palabres/field`](./palabres/field.html) module.
/// A field set on a log takes precedence over a global field with the same
/// key. Calling `fields` several times accumulates the fields.
///
/// ```gleam
/// options.defaults()
/// |> options.fields([
///   field.string("service", "my_service"),
///   field.int("version", 3),
///   field.lazy(fn() { field.string("hostname", read_hostname()) }),
/// ])
/// ```
pub fn fields(options: Options, fields: List(Field)) -> Options {
  Options(..options, fields: list.append(options.fields, fields))
}
