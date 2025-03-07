//// Options to configure Palabres at start.
////
//// ```gleam
//// import palabres
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
////   // Configure the logger.
////   |> palabres.configure
//// }
//// ```

import gleam/option.{type Option, None, Some}
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
pub opaque type Options {
  Options(
    color: Option(Bool),
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
pub fn defaults() -> Options {
  Options(
    color: None,
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

/// Creates the stdout output.
pub fn stdout() -> Output {
  Stdout
}

/// Creates a file output, to use with `output`. Flush interval can be configured
/// with `flush`.
pub fn file(filename: String) -> Output {
  File(filename:, flush_interval: None)
}

/// When outputting to a file, logger will dump messages in file every 5 seconds
/// by defaults. `flush` allows to choose how long to wait before dumping
/// messages in the file. Be careful, setting a too small interval could lead to
/// performance issues, because BEAM will spend too much time in file writing.
pub fn flush(file: Output, every interval: Int) -> Output {
  case file {
    File(..) -> File(..file, flush_interval: Some(interval))
    Stdout -> Stdout
  }
}
