//// Options to configure Palabre at start.
////
//// ```gleam
//// import palabre
//// import palabre/level
//// import palabre/options
////
//// pub fn configure_logger() {
////   options.default()
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
////   // Applies or not palabre styling to default logger (on stdout).
////   |> options.style_default_logger(True)
////   // Configure the logger.
////   |> palabre.configure
//// }
//// ```

import gleam/option.{type Option, None, Some}
import palabre/level

/// Options to configure the logger. To create with [`defaults`](#defaults).
/// Below are the configurable options, usable with the corresponding functions.
///
/// - [`color`](#color) allow to choose if the output is colored or not. Default to `True`.
///   Can be deactivated globally with the `NO_COLOR` (or `NO_COLOUR`)
///   environment variable. Once set, `color` defaults to `False`.
/// - [`json`](#json) will activate the JSON mode, forcing the logger to output JSON
///   instead of strings. JSON output will automatically disable colored logging.
///   Default to `False`.
/// - [`level`](#level) will set the minimum required log level. Every level below will
///   be ignored. Default to `level.Info`.
/// - [`output`](#output) allow to choose writing to `stdout` or a file. Default to
///   `stdout`.
/// - [`style_default_logger`](#style_default_logger) apply the palabre styling to the default logger.
///   Default to `True`.
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
/// selected file. Standard output can be simply selected with
/// [`stdout`](#stdout), while file logging is selected with [`file`](#file).
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

pub fn default() -> Options {
  Options(
    color: None,
    json: False,
    level: level.Info,
    output: Stdout,
    style_default: True,
  )
}

/// Activate or deactivate colored output.
/// Defaults to `True` if environment variable `NO_COLOR` is unset.
/// Defaults to `False` otherwise. If `color` is called, it takes precedence on
/// environment variable.
pub fn color(options: Options, color: Bool) -> Options {
  Options(..options, color: Some(color))
}

/// Activate or deactive JSON ouput.
/// Defaults to `False`. If activated, colored output is automatically
/// deactivated.
pub fn json(options: Options, json: Bool) -> Options {
  Options(..options, json:)
}

/// Set the minimum desired log level. Defaults to `level.Info`.
pub fn level(options: Options, level: level.Level) -> Options {
  Options(..options, level:)
}

/// Choose the desired output. Defaults to `stdout`.
pub fn output(options: Options, to output: Output) -> Options {
  Options(..options, output:)
}

/// Activate or deactivate styling from Palabre on default logger. Defaults to
/// `True`. Has no effect on JavaScript, since there's no default logger.
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
