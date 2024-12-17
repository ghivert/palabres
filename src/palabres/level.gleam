//// Log level used by most loggers. Erlang logger uses those default log levels
//// while vailla JavaScript use a subset of them. Palabres implements them all,
//// whether you're running on JavaScript or Erlang. Log level will _always_
//// appears in your logs, and will correctly be filtered by the logger.
////
//// #### JavaScript log level equivalent
////
//// Find the correspondance between log levels, and JavaScript `console`
//// functions. By default, JavaScript applies some styling in browser for
//// those levels. \
//// While the table can help you figure out which level to chooses, keep in
//// mind it's mainly there to help you understand how to use log level, as
//// palabres works differently: everything goes through `console.log` to output
//// on `stdout`. When outputting to a file, logs will never go to `console`
//// functions. Log level will always be indicated in the log, through
//// `level=[level]` or `{ level: [level] }`, allowing for quick and easy
//// indexing by a log crawler.
////
////
////
//// log level | JS
//// ----------|-------------------
//// Emergency |
//// Alert     |
//// Critical  |
//// Error     | `console.error`
//// Warning   | `console.warning`
//// Notice    |
//// Info      | `console.log`
//// Debug     | `console.debug`

import gleam
import gleam/string

/// Different levels of logs. Log levels correspond directly to a level of
/// danger. `Emergency` is the most dangerous one, while `Debug` is the least
/// dangerous one. `Info` is the default value with palabres. \
/// Once chosen, every level below the desired level are ignored.
pub type Level {
  Emergency
  Alert
  Critical
  Error
  Warning
  Notice
  Info
  Debug
}

/// Parse a string log level, and returns the corresponding log level. \
/// Log levels are case-insensitive, and correspond litteraly to log levels
/// from `Level` type.
pub fn from_string(level: String) -> Result(Level, Nil) {
  case string.lowercase(level) {
    "emergency" -> Ok(Emergency)
    "alert" -> Ok(Alert)
    "critical" -> Ok(Critical)
    "error" -> Ok(Error)
    "warning" -> Ok(Warning)
    "notice" -> Ok(Notice)
    "info" -> Ok(Info)
    "debug" -> Ok(Debug)
    _ -> gleam.Error(Nil)
  }
}
