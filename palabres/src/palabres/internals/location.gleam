//// Location of a log in the code base, i.e. the module and the function from
//// where the log has been created. Should never be leaked outside of the
//// package.

import gleam/option.{type Option}
import palabres/level

/// Module is always known. Function may be unknown on JavaScript, as
/// anonymous functions carry no name in the stack trace.
pub type Location {
  Location(module: String, function: Option(String))
}

/// Capture the location of the first function outside of Palabres in the
/// current call stack. Returns `None` when automatic location is disabled, or
/// when the log would be filtered out by the configured level, to avoid
/// paying for a stack inspection on logs that will never be printed.
@external(erlang, "palabres_ffi", "locate")
@external(javascript, "../../palabres.ffi.mjs", "locate")
pub fn capture(level: level.Level) -> Option(Location)
