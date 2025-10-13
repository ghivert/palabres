//// Collection of utils to work with internal structures.
//// Should never be leaked outside of the package.

/// Generates the current date (i.e. now) in ISO-8601 format, in string.
/// The date is in UTC.
@external(erlang, "palabres_utils_ffi", "format_iso8601")
@external(javascript, "../../palabres.ffi.mjs", "formatIso8601")
pub fn iso8601() -> String
