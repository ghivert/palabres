//// Options to configure the request logging middleware.
////
//// ```gleam
//// import palabres_wisp
//// import palabres_wisp/options
////
//// pub fn handle_request(req: Request) -> Response {
////   let log_options =
////     options.defaults()
////     // Never log those paths.
////     |> options.except(["/health"])
////     // Decide for every other request and response.
////     |> options.filter(fn(req, response) { response.status != 304 })
////   use <- palabres_wisp.log_request(req, log_options)
////   route(req)
//// }
//// ```

import gleam/list
import wisp.{type Request, type Response}

/// Options for [`log_request`](../palabres_wisp.html#log_request). To create
/// with [`defaults`](#defaults).
///
/// - [`except`](#except) Defaults to `[]`. \
///   Paths that are never logged.
/// - [`filter`](#filter) Defaults to logging everything. \
///   A predicate deciding whether a request is logged, given the request and
///   its response.
pub opaque type Options {
  Options(except: List(String), filter: fn(Request, Response) -> Bool)
}

/// Initialise the options with defaults: every request is logged.
pub fn defaults() -> Options {
  Options(except: [], filter: fn(_, _) { True })
}

/// Defaults to `[]`. \
/// Skip the log for the given paths. Useful to keep health checks or metrics
/// endpoints, polled every few seconds, out of your logs.
///
/// ```gleam
/// options.defaults()
/// |> options.except(["/health", "/metrics"])
/// ```
pub fn except(options: Options, paths: List(String)) -> Options {
  Options(..options, except: paths)
}

/// Defaults to logging everything. \
/// Decide yourself whether a request should be logged, from the request and
/// the response it got. Return `True` to log, `False` to skip. Paths given to
/// [`except`](#except) are skipped before the filter runs.
///
/// ```gleam
/// options.defaults()
/// |> options.filter(fn(req, response) {
///   req.method != http.Head && response.status != 304
/// })
/// ```
pub fn filter(
  options: Options,
  keep: fn(Request, Response) -> Bool,
) -> Options {
  Options(..options, filter: keep)
}

/// Whether the request should be logged, according to the options.
@internal
pub fn should_log(options: Options, req: Request, response: Response) -> Bool {
  !list.contains(options.except, req.path) && options.filter(req, response)
}
