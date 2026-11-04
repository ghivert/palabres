import gleam/http
import gleam/int
import gleam/string
import palabres.{type Log}
import wisp.{type Request, type Response}

/// Provides a middleware to display every incoming request for a Wisp server.
/// Use it in your router to log requests with their status code, method, path
/// and duration in milliseconds. The log level depends on the response status:
/// `error` for 5xx, `warning` for 4xx, `info` otherwise.
///
/// ```gleam
/// import palabres_wisp
/// import wisp
/// pub fn router(req: wisp.Request, ctx: context) -> wisp.Response {
///   use <- palabres_wisp.log_request(req)
///   route_request(req)
/// }
/// // level=info when=2024-12-15T15:59:17Z
/// //   status=200 method=GET where=/users duration=1.234
/// ```
pub fn log_request(req: Request, handler: fn() -> Response) -> Response {
  let start = monotonic_time()
  let response = handler()
  let duration = int.to_float(monotonic_time() - start) /. 1000.0
  let method = string.uppercase(http.method_to_string(req.method))
  request_log(response.status)
  |> palabres.int("status", response.status)
  |> palabres.string("method", method)
  |> palabres.string("where", req.path)
  |> palabres.float("duration", duration)
  |> palabres.log
  response
}

fn request_log(status: Int) -> Log {
  case status {
    status if status >= 500 -> palabres.error("")
    status if status >= 400 -> palabres.warning("")
    _ -> palabres.info("")
  }
}

@external(erlang, "palabres_wisp_ffi", "monotonic_time")
fn monotonic_time() -> Int
