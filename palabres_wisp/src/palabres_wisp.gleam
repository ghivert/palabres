import gleam/http
import gleam/string
import palabres
import wisp.{type Request, type Response}

/// Provides a middleware to display every incoming request for a Wisp server.\
/// Use it in your router to log request with status code, path and method.
///
/// ```gleam
/// import palabres_wisp
/// import wisp
/// pub fn router(req: wisp.Request, ctx: context) -> wisp.Response {
///   use <- palabres_wisp.log_request(req)
///   route_request(req)
/// }
/// ```
pub fn log_request(req: Request, handler: fn() -> Response) -> Response {
  let response = handler()
  let method = string.uppercase(http.method_to_string(req.method))
  palabres.notice("")
  |> palabres.int("status", response.status)
  |> palabres.string("method", method)
  |> palabres.string("where", req.path)
  |> palabres.log
  response
}
