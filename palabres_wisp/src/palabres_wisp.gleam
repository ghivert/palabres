import exception
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/http
import gleam/int
import gleam/string
import palabres.{type Log}
import palabres_wisp/options.{type Options}
import wisp.{type Request, type Response}

/// Provides a middleware to display every incoming request for a Wisp server.
/// Use it in your router to log requests with their status code, method, path
/// and duration in milliseconds. The log level depends on the response status:
/// `error` for 5xx, `warning` for 4xx, `info` otherwise. Requests can be kept
/// out of the logs with [`options`](./palabres_wisp/options.html).
///
/// ```gleam
/// import palabres_wisp
/// import palabres_wisp/options
/// import wisp
/// pub fn router(req: wisp.Request, ctx: context) -> wisp.Response {
///   use <- palabres_wisp.log_request(req, options.defaults())
///   route_request(req)
/// }
/// // level=info at=palabres_wisp.log_request when=2024-12-15T15:59:17Z
/// //   status=200 method=GET where=/users duration=1.234
/// ```
pub fn log_request(
  req: Request,
  options: Options,
  handler: fn() -> Response,
) -> Response {
  let start = monotonic_time()
  let response = handler()
  case options.should_log(options, req, response) {
    False -> Nil
    True -> {
      let duration = int.to_float(monotonic_time() - start) /. 1000.0
      let method = string.uppercase(http.method_to_string(req.method))
      request_log(response.status)
      |> palabres.at(module: "palabres_wisp", function: "log_request")
      |> palabres.int("status", response.status)
      |> palabres.string("method", method)
      |> palabres.string("where", req.path)
      |> palabres.float("duration", duration)
      |> palabres.log
    }
  }
  response
}

/// Provides a middleware to rescue crashes, and returns a response with
/// status code 500: Internal Server Error. Replaces `wisp.rescue_crashes`, and
/// logs the crash as a structured `error` log, with the kind of crash in the
/// `class` field (`error`, `throw` or `exit`). Gleam panics and failed
/// `let assert` get `message`, `module`, `function` and `line` fields, while
/// any other crash gets its inspected reason in the `reason` field.
///
/// ```gleam
/// pub fn router(req: wisp.Request, ctx: context) -> wisp.Response {
///   use <- palabres_wisp.log_request(req, options.defaults())
///   use <- palabres_wisp.rescue_crashes
///   route_request(req)
/// }
/// ```
pub fn rescue_crashes(handler: fn() -> Response) -> Response {
  case exception.rescue(handler) {
    Ok(response) -> response
    Error(error) -> {
      let #(class, detail) = case error {
        exception.Errored(detail) -> #("error", detail)
        exception.Thrown(detail) -> #("throw", detail)
        exception.Exited(detail) -> #("exit", detail)
      }
      palabres.error("Request handler crashed")
      |> palabres.at(module: "palabres_wisp", function: "rescue_crashes")
      |> palabres.string("class", class)
      |> crash_fields(detail)
      |> palabres.log
      wisp.internal_server_error()
    }
  }
}

/// Gleam panics are raised as maps with atom keys, holding the message and
/// the location of the crash. Extract them when present, and fall back on
/// the inspected reason for any other crash.
fn crash_fields(log: Log, detail: Dynamic) -> Log {
  let decoder = {
    use message <- decode.field(atom.create("message"), decode.string)
    use module <- decode.field(atom.create("module"), decode.string)
    use function <- decode.field(atom.create("function"), decode.string)
    use line <- decode.field(atom.create("line"), decode.int)
    decode.success(#(message, module, function, line))
  }
  case decode.run(detail, decoder) {
    Error(_) -> palabres.string(log, "reason", string.inspect(detail))
    Ok(#(message, module, function, line)) ->
      log
      |> palabres.string("message", message)
      |> palabres.string("module", module)
      |> palabres.string("function", function)
      |> palabres.int("line", line)
  }
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
