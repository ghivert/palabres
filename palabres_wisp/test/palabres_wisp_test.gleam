import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/http
import gleam/json
import gleam/list
import gleam/string
import gleeunit
import palabres
import palabres/options
import palabres_wisp
import simplifile
import wisp
import wisp/simulate

const log_file = "/tmp/palabres_wisp.log"

pub fn main() {
  gleeunit.main()
}

pub fn log_request_test() {
  use logs <- with_logs
  let request = simulate.request(http.Post, "/users/1")
  let response = {
    use <- palabres_wisp.log_request(request)
    wisp.response(201)
  }
  // The handler response is returned untouched.
  assert response.status == 201
  let assert [log] = logs()
  assert dict.get(log, "level") == Ok(dynamic.string("info"))
  assert dict.get(log, "at") == Ok(dynamic.string("palabres_wisp.log_request"))
  assert dict.get(log, "status") == Ok(dynamic.int(201))
  assert dict.get(log, "method") == Ok(dynamic.string("POST"))
  assert dict.get(log, "where") == Ok(dynamic.string("/users/1"))
  let assert Ok(duration) = dict.get(log, "duration")
  let assert Ok(duration) = decode.run(duration, decode.float)
  assert duration >=. 0.0
}

pub fn log_request_levels_test() {
  use logs <- with_logs
  let request = simulate.request(http.Get, "/")
  let _ = {
    use <- palabres_wisp.log_request(request)
    wisp.not_found()
  }
  let _ = {
    use <- palabres_wisp.log_request(request)
    wisp.internal_server_error()
  }
  let assert [not_found, crashed] = logs()
  assert dict.get(not_found, "level") == Ok(dynamic.string("warning"))
  assert dict.get(not_found, "status") == Ok(dynamic.int(404))
  assert dict.get(crashed, "level") == Ok(dynamic.string("error"))
  assert dict.get(crashed, "status") == Ok(dynamic.int(500))
}

/// Configure Palabres to write JSON logs in a file, and hand the test a
/// function reading the logs written so far, once the logger has flushed.
fn with_logs(run: fn(fn() -> List(Dict(String, Dynamic))) -> Nil) -> Nil {
  let _ = simplifile.delete(log_file)
  options.defaults()
  |> options.json(True)
  |> options.output(to: options.file(log_file) |> options.flush(every: 10))
  |> palabres.configure
  run(fn() {
    process.sleep(50)
    read_logs()
  })
}

fn read_logs() -> List(Dict(String, Dynamic)) {
  let assert Ok(content) = simplifile.read(log_file)
  content
  |> string.split("\n")
  |> list.filter(fn(line) { line != "" })
  |> list.map(fn(line) {
    let assert Ok(log) =
      json.parse(line, decode.dict(decode.string, decode.dynamic))
    log
  })
}
