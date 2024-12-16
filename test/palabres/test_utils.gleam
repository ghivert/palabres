import gleam/dict
import gleam/dynamic
import gleam/json
import gleam/list
import gleam/string
import simplifile
import startest/expect

pub const log_file = "/tmp/erlang.log"

@external(erlang, "palabres_test_ffi", "destroy")
@external(javascript, "../palabres.ffi.mjs", "destroy")
pub fn destroy_logger() -> Nil

@external(erlang, "thoas", "encode")
@external(javascript, "../palabres.ffi.mjs", "encodeJson")
fn json_encode(a: a) -> String

@external(erlang, "palabres_test_ffi", "sleep")
@external(javascript, "../palabres_test.ffi.mjs", "sleep")
pub fn sleep(timeout: Int, continuation: fn() -> a) -> Nil

fn clean(content: String) {
  case json.decode(content, dynamic.dict(dynamic.string, dynamic.dynamic)) {
    Ok(dict) -> dict.delete(dict, "when") |> dict.delete("id") |> json_encode
    Error(_) ->
      content
      |> string.split(" ")
      |> list.filter(remove_when_id)
      |> string.join(" ")
  }
}

fn remove_when_id(part: String) {
  let is_when = string.contains(part, "when")
  let is_id = string.contains(part, "id")
  !{ is_when || is_id }
}

pub fn read_logs() {
  simplifile.read(log_file)
  |> expect.to_be_ok
  |> string.split("\n")
  |> list.map(clean)
  |> string.join("\n")
}

pub fn remove_logs() {
  let _ = simplifile.delete(log_file)
  Nil
}
