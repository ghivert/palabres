import gleam/dict
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/string
import simplifile

pub const log_file = "/tmp/erlang.log"

@external(erlang, "palabres_test_ffi", "destroy")
@external(javascript, "../palabres.ffi.mjs", "destroy")
pub fn destroy_logger() -> Nil

@external(erlang, "thoas", "encode")
@external(javascript, "../palabres_test.ffi.mjs", "encodeJson")
fn json_encode(a: a) -> String

@external(erlang, "palabres_test_ffi", "sleep")
@external(javascript, "../palabres_test.ffi.mjs", "sleep")
pub fn sleep(timeout: Int, continuation: fn() -> a) -> Nil

fn clean(content: String) {
  case json.parse(content, decode.dict(decode.string, decode.dynamic)) {
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
  let assert Ok(file) = simplifile.read(log_file)
  file
  |> string.split("\n")
  |> list.map(clean)
  |> string.join("\n")
}

pub fn remove_logs() {
  let _ = simplifile.delete(log_file)
  Nil
}
