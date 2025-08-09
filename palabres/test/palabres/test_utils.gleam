import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/function
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import simplifile

pub const log_file = "/tmp/palabres.log"

@external(erlang, "palabres_test_ffi", "destroy")
@external(javascript, "../palabres.ffi.mjs", "destroy")
pub fn destroy_logger() -> Nil

@external(erlang, "thoas", "encode")
@external(javascript, "../palabres_test.ffi.mjs", "encode")
fn encode(a: Dict(String, Dynamic)) -> String

@external(erlang, "palabres_test_ffi", "sleep")
@external(javascript, "../palabres_test.ffi.mjs", "sleep")
pub fn sleep(timeout: Int, continuation: fn() -> a) -> Nil

pub fn read_logs() {
  let assert Ok(file) = simplifile.read(log_file)
  file
  |> string.split("\n")
  |> list.map(clean)
  |> string.join("\n")
}

pub fn remove_logs() {
  use _ <- function.tap(Nil)
  let _ = simplifile.delete(log_file)
}

fn clean(content: String) {
  content
  |> json.parse(decode.dict(decode.string, decode.dynamic))
  |> result.map(dict.delete(_, "when"))
  |> result.map(dict.delete(_, "id"))
  |> result.map(encode)
  |> result.lazy_unwrap(fn() {
    content
    |> string.split(on: " ")
    |> list.filter(remove_when_id)
    |> string.join(with: " ")
  })
}

fn remove_when_id(part: String) {
  let is_when = string.contains(part, "when")
  let is_id = string.contains(part, "id")
  !{ is_when || is_id }
}

@target(javascript)
pub fn multitarget(title: String) {
  "js_" <> title
}

@target(erlang)
pub fn multitarget(title: String) {
  "erlang_" <> title
}
