import gleam/list
import palabres/level
import startest.{describe, it}
import startest/expect

const levels = [
  #("emergency", level.Emergency),
  #("alert", level.Alert),
  #("critical", level.Critical),
  #("error", level.Error),
  #("warning", level.Warning),
  #("notice", level.Notice),
  #("info", level.Info),
  #("debug", level.Debug),
]

pub fn level_tests() {
  describe("palabres/level", [
    describe("from_string", [
      it("validates correct log levels", fn() {
        use #(str, expected) <- list.each(levels)
        level.from_string(str)
        |> expect.to_equal(Ok(expected))
      }),
      it("fails with invalid log level", fn() {
        level.from_string("anything")
        |> expect.to_be_error
      }),
    ]),
  ])
}
