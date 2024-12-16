import birdie
import palabre
import palabre/level.{Debug, Info}
import palabre/options
import palabre/test_utils
import startest.{describe}

@target(erlang)
import wisp

pub fn main() {
  startest.default_config()
  |> startest.run
}

fn configure_logger(
  color color: Bool,
  json json: Bool,
  level level: level.Level,
) -> Nil {
  options.default()
  |> options.color(color)
  |> options.json(json)
  |> options.level(level)
  |> options.output(to: {
    options.file(test_utils.log_file)
    |> options.flush(every: 10)
  })
  |> options.style_default_logger(True)
  |> palabre.configure
}

fn messages() {
  // Print a debug message, should be displayed almost every time while
  // log level, because log level should be Debug almost every time in tests.
  palabre.debug("Debug testing message")
  |> palabre.at("palabre_test", "messages")
  |> palabre.string("test_field1", "test_value1")
  |> palabre.string("test_field1", "test_value2")
  |> palabre.string("test_field2", "test_value1")
  |> palabre.dump

  // Print an info message, that should be display every time.
  palabre.info("Info testing message")
  |> palabre.at("palabre_test", "messages")
  |> palabre.string("test_field1", "test_value1")
  |> palabre.string("test_field1", "test_value2")
  |> palabre.string("test_field2", "test_value1")
  |> palabre.dump
}

fn it(title: String, color, json, level, run_test: fn() -> Nil) {
  startest.it(title, fn() {
    configure_logger(color, json, level)
    run_test()
    use <- test_utils.sleep(100)
    let content = test_utils.read_logs()
    test_utils.remove_logs()
    test_utils.destroy_logger()
    birdie.snap(title:, content:)
  })
}

pub fn palabre_tests() {
  describe("palabre", [
    describe("common", [
      it("should print in color", True, False, Debug, messages),
      it("should print without color", False, False, Debug, messages),
      it("should print in JSON", False, True, Debug, messages),
      it("should print in JSON and ignore color", True, True, Debug, messages),
      it("should ignore low levels", True, True, Info, messages),
    ]),
  ])
}

@target(erlang)
pub fn erlang_tests() {
  describe("palabre", [
    describe("on BEAM", [
      it("should format logger message when asked", True, False, Debug, fn() {
        wisp.log_debug("Test message")
      }),
    ]),
  ])
}
