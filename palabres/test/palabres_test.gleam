import birdie
import gleam/option
import gleeunit
import palabres
import palabres/level.{Debug, Info}
import palabres/options
import palabres/test_utils

pub fn main() {
  gleeunit.main()
}

fn configure_logger(
  color color: Bool,
  json json: Bool,
  level level: level.Level,
) -> Nil {
  options.defaults()
  |> options.color(color)
  |> options.json(json)
  |> options.level(level)
  |> options.output(to: {
    options.file(test_utils.log_file)
    |> options.flush(every: 10)
  })
  |> options.style_default_logger(True)
  |> palabres.configure
}

fn messages() {
  // Print a debug message, should be displayed almost every time while
  // log level, because log level should be Debug almost every time in tests.
  palabres.debug("Debug testing message")
  |> palabres.at("palabres_test", "messages")
  |> palabres.string("test_field1", "test_value1")
  |> palabres.string("test_field1", "test_value2")
  |> palabres.string("test_field2", "test_value1")
  |> palabres.log

  // Print an info message, that should be display every time.
  palabres.info("Info testing message")
  |> palabres.at("palabres_test", "messages")
  |> palabres.string("test_field1", "test_value1")
  |> palabres.string("test_field1", "test_value2")
  |> palabres.string("test_field2", "test_value1")
  |> palabres.log

  palabres.alert("Alert testing message")
  |> palabres.at("palabres_test", "messages")
  |> palabres.string("test_field1", "test_value1")
  |> palabres.int("test_field2", 1)
  |> palabres.float("test_field3", 1.0)
  |> palabres.nullable("test_field4", option.None, palabres.string)
  |> palabres.nullable("test_field5", option.Some(1), palabres.int)
  |> palabres.log
}

fn run(title: String, color, json, level, run_test: fn() -> Nil) {
  configure_logger(color, json, level)
  run_test()
  use <- test_utils.sleep(100)
  let content = test_utils.read_logs()
  test_utils.remove_logs()
  test_utils.destroy_logger()
  birdie.snap(title:, content:)
}

pub fn should_print_in_color_test() {
  run("should print in color", True, False, Debug, messages)
}

pub fn should_print_without_color_test() {
  run("should print without color", False, False, Debug, messages)
}

pub fn should_print_in_json_test() {
  run("should print in JSON", False, True, Debug, messages)
}

pub fn should_print_in_json_and_ignore_color_test() {
  run("should print in JSON and ignore color", True, True, Debug, messages)
}

pub fn should_ignore_low_levels_test() {
  run("should ignore low levels", True, True, Info, messages)
}
