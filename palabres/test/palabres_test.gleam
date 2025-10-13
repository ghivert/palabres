import birdie
import envoy
import gleam/list
import gleam/option
import gleeunit
import palabres
import palabres/level.{Debug, Info}
import palabres/options
import palabres/test_utils

pub fn main() {
  gleeunit.main()
}

fn configure_logger(opts: Options) -> Nil {
  let Options(color:, json:, level:, default_fields:) = opts
  options.defaults()
  |> options.color(color)
  |> options.json(json)
  |> options.level(level)
  |> options.output(to: {
    options.file(test_utils.log_file)
    |> options.flush(every: 10)
  })
  |> list.fold(default_fields, _, fn(options, default_field) {
    let #(key, value) = default_field
    options.default_string(options, key, value)
  })
  |> options.style_default_logger(True)
  |> palabres.configure
}

fn example_messages() {
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

type Options {
  Options(
    color: Bool,
    json: Bool,
    level: level.Level,
    default_fields: List(#(String, String)),
  )
}

fn run(options: Options, title: String) -> Nil {
  let title = test_utils.multitarget(title)
  configure_logger(options)
  example_messages()
  use <- test_utils.sleep(100)
  let content = test_utils.read_logs()
  test_utils.destroy_logger()
  test_utils.remove_logs()
  birdie.snap(title:, content:)
}

pub fn should_print_in_color_test() {
  Options(color: True, json: False, level: Debug, default_fields: [])
  |> run("should print in color")
}

pub fn should_print_without_color_test() {
  Options(color: False, json: False, level: Debug, default_fields: [])
  |> run("should print without color")
}

pub fn should_print_in_json_test() {
  Options(color: False, json: True, level: Debug, default_fields: [])
  |> run("should print in JSON")
}

pub fn should_print_in_json_and_ignore_color_test() {
  Options(color: True, json: True, level: Debug, default_fields: [])
  |> run("should print in JSON and ignore color")
}

pub fn should_ignore_low_levels_test() {
  Options(color: True, json: True, level: Info, default_fields: [])
  |> run("should ignore low levels")
}

pub fn should_use_default_fields_test() {
  Options(color: True, json: False, level: Debug, default_fields: [
    #("default_field1", "example1"),
    #("default_field2", "example2"),
    #("default_field1", "example3"),
  ])
  |> run("should use default fields")
}

pub fn should_use_json_default_fields_test() {
  Options(color: True, json: True, level: Debug, default_fields: [
    #("default_field1", "example1"),
    #("default_field2", "example2"),
    #("default_field1", "example3"),
  ])
  |> run("should use JSON default fields")
}

pub fn should_use_environment_variables_test() {
  envoy.set("PALABRES_DEFAULT_FIELD1", "example1")
  envoy.set("PALABRES_DEFAULT_FIELD2", "example2")
  Options(color: True, json: False, level: Debug, default_fields: [])
  |> run("should use default fields environment variables")
  envoy.unset("PALABRES_DEFAULT_FIELD1")
  envoy.unset("PALABRES_DEFAULT_FIELD2")
}

pub fn should_use_json_environment_variables_test() {
  envoy.set("PALABRES_DEFAULT_FIELD1", "example1")
  envoy.set("PALABRES_DEFAULT_FIELD2", "example2")
  Options(color: True, json: True, level: Debug, default_fields: [])
  |> run("should use JSON default fields environment variables")
  envoy.unset("PALABRES_DEFAULT_FIELD1")
  envoy.unset("PALABRES_DEFAULT_FIELD2")
}
