import birdie
import envoy
import gleam/list
import gleam/option
import gleam/string
import gleeunit
import palabres
import palabres/field
import palabres/level.{Debug, Info}
import palabres/options
import palabres/test_utils

pub fn main() {
  gleeunit.main()
}

fn configure_logger(opts: Options) -> Nil {
  options.defaults()
  |> options.color(opts.color)
  |> options.json(opts.json)
  |> options.level(opts.level)
  |> options.output(to: {
    options.file(test_utils.log_file())
    |> options.flush(every: 10)
  })
  |> options.fields(opts.fields)
  |> options.style_default_logger(True)
  |> options.auto_at(opts.auto_at)
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

/// Messages without explicit `at`, to exercise the automatic location.
fn located_messages() {
  // From a named function: `at` points to this function on both targets.
  palabres.info("Located message")
  |> palabres.string("test_field1", "test_value1")
  |> palabres.log

  // From an anonymous function: Erlang recovers the enclosing function name,
  // while JavaScript only knows the module.
  list.each([Nil], fn(_) {
    palabres.notice("Located from anonymous function")
    |> palabres.log
  })

  // `log_*` helpers are located too. Note it is not the last expression of
  // this function, otherwise Erlang would see a tail call and report the
  // caller of `located_messages` instead.
  palabres.log_warning("Located one-liner")

  // Explicit `at` takes precedence over the automatic location.
  palabres.info("Explicit location")
  |> palabres.at("my_module", "my_function")
  |> palabres.log
}

type Options {
  Options(
    auto_at: Bool,
    color: Bool,
    json: Bool,
    level: level.Level,
    fields: List(field.Field),
  )
}

fn run(title: String, options: Options, next: fn() -> Nil) -> Nil {
  run_with(title, options, example_messages, next)
}

fn run_with(
  title: String,
  options: Options,
  messages: fn() -> Nil,
  next: fn() -> Nil,
) -> Nil {
  let title = test_utils.multitarget(title)
  configure_logger(options)
  messages()
  use <- test_utils.sleep(100)
  let content = test_utils.read_logs()
  test_utils.destroy_logger()
  test_utils.remove_logs()
  birdie.snap(title:, content:)
  next()
}

pub fn should_print_in_color_test() {
  use <- run("should print in color", {
    Options(auto_at: True, color: True, json: False, level: Debug, fields: [])
  })
  Nil
}

pub fn should_print_without_color_test() {
  use <- run("should print without color", {
    Options(auto_at: True, color: False, json: False, level: Debug, fields: [])
  })
  Nil
}

pub fn should_print_in_json_test() {
  use <- run("should print in JSON", {
    Options(auto_at: True, color: False, json: True, level: Debug, fields: [])
  })
  Nil
}

pub fn should_print_in_json_and_ignore_color_test() {
  use <- run("should print in JSON and ignore color", {
    Options(auto_at: True, color: True, json: True, level: Debug, fields: [])
  })
  Nil
}

pub fn should_ignore_low_levels_test() {
  use <- run("should ignore low levels", {
    Options(auto_at: True, color: True, json: True, level: Info, fields: [])
  })
  Nil
}

fn global_fields() {
  [
    field.string("global_field1", "example1"),
    field.int("global_field2", 2),
    field.string("global_field1", "example3"),
    field.list("global_field3", [1, 2], field.int),
    field.nullable("global_field4", option.None, field.string),
    field.bool("global_field5", True),
  ]
}

fn global_lazy_fields() {
  [
    field.lazy(fn() { field.string("global_lazy_field1", "lazy-example1") }),
    field.lazy(fn() { field.string("global_lazy_field2", "lazy-example2") }),
    field.lazy(fn() { field.string("global_lazy_field1", "lazy-example3") }),
  ]
}

pub fn should_use_global_fields_test() {
  use <- run("should use global fields", {
    Options(
      auto_at: True,
      color: True,
      json: False,
      level: Debug,
      fields: global_fields(),
    )
  })
  Nil
}

pub fn should_use_json_global_fields_test() {
  use <- run("should use JSON global fields", {
    Options(
      auto_at: True,
      color: True,
      json: True,
      level: Debug,
      fields: global_fields(),
    )
  })
  Nil
}

pub fn should_use_global_lazy_fields_test() {
  use <- run("should use global lazy fields", {
    Options(
      auto_at: True,
      color: True,
      json: False,
      level: Debug,
      fields: global_lazy_fields(),
    )
  })
  Nil
}

pub fn should_use_json_global_lazy_fields_test() {
  use <- run("should use JSON global lazy fields", {
    Options(
      auto_at: True,
      color: True,
      json: True,
      level: Debug,
      fields: global_lazy_fields(),
    )
  })
  Nil
}

/// A field set on the log wins over a global field with the same key.
fn overriding_messages() {
  palabres.info("Overriding testing message")
  |> palabres.at("palabres_test", "messages")
  |> palabres.string("global_field1", "from_log")
  |> palabres.log
}

pub fn should_override_global_fields_test() {
  let options =
    Options(auto_at: True, color: False, json: False, level: Debug, fields: [
      field.string("global_field1", "from_global"),
      field.string("global_field2", "from_global"),
    ])
  let title = "should override global fields"
  use <- run_with(title, options, overriding_messages)
  Nil
}

pub fn should_override_json_global_fields_test() {
  let options =
    Options(auto_at: True, color: False, json: True, level: Debug, fields: [
      field.string("global_field1", "from_global"),
      field.string("global_field2", "from_global"),
    ])
  let title = "should override JSON global fields"
  use <- run_with(title, options, overriding_messages)
  Nil
}

pub fn should_use_environment_variables_test() {
  envoy.set("PALABRES_GLOBAL_FIELD1", "example1")
  envoy.set("PALABRES_GLOBAL_FIELD2", "example2")
  envoy.set("OTHER_GLOBAL_FIELD3", "example3")
  use <- run("should use global fields environment variables", {
    Options(
      auto_at: True,
      color: True,
      json: False,
      level: Debug,
      fields: field.from_environment(prefix: "PALABRES_"),
    )
  })
  envoy.unset("PALABRES_GLOBAL_FIELD1")
  envoy.unset("PALABRES_GLOBAL_FIELD2")
  envoy.unset("OTHER_GLOBAL_FIELD3")
}

pub fn should_use_json_environment_variables_test() {
  envoy.set("PALABRES_GLOBAL_FIELD1", "example1")
  envoy.set("PALABRES_GLOBAL_FIELD2", "example2")
  envoy.set("OTHER_GLOBAL_FIELD3", "example3")
  use <- run("should use JSON global fields environment variables", {
    Options(
      auto_at: True,
      color: True,
      json: True,
      level: Debug,
      fields: field.from_environment(prefix: "PALABRES_"),
    )
  })
  envoy.unset("PALABRES_GLOBAL_FIELD1")
  envoy.unset("PALABRES_GLOBAL_FIELD2")
  envoy.unset("OTHER_GLOBAL_FIELD3")
}

pub fn should_locate_logs_test() {
  let options =
    Options(auto_at: True, color: True, json: False, level: Debug, fields: [])
  use <- run_with("should locate logs", options, located_messages)
  Nil
}

pub fn should_locate_logs_in_json_test() {
  let options =
    Options(auto_at: True, color: False, json: True, level: Debug, fields: [])
  use <- run_with("should locate logs in JSON", options, located_messages)
  Nil
}

pub fn should_not_locate_logs_when_disabled_test() {
  let options =
    Options(auto_at: False, color: False, json: False, level: Debug, fields: [])
  let title = "should not locate logs when disabled"
  use <- run_with(title, options, located_messages)
  Nil
}

fn configure_when(auto_when: Bool) -> Nil {
  options.defaults()
  |> options.color(False)
  |> options.auto_at(False)
  |> options.auto_when(auto_when)
  |> options.level(Debug)
  |> options.output(to: {
    options.file(test_utils.log_file())
    |> options.flush(every: 10)
  })
  |> palabres.configure
}

/// `when` is stripped from snapshots, so timestamps are checked by hand.
fn run_when(auto_when: Bool, next: fn(String) -> Nil) -> Nil {
  configure_when(auto_when)
  palabres.log_info("Timestamp testing message")
  use <- test_utils.sleep(100)
  let content = test_utils.read_raw_logs()
  test_utils.destroy_logger()
  test_utils.remove_logs()
  next(content)
}

pub fn should_add_timestamp_test() {
  use content <- run_when(True)
  assert string.starts_with(content, "level=info when=")
}

pub fn should_not_add_timestamp_when_disabled_test() {
  use content <- run_when(False)
  assert string.starts_with(content, "level=info Timestamp testing message")
}
