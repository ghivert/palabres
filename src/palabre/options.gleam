import gleam/option.{type Option, None, Some}
import palabre/level

pub opaque type Options {
  Options(
    color: Option(Bool),
    json: Bool,
    level: level.Level,
    output: Output,
    style_default: Bool,
  )
}

pub opaque type Output {
  File(filename: String, flush_interval: Option(Int))
  Stdout
}

pub fn default() -> Options {
  Options(
    color: None,
    json: False,
    level: level.Info,
    output: Stdout,
    style_default: True,
  )
}

pub fn color(options: Options, color: Bool) -> Options {
  Options(..options, color: Some(color))
}

pub fn json(options: Options, json: Bool) -> Options {
  Options(..options, json:)
}

pub fn level(options: Options, level: level.Level) -> Options {
  Options(..options, level:)
}

pub fn output(options: Options, to output: Output) -> Options {
  Options(..options, output:)
}

pub fn file(filename: String) -> Output {
  File(filename:, flush_interval: None)
}

pub fn style_default_logger(options: Options, style_default: Bool) -> Options {
  Options(..options, style_default:)
}

pub fn stdout() -> Output {
  Stdout
}

pub fn flush(file: Output, every interval: Int) -> Output {
  case file {
    File(..) -> File(..file, flush_interval: Some(interval))
    Stdout -> Stdout
  }
}
