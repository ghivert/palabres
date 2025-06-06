import gleam/list
import palabres/level

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

pub fn validates_correct_log_levels_test() {
  use #(str, expected) <- list.each(levels)
  let level = level.from_string(str)
  assert level == Ok(expected)
}

pub fn fails_with_invalid_log_level_test() {
  let level = level.from_string("anything")
  assert level == Error(Nil)
}
