//// Tests of the Erlang formatter on events not emitted by Palabres: plain
//// `logger` calls from Erlang code and OTP reports. Events are injected
//// directly in `palabres_ffi:format/2`, which is a pure function. The module
//// is Erlang only, and imports nothing on purpose: an import would be
//// reported as unused on JavaScript, where only the constant below remains,
//// so that the module is not empty either.

/// Timestamp of every synthetic event, as the test FFI sets it.
pub const timestamp = "2024-12-15T15:59:17.123Z"

@target(erlang)
@external(erlang, "palabres_test_ffi", "format_event")
fn format_event(kind: String, json: Bool, auto_when: Bool) -> String

@target(erlang)
fn text(kind: String) -> String {
  format_event(kind, False, False)
}

@target(erlang)
/// JSON output, with sorted keys and without trailing newline.
fn json(kind: String) -> String {
  format_event(kind, True, False)
}

@target(erlang)
pub fn should_format_io_format_messages_test() {
  assert text("format_args") == "level=info user 42 logged in\n"
  assert json("format_args")
    == "{\"__message\":\"user 42 logged in\",\"level\":\"info\"}"
}

@target(erlang)
pub fn should_format_charlist_messages_test() {
  assert text("string_charlist") == "level=info plain string\n"
  assert json("string_charlist")
    == "{\"__message\":\"plain string\",\"level\":\"info\"}"
}

@target(erlang)
pub fn should_not_crash_on_bad_format_test() {
  let expected = "level=info FORMAT ERROR: \"user ~p logged in ~p\" - [-42]\n"
  assert text("bad_format_args") == expected
}

@target(erlang)
pub fn should_format_map_reports_as_fields_test() {
  assert text("map_report") == "level=info action=login user=42\n"
  assert json("map_report")
    == "{\"action\":\"login\",\"level\":\"info\",\"user\":42}"
}

@target(erlang)
pub fn should_format_list_reports_as_fields_test() {
  assert text("list_report") == "level=info user=42 action=login\n"
  assert json("list_report")
    == "{\"action\":\"login\",\"level\":\"info\",\"user\":42}"
}

@target(erlang)
pub fn should_render_charlists_as_strings_test() {
  assert text("charlist_report") == "level=info name=hello ids=[1,2] empty=[]\n"
  assert json("charlist_report")
    == "{\"empty\":\"[]\",\"ids\":\"[1,2]\",\"level\":\"info\",\"name\":\"hello\"}"
}

@target(erlang)
pub fn should_render_reports_with_report_cb_test() {
  assert text("report_cb_1") == "level=info user 42 crashed\n"
  assert text("report_cb_2") == "level=info user 42 crashed\n"
  assert json("report_cb_2")
    == "{\"__message\":\"user 42 crashed\",\"level\":\"info\"}"
}
