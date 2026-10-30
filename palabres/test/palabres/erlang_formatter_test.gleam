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
pub fn should_not_crash_on_bad_format_test() {
  let expected = "level=info FORMAT ERROR: \"user ~p logged in ~p\" - [-42]\n"
  assert text("bad_format_args") == expected
}
