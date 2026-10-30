### Unreleased

- Add `palabres.lazy` to defer part of a log construction until the log is
  emitted, so expensive fields are never computed for logs dropped by the
  configured level.
- Automatically add the `at` field to every log, pointing to the module and
  the function creating the log. `palabres.at` is now only needed to override
  the detected location. Deactivate it with `options.auto_at(False)`.
- Add `options.auto_when(False)` to deactivate the automatic `when` field,
  for log collectors adding their own timestamp.
- Fix JSON output on JavaScript with `gleam_stdlib` 1.0, and sort JSON keys to
  get the same output on both targets.
- Sort `PALABRES_*` environment variables to keep default fields in a
  deterministic order.
- Format `{Format, Args}` messages sent to `logger` on Erlang, like
  `logger:info("user ~p", [Id])`, with `io_lib:format/2` instead of inspecting
  them.
- Display reports sent to `logger` on Erlang as fields, like `user=42`, typed in
  JSON instead of serialised as arrays of integers, with charlists as strings.
  OTP crash reports are rendered through their `report_cb`, as OTP does, and
  plain `{string, Charlist}` messages are proper JSON strings.
- Decorate plain `logger` messages like Palabres logs on Erlang: their default
  fields are scalars in JSON, like on Palabres logs, instead of arrays.
- Use the timestamp of the event for `when` on Erlang, with millisecond
  precision like JavaScript, instead of the formatting time in seconds.
- Send Palabres logs as a valid `logger` report on Erlang, `#{palabres =>
  {Fields, Message, At}}`, so that third-party handlers can receive them.
- Install a single primary `logger` filter on Erlang, under the id
  `palabres_domain`, instead of two conflicting `domain` filters.
- Follow the `NO_COLOR` convention on both targets: any non-empty value disables
  colour, including `false`; an empty value keeps it.
- Reconfigure the handler when `palabres.configure` is called a second time on
  Erlang, instead of silently keeping the first configuration.
- Fix the file output on JavaScript: logs go through a single stream, so each
  log ends up on its own line, and a file output without `options.flush` writes
  too. `options.flush` has no effect on JavaScript, where logs are written as
  they come. Every log now ends with a newline, as on Erlang, which is the only
  change in the JavaScript snapshots.

### v1.0.4 - 2025-12-24

- Adapt code to Gleam 1.13.0.

### v1.0.3 - 2025-11-10

- Fix a bug where JavaScript `Logger` was failing when `output` was `Stdout`.

### v1.0.2 - 2025-10-24

- Fix a bug where `at` was displayed in JSON output.

### v1.0.1 - 2025-10-13

- Remove `id` from the generated log, as it's often useless in the log.

### v1.0.0 - 2025-08-09

- Add `bool` field handler.
- Add `list` field handler.
- Fix JavaScript runtime.
- Fix JSON generation: `null` and numbers are now properly JSON serialised.
- Improve internal code and make it easier to maintain.

### v0.4.1 - 2025-03-07

- Fix JS runtime

### v0.4.0 - 2025-03-07

- Refactor formatting to avoid `persistent_term` on Erlang target.
- Separate `palabres_wisp` from `palabres` to avoid unnecessary dependencies.

### v0.3.0 - 2025-02-25

- Add `nullable` function to help working with `Option(x)`.
- Add documentation for `int`, `float` and `string`.

### v0.2.0 - 2024-12-17

- Add `log_*` functions to avoid writing `palabres.*() |> palabres.log`.
- Change `log_request` level from `INFO` to `DEBUG`.

### v0.1.0 - 2024-12-17

- First publication! 🎉
