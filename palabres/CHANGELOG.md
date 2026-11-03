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
- Add `options.fields` and the public `palabres/field` module to attach
  fields to every log. It replaces the hidden `options.default_*` and
  `options.default_lazy_*` functions: build fields with `field.string`,
  `field.int`, `field.float`, `field.bool`, `field.null`, `field.nullable`,
  `field.list` and `field.lazy`, and pass them in one list.
- A field set on a log now takes precedence over a global field with the same
  key. Global fields used to overwrite the log field in JSON output, and to be
  duplicated in string output.
- `PALABRES_*` environment variables are no longer read implicitly by
  `palabres.configure`. Read them explicitly with
  `field.from_environment(prefix: "PALABRES_")`. Values are always strings,
  instead of being guessed as booleans, integers or floats.

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
