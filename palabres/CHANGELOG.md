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
