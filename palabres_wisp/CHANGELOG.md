### Unreleased

- Dependency bump: `wisp` 2.2, `gleam_stdlib` 1.0, `mist` 6.
- Require `palabres` >= 1.0.0, `wisp` >= 2.0.0 and `gleam_http` >= 4.0.0.
- Add a test suite.
- `log_request` now logs the duration of the request in milliseconds in `duration`, and
  picks its level from the response status: `error` for 5xx, `warning` for
  4xx, `info` otherwise.
- `log_request` sets an explicit `at=palabres_wisp.log_request`.
- `log_request` now takes options, built with `palabres_wisp/options`:
  `except` skips the log for some paths, like health checks, and `filter`
  lets you decide from the request and its response.

### v1.0.2 - 2025-12-24

- Dependency bump for Wisp 2.0.

### v1.0.1 - 2025-10-13

- Switch log level to `INFO`.

### v1.0.0 - 2025-08-09

- No changes, dependency bump.

### v0.4.0 - 2025-03-07

- First publication! 🎉
