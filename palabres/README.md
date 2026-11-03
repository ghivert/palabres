# Palabres

> Palabres **[\\pa.labʁ\\]** \
> French word, coming from the Spanish word _palabra_, meaning "parole" &
> "word". \
>
> 1. Long & hard discussion to get a precise result.
> 2. An interminable and often idle discussion.

Palabres is an opinionated logger, built in Gleam for Gleamlins, targeting both
Erlang and JavaScript. Palabres tries to be compatible with as many runtimes as
possible, so you can expect it to work on BEAM & any JavaScript runtime
supporting Node.js imports, like Node, Deno or Bun.

Palabres tries to stay simple yet powerful. By using structured data, text
messages and potential metadata, Palabres tries to provide a nice and pleasant
experience out-of-the-box with few to no configuration and helps to build &
monitor complex systems.

> ⚠️ **About Erlang/OTP compatibility**
>
> Palabres requires Erlang/OTP to support
> [`json`](https://www.erlang.org/doc/apps/stdlib/json.html) module. `json` is
> supported starting with OTP 27. All Erlang/OTP versions before 27 **will not
> work** with Palabres.

## But what does it look like?

Generally, when you're starting out a project, you rely on the default logger,
with its default abilities. However, in the long run, you probably want to add
context, to add advanced monitoring, to follow user flows, and much more, in your
logs. Outputting plain text in such case can make the task hard. That's where
Palabres comes into play, it helps you to go from

```sh
INFO Server listening on http://0.0.0.0:3000
INFO 200 GET /healthcheck
```

to

```json
{
  "__message": "Server started, listening",
  "at": "my_app.main",
  "host": "0.0.0.0",
  "level": "info",
  "port": 3000,
  "scheme": "http",
  "when": "2024-12-16T17:43:40Z"
}
{
  "at": "palabres_wisp.log_request",
  "level": "info",
  "method": "GET",
  "status": 404,
  "when": "2024-12-16T17:43:59Z",
  "where": "/"
}
```

## Installation

```sh
gleam add palabres@1
```

## Getting started

To get started, everything you need is to configure your logger, and you're
done! Everything remaining is to use Palabres to create logs!

```gleam
import palabres
import palabres/options
import palabres/level

pub fn configure_logger() {
  options.defaults()
  |> options.color(True)
  |> options.json(False)
  // The minimum log level. Defaults to `level.Info`, which means `debug`
  // logs are dropped unless you lower it, like here.
  |> options.level(level.Debug)
  |> options.output(to: options.stdout())
  |> palabres.configure
}

pub fn log_message() {
  palabres.debug("My debug message")
  |> palabres.string("my_field", "content")
  |> palabres.int("my_other_field", 12)
  |> palabres.log
}
```

And you're good to go! Every log is automatically tagged with its level, its
timestamp, and its location in your code base, as `at=module.function`, so you
can find where a log comes from without adding anything:

```
level=debg at=my_app.log_message when=2024-12-16T17:43:40Z my_field=content my_other_field=12 My debug message
```

Location and timestamp can be deactivated with `options.auto_at(False)` and
`options.auto_when(False)`.

Fields that are expensive to compute can be deferred with `palabres.lazy`: the
function only runs when the log is emitted, so a dropped `debug` log costs
nothing.

```gleam
palabres.debug("User fetched")
|> palabres.lazy(fn(log) {
  log
  |> palabres.string("user", render_user(user))
})
|> palabres.log
```

### Global fields

Some context is true for the whole process: the service name, its version, the
hostname, the environment… Declare it once with `options.fields`, and it is
added to every log, including, on Erlang, the logs of other libraries flowing
through `logger`. Fields are built with the `palabres/field` module, which
mirrors the `palabres.string`, `palabres.int`, etc. functions used on logs.

```gleam
import palabres
import palabres/field
import palabres/options

pub fn configure_logger() {
  options.defaults()
  |> options.fields([
    field.string("service", "my_service"),
    field.int("version", 3),
    // Computed every time a log is printed, not once at configuration.
    field.lazy(fn() { field.string("hostname", read_hostname()) }),
    // Read the `PALABRES_*` environment variables, as string fields.
    ..field.from_environment(prefix: "PALABRES_")
  ])
  |> palabres.configure
}
```

```
level=info at=my_app.main service=my_service version=3 hostname=web-1 Server started
```

A field set on a log takes precedence over a global field with the same key.

Find the entire explanations on
[Hexdocs](https://hexdocs.pm/palabres/palabres.html).

## Integrations

Palabres provides out-of-the-box integration with `wisp`, the main webserver on
Gleam! To get it, you can take a look at
[`palabres_wisp`](https://hexdocs.pm/palabres_wisp) or follow the next steps.

### Using `palabres_wisp`

First, install the dependency:

```sh
gleam add palabres_wisp
```

After you configured the logger as indicated above, simply use the provided
middleware!

```gleam
import palabres_wisp
import wisp

pub fn handle_request(request: wisp.Request) {
  use <- palabres_wisp.log_request(request)
  wisp.ok()
}
```
