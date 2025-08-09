# Palabres

> Palabres **[\\pa.labʁ\\]** \
> French word, coming from the Spanish word _palabra_, meaning "parole" &
> "word". \
>
> 1. Long & hard discussion to get a precise result.
> 2. An interminable and often idle discussion.

Palabres is an opinionated logger, built in Gleam for Gleamlins, targetting both
Erlang and JavaScript. Palabres tries to be compatible with the most runtimes
possible, so you can expect it to work on BEAM & any flavour of
JavaScript — with 100% compatibility while the runtime respect Node.js imports.

Palabres tries to stay simple yet powerful. By using structured data, text
messages and potential metadata, Palabres tries to provides a nice and pleasant
experience out-of-the-box with few to no configuration and helps to build &
monitor complex system.

> ⚠️ **About Erlang/OTP compatibility**
>
> Palabres requires Erlang/OTP to support
> [`json`](https://www.erlang.org/doc/apps/stdlib/json.html) module. `json` is
> supported starting with OTP 27. All Erlang/OTP versions before 27 **will not
> work** with Palabres.

## But what does it look like?

Generally, when you're starting out a project, you rely on the default logger,
with its default abilities. However, in the long run, you probably want to add
context, to add advance monitoring, to follow user flows, and much more, in your
logs. Outputting plain text in such case can make the task hard. That's where
Palabres comes into play, it helps you to go from

```sh
INFO Server listening on http://0.0.0.0:3000
INFO 200 GET /healthcheck
```

to

```json
{
  "level": "info",
  "host": "0.0.0.0",
  "id": "3aa3660c-8f1f-4bbc-a530-346e140b0015",
  "message": "Server started, listening",
  "port": "3000",
  "scheme": "http","when":"2024-12-16T17:43:40Z"
}
{
  "level": "info",
  "id": "c848d8f9-6c64-4d25-90dd-3ee3b14ec7a9",
  "method": "GET",
  "status": "404",
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

And you're good to go! Find the entire explanations on
[Hexdocs](https://hexdocs.pm/palabres/palabres.html).

## Integrations

Palabres provides out-of-the-box integration with `wisp`, the main webserver on
Gleam! To get it, you can take a look at
[`palabres_wisp`](https://github.com/ghivert/palabres_wisp) or follow the next
steps.

### Using `palabres_wisp`

First, install the dependency:

```sh
gleam add palabres_wisp`
```

After you configured the logger as indicated abover, simply use the provided
middleware!

```gleam
import palabres_wisp
import wisp

pub fn handle_request(request: Request) {
  use <- palabres_wisp.log_request(request)
  wisp.ok()
}
```
