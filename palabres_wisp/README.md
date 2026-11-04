# Palabres + Wisp

`palabres_wisp` is an integration of [`palabres`](https://hexdocs.pm/palabres)
for [`wisp`](https://hexdocs.pm/wisp). When using `wisp`, you can simply use the
Palabres integration, and get your logs nicely formatted!

## Installation

```sh
gleam add palabres_wisp
```

## Getting started

To get started, everything you need is to configure your logger, calling the
`wisp` middleware and you're done! Everything remaining is to use Palabres to
create logs!

```gleam
import palabres
import palabres/options
import palabres/level
import palabres_wisp
import wisp

pub fn configure_logger() {
  options.defaults()
  |> options.color(True)
  |> options.json(False)
  |> options.output(to: options.stdout())
  |> palabres.configure
}

pub fn handle_request(request: Request) -> Response {
  use <- palabres_wisp.log_request(request)
  wisp.ok()
}
```

Every request is logged with its status, method, path and duration, at a
level depending on the response status: `error` for 5xx, `warning` for 4xx,
`info` otherwise.

```
level=info when=2026-11-03T15:38:19Z status=200 method=GET where=/users duration=0.312
```

And you're good to go! Explore the possibilities you got with Palabres on
[Hexdocs](https://hexdocs.pm/palabres/palabres.html).
