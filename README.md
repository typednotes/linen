<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="logo-dark.svg">
    <img src="logo.svg" alt="LINEN" width="460">
  </picture>
</p>

<p align="center">
  A curated standard-library companion for Lean 4 — external concepts ported in,
  with everything Lean already provides stripped out.
</p>

<p align="center">
  <a href="https://github.com/typednotes/linen/actions/workflows/lean_action_ci.yml"><img src="https://github.com/typednotes/linen/actions/workflows/lean_action_ci.yml/badge.svg" alt="CI"></a>
  <a href="https://github.com/typednotes/linen/stargazers"><img src="https://img.shields.io/github/stars/typednotes/linen?style=flat" alt="GitHub Stars"></a>
  <a href="https://github.com/typednotes/linen/blob/main/LICENSE"><img src="https://img.shields.io/github/license/typednotes/linen" alt="License"></a>
  <a href="https://github.com/typednotes/linen"><img src="https://img.shields.io/github/last-commit/typednotes/linen" alt="Last Commit"></a>
  <a href="https://lean-lang.org/"><img src="https://img.shields.io/badge/Lean-4.31.0-blue" alt="Lean 4"></a>
</p>

<p align="center">
  <strong>267 modules</strong> · <strong>371 compile-time theorems</strong> · <strong>3460 <code>#guard</code> checks</strong>
</p>

## Overview

`linen` is a small, opinionated extension of the Lean 4 standard library. When a
useful concept exists in another ecosystem (a Haskell package, another Lean
project, a single module), it is ported in — but **every concept that already
has a Lean standard-library equivalent is replaced with that equivalent**, and
the result is reshaped to follow Lean's own module hierarchy and naming. What
ships is only what core genuinely lacks, written as idiomatic Lean on top of the
standard library.

Three rules hold across the whole library:

- **Stdlib-first** — no bespoke re-implementation of anything core provides
  (e.g. `Id` over a hand-rolled identity functor, `· >=> ·` over a custom
  Kleisli combinator, `List.foldlM` over `foldM`).
- **No `partial`, no `sorry`** — all recursion is structural or has a proven
  termination argument, and proofs are complete.
- **Everything is tested** — each module has a `Tests/` counterpart whose
  `#guard` examples run on every build.

## Features

`linen` covers the following areas — see **[docs/MODULES.md](docs/MODULES.md)**
for the full per-module feature list and module table.

- **`Data.Functor` / `Control`** — functor, applicative & monad constructions
  missing from core (`Compose`/`Product`/`FunctorSum`, `Bifunctor`,
  `Foldable`/`Traversable`, `mtl`-style `Reader`/`State`/`Except`, STM,
  green-thread concurrency, …).
- **`Data.ByteString*` / `Data.String` / `Data.Word8`** — byte strings (strict,
  lazy, short, builder), Base64, case-insensitive text, and ASCII byte
  classification.
- **`Data.Json`** — a tiny JSON library with `ToJSON`/`FromJSON` and proven
  encode→decode round trips.
- **`Data.Map` / `Data.Set` / `Data.IntMap` / `Data.List'` / `Data.List.NonEmpty`
  / …** — Haskell-style container and list APIs over core `Std`/`List` types.
- **`Network.HTTP` / `HTTP2` / `HTTP3` / `Socket` / `TLS` / `QUIC` /
  `WebSockets`** — a full network stack: HTTP/1.1 client & wire types,
  HTTP/2 framing + HPACK, HTTP/3 over QUIC + QPACK, POSIX sockets with a
  green-thread event dispatcher, TLS 1.2/1.3 over OpenSSL, and WebSockets.
- **`Network.WebApp` / `Network.WebApp.Server`** — a WAI-style application
  interface plus an HTTP server implementing it.
- **`Web.Html` / `Web.Css`** — typed HTML5/CSS construction where illegal
  nesting and property/value mismatches are compile-time errors, with `elem!`
  and `rule!` macro sugar.
- **`DataFrame`** — typed tabular data with a proven rectangular invariant,
  CSV I/O, joins, sorting, grouping/aggregation, and statistics.
- **`Database.PostgreSQL` / `Database.SQL`** — libpq FFI bindings and a
  hasql-style typed client (encoders/decoders, sessions, pooling).
- **`Crypto.JOSE`** — JOSE/JWT verification (HMAC/RSA/EC) over OpenSSL.
- **`Options.Applicative`** — `optparse-applicative`-style command-line
  argument parsing.
- **`PostgREST`** — a Lean port of PostgREST's request/response pipeline:
  API request parsing, config, schema cache introspection, query planning,
  auth, and OpenAPI generation.
- **`System.Console.Ansi` / `System.Exit` / `System.Log.FastLogger`** —
  terminal styling, process exit codes, and buffered logging.

## Quick Start

Add to your `lakefile.toml`:

```toml
[[require]]
name = "linen"
git = "https://github.com/typednotes/linen"
rev = "main"
```

Then import what you need:

```lean
import Linen.Data.Functor
import Linen.Control.Monad

open Data.Functor Control.Monad

#eval join (some (some 3))            -- some 3
#eval replicateM 3 (some 7)           -- some [7, 7, 7]
```

## Modules

See **[docs/MODULES.md](docs/MODULES.md)** for the full module table (all 267 modules).

## Build & Test

```bash
lake build          # build the library
lake build Tests    # run every #guard / #eval check
```

## Examples

Example programs live under [`Examples/`](Examples) and share one entrypoint,
`lake exe examples <name> [args...]` (run with no name to list them):

```bash
lake exe examples                  # list the available examples
lake exe examples echo             # green-threaded echo server — self-checking demo (exits 0)
lake exe examples echo serve 9099  # run the echo server forever; then:  nc 127.0.0.1 9099
lake exe examples bench            # network round-trips w/ a few-ms server delay: Green vs blocking pool (same #cores threads)
lake exe examples postgrest        # in-memory PostgREST request handling + OpenAPI spec generation — self-checking demo
lake exe examples quic             # QUIC types/config + HTTP/3 QPACK/frame wire round trip — self-checking demo
lake exe examples recv             # Network.Socket.Blocking accept/connect/send/recv round trip — self-checking demo
lake exe examples resourcet        # Control.Monad.Trans.Resource LIFO cleanup over real scratch files — self-checking demo
lake exe examples conduit          # Data.Conduit / Data.Conduit.Combinators pipelines, incl. bracketP/runConduitRes — self-checking demo
lake exe examples stm              # Control.Monad.STM + Concurrent.STM.{TVar,TMVar,TQueue} — self-checking demo
lake exe examples streaming-commons        # Data.Streaming.Network bindPortTCP/getSocketTCP/acceptSafe/AppData round trip — self-checking demo
lake exe examples streaming-commons serve 9098  # run it forever; then:  nc 127.0.0.1 9098
lake exe examples tls              # Network.TLS.Context handshake over loopback against a self-signed cert — self-checking demo
lake exe examples httpclient       # Network.HTTP.Client connect/request/response + redirect-following, over loopback — self-checking demo
lake exe examples httpconduit      # Network.HTTP.Client.Conduit / Network.HTTP.Simple streaming HTTP, over loopback — self-checking demo
lake exe examples req              # Network.HTTP.Req type-safe req/runReq (HttpBodyAllowed-checked GET/POST), over loopback — self-checking demo
lake exe examples webapp           # Network.WebApp: Application/Middleware/AppM (composeMiddleware/ifRequest/modifyResponse), over loopback — self-checking demo
lake exe examples webappstatic     # Network.WebApp.Static: staticApp/static + defaultFileServerSettings over a real scratch directory — self-checking demo
lake exe examples vault            # Data.Vault type-safe heterogeneous map: typed keys, adjust/delete/union — self-checking demo
lake exe examples vector           # Data.Vector-derived Array combinators: generate/ifilter/folds/reductions/backpermute/slice — self-checking demo
lake exe examples todo             # Web.Html/Web.Css typed TODO list over Network.WebApp.Server — self-checks, then keeps serving; try:  curl localhost:<port>
lake exe examples todo check       # same self-check round trip, but exits instead of staying up (for scripting)
```

The `echo` example exercises the whole socket stack end-to-end — a green accept
loop forks a green handler per connection, each suspending on
`recvGreen`/`sendAllGreen` (via the kqueue/epoll `EventDispatcher`) instead of
holding an OS thread, so one small worker pool serves many connections. Adding
an example is a new module under `Examples/` plus one line in the registry in
`Examples/Main.lean`.

The `quic` example demonstrates the HTTP/3-over-QUIC wire format end-to-end —
`Network.HTTP3.QPACK.Encode`/`Frame.encode` producing bytes that
`Frame.decode`/`Network.HTTP3.QPACK.Decode` reproduce exactly — without
needing a live connection, since `Network.QUIC.Client`/`Server` are stubbed
pending TLS 1.3 FFI. It also calls `Client.connect`/`Server.run`/`Server.accept`
directly and checks that each fails with exactly its documented
"not yet implemented" error, so the demo stays honest about what is and isn't
wired up yet.

The `stm` example puts ten green tasks through a thousand `atomically`
increments each of a shared `TVar`, hands values between a producer and
consumer through an empty `TMVar`, checks `TQueue`'s FIFO order survives its
two-list representation, and shows `orElse` falling through to its alternative
on `retry`.

The `streaming-commons` example drives `Data.Streaming.Network`'s `AppData`
abstraction over a real loopback connection; `streaming-commons serve <port>`
runs `runTCPServer` forever for manual testing with `nc`.

The `tls` example runs a full TLS 1.2/1.3 handshake over loopback against a
self-signed `CN=localhost` certificate, trusting it directly as its own CA via
`createClientContextWithCA` so the demo stays fully offline. It also documents
a real API limitation: `getAlpn` always reports `none`, because `setAlpn` only
registers the server's selection callback — nothing in the current client API
calls `SSL_set_alpn_protos` to advertise a protocol list for it to select from.

The `httpclient` and `httpconduit` examples each stand up a tiny hand-rolled
HTTP/1.1 server over a real loopback socket and drive it with a different
layer of the client stack: `httpclient` uses `Client.connectPlain` +
`Client.performRequest` directly, then `Client.execute` to show a `302 Found`
→ `/final` redirect followed automatically; `httpconduit` uses
`Simple.parseUrl!`/`httpBS`, the callback-scoped `Client.Conduit.withResponse`,
and `Client.Conduit.httpSource` streamed through a `.| sinkList` conduit
pipeline.

The `req` example exercises `Network.HTTP.Req`'s type-safe client — a `GET`
with `NoReqBody` and a `POST` with a `ReqBodyBs` payload, both against a
loopback server, both admitted by the `HttpBodyAllowed` typeclass at compile
time (swapping a body onto the `GET` would instead fail to compile, since
there is no `HttpBodyAllowed .NoBody .YesBody` instance).

The `webapp` example drives a `Network.WebApp.Application` through the same
kind of hand-rolled loopback HTTP/1.1 server as `httpclient`/`req`, but this
time the request handler itself is the thing under test: raw bytes are parsed
into a `Request`, run through the application via `Green.block`, and the
resulting `Response` serialized back. The demo application composes an echo
handler with a `/health` route and a `Server` header, entirely from
`Middleware` combinators — `composeMiddleware`, `ifRequest`,
`modifyResponse`, `addHeader` — the same combinators the algebraic-law
theorems in `Network.WebApp` (`idMiddleware_comp_left`/`_right`,
`modifyResponse_id`, `ifRequest_false`) prove associative/identity laws for.

The `webappstatic` example serves a real scratch directory through
`Network.WebApp.Static.static` (`defaultFileServerSettings` + `staticApp`),
reusing `webapp`'s loopback harness — including its `Sendfile.sendFile` path
for `.responseFile`, since `defaultFileServerSettings` serves files that way
rather than buffering them into a `.responseBuilder`. It checks a direct file
hit (with its `Cache-Control: max-age=3600` default), a directory request
redirected to `index.html`, a 404 for a missing path, and a 403 for a
dotfile-shaped path segment (rejected by `Piece`'s `no_dot` invariant before
any filesystem lookup runs).

The `vault` example mints distinctly-typed keys with `Key.new` and stores
unrelated payloads under each in the same `Vault`, showing that a key only
ever yields back the type it was minted for, plus `adjust`/`delete`/`union`.

The `vector` example runs through every combinator `Linen.Data.Vector` adds to
`Array` (`generate`, `ifilter`, `foldl1'`/`foldr1`, `ifoldl'`/`ifoldr`,
`and`/`or`/`product`, `notElem`, `backpermute`, `slice`) — everything else
Haskell's `Data.Vector` offers already exists verbatim on `Array`.

The `todo` example is a small in-memory TODO list whose every page is built
from `Web.Html`/`Web.Css` typed constructors — the `<ul>`/`<li>` nesting, each
item's `<form>`s, and its inline `style` all go through the same
illegal-construct-is-a-compile-error discipline as `Tests.Linen.Web.HtmlTest`/
`CssTest` (e.g. a `<div>` inside a `<p>`, or a `color` declaration given a
`Display` value, simply fails to compile). Routing and state reuse
`Network.WebApp`'s `Application`/`AppM`, driven by the real
`Network.WebApp.Server` engine via `withApplication`, exactly as the `server`
example drives `webapp`'s `demoApplication`. Unlike the other examples, `todo`
doesn't exit after checking itself — it self-checks against its own live
server and then keeps that same server (with its accumulated state) running
on the printed OS-assigned port, so you can immediately `curl` it by hand;
`todo check` runs the identical round trip but exits instead, for scripting.

### Running `postgrest` against a real database

`lake exe examples postgrest` (no args) and `... postgrest spec` are fully
self-contained — they run against a hand-built, in-memory `SchemaCache` and
need no external services. `postgrest live` instead connects to a real
PostgreSQL instance, introspects its `public` schema with the same catalog
queries (`SchemaCache.tablesSql`/`columnsSql`) PostgREST itself runs at
startup, and serves a couple of requests against the real tables.

Start a disposable local Postgres with Docker:

```bash
docker run --rm -e POSTGRES_PASSWORD=postgres -p 5432:5432 postgres
```

Then, in another terminal:

```bash
lake exe examples postgrest live
```

This connects with `host=localhost port=5432 user=postgres password=postgres
dbname=postgres` — matching the container above — prints every table found in
`public`, serves `GET /` and `GET /<first table>` through the same
`App.handleRequest` code path as the in-memory demo, and prints the live
schema's OpenAPI spec. Pass a different libpq connection string as the next
argument to point at another instance or database, e.g.:

```bash
lake exe examples postgrest live "host=localhost port=5432 user=postgres password=postgres dbname=mydb"
```

If nothing is listening, the example prints a short "could not connect" hint
(with this same `docker run` command) and exits 1, rather than crashing.

## Documentation

- [docs/MODULES.md](docs/MODULES.md) — the full module feature list and module table.
- [docs/imports/index.md](docs/imports/index.md) — Hackage-package import order, with a
  per-package module dependency list under `docs/imports/<Package>/dependencies.md`.
- [AGENTS.md](AGENTS.md) — conventions for contributing to the library.

## License

See [LICENSE](LICENSE) for details.
