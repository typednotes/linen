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
  <strong>20 modules</strong> · <strong>46 compile-time theorems</strong> · <strong>232 <code>#guard</code> checks</strong>
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

### `Data.Functor` — functor constructions missing from core

- `Compose F G`, `Product F G`, `FunctorSum F G` — composition, product and
  coproduct of functors, with `Functor`/`Applicative` instances and verified
  `map_id` / `map_comp` laws.
- `Const α` — the constant (phantom) functor, the building block for `foldMap`.
- `Contravariant` / `LawfulContravariant` — contravariant functors, with
  `Predicate` and `Equivalence` instances.

### `Control` — applicative & monad combinators missing from core

- `Control.Applicative.asum` — fold a list of alternatives with `<|>`.
- `Control.Monad.join`, `replicateM`, `replicateM_` — flatten and repeat
  monadic actions (with the `join_pure` law).
- `Control.Category` / `LawfulCategory` — categories with identity and
  associative composition (`≫`, diagrammatic), with the lawful `Fun` instance.
- `Control.AutoUpdate` — periodically refreshed cached values: a non-blocking
  getter backed by a dedicated OS thread and a `Std.CancellationToken` for clean
  shutdown.
- `Control.Concurrent.MVar` — a promise-based synchronisation variable (empty or
  full) with FIFO-fair waiters that are dormant tasks, not blocked OS threads.
- `Control.Concurrent.Chan` — an unbounded FIFO channel with `dup` (broadcast to
  independent readers); blocking reads are dormant promises, not blocked threads.
- `Control.Concurrent.QSem` — a quantity semaphore (`wait`/`signal`/`withSem`)
  with a `Nat` count that can't underflow and FIFO-fair, promise-based waiters.
- `Control.Concurrent.QSemN` — a generalised semaphore that acquires/releases
  arbitrary quantities (`wait n`/`signal n`/`withSemN`), greedily waking waiters.
- `Control.Concurrent.Green` — a fair green-thread monad: awaiting a `Task` frees
  the pool thread (via `BaseIO.bindTask`, never `IO.wait`), with cancellation,
  error handling, and `MVar`/`Chan`/`QSem` integration.
- `Control.Concurrent` — thread management built entirely on the `Green` model:
  `forkIO`, `forkFinally`, `forkGreen`, `killThread` (cooperative), `waitThread`,
  `threadDelay`, `yield`, and a monotonic `ThreadId`. All forks run as fair green
  threads started on Lean's task pool.

### `Data.Json` — a tiny JSON library

- `Value` AST with predicates, accessors and object field access.
- `ToJSON` / `FromJSON` typeclasses.
- `encode` / `encodePretty` and `decode` / `decodeAs`, with proven
  encode→decode **roundtrip theorems**.

### `System.Console.Ansi` — terminal styling

- `Color` / `Intensity` enums and the ANSI escape-code builders
  (`setFg`, `setBg`, `colored`, `bold`, …).

### `Network.Socket` — POSIX sockets & event multiplexing

- `Network.Socket.Types` — the type layer for a phantom-typed socket API:
  `Family` / `SocketType` / `ShutdownHow` enums with their FFI tag encodings, an
  `EventType` readiness bitmask (kqueue/epoll), `SockAddr` / `AddrInfo`, and a
  `Socket (state : SocketState)` handle whose POSIX lifecycle (`fresh → bound →
  listening`, `connecting → connected`, `closed`) is **enforced at compile time**
  (15 state-distinctness theorems; `close` carries a `state ≠ .closed` proof
  obligation that makes double-close a type error). Non-blocking operations
  return `Accept` / `Connect` / `Recv` / `Send` / `Poll` outcome sum types.
- `Network.Socket.FFI` — `@[extern]` bindings to a portable C shim
  (`ffi/network.c`): socket create / bind / listen / accept / connect, blocking
  and non-blocking send / recv, UDP `sendto` / `recvfrom`, socket options,
  `getAddrInfo`, a buffered `RecvBuffer`, and an event loop over **kqueue
  (macOS) / epoll (Linux)**. The shim is compiled and linked by `lakefile.lean`
  (`extern_lib linenffi`); the `Linen` library is `precompileModules`-enabled so
  the bindings are callable from `#eval`.
- `Network.Socket` — the safe, high-level API over the FFI: `socket → bind →
  listen → accept` (and `connect`/`connectFinish`, `send`/`recv`) with each
  transition's pre/post state in its signature, a `close` whose `state ≠ .closed`
  proof obligation makes double-close a type error, `withSocket` / `withListenTCP`
  / `withEventLoop` bracket helpers, `listenTCP`/`listenTCP6`, address
  introspection, and an `EventLoop` (kqueue/epoll) wrapper.
- `Network.Socket.EventDispatcher` — the bridge from socket readiness to the
  green-thread model: a **sharded** set of dispatch threads (fds partitioned by
  `fd % N`, each shard its own kqueue/epoll loop + waiter map) resolves an
  `IO.Promise` when a socket is ready, so `waitReadable` / `waitWritable` (and
  `recvGreen` / `sendAllGreen`) **suspend a `Green` thread as a heap object
  instead of holding an OS thread**. This is what lets one worker pool serve many
  thousands of IO-bound connections.

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

| Module | Description |
|---|---|
| `Linen.Data.Functor` | `Compose`, `Const`, `Product`, `FunctorSum`, `Contravariant` |
| `Linen.Control.Applicative` | `asum` |
| `Linen.Control.Monad` | `join`, `replicateM`, `replicateM_` |
| `Linen.Control.Category` | `Category`, `LawfulCategory`, `Fun`, the `≫` operator |
| `Linen.Control.AutoUpdate` | periodically cached values on a dedicated thread |
| `Linen.Control.Concurrent.MVar` | promise-based synchronisation variable (`take`/`put`/`swap`/…) |
| `Linen.Control.Concurrent.Chan` | unbounded FIFO channel with `dup` (`write`/`read`/`tryRead`) |
| `Linen.Control.Concurrent.QSem` | quantity semaphore (`wait`/`signal`/`withSem`) |
| `Linen.Control.Concurrent.QSemN` | generalised semaphore over arbitrary quantities |
| `Linen.Control.Concurrent.Green` | fair green-thread monad (non-blocking `await`, cancellation) |
| `Linen.Control.Concurrent` | thread management (`forkIO`/`forkFinally`/`forkGreen`/`killThread`/`waitThread`) |
| `Linen.Data.Json` | JSON AST, `ToJSON`/`FromJSON`, encode/decode + roundtrip proofs |
| `Linen.System.Console.Ansi` | ANSI terminal colors and styles |
| `Linen.Network.Socket.Types` | phantom-typed `Socket` lifecycle, `Family`/`SockAddr`/`EventType`, non-blocking outcome types |
| `Linen.Network.Socket.FFI` | `@[extern]` C bindings: sockets, options, UDP, `getAddrInfo`, kqueue/epoll event loop |
| `Linen.Network.Socket` | safe phantom-typed lifecycle API, `withSocket`/`listenTCP`/`withEventLoop`, `EventLoop` |
| `Linen.Network.Socket.EventDispatcher` | kqueue/epoll → `Green` bridge: `waitReadable`/`waitWritable`/`recvGreen`/`sendAllGreen` |

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
```

The `echo` example exercises the whole socket stack end-to-end — a green accept
loop forks a green handler per connection, each suspending on
`recvGreen`/`sendAllGreen` (via the kqueue/epoll `EventDispatcher`) instead of
holding an OS thread, so one small worker pool serves many connections. Adding
an example is a new module under `Examples/` plus one line in the registry in
`Examples/Main.lean`.

## Documentation

- [docs/module-dependencies.md](docs/module-dependencies.md) — module dependency
  graph and topological build order.
- [AGENTS.md](AGENTS.md) — conventions for contributing to the library.

## License

See [LICENSE](LICENSE) for details.
