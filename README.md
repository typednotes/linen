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
  <strong>37 modules</strong> · <strong>91 compile-time theorems</strong> · <strong>480 <code>#guard</code> checks</strong>
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
- `Data.Bifunctor` / `LawfulBifunctor` — map over *both* type parameters
  (`bimap`, `mapFst`, `mapSnd`), with `Prod` / `Sum` / `Except` instances and
  verified identity & composition laws.
- `Data.Bits` / `FiniteBits` — a Haskell-style bitwise typeclass over
  `UInt8/16/32/64`: `and`/`or`/`xor`/`complement`/shifts, plus `testBit`, `bit`,
  `popCount`, `setBit`/`clearBit`/`complementBit`, and width-bounded
  `countLeadingZeros` / `countTrailingZeros` (carrying `≤ finiteBitSize` proofs).
- `Data.Bool.guard'` — the list-valued guard (`[x]` / `[]`) that core lacks
  (`Data.Bool.bool` is already Lean core's `bool`, so it isn't re-ported).
- `Data.Char'` — the Haskell `Data.Char` predicates core lacks (`isAscii`,
  `isLatin1`, `isControl`, `isPrint`, `isOctDigit`, `isAsciiUpper`/`Lower`,
  `isPunctuation`) plus `digitToInt` (proof-carrying `{n // n < 16}`) and
  `intToDigit`, with a verified hex roundtrip.
- `Data.Complex α` — complex numbers over any numeric type: `Add`/`Sub`/`Mul`/`Neg`
  instances, `conjugate`, `magnitudeSquared`, with `conjugate`-involution and
  addition-commutativity proofs.
- `Data.Fixed` — fixed-point decimals with **type-level precision** (`Fixed 2` ≠
  `Fixed 4`): exact `Add`/`Sub`/`Neg`, rescaling `Mul`, `ToString`, and exact
  `toRat`, with `add_exact`/`sub_exact`/`neg_neg` proofs.
- `Data.Function.on` / `applyTo` — the two `Data.Function` combinators core lacks
  (`flip`/`const` already exist); `applyTo` is the function form of the `|>` pipe.
- `Data.Ix` — an index typeclass (Haskell `Data.Ix`): `range`, `rangeSize`,
  `inRange`, and a proof-carrying `index` (`{n // n < rangeSize bounds}`), with
  `Nat`/`Int`/`Char`/`Bool`/product instances.
- `Data.List.NonEmpty` — a non-empty list (`head`/`tail`) with total `head`/`last`,
  `length : {n // n ≥ 1}`, folds (`foldr1`/`foldl1`), and `Functor`/`Monad`
  instances; length-preservation proofs for `reverse`/`map`.
- `Data.List'` — the `Data.List` operations core lacks: `transpose` (structural,
  no fuel), `tails`/`inits` (as `NonEmpty`), `subsequences`, `permutations`,
  `mapAccumL`/`mapAccumR`, `sortOn`, `maximumBy`/`minimumBy`, `unionBy`/
  `intersectBy`, `insertBy`.
- `Data.Foldable` — a `Foldable` typeclass (`foldr`/`foldl`/`toList`) with derived
  `foldMap`/`null`/`length`/`any`/`all`/`find?`/`elem`/`sum`/`product`/`minimum?`/
  `maximum?` and total `minimum1`/`maximum1`; instances for `List`/`Option`/`NonEmpty`/`Sum`.
- `Data.Newtype` — the Haskell monoid/semigroup wrappers `Dual`, `Endo`, `First`,
  `Last`, `Sum`, `Product`, `All`, `Any`, each with an `Append` instance and a
  verified associativity law.
- `Data.Ord` — `Down` (reversed `Ord`/`BEq`, for descending sorts) and a
  proof-carrying `clamp` returning `{y // lo ≤ y ∧ y ≤ hi}` (`comparing` is core's
  `compareOn`).
- `Data.Proxy` — a phantom-type proxy (no runtime data) with `Functor`/`Monad`
  instances and verified functor/monad laws.
- `Data.Rat.round` — round-half-away-from-zero for core `Rat` (Haskell `Data.Ratio`
  is core's `Rat`, which already has the arithmetic, `floor`/`ceil`/`abs`).

### `Control` — applicative & monad combinators missing from core

- `Control.Applicative.asum` — fold a list of alternatives with `<|>`.
- `Control.Monad.join`, `replicateM`, `replicateM_`, `when`, `unless` — flatten,
  repeat, and conditionally run
  monadic actions (with the `join_pure` law).
- `Control.Category` / `LawfulCategory` — categories with identity and
  associative composition (`≫`, diagrammatic), with the lawful `Fun` instance.
- `Control.Arrow` / `ArrowChoice` — arrows over a `Category`: `arr`, `first`,
  `second`, `split`, and (over `Sum`) `left`, `right`, `fanin`, with `Fun`
  instances.
- `Control.Exception.bracket` / `onException` — the IO resource/cleanup patterns
  core lacks as functions (`try`/`catch`/`finally` map to `IO.toBaseIO`/`tryCatch`/
  `tryFinally`), built on `tryFinally` / `tryCatch`.
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
| `Linen.Data.Bifunctor` | `Bifunctor`/`LawfulBifunctor`, `bimap`, `Prod`/`Sum`/`Except` instances |
| `Linen.Data.Bits` | `Bits`/`FiniteBits` over `UInt8/16/32/64`: `popCount`, `testBit`, `setBit`, bounded clz/ctz |
| `Linen.Data.Bool` | `guard'` (list-valued guard; `bool` is already in Lean core) |
| `Linen.Data.Char` | `Data.Char'` predicates (`isAscii`/`isControl`/…) + `digitToInt`/`intToDigit` |
| `Linen.Data.Complex` | `Complex α` over any numeric type: arithmetic, `conjugate`, `magnitudeSquared` |
| `Linen.Data.Fixed` | `Fixed p` fixed-point decimals with type-level precision: exact `+`/`-`, rescaling `*`, `toRat` |
| `Linen.Data.Function` | `on`, `applyTo` (the `Data.Function` combinators core lacks) |
| `Linen.Data.Ix` | `Ix` index class: `range`/`rangeSize`/`inRange` + proof-carrying `index`, `Nat`/`Int`/`Char`/`Bool`/product |
| `Linen.Data.List` | `Data.List'` extras: `transpose`, `tails`/`inits`, `permutations`, `sortOn`, `maximumBy`, `unionBy`, … |
| `Linen.Data.List.NonEmpty` | non-empty list: total `head`/`last`, `length ≥ 1`, `foldr1`/`foldl1`, `Functor`/`Monad` |
| `Linen.Data.Foldable` | `Foldable` class + derived `sum`/`any`/`find?`/`minimum?`/…; `List`/`Option`/`NonEmpty`/`Sum` |
| `Linen.Data.Newtype` | monoid wrappers `Dual`/`Endo`/`First`/`Last`/`Sum`/`Product`/`All`/`Any` (+ assoc laws) |
| `Linen.Data.Ord` | `Down` (reversed ordering) + proof-carrying `clamp` |
| `Linen.Data.Proxy` | phantom-type proxy with `Functor`/`Monad` + verified laws |
| `Linen.Data.Rat` | `Rat.round` (round-half-away-from-zero; `Data.Ratio` is core's `Rat`) |
| `Linen.Control.Applicative` | `asum` |
| `Linen.Control.Monad` | `join`, `replicateM`, `replicateM_`, `when`, `unless` |
| `Linen.Control.Category` | `Category`, `LawfulCategory`, `Fun`, the `≫` operator |
| `Linen.Control.Arrow` | `Arrow`/`ArrowChoice`: `arr`/`first`/`split`/`left`/`right`/`fanin`, `Fun` instances |
| `Linen.Control.Exception` | IO `bracket` / `onException` (resource safety & failure cleanup) |
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
