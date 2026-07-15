# `hedis` module dependencies

Topological order of every module of the
[`hedis`](https://hackage.haskell.org/package/hedis) package (v0.16.1,
source: https://hackage-content.haskell.org/package/hedis-0.16.1/src/hedis.cabal
— the real `exposed-modules`/`build-depends` fields were fetched and read
verbatim, not recalled from memory) planned for import into `linen`, per
[AGENTS.md](../../AGENTS.md)'s Hackage-import convention.

**Status: planned, not yet ported.** This document is the dependency plan
only — no `Linen/`/`Tests/` code has been written yet.

An edge **A → B** means *module A imports module B*, so **B must be built
before A**. Internal edges below were confirmed by fetching each module's
own import list from the `hedis-0.16.1` source tree (`Cluster.hs`,
`Cluster/HashSlot.hs`, `Connection.hs`, `Core.hs`, `Protocol.hs`, `PubSub.hs`,
`Sentinel.hs`, `Types.hs`, `URL.hs`) rather than guessed from the module
names alone; the handful not individually fetched (`Cluster/Command.hs`,
`Commands.hs`, `ManualCommands.hs`, `Transactions.hs`, `Hooks.hs`,
`ConnectionContext.hs`, the `Database.Redis` facade) are placed by the same
layering their (fetched) callers' import lists imply — each is only ever
imported *after* the layer below it in every caller checked.

## Headline finding: no blocking not-yet-ported prerequisite

Unlike `lens` (which needed `profunctors`/`indexed-traversable` planned
first), **`hedis` needs no separate Hackage-import package planned before
it.** Every one of its 18 `build-depends` entries resolves against either
the Lean stdlib, an already-ported `linen` module, or a narrow enough slice
that inlining it directly (the same treatment `hoauth2`'s and `lens`'s own
`dependencies.md` give `microlens`/`assoc`/`call-stack`/`these`) is more
faithful than a whole fresh package import for 1–6 functions. See "External
dependencies" below for the full resolution, function-by-function for the
narrow cases — each was checked by fetching the actual `hedis` source file
that uses it, not assumed.

`hedis`'s wire-protocol parsing (RESP2; `hedis` does not implement RESP3)
is the one genuinely new port here, and it is architecturally the same
shape as this codebase's existing `Linen/Network/HTTP2/Frame/*.lean` and
`Linen/Network/HTTP3/Frame.lean` — a length/type-prefixed frame parser
layered over an existing socket abstraction, not a new socket layer. It
sits at `Linen.Database.Redis.Protocol`, parsing bytes read via the
already-ported `Linen.Network.Socket`/`Linen.Network.TLS`.

## Namespace decision

Kept as upstream's own `Database.Redis.*` hierarchy, re-rooted as
`Linen.Database.Redis.*` — this mirrors the existing sibling database
clients in this codebase (`Linen.Database.PostgreSQL.*`,
`Linen.Database.SQLite.*`, `Linen.Database.DuckDB.*`), all of which are
`Database.<Engine>.*`-shaped rather than reinvented namespaces, and
`Redis`/`Cluster`/`Sentinel`/`PubSub`/`Protocol`/`Transactions` are Redis's
own domain vocabulary, not Haskell/GHC branding — so AGENTS.md's Lean-ify
rule does not require renaming them (the same reasoning `lens`'s
`dependencies.md` gives for keeping `Lens`/`Prism`/`Iso` as-is; contrast
`WaiAppStatic`→`WebApp.Static`, which *is* a Haskell-package-name rename).

## External (non-`hedis`) dependencies

Resolved against `hedis-0.16.1.cabal`'s library `build-depends`, in
Hackage-import precedence order (Lean stdlib > existing `linen` Haskell
port > new source):

Already ported, reused as-is:

- `base`, `bytestring`, `containers`, `mtl`, `text`, `time`, `vector` →
  `Base`, `ByteString`, `Containers`, `Mtl`, `Text`, `Time`, `Vector`.
- `network` → `Network` (already backs `Linen.Network.Socket`).
- `network-uri` → `network-uri` (#57 in the top-level index; already backs
  `Linen.Network.URI`).
- `http-types` → `HttpTypes` (`Database.Redis.URL` needs exactly one
  function from it, `parseSimpleQuery`, for parsing a `redis-socket://…`
  URL's query string — already covered).
- `stm` → `STM` (`TVar`/`TQueue`, used by `Database.Redis.PubSub`'s
  listener-thread coordination — see the `async` substitution note below,
  which this pairs with in upstream too).
- `tls` → `TLS` (backs `Database.Redis.ConnectionContext`'s and
  `.URL`'s `defaultParamsClient`/`ClientParams` usage for `rediss://`
  connections).
- `hashable` → Lean's own `Hashable` class (stdlib).
- `unordered-containers` → `Std.HashMap`/`Std.HashSet` (stdlib).
- `unliftio-core` → `UnliftIO` (`Database.Redis.PubSub`'s
  `MonadUnliftIO`/`withRunInIO` usage).

Substituted with directly-inlined code rather than a separate package
import (each checked against the one `hedis` source file that actually uses
it — narrow usage, the same treatment `hoauth2`'s `dependencies.md` gives
`microlens`/`binary` and `lens`'s gives `assoc`/`call-stack`/`these`):

- **`scanner`** (`Data.Scanner`) — `Database.Redis.Protocol` (the RESP
  parser) uses exactly four combinators from it: `anyChar8` (dispatch on
  the reply-type prefix byte), `takeWhileChar8` (consume up to `\r`),
  `char8` (match `\r`/`\n`), `take` (read a bulk string's declared byte
  length). This is precisely the shape of incremental byte-parser the
  `hip`/`JuicyPixels`/`netpbm` entries already resolved onto Lean stdlib's
  `Std.Internal.Parsec`/`Std.Internal.Parsec.ByteArray` instead of
  `attoparsec`; the RESP parser is built the same way, directly on
  `Std.Internal.Parsec`, with no separate `scanner` port.
- **`bytestring-lexing`** (`Data.ByteString.Lex.Integral`/`.Fractional`) —
  `Database.Redis.Types`'s `RedisResult Integer`/`Int64`/`Double` `decode`
  instances use exactly `readSigned`/`readDecimal` (integer) and
  `readSigned`/`readExponential` (double) to parse a bulk/single-line reply
  payload into a number. Two small hand-written ByteString-to-number
  parsers (signed-decimal, signed-exponential) inlined in
  `Linen.Database.Redis.Types` substitute for the whole package.
- **`errors`** (`Control.Error.Util`) — `Database.Redis.URL` uses exactly
  one function, `note :: e -> Maybe a -> Either e a`. Written inline as a
  one-line local helper in `Linen.Database.Redis.URL` rather than a package
  import.
- **`exceptions`** (`Control.Monad.Catch`) — `Database.Redis.Sentinel`
  (`Handler`, `MonadCatch`, `catches`, `throwM`, `bracket`) and
  `Database.Redis.Connection` (`Catch.catches`, `bracket`) use it for
  GHC-generic multi-exception-type catching. Per the same precedence-rule
  application as `hoauth2`'s and `lens`'s own `dependencies.md` notes on
  this package: ported directly against `Linen.Control.Exception`'s
  existing `IO`/`Except`-based exception type instead of a generic
  `exceptions`-style `MonadThrow`/`MonadCatch` port.
- **`async`** (`Control.Concurrent.Async`) — used in exactly one place,
  `Database.Redis.PubSub`'s `pubSubForeverOnConn`/`withPubSubOnConn`, via
  four combinators (`withAsync`, `waitEitherCatch`, `waitEitherCatchSTM`,
  `concurrently`) to race a listener thread against a sender/callback
  thread and propagate whichever fails first. (Confirmed *not* used
  anywhere else in the library — `Cluster.hs` and `Core.hs`, both checked
  directly, rely only on `MVar`/`IORef`/`unsafeInterleaveIO` for their own
  concurrency, and upstream's `build-depends` on `async` is for this one
  module plus its benchmark/test suites.) Lean has native `Task`-based
  concurrency (`IO.asTask`, `Task.get`, `IO.waitAny`) that this codebase
  already treats as `async`'s substitute in spirit (no separate `async`
  package exists in the index); `withAsync`/`waitEitherCatch`/
  `waitEitherCatchSTM`/`concurrently`'s four call sites are re-expressed
  directly over `IO.asTask`/`Task.get`/`STM`'s already-ported `TVar` in
  `Linen.Database.Redis.PubSub`, rather than importing a package whose
  only other capabilities (thread pools, `mapConcurrently`, `race`, …)
  `hedis` never touches.
- **`resource-pool`** (`Data.Pool`) — `Database.Redis.Connection` uses
  `defaultPoolConfig`/`setNumStripes`/`setPoolLabel`/`newPool`/
  `withResource`/`tryWithResource`/`destroyAllResources` to pool live
  connections. `linen` already has exactly this shape of thing,
  `Linen.Database.SQL.Pool` (an `IO.Ref`-guarded array of resources,
  created on demand up to a configured maximum, recycled after use) — but
  that module is written concretely against `Database.SQL.Connection`
  (PostgreSQL), not generically over an arbitrary resource type, so it
  cannot be imported as-is. Per the precedence rule (an existing `linen`
  *pattern* outranks a fresh package import even when the existing module
  isn't literally reusable), `Linen.Database.Redis.Connection` gets its own
  small pool following `Database.SQL.Pool`'s exact same design (an
  `IO.Ref`-guarded `Array`/count of `Linen.Database.Redis.ProtocolPipelining`
  connections, `maxSize`/`idleTimeout` settings, `withResource`-shaped
  acquire/release) rather than importing `resource-pool` — no separate
  `dependencies.md` needed for it, the same way `duckdb-simple`'s entry
  doesn't re-import `sqlite-simple`'s already-covered pattern.
- **`HTTP`** (`Network.HTTP.Base`) — `Database.Redis.URL` imports this
  *unqualified* but, checked directly against the file body, calls only
  `parseURIAuthority`/`uriToAuthorityString` and three record accessors
  (`host`/`user`/`password`/`port`) on the authority type it returns — i.e.
  it uses this large legacy HTTP-client package purely as a
  `"user:pass@host:port"` authority-string splitter for `redis://`/
  `rediss://` URLs, nothing HTTP-protocol-specific at all. A small
  hand-written authority parser (split on `@`, then `:` twice) inlined in
  `Linen.Database.Redis.URL` substitutes for the whole package; `network-uri`
  (already ported) still supplies the surrounding `parseURI`/`uriPath`/
  `uriScheme`/`uriQuery` that URL.hs also uses.

Dropped outright (GHC-version-compatibility shims, no Lean analogue — same
category as `base-orphans` in the `lens` entry):

- **`semigroups`** — a conditional dependency only for GHC < 8.0
  (`if(impl(ghc <8.0))`); `linen` targets one pinned Lean toolchain, this
  branch of the cabal file is dead code for the port.
- **`deepseq`** — controls GHC's laziness (`NFData`/`rnf` used on a couple
  of record types for benchmark forcing), which Lean (eager by default)
  has no equivalent notion of; the same "genuinely out of scope, not a
  simplification of in-scope behavior" category the `hip` entry gives this
  same package.

## Topologically sorted `hedis` modules (genuinely new port)

All 18 upstream `exposed-modules` (there are no `other-modules` in the
library component); none are dropped, folded, or deferred — RESP2 framing,
cluster/sentinel topology, transactions, and pub/sub are all in scope.

1. `Database.Redis.Cluster.HashSlot` → `Linen.Database.Redis.Cluster.HashSlot`
   — CRC16-based key→slot hashing for cluster mode. No internal deps
   (`Data.Bits`, `ByteString` only — confirmed via direct fetch).
2. `Database.Redis.ConnectionContext` → `Linen.Database.Redis.ConnectionContext`
   — the raw connection handle (plain TCP, Unix socket, or TLS) built
   directly on `Linen.Network.Socket` and `Linen.Network.TLS`. No internal
   `Database.Redis.*` deps.
3. `Database.Redis.Protocol` → `Linen.Database.Redis.Protocol` — the RESP2
   wire-format encoder/decoder (`Reply`, `renderRequest`, `reply`); the
   frame-parsing core, built on `Std.Internal.Parsec` per the `scanner`
   substitution note above. No internal deps (confirmed via direct fetch:
   only base/`Scanner` imports).
4. `Database.Redis.Hooks` → `Linen.Database.Redis.Hooks` — the
   before/after-request instrumentation-hook type threaded through `Core`.
   No internal deps (a small standalone type).
5. `Database.Redis.Types` → `Linen.Database.Redis.Types` — `RedisResult`
   class and instances decoding a `Reply` into each Redis reply shape
   (`Integer`, `Double`, `ByteString`, `[a]`, …); folds in the
   `bytestring-lexing` substitution above. Depends on #3.
6. `Database.Redis.Cluster.Command` → `Linen.Database.Redis.Cluster.Command`
   — parses a `CLUSTER COMMAND` reply into per-command routing metadata
   (key positions, flags) used to route cluster requests. Depends on #3, #5
   (placed here: every fetched caller of it — `Cluster.hs` — only ever
   imports it after `Protocol`/`Types` are available).
7. `Database.Redis.ProtocolPipelining` → `Linen.Database.Redis.ProtocolPipelining`
   — wraps a `ConnectionContext` with a request/response pipeline queue
   (send many requests before reading their replies) over `Protocol`.
   Depends on #2, #3.
8. `Database.Redis.Cluster` → `Linen.Database.Redis.Cluster` — cluster
   topology (`ShardMap`, `Node`, `Shard`), slot→node routing, and
   redirect (`MOVED`/`ASK`) handling. Depends on #1, #2, #3, #4, #6
   (confirmed via direct fetch: imports `HashSlot`, `ConnectionContext`,
   `Protocol`, `Cluster.Command`, `Hooks`).
9. `Database.Redis.Core.Internal` → `Linen.Database.Redis.Core.Internal` —
   the `RedisEnv`/low-level `Redis` monad representation (a `ReaderT`-ish
   wrapper holding a pipelined connection or cluster shard map plus hooks).
   Depends on #3, #4, #5, #7, #8.
10. `Database.Redis.Core` → `Linen.Database.Redis.Core` — the public
    `Redis` monad (`runRedisInternal`, `runRedisClusteredInternal`,
    `defaultHooks`), request dispatch through either a single pipelined
    connection or a cluster shard map. Depends on #3, #4, #5, #7, #8, #9
    (confirmed via direct fetch).
11. `Database.Redis.Commands` → `Linen.Database.Redis.Commands` — the full
    generated-by-hand Redis command surface (`GET`/`SET`/`LPUSH`/…, by far
    the largest module upstream). Depends on #3, #5, #10.
12. `Database.Redis.ManualCommands` → `Linen.Database.Redis.ManualCommands`
    — commands whose reply shape or argument encoding is irregular enough
    that upstream hand-writes them separately from #11's uniform generation
    pattern (e.g. `OBJECT`, `SORT`, `ZADD` with flags). Depends on #3, #5,
    #10, #11.
13. `Database.Redis.Connection` → `Linen.Database.Redis.Connection` —
    `ConnectInfo`, `connect`/`connectCluster`, and the connection pool
    (the `Database.SQL.Pool`-patterned replacement for `resource-pool`, see
    the substitution note above). Depends on #2, #3, #7, #8, #10, #11
    (confirmed via direct fetch: imports `ProtocolPipelining`, `Core`,
    `Protocol`, `Cluster`, `ConnectionContext`, `Commands`).
14. `Database.Redis.Transactions` → `Linen.Database.Redis.Transactions` —
    `MULTI`/`EXEC`/`DISCARD`/`WATCH` transaction support (`multiExec`,
    `RedisTx`). Depends on #3, #5, #10, #11.
15. `Database.Redis.PubSub` → `Linen.Database.Redis.PubSub` — `SUBSCRIBE`/
    `PUBLISH` support, including the two-thread listener/callback race;
    folds in the `async`/`stm`/`unliftio-core` substitutions above. Depends
    on #3, #5, #7, #8, #10, #13 (confirmed via direct fetch: imports
    `ProtocolPipelining`, `Cluster`, `Core`, `Connection`, `Protocol`,
    `Types`).
16. `Database.Redis` → `Linen.Database.Redis` — the top-level facade,
    re-exporting #10–#15 plus #2's `ConnectionContext` and #3's `Reply`.
    Depends on all of the above.
17. `Database.Redis.URL` → `Linen.Database.Redis.URL` — `redis://`/
    `rediss://`/`redis-socket://` connection-string parsing into
    `ConnectInfo`; folds in the `errors`/`HTTP` substitutions above.
    Depends on #2, #13 (confirmed via direct fetch: imports `Connection`,
    `ConnectionContext`; independent of #16).
18. `Database.Redis.Sentinel` → `Linen.Database.Redis.Sentinel` — Redis
    Sentinel-aware connection wrapper (`connectRedis`, master-node
    discovery via `SENTINEL get-master-addr-by-name`), built *on top of*
    the public facade rather than the internals (confirmed via direct
    fetch: `import Database.Redis hiding (Connection, connect, runRedis)`).
    Depends on #16. Last in the order — nothing else depends on it.

**Total: 18 modules, all genuinely new port** (RESP2 protocol framing,
connection/pooling, cluster routing, transactions, pub/sub, sentinel, and
URL parsing) — none folded, dropped, or deferred; the plan carries zero
GHC/TH-specific modules to drop (`hedis` uses no Template Haskell and no
GHC-`Generic`-derived instances in its exposed surface) and zero deferred
modules (every container/typeclass it touches is already covered by the
Lean stdlib or an existing `linen` port per the substitution table above).

## Scope note: RESP3 is out of scope, matching upstream

`hedis` 0.16.1 itself only implements the RESP2 wire protocol (`HELLO`/
RESP3's map/set/push/double/big-number/boolean reply types are not in its
`Protocol.hs`); this plan ports exactly what upstream has, so RESP3 support
is not something this import defers or drops — it was never upstream's
scope to begin with.
