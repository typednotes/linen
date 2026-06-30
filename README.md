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
  <strong>94 modules</strong> · <strong>184 compile-time theorems</strong> · <strong>1739 <code>#guard</code> checks</strong>
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
- `Data.Base64` — RFC 4648 Base64 `encode`/`decode` over core `ByteArray`,
  written as structural recursion (no `partial`, no `while`); the alphabet is
  computed arithmetically and roundtrips are exercised in the tests.
- `Data.Bifunctor` / `LawfulBifunctor` — map over *both* type parameters
  (`bimap`, `mapFst`, `mapSnd`), with `Prod` / `Sum` / `Except` instances and
  verified identity & composition laws.
- `Data.ByteString` — a slice over core `ByteArray` (`data`/`off`/`len` with a
  proof `off + len ≤ data.size`) giving **O(1) `take`/`drop`/`splitAt`**; a broad
  Haskell-`Data.ByteString` API (pack/unpack, folds, scans, search,
  group/inits/tails, prefix/suffix/infix, file & handle I/O) plus `BEq`/`Ord`/
  `Hashable`. All loops are structural (no `partial`/fuel).
- `Data.ByteString.Char8` — a Latin-1 `Char` view of `ByteString` (`String ↔
  ByteString`, char-wise `map`/`filter`/`fold`/search), with `lines`/`words`/
  `unlines`/`unwords` as structural recursions over the byte list.
- `Data.ByteString.Lazy` — chunked lazy byte strings: non-empty strict chunks
  with a `Thunk`-deferred tail (structural recursion through `Thunk`), with
  `fromChunks`/`toStrict`, O(1) lazy `append`, chunk-spanning `take`/`drop`,
  folds, and content-based `BEq`/`Ord`/`Hashable`.
- `Data.ByteString.Lazy.Char8` — a Latin-1 `Char` view of `LazyByteString`
  (`String ↔ LazyByteString`, char-wise `map`/`filter`/`fold`/`elem`).
- `Data.ByteString.Short` — `ShortByteString`, a thin `ByteArray` newtype with
  `pack`/`unpack`/`index` and `toShort`/`fromShort` conversions to the strict
  slice, with a verified `length_toShort`.
- `Data.ByteString.Builder` — a difference-list (`LazyByteString → LazyByteString`)
  builder with O(1) `append`: byte/word (BE/LE)/UTF-8/decimal/hex encoders,
  `toLazyByteString`/`toStrictByteString`, and verified monoid laws.
- `Data.CaseInsensitive` — a `FoldCase` class and a proof-carrying `CI α` wrapper
  whose `BEq`/`Ord`/`Hashable` compare a folded copy (case-insensitively) while
  `ToString`/`Repr` keep the original; `String`/`Char` instances.
- `Data.Conduit.Internal.Pipe` — conduit's core streaming `Pipe` type, ported
  **without `unsafe`**: a Freer-style `pipeM` (strictly positive) and a strict
  spine make it a total, kernel-checked `Functor`/`Monad` for any effect `m`.
- `Data.Configurator.Types` — a typed config `Value` (string/number/bool/list)
  with a structural (no-`partial`) `toString`, and `Config = HashMap String Value`.
- `Data.Configurator` — a `key = value` config loader/parser (comments, dotted
  keys, quoted strings + escapes, numbers, booleans) with `lookup`/`require`/
  `load`; parsers are structural recursions (no `Id.run`/`while`).
- `Data.Default` — the `Default` typeclass (Haskell's `Data.Default`): sensible
  default values (`false`/`0`/`""`/`[]`/`none`/…), distinct from `Inhabited`.
- `Data.IntMap` — Haskell's `Data.IntMap` API (`union`/`unionWith`/`intersection`/
  `difference`/`adjust`/`toAscList`/`lookupMin`/`Max`/`isSubmapOf`/…) over
  `Std.HashMap Nat v`.
- `Data.Map` — Haskell's ordered `Data.Map k v` over `Lean.RBMap` ($O(\log n)$):
  the same combinator surface as `IntMap`, plus `mapKeys`, ascending
  `toList`/`keys`/`elems`, and verified empty-map laws.
- `Data.Set` — Haskell's ordered `Data.Set` (`Set'`) over `Lean.RBMap _ Unit`:
  `member`/`insert`/`union`/`intersection`/`difference`/`isSubsetOf`/`mapSet`/
  folds/`findMin`/`Max`, ascending dedup `toList'`, and empty-set laws.
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
- `Data.String` — Haskell's `Data.String`: the `IsString` class for overloaded
  literals, plus `String.words`/`unwords`/`unlines` (`lines` is core's
  `splitOn "\n"`).
- `Data.Traversable` — a `Traversable` typeclass (`traverse`/`sequence`) over
  core `Functor`/`Applicative`, with `List`/`Option`/`NonEmpty` instances, a
  `LawfulTraversable` law class, and the verified `traverse pure = pure` law for
  `Option` (Haskell's `Identity` is core's `Id`).
- `Data.Unique` — globally unique identifiers (Haskell's `Data.Unique`):
  `newUnique : IO Unique` hands out distinct, strictly increasing values from a
  process-global `IO.Ref` counter, with `BEq`/`Ord`/`Hashable`/`hashUnique`.
- `Data.Void` — the uninhabited type (Haskell's `Void` is core's `Empty`,
  `absurd` is `Empty.elim`): adds the vacuous `BEq`/`Ord`/`Hashable`/`ToString`
  and `Inhabited (Empty → α)` instances plus the `Empty → α` singleton law.

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

### `System.Exit` — process termination

- `ExitCode` (`success` | `failure n`) with `toUInt32`/`isSuccess`/`ToString`
  and the verified `isSuccess_iff` law, plus `exitWith`/`exitSuccess`/
  `exitFailure` wrapping core `IO.Process.exit`.

### `System.Log.FastLogger` — buffered logging

- `System.Log.FastLogger` — a thread-safe buffered logger (`Std.Mutex`-protected
  buffer, auto-flush on full / on close) to stdout/stderr/file/callback:
  `newLoggerSet`/`pushLogStr`/`flushLogStr`/`withFastLogger`.

### `Network.HTTP` — HTTP wire framing

- `Network.HTTP.Chunked` — HTTP/1.1 chunked transfer encoding over `ByteArray`:
  `chunkedTransferEncoding` / `chunkedTransferTerminator` / `encodeChunked`, with
  the hex chunk length via core `Nat.toDigits`.

### `Network.HTTP2` — HTTP/2 framing (RFC 9113)

- `Network.HTTP2.Frame.Types` — core framing types: a `StreamId` carrying an
  (erased) 31-bit proof, the `FrameType`/`ErrorCode`/`SettingsKeyId` closed
  inductives with total `UInt8`/`UInt16`/`UInt32` conversions (provably inverse
  for defined values), `FrameFlags` bit ops, `FrameHeader`/`Frame`, and a
  `Settings` record whose fields carry RFC value-range proofs.
- `Network.HTTP2.Frame.Decode` — wire-format parsing: big-endian integers,
  `decodeFrameHeader`, SETTINGS (`decodeSettingsPayload` via fuel-free
  `List.mapM`, `applySettings` with proof-carrying updates), GOAWAY /
  WINDOW_UPDATE / RST_STREAM / PRIORITY / padding, and `validateFrameSize`.
- `Network.HTTP2.Frame.Encode` — wire-format serialisation: big-endian
  integers, `encodeFrameHeader`/`encodeFrame`, frame builders
  (SETTINGS/PING/GOAWAY/WINDOW_UPDATE/RST_STREAM/HEADERS/DATA/CONTINUATION),
  `encodePriority`/`encodePadding`, and `splitHeaderBlock` (fuel-free chunking).
- `Network.HTTP2.HPACK.Huffman` — a complete HPACK (RFC 7541 Appendix B)
  Huffman codec: the fixed 257-entry code table, `huffmanEncode` (MSB-first
  bit packing with EOS-`1`s padding) and `huffmanDecode` (prefix-trie walk with
  padding validation), verified against the RFC's published test vectors. Total
  (structural fold over the bit list — no `partial`/fuel).
- `Network.HTTP2.HPACK.Table` — the HPACK header tables: the 61-entry RFC 7541
  Appendix A static table, and a `DynamicTable` FIFO with size-based eviction
  (entry size `|name|+|value|+32`, fuel-free), plus `find`/`indexLookup`/
  `findInTables` over the combined static + dynamic index space.
- `Network.HTTP2.HPACK.Decode` — HPACK header-block decoding: the variable-length
  `decodeInteger` (bounded structural fold) and `decodeString` (raw + Huffman)
  primitives, and `decodeHeaders` dispatching the indexed / literal / size-update
  representations (well-founded on the unconsumed input), threading the dynamic
  table. Tested against the RFC 7541 Appendix C wire vectors.
- `Network.HTTP2.HPACK.Encode` — HPACK header-block encoding: `encodeInteger`
  (prefix varint, recursing on a strictly-decreasing value), `encodeString`, the
  `HeaderRep` representations (`encodeHeaderRep`), and `encodeHeaders` (greedy
  indexing), verified by encode→decode round-trips.
- `Network.HTTP2.Types` — connection-level types: `ConnectionError` (→ GOAWAY)
  and `StreamError` (→ RST_STREAM), the `HeaderBlockState` machine assembling
  header blocks across HEADERS + CONTINUATION frames, and an `HTTP2Result`
  three-way result with `map`/`bind`.
- `Network.HTTP2.Stream` — the stream lifecycle (RFC 9113 §5.1): the
  `StreamState` machine, per-stream `StreamInfo` (windows + priority), and a
  `StreamTable` over `Std.HashMap` with `openClientStream`/`updateState`/
  `updatePriority`/`activeStreamCount` and stream-id classification.
- `Network.HTTP2.FlowControl` — flow-control windows (RFC 9113 §5.2): `FlowWindow`
  with `increment` (WINDOW_UPDATE, zero/overflow checks), `consume`/`available`,
  and signed `adjust` for SETTINGS changes; plus `ConnectionFlowControl` and
  per-stream window updates.
- `Network.HTTP2.Server` — the server-side connection handler: preface
  validation, SETTINGS/PING/WINDOW_UPDATE/GOAWAY handling, HEADERS + CONTINUATION
  assembly and HPACK decode, response encoding (`sendResponse`), and the
  `runHTTP2Connection` frame loop (driven by EOF/GOAWAY — no fuel counter).

### `Network.HTTP3` — HTTP/3 over QUIC (RFC 9114)

- `Network.HTTP3.Error` — the `H3Error` error-code enum (RFC 9114 §8.1,
  `0x100`–`0x110`) with total `toCode`/`fromCode` conversions and verified
  round-trip laws.
- `Network.HTTP3.Frame` — HTTP/3 framing (RFC 9114 §7): `FrameType`, the QUIC
  variable-length integer codec (RFC 9000 §16, minimal encoding, fuel-free
  decode), `Frame.encode`/`decode`, and `H3Settings` encode/decode.
- `Network.HTTP3.QPACK.Table` — the 99-entry QPACK static table (RFC 9204
  Appendix A, 0-indexed) with `staticLookup` and `staticFind` (exact then
  name-only).
- `Network.HTTP3.QPACK.Decode` — static-table-only QPACK decoding (RFC 9204):
  the prefix integer (`decodeQInt`, bounded fold) and string-literal primitives,
  and `decodeHeaders` for indexed / literal-with-name-reference / literal-name
  field lines (well-founded loop; rejects dynamic-table references).
- `Network.HTTP3.QPACK.Encode` — static-table-only QPACK encoding: `encodeQInt`
  (prefix varint, recursing on a strictly-decreasing value), `encodeStringLiteral`,
  and `encodeHeaders` (compact indexed form where possible), verified by
  encode→decode round-trips.

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

### `DataFrame` — typed tabular data

- `DataFrame.Internal.Types` — a `DataFrame` with a **proven rectangular
  invariant** (`columns_aligned`: every column has exactly `nRows` elements,
  carried as a runtime-erased proof field). A heterogeneous `Value`
  (int/float/str/bool/null) with `Ord`/conversions, named `Column`s with a
  `ColumnType` tag, and smart constructors (`fromColumns`/`fromRows`/
  `fromNamedColumns`) that discharge the alignment proof; safe proof-carrying
  row access (`getRow?`), plus `GroupedDataFrame`.
- `DataFrame.IO.CSV` — RFC 4180 CSV read/write (`parseCsv`/`toCsv`/`readCsv`/
  `writeCsv`/`readTsv`): a finite `for`-loop state machine (quoted fields,
  doubled-quote escapes, CRLF), `Value` type inference, and a pure float parser.
- `DataFrame.Internal.Column` — column ops: `inferType`/`mk'`/`mapValues`/
  `reInferType`/`filterByMask`/`toFloats`/`toStrings`/null counts/`take`/`drop`/
  `unique` (all pure).
- `DataFrame.Display` — render a frame as an aligned plain-text table
  (`toString`, with truncation + ellipsis) or a Markdown table (`toMarkdown`),
  plus `ToString`/`Repr` instances; pure `.map`/`.flatMap` rendering.
- `DataFrame.Operations.Join` — inner/left/right/outer joins on shared key
  columns (`join`/`innerJoin`/`leftJoin`/`rightJoin`/`outerJoin`); the result's
  rectangular invariant is re-established via `map_column_aligned`.
- `DataFrame.Operations.Sort` — `sortBy`/`sortByMultiple` (asc/desc, multi-key
  with tie-breaking) via `List.mergeSort` over a row-index permutation, with a
  proof the permuted columns stay aligned.
- `DataFrame.Operations.Statistics` — column stats: `sum`/`mean`/`variance`/
  `std`/`median`/`min`/`max`/`minValue`/`maxValue` and `count`/null counts
  (numeric stats `Option Float`, skipping non-numeric/null).
- `DataFrame.Operations.Aggregation` — `groupBy` into a `GroupedDataFrame`
  (pure `foldl` find-or-append) and `aggregate` with `AggFunc`
  (`sum`/`mean`/`count`/`min`/`max`/`first`/`last`/`std`/`var`).
- `DataFrame.Operations.Subset` — `select`/`exclude` columns, `take`/`drop`/
  `head`/`tail`/`slice` rows, `filterBy`/`filterWhere`, and `rename` — each
  re-establishing the rectangular invariant.
- `DataFrame.Operations.Transform` — `addColumn`/`derive` (computed columns),
  `mapColumn`, `dropColumn`, `renameColumn`, and `dimensions`/`info`.

### `Web.Cookie` — HTTP cookies

- `Web.Cookie` — RFC 6265 cookie parsing/rendering: `parseCookies`/`renderCookies`
  for `Cookie:` headers, and a `SetCookie` record (`path`/`domain`/`maxAge`/
  `secure`/`httpOnly`/`sameSite`) with `renderSetCookie`/`parseSetCookie` for
  `Set-Cookie:` (pure parsers, no `Id.run`/`while`).

### `Database.PostgreSQL` — libpq bindings

- `Database.PostgreSQL.LibPQ.Types` — opaque `PgConn`/`PgResult` handles (external
  objects, same pattern as `RawSocket`) and the libpq status enums
  (`ConnStatus`/`ExecStatus`/`TransactionStatus`) with `ofUInt8` decoders, an
  `ExecStatus.isOk` predicate with verified laws, and `PgError`/`PgNotification`.
- `Database.PostgreSQL.LibPQ` — `@[extern]` bindings to PostgreSQL's libpq C
  library (`ffi/postgres.c`): `connect`/`exec`/`execParams`/`prepare`/`execPrepared`,
  result inspection (`ntuples`/`nfields`/`getvalue`/`fname`/`ftype`), escaping,
  LISTEN/NOTIFY, transaction status, and `execCheck`/`connectCheck` helpers. libpq
  is discovered via `pkg-config` in the lakefile.

### `Database.SQL` — high-level client (hasql-style)

- `Database.SQL.Connection` — managed connections over `LibPQ`: a `Settings`
  builder (`uri`/`components`, carrying a proof the connection string is
  non-empty), `acquire`/`release` (idempotent) and bracketed `withConnection`,
  plus a `ConnectionError` type.
- `Database.SQL.Encoders` — composable parameter encoders (`Params α`) that
  serialize typed values to `Array (Option String)` for `execParams`: `text`/
  `int`/`nat`/`float`/`bool`/`ofToString` primitives, `nullable`, `contramap`,
  `pair`/`triple`, each with a `width` and verified width laws.
- `Database.SQL.Session` — the session monad, an `abbrev` for
  `ReaderT Connection (ExceptT SessionError IO)` (so `Monad`/`MonadExcept`/`IO`
  lifting come from the stdlib): `sql`/`query` execution, `transaction`
  (BEGIN/COMMIT/ROLLBACK), `getConnection`, `run`, and a `SessionError` type.
- `Database.SQL.Decoders` — three-level result decoders: `Value` (single
  column: `text`/`int`/`nat`/`float`/`bool`/`nullable`/`map`, with a hand-rolled
  float parser), `Row` (`column`/`seq`/`pair`/`triple` with width laws), and
  `Result` (`rowList`/`rowArray`/`singleRow`/`maybeRow`/`rowsAffected`).
- `Database.SQL.Pool` — a thread-safe connection pool over `IO.Ref` + `Array`:
  `PoolSettings` (bounded `maxSize`/`idleTimeout` proofs), on-demand connection
  creation up to `maxSize`, `use` (auto-return on success/error/exception),
  `destroy`, `stats`, and a `PoolError` type.
- `Database.SQL.Statement` — type-safe parameterized statements
  `Statement p r` composing an `Encoders.Params p` with a `Decoders.Result r`:
  `run` (within a `Session`), `command`/`sql_` constructors, and
  `mapResult`/`contramapParams` (reusing `Result.map`/`Params.contramap`).

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
| `Linen.Data.Base64` | RFC 4648 `encode`/`decode` over `ByteArray` (structural, no `partial`) |
| `Linen.Data.Bifunctor` | `Bifunctor`/`LawfulBifunctor`, `bimap`, `Prod`/`Sum`/`Except` instances |
| `Linen.Data.ByteString` | slice over `ByteArray` (O(1) `take`/`drop`/`splitAt`); full `Data.ByteString` API + `BEq`/`Ord`/`Hashable` |
| `Linen.Data.ByteString.Char8` | Latin-1 `Char` view of `ByteString`: `String`↔`ByteString`, `lines`/`words`/`unlines`/`unwords` |
| `Linen.Data.ByteString.Lazy` | chunked lazy byte strings (`Thunk` tail): `fromChunks`/`toStrict`, lazy `append`, `take`/`drop`, folds |
| `Linen.Data.ByteString.Lazy.Char8` | Latin-1 `Char` view of `LazyByteString`: `String`↔`LazyByteString`, char-wise ops |
| `Linen.Data.ByteString.Short` | `ShortByteString` (`ByteArray` newtype): `pack`/`unpack`/`index`, `toShort`/`fromShort` |
| `Linen.Data.ByteString.Builder` | difference-list builder (O(1) `append`): word/UTF-8/decimal/hex encoders + monoid laws |
| `Linen.Data.CaseInsensitive` | `FoldCase` class + `CI α` wrapper: case-insensitive `BEq`/`Ord`/`Hashable`, original-preserving `ToString` |
| `Linen.Data.Conduit.Internal.Pipe` | conduit's streaming `Pipe` (Freer `pipeM`, strict spine): total `Functor`/`Monad`, no `unsafe` |
| `Linen.Data.Configurator.Types` | config `Value` (string/number/bool/list) + `Config = HashMap String Value` |
| `Linen.Data.Configurator` | `key = value` config parser/loader: `parseConfig`/`lookup`/`require`/`load` |
| `Linen.Data.Default` | `Default` typeclass (sensible defaults) + instances for `Bool`/`Nat`/`String`/`List`/`Option`/… |
| `Linen.Data.IntMap` | `Data.IntMap` API over `Std.HashMap Nat v`: union/intersection/difference/folds/`toAscList`/min-max |
| `Linen.Data.Map` | ordered `Data.Map k v` over `Lean.RBMap`: union/intersection/difference/`mapKeys`/folds/min-max + laws |
| `Linen.Data.Set` | ordered `Data.Set` (`Set'`) over `Lean.RBMap _ Unit`: member/union/intersection/`isSubsetOf`/folds/min-max |
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
| `Linen.Data.String` | `IsString` class + `String.words`/`unwords`/`unlines` (`lines` is core's `splitOn`) |
| `Linen.Data.Traversable` | `Traversable` class (`traverse`/`sequence`) + `List`/`Option`/`NonEmpty`; `LawfulTraversable` |
| `Linen.Data.Unique` | globally unique ids: `newUnique : IO Unique` from a global counter (`BEq`/`Ord`/`Hashable`) |
| `Linen.Data.Void` | vacuous `Empty` instances (`BEq`/`Ord`/`Hashable`/`ToString`) + `Empty → α` singleton law |
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
| `Linen.System.Exit` | `ExitCode` (success/failure) + `exitWith`/`exitSuccess`/`exitFailure` over `IO.Process.exit` |
| `Linen.System.Log.FastLogger` | buffered thread-safe logger (`Std.Mutex`): `newLoggerSet`/`pushLogStr`/`flushLogStr`/`withFastLogger` |
| `Linen.Network.HTTP.Chunked` | HTTP/1.1 chunked transfer encoding over `ByteArray` (`chunkedTransferEncoding`/`encodeChunked`) |
| `Linen.Network.HTTP2.Frame.Types` | HTTP/2 (RFC 9113) framing types: 31-bit `StreamId`, `FrameType`/`ErrorCode`/`SettingsKeyId` + total conversions, `FrameFlags`, `Settings` |
| `Linen.Network.HTTP2.Frame.Decode` | HTTP/2 frame parsing: header, SETTINGS/`applySettings`, GOAWAY/WINDOW_UPDATE/RST_STREAM/PRIORITY/padding, `validateFrameSize` |
| `Linen.Network.HTTP2.Frame.Encode` | HTTP/2 frame serialisation: header/frame, builders (SETTINGS/PING/GOAWAY/HEADERS/DATA/…), `encodePriority`/`encodePadding`, `splitHeaderBlock` |
| `Linen.Network.HTTP2.HPACK.Huffman` | complete HPACK (RFC 7541 App. B) Huffman codec: 257-entry table, `huffmanEncode`/`huffmanDecode` (trie + padding validation), RFC-vector tested |
| `Linen.Network.HTTP2.HPACK.Table` | HPACK tables: 61-entry static (RFC 7541 App. A) + `DynamicTable` FIFO with eviction, `find`/`indexLookup`/`findInTables` |
| `Linen.Network.HTTP2.HPACK.Decode` | HPACK header-block decoding: `decodeInteger`/`decodeString` primitives + `decodeHeaders` (indexed/literal/size-update), RFC App. C tested |
| `Linen.Network.HTTP2.HPACK.Encode` | HPACK header-block encoding: `encodeInteger`/`encodeString`, `HeaderRep`/`encodeHeaderRep`, `encodeHeaders`, round-trip tested |
| `Linen.Network.HTTP2.Types` | HTTP/2 connection types: `ConnectionError`/`StreamError`, `HeaderBlockState` (CONTINUATION assembly), `HTTP2Result` |
| `Linen.Network.HTTP2.Stream` | HTTP/2 stream lifecycle: `StreamState` machine, `StreamInfo`, `StreamTable` (`Std.HashMap`) with open/update/priority/active-count |
| `Linen.Network.HTTP2.FlowControl` | HTTP/2 flow control: `FlowWindow` (`increment`/`consume`/`available`/signed `adjust`), `ConnectionFlowControl`, stream window updates |
| `Linen.Network.HTTP2.Server` | HTTP/2 server connection handler: preface/SETTINGS/PING/WINDOW_UPDATE/GOAWAY, HEADERS+CONTINUATION+HPACK, `runHTTP2Connection` |
| `Linen.Network.HTTP3.Error` | HTTP/3 (RFC 9114 §8.1) `H3Error` codes with `toCode`/`fromCode` and round-trip laws |
| `Linen.Network.HTTP3.Frame` | HTTP/3 framing: `FrameType`, QUIC varint codec (RFC 9000 §16), `Frame.encode`/`decode`, `H3Settings` |
| `Linen.Network.HTTP3.QPACK.Table` | QPACK static table (RFC 9204 App. A, 99 entries, 0-indexed): `staticLookup`/`staticFind` |
| `Linen.Network.HTTP3.QPACK.Decode` | static-only QPACK decoding: `decodeQInt`/`decodeStringLiteral` + `decodeHeaders` (indexed/literal field lines) |
| `Linen.Network.HTTP3.QPACK.Encode` | static-only QPACK encoding: `encodeQInt`/`encodeStringLiteral` + `encodeHeaders`, round-trip tested |
| `Linen.Network.Socket.Types` | phantom-typed `Socket` lifecycle, `Family`/`SockAddr`/`EventType`, non-blocking outcome types |
| `Linen.Network.Socket.FFI` | `@[extern]` C bindings: sockets, options, UDP, `getAddrInfo`, kqueue/epoll event loop |
| `Linen.Network.Socket` | safe phantom-typed lifecycle API, `withSocket`/`listenTCP`/`withEventLoop`, `EventLoop` |
| `Linen.Network.Socket.EventDispatcher` | kqueue/epoll → `Green` bridge: `waitReadable`/`waitWritable`/`recvGreen`/`sendAllGreen` |
| `Linen.Web.Cookie` | RFC 6265 cookie parse/render: `parseCookies`/`renderCookies`, `SetCookie` + `parseSetCookie`/`renderSetCookie` |
| `Linen.DataFrame.Internal.Types` | typed tabular `DataFrame` with a proven rectangular invariant; `Value`/`Column`/`ColumnType` + smart constructors |
| `Linen.DataFrame.IO.CSV` | RFC 4180 CSV `parseCsv`/`toCsv`/`readCsv`/`writeCsv` with type inference |
| `Linen.DataFrame.Internal.Column` | column ops: `inferType`/`mk'`/`mapValues`/`filterByMask`/`toFloats`/`unique`/… |
| `Linen.DataFrame.Display` | render a frame as an aligned text table (`toString`) or Markdown (`toMarkdown`) + `ToString`/`Repr` |
| `Linen.DataFrame.Operations.Join` | inner/left/right/outer joins on shared key columns |
| `Linen.DataFrame.Operations.Sort` | `sortBy`/`sortByMultiple` (asc/desc, multi-key) via `List.mergeSort` |
| `Linen.DataFrame.Operations.Statistics` | column stats: `sum`/`mean`/`variance`/`std`/`median`/`min`/`max`/counts |
| `Linen.DataFrame.Operations.Aggregation` | `groupBy` → `GroupedDataFrame` + `aggregate` with `AggFunc` |
| `Linen.DataFrame.Operations.Subset` | `select`/`exclude`/`take`/`drop`/`slice`/`filterBy`/`filterWhere`/`rename` |
| `Linen.DataFrame.Operations.Transform` | `addColumn`/`derive`/`mapColumn`/`dropColumn`/`renameColumn`/`dimensions`/`info` |
| `Linen.Database.PostgreSQL.LibPQ.Types` | opaque `PgConn`/`PgResult` handles + libpq status enums/decoders, `PgError`/`PgNotification` |
| `Linen.Database.PostgreSQL.LibPQ` | `@[extern]` libpq bindings: connect/exec/prepare, result inspection, escaping, LISTEN/NOTIFY, transactions |
| `Linen.Database.SQL.Connection` | managed connections over libpq: `Settings` builder (non-empty proof), `acquire`/`release`/`withConnection`, `ConnectionError` |
| `Linen.Database.SQL.Encoders` | composable parameter encoders `Params α → Array (Option String)`: primitives, `nullable`/`contramap`/`pair`/`triple`, width laws |
| `Linen.Database.SQL.Session` | session monad (`ReaderT`/`ExceptT IO` stack): `sql`/`query`, `transaction`, `run`, `SessionError` |
| `Linen.Database.SQL.Decoders` | result decoders: `Value`/`Row`/`Result` levels, `singleRow`/`rowList`/`maybeRow`, row width laws |
| `Linen.Database.SQL.Pool` | thread-safe connection pool (`IO.Ref`): `PoolSettings` (bounded proofs), `create`/`use`/`destroy`/`stats`, `PoolError` |
| `Linen.Database.SQL.Statement` | type-safe `Statement p r` = `Params p` + `Result r`: `run`/`command`/`sql_`, `mapResult`/`contramapParams` |

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
