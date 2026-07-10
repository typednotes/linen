# Module Reference

The full feature list and module table for `linen`. See [README.md](../README.md) for
the project overview and quick start.

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
- `Data.Conduit.Internal.Conduit` — the `ConduitT` CPS/codensity wrapper over
  `Pipe` (O(1) monadic bind): `await`/`yield`/`leftoverC`/`liftConduit`/
  `awaitForever`, the `.|` fusion operator, `runConduit`/`runConduitPure`/
  `runConduitRes`, and `bracketP` for resource-safe streaming (built on
  `Control.Monad.Trans.Resource`). Marked `unsafe`: `awaitForever` recurses on
  a runtime `await` result with no structural or well-founded measure — a
  genuine unbounded corecursion, the same one Haskell accepts through laziness.
- `Data.Conduit.Combinators` — the conduit combinator library over `ConduitT`:
  sources (`sourceList`/`sourceArray`/`unfoldC`/`repeatC`/`replicateC`/
  `enumFromToC`), sinks (`sinkList`/`sinkArray`/`foldlC`/`foldMC`/`headC`/
  `lengthC`/`sumC`/`allC`/`anyC`/`findC`/`maximumC`/…), and transformers
  (`mapC`/`mapMC`/`filterC`/`takeC`/`dropC`/`takeWhileC`/`concatMapC`/
  `scanlC`/`intersperseC`/`chunksOfC`/…).
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
- `Data.Scientific` — arbitrary-precision scientific notation $c \times 10^{e}$
  (Haskell's `scientific` package): `normalize`/`isZero`/`isInteger`,
  `toRealFloat`/`fromFloatDigits`, `toBoundedInteger`, `toDecimalDigits`, and
  `Add`/`Sub`/`Mul`/`Neg`/`BEq`/`Ord`/`OfScientific` instances, with verified
  `isZero_iff`/`normalize_zero`/`neg_neg` laws.
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
- `Control.Monad.Except` — `mtl`-named `throwError`/`catchError`/`liftEither`/
  `mapExceptT`/`withExceptT`/`runExceptT` over core's own `ExceptT`/`Except`.
- `Control.Monad.Reader` — the `Reader` alias plus `mtl`-named `ask`/`asks`/
  `local`/`runReaderT`/`runReader`/`mapReaderT` over core's own `ReaderT`/
  `read`/`ReaderT.adapt`.
- `Control.Monad.State` — the `State` alias plus `mtl`-named `put`/`gets`/
  `runStateT`/`evalStateT`/`execStateT`/`runState`/`evalState`/`execState`
  over core's own `StateT` (`get`/`set`/`modify` are core's `MonadState`
  names already — used directly, not re-wrapped).
- `Control.Monad.Trans` — the `mtl`-named `lift` over core's own
  `MonadLift`/`monadLift`, with the `lift_pure`/`lift_bind` laws restated
  generically (core's `MonadLift`/`LawfulMonadLift` already generalize
  Haskell's `MonadTrans` class, with lawful instances for `ExceptT`,
  `ReaderT`, and `StateT`).
- `Control.Monad.Trans.Resource` — deterministic, exception-safe LIFO resource
  cleanup: `ResourceT = ReaderT (IO.Ref CleanupMap)` over core's own `ReaderT`
  (only a `MonadLift IO` instance needed on top), `allocate`/`release`/
  `runResourceT` (cleanup runs via `try`/`finally`, even on an exception), and
  a verified `releaseKey_eq` law.
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
- `Control.Monad.STM` — software transactional memory: `STM α = BaseIO
  (STMResult α)`, with every transaction serialized on a single global
  `Std.Mutex` (`atomically`/`retry`/`orElse`/`check`); `atomically`'s
  retry-until-commit loop is a plain `while`, not `partial def`.
- `Control.Concurrent.STM.TVar` — a transactional variable over `IO.Ref`:
  `newTVarIO`/`newTVar`/`readTVar`/`writeTVar`/`modifyTVar'`.
- `Control.Concurrent.STM.TMVar` — `TVar (Option α)`: `newTMVar(IO)`/
  `newEmptyTMVar(IO)`/`takeTMVar`/`putTMVar`/`readTMVar`/`tryTakeTMVar`/
  `tryPutTMVar`/`isEmptyTMVar`.
- `Control.Concurrent.STM.TQueue` — a transactional, amortized-O(1) FIFO over
  two `TVar`-held lists: `newTQueue(IO)`/`writeTQueue`/`readTQueue`/
  `tryReadTQueue`/`isEmptyTQueue`/`peekTQueue`.

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
- `Network.HTTP.Date` — HTTP date parsing/formatting (RFC 7231): `HTTPDate`,
  `parseHTTPDate` (IMF-fixdate + asctime), and `formatHTTPDate` (IMF-fixdate with
  the day-of-week from Zeller's congruence).
- `Network.HTTP.Types.Header` — case-insensitive header names (`HeaderName =
  CI String`), the `Header`/`RequestHeaders`/`ResponseHeaders` aliases, and the
  ~50 standard header-name constants (`hContentType`, `hHost`, …).
- `Network.HTTP.Types.Method` — `StdMethod`/`Method` (standard or custom),
  `parseMethod`/`renderMethod`, and the RFC 9110 §9.2 `isSafe`/`isIdempotent`
  predicates with verified laws (incl. safe ⇒ idempotent).
- `Network.HTTP.Types.Status` — proof-carrying `Status` (a `statusValid : 100 ≤
  code ≤ 999` field, erased at runtime), ~50 named codes + aliases, the
  `isInformational`/…/`isServerError` class predicates, and the RFC 9110 §6.4.1
  `mustNotHaveBody` rule with verified theorems.
- `Network.HTTP.Types.URI` — query-string `parseQuery`/`renderQuery` (over
  `Query = List (String × Option String)`) and percent-encoding
  `urlEncode`/`urlDecode` (the latter a structural recursion over the char list).
- `Network.HTTP.Types.Version` — `HttpVersion` (major/minor) with lexicographic
  `Ord`, `ToString` (`HTTP/1.1`), the `http09`/`http10`/`http11`/`http20`
  constants, and well-formedness theorems.
- `Network.HTTP.Client.Types` — HTTP/1.1 client core types: the transport
  `Connection` (read/write/close callbacks abstracting TCP vs TLS), the wire-level
  `Request`, and the parsed `Response` with case-insensitive `findHeader`,
  `contentLength`, and `isSuccess`.
- `Network.HTTP.Client.Request` — HTTP/1.1 request serialization
  (`serializeRequest`): request line + headers, auto-adding `Host` (with
  non-default port), `Content-Length` (when a body is present), and
  `Connection: close`, plus `sendRequest` over a `Connection`.
- `Network.HTTP.Client.Response` — HTTP/1.1 response parsing: status line,
  headers, and bodies via Content-Length / chunked / read-until-close
  (`receiveResponse`, `performRequest`). The network read-loops are
  condition-driven `while`s — no `partial`.

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
- `Network.HTTP3.Server` — the HTTP/3 request/response layer on top of a QUIC
  connection: `H3Request`/`H3Response`, the `H3Handler` handler type,
  `sendResponse` (QPACK-encodes and frames a response over a `QUICStream`),
  and `handleRequestStream`/`handleConnection` (decode HEADERS, dispatch,
  reply). `handleConnection` is stubbed pending QUIC stream-accept support.

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
  listen → accept` (and `connect`/`connectFinish`, `send`/`recv`, `sendAll`,
  UDP `sendTo`/`recvFrom`) with each transition's pre/post state in its
  signature, a `close` whose `state ≠ .closed` proof obligation makes
  double-close a type error, `withSocket` / `withListenTCP` / `withEventLoop`
  bracket helpers, `listenTCP`/`listenTCP6`, address introspection, and an
  `EventLoop` (kqueue/epoll) wrapper.
- `Network.Socket.EventDispatcher` — the bridge from socket readiness to the
  green-thread model: a **sharded** set of dispatch threads (fds partitioned by
  `fd % N`, each shard its own kqueue/epoll loop + waiter map) resolves an
  `IO.Promise` when a socket is ready, so `waitReadable` / `waitWritable` (and
  `recvGreen` / `sendAllGreen`) **suspend a `Green` thread as a heap object
  instead of holding an OS thread**. This is what lets one worker pool serve many
  thousands of IO-bound connections.
- `Network.Socket.Blocking` — blocking-style `accept` / `connect` / `send` /
  `sendAll` / `recv`, retrying on `wouldBlock` for tests, scripts, and code
  that doesn't need event-loop integration (production non-blocking I/O should
  use `EventDispatcher` instead). Every retry loop is a plain `while`, so the
  module needs no `partial def`.
- `Network.Sendfile` — a portable `sendFile`/`sendFileSimple` for transferring
  a file (or `FilePart` range) over a connected socket, via chunked read +
  `Blocking.sendAll` (no platform `sendfile(2)` zero-copy syscall).
- `Data.Streaming.Network` — Haskell's `Data.Streaming.Network`: `AppData`,
  `bindPortTCP`/`getSocketTCP`/`mkAppData`/`runTCPServer`, and `acceptSafe`
  (retry-on-transient-accept-error), with its retry loop a plain `while`
  instead of the upstream `partial def`.

### `Network.Mime` — MIME type lookup

- `Network.Mime` — a port of Haskell's `Network.Mime` (`mime-types`): a
  `defaultMimeMap` (Apache/nginx/IANA extensions → MIME types), `mimeByExt`
  and `defaultMimeLookup` for resolving a file name to its content type, and a
  `fileNameExtensions` that yields the multi-part extensions most-specific-first
  (`"foo.tar.gz" ↦ ["tar.gz", "gz"]`) — rewritten from the upstream `partial`
  helper into **structural recursion** over the dot-separated components, and
  using `List.lookup` / `List.findSome?` instead of a bespoke assoc scan.

### `Network.URI` — RFC 3986 URI parsing, rendering, and resolution

- `Network.URI` — a port of the `network-uri` package's `Network.URI`: the
  `URI`/`URIAuth` types, `parseURI`/`parseURIReference`/`parseRelativeReference`/
  `parseAbsoluteURI`, `isURI`-style classifiers, percent-encoding
  (`escapeURIString`/`unEscapeString`), rendering (`uriToString`, a
  password-masking `ToString` instance), `pathSegments`, dot-segment removal, and
  relative-URI resolution (`relativeTo`/`relativeFrom`, matching every case in
  RFC 3986 section 5.4's worked example table). The upstream parser is built on
  `parsec`; here the grammar is a direct structurally-recursive recursive-descent
  parser over `List Char` instead. Two documented simplifications: bracketed
  IP-literal hosts (`[::1]`) are accepted at the character-class level rather
  than RFC 3986's full IPv6 group-count grammar, and `unEscapeString` decodes
  each `%XX` to its raw byte rather than reassembling multi-byte UTF-8 (unneeded
  by anything currently in `linen`).

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

### `Web.Css` / `Web.Html` — typed CSS and HTML5, illegal constructs are compile errors

- `Web.Css` — every declaration comes from a typed smart constructor
  (`color`/`margin`/`display`/…) that pins down both the property name and the
  Lean type of its value; `Declaration`'s `private` constructor makes an
  arbitrary `property := value` pairing a compile-time error. `Length`
  (`px`/`pct`/`em`/`rem`/`vw`/`vh`/`auto`/`zero`) rules out unit-less values,
  and `FontWeight.numeric`'s `by decide` proof rejects out-of-range weights.
  Selectors, `Rule`s, and `Stylesheet`s compose, with `rule!` macro sugar for
  building a `Rule` from a selector and a list of declarations.
- `Web.Html` — `Html` is indexed by a `Category` (flow/phrasing/list-item/
  table-row/table-cell) that encodes HTML5's content model: each element
  constructor fixes the category of children it accepts, so a `<div>` inside a
  `<p>`, a `<li>` outside a `<ul>`/`<ol>`, or children on a void element like
  `<img>` are all Lean type errors, not browser auto-corrections. Attributes
  go through the same `private`-constructor discipline as `Web.Css.Declaration`,
  and `elem!` macro sugar builds elements from a tag/attrs/children triple.

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

### `Crypto.JOSE` — JOSE/JWT cryptography (OpenSSL)

- `Crypto.JOSE.FFI` — `@[extern]` bindings to OpenSSL's EVP API (`ffi/jose.c`):
  HMAC (HS256/384/512), RSA verification (RS/PS), EC verification (ES256/384/512),
  JWK→DER public-key construction, and base64url. OpenSSL is discovered via
  `pkg-config` in the lakefile.
- `Crypto.JOSE.Types` — JOSE/JWT/JWK data types: `JWSAlgorithm`, `ECCurve`,
  `JWKKeyType`/`JWKKeyMaterial`, a proof-carrying `JWK` (`kty` coherent with its
  material), `ClaimsSet`, `JWSHeader`, `JWTValidationSettings` (bounded skew),
  and `JwtError` — with parse/round-trip laws.
- `Crypto.JOSE.JWK` — JWK helpers over the FFI: `parseOctKey` (base64url →
  symmetric key) and `toDerPublicKey` (RSA/EC JWK → DER public key via OpenSSL).
- `Crypto.JOSE.JWS` — JWS compact-serialization verification (RFC 7515):
  `splitCompact` and `verifySignature`, dispatching HMAC / RSA (PKCS1+PSS) / EC
  to the OpenSSL FFI.
- `Crypto.JOSE.JWT` — JWT verification (RFC 7519): pure `validateClaims`
  (`exp`/`nbf` with bounded skew, `aud`/`iss` matching) and IO `verifyJWT`
  (parse compact form, verify the signature over the candidate JWK set, then
  validate the claims).

### `Options.Applicative` — command-line argument parsing

- `Options.Applicative.Types` — the core types of Haskell's
  `optparse-applicative`: `ReadM` (a `String → Except String α` reader),
  `Mod` (a right-biased monoid of option modifiers — long/short name, help,
  metavar, hidden, showDefault), `InfoMod`/`ParserInfo` (program description,
  header/footer, failure code), and `OptDescr` (option/flag/argument/
  subcommand descriptions kept separate for help generation). `Parser` is a
  **functional** representation (`List String → Except String (α × List
  String)`) standing in for Haskell's free-applicative GADT, which Lean's
  positivity checker rejects.
- `Options.Applicative.Builder` — the fluent builder API over `Types`:
  modifier constructors (`long`/`short`/`help`/`metavar`/`hidden`/
  `showDefault`), readers (`str`/`eitherReader`/`auto` via a `FromString`
  class), option/flag/argument/subcommand builders (`option`/`strOption`/
  `switch`/`flag`/`flag'`/`argument`/`subparser`), `Pure`/`Functor`/`Seq`/
  `Applicative`/`OrElse` instances for `Parser` (so parsers compose with
  `<*>`/`<|>`), and `withDefault`/`optionWithDefault`/
  `strOptionWithDefault`/`command`.
- `Options.Applicative.Extra` — the high-level execution API: `renderHelp`
  (generates usage/options/commands help text from a `ParserInfo`), `helper`
  (adds `--help`/`-h` support as an identity-returning flag), `info` (wraps a
  parser with `helper` and `InfoMod` metadata), `hsubparser` (a subcommand
  parser hidden from help), `execParser` (runs a `ParserInfo` against `IO`
  arguments, printing help and exiting on failure or `--help`), and
  `execParserPure` (an `Except`-returning variant for testing).

### `PostgREST.ApiRequest` — request-preference parsing

- `PostgREST.ApiRequest.Preferences` — parses the HTTP `Prefer` header used by
  PostgREST-style APIs: `PreferCount`/`PreferReturn`/`PreferResolution`/
  `PreferTransaction`/`PreferMissing`/`PreferHandling` enums (each with a
  `none_`/unspecified variant and a `ToString` rendering back to the wire
  format), a `Preferences` struct bundling all dimensions plus an optional
  `preferMaxAffected` count, and `parsePreferences` to fold a list of header
  values (comma- or semicolon-separated, whitespace-trimmed) into a
  `Preferences`.
- `PostgREST.Auth` — the result of authenticating a request: `AuthResult`
  bundles the PostgreSQL role to assume for the request (with a proof that
  the role name is non-empty, since PostgreSQL rejects empty role names for
  `SET ROLE`) together with the JWT claims as key-value pairs, plus
  `AuthResult.lookupClaim` to look up a claim by key.
- `PostgREST.Auth` — the authentication middleware built on `Auth.Types`:
  `extractBearerToken`/`findAuthHeader` pull a Bearer token out of the
  Authorization header, `extractRole` resolves a role from a dot-separated
  claim path (falling back to the anonymous role), and `authenticate` ties
  it together into an `Except String AuthResult`. JWT signature validation
  is stubbed pending wiring to `Crypto.JOSE`, matching the equivalent point
  in the upstream `postgrest` project.
- `PostgREST.Cache.Sieve` — the SIEVE cache eviction policy (scan-resistant,
  simpler than ARC): `SieveCache` holds entries in a circular buffer with a
  "hand" pointer; `create`/`lookup`/`insert`/`remove`/`size` manage the
  cache, with `lookup` marking an entry visited and eviction sweeping the
  hand to clear visited flags before evicting the first unvisited entry.
- `PostgREST.Config.JSPath` — JWT claim path parsing: `JSPath` is a list of
  `JSPathSegment`s (`key`/`index`) parsed from strings like `.role` or
  `.user.permissions` via `JSPath.parse`; `follow`/`followNested` walk a
  flat (or one-level-nested) key-value claim association list, and
  `defaultRoleClaimPath` is the default `.role` path.
- `PostgREST.Config.PgVersion` — PostgreSQL version parsing and comparison:
  `PGVersion` (major/minor/patch) with `ToString`/`Ord`/`Inhabited`,
  `fromVersionNum`/`toVersionNum` for PostgreSQL's packed integer encoding
  (e.g. `150004` ↔ `15.0.4`), `PGVersion.parse` for dotted version strings,
  and `pgVersionMin`/`isSupported`/`isAtLeastMajor`/`isAtLeast` for
  minimum-version checks (PostgreSQL 9.6+).
- `PostgREST.Config.Proxy` — reverse-proxy URI configuration for OpenAPI
  spec generation: `ProxyUri` (scheme/host/port/base path) with
  `ProxyUri.parse`/`toUri`/`ToString`, and `openApiServerUrl` to build the
  OpenAPI server URL from either the configured proxy or the bound
  host/port.
- `PostgREST.Cors` — CORS middleware: `corsHeaders` and `preflightHeaders`
  compute the `Access-Control-*` response headers for an origin against an
  optional allow-list, plus `defaultExposedHeaders`/`defaultAllowedHeaders`.
- `PostgREST.Debounce` — schema-cache-reload rate limiting: `Debouncer`
  (an `IO.Ref`-backed timestamp plus a minimum interval) and
  `Debouncer.create`/`run`, which only invokes the given action once the
  interval has elapsed since the last invocation.
- `PostgREST.Listener` — PostgreSQL `LISTEN`/`NOTIFY` channel handling:
  `pgrstChannel`/`listenSql`, and `NotificationAction`/`parseNotification`
  to classify a notification payload as a schema-cache reload, a config
  reload, or an unrecognized payload.
- `PostgREST.Logger` — structured logging: `LogLevel` (`crit`/`error`/
  `warn`/`info`/`debug`) with `Ord`/`ToString`, and `log`/`logCrit`/
  `logError`/`logWarn`/`logInfo`/`logDebug`, which print a timestamped
  message to stderr only when the level is at or above the configured
  threshold.
- `PostgREST.MediaType` — content-type negotiation: `MediaType` (JSON,
  CSV, plain, XML, octet-stream, GeoJSON, OpenAPI, the `vnd.pgrst.object`
  singular-object types, an `EXPLAIN` plan type, and an `other` escape
  hatch) with `toMime`/`toContentType`/`ofMime`/`isJSON`/`isText`; 9
  `native_decide` theorems prove the `ofMime`/`toMime` roundtrip for every
  standard variant.
- `PostgREST.Network` — server binding helpers: `resolveHost` maps the
  `"!4"`/`"!6"`/`"*"` bind-address shorthands to their concrete
  IPv4/IPv6 wildcard addresses.
- `PostgREST.RangeQuery` — HTTP range-based pagination: `NonnegRange`
  (offset + optional limit) with `parseRange` for the `Range` header, and
  `ContentRange` (offset/limit/optional total, with a proof that the range
  fits within the total) with `contentRangeHeader`/`ContentRange.fromNonnegRange`
  for the `Content-Range` response header.
- `PostgREST.Response` — HTTP response construction: `contentRangeHeader`,
  `readHeaders`/`mutateHeaders`, and `readStatus` (200/206/416 depending on
  offset/count/total), with a `readStatus_valid` theorem proving the
  returned status code is always in `[100, 599]`.
- `PostgREST.Response.GucHeader` — GUC-variable-to-HTTP-header mapping:
  `gucHeaderPrefix`/`gucStatusVar` and `parseGucHeaders`/`parseGucStatus`
  for the `response.headers`/`response.status` PostgreSQL session settings.
- `PostgREST.Response.Performance` — performance timing headers:
  `serverTimingHeader`, `serverTimingValue`, and `timingHeaders`, which
  builds a `Server-Timing` header from total/plan/exec durations.
- `PostgREST.SchemaCache.Identifiers` — schema-qualified identifiers:
  `QualifiedIdentifier` (schema + name) with `BEq`/`Hashable`/`Ord`/
  `ToString`, `escapeIdent`/`quoteIdent`/`quoteQi` for injection-safe SQL
  quoting, `toQi` parsing, `anyElement`/`isAnyElement`, and `RelIdentifier`;
  4 theorems prove `quoteIdent`'s quoting/escaping behavior.
- `PostgREST.ApiRequest.Types` — the API-request domain model: `Mutation`/
  `InvokeMethod`/`Action`, `JsonOperation`, `SimpleOperator`/`FtsOperator`/
  `QuantOperator`/`FilterOperator`, `Filter`, `LogicOperator`/`LogicTree`,
  `OrderDirection`/`OrderNulls`/`OrderTerm`, `SelectItem`, `Payload`, `IsVal`,
  and `Target`, each with a `ToString` rendering back to PostgREST filter/
  select syntax.
- `PostgREST.Config` — application configuration: `LogLevel`/`OpenAPIMode`,
  a refined `Port` (1-65535, proof-carrying), and the flat `AppConfig`
  record (`configDbUri`/`configDbSchemas`/`configJwtSecret`/`configServerPort`/
  …) with non-emptiness/positivity proof fields, `AppConfig.default`, and
  query helpers (`hasJwtSecret`, `hasAdminServer`, `mainSchema`, …); 2
  roundtrip theorems for `LogLevel`/`OpenAPIMode` parsing.
- `PostgREST.Config.Database` — database connection helpers: `DbUriParts`
  (host/port/dbname/user/password) with `toUri` reconstruction,
  `searchPathSql`/`searchPathDisplay` for the `search_path` GUC,
  `setRoleSql`/`resetRoleSql` for `SET LOCAL ROLE`, and `TxMode`/`TxEnd`
  for transaction access mode and commit/rollback.
- `PostgREST.Error.Types` — the PostgREST error hierarchy: `RangeError`/
  `QPError`/`ApiRequestError`/`SchemaCacheError`/`JwtError`/`PgError`
  (SQLSTATE-code-carrying, proof that the code is exactly 5 characters)
  combine into the top-level `Error` union, each with `ToString`/`BEq`/
  `Repr` and a `toHttpStatus` mapping to the appropriate HTTP status code;
  3 theorems prove every `toHttpStatus` mapping stays within 100-599.
- `PostgREST.Error` — error formatting: `errorPayload` renders any `Error`
  as a JSON body (`message`/`details`/`hint`/`code`, with PostgREST's
  `PGRST…` error codes) and `errorHeaders` produces the matching HTTP
  headers, adding `WWW-Authenticate: Bearer` for JWT errors and
  unauthenticated `insufficient_privilege` Postgres errors.
- `PostgREST.MainTx` — the per-request transaction wrapper: `sqlLit`
  (SQL string-literal escaping), `setSearchPath`/`setRole` and
  `setRequestContext` (the `SET LOCAL` statements for role, JWT claims,
  method, path, and headers), and `preRequestSql` for invoking a
  configured pre-request function.
- `PostgREST.Plan.Types` — resolved query-plan types: `CoercibleField` (column
  reference with JSON path traversal and optional type coercion),
  `AggregateFunction` (`count`/`sum`/`avg`/`max`/`min`/`json_agg`/
  `jsonb_agg`) with `toSql`, `CoercibleSelectField`/`CoercibleFilter`/
  `CoercibleOrderTerm`, the recursive `CoercibleLogicTree` boolean
  expression tree, `SpreadType`/`RelJsonEmbedMode` for embedded
  resources, and `ConflictAction` for `INSERT ... ON CONFLICT`.
- `PostgREST.Query.SqlFragment` — the core SQL builder: `pgFmtIdent`/
  `pgFmtQi`/`pgFmtLit` for injection-safe identifier and literal quoting,
  `pgFmtField` (column reference with JSON path traversal and cast),
  `simpleOpToSql`/`ftsOpToSql` operator mapping, `pgFmtFilter`, the
  recursive `pgFmtLogicTree` (ported without `partial`, using the same
  structural-recursion-through-`Array.map` pattern as
  `ApiRequest.Types.LogicTree.toString`), `pgFmtOrderTerm`, JSON
  aggregation wrappers (`asJsonF`/`asJsonSingleF`), and `SET LOCAL` GUC
  helpers; 2 theorems prove `pgFmtIdent`/`pgFmtLit` always quote.
- `PostgREST.SchemaCache.Relationship` — foreign-key relationships between
  tables: `Cardinality` (`O2M`/`M2O`/`O2O`/`M2M` via a junction table) with
  `BEq`/`ToString`, `Relationship` (table/foreign table/cardinality/column
  pairs) with `BEq`/`ToString`, and `localColumns`/`foreignColumns`
  accessors.
- `PostgREST.Plan.ReadPlan` — a resolved SELECT query plan: `ReadPlan`
  (select list, source table, filters, ordering, pagination via
  `RangeQuery.NonnegRange`, and `rpRelationships` — embedded sub-queries
  that become lateral joins, making `ReadPlan` recursive) with
  `hasEmbeds`/`embedCount`/`hasFilters`/`hasOrdering` queries. Reuses
  `PostgREST.RangeQuery.NonnegRange` for pagination rather than
  redeclaring an equivalent range type.
- `PostgREST.Plan.MutatePlan` — resolved INSERT/UPDATE/DELETE plans:
  `MutatePlan` (`insert` with an optional `ConflictAction` for upserts,
  `update`/`delete` with a `NonnegRange` for `LIMIT`-ed mutations) with
  `targetTable`/`returningFields`/`hasReturning`.
- `PostgREST.SchemaCache.Representations` — PostgreSQL type casts for
  output formatting and input parsing: `Representation` (source/target
  type + conversion function) and `MediaHandler` (custom content-type
  output function).
- `PostgREST.SchemaCache.Routine` — PostgreSQL function/procedure metadata
  exposed as RPC endpoints: `Volatility`/`IsolationLevel`/`ParamMode`,
  `RoutineParam`, `RoutineReturnType` (`single`/`setof`/`void`) with
  `isSetof`, and `Routine` with `toQi`/`requiredParams`/`isSafeForGet`;
  a theorem proves `isSafeForGet` holds exactly for non-`volatile`
  functions.
- `PostgREST.Plan.CallPlan` — a resolved RPC call plan: `CallPlan` binds
  a `Routine` to concrete parameter values and a returning clause, with
  `routineQi`/`isSetof`/`isSafeForGet`/`paramCount`.
- `PostgREST.SchemaCache.Table` — table/view metadata from the system
  catalogs: `Column` (name/type/nullability/default/enum values) and
  `Table` (columns, primary key, INSERT/UPDATE/DELETE permissions) with
  a proof field (`pk_subset`) that every primary-key column genuinely
  appears in `tableColumns`, plus `toQi`/`findColumn`/`columnNames`/
  `pkColumnNames`/`hasPrimaryKey`.
- `PostgREST.SchemaCache` — the schema introspection cache: `SchemaCache`
  aggregates tables, relationships, routines, and representations with
  `empty`/`findTable`/`findRelationships`/`findRoutines`/
  `tablesInSchemas`, plus the catalog-introspection SQL literals
  (`tablesSql`/`columnsSql`/`relationshipsSql`/`routinesSql`/`versionSql`)
  used to populate it from `pg_catalog`.
- `PostgREST.AppState` — the shared mutable state of a running instance:
  `Observation` events (for logging/metrics), `Metrics` counters, and
  `AppState` itself (`IO.Ref`-backed schema cache and metrics plus an
  observer callback) with `create`/`getSchemaCache`/`putSchemaCache`/
  `observe`/`incRequestCount`/`incErrorCount`.
- `PostgREST.Metrics` — `renderMetrics` renders `AppState.Metrics`
  counters in Prometheus text exposition format.
- `PostgREST.Admin` — the optional admin HTTP server: `handleAdminRequest`
  answers `/live` (liveness), `/ready` (schema cache loaded), `/metrics`
  (Prometheus exposition), and 404s any other path.
- `PostgREST.Observation` — `defaultObserver` logs every `AppState.Observation`
  event to stderr (schema cache load/failure, pool exhaustion, JWT
  failures, request completion, server start, config reload, LISTEN
  notifications).
- `PostgREST.TimeIt` — `timeIt`/`timeIt_` time an `IO` action and return
  its result (or discard it) alongside the elapsed milliseconds.
- `PostgREST.Unix` — `defaultSocketMode` is the default Unix socket file
  permission mode (`0o660`).
- `PostgREST.Version` — `version`/`prettyVersion` identify this port
  (`12.2.0-linen`, "Linen/Lean 4 port").
- `PostgREST.App` — the core request-handling application:
  `SimpleRequest`/`SimpleResponse`, `handleRequest` (root table listing,
  CORS preflight, per-table GET/HEAD/POST/PATCH/DELETE/OPTIONS dispatch,
  an RPC stub, 404/405/501 errors, metrics and observation recording,
  and CORS headers on every response), and `printBanner` for startup.
- `PostgREST.CLI` — command-line argument parsing: `Command`
  (`serve`/`version`/`dumpConfig`/`dumpSchema`/`help`), `parseArgs`,
  and `printUsage`.
- `PostgREST.Response.OpenAPI` — OpenAPI 3.0 specification generation:
  `pgTypeToOpenAPI` maps PostgreSQL types to OpenAPI type/format pairs,
  `columnSchema` renders a column's JSON schema, and
  `generateOpenAPISpec` builds the full spec (paths, schemas) from a
  `SchemaCache`.

### `Network.TLS` — TLS 1.2/1.3 over OpenSSL (FFI)

- `Network.TLS.Types` — `TLSVersion` (`tls10`–`tls13`), a `CipherID` alias, and
  `TLSOutcome α` — the `.ok`/`.wantRead`/`.wantWrite`/`.error` sum type every
  non-blocking TLS operation returns on `SSL_ERROR_WANT_READ`/`WANT_WRITE`.
- `Network.TLS.Context` — opaque `TLSContext`/`TLSSession` handles over
  OpenSSL's `SSL_CTX`/`SSL` (`ffi/tls.c`, GC-finalized): server-side
  `createContext`/`setAlpn`/`acceptSocket`/`read`/`write`/`close`/
  `getVersion`/`getAlpn`, non-blocking `*NB` variants, and client-side
  `createClientContext` (system CA trust) / `createClientContextWithCA`
  (trust a specific CA file — e.g. a self-signed cert in tests) /
  `connectSocket` (SNI + hostname verification, with a `while`-loop retry on
  `WANT_READ`/`WANT_WRITE`, not `partial def`).

### `Network.QUIC` — QUIC transport protocol (RFC 9000)

- `Network.QUIC.Types` — core QUIC types: a proof-carrying `ConnectionId`
  (`bytes.size ≤ 20`, RFC 9000 §17.2), `Version`, `TransportParams` with
  RFC 9000 §18 defaults, `StreamId` with 2-bit type/directionality/initiator
  classification, the `TransportError` code enum (RFC 9000 §20), and
  `TLSConfig`.
- `Network.QUIC.Config` — `ServerConfig`/`ClientConfig`, bundling a `TLSConfig`,
  `TransportParams`, and host/port (or server-name) fields with sensible
  defaults.
- `Network.QUIC.Connection` — an opaque, only-internally-constructible
  `Connection` handle plus `ConnectionState`; `sendStream`/`recvStream`/
  `openStream`/`closeStream`/`getState`/`close` are stubbed pending TLS 1.3
  FFI (to `quiche` or `ngtcp2`).
- `Network.QUIC.Client` — `connect : ClientConfig → IO Connection`, stubbed
  pending TLS 1.3 FFI.
- `Network.QUIC.Server` — `run`/`accept : ServerConfig → IO Connection`
  (or handler-dispatching loop), stubbed pending TLS 1.3 FFI.
- `Network.QUIC.Stream` — `QUICStream`, a `Connection` + `StreamId` pair with
  `send`/`recv`/`close` delegating to the underlying `Connection`.

### `Network.WebApp` — WAI-style web application interface

- `Network.WebApp.Internal` — the core types: `Request`/`Response`/
  `Application`/`Middleware`, and the `AppM .pending .sent` indexed monad
  that enforces **exactly-once `respond`** at the type level (double-respond
  and no-respond are both type errors).
- `Network.WebApp` — the public API: response constructors (`responseLBS`/
  `responseFile'`/`responseStream'`), request body accessors
  (`getRequestBodyChunk`/`strictRequestBody`, non-idempotent one-shot
  semantics), `defaultRequest`, header mappers, and verified middleware-
  algebra laws (`idMiddleware_comp_left`/`_right`, `modifyRequest_id`,
  `modifyResponse_id`, `ifRequest (fun _ => false) m = id`).
- `Network.WebApp.Static.Types` — `Piece`, a refined path segment that
  prevents directory-traversal attacks, plus `StaticSettings`.
- `Network.WebApp.Static.Storage.Filesystem` — `defaultFileServerSettings`:
  filesystem-backed static file storage.
- `Network.WebApp.Static.Application` — `staticApp`/`static`: an
  `Application` serving files per `StaticSettings`.
- `Network.WebApp.Extra.Header` — request header convenience queries.
- `Network.WebApp.Extra.Request` — request convenience queries (path,
  method, content-type helpers).
- `Network.WebApp.Extra.UrlMap` — dispatch to sub-applications by path
  prefix.
- `Network.WebApp.Extra.Parse` — URL-encoded form body parsing.
- `Network.WebApp.Extra.Test` — a simulated testing harness (`SRequest`/
  `SResponse`/`runSession`/`get`/`post`) with a genuine one-shot
  request-body contract (full body on first read, empty on every
  subsequent read).
- `Network.WebApp.Extra.Test.Internal` — re-exports `Test`.
- `Network.WebApp.Extra.EventSource` — Server-Sent Events (W3C, compatible
  with the JS `EventSource` API): `ServerEvent`/`render`/`eventSourceApp`.
- `Network.WebApp.Extra.EventSource.EventStream` — SSE framing helpers:
  `dataEvent`/`namedEvent`/`retryEvent`/`commentEvent`.
- `Network.WebApp.Extra.Middleware.AcceptOverride` — override the `Accept`
  header from a query-string parameter.
- `Network.WebApp.Extra.Middleware.AddHeaders` — add fixed headers to every
  response.
- `Network.WebApp.Extra.Middleware.Approot` — detect the application root
  URL from headers or configuration.
- `Network.WebApp.Extra.Middleware.Autohead` — convert `HEAD` requests to
  `GET` and strip the response body.
- `Network.WebApp.Extra.Middleware.CleanPath` — normalize double/trailing
  slashes, redirecting to the canonical path.
- `Network.WebApp.Extra.Middleware.CombineHeaders` — merge duplicate
  response headers, joining values with commas.
- `Network.WebApp.Extra.Middleware.ForceDomain` — redirect to a canonical
  domain.
- `Network.WebApp.Extra.Middleware.ForceSSL` — redirect HTTP to HTTPS.
- `Network.WebApp.Extra.Middleware.Gzip` — gzip `Accept-Encoding`
  negotiation (compression itself deferred pending zlib FFI, matching
  the upstream's own stub).
- `Network.WebApp.Extra.Middleware.HealthCheckEndpoint` — a health-check
  endpoint that returns 200 OK without hitting the wrapped app.
- `Network.WebApp.Extra.Middleware.HttpAuth` — HTTP Basic Authentication.
- `Network.WebApp.Extra.Middleware.Jsonp` — wrap JSON responses in a
  callback function for cross-origin requests.
- `Network.WebApp.Extra.Middleware.Local` — restrict access to localhost.
- `Network.WebApp.Extra.Middleware.MethodOverride` — override the HTTP
  method from a query-string parameter.
- `Network.WebApp.Extra.Middleware.MethodOverridePost` — override the HTTP
  method from a POST body's `_method` field.
- `Network.WebApp.Extra.Middleware.RealIp` — update `remoteHost` from
  `X-Forwarded-For`/`X-Real-IP` headers.
- `Network.WebApp.Extra.Middleware.RequestLogger` — request logging to a
  configurable destination, in Apache Combined or dev-friendly colorized
  format.
- `Network.WebApp.Extra.Middleware.RequestLogger.JSON` — structured JSON
  request logging.
- `Network.WebApp.Extra.Middleware.RequestSizeLimit` — reject requests
  whose body exceeds a size limit, without ever buffering the excess.
- `Network.WebApp.Extra.Middleware.RequestSizeLimit.Internal` — re-exports
  `RequestSizeLimit`.
- `Network.WebApp.Extra.Middleware.Rewrite` — rewrite request paths by
  custom rule.
- `Network.WebApp.Extra.Middleware.Routed` — apply a middleware only to
  requests matching a path predicate.
- `Network.WebApp.Extra.Middleware.Select` — conditionally apply a
  middleware.
- `Network.WebApp.Extra.Middleware.StreamFile` — convert `.responseFile`
  into `.responseStream`, for servers without `sendfile(2)` support.
- `Network.WebApp.Extra.Middleware.StripHeaders` — remove specified headers
  from responses.
- `Network.WebApp.Extra.Middleware.Timeout` — enforce a processing timeout,
  returning 503 on expiry.
- `Network.WebApp.Extra.Middleware.ValidateHeaders` — validate response
  headers against HTTP spec constraints.
- `Network.WebApp.Extra.Middleware.Vhost` — route requests to different
  applications by `Host` header.
- `Network.WebApp.Extra.Middleware.Push.Referer.LRU` — a list-backed LRU
  cache (`empty`/`lookup`/`insert`/`size`) for push predictions.
- `Network.WebApp.Extra.Middleware.Push.Referer.ParseURL` — `extractPath`/
  `isStaticResource` for Referer-header analysis.
- `Network.WebApp.Extra.Middleware.Push.Referer.Types` — `PushPath`/
  `PushEntry`/`PushSettings`.
- `Network.WebApp.Extra.Middleware.Push.Referer.Manager` — `PushManager`:
  learns page → resource associations from Referer headers, capped by
  `maxPushesPerPage` and LRU-evicted by `maxEntries`.
- `Network.WebApp.Extra.Middleware.Push.Referer` — `pushOnReferer`: HTTP/2
  server-push-via-Referer prediction, injecting `Link: …; rel=preload`
  response headers for learned resources.
- `Network.WebApp.Logger` — Apache Combined Log Format: `apacheFormat`/
  `apacheFormatWithDate` (pure) and `ApacheLogger.log` (IO, via a
  `getDate`/`output` callback pair).

### `Network.WebApp.Server` — a WAI-style HTTP server

A ground-up HTTP/1.1 server for `Network.WebApp.Application`s, over both
blocking sockets and the `EventDispatcher`/`Green` non-blocking runtime.
Named `Server` rather than the Haskell-specific `Warp`, matching this
project's `Network.HTTP2.Server`/`Network.HTTP3.Server`/`Network.QUIC.Server`
convention.

- `Network.WebApp.Server.Types` — connection/error types shared across the
  server.
- `Network.WebApp.Server.Settings` — `Settings`/`defaultSettings`: port,
  host, timeouts, backlog, graceful-shutdown timeout, auto `Date`/`Server`
  headers.
- `Network.WebApp.Server.Request` — HTTP request-line and header parsing off
  a raw socket buffer, producing a `Network.WebApp.Request`.
- `Network.WebApp.Server.Response` — status-line/header rendering and
  response transmission, both blocking (`sendResponse`) and event-driven
  (`sendResponseEL`).
- `Network.WebApp.Server.Run` — `runSettings`/`runSettingsEventLoop`: the
  accept loop and per-connection request/response cycle, keep-alive aware.
- `Network.WebApp.Server.Conduit` — `ISource`: buffered incremental body
  reading for known-length and chunked request bodies.
- `Network.WebApp.Server.IO` — low-level connection byte-sending helpers.
- `Network.WebApp.Server.SendFile` — portable `sendFile` response body
  transmission.
- `Network.WebApp.Server.WithApplication` — `withApplication`/
  `withApplicationSettings`: run an `Application` on an OS-assigned free
  port for the duration of an action — reads the real bound port back via
  `Network.Socket.getSockName` (a correctness fix over the upstream source,
  which always passed port `0` to the callback).
- `Network.WebApp.Server` — the package aggregator plus `run`, a one-line
  entry point (`run port app`).
- `Network.WebApp.Server.QUIC` — bridges `Network.WebApp` to HTTP/3 over
  `Network.QUIC` + `Network.HTTP3`; TLS 1.3 is mandatory, so `certFile`/
  `keyFile` are required settings (not `Option`).
- `Network.WebApp.Server.TLS` — HTTPS support via `Network.TLS.Context`
  (OpenSSL FFI) over the `EventDispatcher`/`Green` runtime, with configurable
  insecure-connection handling and optional ALPN negotiation.
- `Network.WebApp.Server.TLS.Internal` — re-exports `Server.TLS` for advanced
  usage.
- `Network.WebApp.Server.WebSockets` — upgrades `Network.WebApp` requests to
  `Network.WebSockets` connections via `responseRaw`, with a fallback path
  for non-WebSocket requests (`websocketsApp`/`websocketsOr`).

### `Network.WebSockets` — WebSocket protocol support (RFC 6455)

- `Network.WebSockets.Types` — `Opcode`/`CloseCode`/`ConnectionState`/
  `ConnectionOptions`/`Connection`/`PendingConnection`/`ServerApp`.
- `Network.WebSockets.Frame` — frame encoding/decoding: FIN/opcode byte,
  7/16/64-bit payload-length thresholds, and XOR masking (its own inverse).
- `Network.WebSockets.Handshake` — the RFC 6455 §4 upgrade handshake:
  `computeAcceptKey`/`isValidHandshake`/`buildHandshakeResponse`. The SHA-1
  step is an honest non-functional placeholder (documented `TODO`,
  no SHA-1 in the Lean stdlib) — not production-ready as-is.
- `Network.WebSockets.Connection` — `mkConnection`: frames outgoing
  text/binary/close/ping messages and auto-responds to incoming pings.
- `Network.WebSockets.Client` — `runClient`: outbound (client-side)
  connections, performing the RFC 6455 §4.1 opening handshake over a plain
  TCP connection.

### `CDP` — Chrome DevTools Protocol client

A port of [`cdp-hs`](https://github.com/arsalan0c/cdp-hs) (see
[`docs/imports/cdp/dependencies.md`](imports/cdp/dependencies.md)): typed
commands/events/types for every CDP domain, plus a WebSocket-based runtime to
connect to a browser, send commands, and subscribe to events.

- `CDP.Definition` — the protocol's own JSON-Schema-style self-description
  (`Domain`/`Command`/`Event`/`TypeDef`), as served by a live browser's
  `/json/protocol` endpoint.
- `CDP.Internal.Utils` — shared runtime scaffolding: `Config`/`Handle`,
  `SessionId`/`CommandId`, `ProtocolError`, and the `Command`/`Event` classes
  every domain module instantiates.
- `CDP.Domains.CacheStorage` / `.Cast` / `.DOMStorage` / `.Database` /
  `.DeviceOrientation` / `.EventBreakpoints` / `.HeadlessExperimental` /
  `.Input` / `.Inspector` / `.Media` / `.Memory` / `.Performance` /
  `.Debugger` / `.HeapProfiler` / `.IO` / `.IndexedDB` / `.SystemInfo` /
  `.Tethering` / `.WebAudio` / `.WebAuthn` / `.Tracing` / `.Profiler` /
  `.Log` / `.DOMDebugger` / `.PerformanceTimeline` / `.Animation` /
  `.LayerTree` / `.Audits` / `.CSS` / `.Overlay` / `.Accessibility` /
  `.DOMSnapshot` / `.Fetch` / `.ServiceWorker` / `.Storage` /
  `.BackgroundService` — one module per CDP domain, each a direct port of
  the corresponding upstream `CDP.Domains.*` module: parameter/response
  structures for every command, payload structures for every event, and the
  domain's own types, with `ToJSON`/`FromJSON` and `Command`/`Event`
  instances.
- `CDP.Domains.Runtime` — the `Runtime` domain (JavaScript remote evaluation
  and mirror objects); also the module every other domain's cross-references
  to `RemoteObject`/`RemoteObjectId` resolve against.
- `CDP.Domains.DOMPageNetworkEmulationSecurity` — the `DOM`, `Emulation`,
  `Network`, `Page` and `Security` domains, bundled into one module as
  upstream does (they're mutually referential) and separated into nested
  namespaces.
- `CDP.Domains.BrowserTarget` — the `Browser` and `Target` domains, likewise
  bundled as upstream does.
- `CDP.Domains` — the package aggregator: re-exports all 39 domain modules
  above with a single import.
- `CDP.Endpoints` — the browser's HTTP discovery endpoints
  (`/json/version`, `/json/list`, `/json/new`, `/json/activate/…`,
  `/json/close/…`, `/json/protocol`) plus `connectToTab`/`browserAddress`/
  `pageAddress` to resolve a target's WebSocket debugger URL.
- `CDP.Runtime` — the client runtime: `runClient` opens the WebSocket
  connection and drives a dispatch loop that routes incoming frames to
  either a pending command's result (via `MVar`-backed promises) or a
  subscribed event handler; `sendCommand`/`sendCommandWait` and
  `subscribe`/`unsubscribe` are the client-facing API.

### `Data.Word8` — ASCII byte classification

`isUpper`/`isLower`/`isAlpha`/`isDigit`/`isAlphaNum`/`isSpace`/`isControl`/
`isPrint`/`isHexDigit`/`isOctDigit`/`isAscii`, `toLower`/`toUpper`, and named
byte constants (`_A`.._Z`, `_a`.._z`, `_0`.._9`, punctuation), with
`native_decide`-proved idempotency and classification/conversion coherence
over all 256 `UInt8` values.

## Module Table

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
| `Linen.Data.Conduit.Internal.Conduit` | `ConduitT` CPS wrapper over `Pipe`: `await`/`yield`/`leftoverC`/`awaitForever`, `.\|` fusion, `runConduit`/`runConduitRes`, `bracketP` (`unsafe`) |
| `Linen.Data.Conduit.Combinators` | conduit's combinator library over `ConduitT`: sources/sinks/transformers (`sourceList`/`sinkList`/`mapC`/`filterC`/`takeC`/`foldMC`/…) (`unsafe`) |
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
| `Linen.Data.Scientific` | arbitrary-precision `coefficient * 10^exponent`: `normalize`/`toRealFloat`/`toBoundedInteger` + `Add`/`Sub`/`Mul`/`Neg`/`Ord` + laws |
| `Linen.Data.IP` | IPv4/IPv6 addresses, CIDR `AddrRange` (bounded mask proof) with `isMatchedTo`, `parseIPv4`/`parseCIDR4` |
| `Linen.Data.String` | `IsString` class + `String.words`/`unwords`/`unlines` (`lines` is core's `splitOn`) |
| `Linen.Data.Text` | Haskell-compatible `Data.Text` API (`Text := String`): `chunksOf`/`isInfixOf`/`transpose`/… over Lean's UTF-8 `String`, no fuel counters |
| `Linen.Data.Text.Encoding` | `Text`↔`ByteString` UTF-8 codec: `encodeUtf8`/`decodeUtf8'` (via `String.fromUTF8?`), `decodeUtf8With` (well-founded byte scanner, ≥1 byte consumed per step) |
| `Linen.Data.Time.Clock` | UTC time/durations: `NominalDiffTime` (`Int` nanoseconds, `Add`/`Sub`/`Neg`/`Ord`), `UTCTime` (`getCurrentTime` via `IO.monoNanosNow`, `diffUTCTime`/`addUTCTime`) |
| `Linen.Data.Traversable` | `Traversable` class (`traverse`/`sequence`) + `List`/`Option`/`NonEmpty`; `LawfulTraversable` |
| `Linen.Data.Unique` | globally unique ids: `newUnique : IO Unique` from a global counter (`BEq`/`Ord`/`Hashable`) |
| `Linen.Data.Vault` | type-safe heterogeneous map: `Key α` tokens (unique, `IO`-minted) over a `Std.HashMap Nat Erased`, `insert`/`lookup`/`delete`/`newKey` |
| `Linen.Data.Vector` | the handful of `Data.Vector` combinators `Array` lacks: `generate`/`ifilter`/`foldl1'`/`ifoldl'`/`and`/`or`/`product`/`notElem`/`backpermute`/`slice` |
| `Linen.Data.Void` | vacuous `Empty` instances (`BEq`/`Ord`/`Hashable`/`ToString`) + `Empty → α` singleton law |
| `Linen.Control.Applicative` | `asum` |
| `Linen.Control.Monad` | `join`, `replicateM`, `replicateM_`, `when`, `unless` |
| `Linen.Control.Monad.Except` | `mtl` names over core `ExceptT`/`Except`: `throwError`, `catchError`, `liftEither`, `mapExceptT`, `withExceptT`, `runExceptT` |
| `Linen.Control.Monad.Reader` | `Reader` alias + `mtl` names over core `ReaderT`/`read`/`adapt`: `ask`, `asks`, `local`, `runReaderT`, `runReader`, `mapReaderT` |
| `Linen.Control.Monad.State` | `State` alias + `mtl` names over core `StateT`: `put`, `gets`, `runStateT`, `evalStateT`, `execStateT`, `runState`, `evalState`, `execState` (`get`/`set`/`modify` reused as-is) |
| `Linen.Control.Monad.Trans` | `mtl` name `lift` over core `MonadLift`/`monadLift`, plus generic `lift_pure`/`lift_bind` laws (no bespoke `MonadTrans` class or per-transformer instances) |
| `Linen.Control.Monad.Trans.Resource` | `ResourceT` (deterministic LIFO cleanup) over core `ReaderT`: `allocate`/`release`/`runResourceT`, `releaseKey_eq` |
| `Linen.Control.Monad.IO.Unlift` | `MonadUnliftIO` (CPS `withRunInIO` over `MonadLiftT IO m`) + `toIO`/`liftIOOp`, `IO`/`ReaderT r IO` instances |
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
| `Linen.Control.Monad.STM` | STM = `BaseIO (STMResult _)`, global-mutex-serialized: `atomically`/`retry`/`orElse`/`check` |
| `Linen.Control.Concurrent.STM.TVar` | transactional variable over `IO.Ref`: `newTVarIO`/`readTVar`/`writeTVar`/`modifyTVar'` |
| `Linen.Control.Concurrent.STM.TMVar` | `TVar (Option α)`: `takeTMVar`/`putTMVar`/`readTMVar`/`tryTakeTMVar`/`tryPutTMVar`/`isEmptyTMVar` |
| `Linen.Control.Concurrent.STM.TQueue` | transactional two-list FIFO: `writeTQueue`/`readTQueue`/`tryReadTQueue`/`isEmptyTQueue`/`peekTQueue` |
| `Linen.Data.Json` | JSON AST, `ToJSON`/`FromJSON`, encode/decode + roundtrip proofs |
| `Linen.System.Console.Ansi` | ANSI terminal colors and styles |
| `Linen.System.Exit` | `ExitCode` (success/failure) + `exitWith`/`exitSuccess`/`exitFailure` over `IO.Process.exit` |
| `Linen.System.Log.FastLogger` | buffered thread-safe logger (`Std.Mutex`): `newLoggerSet`/`pushLogStr`/`flushLogStr`/`withFastLogger` |
| `Linen.System.TimeManager` | connection-timeout sweeper: `Manager` (dedicated-task cooperative-cancellation loop over `Std.CancellationToken`), `Handle.tickle`/`cancel`/`pause`/`resume` |
| `Linen.System.Posix.Compat` | minimal POSIX compatibility: `Fd`/`closeFd`, `FileStatus`/`getFileStatus`/`fileExist` over `System.FilePath.metadata` |
| `Linen.Network.HTTP.Chunked` | HTTP/1.1 chunked transfer encoding over `ByteArray` (`chunkedTransferEncoding`/`encodeChunked`) |
| `Linen.Network.HTTP.Date` | HTTP date parsing/formatting (RFC 7231): `HTTPDate`, `parseHTTPDate` (IMF-fixdate/asctime), `formatHTTPDate` |
| `Linen.Network.HTTP.Types.Header` | case-insensitive `HeaderName` (`CI String`), `Header`/`RequestHeaders`/`ResponseHeaders`, ~50 standard header constants |
| `Linen.Network.HTTP.Types.Method` | `StdMethod`/`Method`, `parseMethod`/`renderMethod`, RFC 9110 `isSafe`/`isIdempotent` + laws |
| `Linen.Network.HTTP.Types.Status` | proof-carrying `Status` (100–999), ~50 codes + aliases, class predicates, RFC 9110 `mustNotHaveBody` + theorems |
| `Linen.Network.HTTP.Types.URI` | query strings (`parseQuery`/`renderQuery`) + percent-encoding (`urlEncode`/`urlDecode`) |
| `Linen.Network.HTTP.Types.Version` | `HttpVersion` (major/minor), lexicographic `Ord`, `http09`/`http10`/`http11`/`http20` |
| `Linen.Network.HTTP.Client.Types` | HTTP/1.1 client types: transport `Connection` (TCP/TLS callbacks), `Request`, `Response` (`findHeader`/`contentLength`/`isSuccess`) |
| `Linen.Network.HTTP.Client.Request` | HTTP/1.1 request serialization (`serializeRequest` — auto Host/Content-Length/Connection) + `sendRequest` |
| `Linen.Network.HTTP.Client.Response` | HTTP/1.1 response parsing (`receiveResponse`/`performRequest`): status/headers + Content-Length/chunked/until-close bodies |
| `Linen.Network.HTTP.Client.Connection` | HTTP/1.1 client connections: `connect` (TCP/TLS), `defaultPort`, `Connection.connClose` |
| `Linen.Network.HTTP.Client.Redirect` | HTTP redirect following: `executeWithRedirects` (bounded hop count, relative/absolute `Location` resolution) |
| `Linen.Network.HTTP.Client.Conduit` | conduit bridge for HTTP client bodies: `httpSource`/`httpSink` (streaming, `unsafe` via `ConduitT`), `withResponse` |
| `Linen.Network.HTTP.Simple` | `http-conduit`-style convenience client: `parseUrl`/`parseUrl!`, `simpleHttp`/`httpBS`/`httpLbs` |
| `Linen.Network.HTTP.Req` | type-safe `req` client (`req`/library): phantom `Scheme`-indexed `Url`/`ReqOption` (HTTPS-only auth), `HttpMethod`/`HttpBody`/`HttpBodyAllowed` compile-time method-body constraints, `Req` monad, `runReq` |
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
| `Linen.Network.Socket` | safe phantom-typed lifecycle API, `withSocket`/`listenTCP`/`withEventLoop`, `EventLoop`, `sendAll`/`sendTo`/`recvFrom` |
| `Linen.Network.Socket.EventDispatcher` | kqueue/epoll → `Green` bridge: `waitReadable`/`waitWritable`/`recvGreen`/`sendAllGreen` |
| `Linen.Network.Socket.Blocking` | blocking-style `accept`/`connect`/`send`/`sendAll`/`recv` over the non-blocking API, retrying on `wouldBlock` |
| `Linen.Network.Sendfile` | portable `sendFile`/`sendFileSimple` (chunked read + `Blocking.sendAll`, no zero-copy syscall) |
| `Linen.Data.Streaming.Network` | `AppData`, `bindPortTCP`/`getSocketTCP`/`mkAppData`/`runTCPServer`, `acceptSafe` (retry loop, no `partial`) |
| `Linen.Network.Mime` | MIME lookup (`mime-types`): `defaultMimeMap`, `fileNameExtensions`, `mimeByExt`, `defaultMimeLookup` |
| `Linen.Network.URI` | RFC 3986 URI parsing/rendering/resolution (`network-uri`): `parseURI`, percent-encoding, `relativeTo`/`relativeFrom` |
| `Linen.Web.Cookie` | RFC 6265 cookie parse/render: `parseCookies`/`renderCookies`, `SetCookie` + `parseSetCookie`/`renderSetCookie` |
| `Linen.Web.Css` | typed CSS: `private`-constructor `Declaration`/`FontWeight` (`by decide`-bounded), `Length`/`Color`/`Selector`/`Rule`/`Stylesheet`, `rule!` macro |
| `Linen.Web.Html` | typed HTML5: `Category`-indexed `Html` encodes the content model at compile time; `private`-constructor `Attr`, `elem!` macro |
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
| `Linen.Crypto.JOSE.FFI` | `@[extern]` OpenSSL bindings: HMAC, RSA/EC verify, JWK→DER key build, base64url (`ffi/jose.c`) |
| `Linen.Crypto.JOSE.Types` | JOSE/JWT/JWK types: `JWSAlgorithm`/`ECCurve`/`JWKKeyType`, proof-carrying `JWK`, `ClaimsSet`, `JWSHeader`, `JwtError` + laws |
| `Linen.Crypto.JOSE.JWK` | JWK helpers: `parseOctKey` (base64url), `toDerPublicKey` (RSA/EC → DER via OpenSSL) |
| `Linen.Crypto.JOSE.JWS` | JWS compact verification (RFC 7515): `splitCompact`, `verifySignature` (HMAC/RSA/EC via OpenSSL) |
| `Linen.Crypto.JOSE.JWT` | JWT verification (RFC 7519): `validateClaims` (exp/nbf/aud/iss, bounded skew), `verifyJWT` (signature + claims) |
| `Linen.Options.Applicative.Types` | `optparse-applicative` core types: `ReadM`, `Mod` (right-biased modifier monoid), `InfoMod`/`ParserInfo`, `OptDescr`, functional `Parser` |
| `Linen.Options.Applicative.Builder` | builder API: `option`/`strOption`/`switch`/`flag`/`flag'`/`argument`/`subparser`, `Pure`/`Functor`/`Seq`/`Applicative`/`OrElse` for `Parser`, `withDefault`/`command` |
| `Linen.Options.Applicative.Extra` | execution API: `renderHelp`, `helper`, `info`, `hsubparser`, `execParser`, `execParserPure` |
| `Linen.PostgREST.ApiRequest.Preferences` | HTTP `Prefer` header parsing: `PreferCount`/`PreferReturn`/`PreferResolution`/`PreferTransaction`/`PreferMissing`/`PreferHandling`, `Preferences`, `parsePreferences` |
| `Linen.PostgREST.Auth.Types` | `AuthResult` (role + non-emptiness proof + JWT claims), `AuthResult.lookupClaim` |
| `Linen.PostgREST.Auth` | auth middleware: `extractBearerToken`, `findAuthHeader`, `extractRole`, `authenticate` |
| `Linen.PostgREST.Cache.Sieve` | SIEVE cache eviction: `SieveCache`, `create`/`lookup`/`insert`/`remove`/`size` |
| `Linen.PostgREST.Config.JSPath` | JWT claim path parsing: `JSPath`, `JSPath.parse`, `follow`/`followNested`, `defaultRoleClaimPath` |
| `Linen.PostgREST.Config.PgVersion` | PostgreSQL version parsing: `PGVersion`, `fromVersionNum`/`toVersionNum`/`parse`, `isSupported`/`isAtLeastMajor`/`isAtLeast` |
| `Linen.PostgREST.Config.Proxy` | proxy configuration: `UriScheme`, `ProxyUri`, `ProxyUri.parse`/`toUri`, `openApiServerUrl` |
| `Linen.PostgREST.Cors` | CORS middleware: `corsHeaders`, `preflightHeaders`, `defaultExposedHeaders`/`defaultAllowedHeaders` |
| `Linen.PostgREST.Debounce` | debounce utility: `Debouncer`, `Debouncer.create`/`run` |
| `Linen.PostgREST.Listener` | LISTEN/NOTIFY handling: `pgrstChannel`, `listenSql`, `NotificationAction`, `parseNotification` |
| `Linen.PostgREST.Logger` | structured logging: `LogLevel`, `log`, `logCrit`/`logError`/`logWarn`/`logInfo`/`logDebug` |
| `Linen.PostgREST.MediaType` | content-type negotiation: `MediaType`, `toMime`/`toContentType`/`ofMime`/`isJSON`/`isText` |
| `Linen.PostgREST.Network` | server binding helpers: `resolveHost` |
| `Linen.PostgREST.RangeQuery` | HTTP range pagination: `NonnegRange`, `parseRange`, `ContentRange`, `contentRangeHeader`, `ContentRange.fromNonnegRange` |
| `Linen.PostgREST.Response` | HTTP response construction: `contentRangeHeader`, `readHeaders`/`mutateHeaders`, `readStatus`, `readStatus_valid` |
| `Linen.PostgREST.Response.GucHeader` | GUC-to-HTTP-header mapping: `gucHeaderPrefix`, `gucStatusVar`, `parseGucHeaders`, `parseGucStatus` |
| `Linen.PostgREST.Response.Performance` | performance timing headers: `serverTimingHeader`, `serverTimingValue`, `timingHeaders` |
| `Linen.PostgREST.SchemaCache.Identifiers` | schema-qualified identifiers: `QualifiedIdentifier`, `escapeIdent`/`quoteIdent`/`quoteQi`/`toQi`, `RelIdentifier` |
| `Linen.PostgREST.ApiRequest.Types` | API-request domain model: `Action`, `Filter`, `LogicTree`, `OrderTerm`, `SelectItem`, `Payload`, `Target` |
| `Linen.PostgREST.Config` | application configuration: `AppConfig`, `LogLevel`, `OpenAPIMode`, refined `Port` |
| `Linen.PostgREST.Config.Database` | database connection helpers: `DbUriParts`, `toUri`, `searchPathSql`/`searchPathDisplay`, `setRoleSql`/`resetRoleSql`, `TxMode`/`TxEnd` |
| `Linen.PostgREST.Error.Types` | PostgREST error hierarchy: `ApiRequestError`, `JwtError`, `PgError`, `SchemaCacheError`, `Error`, `toHttpStatus` |
| `Linen.PostgREST.Error` | error response rendering: `errorPayload` (JSON body), `errorHeaders` (`Content-Type`/`WWW-Authenticate`) |
| `Linen.PostgREST.MainTx` | per-request transaction wrapper: `sqlLit`, `setSearchPath`, `setRole`, `setRequestContext`, `preRequestSql` |
| `Linen.PostgREST.Plan.Types` | resolved query-plan types: `CoercibleField`, `AggregateFunction`, `CoercibleFilter`, `CoercibleLogicTree`, `CoercibleOrderTerm`, `ConflictAction` |
| `Linen.PostgREST.Query.SqlFragment` | SQL fragment builder: `pgFmtIdent`/`pgFmtLit`, `pgFmtField`, `pgFmtFilter`, `pgFmtLogicTree`, `pgFmtOrderTerm`, `asJsonF`/`asJsonSingleF` |
| `Linen.PostgREST.SchemaCache.Relationship` | foreign-key relationships: `Cardinality`, `Relationship`, `localColumns`/`foreignColumns` |
| `Linen.PostgREST.Plan.ReadPlan` | resolved SELECT query plan: `ReadPlan` (recursive via embedded `rpRelationships`), `hasEmbeds`/`embedCount`/`hasFilters`/`hasOrdering` |
| `Linen.PostgREST.Plan.MutatePlan` | resolved INSERT/UPDATE/DELETE plans: `MutatePlan`, `targetTable`, `returningFields`, `hasReturning` |
| `Linen.PostgREST.SchemaCache.Representations` | PostgreSQL type casts: `Representation`, `MediaHandler` |
| `Linen.PostgREST.SchemaCache.Routine` | function/procedure metadata: `Routine`, `Volatility`, `RoutineParam`, `RoutineReturnType`, `isSafeForGet` |
| `Linen.PostgREST.Plan.CallPlan` | resolved RPC call plan: `CallPlan`, `routineQi`, `isSetof`, `isSafeForGet`, `paramCount` |
| `Linen.PostgREST.SchemaCache.Table` | table/column metadata: `Column`, `Table` (proof-carrying `pk_subset`), `findColumn`, `hasPrimaryKey` |
| `Linen.PostgREST.SchemaCache` | schema introspection cache: `SchemaCache`, `findTable`/`findRelationships`/`findRoutines`, catalog SQL literals |
| `Linen.PostgREST.AppState` | shared mutable app state: `AppState`, `Observation`, `Metrics`, `create`/`getSchemaCache`/`putSchemaCache`/`observe` |
| `Linen.PostgREST.Metrics` | Prometheus text exposition: `renderMetrics` |
| `Linen.PostgREST.Admin` | admin HTTP server: `handleAdminRequest` (`/live`, `/ready`, `/metrics`) |
| `Linen.PostgREST.Observation` | observability events: `defaultObserver` (stderr logging) |
| `Linen.PostgREST.TimeIt` | IO timing: `timeIt`, `timeIt_` |
| `Linen.PostgREST.Unix` | Unix socket handling: `defaultSocketMode` |
| `Linen.PostgREST.Version` | version constants: `version`, `prettyVersion` |
| `Linen.PostgREST.App` | core request handler: `handleRequest`, `SimpleRequest`/`SimpleResponse`, `printBanner` |
| `Linen.PostgREST.CLI` | command-line parsing: `Command`, `parseArgs`, `printUsage` |
| `Linen.PostgREST.Response.OpenAPI` | OpenAPI 3.0 spec generation: `pgTypeToOpenAPI`, `columnSchema`, `generateOpenAPISpec` |
| `Linen.Network.TLS.Types` | `TLSVersion` (`tls10`–`tls13`), `CipherID`, `TLSOutcome` (`.ok`/`.wantRead`/`.wantWrite`/`.error`) |
| `Linen.Network.TLS.Context` | OpenSSL `SSL_CTX`/`SSL` FFI (`ffi/tls.c`): `createContext`/`acceptSocket`/`read`/`write`/`getVersion`/`getAlpn`, `createClientContext(WithCA)`/`connectSocket` |
| `Linen.Network.QUIC.Types` | QUIC (RFC 9000) core types: proof-carrying `ConnectionId`, `Version`, `TransportParams`, `StreamId`, `TransportError`, `TLSConfig` |
| `Linen.Network.QUIC.Config` | `ServerConfig`/`ClientConfig` with TLS, transport-parameter, and host/port defaults |
| `Linen.Network.QUIC.Connection` | opaque `Connection` handle, `ConnectionState`; stream/close/state ops stubbed pending TLS 1.3 FFI |
| `Linen.Network.QUIC.Client` | `connect : ClientConfig → IO Connection`, stubbed pending TLS 1.3 FFI |
| `Linen.Network.QUIC.Server` | `run`/`accept : ServerConfig → IO Connection`, stubbed pending TLS 1.3 FFI |
| `Linen.Network.QUIC.Stream` | `QUICStream` (`Connection` + `StreamId`): `send`/`recv`/`close` |
| `Linen.Network.HTTP3.Server` | HTTP/3 request/response layer: `H3Request`, `H3Response`, `H3Handler`, `sendResponse`, `handleRequestStream`, `handleConnection` |
| `Linen.Network.WebApp.Internal` | core WAI-style types: `Request`/`Response`/`Application`/`Middleware`, `AppM .pending .sent` exactly-once-respond monad |
| `Linen.Network.WebApp` | `responseLBS`/`responseFile'`/`responseStream'`, `getRequestBodyChunk`/`strictRequestBody`, `defaultRequest`, middleware-algebra laws |
| `Linen.Network.WebApp.Static.Types` | `Piece` (traversal-safe path segment), `StaticSettings` |
| `Linen.Network.WebApp.Static.Storage.Filesystem` | `defaultFileServerSettings`: filesystem-backed static storage |
| `Linen.Network.WebApp.Static.Application` | `staticApp`/`static`: `Application` serving files per `StaticSettings` |
| `Linen.Network.WebApp.Extra.Header` | request header convenience queries |
| `Linen.Network.WebApp.Extra.Request` | request convenience queries (path/method/content-type) |
| `Linen.Network.WebApp.Extra.UrlMap` | dispatch to sub-applications by path prefix |
| `Linen.Network.WebApp.Extra.Middleware.AcceptOverride` | override `Accept` header from a query-string parameter |
| `Linen.Network.WebApp.Extra.Middleware.AddHeaders` | add fixed headers to every response |
| `Linen.Network.WebApp.Extra.Middleware.Autohead` | convert `HEAD` to `GET`, strip response body |
| `Linen.Network.WebApp.Extra.Middleware.CleanPath` | normalize double/trailing slashes, redirect to canonical path |
| `Linen.Network.WebApp.Extra.Middleware.CombineHeaders` | merge duplicate response headers, comma-joined |
| `Linen.Network.WebApp.Extra.Middleware.ForceDomain` | redirect to a canonical domain |
| `Linen.Network.WebApp.Extra.Middleware.ForceSSL` | redirect HTTP to HTTPS |
| `Linen.Network.WebApp.Extra.Middleware.HealthCheckEndpoint` | health-check endpoint returning 200 OK without hitting the app |
| `Linen.Network.WebApp.Extra.Middleware.Local` | restrict access to localhost |
| `Linen.Network.WebApp.Extra.Middleware.MethodOverride` | override HTTP method from a query-string parameter |
| `Linen.Network.WebApp.Extra.Middleware.MethodOverridePost` | override HTTP method from a POST body's `_method` field |
| `Linen.Network.WebApp.Extra.Middleware.Rewrite` | rewrite request paths by custom rule |
| `Linen.Network.WebApp.Extra.Middleware.Routed` | apply a middleware only to requests matching a path predicate |
| `Linen.Network.WebApp.Extra.Middleware.Select` | conditionally apply a middleware |
| `Linen.Network.WebApp.Extra.Middleware.StreamFile` | convert `.responseFile` to `.responseStream` |
| `Linen.Network.WebApp.Extra.Middleware.StripHeaders` | remove specified headers from responses |
| `Linen.Network.WebApp.Extra.Middleware.Timeout` | enforce a processing timeout, returning 503 on expiry |
| `Linen.Network.WebApp.Extra.Middleware.ValidateHeaders` | validate response headers against HTTP spec constraints |
| `Linen.Network.WebApp.Extra.Middleware.Vhost` | route requests to different applications by `Host` header |
| `Linen.Network.WebApp.Extra.Middleware.HttpAuth` | HTTP Basic Authentication |
| `Linen.Network.WebApp.Extra.Middleware.RequestSizeLimit` | reject over-limit request bodies without buffering the excess |
| `Linen.Network.WebApp.Extra.Middleware.RealIp` | update `remoteHost` from `X-Forwarded-For`/`X-Real-IP` |
| `Linen.Network.WebApp.Extra.Middleware.Jsonp` | wrap JSON responses in a callback for cross-origin requests |
| `Linen.Network.WebApp.Extra.Middleware.Approot` | detect application root URL from headers/configuration |
| `Linen.Network.WebApp.Extra.Middleware.RequestLogger` | request logging, Apache Combined or colorized dev format |
| `Linen.Network.WebApp.Extra.Middleware.Gzip` | gzip `Accept-Encoding` negotiation |
| `Linen.Network.WebApp.Extra.Middleware.RequestLogger.JSON` | structured JSON request logging |
| `Linen.Network.WebApp.Extra.Middleware.RequestSizeLimit.Internal` | re-exports `RequestSizeLimit` |
| `Linen.Network.WebApp.Extra.Parse` | URL-encoded form body parsing |
| `Linen.Network.WebApp.Extra.Test` | simulated testing harness: `SRequest`/`SResponse`/`runSession`/`get`/`post` |
| `Linen.Network.WebApp.Extra.Test.Internal` | re-exports `Test` |
| `Linen.Network.WebApp.Extra.EventSource` | Server-Sent Events: `ServerEvent`/`render`/`eventSourceApp` |
| `Linen.Network.WebApp.Extra.EventSource.EventStream` | SSE framing: `dataEvent`/`namedEvent`/`retryEvent`/`commentEvent` |
| `Linen.Network.WebApp.Extra.Middleware.Push.Referer.LRU` | list-backed LRU cache: `empty`/`lookup`/`insert`/`size` |
| `Linen.Network.WebApp.Extra.Middleware.Push.Referer.ParseURL` | `extractPath`/`isStaticResource` for Referer analysis |
| `Linen.Network.WebApp.Extra.Middleware.Push.Referer.Types` | `PushPath`/`PushEntry`/`PushSettings` |
| `Linen.Network.WebApp.Extra.Middleware.Push.Referer.Manager` | `PushManager.new`/`.record`/`.getPushes`, LRU-evicted |
| `Linen.Network.WebApp.Extra.Middleware.Push.Referer` | `pushOnReferer`: HTTP/2 push-via-Referer prediction |
| `Linen.Network.WebApp.Logger` | `apacheFormat`/`apacheFormatWithDate`/`ApacheLogger.log` |
| `Linen.Network.WebApp.Server.Types` | connection/error types shared across the server: `InvalidRequest`, `Transport`, `Source`, `Connection` |
| `Linen.Network.WebApp.Server.Counter` | atomic request counter used for keep-alive/`X-Request-Id`-style bookkeeping |
| `Linen.Network.WebApp.Server.HashMap` | small case-insensitive header multimap helper |
| `Linen.Network.WebApp.Server.Header` | header rendering/lookup helpers shared by `Request`/`Response` |
| `Linen.Network.WebApp.Server.ReadInt` | fast byte-buffer integer parsing (chunk sizes, `Content-Length`) |
| `Linen.Network.WebApp.Server.PackInt` | integer-to-`ByteArray` encoding for chunked transfer framing |
| `Linen.Network.WebApp.Server.Date` | cached HTTP-date string generation for the `Date` response header |
| `Linen.Network.WebApp.Server.Settings` | `Settings`/`defaultSettings`: port, host, timeouts, hooks |
| `Linen.Network.WebApp.Server.Response` | status-line/header rendering and `sendResponse`/`sendResponseEL` connection writers |
| `Linen.Network.WebApp.Server.Request` | HTTP request-line and header parsing off a buffered socket reader |
| `Linen.Network.WebApp.Server.Conduit` | `ISource`: buffered incremental body reading (`mkKnown`/`mkChunked`) |
| `Linen.Network.WebApp.Server.IO` | low-level connection byte-sending helpers |
| `Linen.Network.WebApp.Server.SendFile` | portable `sendFile` response body streaming |
| `Linen.Network.WebApp.Server.Internal` | re-exports the server's public surface |
| `Linen.Network.WebApp.Server.Run` | `runSettings`/`runSettingsEventLoop`: the accept-loop/connection-handling core |
| `Linen.Network.WebApp.Server.WithApplication` | `withApplication`/`withApplicationSettings`: run a server for the duration of an `IO` action |
| `Linen.Network.WebApp.Server` | the package aggregator plus `run`, a one-line server entry point |
| `Linen.Network.WebApp.Server.QUIC` | bridges `Network.WebApp` to HTTP/3 over `Network.QUIC` |
| `Linen.Network.WebApp.Server.TLS` | HTTPS support via `Network.TLS.Context` |
| `Linen.Network.WebApp.Server.TLS.Internal` | re-exports `Server.TLS` for advanced use |
| `Linen.Network.WebApp.Server.WebSockets` | upgrades `Network.WebApp` requests to `Network.WebSockets` connections via `responseRaw` |
| `Linen.Network.WebSockets.Types` | `Opcode`/`CloseCode`/`ConnectionState`/`ConnectionOptions`/`Connection`/`PendingConnection`/`ServerApp` |
| `Linen.Network.WebSockets.Frame` | frame encoding/decoding: FIN/opcode byte, length variants, masking |
| `Linen.Network.WebSockets.Handshake` | the RFC 6455 §4 upgrade handshake: `computeAcceptKey`/`isValidHandshake`/`buildHandshakeResponse` |
| `Linen.Network.WebSockets.Connection` | `mkConnection`: frames outgoing sends, decodes/dispatches incoming frames |
| `Linen.Network.WebSockets.Client` | `runClient`: outbound (client-side) connections, RFC 6455 §4.1 opening handshake |
| `Linen.Network.WebSockets` | the package aggregator: `Types`/`Frame`/`Handshake`/`Connection`/`Client` |
| `Linen.Data.Word8` | ASCII byte classification (`isUpper`/`isDigit`/…), case conversion, named byte constants |
| `Linen.CDP.Definition` | the protocol's self-description: `Domain`/`Command`/`Event`/`TypeDef` |
| `Linen.CDP.Internal.Utils` | `Config`/`Handle`, `SessionId`/`CommandId`, `ProtocolError`, the `Command`/`Event` classes |
| `Linen.CDP.Domains.CacheStorage` | the `CacheStorage` CDP domain |
| `Linen.CDP.Domains.Cast` | the `Cast` CDP domain (Presentation API / Remote Playback API) |
| `Linen.CDP.Domains.DOMStorage` | the `DOMStorage` CDP domain |
| `Linen.CDP.Domains.Database` | the `Database` CDP domain |
| `Linen.CDP.Domains.DeviceOrientation` | the `DeviceOrientation` CDP domain |
| `Linen.CDP.Domains.EventBreakpoints` | the `EventBreakpoints` CDP domain |
| `Linen.CDP.Domains.HeadlessExperimental` | the `HeadlessExperimental` CDP domain |
| `Linen.CDP.Domains.Input` | the `Input` CDP domain: dispatching keyboard/mouse/touch events |
| `Linen.CDP.Domains.Inspector` | the `Inspector` CDP domain |
| `Linen.CDP.Domains.Media` | the `Media` CDP domain |
| `Linen.CDP.Domains.Memory` | the `Memory` CDP domain |
| `Linen.CDP.Domains.Performance` | the `Performance` CDP domain |
| `Linen.CDP.Domains.Runtime` | the `Runtime` CDP domain: JavaScript remote evaluation and mirror objects |
| `Linen.CDP.Domains.Debugger` | the `Debugger` CDP domain: breakpoints, stepping, stack traces |
| `Linen.CDP.Domains.HeapProfiler` | the `HeapProfiler` CDP domain |
| `Linen.CDP.Domains.IO` | the `IO` CDP domain: streams produced by DevTools |
| `Linen.CDP.Domains.DOMPageNetworkEmulationSecurity` | the `DOM`/`Emulation`/`Network`/`Page`/`Security` CDP domains, bundled as upstream does |
| `Linen.CDP.Domains.Accessibility` | the `Accessibility` CDP domain |
| `Linen.CDP.Domains.Animation` | the `Animation` CDP domain |
| `Linen.CDP.Domains.Audits` | the `Audits` CDP domain: page violations and possible improvements |
| `Linen.CDP.Domains.BrowserTarget` | the `Browser`/`Target` CDP domains, bundled as upstream does |
| `Linen.CDP.Domains.CSS` | the `CSS` CDP domain: stylesheet/rule/style read/write |
| `Linen.CDP.Domains.DOMDebugger` | the `DOMDebugger` CDP domain: breakpoints on DOM operations/events |
| `Linen.CDP.Domains.DOMSnapshot` | the `DOMSnapshot` CDP domain: DOM/layout/style document snapshots |
| `Linen.CDP.Domains.Fetch` | the `Fetch` CDP domain: substituting the browser's network layer |
| `Linen.CDP.Domains.IndexedDB` | the `IndexedDB` CDP domain |
| `Linen.CDP.Domains.LayerTree` | the `LayerTree` CDP domain: the tree of compositor layers |
| `Linen.CDP.Domains.Log` | the `Log` CDP domain: access to log entries |
| `Linen.CDP.Domains.Overlay` | the `Overlay` CDP domain: page overlays for highlighting/inspection |
| `Linen.CDP.Domains.PerformanceTimeline` | the `PerformanceTimeline` CDP domain |
| `Linen.CDP.Domains.Profiler` | the `Profiler` CDP domain: JavaScript CPU profiling |
| `Linen.CDP.Domains.ServiceWorker` | the `ServiceWorker` CDP domain |
| `Linen.CDP.Domains.BackgroundService` | the `BackgroundService` CDP domain |
| `Linen.CDP.Domains.Storage` | the `Storage` CDP domain: quota, cookies, and storage buckets |
| `Linen.CDP.Domains.SystemInfo` | the `SystemInfo` CDP domain: low-level system information |
| `Linen.CDP.Domains.Tethering` | the `Tethering` CDP domain: browser port binding |
| `Linen.CDP.Domains.Tracing` | the `Tracing` CDP domain |
| `Linen.CDP.Domains.WebAudio` | the `WebAudio` CDP domain |
| `Linen.CDP.Domains.WebAuthn` | the `WebAuthn` CDP domain |
| `Linen.CDP.Domains` | the package aggregator: re-exports all 39 `CDP.Domains.*` modules above |
| `Linen.CDP.Endpoints` | the browser's HTTP discovery endpoints (`/json/version`, `/json/list`, …), `connectToTab` |
| `Linen.CDP.Runtime` | the client runtime: `runClient`, `sendCommand`/`sendCommandWait`, `subscribe`/`unsubscribe` |
| `Linen.CDP` | the package aggregator: `CDP.Domains` + `CDP.Runtime` |
