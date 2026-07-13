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

### `Time` — the `time` package, over `Std.Time`

A port of Hackage's [`time`](https://hackage.haskell.org/package/time)
(v1.15), per [`docs/imports/Time/dependencies.md`](imports/Time/dependencies.md).
Lean's own `Std.Time` (ships with the pinned toolchain) already covers `time`'s
core job — Gregorian/ISO-week/ordinal calendar arithmetic, clocks, durations,
IANA-tzdata timezones, and locale-aware `strftime`-style formatting/parsing —
so this import is mostly a documented substitution; `Linen.Data.Time.Calendar`/
`.Clock`/`.LocalTime` (added ad hoc during the `sqlite-simple` import, before
`Std.Time` was known) are now rebuilt on `Std.Time.Date.PlainDate`/
`Std.Time.Duration`/`Std.Time.Zoned` respectively, fixing a bug where
`Data.Time.Clock.getCurrentTime` read a *monotonic* clock instead of real
wall-clock time. The bespoke `Linen.System.Time`/`ffi/time.c` wall-clock FFI
shim is retired outright — subsumed by `Std.Time.DateTime.Timestamp.now`.

- `Linen.Time.Calendar.CalendarDiffDays` — a `(months, days)` calendrical
  period, `Semigroup`/`Monoid` under addition, `calendarDay`/`calendarWeek`/
  `calendarMonth`/`calendarYear` constants, scale-by-integer.
- `Linen.Time.Calendar.Month` — an absolute month counter since a fixed
  origin, `addMonths`/`diffMonths`, and `DayPeriod`-style
  `periodFirstDay`/`periodLastDay`/`dayPeriod` relating it to
  `Std.Time.Date.PlainDate` — a standalone counter type `Std.Time`'s
  per-date `Month.Ordinal` field doesn't provide.
- `Linen.Time.Calendar.Quarter` — the same shape one level up: `QuarterOfYear`
  and an absolute `Quarter` counter, `addQuarters`/`diffQuarters`,
  `monthQuarter`/`dayQuarter`.
- `Linen.Time.Calendar.Julian` — the proleptic Julian calendar: its own
  leap-year rule (no Gregorian century correction), month lengths, and
  `addJulianMonthsClip`/`RollOver`/`addJulianYearsClip`/`RollOver` etc.
  arithmetic — a genuinely different calendar system from `Std.Time`'s
  Gregorian-only implementation.
- `Linen.Time.Calendar.Easter` — the Gregorian and Orthodox Easter-date
  algorithms (`gregorianEaster`/`orthodoxEaster`, `sundayAfter`), per
  Reingold & Dershowitz's *Calendrical Calculations*.
- `Linen.Time.CalendarDiffTime` — the time-valued sibling of
  `CalendarDiffDays`: `(months, Duration)` instead of `(months, days)`.
- `Linen.Time.UniversalTime` — `UT1` mean solar time as a
  Modified-Julian-Date-plus-fraction rational, with longitude-parameterised
  conversion to/from `Std.Time`'s civil wall-clock time — `Std.Time` only
  models UTC/civil time, never earth-rotation-based UT1.
- `Linen.Time.Clock.TAI` — `AbsoluteTime` (a TAI instant) and day-keyed
  leap-second-map conversions `utcToTAITime`/`taiToUTCTime`/`utcDayLength`
  (`LeapSecondMap = Day -> Option Int`, caller-supplied, matching upstream's
  own refusal to bundle a hardcoded leap-second table).

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

### `System.Keychain` — OS credential-store access

- Ports the Rust [`keyring`](https://crates.io/crates/keyring) crate
  (`keyring-rs`), Lean-ified: the crate name only makes sense as a registry
  identifier, so the stdlib's own `System.…` convention is used instead of
  mirroring it (`AGENTS.md`'s `WaiAppStatic` → `WebApp.Static` treatment).
- `Entry`/`Credential` façade over a small handle identifying a secret by
  `(service, account)`: `setPassword`/`getPassword`/`deleteCredential`
  (UTF-8 text) and `setSecret`/`getSecret` (raw bytes), dispatching in C
  (`ffi/keychain.c`, symbols `linen_keychain_*`) to whichever native store
  the platform provides — macOS Security.framework Keychain, Linux D-Bus
  Secret Service (via libsecret, only linked when its `.pc` file is
  present), or the Windows Credential Manager. Only the macOS backend is
  exercised by this repository's CI; Linux/Windows are written against the
  real APIs but unverified in this environment.
- All three operations raise a plain `IO.Error` on failure (matching every
  other native FFI module here), including on a missing entry — mirroring
  upstream's `Err(Error::NoEntry)` rather than degrading to `Option`.

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

### `Database.SQLite3` — SQLite3 bindings (`direct-sqlite`/`sqlite-simple`)

- `Database.SQLite3.Bindings.Types` — opaque `Database`/`Statement` handles
  (external objects wrapping `sqlite3*`/`sqlite3_stmt*`, same pattern as
  `LibPQ.Types`'s `PgConn`/`PgResult`), plus the `Error` result-code enum
  (`ofUInt32`/`toUInt32`, with an `.other` fallback for unrecognized/extended
  codes) and the `ColumnType`/`StepResult` enums.
- `Database.SQLite3.Bindings` — `@[extern]` bindings to the raw `sqlite3_*` C
  entry points (`ffi/sqlite3_shim.c`, linking the vendored amalgamation at
  `ffi/vendor/sqlite3/`): open/close/errmsg/interrupt/autocommit, `exec`,
  prepare/step/reset/finalize/clear-bindings, parameter/column metadata,
  binding values, reading columns, and the result-statistics counters.
- `Database.SQLite3.Direct` — decodes raw result codes into `Error` and
  returns `Except Error`, pairing each `Statement` with its owning
  `Database` so `errmsg` can be looked up without `sqlite3_db_handle`.
- `Database.SQLite3` — the public, `IO`-throwing API (`SQLError`, untyped
  `SQLData` values, `bind`/`bindNamed`, `columns`) built on `Direct`.

### `Database.SQLite.Simple` — `sqlite-simple`-style API

- `Database.SQLite.Simple.Types` — `Null` (deliberately never `BEq`-equal, to
  match `NULL`'s SQL semantics), `Query` (a `String` newtype with `Coe`/
  `ToString`/`Append`), `Only` (single-column row wrapper), and the
  heterogeneous row-cons `Cons`/`(:.)`.
- `Database.SQLite.Simple.Ok` — an accumulating-errors result type `Ok`
  (`.ok`/`.errors (Array String)`) with `Functor`/`Applicative`/`Alternative`/
  `Monad` instances (`Alternative` concatenates errors on double failure),
  `fail`, and `toExcept`/`ofExcept` with a round-trip theorem.
- `Database.SQLite.Simple.Time.Implementation` — parsing/rendering for
  SQLite's textual date/time formats via `Std.Internal.Parsec`: `parseDay`/
  `dayToString`, `parseUTCTime`/`utcTimeToString`, `timeZoneParser`/
  `timeZoneToString`, built on `dayAndTimeOfDayToUTCTime`/
  `utcTimeToDayAndTimeOfDay`. Ports only the textual forms upstream's
  `Data.Time.Format`-based parser accepts — not the Julian-day/Unix-epoch
  numeric forms `docs/imports/sqlite-simple/dependencies.md` describes, which
  checking upstream's actual source shows it never parses either.
- `Database.SQLite.Simple.Time` — thin re-export facade over
  `Time.Implementation`.
- `Database.SQLite.Simple.Internal` — `Connection` (an open database plus a
  savepoint-name counter), `Statement` (a prepared statement paired with its
  owning `Connection`), and `Field` (a decoded column value with its index/
  name/declared-type, `Field.typeName`), plus `currentRowFields`. `Statement`
  is relocated here from upstream's top-level `Database.SQLite.Simple` module
  per this port's module plan; `ColumnOutOfBounds`/`RowParser` are left to a
  later `FromRow`/`FromField` port.
- `Database.SQLite.Simple.ToField` — the `ToField` class, converting a Lean
  value to `SQLData` for parameter binding: numeric/`Bool`/`String`/
  `ByteArray`/`Option`/`Null`/`Day`/`UTCTime` instances.
- `Database.SQLite.Simple.FromField` — the dual `FromField` class, decoding a
  `Field` back to a Lean value, plus `ResultError` (`incompatible`/
  `unexpectedNull`/`conversionFailed`) folded into `Ok.errors` messages via
  `returnError`.
- `Database.SQLite.Simple.FromRow` — the applicative `RowParser` (a direct
  `Array Field → Nat → Ok (α × Nat)`, substituting upstream's
  `ReaderT`/`StateT`/`Ok` stack) and the `FromRow` class, with tuple/`Only`/
  `Cons` instances up to arity 7.
- `Database.SQLite.Simple.ToRow` — the dual `ToRow` class, rendering a
  collection into a flat `Array SQLData`; `Unit`/`Only`/tuple (up to arity
  7)/`Cons` instances.
- `Database.SQLite.Simple.QQ` — the `sql "…"` syntax, substituting upstream's
  Template-Haskell `[sql| … |]` quasiquoter with Lean `syntax`/`macro_rules`;
  elaborates directly to `Query.ofString`, matching how little validation
  upstream's own `quoteExp` performs (none — it is a plain string splice).
- `Database.SQLite.Simple` — the public facade: `withConnection` (bracketed
  `openConnection`/`closeConnection`), `query`/`query_`/`execute`/`execute_`,
  `fold`/`fold_` (streaming), `withTransaction`/`withImmediateTransaction`/
  `withExclusiveTransaction` and `withSavepoint` (commit/rollback via
  `try … catch … throw`), `lastInsertRowId`/`changes`/`totalChanges`, and
  `Database.SQLite3.SQLError`'s pretty-printed `ToString`.
- `Database.SQLite.Simple.Function` — user-defined scalar SQL function
  registration on top of `sqlite3_create_function_v2`: `createFunction0`
  through `createFunction3` (a fixed 0..3 arity cutoff substituting
  upstream's GHC-overlapping-instance `Function` typeclass, the same shape
  of substitution `FromRow`/`ToRow`'s own arity cutoff already uses) and
  `deleteFunction`. Required this codebase's first Lean-closure-called-from-C
  machinery (`ffi/sqlite3_shim.c`'s `xFunc`/`xDestroy` trampolines,
  `lean_apply_4`/`lean_inc_ref`/`lean_dec_ref`), justified safe because
  SQLite always invokes the callback synchronously on the calling thread.

### `Database.DuckDB.FFI` — low-level DuckDB FFI bindings (`duckdb-ffi`)

- `Database.DuckDB.FFI.Types` — opaque `Database`/`Connection`/`Result`/
  `PreparedStatement`/`Appender`/`LogicalType`/`DataChunk`/`Vector` handles
  (same external-object pattern as `LibPQ.Types`/`Database.SQLite3.Bindings.Types`),
  plus the `duckdb_state`/`duckdb_type`/`duckdb_error_type`/
  `duckdb_statement_type` enums (`ofUInt32`/`toUInt32`, `.other` fallback for
  unrecognized codes).
- `Database.DuckDB.FFI.OpenConnect` — open/close a database and connection
  (`ffi/duckdb_shim.c`, linking `libduckdb`), idempotent `close`.
- `Database.DuckDB.FFI.Configuration` — `duckdb_config` creation/destruction
  and `duckdb_set_config`; always passes a `NULL` `duckdb_config` where
  upstream would otherwise thread one through, narrowing this batch's scope.
- `Database.DuckDB.FFI.ErrorData` — `duckdb_error_data` inspection
  (`hasError`/`errorMessage`/`errorType`) and the `duckdb_result` error
  accessors.
- `Database.DuckDB.FFI.Logging` — `duckdb_log`-style logging plus this
  port's first Lean-closure-called-from-C trampoline (write/call/delete
  trampoline pair using `lean_inc_ref`/`lean_dec_ref`/`lean_apply_N`), later
  reused by `ScalarFunctions`.
- `Database.DuckDB.FFI.Catalog` — catalog/table-existence and search-path
  introspection.
- `Database.DuckDB.FFI.FileSystem` — `duckdb_extract_statements`/file-system
  helper bindings.
- `Database.DuckDB.FFI.Helpers` — shared marshalling helpers (owned/borrowed
  string decoding, `ByteArray`↔`duckdb_string_t` conversions) used across the
  rest of the batch.
- `Database.DuckDB.FFI.LogicalTypes` — building/inspecting
  `duckdb_logical_type` values: primitive/`LIST`/`ARRAY`/`MAP`/`STRUCT`/
  `UNION`/`ENUM`/`DECIMAL` construction, alias get/set, `duckdb_type` id,
  decimal width/scale, enum dictionary, struct/union child names/types,
  list/array/map child types. `create` decodes a `Type_` on the Lean side
  before calling the raw `createRaw : UInt32 → IO LogicalType` extern — a
  boxed inductive like `Type_` (it has a data-carrying `.other` constructor)
  cannot be passed directly as a raw C `uint32_t` parameter, so the
  encode/decode must happen in pure Lean, mirroring `columnTypeRaw`/
  `columnType`'s existing pattern elsewhere in this port.
- `Database.DuckDB.FFI.BindValues` — binding scalar/`NULL` parameter values
  to a `PreparedStatement` (`duckdb_bind_*`).
- `Database.DuckDB.FFI.PreparedStatements` — preparing/destroying statements,
  parameter count/name/type inspection, `duckdb_clear_bindings`, and
  result-set metadata (statement type, column count/name/type/logical type).
- `Database.DuckDB.FFI.QueryExecution` — `duckdb_query`/executing a
  connection's SQL text directly, result-set inspection (column name/type/
  logical type, statement type, rows-changed), and error reporting for
  malformed queries.
- `Database.DuckDB.FFI.ExecutePrepared` — executing a bound
  `PreparedStatement` and materializing/streaming its `Result`.
- `Database.DuckDB.FFI.DataChunk` — `duckdb_data_chunk` creation/destruction,
  column count/vector access, and row-count get/set.
- `Database.DuckDB.FFI.Vector` — typed scalar get/set accessors
  (`Int32`/`Int64`/`Double`/`Bool`, …) into a `duckdb_vector`'s raw data
  buffer, raw `ByteArray` access, string element assignment, `LIST`/`STRUCT`
  child-vector access, and `duckdb_vector_reference_value_vector`. Both
  owning (`DataChunk`-derived) and non-owning/borrowed vectors share this
  API; the finalizer distinction lives in `Types`.
- `Database.DuckDB.FFI.Validity` — the validity-mask accessors
  (`rowIsValid`/`setRowValidity`/`setRowValid`/`setRowInvalid`,
  `ensureValidityWritable`) shared by result vectors and the appender.
- `Database.DuckDB.FFI.Appender` — the bulk-insert `duckdb_appender`
  lifecycle and per-column `append*` value writers.
- `Database.DuckDB.FFI.ScalarFunctions` — user-defined scalar SQL function
  registration (`duckdb_create_scalar_function`/`ScalarFunctionSet`):
  `setName`/`addParameter`/`setReturnType`/`setFunction`, and
  `setOnCall`/registration built on a second Lean-closure-called-from-C
  trampoline pair (mirroring `Logging`'s), whose call trampoline retrieves
  the registered closure via `duckdb_scalar_function_get_extra_info` (the
  callback signature `(duckdb_function_info, duckdb_data_chunk,
  duckdb_vector)` carries no direct closure parameter) rather than
  receiving it as a direct argument.

### `Database.DuckDB.Simple` — mid-level DuckDB client library (`duckdb-simple`)

- `Database.DuckDB.Simple.Internal` — `Connection`/`SQLError`, the
  `withDatabaseHandle`/`withConnectionHandle`/`withClientContext`
  bracket-style handle accessors, and delete-callback/`StablePtr`
  registration helpers reused by `Logging`/`Copy`/`Function`.
- `Database.DuckDB.Simple.LogicalRep` — building/destroying
  `duckdb_logical_type` descriptors (struct/list/map/enum/decimal) used both
  when binding parameters and when decoding result columns.
- `Database.DuckDB.Simple.Ok` — the error-accumulating `Ok a | Errors
  [SomeException]` applicative, ported the same way as `sqlite-simple`'s
  `Ok` (`Except (Array String)`-style).
- `Database.DuckDB.Simple.Types` — `Query`, `Null`, `FormatError`, `(:.)`,
  the folded-in `Only` (reused from the `sqlite-simple` port) and a
  folded-in 128-bit `UUID` type (marshalling DuckDB's native `UUID` column
  type; the `uuid` package's generation machinery is out of scope).
- `Database.DuckDB.Simple.FromField` — `Field`, `FieldValue` (DuckDB's
  tagged-union decoded value, covering every DuckDB logical type: ints of
  every width, `DecimalValue`, `BigNum`, `BitString`, `IntervalValue`,
  `TimeWithZone`, lists/structs/maps/enums, `UUID`, …), the `FromField`
  class, and `ResultError`.
- `Database.DuckDB.Simple.Materialize` — converts one DuckDB
  `duckdb_vector`/`data_chunk` column into a `FieldValue` per row, the
  single place that walks DuckDB's native columnar chunk representation
  (an internal, not-exposed-upstream module, load-bearing for `Copy`/
  `Function`/the facade).
- `Database.DuckDB.Simple.ToField` — `DuckDBColumnType`, `FieldBinding`,
  `NamedParam`, the `ToField` class and instances, binding a Lean value
  into a prepared-statement parameter slot via
  `Database.DuckDB.FFI.BindValues`. `STRUCT`/`UNION`/`LIST`/`MAP`/`ENUM`
  values have no `ToField` instance — `duckdb.h` exposes no
  `duckdb_bind_struct`/`duckdb_bind_union`-style entry point.
- `Database.DuckDB.Simple.FromRow` — the applicative row-consuming parser,
  same shape as `sqlite-simple`'s `FromRow`.
- `Database.DuckDB.Simple.ToRow` — dual of `FromRow`, encoding a row of
  parameters via `ToField`.
- `Database.DuckDB.Simple.Catalog` — catalog/table-existence and
  search-path queries.
- `Database.DuckDB.Simple.Config` — connection-config key/value setting
  prior to `open`.
- `Database.DuckDB.Simple.FileSystem` — registering a virtual filesystem
  callback set.
- `Database.DuckDB.Simple.Logging` — log-callback registration.
- `Database.DuckDB.Simple.Copy` — bulk row-append ("COPY"-style bulk load)
  via `Database.DuckDB.FFI.Appender`.
- `Database.DuckDB.Simple.Function` — user-defined scalar SQL function
  registration (`createFunction`/`createFunctionWithState`/
  `deleteFunction`) via `Database.DuckDB.FFI.ScalarFunctions`.
- `Database.DuckDB.Simple.Generic` — hand-written `STRUCT`/`UNION` decode
  combinators (`structField`/`withStruct`/`unionField`/`unionFieldNamed`/
  `withUnion`/`firstMatch`) standing in for upstream's GHC-`Generic`-derived
  `FromField`/`ToField` instances, which have no Lean counterpart (no
  `Rep`/`Generic` machinery to walk); includes a worked two-constructor
  sum-type (`Shape`) example with a hand-written `FromField` instance.
- `Database.DuckDB.Simple` — the public facade: `withConnection`,
  `query`/`query_`/`execute`/`execute_`, streaming `fold`/`fold_`, and
  `withTransaction`, built on prepared statements + repeated
  `QueryExecution.fetchChunk` + `Materialize` + `FromRow`/`ToRow`. Its test
  is a genuine end-to-end round trip against a real in-memory DuckDB
  connection. Completes the `sqlite-simple` → `duckdb-ffi` → `duckdb-simple`
  import chain.

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

### `Network.OAuth2` — OAuth2 client (`hoauth2`)

A port of Hackage's [`hoauth2`](https://hackage.haskell.org/package/hoauth2)
(v2.15.1), per [`docs/imports/hoauth2/dependencies.md`](imports/hoauth2/dependencies.md).
`memory`, `exceptions`, `microlens`, `uri-bytestring`(`-aeson`), and
`binary`(`-instances`) are all substituted with existing `linen`/stdlib
equivalents rather than freshly ported; `crypton` is scoped down to two new
OpenSSL-backed FFI primitives reusing `Crypto.JOSE.FFI`'s already-linked
OpenSSL — see the dependencies doc for the full rationale on each.

- `Crypto.SHA256` / `Crypto.SecureRandom` — two new `@[extern]` bindings added
  to `ffi/jose.c`, reusing the OpenSSL link already established for
  `Crypto.JOSE.FFI`: a plain SHA-256 digest (`EVP_sha256`/`EVP_Digest`) and a
  CSPRNG byte generator (`RAND_bytes`) — the only two primitives PKCE's
  `S256` challenge method needs, in place of a full `crypton` import.
- `Network.HTTP.Client.Contrib` — small response-handling helpers
  (`handleResponse`/`handleResponseJSON`) shared by the OAuth2 HTTP flows.
- `Network.OAuth2.Internal` — `OAuth2` client config, `AccessToken`/
  `RefreshToken`/`IdToken`/`ExchangeToken` newtypes, `ClientAuthenticationMethod`,
  and URI/request helpers (`uriToRequest`, query-parameter injection)
  substituting `exceptions`'s bare `MonadThrow` constraint and `microlens`'s
  `over` with plain `Except`-returning functions.
- `Network.OAuth2.AuthorizationRequest` — the authorization-code redirect URL
  builder (RFC 6749 §4.1.1).
- `Network.OAuth2.HttpClient` — bearer/basic-auth request application and
  JSON-decoding HTTP calls over `Network.HTTP.Client.Conduit`.
- `Network.OAuth2.TokenRequest` — `TokenResponse`/`OAuth2Token` parsing and
  `fetchAccessToken`/refresh; the upstream `Binary TokenResponse` instance is
  dropped (it only proxied through the already-derived JSON codec).
- `Network.OAuth2` — facade re-exporting the four modules above.
- `Network.OAuth2.Experiment.Utils` / `.Pkce` — query-parameter map flattening
  and RFC 7636 PKCE (`CodeVerifier`/`CodeChallenge`, `S256`); `genCodeVerifier`
  documents its one simplification (a `% 66` mapping onto the unreserved
  alphabet, replacing upstream's probabilistic rejection-sampling loop).
- `Network.OAuth2.Experiment.Types` — the typed OAuth2-request-builder
  machinery (`IdpApplication` family, `GrantTypeFlow`, phantom-typed
  application config); upstream's `TypeFamilies`/`PolyKinds` are ported via
  `outParam` class parameters (the same idiom the Lean stdlib uses for
  `Membership`/`GetElem`) and a plain `Type` parameter respectively.
- `Network.OAuth2.Experiment.Flows.UserInfoRequest` / `.AuthorizationRequest`
  / `.DeviceAuthorizationRequest` / `.TokenRequest` / `.RefreshTokenRequest` —
  per-flow request parameter types and their `ToQueryParam` instances.
- `Network.OAuth2.Experiment.Flows` — the package's real HTTP-performing
  entry points (token/device-authorization/refresh/user-info requests,
  including RFC 8628 §3.5's device-token polling loop, written with a
  structural `Loop.forIn`, not `partial def` or a fuel parameter).
- `Network.OAuth2.Experiment.Grants.ClientCredentials` / `.DeviceAuthorization`
  / `.JwtBearer` / `.ResourceOwnerPassword` / `.AuthorizationCode` — one
  `IdpApplication` instance per OAuth2 grant type (RFC 6749 + RFC 8628); the
  authorization-code grant additionally wires up PKCE.
- `Network.OAuth2.Experiment.Grants` / `Network.OAuth2.Experiment` — facades
  re-exporting the grant modules, and the package's full typed surface
  (`Types`/`Grants`/`Flows`/`Pkce`/`Utils`), respectively.

### `Crypto.Zlib` / `Crypto.MD5` / `Crypto.RC4` / `Crypto.AES` — primitives for PDF encryption

- `Crypto.Zlib.FFI` — a `@[extern]` opaque handle wrapping zlib's `z_stream *`
  for raw zlib/RFC 1950 *inflate* (decompress) only, ported from Hackage's
  `zlib` (`Codec.Zlib`'s low-level FFI surface). Modeled on
  `Network.TLS.Context`'s OpenSSL-handle pattern: `ffi/zlib.c` allocates the
  stream and registers a GC finalizer (`inflateEnd`) so it is freed exactly
  once, whether via an explicit `finish` or by being dropped.
- `Crypto.MD5` — a pure, structurally-recursive port of `cryptohash`'s
  `Crypto.Hash.MD5.hash` (RFC 1321), scoped to MD5 alone since it is the
  only digest the PDF Standard Security Handler's key derivation needs.
  Padding fixes the block count before the compression loop starts, so the
  64-round loop is a plain `Array.foldl` — no `partial def` or fuel.
- `Crypto.RC4` — Hackage's `cipher-rc4` stream cipher: `initCtx` (key
  scheduling, a 256-round structural fold building the S-box permutation)
  and `combine` (the pseudo-random generation algorithm, structurally
  recursive over the input bytes), feeding the PDF `AESV2`/RC4 decryptor.
- `Crypto.AES` — the scoped slice of `cipher-aes` (AES-128 key schedule +
  CBC decrypt only, no ECB/CTR/GCM, no encrypt, no other key sizes) plus
  `crypto-api`'s `unpadPKCS5`, folded into one module since PKCS5 unpadding
  has a single caller here. Every function operates on fixed-size data or
  recurses structurally on a length-derived `Nat`.

### `Data.PDF.Stream` — buffer-resident `io-streams` port

- Ports the scoped slice of Hackage's `io-streams` actually used by
  `pdf-toolbox-*` (`docs/imports/IoStreams/dependencies.md`). Every real
  call site reads a fully-resident PDF file/buffer, never an unbounded
  network source, so `InputStream`/`OutputStream` are `ByteArray`-backed
  cursor-plus-pushback types rather than upstream's general lazy/incremental
  stream machinery.
- Constructors (`fromByteString`, `fromList`, `makeInputStream`,
  `countInput`/`countOutput`, `takeBytes`, `decompress`) are `mkRef`-backed
  closures; the one unbounded drain (`toList`) is a `while` loop over local
  mutable state, matching this project's established idiom elsewhere
  (`Network.WebApp.strictRequestBody`) rather than upstream's `partial`
  recursive drain.

### `Data.PDF.Core` — PDF object model, parsing, xref, encryption (`pdf-toolbox-core`)

19 modules porting Hackage's `pdf-toolbox-core`
(`docs/imports/PdfToolboxCore/dependencies.md`), the low-level layer every
higher PDF module builds on:

- `Name`/`Exception`/`Parsers.Util`/`IO.Buffer` lay the groundwork: atomic
  PDF names (§7.3.5) over `Data.ByteString`, structured `corrupted`/
  `unexpected` errors rendered through `IO.Error.userError`, shared
  `Std.Internal.Parsec.ByteArray` combinators, and a cursor-based `Buffer`
  abstraction that adapts directly onto `Data.PDF.Stream.InputStream`.
- `Object`/`Object.Util`/`Object.Builder` are the object model (§7.3):
  `Object` is ported as a genuinely recursive sum type — no flattening or
  un-decoding to dodge a termination proof, per `AGENTS.md`'s explicit
  warning — with `Dict = Std.HashMap Name Object` exposed publicly while
  stored internally as an `Array (Name × Object)` to satisfy Lean's
  positivity checker. `buildObject`'s upstream `error`-on-`Stream` partial
  function becomes a total `Except String Builder`.
- `Parsers.Object`/`Parsers.XRef`/`Util` parse objects and the xref
  table/trailer against `Std.Internal.Parsec`, wrapping every
  input-consuming alternative in `attempt` (attoparsec's `<|>` always
  backtracks fully; `Parsec`'s only backtracks on zero consumption) and
  using a fuel parameter for `startXRef`'s otherwise-unbounded
  scan-and-take-last search.
- `Stream.Filter.Type`/`Stream.Filter.FlateDecode`/`Stream` implement stream
  decoding: `FlateDecode` (§7.4.4) drives `Crypto.Zlib.FFI` and reverses the
  PNG-Up (`12`) or no-op (`1`) predictor — the only two predictors upstream
  itself implements.
- `XRef`/`Encryption`/`File` tie everything together: the cross-reference
  index (§7.5.4/§7.5.8, table or stream form, `/Prev`-chained), the Standard
  Security Handler (§7.6.3.3, revisions 2–4, real RC4/AES-128-CBC over the
  `Crypto.*` primitives above — not a stub), and `File.findObject`
  resolving indirect references through both.
- `Core`/`Types`/`Writer` round out the package: a thin re-export
  aggregator, compound types (generic `Rectangle`, §7.9), and a PDF writer
  supporting both fresh files and incremental updates.

### `Data.PDF.Content` — content-stream operators, fonts, text encoding (`pdf-toolbox-content`)

13 modules porting Hackage's `pdf-toolbox-content`
(`docs/imports/PdfToolboxContent/dependencies.md`):

- `Transform`/`Ops` — 2D affine transforms for the `cm` operator (§8.3.4),
  and the closed enumeration of every content-stream operator keyword
  (Annex A) plus an `UnknownOp` catch-all.
- `FontDescriptor`/`GlyphList`/`TexGlyphList`/`Encoding.WinAnsi`/
  `Encoding.MacRoman`/`Encoding.PdfDoc` — font metrics (§9.8) and four fixed
  lookup tables transcribed verbatim from upstream: the ~4280-entry Adobe
  Glyph List, the ~284-entry supplementary TeX glyph list, and the
  WinAnsi/MacRoman/PDFDoc single-byte encodings (Annex D).
- `UnicodeCMap`/`Parser`/`Processor` — parsing a `ToUnicode` CMap's
  codespace ranges and `bfchar`/`bfrange` mappings, parsing a content
  stream into `Operator`s, and `processOp` interpreting them against a
  tracked graphics state (upstream's own doc-comment calls this module
  "pretty experimental" — carried forward as-is).
- `FontInfo` ties the package together: simple vs. composite (Type 0/CID)
  font decoding into Unicode text, and `Content` is the thin re-export
  aggregator.

### `Data.PDF.Document` — document/page-tree API, text extraction (`pdf-toolbox-document`)

11 modules porting Hackage's `pdf-toolbox-document`
(`docs/imports/PdfToolboxDocument/dependencies.md`), the highest-level PDF
layer:

- `Types`/`Internal.Types`/`Internal.Util`/`Pdf`/`Document`/`Info` build the
  document handle: a cached `lookupObject`/`deref` wrapper around
  `Data.PDF.Core.File`, `document`'s encryption check, and single-indirection
  trailer/info-dictionary accessors (§14.3.3).
- `Catalog`/`FontDict` are further single-indirection accessors — `/Pages`
  (§7.7.2) and font-dictionary decoding (§9.6) via `Data.PDF.Content.FontInfo`.
- `PageNode`/`Page` are the one place in this batch with genuine
  **untrusted-graph recursion**: a malformed PDF's `/Kids` or `/Parent`
  entries can point back at an ancestor, and upstream has no cycle guard at
  all. Both are ported against a **fuel parameter *and* an explicit
  visited-`Ref` set**: the fuel is seeded from the trailer's own `/Size`
  entry (a genuine, file-declared upper bound on distinct object
  references), consumed once per new tree node descended into (an ordinary
  structurally-decreasing `Nat`, so Lean's termination checker accepts it
  outright); the visited set turns a repeat visit into a "cycle detected"
  error instead of an infinite loop — a deliberate, documented improvement
  over upstream's behaviour on malformed input, not a change on any
  well-formed file. `pageNodePageByNum` descends `/Kids` this way;
  `pageMediaBox`'s `mediaBoxRec` ascends `/Parent` the same way.
- `Document` is the thin re-export aggregator for the whole batch.

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

### `Codec.Picture` — image codecs (PNG, JPEG, GIF, BMP, TGA, TIFF, HDR)

A port of [`JuicyPixels`](https://hackage.haskell.org/package/JuicyPixels)
(see [`docs/imports/JuicyPixels/dependencies.md`](imports/JuicyPixels/dependencies.md)):
pixel/image types and colorspace conversions, plus independent encoders/
decoders for every major raster format. "Codec" and "Picture" name a general
subject area rather than Haskell/GHC itself, so the package is ported as
`Linen.Codec.Picture.*` (same reasoning as `Graphics.Netpbm`).

- `Codec.Picture.Types` — `Image`/`MutableImage`/`DynamicImage`, the
  `Pixel`/`ColorConvertible`/`ColorSpaceConvertible`/`ColorPlane`/
  `LumaPlaneExtractable`/`TransparentPixel` classes, and every concrete pixel
  type (`PixelYA8`, `PixelRGB8`, `PixelYCbCr8`, `PixelCMYK8`, … and their
  16-bit/float variants).
- `Codec.Picture.VectorByteConversion` — `Array UInt8` ↔ `ByteArray`
  conversion (an explicit element-wise copy, since Lean's `Array`/`ByteArray`
  share no storage-layout trick to exploit as upstream's unsafe pointer cast
  does).
- `Codec.Picture.InternalHelper` — small `ByteString`/file-loading helpers.
- `Codec.Picture.BitWriter` — MSB-first bit-level reader/writer, used by both
  the JPEG and PNG (and GIF LZW) codecs.
- `Codec.Picture.Metadata.Exif` — Exif tag vocabulary and TIFF-embedded Exif
  directory parsing.
- `Codec.Picture.Metadata` — the generic `Metadatas`/`Keys`/`Elem` map
  attached to decoded images.
- `Codec.Picture.ColorQuant` — median-cut colour quantization (GIF/PNG
  palette generation).
- `Codec.Picture.Bitmap` — BMP decode/encode.
- `Codec.Picture.Tga` — TGA decode/encode.
- `Codec.Picture.HDR` — Radiance HDR decode/encode.
- `Codec.Picture.Png.Internal.Type` — PNG chunk structure, `ChunkSignature`,
  filter types.
- `Codec.Picture.Png.Internal.Metadata` — PNG ancillary-chunk ↔ `Metadata`
  conversion.
- `Codec.Picture.Png.Internal.Export` — image → PNG chunk encoding.
- `Codec.Picture.Png` — top-level PNG decode/encode, over zlib
  inflate/deflate (`Linen.Data.Compression.Zlib`).
- `Codec.Picture.Tiff.Internal.Types` — TIFF IFD/tag structure.
- `Codec.Picture.Tiff.Internal.Metadata` — TIFF tag ↔ `Metadata` conversion.
- `Codec.Picture.Tiff` — top-level TIFF decode/encode.
- `Codec.Picture.Gif.Internal.LZW` — GIF LZW decompression.
- `Codec.Picture.Gif.Internal.LZWEncoding` — GIF LZW compression.
- `Codec.Picture.Gif` — top-level GIF decode/encode, including animated GIFs
  (frame sequencing, disposal methods, delays), with palette quantization on
  encode.
- `Codec.Picture.Jpg.Internal.DefaultTable` — standard JPEG
  quantization/Huffman tables.
- `Codec.Picture.Jpg.Internal.Types` — JPEG marker/segment/scan structure.
- `Codec.Picture.Jpg.Internal.Common` — shared JPEG decode helpers.
- `Codec.Picture.Jpg.Internal.FastDct` / `.FastIdct` — integer DCT/IDCT.
- `Codec.Picture.Jpg.Internal.Metadata` — JFIF/Exif ↔ `Metadata` conversion.
- `Codec.Picture.Jpg.Internal.Progressive` — progressive-scan JPEG decoding.
- `Codec.Picture.Jpg` — top-level JPEG decode/encode (baseline +
  progressive).
- `Codec.Picture.Saving` — format-agnostic "save with extension" dispatch
  over every codec above.
- `Codec.Picture` — the package's public re-export facade.

### `Graphics.Image` — image processing (colour spaces, geometry, filters, morphology, FFT, AHE, Hough, noise)

A port of [`hip`](https://hackage.haskell.org/package/hip) (see
[`docs/imports/hip/dependencies.md`](imports/hip/dependencies.md)): pixel/
colour-space abstractions over a `repa`-backed image array, geometric and
convolution-based processing, and file I/O reusing `Codec.Picture`/
`Graphics.Netpbm`. `hip`'s own representation-selection layer
(`Interface.Vector.*`/`Interface.Repa.*`, 8 modules) needs no separate port —
every image here is backed directly by `Linen.Data.Array.Shaped`. "Graphics.
Image" names a general subject area rather than Haskell/GHC itself, so the
package is ported as `Linen.Graphics.Image.*`.

- `Graphics.Image.Utils` — small numeric/list helpers with no
  `Graphics.Image.*` dependency of their own.
- `Graphics.Image.Interface.Elevator` — the `Elevator` class: precision-
  changing conversions between pixel component types (e.g. `Float ↔ UInt8`),
  with range scaling, plus `clamp01`.
- `Graphics.Image.Interface` — the central `Pixel`/`ColorSpace`/
  `AlphaSpace`/array classes tying a pixel type and component type to an
  image's array representation, a `Manifest`-backed `Image` type, and
  generic border-handling indexing (`index`, `borderIndex`, `Border`, …).
- `Graphics.Image.ColorSpace.Y` — single-channel luma colour space (`Y`),
  plus its alpha-carrying counterpart `YA`.
- `Graphics.Image.ColorSpace.RGB` — three-channel red/green/blue colour
  space, plus `RGBA`.
- `Graphics.Image.ColorSpace.HSI` — three-channel hue/saturation/intensity
  colour space, plus `HSIA`.
- `Graphics.Image.ColorSpace.CMYK` — four-channel cyan/magenta/yellow/key
  (black) colour space, plus `CMYKA`.
- `Graphics.Image.ColorSpace.YCbCr` — three-channel luma/chroma colour
  space, plus `YCbCrA`.
- `Graphics.Image.ColorSpace.Complex` — complex-valued pixels over any
  existing colour space, used by the FFT-based processing modules.
- `Graphics.Image.ColorSpace.X` — a generic, "unlabeled" single-channel
  colour space used as a building block, distinct from luma `Y`.
- `Graphics.Image.ColorSpace.Binary` — bit-valued binary pixels built on `X`,
  for thresholding/morphology.
- `Graphics.Image.ColorSpace` — the colour-space facade: re-exports every
  individual colour space, plus the cross-colour-space conversion matrix
  (`ToY`/`ToRGB`/`ToHSI`/`ToCMYK`/`ToYCbCr`, and their alpha-carrying
  counterparts).
- `Graphics.Image.Processing.Interpolation` — sampling an image at a
  non-integer coordinate (nearest-neighbour/bilinear).
- `Graphics.Image.Processing.Geometric` — resampling, cropping, flipping,
  rotation, and resizing of images.
- `Graphics.Image.Processing.Complex.Fourier` — the 2-D fast Fourier
  transform (and its inverse) on complex-pixel images.
- `Graphics.Image.Processing.Complex` — whole-image complex-pixel
  operations, plus the `fft`/`ifft` re-export.
- `Graphics.Image.Processing.Convolution` — kernel convolution/correlation
  of an image.
- `Graphics.Image.Processing.Filter` — named filter kernels (Sobel,
  Gaussian, Laplacian, …) built on convolution.
- `Graphics.Image.Processing.Binary` — binary-image construction and
  morphology (erode/dilate/opening/closing) built on `ColorSpace.Binary` and
  `Processing.Convolution`.
- `Graphics.Image.Processing` — the processing facade: re-exports
  `Geometric`/`Interpolation`/`Convolution`/`Filter`, plus its own
  `pixelGrid`.
- `Graphics.Image.IO.Base` — shared reader/writer typeclasses and
  pixel-precision normalisation used by every concrete image-format backend.
- `Graphics.Image.IO.Formats.JuicyPixels` — glue between `Image cs e` and
  `Linen.Codec.Picture`, reusing that suite for the actual PNG/JPEG/GIF/BMP/
  TIFF/TGA/HDR decode/encode.
- `Graphics.Image.IO.Formats.Netpbm` — glue between `Image cs e` and
  `Linen.Graphics.Netpbm`, reusing it for the actual PNM/PGM/PPM decode.
- `Graphics.Image.IO.Formats` — the format-dispatch facade tying together
  every JuicyPixels-backed and Netpbm-backed format tag.
- `Graphics.Image.IO` — the top-level file-I/O facade: format-guessing
  read/write wrappers built on `IO.Base`/`IO.Formats`.
- `Graphics.Image.Types` — the package-level type/re-export facade:
  concrete (colour space × precision) image type aliases.
- `Graphics.Image` — the top-level public facade of the whole `hip` library.
- `Graphics.Image.Processing.Ahe` — adaptive (local-rank) histogram
  equalization.
- `Graphics.Image.Processing.Hough` — the linear Hough transform for line
  detection (a vote-count heatmap over discretized angle/distance, per
  upstream's own experimental algorithm).
- `Graphics.Image.Processing.Noise` — salt-and-pepper (impulse) noise
  generation, threading Lean core's `StdGen`/`randNat` (`Init.Data.Random`,
  already a direct port of the same `System.Random` this module builds on)
  as a pure, explicit-seed generator.

## Module Table

| Module | Description |
|---|---|
| `Linen.Data.Functor` | `Compose`, `Const`, `Product`, `FunctorSum`, `Contravariant` |
| `Linen.Data.Array.Shaped` | `repa`'s package facade: re-exports `Shape`/`Index`/`Slice`/`Base`, every representation/operator/`Specialised.Dim2`/`Stencil` module below |
| `Linen.Data.Array.Shaped.Base` | the `Source` class of readable array representations (`repa`'s associated-data-family `Array r sh e` becomes concrete types per representation) |
| `Linen.Data.Array.Shaped.Index` | index/shape types (`Z`, `Snoc`/`:.`) and their `Shape` instances |
| `Linen.Data.Array.Shaped.Operators.IndexSpace` | index-space transforms: `reshape`/`append`/`transpose`/`extract`/`backpermute`/`backpermuteDft`/`extend`/`slice` |
| `Linen.Data.Array.Shaped.Operators.Interleave` | interleaves the elements of two to four same-extent arrays along the lowest dimension |
| `Linen.Data.Array.Shaped.Operators.Mapping` | element-wise `map`/`zipWith`, arithmetic operators, the `Structured` class |
| `Linen.Data.Array.Shaped.Operators.Reduction` | sequential folding/summing over arrays (`foldS`/`sumS`/`equalsS`); parallel `*P` variants dropped, no distinct sequential behaviour |
| `Linen.Data.Array.Shaped.Operators.Selection` | sequential `select`, filtering a range of indices into a `Manifest` array |
| `Linen.Data.Array.Shaped.Operators.Traversal` | generic unstructured `traverse` building a `Delayed` array from one to four source arrays |
| `Linen.Data.Array.Shaped.Repr.Cursored` | the `Cursored` array representation: shiftable index-computation cursors shared between neighbouring reads |
| `Linen.Data.Array.Shaped.Repr.Delayed` | the `Delayed` representation: a shape paired with an index→element function, recomputed on every read |
| `Linen.Data.Array.Shaped.Repr.Manifest` | the `Manifest` representation over a flat `Array e`, collapsing upstream's `Unboxed`/`ForeignPtr`/`Vector`/`ByteString` variants; `computeS`/`copyS` materialize a `Source` array |
| `Linen.Data.Array.Shaped.Repr.Partitioned` | the `Partitioned` representation: dispatches between two same-shape sub-arrays by an index-range predicate |
| `Linen.Data.Array.Shaped.Repr.Undefined` | the `Undefined` representation: known extent, `panic!` on read, used as a partition's never-read fallback |
| `Linen.Data.Array.Shaped.Shape` | the `Shape` class of types usable as array shapes/indices |
| `Linen.Data.Array.Shaped.Slice` | index-space transformation between arrays and slices via the `outParam`-based `Slice` class |
| `Linen.Data.Array.Shaped.Specialised.Dim2` | functions specialised for rank-2 arrays |
| `Linen.Data.Array.Shaped.Stencil` | thin aggregator: pulls in stencil creation (`Stencil.Base`) and application (`Stencil.Dim2`, `Specialised.Dim2`) |
| `Linen.Data.Array.Shaped.Stencil.Base` | basic stencil definitions: `Boundary`, stencil coefficients/offsets |
| `Linen.Data.Array.Shaped.Stencil.Dim2` | applies a stencil to a 2D array by folding over its offsets (generalizes upstream's fixed 7×7 GHC-optimiser unrolling) |
| `Linen.Data.Array.Shaped.Stencil.Partition` | pure 2D geometry for partitioning a region for stencil application |
| `Linen.Data.Base64` | RFC 4648 `encode`/`decode` over `ByteArray` (structural, no `partial`) |
| `Linen.Data.Bifunctor` | `Bifunctor`/`LawfulBifunctor`, `bimap`, `Prod`/`Sum`/`Except` instances |
| `Linen.Data.ByteString` | slice over `ByteArray` (O(1) `take`/`drop`/`splitAt`); full `Data.ByteString` API + `BEq`/`Ord`/`Hashable` |
| `Linen.Data.ByteString.Char8` | Latin-1 `Char` view of `ByteString`: `String`↔`ByteString`, `lines`/`words`/`unlines`/`unwords` |
| `Linen.Data.ByteString.Lazy` | chunked lazy byte strings (`Thunk` tail): `fromChunks`/`toStrict`, lazy `append`, `take`/`drop`, folds |
| `Linen.Data.ByteString.Lazy.Char8` | Latin-1 `Char` view of `LazyByteString`: `String`↔`LazyByteString`, char-wise ops |
| `Linen.Data.ByteString.Short` | `ShortByteString` (`ByteArray` newtype): `pack`/`unpack`/`index`, `toShort`/`fromShort` |
| `Linen.Data.ByteString.Builder` | difference-list builder (O(1) `append`): word/UTF-8/decimal/hex encoders + monoid laws |
| `Linen.Data.CaseInsensitive` | `FoldCase` class + `CI α` wrapper: case-insensitive `BEq`/`Ord`/`Hashable`, original-preserving `ToString` |
| `Linen.Data.Colour` | package facade over `Colour.Internal`: `Colour`, `AlphaColour`, `black`/`opaque`/`withOpacity`/`transparent`/`alphaChannel`/`blend`/`dissolve`/`atop` |
| `Linen.Data.Colour.Chan` | a single, phantom-tagged colour channel, specialized to `Float` |
| `Linen.Data.Colour.CIE` | colour operations defined by the CIE |
| `Linen.Data.Colour.CIE.Chromaticity` | CIE xy chromaticity coordinates, specialized to `Float` |
| `Linen.Data.Colour.CIE.Illuminant` | standard illuminants defined by the CIE (A, D65, …) |
| `Linen.Data.Colour.Internal` | `Colour`/`AlphaColour` over `Float`: `blend`/`over`/`darken` via a merged `AffineSpace`/`ColourOps` closed world |
| `Linen.Data.Colour.Matrix` | dense 3×3 matrices (`Matrix3`) and 3-vectors (`Vec3`) for RGB↔XYZ colour-space transforms |
| `Linen.Data.Colour.Names` | SVG 1.1 named colours + `readColourName : String → Option Colour` |
| `Linen.Data.Colour.RGB` | a generic RGB triple (`RGB α`) for an unspecified colour space, plus `RGBGamut` |
| `Linen.Data.Colour.RGBSpace` | RGB colour coordinate systems: `RGBSpace`, `TransferFunction` (+ `append`/`linear`) |
| `Linen.Data.Colour.RGBSpace.HSL` | HSL (hue-saturation-lightness) colours: `hslView`, RGB↔HSL conversion |
| `Linen.Data.Colour.RGBSpace.HSV` | HSV (hue-saturation-value) colours: `hsvView`, RGB↔HSV conversion |
| `Linen.Data.Colour.SRGB` | `Colour`s in accordance with the sRGB standard; `sRGB24`/`toSRGB24` quantized to `UInt8` |
| `Linen.Data.Colour.SRGB.Linear` | a linear colour space with sRGB's gamut, built from the CIE illuminant D65 |
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
| `Linen.Data.Time.Calendar` | proleptic Gregorian calendar `Day` (Modified-Julian-Day count), `fromGregorian`/`toGregorian`/`fromGregorianValid`, built on `Std.Time.Date.PlainDate` |
| `Linen.Data.Time.Clock` | UTC time/durations: `NominalDiffTime` (`Std.Time.Duration`), `UTCTime` (`Std.Time.DateTime.Timestamp`; `getCurrentTime` is genuine wall-clock time via `Timestamp.now`, `diffUTCTime`/`addUTCTime`) |
| `Linen.Data.Time.LocalTime` | `TimeOfDay` (`Std.Time.Time.PlainTime`) and `TimeZone` (signed-offset minutes, `Std.Time.Zoned.TimeZone.Offset`) |
| `Linen.Time.Calendar.CalendarDiffDays` | a `(months, days)` calendrical period, `Semigroup`/`Monoid` under addition, `calendarDay`/`calendarWeek`/`calendarMonth`/`calendarYear` constants, scale-by-integer |
| `Linen.Time.Calendar.Month` | an absolute month counter since a fixed origin, `addMonths`/`diffMonths`, `periodFirstDay`/`periodLastDay`/`dayPeriod` relating it to `Std.Time.Date.PlainDate` |
| `Linen.Time.Calendar.Quarter` | `QuarterOfYear` and an absolute `Quarter` counter, `addQuarters`/`diffQuarters`, `monthQuarter`/`dayQuarter` |
| `Linen.Time.Calendar.Julian` | the proleptic Julian calendar: its own leap-year rule (no Gregorian century correction), month lengths, `addJulianMonthsClip`/`RollOver`/`addJulianYearsClip`/`RollOver` arithmetic |
| `Linen.Time.Calendar.Easter` | the Gregorian and Orthodox Easter-date algorithms (`gregorianEaster`/`orthodoxEaster`, `sundayAfter`), per Reingold & Dershowitz |
| `Linen.Time.CalendarDiffTime` | the time-valued sibling of `CalendarDiffDays`: `(months, Duration)` instead of `(months, days)` |
| `Linen.Time.UniversalTime` | `UT1` mean solar time as a Modified-Julian-Date-plus-fraction rational, with longitude-parameterised conversion to/from `Std.Time`'s civil wall-clock time |
| `Linen.Time.Clock.TAI` | `AbsoluteTime` (a TAI instant) and day-keyed `LeapSecondMap`, TAI↔UTC conversion accounting for leap seconds |
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
| `Linen.Data.Json.Types` | the `Value` JSON AST (`null`/`bool`/`number`/`string`/`array`/`object`) plus the `ToJSON`/`FromJSON` classes |
| `Linen.Data.Json.Encode` | `Value → String` rendering (`encode`/`encodePretty`) |
| `Linen.Data.Json.Decode` | a `String → Except String Value` parser (`decode`), plus `decodeAs` via `FromJSON` |
| `Linen.Data.Json` | JSON AST, `ToJSON`/`FromJSON`, encode/decode + roundtrip proofs |
| `Linen.System.Console.Ansi` | ANSI terminal colors and styles |
| `Linen.System.Exit` | `ExitCode` (success/failure) + `exitWith`/`exitSuccess`/`exitFailure` over `IO.Process.exit` |
| `Linen.System.Keychain` | OS credential store (`keyring` crate): `Entry`, `setPassword`/`getPassword`/`deleteCredential`, `setSecret`/`getSecret` (macOS Keychain / libsecret / Credential Manager FFI) |
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
| `Linen.Database.SQLite.Bindings.Types` | opaque `Database`/`Statement` handles + SQLite result-code (`Error`) and `ColumnType`/`StepResult` enums |
| `Linen.Database.SQLite.Bindings` | `@[extern]` bindings to the raw `sqlite3_*` C API (vendored amalgamation, no pkg-config) |
| `Linen.Database.SQLite.Direct` | `Except Error`-returning wrapper over `Bindings`; pairs a `Statement` with its owning `Database` |
| `Linen.Database.SQLite` | public `IO`-throwing SQLite3 API: `SQLError`, untyped `SQLData`, `bind`/`bindNamed`, `columns` |
| `Linen.Database.SQLite.Simple.Types` | `Query` (`Coe String Query`), `Null`, folded-in `Only`, row-cons `(:.)` |
| `Linen.Database.SQLite.Simple.Ok` | error-accumulating `Ok` (`Except (Array String)`-isomorphic): `Functor`/`Applicative`/`Alternative`/`Monad`, `toExcept`/`ofExcept` |
| `Linen.Database.SQLite.Simple.Time.Implementation` | SQLite date/time text parsing/rendering (`Std.Internal.Parsec.String`) to/from `Day`/`UTCTime`/`TimeOfDay`/`TimeZone` |
| `Linen.Database.SQLite.Simple.Time` | thin re-export facade over `Time.Implementation` |
| `Linen.Database.SQLite.Simple.Internal` | `Connection`/`Statement` handle pairs over `Database.SQLite3`; `Field` (decoded value + column index/name + declared type) |
| `Linen.Database.SQLite.Simple.ToField` | `ToField` class: numeric/`Bool`/`String`/`ByteArray`/`Option`/`Null`/`Day`/`UTCTime` → `SQLData` |
| `Linen.Database.SQLite.Simple.FromField` | `FromField` class: `Field` → Lean value, `ResultError` folded into `Ok.errors` via `returnError` |
| `Linen.Database.SQLite.Simple.FromRow` | applicative `RowParser`, `FromRow` class, tuple/`Only`/`Cons` instances up to arity 7 |
| `Linen.Database.SQLite.Simple.ToRow` | `ToRow` class, `Unit`/`Only`/tuple (up to arity 7)/`Cons` instances → `Array SQLData` |
| `Linen.Database.SQLite.Simple.QQ` | the `sql "…"` syntax/`macro_rules` substitute for upstream's Template-Haskell quasiquoter, elaborating directly to `Query.ofString` |
| `Linen.Database.SQLite.Simple` | the public facade: `withConnection`, `query`/`query_`/`execute`/`execute_`, `fold`/`fold_`, `withTransaction`/`withImmediateTransaction`/`withExclusiveTransaction`/`withSavepoint`, `lastInsertRowId`/`changes`/`totalChanges` |
| `Linen.Database.SQLite.Simple.Function` | user-defined scalar SQL function registration, `createFunction0`–`createFunction3`/`deleteFunction`, built on `sqlite3_create_function_v2` |
| `Linen.Database.DuckDB.FFI.Types` | opaque `Database`/`Connection`/`Result`/`PreparedStatement`/`Appender`/`LogicalType`/`DataChunk`/`Vector` handles; `duckdb_state`/`duckdb_type`/`duckdb_error_type`/`duckdb_statement_type` enums |
| `Linen.Database.DuckDB.FFI.OpenConnect` | open/close a `Database` and `Connection` (`ffi/duckdb_shim.c`, links `libduckdb`), idempotent `close` |
| `Linen.Database.DuckDB.FFI.Configuration` | `duckdb_config` create/destroy/`set`, always passed as `NULL` in this batch's scope |
| `Linen.Database.DuckDB.FFI.ErrorData` | `duckdb_error_data` inspection (`hasError`/`errorMessage`/`errorType`), `duckdb_result` error accessors |
| `Linen.Database.DuckDB.FFI.Logging` | `duckdb_log`-style logging; first Lean-closure-called-from-C trampoline pair in this port |
| `Linen.Database.DuckDB.FFI.Catalog` | catalog/table-existence and search-path introspection |
| `Linen.Database.DuckDB.FFI.FileSystem` | `duckdb_extract_statements`/file-system helper bindings |
| `Linen.Database.DuckDB.FFI.Helpers` | shared marshalling helpers: owned/borrowed string decoding, `ByteArray`↔`duckdb_string_t` |
| `Linen.Database.DuckDB.FFI.LogicalTypes` | build/inspect `duckdb_logical_type`: primitive/`LIST`/`ARRAY`/`MAP`/`STRUCT`/`UNION`/`ENUM`/`DECIMAL`, alias, decimal width/scale, enum dictionary, child types |
| `Linen.Database.DuckDB.FFI.BindValues` | binding scalar/`NULL` parameter values to a `PreparedStatement` (`duckdb_bind_*`) |
| `Linen.Database.DuckDB.FFI.PreparedStatements` | prepare/destroy statements, parameter count/name/type, `clearBindings`, result-set metadata |
| `Linen.Database.DuckDB.FFI.QueryExecution` | `duckdb_query` direct SQL execution, result-set inspection, rows-changed, malformed-query error reporting |
| `Linen.Database.DuckDB.FFI.ExecutePrepared` | execute a bound `PreparedStatement`, materialize/stream its `Result` |
| `Linen.Database.DuckDB.FFI.DataChunk` | `duckdb_data_chunk` create/destroy, column count/vector access, row-count get/set |
| `Linen.Database.DuckDB.FFI.Vector` | typed scalar get/set into a `duckdb_vector`'s data buffer, raw `ByteArray` access, string assignment, `LIST`/`STRUCT` child vectors |
| `Linen.Database.DuckDB.FFI.Validity` | validity-mask accessors (`rowIsValid`/`setRowValidity`/`setRowValid`/`setRowInvalid`, `ensureValidityWritable`) |
| `Linen.Database.DuckDB.FFI.Appender` | bulk-insert `duckdb_appender` lifecycle and per-column `append*` value writers |
| `Linen.Database.DuckDB.FFI.ScalarFunctions` | user-defined scalar SQL function registration (`ScalarFunctionSet`), second Lean-closure-called-from-C trampoline pair, closure retrieved via `duckdb_scalar_function_get_extra_info` |
| `Linen.Database.DuckDB.Simple.Internal` | `Connection`/`SQLError`, bracket-style handle accessors, delete-callback/`StablePtr` registration helpers |
| `Linen.Database.DuckDB.Simple.LogicalRep` | building/destroying `duckdb_logical_type` descriptors (struct/list/map/enum/decimal) |
| `Linen.Database.DuckDB.Simple.Ok` | the error-accumulating `Ok a \| Errors [SomeException]` applicative |
| `Linen.Database.DuckDB.Simple.Types` | `Query`, `Null`, `FormatError`, `(:.)`, folded-in `Only` and 128-bit `UUID` |
| `Linen.Database.DuckDB.Simple.FromField` | `Field`, `FieldValue` (every DuckDB logical type), the `FromField` class, `ResultError` |
| `Linen.Database.DuckDB.Simple.Materialize` | converts a `duckdb_vector`/`data_chunk` column into a `FieldValue` per row (internal) |
| `Linen.Database.DuckDB.Simple.ToField` | `DuckDBColumnType`, `FieldBinding`, `NamedParam`, the `ToField` class binding into `BindValues` |
| `Linen.Database.DuckDB.Simple.FromRow` | applicative row-consuming parser |
| `Linen.Database.DuckDB.Simple.ToRow` | dual of `FromRow`, encoding parameters via `ToField` |
| `Linen.Database.DuckDB.Simple.Catalog` | catalog/table-existence and search-path queries |
| `Linen.Database.DuckDB.Simple.Config` | connection-config key/value setting prior to `open` |
| `Linen.Database.DuckDB.Simple.FileSystem` | registering a virtual filesystem callback set |
| `Linen.Database.DuckDB.Simple.Logging` | log-callback registration |
| `Linen.Database.DuckDB.Simple.Copy` | bulk row-append ("COPY"-style bulk load) via `Appender` |
| `Linen.Database.DuckDB.Simple.Function` | user-defined scalar SQL function registration via `ScalarFunctions` |
| `Linen.Database.DuckDB.Simple.Generic` | hand-written `STRUCT`/`UNION` decode combinators standing in for GHC-`Generic`-derived instances |
| `Linen.Database.DuckDB.Simple` | public facade: `withConnection`, `query`/`query_`/`execute`/`execute_`, `fold`/`fold_`, `withTransaction` |
| `Linen.Crypto.JOSE.FFI` | `@[extern]` OpenSSL bindings: HMAC, RSA/EC verify, JWK→DER key build, base64url (`ffi/jose.c`) |
| `Linen.Crypto.JOSE.Types` | JOSE/JWT/JWK types: `JWSAlgorithm`/`ECCurve`/`JWKKeyType`, proof-carrying `JWK`, `ClaimsSet`, `JWSHeader`, `JwtError` + laws |
| `Linen.Crypto.JOSE.JWK` | JWK helpers: `parseOctKey` (base64url), `toDerPublicKey` (RSA/EC → DER via OpenSSL) |
| `Linen.Crypto.JOSE.JWS` | JWS compact verification (RFC 7515): `splitCompact`, `verifySignature` (HMAC/RSA/EC via OpenSSL) |
| `Linen.Crypto.JOSE.JWT` | JWT verification (RFC 7519): `validateClaims` (exp/nbf/aud/iss, bounded skew), `verifyJWT` (signature + claims) |
| `Linen.Crypto.SHA256` | `@[extern]` OpenSSL SHA-256 digest (`ffi/jose.c`, reusing `Crypto.JOSE.FFI`'s OpenSSL link) |
| `Linen.Crypto.SecureRandom` | `@[extern]` OpenSSL `RAND_bytes` CSPRNG (`ffi/jose.c`, reusing `Crypto.JOSE.FFI`'s OpenSSL link) |
| `Linen.Network.HTTP.Client.Contrib` | `handleResponse`/`handleResponseJSON` response-handling helpers shared by the OAuth2 HTTP flows |
| `Linen.Network.OAuth2.Internal` | `OAuth2` client config, `AccessToken`/`RefreshToken`/`IdToken`/`ExchangeToken` newtypes, `ClientAuthenticationMethod`, URI/request helpers |
| `Linen.Network.OAuth2.AuthorizationRequest` | authorization-code redirect URL builder (RFC 6749 §4.1.1) |
| `Linen.Network.OAuth2.HttpClient` | bearer/basic-auth request application, JSON-decoding HTTP calls over `Network.HTTP.Client.Conduit` |
| `Linen.Network.OAuth2.TokenRequest` | `TokenResponse`/`OAuth2Token` parsing, `fetchAccessToken`/refresh |
| `Linen.Network.OAuth2` | facade re-exporting `.Internal`/`.AuthorizationRequest`/`.HttpClient`/`.TokenRequest` |
| `Linen.Network.OAuth2.Experiment.Utils` | query-parameter map flattening, URI-to-text helpers |
| `Linen.Network.OAuth2.Experiment.Pkce` | RFC 7636 PKCE: `CodeVerifier`/`CodeChallenge`/`S256` challenge method |
| `Linen.Network.OAuth2.Experiment.Types` | typed OAuth2-request-builder machinery: `IdpApplication` family, `GrantTypeFlow`, phantom-typed application config |
| `Linen.Network.OAuth2.Experiment.Flows.UserInfoRequest` | `HasUserInfoRequest` marker class |
| `Linen.Network.OAuth2.Experiment.Flows.AuthorizationRequest` | `AuthorizationRequestParam` + `ToQueryParam` instance |
| `Linen.Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest` | `DeviceCode`, `DeviceAuthorizationResponse` (RFC 8628 §3.2), `DeviceAuthorizationRequestParam` |
| `Linen.Network.OAuth2.Experiment.Flows.TokenRequest` | `HasTokenRequest`/`HasClientAuthenticationMethod`/`NoNeedExchangeToken`, `addSecretToHeader` |
| `Linen.Network.OAuth2.Experiment.Flows.RefreshTokenRequest` | `RefreshTokenRequest` + `ToQueryParam`, `HasRefreshTokenRequest` |
| `Linen.Network.OAuth2.Experiment.Flows` | the real HTTP-performing flows: token/device-authorization/refresh/user-info requests, RFC 8628 §3.5 device-token polling |
| `Linen.Network.OAuth2.Experiment.Grants.ClientCredentials` | client-credentials grant `IdpApplication` instance (RFC 6749 §4.4) |
| `Linen.Network.OAuth2.Experiment.Grants.DeviceAuthorization` | device-authorization grant `IdpApplication` instance (RFC 8628) |
| `Linen.Network.OAuth2.Experiment.Grants.JwtBearer` | JWT-bearer grant `IdpApplication` instance (RFC 7523) |
| `Linen.Network.OAuth2.Experiment.Grants.ResourceOwnerPassword` | resource-owner-password grant `IdpApplication` instance (RFC 6749 §4.3) |
| `Linen.Network.OAuth2.Experiment.Grants.AuthorizationCode` | authorization-code grant `IdpApplication` instance (RFC 6749 §4.1), with PKCE |
| `Linen.Network.OAuth2.Experiment.Grants` | facade re-exporting the five grant modules |
| `Linen.Network.OAuth2.Experiment` | top-level facade re-exporting `Types`/`Grants`/`Flows`/`Pkce`/`Utils` |
| `Linen.Crypto.Zlib.FFI` | `@[extern]` zlib inflate-only FFI (`ffi/zlib.c`): opaque `Inflate` handle, `initInflate`/`feedInflate`/`finishInflate`, one-shot `decompress` |
| `Linen.Crypto.MD5` | RFC 1321 MD5 digest: pure, structurally-recursive `hash` (64-round compression over fixed 64-byte blocks) |
| `Linen.Crypto.RC4` | RC4 stream cipher: `initCtx` (KSA, 256-byte S-box) + `combine` (PRGA keystream XOR), both structurally recursive |
| `Linen.Crypto.AES` | AES-128 block cipher: Rijndael key schedule (`initAES`), CBC `decryptCBC`, PKCS5 `unpadPKCS5` |
| `Linen.Data.PDF.Stream` | buffer-resident `io-streams` port: `InputStream`/`OutputStream` over `ByteArray`, `fromByteString`/`makeInputStream`/`countInput`/`takeBytes`/`decompress` |
| `Linen.Data.PDF.Core.Name` | atomic PDF name objects (§7.3.5): byte-string wrapper `Name`, `make`/`toByteString` |
| `Linen.Data.PDF.Core.Exception` | structured parse-error reporting: `corrupted`/`unexpected` tagged `Exc`, rendered into `IO.Error.userError` with growing `details` context |
| `Linen.Data.PDF.Core.Parsers.Util` | shared low-level parsing combinators over `Std.Internal.Parsec.ByteArray`: `isSpace_w8`, eof-tolerant `skipWhile'` |
| `Linen.Data.PDF.Core.IO.Buffer` | cursor-based file/byte-source abstraction: `read`/`size`/`seek`/`back`/`tell`, `toInputStream` bridge to `Data.PDF.Stream` |
| `Linen.Data.PDF.Core.Object` | PDF object model (§7.3): recursive `Object` sum type (number/bool/name/dict/array/string/stream/ref/null), `Dict = HashMap Name Object` |
| `Linen.Data.PDF.Core.Object.Util` | safe `Object` accessors: `intValue`/`stringValue`/`nameValue`/`dictValue`/… total `Option`-returning views |
| `Linen.Data.PDF.Core.Object.Builder` | render `Object` to bytes: total `buildObject`/`buildDict`/`buildArray` (`Except String Builder`, stream case errors instead of panicking) |
| `Linen.Data.PDF.Core.Parsers.Object` | parse `Object` values: numbers/strings/names/dicts/arrays/refs/streams, `attempt`-wrapped backtracking over `Std.Internal.Parsec` |
| `Linen.Data.PDF.Core.Parsers.XRef` | parsers for the xref table/trailer: classic table rows plus `startXRef`'s fuel-bounded scan-and-take-last search |
| `Linen.Data.PDF.Core.Util` | unclassified parsing tools: `readCompressedObject` (object-stream header pairs), totality-safe `last` |
| `Linen.Data.PDF.Core.Stream.Filter.Type` | the `StreamFilter` type: filter name + `/DecodeParms`-driven decoder function |
| `Linen.Data.PDF.Core.Stream.Filter.FlateDecode` | the `FlateDecode` filter (§7.4.4): zlib inflate + PNG-Up (`Predictor 12`) / no-op (`1`) predictor reversal |
| `Linen.Data.PDF.Core.Stream` | stream-related tools: `readStream`, `knownFilters`, decode dispatch tying dict + raw data + filters together |
| `Linen.Data.PDF.Core.XRef` | cross-reference index (§7.5.4/§7.5.8): table/stream variants, `/Prev`-chained incremental updates |
| `Linen.Data.PDF.Core.Encryption` | PDF Standard Security Handler (§7.6.3.3, revisions 2–4): RC4/AES-128-CBC decryptor construction over `Crypto.RC4`/`Crypto.MD5`/`Crypto.AES` |
| `Linen.Data.PDF.Core.File` | a PDF file as a set of objects: `findObject` (xref-chain resolution, object streams, decryption), `streamContent`/`rawStreamContent` |
| `Linen.Data.PDF.Core` | thin re-export aggregator: `Object`, `File`, `Encryption` |
| `Linen.Data.PDF.Core.Types` | compound data structures (§7.9): generic `Rectangle`, date/matrix helpers |
| `Linen.Data.PDF.Core.Writer` | write PDF files: `writeHeader`/`writeObject`/`writeStream`/`writeXRefTable`/`writeXRefStream`, incremental-update support |
| `Linen.Data.PDF.Content.Transform` | 2D affine transforms (§8.3.4 `cm` matrices): compose/apply row-vector matrix algebra |
| `Linen.Data.PDF.Content.Ops` | content stream operators (Annex A): closed `Op` enum + `UnknownOp` fallback, `toOp` classifier |
| `Linen.Data.PDF.Content.FontDescriptor` | font metrics beyond glyph widths (§9.8): `FontDescriptor` record, flags, weight/stretch |
| `Linen.Data.PDF.Content.GlyphList` | the Adobe Glyph List: ~4280-entry glyph-name → Unicode table |
| `Linen.Data.PDF.Content.TexGlyphList` | supplementary TeX glyph-name table (~284 entries) not covered by the AGL |
| `Linen.Data.PDF.Content.Encoding.WinAnsi` | the WinAnsiEncoding table (Annex D.2): code → Unicode for simple fonts |
| `Linen.Data.PDF.Content.Encoding.MacRoman` | the MacRomanEncoding table (Annex D.5) |
| `Linen.Data.PDF.Content.Encoding.PdfDoc` | the PDFDocEncoding table (Annex D.3), codes 127/159/173 left undefined |
| `Linen.Data.PDF.Content.UnicodeCMap` | `ToUnicode` CMap parsing: codespace ranges + `bfchar`/`bfrange` big-endian UTF-16 decoding |
| `Linen.Data.PDF.Content.Parser` | parse a content stream into operators: `parseContent`, glue operand runs into complete `Operator`s |
| `Linen.Data.PDF.Content.Processor` | interpret content-stream operators, tracking graphics state (`processOp`, text position/matrices) |
| `Linen.Data.PDF.Content.FontInfo` | font metadata for glyph decoding: simple vs. composite (Type 0/CID) `FontInfo`, `fontInfoDecodeGlyphs` |
| `Linen.Data.PDF.Content` | thin re-export aggregator: content-stream operators, parsing, processing, font info |
| `Linen.Data.PDF.Document.Types` | thin re-export of `Data.PDF.Core.Types` |
| `Linen.Data.PDF.Document.Internal.Types` | internal `Pdf`/`Document`/`Info` record declarations shared across the document API |
| `Linen.Data.PDF.Document.Internal.Util` | internal utilities: `ensureType`/`dictionaryType`, `decodeTextString` (UTF-16BE/PDFDocEncoding text strings, §7.9.2.2) |
| `Linen.Data.PDF.Document.Pdf` | the top-level PDF handle: cached `lookupObject`/`deref`, `document` (checks encryption first) |
| `Linen.Data.PDF.Document.Document` | trailer-dictionary accessors: `documentCatalog`/`documentInfo`/`documentEncryption` |
| `Linen.Data.PDF.Document.Info` | the document information dictionary (§14.3.3): `infoTitle`/`infoAuthor`/`infoSubject`/`infoKeywords`/`infoCreator`/`infoProducer` |
| `Linen.Data.PDF.Document.Catalog` | the document catalog: `catalogPageNode` (§7.7.2 `/Pages` entry) |
| `Linen.Data.PDF.Document.PageNode` | page-tree nodes (§7.7.3.2): `pageNodeKids`/`pageNodeParent`, fuel + visited-`Ref`-set `pageNodePageByNum` descent guarding against cyclic `/Kids` |
| `Linen.Data.PDF.Document.FontDict` | font dictionaries (§9.6): `fontDictSubtype`, `fontDictLoadInfo` (simple/composite dispatch into `Data.PDF.Content.FontInfo`) |
| `Linen.Data.PDF.Document.Page` | PDF document pages: `pageMediaBox` (fuel-bounded `/Parent` ascent), `pageExtractText`/`pageExtractGlyphs`, Form-XObject `/Resources` resolution |
| `Linen.Data.PDF.Document` | thin re-export aggregator: `Pdf`, `Document`, `Catalog`, `PageNode`, `Page`, `Info`, `FontDict` |
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
| `Linen.Graphics.Netpbm` | `netpbm`-style parser for the PBM/PGM/PPM "portable anymap" image formats (ASCII/binary `P1`–`P6`) over `ByteArray`, via `Std.Internal.Parsec` |
| `Linen.Codec.Picture.Types` | `Image`/`MutableImage`/`DynamicImage`, pixel classes, and every concrete pixel type |
| `Linen.Codec.Picture.VectorByteConversion` | `Array UInt8` ↔ `ByteArray` conversion |
| `Linen.Codec.Picture.InternalHelper` | small `ByteString`/file-loading helpers |
| `Linen.Codec.Picture.BitWriter` | MSB-first bit-level reader/writer for JPEG/PNG/GIF |
| `Linen.Codec.Picture.Metadata.Exif` | Exif tag vocabulary and TIFF-embedded Exif directory parsing |
| `Linen.Codec.Picture.Metadata` | the generic `Metadatas`/`Keys`/`Elem` map attached to decoded images |
| `Linen.Codec.Picture.ColorQuant` | median-cut colour quantization (GIF/PNG palette generation) |
| `Linen.Codec.Picture.Bitmap` | BMP decode/encode |
| `Linen.Codec.Picture.Tga` | TGA decode/encode |
| `Linen.Codec.Picture.HDR` | Radiance HDR decode/encode |
| `Linen.Codec.Picture.Png.Internal.Type` | PNG chunk structure, `ChunkSignature`, filter types |
| `Linen.Codec.Picture.Png.Internal.Metadata` | PNG ancillary-chunk ↔ `Metadata` conversion |
| `Linen.Codec.Picture.Png.Internal.Export` | image → PNG chunk encoding |
| `Linen.Codec.Picture.Png` | top-level PNG decode/encode over zlib inflate/deflate |
| `Linen.Codec.Picture.Tiff.Internal.Types` | TIFF IFD/tag structure |
| `Linen.Codec.Picture.Tiff.Internal.Metadata` | TIFF tag ↔ `Metadata` conversion |
| `Linen.Codec.Picture.Tiff` | top-level TIFF decode/encode |
| `Linen.Codec.Picture.Gif.Internal.LZW` | GIF LZW decompression |
| `Linen.Codec.Picture.Gif.Internal.LZWEncoding` | GIF LZW compression |
| `Linen.Codec.Picture.Gif` | top-level GIF decode/encode, including animated GIFs, with palette quantization on encode |
| `Linen.Codec.Picture.Jpg.Internal.DefaultTable` | standard JPEG quantization/Huffman tables |
| `Linen.Codec.Picture.Jpg.Internal.Types` | JPEG marker/segment/scan structure |
| `Linen.Codec.Picture.Jpg.Internal.Common` | shared JPEG decode helpers |
| `Linen.Codec.Picture.Jpg.Internal.FastDct` | integer DCT |
| `Linen.Codec.Picture.Jpg.Internal.FastIdct` | integer IDCT |
| `Linen.Codec.Picture.Jpg.Internal.Metadata` | JFIF/Exif ↔ `Metadata` conversion |
| `Linen.Codec.Picture.Jpg.Internal.Progressive` | progressive-scan JPEG decoding |
| `Linen.Codec.Picture.Jpg` | top-level JPEG decode/encode (baseline + progressive) |
| `Linen.Codec.Picture.Saving` | format-agnostic "save with extension" dispatch over every codec |
| `Linen.Codec.Picture` | the package's public re-export facade |
| `Linen.Graphics.Image.Utils` | small numeric/list helpers with no `Graphics.Image.*` dependency of their own |
| `Linen.Graphics.Image.Interface.Elevator` | the `Elevator` class: precision-changing conversions between pixel component types, plus `clamp01` |
| `Linen.Graphics.Image.Interface` | the central `Pixel`/`ColorSpace`/`AlphaSpace` classes, a `Manifest`-backed `Image` type, and generic border-handling indexing |
| `Linen.Graphics.Image.ColorSpace.Y` | single-channel luma colour space `Y`, plus its alpha-carrying counterpart `YA` |
| `Linen.Graphics.Image.ColorSpace.RGB` | three-channel red/green/blue colour space, plus `RGBA` |
| `Linen.Graphics.Image.ColorSpace.HSI` | three-channel hue/saturation/intensity colour space, plus `HSIA` |
| `Linen.Graphics.Image.ColorSpace.CMYK` | four-channel cyan/magenta/yellow/key colour space, plus `CMYKA` |
| `Linen.Graphics.Image.ColorSpace.YCbCr` | three-channel luma/chroma colour space, plus `YCbCrA` |
| `Linen.Graphics.Image.ColorSpace.Complex` | complex-valued pixels over any existing colour space, used by the FFT-based processing modules |
| `Linen.Graphics.Image.ColorSpace.X` | a generic, "unlabeled" single-channel colour space used as a building block, distinct from luma `Y` |
| `Linen.Graphics.Image.ColorSpace.Binary` | bit-valued binary pixels built on `X`, for thresholding/morphology |
| `Linen.Graphics.Image.ColorSpace` | the colour-space facade: re-exports every colour space, plus the cross-colour-space conversion matrix |
| `Linen.Graphics.Image.Processing.Interpolation` | sampling an image at a non-integer coordinate (nearest-neighbour/bilinear) |
| `Linen.Graphics.Image.Processing.Geometric` | resampling, cropping, flipping, rotation, and resizing of images |
| `Linen.Graphics.Image.Processing.Complex.Fourier` | the 2-D fast Fourier transform (and its inverse) on complex-pixel images |
| `Linen.Graphics.Image.Processing.Complex` | whole-image complex-pixel operations, plus the `fft`/`ifft` re-export |
| `Linen.Graphics.Image.Processing.Convolution` | kernel convolution/correlation of an image |
| `Linen.Graphics.Image.Processing.Filter` | named filter kernels (Sobel, Gaussian, Laplacian, …) built on convolution |
| `Linen.Graphics.Image.Processing.Binary` | binary-image construction and morphology (erode/dilate/opening/closing) |
| `Linen.Graphics.Image.Processing` | the processing facade: re-exports `Geometric`/`Interpolation`/`Convolution`/`Filter`, plus its own `pixelGrid` |
| `Linen.Graphics.Image.IO.Base` | shared reader/writer typeclasses and pixel-precision normalisation used by every format backend |
| `Linen.Graphics.Image.IO.Formats.JuicyPixels` | glue between `Image cs e` and `Linen.Codec.Picture`, reusing it for PNG/JPEG/GIF/BMP/TIFF/TGA/HDR decode/encode |
| `Linen.Graphics.Image.IO.Formats.Netpbm` | glue between `Image cs e` and `Linen.Graphics.Netpbm`, reusing it for PNM/PGM/PPM decode |
| `Linen.Graphics.Image.IO.Formats` | the format-dispatch facade tying together every JuicyPixels-backed and Netpbm-backed format tag |
| `Linen.Graphics.Image.IO` | the top-level file-I/O facade: format-guessing read/write wrappers |
| `Linen.Graphics.Image.Types` | the package-level type/re-export facade: concrete (colour space × precision) image type aliases |
| `Linen.Graphics.Image` | the top-level public facade of the whole `hip` library |
| `Linen.Graphics.Image.Processing.Ahe` | adaptive (local-rank) histogram equalization |
| `Linen.Graphics.Image.Processing.Hough` | the linear Hough transform for line detection (a vote-count heatmap over discretized angle/distance) |
| `Linen.Graphics.Image.Processing.Noise` | salt-and-pepper (impulse) noise generation, threading Lean core's `StdGen`/`randNat` as a pure, explicit-seed generator |
