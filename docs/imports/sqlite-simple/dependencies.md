# `sqlite-simple` — dependency plan

Upstream: https://hackage.haskell.org/package/sqlite-simple (version
0.4.19.0, the latest published), source at
https://github.com/nurpax/sqlite-simple. Mid-level SQLite client library,
"based on postgresql-simple" — the same relationship `linen`'s existing
`Hasql` port has to raw `libpq`, but for SQLite.

An edge **A → B** means *module A imports module B*, so **B must be built
before A**. Derived from each module's own `import Database.SQLite...` lines,
checked directly against the `sqlite-simple-0.4.19.0` and
`direct-sqlite-2.3.29` tarball sources (not guessed from Haddock).

Namespace note: `Database.SQLite.Simple` names a subject area (a SQLite
client), not Haskell/GHC itself, so no Lean-ification rename is needed —
ported as `Linen.Database.SQLite.Simple.*`, mirroring the existing
`Linen.Database.PostgreSQL.*` naming used by the `Hasql` port.

## Precedence check (`AGENTS.md`'s Hackage-import precedence rule)

- **`direct-sqlite` is folded directly into this same dependency list**
  (items 1–4 below), exactly the way `docs/imports/Hasql/dependencies.md`
  folds `Database.PostgreSQL.LibPQ`/`LibPQ.Types` into the `Hasql` list
  rather than giving the raw C-binding package its own
  `docs/imports/direct-sqlite/` folder or `index.md` entry: `direct-sqlite`
  plays exactly the role `postgresql-libpq` plays for `Hasql` — a small (4
  modules), single-purpose raw FFI binding to the underlying C library that
  is *only* ever consumed by the one "simple" wrapper package, never
  independently. Unlike `duckdb-ffi` (see `docs/imports/duckdb-ffi/
  dependencies.md`), 4 modules is well below the size where a separate
  index entry earns its keep.
- **`base`, `bytestring`, `text`, `containers`, `time`** — already ported
  (`Base`, `ByteString`, `Text`, `Containers`, `Time` per `docs/imports/
  index.md`).
- **`transformers`** — Lean's own `ExceptT`/`ReaderT`/`StateT`, per the same
  substitution already recorded in `docs/imports/hip/dependencies.md`'s
  external-dependencies note.
- **`exceptions`** (`Control.Monad.Catch`'s `MonadThrow`/`MonadCatch`) —
  covered by Lean's native `Except`/`IO` exception handling plus the
  already-ported `Mtl`; no separate port needed.
- **`attoparsec`** — covered by `Std.Internal.Parsec`, per the same
  substitution already recorded for `netpbm`/`JuicyPixels` in `docs/imports/
  hip/dependencies.md`. Used by `Database.SQLite.Simple.Time.Implementation`
  to parse SQLite's textual date/time formats.
- **`blaze-builder`, `blaze-textual`** — fast `ByteString`/`Builder`
  rendering of numeric/date values, used only internally by `Time.
  Implementation`'s formatter. Lean's `String`/`ByteArray` formatting is
  native and needs no separate builder library; dropped as a stdlib
  substitution, not ported.
- **`template-haskell`** — used by exactly one module,
  `Database.SQLite.Simple.QQ`, to define the `sql` quasiquoter (a
  compile-time syntax check on embedded SQL string literals). GHC Template
  Haskell itself has no Lean analogue, but the *feature* isn't out of
  scope: Lean's own `macro`/`syntax`/elaboration mechanism is the natural,
  idiomatic replacement for a quasiquoter and is a **substitution**, not a
  scope cut — `Linen.Database.SQLite.Simple.QQ` is ported using Lean
  `syntax`/`macro_rules` rather than dropped.
- **`Only`** (a separate, single-purpose Hackage package: a 1-constructor
  `newtype Only a = Only a` tuple wrapper, re-exported by
  `Database.SQLite.Simple.Types`) — folded directly into the port of
  `Database.SQLite.Simple.Types` below, the same way `crypto-api` was
  folded into the `cipher-aes` port (`docs/imports/index.md` entry 64): too
  small (one trivial newtype) to justify its own `docs/imports/Only/`
  folder or `index.md` entry.

## Module list (topological order)

`direct-sqlite` (raw SQLite3 C FFI binding — folded in, see precedence note):

1. `Database.SQLite3.Bindings.Types` → `Linen.Database.SQLite.Bindings.Types`
   — **new port.** `Ptr`-level C struct/enum declarations
   (`sqlite3`/`sqlite3_stmt` opaque handles, result-code enum, column-type
   enum, `sqlite3_destructor_type`, …) generated upstream via `hsc2hs`
   against the bundled `cbits/sqlite3.h`. Ported as plain Lean `opaque`
   pointer wrappers and inductive enums; no `hsc2hs`-equivalent needed since
   Lean's own FFI declares C struct access directly in `ffi/sqlite3.c`
   (see "Native C library" below).
2. `Database.SQLite3.Bindings` → `Linen.Database.SQLite.Bindings` — **new
   port**, on #1. Direct `@[extern]`-style declarations of the raw
   `sqlite3_*` C entry points (`sqlite3_open_v2`, `sqlite3_prepare_v2`,
   `sqlite3_step`, `sqlite3_bind_*`, `sqlite3_column_*`, `sqlite3_close_v2`,
   …), mirroring how `Linen/Database/PostgreSQL/LibPQ.lean` wraps raw
   `libpq` entry points.
3. `Database.SQLite3.Direct` → `Linen.Database.SQLite.Direct` — **new
   port**, on #2. Thin `ByteString`-based (non-UTF-8-decoding) wrapper
   layer with explicit error-code checking.
4. `Database.SQLite3` → `Linen.Database.SQLite` — **new port**, on #3.
   The public UTF-8/`Text`-based low-level API (`open`, `close`, `exec`,
   `prepare`, `step`, `bind`, `column`, `finalize`, `interrupt`, `trace`,
   user-defined function registration, …).

`sqlite-simple` proper:

5. `Database.SQLite.Simple.Types` → `Linen.Database.SQLite.Simple.Types` —
   **new port.** `Query` (a `Text`-newtype SQL statement wrapper with an
   `IsString` instance), `Null`, and the row-cons operator `(:.)`, plus the
   folded-in `Only` newtype (see precedence note). No dependency on any
   other `sqlite-simple` module.
6. `Database.SQLite.Simple.Ok` → `Linen.Database.SQLite.Simple.Ok` — **new
   port.** The `Ok`/error-accumulating applicative used by `FromField`/
   `FromRow` conversions (`Ok a | Errors [SomeException]`). No dependency on
   any other `sqlite-simple` module; ported using Lean's `Except (Array
   String)`-style representation rather than re-wrapping `SomeException`
   (Lean has no open exception hierarchy to accumulate).
7. `Database.SQLite.Simple.Time.Implementation` →
   `Linen.Database.SQLite.Simple.Time.Implementation` — **new port.**
   Parsing/rendering of SQLite's several accepted textual date-time formats
   (`YYYY-MM-DD`, `YYYY-MM-DD HH:MM:SS[.SSS]`, ISO 8601 `T`-separated, plus
   Julian-day and Unix-epoch numeric forms) to/from `Time`'s `Day`/
   `UTCTime`/`TimeOfDay`. Built on `Std.Internal.Parsec` (substituting
   `attoparsec`, see precedence note) and `Linen`'s existing `Time` port.
8. `Database.SQLite.Simple.Time` →
   `Linen.Database.SQLite.Simple.Time` — **new port**, on #7. Thin
   re-export facade.
9. `Database.SQLite.Simple.Internal` →
   `Linen.Database.SQLite.Simple.Internal` — **new port**, on #4, #6.
   `Connection`, `Statement`, `Field` (a decoded column value plus its
   declared SQLite type and column index/name, for error messages), and
   connection-open/close bookkeeping.
10. `Database.SQLite.Simple.ToField` →
    `Linen.Database.SQLite.Simple.ToField` — **new port**, on #4, #5, #7.
    The `ToField` class and instances (`Int*`/`Word*`/`Double`/`Text`/
    `ByteString`/`Bool`/`Maybe`/`Day`/`UTCTime`/…) converting Lean values to
    `SQLData` for parameter binding.
11. `Database.SQLite.Simple.FromField` →
    `Linen.Database.SQLite.Simple.FromField` — **new port**, on #4, #5, #6,
    #7, #9. The dual `FromField` class/instances decoding a `Field` back
    into a Lean value, plus `ResultError`.
12. `Database.SQLite.Simple.FromRow` →
    `Linen.Database.SQLite.Simple.FromRow` — **new port**, on #6, #9, #11.
    The `FromRow` class (an applicative row-consuming parser over a list of
    `Field`s) and tuple/`(:.)` instances up to the usual small arity.
13. `Database.SQLite.Simple.ToRow` →
    `Linen.Database.SQLite.Simple.ToRow` — **new port**, on #4, #5, #10.
    The dual `ToRow` class and tuple/`Only`/`(:.)` instances.
14. `Database.SQLite.Simple.QQ` → `Linen.Database.SQLite.Simple.QQ` —
    **new port**, on #5. The `sql` quasiquoter, ported via Lean `syntax`/
    `macro_rules` (see precedence note) rather than Template Haskell.
15. `Database.SQLite.Simple` → `Linen.Database.SQLite.Simple` — **new
    port**, on #3, #4, #5, #6, #9, #10, #11, #12, #13. The public facade:
    `open`/`close`/`withConnection`, `query`/`query_`/`execute`/`execute_`,
    `fold`/`fold_`, transactions (`withTransaction`, `withSavepoint`),
    `lastInsertRowId`, `changes`, pretty-printed `SQLError`.
16. `Database.SQLite.Simple.Function` →
    `Linen.Database.SQLite.Simple.Function` — **new port**, on #3, #4, #6,
    #9, #10, #11, #15. User-defined scalar SQL function registration
    (`createFunction`/`deleteFunction`), built on `sqlite3_create_function`.

## Native C library

`direct-sqlite` binds against the SQLite3 C library. Upstream itself defaults
to *bundling* the SQLite amalgamation (`cbits/sqlite3.c`) and compiling it
in-tree (its `systemlib` cabal flag, off by default, switches to the system
library instead) — i.e. vendoring, not system-linking, is upstream's own
default. This port follows that default rather than the `libpq`/`openssl`/
`libsecret-1` pattern (decided with the user 2026-07-12, per `AGENTS.md`'s
FFI note): SQLite ships a small, self-contained, public-domain single
`.c`/`.h` amalgamation, so vendoring it removes the `sqlite3-dev`/Homebrew
dependency entirely and pins the exact version in git, rather than adding a
fifth `pkg-config` probe:

- Vendor `sqlite3.c`/`sqlite3.h` (the amalgamation for a pinned SQLite
  release, matching the version `direct-sqlite-2.3.29` itself bundles) under
  `ffi/vendor/sqlite3/`.
- A new `target sqlite3.o` compiles `ffi/vendor/sqlite3/sqlite3.c` directly
  (no `pkg-config` probe needed — the include path is the vendored directory
  itself), plus a small `ffi/sqlite3_shim.c` for the `@[extern]` entry
  points, which `#include`s the vendored `sqlite3.h`.
- No `pkgLinkFlags`/`nativeLinkArgs` addition needed for SQLite specifically
  (no system library to link against — `sqlite3.o` is self-contained), and no
  `apt-get`/`brew install` step for SQLite in
  `.github/workflows/lean_action_ci.yml`. This makes the SQLite FFI build
  identical on macOS and Linux (and any other platform with a C compiler),
  satisfying `AGENTS.md`'s "must build on at least macOS and Linux"
  requirement more directly than a `pkg-config`-discovered system library
  would.

## Termination notes

Nothing in this module list needs a nontrivial termination argument: the
FFI layer (#1–#4) is direct C calls, `Time.Implementation`'s parser (#7)
runs over a fixed-length input string via `Std.Internal.Parsec`
combinators (structurally decreasing), and `FromRow`/`ToRow` (#12–#13)
consume a statically-known, fixed-arity tuple shape per instance — no
`partial def` or fuel parameter is needed anywhere in this port.

## Tally

- 16 modules total: 4 from `direct-sqlite` (folded in) + 12 from
  `sqlite-simple` itself.
