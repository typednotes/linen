# `duckdb-simple` — dependency plan

Upstream: https://hackage.haskell.org/package/duckdb-simple (version
0.1.5.1, the latest published), source at
https://github.com/Tritlo/duckdb-haskell (`subdir: duckdb-simple`). "A
mid-level interface for interacting with DuckDB, in the style of other
'simple' libraries such as sqlite-simple and postgresql-simple."

**Confirmed independent of `sqlite-simple`/`direct-sqlite`**: checked
`duckdb-simple.cabal`'s `build-depends` directly — it pulls in
`duckdb-ffi`, not `direct-sqlite` or `sqlite-simple`; no import in the
source tree mentions `Database.SQLite*` either. `duckdb-simple` is modeled
*after* `sqlite-simple`'s API shape (hence the near-identical module names:
`Types`, `Ok`, `FromField`, `ToField`, `FromRow`, `ToRow`, `Internal`) but
shares no code or package dependency with it. It is listed after
`sqlite-simple` in `docs/imports/index.md` only because the user asked for
that import order, not because of any real dependency edge.

An edge **A → B** means *module A imports module B*, so **B must be built
before A**. Derived from each module's own `import Database.DuckDB.Simple...`
lines, checked directly against the `duckdb-simple-0.1.5.1` tarball source.

Namespace note: ported as `Linen.Database.DuckDB.Simple.*`, mirroring
`Linen.Database.SQLite.Simple.*` and `Linen.Database.PostgreSQL.*`.

## Precedence check (`AGENTS.md`'s Hackage-import precedence rule)

- **`duckdb-ffi`** — new Hackage dependency, own port; see
  `docs/imports/duckdb-ffi/dependencies.md` (44 modules, too large to fold
  in — unlike `direct-sqlite`).
- **`array`, `base`, `bytestring`, `containers`, `text`, `time`,
  `transformers`** — already ported (`Vector`/Lean `Array`, `Base`,
  `ByteString`, `Containers`, `Text`, `Time`) or Lean's own
  `transformers`-equivalent, per the substitutions already recorded in
  `docs/imports/hip/dependencies.md`.
- **`uuid`** (used only for `Data.UUID`'s 128-bit `UUID` type, to marshal
  DuckDB's native `UUID` column type) — checked the actual `uuid.cabal`:
  it re-exports `Data.UUID` from the separate `uuid-types` package and adds
  generation modules (`Data.UUID.V1`/`V3`/`V4`/`V5`, MAC-address/random/
  hash-based) that `duckdb-simple` never imports (it only ever uses
  `qualified Data.UUID as UUID` for the plain type plus byte<->`UUID`
  conversions). Neither the Lean stdlib nor any existing `linen` port
  provides a 128-bit UUID type, so this is genuinely new — but, like
  `Only` was for `sqlite-simple`, it's small enough (one real type plus a
  handful of pure conversion functions, not the generation machinery) to
  fold directly into the port of `Database.DuckDB.Simple.Types`/
  `LogicalRep` below rather than opening a separate `docs/imports/uuid/`
  folder or `index.md` entry for a package whose generation half is unused
  here anyway.
- **`Only`** (re-exported by `Database.DuckDB.Simple.Types`, same tiny
  package as for `sqlite-simple`) — already covered; see
  `docs/imports/sqlite-simple/dependencies.md`'s precedence note. Not
  re-ported a second time — both packages' `Types` module reuse the same
  `Linen.Data.Tuple.Only`-style definition once it exists (ported the
  first time as part of whichever of the two, `sqlite-simple` or
  `duckdb-simple`, lands first per the requested import order —
  `sqlite-simple`).

## Module list (topological order)

1. `Database.DuckDB.Simple.Internal` →
   `Linen.Database.DuckDB.Simple.Internal` — **new port**, on
   `Linen.Database.DuckDB.FFI` (`OpenConnect`, `PreparedStatements`,
   `QueryExecution`, `ErrorData`, `Helpers`). `Connection`/`SQLError`, the
   `withDatabaseHandle`/`withConnectionHandle`/`withClientContext`
   bracket-style handle accessors, delete-callback/`StablePtr`
   registration helpers used by callback-registering modules (`Logging`,
   `Copy`, `Function`).
2. `Database.DuckDB.Simple.LogicalRep` →
   `Linen.Database.DuckDB.Simple.LogicalRep` — **new port**, on
   `Linen.Database.DuckDB.FFI.LogicalTypes`. Building/destroying
   `duckdb_logical_type` descriptors (struct/list/map/enum/decimal) used
   both when binding parameters and when decoding result columns. No
   dependency on any other `duckdb-simple` module.
3. `Database.DuckDB.Simple.Ok` → `Linen.Database.DuckDB.Simple.Ok` — **new
   port.** Identical shape to `sqlite-simple`'s `Ok` (see
   `docs/imports/sqlite-simple/dependencies.md` #6) — the error-
   accumulating `Ok a | Errors [SomeException]` applicative, ported the
   same way (`Except (Array String)`-style). No dependency on any other
   `duckdb-simple` module.
4. `Database.DuckDB.Simple.Types` → `Linen.Database.DuckDB.Simple.Types`
   — **new port**, on #1. `Query`, `Null`, `FormatError`, `(:.)`, plus the
   folded-in `Only` (reused from the `sqlite-simple` port, see precedence
   note) and the folded-in `UUID` type (see precedence note above) if not
   already introduced by #2/#5's needs.
5. `Database.DuckDB.Simple.FromField` →
   `Linen.Database.DuckDB.Simple.FromField` — **new port**, on #2, #3, #4.
   `Field`, `FieldValue` (DuckDB's tagged-union decoded value, covering
   every DuckDB logical type: ints of every width, `DecimalValue`,
   `BigNum`, `BitString`, `IntervalValue`, `TimeWithZone`, lists/structs/
   maps/enums, `UUID`, …), the `FromField` class, and `ResultError`.
6. `Database.DuckDB.Simple.Materialize` →
   `Linen.Database.DuckDB.Simple.Materialize` — **new port**, on #5, #2,
   plus `Linen.Database.DuckDB.FFI.{DataChunk,Vector,Validity,Helpers}`.
   Converts one DuckDB `duckdb_vector`/`data_chunk` column into a
   `FieldValue` per row — the single place that walks DuckDB's native
   columnar chunk representation. (Not itself exposed upstream — an
   internal `other-module` — but load-bearing for #14/#15/#17.)
7. `Database.DuckDB.Simple.ToField` →
   `Linen.Database.DuckDB.Simple.ToField` — **new port**, on #5, #1, #2,
   #4. `DuckDBColumnType`, `FieldBinding`, `NamedParam`, the `ToField`
   class and instances, binding a Lean value into a prepared-statement
   parameter slot via `Linen.Database.DuckDB.FFI.BindValues`.
8. `Database.DuckDB.Simple.FromRow` →
   `Linen.Database.DuckDB.Simple.FromRow` — **new port**, on #5, #1, #3,
   #4. Applicative row-consuming parser, same shape as
   `sqlite-simple`'s (see `docs/imports/sqlite-simple/dependencies.md`
   #12).
9. `Database.DuckDB.Simple.ToRow` → `Linen.Database.DuckDB.Simple.ToRow`
   — **new port**, on #7, #4. Dual of #8.
10. `Database.DuckDB.Simple.Catalog` →
    `Linen.Database.DuckDB.Simple.Catalog` — **new port**, on #1, plus
    `Linen.Database.DuckDB.FFI.Catalog`. Catalog/search-path queries.
11. `Database.DuckDB.Simple.Config` →
    `Linen.Database.DuckDB.Simple.Config` — **new port**, on #1, plus
    `Linen.Database.DuckDB.FFI.Configuration`. Connection-config
    key/value setting prior to `open`.
12. `Database.DuckDB.Simple.FileSystem` →
    `Linen.Database.DuckDB.Simple.FileSystem` — **new port**, on #1, plus
    `Linen.Database.DuckDB.FFI.FileSystem`. Registering a virtual
    filesystem callback set.
13. `Database.DuckDB.Simple.Logging` →
    `Linen.Database.DuckDB.Simple.Logging` — **new port**, on #1, plus
    `Linen.Database.DuckDB.FFI.Logging`. Log-callback registration.
14. `Database.DuckDB.Simple.Copy` → `Linen.Database.DuckDB.Simple.Copy` —
    **new port**, on #5, #1, #6, plus `Linen.Database.DuckDB.FFI.Appender`.
    Bulk row-append ("COPY"-style bulk load).
15. `Database.DuckDB.Simple.Function` →
    `Linen.Database.DuckDB.Simple.Function` — **new port**, on #5, #1, #6,
    #3, plus `Linen.Database.DuckDB.FFI.ScalarFunctions`. User-defined
    scalar SQL function registration (`createFunction`/
    `createFunctionWithState`/`deleteFunction`).
16. `Database.DuckDB.Simple.Generic` →
    `Linen.Database.DuckDB.Simple.Generic` — **new port**, on #5, #2, #3,
    #7. `Generic`-deriving support for `FromRow`/`ToRow` instances
    (structural/record marshalling). Lean has its own `deriving`/generic-
    structure story (or, failing a direct analogue, this reduces to
    hand-written per-field instances) — flagged for a closer look during
    the actual port, not a blocker for this plan.
17. `Database.DuckDB.Simple` → `Linen.Database.DuckDB.Simple` — **new
    port**, on #5, #8, #15, #1, #6, #3, #7, #9, #4. The public facade:
    `open`/`close`/`withConnection`, `query`/`query_`/`execute`/`execute_`,
    `fold`/`fold_`, transactions, error types.

## Native C library

No new native dependency at this layer — `duckdb-simple` itself is pure
Haskell, calling only into `duckdb-ffi`'s bindings (`Linen.Database.DuckDB.
FFI.*`). All `libduckdb` linking/discovery concerns are handled once, at
the `duckdb-ffi` layer — see `docs/imports/duckdb-ffi/dependencies.md`'s
"Native C library" section (no `pkg-config` file for DuckDB on either
macOS/Homebrew or Ubuntu's default apt repos — a real blocker to resolve
before porting, not just a linting concern).

## Termination notes

`FromRow`/`ToRow` (#8/#9) and `Generic` (#16) consume statically fixed-
arity tuple/record shapes, same as `sqlite-simple`'s equivalents — no
`partial def` or fuel parameter needed. `Materialize` (#6) walks a
`data_chunk`'s columns and a chunk's fixed row count (both are runtime
`Nat`s bounded by the chunk itself, iterated by simple structural
recursion on a decreasing counter) — a standard bounded-loop termination
argument, no self-referential typing concern like `hip`'s FFT or
`pdf-toolbox`'s object graph.

## Tally

- 17 modules total (16 exposed + 1 internal `other-module`,
  `Materialize`), all genuinely new work (`duckdb-simple` shares no code
  with `sqlite-simple` despite the similar module names/shapes).
