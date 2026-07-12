# `duckdb-ffi` — dependency plan

Upstream: https://hackage.haskell.org/package/duckdb-ffi (version 1.5.0.0,
the latest published), source at https://github.com/Tritlo/duckdb-haskell
(`subdir: duckdb-ffi`). Low-level FFI bindings exposing "the full DuckDB C
API" — the role `direct-sqlite` plays for `sqlite-simple`, but **not**
folded into `duckdb-simple`'s own dependency doc (see the size discussion
below).

An edge **A → B** means *module A imports module B*, so **B must be built
before A**. Derived from each module's own `import Database.DuckDB.FFI...`
lines, checked directly against the `duckdb-ffi-1.5.0.0` tarball source (not
guessed from Haddock). The internal import graph is unusually flat: every
non-facade module imports only `Database.DuckDB.FFI.Types` (verified by
grepping every module's `import Database.DuckDB.FFI` lines — none import
each other).

Namespace note: ported as `Linen.Database.DuckDB.FFI.*`, mirroring
`Linen.Database.SQLite.Bindings`/`Linen.Database.PostgreSQL.LibPQ` as the
raw-C-binding layer beneath a "simple" wrapper.

## Precedence check / size decision (why this is *not* folded into `duckdb-simple`)

`direct-sqlite` (4 modules) was small enough to fold directly into
`sqlite-simple`'s own `dependencies.md`, following the `Hasql`/`LibPQ`
precedent. `duckdb-ffi` cannot be treated the same way: it has **44
modules** (2 exposed + 42 internal), each corresponding to one section of
DuckDB's C API (aggregate/scalar/table function registration, the Arrow
interop layer, streaming results, the append API, a whole parallel
`Deprecated.*` sub-tree of legacy pre-1.0 API shims, …) — comparable in
scale to this repo's largest existing single-package ports (`WaiExtra`, 37
modules; `Warp`, 18 modules), not to a small helper like `direct-sqlite`.
Per `AGENTS.md`'s "decide based on real module count/complexity" guidance,
this warrants its own `docs/imports/duckdb-ffi/` folder and its own
`index.md` entry, listed immediately before `duckdb-simple`.

**Scope decision (decided with the user 2026-07-11):** `duckdb-ffi` is
**scoped down to exactly the modules `duckdb-simple` needs**, the same
treatment `docs/imports/hip/dependencies.md` gives
`Graphics.Image.IO.Histogram` — a whole subsystem dropped by explicit
decision, recorded in `docs/imports/index.md`. Checked against
`duckdb-simple`'s own source: every `duckdb-simple` module imports an
individual `Database.DuckDB.FFI.*` submodule directly (never the
top-level `Database.DuckDB.FFI` facade unqualified), and only a modest
subset of the full C API is ever called — open/connect, prepare/bind/
execute, result-chunk/vector materialization, appender for `Copy`, a
handful of config/catalog/filesystem/logging calls, and function-
registration for user-defined scalar functions. This port therefore
includes only the **18 load-bearing modules** listed below (`Types` plus
17 modules that are direct dependencies of `duckdb-simple`); the other
**26 modules** of the full 44-module upstream surface — the Arrow export/
import interface, aggregate- and table-function registration, the entire
`Deprecated.*` legacy shim tree, streaming (as opposed to the default
materialized) results, and the top-level `Database.DuckDB.FFI`/
`Database.DuckDB.FFI.Deprecated` facades themselves — are **excluded**;
see the "Excluded" section below for the full list and rationale.

## External dependencies

- `base`, `bytestring`, `containers`, `text`, `time`, `transformers` —
  already ported (`Base`, `ByteString`, `Containers`, `Text`, `Time`) or
  Lean's own `transformers`-equivalent, per the same substitutions recorded
  in `docs/imports/hip/dependencies.md`.
- `exceptions` — covered by Lean's native `Except`/`IO`, same as for
  `sqlite-simple` (see `docs/imports/sqlite-simple/dependencies.md`).
- `mtl` — already ported (`Mtl`).

## Module list (topological order)

Foundational (no dependency on any other `duckdb-ffi` module):

1. `Database.DuckDB.FFI.Types` → `Linen.Database.DuckDB.FFI.Types` — **new
   port.** Every opaque handle type (`duckdb_database`, `duckdb_connection`,
   `duckdb_result`, `duckdb_prepared_statement`, `duckdb_data_chunk`,
   `duckdb_vector`, `duckdb_logical_type`, `duckdb_value`, …), the
   `duckdb_type`/`duckdb_state`/`duckdb_error_type` enums, and shared
   `Storable` marshalling helpers. *Load-bearing for `duckdb-simple`.*

The following 17 modules each depend on **only** `Types` (#1) — listed
alphabetically since upstream imposes no further order among them. Every
one is load-bearing for `duckdb-simple` (this is the full in-scope set per
the scope decision above; the 26 excluded modules are listed separately
below):

2. `Database.DuckDB.FFI.Appender` → `...FFI.Appender` — **new port.**
   Bulk-append API. *Load-bearing (`Database.DuckDB.Simple.Copy`).*
3. `Database.DuckDB.FFI.BindValues` → `...FFI.BindValues` — **new port.**
   Prepared-statement parameter binding. *Load-bearing.*
4. `Database.DuckDB.FFI.Catalog` → `...FFI.Catalog` — **new port.**
   Catalog search-path queries. *Load-bearing
   (`Database.DuckDB.Simple.Catalog`).*
5. `Database.DuckDB.FFI.Configuration` → `...FFI.Configuration` — **new
   port.** `duckdb_config` creation/option-setting. *Load-bearing
   (`Database.DuckDB.Simple.Config`).*
6. `Database.DuckDB.FFI.DataChunk` → `...FFI.DataChunk` — **new port.**
   Result data-chunk/column access. *Load-bearing
   (`Database.DuckDB.Simple.Materialize`).*
7. `Database.DuckDB.FFI.ErrorData` → `...FFI.ErrorData` — **new port.**
   Structured error object accessors. *Load-bearing.*
8. `Database.DuckDB.FFI.ExecutePrepared` → `...FFI.ExecutePrepared` —
   **new port.** Executing a bound prepared statement. *Load-bearing.*
9. `Database.DuckDB.FFI.FileSystem` → `...FFI.FileSystem` — **new port.**
   Virtual/attached filesystem registration. *Load-bearing
   (`Database.DuckDB.Simple.FileSystem`).*
10. `Database.DuckDB.FFI.Helpers` → `...FFI.Helpers` — **new port.** Misc.
    small helpers (e.g. `duckdb_free`, vector/validity bit helpers).
    *Load-bearing.*
11. `Database.DuckDB.FFI.Logging` → `...FFI.Logging` — **new port.**
    Log-callback registration. *Load-bearing
    (`Database.DuckDB.Simple.Logging`).*
12. `Database.DuckDB.FFI.LogicalTypes` → `...FFI.LogicalTypes` — **new
    port.** Building/inspecting `duckdb_logical_type` (struct/list/map/enum/
    decimal type descriptors). *Load-bearing
    (`Database.DuckDB.Simple.LogicalRep`).*
13. `Database.DuckDB.FFI.OpenConnect` → `...FFI.OpenConnect` — **new
    port.** `duckdb_open`/`duckdb_connect`/`duckdb_close`/
    `duckdb_disconnect`. *Load-bearing.*
14. `Database.DuckDB.FFI.PreparedStatements` → `...FFI.PreparedStatements`
    — **new port.** `duckdb_prepare`/parameter-count/-type introspection.
    *Load-bearing.*
15. `Database.DuckDB.FFI.QueryExecution` → `...FFI.QueryExecution` — **new
    port.** `duckdb_query`, `duckdb_result` row/column-count accessors.
    *Load-bearing.*
16. `Database.DuckDB.FFI.ScalarFunctions` → `...FFI.ScalarFunctions` —
    **new port.** User-defined scalar function registration.
    *Load-bearing (`Database.DuckDB.Simple.Function`).*
17. `Database.DuckDB.FFI.Validity` → `...FFI.Validity` — **new port.**
    NULL-validity bitmask accessors. *Load-bearing.*
18. `Database.DuckDB.FFI.Vector` → `...FFI.Vector` — **new port.** Column
    vector data/validity-pointer accessors within a `DataChunk`.
    *Load-bearing.*

No facade module is ported: `duckdb-simple` imports each
`Database.DuckDB.FFI.*` submodule directly (confirmed against its source
— never the unqualified top-level `Database.DuckDB.FFI` facade), so
neither `Database.DuckDB.FFI` nor `Database.DuckDB.FFI.Deprecated` is
needed here.

## Excluded

Per the scope decision above, the following **26 modules** of the full
44-module upstream surface are excluded — deprecated or unused C-API
surface with no consumer in `duckdb-simple` (checked directly against
`duckdb-simple`'s import list; none of these are imported, transitively or
otherwise):

- **15 unused domain modules** (each would otherwise have depended on only
  `Types`, same as the 17 kept above): `AggregateFunctions` (user-defined
  aggregate function registration), `Arrow` (Apache Arrow C-data-interface
  export/import), `CastFunctions` (user-defined type-cast registration),
  `CopyFunctions` (user-defined `COPY` format registration), `Expression`
  (scalar-function expression-tree introspection), `ExtractStatements`
  (splitting a multi-statement SQL string), `PendingResult` (async/pending
  query execution and interrupt support), `ProfilingInfo` (query-plan
  profiling metadata access), `ReplacementScans` (custom table-name-
  resolution registration), `ResultFunctions` (per-cell accessors on the
  legacy non-chunked result object), `SelectionVector` (selection-vector
  construction), `TableDescription` (table column/constraint introspection),
  `TableFunctions` (user-defined table function registration), `Threading`
  (task-scheduler/thread-pool hooks), `ValueInterface` (boxed
  `duckdb_value` construction/decoding).
- **Correction (2026-07-12):** `StreamingResult` was originally listed here
  too, on the assumption that `duckdb-simple` never consumes it. That was
  wrong: `duckdb-simple`'s facade (`collectRows`/`streamNextRow`) calls
  `duckdb_fetch_chunk` directly on ordinary, materialized query/prepared-
  statement results — not only on a `duckdb_pending_prepared_streaming`
  result — and `duckdb_fetch_chunk` carries no deprecation notice (unlike
  its neighbour `duckdb_stream_fetch_chunk`), so it is in fact load-bearing.
  Rather than reopening this file's 17/26 split, the single binding was
  added directly to the already-kept `Database.DuckDB.FFI.QueryExecution`
  module (as `fetchChunk`) when module #17 of `docs/imports/duckdb-simple/
  dependencies.md` needed it — see that module's own doc-comment addendum
  for the full justification. No other part of `StreamingResult` (pending-
  result polling, stream lifecycle management, etc.) is ported; only this
  one already-non-deprecated fetch primitive was.
- **The entire 8-module `Deprecated.*` sub-tree** — `Deprecated.Appender`,
  `Deprecated.Arrow`, `Deprecated.ExecutePrepared`,
  `Deprecated.PendingResult`, `Deprecated.QueryExecution`,
  `Deprecated.ResultFunctions`, `Deprecated.SafeFetch`,
  `Deprecated.StreamingResult` — legacy pre-1.0 API shims, by definition
  superseded and unused by any current consumer, plus its own
  `Deprecated` re-export facade.
- **The top-level `Database.DuckDB.FFI` re-export facade** — not needed
  since `duckdb-simple` imports each load-bearing submodule directly (see
  the module-list note above).

This is the same treatment `docs/imports/hip/dependencies.md` gives
`Graphics.Image.IO.Histogram`: a real, cleanly-severable subsystem dropped
by explicit decision with the user rather than an improvised cut — no
kept module imports anything in this excluded set. Decided with the user
2026-07-11.

## Native C library

`duckdb-ffi` binds against `libduckdb`, DuckDB's single-file C API shared
library (`extra-libraries: duckdb`, `install-includes: duckdb.h`, a small
`cbits/duckdb_stub.c`). Unlike `libpq`/`openssl`/`libsecret-1`/`sqlite3`,
**DuckDB does not ship a `.pc` pkg-config file**, and there is no
single-file amalgamation to vendor the way `sqlite-simple` vendors
`sqlite3.c` (`libduckdb` is a large prebuilt shared library, not a small
`.c`/`.h` pair) — so per `AGENTS.md`'s FFI precedence note, this port uses
the **"download a pinned prebuilt release archive"** path (decided with the
user 2026-07-12), not vendoring:

- A pinned DuckDB release version's platform archive
  (`libduckdb-osx-universal.zip` for macOS,
  `libduckdb-linux-<arch>.zip` for Linux, from
  `https://github.com/duckdb/duckdb/releases`) is downloaded and unpacked
  into a build-local directory (e.g. `.lake/duckdb/`) by a new step — a
  `lake`-invoked script or a `pre_build` lakefile hook, not `pkg-config`.
  The archive itself is **not** checked into git (`AGENTS.md`: "do not check
  prebuilt per-platform binaries into git").
- `lakefile.lean` gets a new `duckdbIncludeArgs`/`duckdbLinkArgs` pair
  (parallel to `pkgLinkFlags`, but pointing `-I`/`-L` at the unpacked
  archive directory directly rather than shelling out to `pkg-config`) and
  a `DUCKDB_PREFIX` env var override for local dev machines that already
  have `libduckdb` installed some other way (e.g. via Homebrew).
- **`.github/workflows/lean_action_ci.yml`** needs a new step, on *both* the
  `ubuntu-latest` and `macos-latest` matrix legs (per `AGENTS.md`'s
  macOS+Linux FFI-testing requirement), that downloads and unpacks the
  matching pinned archive before the `lean-action` build step runs.
- This keeps the two native-dependency strategies in this import
  consistent with the precedence note: SQLite (small amalgamation) is
  vendored; DuckDB (large prebuilt shared library, no amalgamation) is
  downloaded pinned, in both CI and — via the same script — local dev.

## Termination notes

Every module here is a direct 1:1 C-function-pointer binding (`@[extern]`-
style declarations plus `Storable`/marshalling glue) with no recursion of
its own — no termination concerns anywhere in this port.

## Tally

- **18 modules ported** — `Types` plus 17 independent domain modules, all
  marked *load-bearing* above (directly needed for `duckdb-simple`'s own
  module list in `docs/imports/duckdb-simple/dependencies.md`).
- **26 modules excluded** (of the full 44-module upstream surface) — see
  the "Excluded" section above for the full list and rationale.
