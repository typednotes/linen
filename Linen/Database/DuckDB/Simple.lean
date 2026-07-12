/-
  Linen.Database.DuckDB.Simple — the public `duckdb-simple` facade

  Module #17 (the last) of `docs/imports/duckdb-simple/dependencies.md`, on
  module #5 (`…FromField`), module #8 (`…FromRow`), module #15
  (`…Function`, not used directly here — see "Scope" below), module #1
  (`…Internal`, for `Connection`/`Query`/`SQLError`), module #6
  (`…Materialize`), module #3 (`…Ok`), module #7 (`…ToField`, for
  `FieldBinding`), module #9 (`…ToRow`), module #4 (`…Types`), plus
  `Linen.Database.DuckDB.FFI.{PreparedStatements,ExecutePrepared,
  QueryExecution,DataChunk}`.

  ## Scope

  Mirrors the surface `Linen.Database.SQLite.Simple`'s own module doc
  settled on for the sibling `sqlite-simple` port: `open`/`close`/
  `withConnection`, `query`/`query_`/`execute`/`execute_`, `fold`/`fold_`,
  and transactions — a deliberately smaller cut than upstream's full export
  list, for the same reasons. Kept **out of scope**, matching that
  precedent:

  - **Named-parameter variants** (`queryNamed`/`executeNamed`/`foldNamed`)
    and **explicit-`RowParser` variants** (`queryWith`/`foldStatementWith`/
    …): a caller needing an explicit `RowParser` can already reach
    `Database.DuckDB.Simple.runFromRow`/`RowParser.run` directly; named
    parameters would need `Database.DuckDB.FFI.PreparedStatements.
    parameterName`/a name→index resolution pass this facade does not add,
    matching `…SQLite.Simple`'s identical omission.
  - **`executeMany`**: a thin loop over `execute`, trivially written by a
    caller as `params.forM (execute conn templ ·)`.
  - **The standalone streaming-statement API** (`openStatement`/
    `resetStatementStream`/`nextRow`/`consumeStream`/…): upstream exposes
    these as a *second*, lower-level way to drive a prepared statement
    one row at a time without a callback; `fold`/`fold_` below already
    stream (never materializing the whole result, via repeated
    `Linen.Database.DuckDB.FFI.QueryExecution.fetchChunk` calls) and are
    this port's only, callback-based, streaming entry point — matching how
    `Linen.Database.SQLite.Simple` itself only exposes `fold`/`fold_`, not
    a separate cursor API, for the same underlying SQLite `step`-loop.
  - **`Database.DuckDB.Simple.Function`'s own re-exports**: that module
    (module #15) already stands on its own; this facade does not
    re-re-export its registration API, matching how `…SQLite.Simple`
    doesn't fold in `Linen.Database.SQLite.Bindings`'s own standalone
    surface either.
  - **`openWithConfig`/`withConnectionWithConfig`**: `Database.DuckDB.
    Simple.Internal.openConnection` (module #1) only takes a path, not a
    `duckdb_config`; wiring one through would mean this facade reaching
    past `Internal`'s own already-settled scope into
    `Linen.Database.DuckDB.FFI.OpenConnect`'s config-handle constructors
    directly, which module #1's own doc comment did not anticipate needing.
    A later batch can add `openWithConfig` alongside `Internal` itself if a
    consumer needs it.

  `SQLError` is not redefined here either: it is already
  `Database.DuckDB.Simple.SQLError` (module #1), used as-is.

  ## The missing `fetchChunk` FFI binding

  Upstream's `collectRows`/`streamNextRow` call `duckdb_fetch_chunk`
  directly on an ordinary (non-streaming) `duckdb_result`/prepared
  statement's result to walk it a batch at a time. No such binding existed
  in `Linen.Database.DuckDB.FFI` before this module: `docs/imports/
  duckdb-ffi/dependencies.md`'s original scope decision had filed
  `duckdb_fetch_chunk` under upstream Haskell's `StreamingResult` module and
  excluded it, on the mistaken assumption that `duckdb-simple` never calls
  it on a materialized result. That was corrected (see that dependency
  file's own 2026-07-12 note) by adding the single binding directly to the
  already-kept `Database.DuckDB.FFI.QueryExecution` module as `fetchChunk`
  — see that module's own doc-comment addendum for why `duckdb_fetch_chunk`
  (unlike its deprecated neighbours) is the correct, current way to walk a
  materialized result's rows. This facade is what actually needed it.

  ## Design

  - `withConnection` wraps `Internal.openConnection`/`closeConnection` in a
    `try … finally …` bracket, exactly as `…SQLite.Simple.withConnection`
    does (see that module's doc for why `open`/`close` themselves are not
    re-exported under those names — already settled by module #1, for the
    same "`open` is a keyword" reason).
  - A prepared statement's full lifecycle (`prepare` → bind each `ToRow`
    parameter via its rendered `FieldBinding.bind` → `execute` → walk the
    result → `PreparedStatements.destroy`) is threaded through a private
    `withPrepared` bracket for every one of `query`/`query_`/`execute`/
    `execute_`/`fold`/`fold_`, mirroring the equivalent `openStatement`/
    `closeStatement` bracket `…SQLite.Simple` already uses, adapted to
    DuckDB's prepare/execute split (`sqlite3_step` interleaves preparation
    and iteration; DuckDB's C API separates "execute, producing a
    materialized `Result`" from "walk the `Result`'s chunks" into two
    stages, `ExecutePrepared.execute` then repeated `QueryExecution.
    fetchChunk`).
  - `collectRows`/`streamResult` decode each `DataChunk` row into an `Array
    Field` by calling `Materialize.materializeValue` once per column
    (fetching that column's `Vector` via `DataChunk.getVector`) and
    `QueryExecution.columnName` for the column's label, then decode the row
    via `FromRow.runFromRow`, throwing `IO.userError` on a failed
    conversion — the same `Ok`-to-`IO`-exception substitution
    `…SQLite.Simple`'s `decodeRow` already documents. `fold`/`fold_` reuse
    `streamResult` with an `action`-driven accumulator instead of a plain
    `Array.push`, so a caller can process arbitrarily many rows without
    materializing them all — DuckDB's own chunk-at-a-time `fetchChunk`
    still batches internally (one `DataChunk`, typically up to 2048 rows,
    per call), but no more than one chunk is ever held at once.
  - `execute`/`execute_` run the statement to completion (draining every
    chunk with `fetchChunk`, even though an `INSERT`/`UPDATE`/`DDL`
    statement's `Result` normally has none) and report failure via
    `QueryExecution.resultError`/`resultErrorType` wrapped in `SQLError`,
    matching upstream's own `mkExecuteError`.
  - `withTransaction` mirrors `…SQLite.Simple.withTransaction`'s
    `BEGIN`/`COMMIT`/`ROLLBACK`-via-`execute_` structure exactly, using
    DuckDB's own transaction statements (`BEGIN TRANSACTION`/`COMMIT`/
    `ROLLBACK`) verbatim in place of SQLite's `BEGIN TRANSACTION`/`COMMIT
    TRANSACTION`/`ROLLBACK TRANSACTION` spelling.

  ## Haskell source
  - `Database.DuckDB.Simple` (`duckdb-simple` package, version 0.1.5.1)
-/

import Linen.Database.DuckDB.Simple.Internal
import Linen.Database.DuckDB.Simple.ToRow
import Linen.Database.DuckDB.Simple.FromRow
import Linen.Database.DuckDB.Simple.Materialize
import Linen.Database.DuckDB.FFI.PreparedStatements
import Linen.Database.DuckDB.FFI.ExecutePrepared

namespace Database.DuckDB.Simple

open Database.DuckDB.FFI.Types (PreparedStatement DataChunk Idx State)

-- ────────────────────────────────────────────────────────────────────
-- Connections
-- ────────────────────────────────────────────────────────────────────

/-- Run `action` with a freshly-opened connection to the database at
    `path` (`none`, or `":memory:"`, for an in-memory database), closing it
    afterwards even if `action` throws. -/
def withConnection (path : Option String) (action : Connection → IO α) : IO α := do
  let conn ← openConnection path
  try
    action conn
  finally
    closeConnection conn

-- ────────────────────────────────────────────────────────────────────
-- Prepared-statement lifecycle
-- ────────────────────────────────────────────────────────────────────

/-- Bind every element of `params` (rendered via `ToRow`) to `stmt`, in
    order, 1-indexed as DuckDB's `duckdb_bind_*` calls expect. -/
private def bindAll (stmt : PreparedStatement) (params : Array FieldBinding) : IO Unit := do
  let mut idx : UInt64 := 1
  for b in params do
    b.bind stmt idx
    idx := idx + 1

/-- Prepare `templ` against `conn`, run `action` against the resulting
    statement, and destroy it afterwards regardless of whether `action`
    threw. Throws `SQLError` if preparation itself failed. -/
private def withPrepared (conn : Connection) (templ : Query) (action : PreparedStatement → IO α) :
    IO α :=
  withConnectionHandle conn fun connHandle => do
    let (state, stmt) ← Database.DuckDB.FFI.PreparedStatements.prepare connHandle templ.fromQuery
    try
      unless state.isSuccess do
        let msg ← Database.DuckDB.FFI.PreparedStatements.error stmt
        throwSQLError { message := msg.getD "prepare failed", query := some templ }
      action stmt
    finally
      Database.DuckDB.FFI.PreparedStatements.destroy stmt

/-- Execute `stmt` (whatever parameters were last bound to it), returning
    its materialized `Result`. Throws `SQLError` if execution itself
    failed to even produce a result object worth walking. -/
private def executeStatement (templ : Query) (stmt : PreparedStatement) :
    IO Database.DuckDB.FFI.Types.Result := do
  let (state, result) ← Database.DuckDB.FFI.ExecutePrepared.execute stmt
  unless state.isSuccess do
    let msg ← Database.DuckDB.FFI.QueryExecution.resultError result
    let errType ← Database.DuckDB.FFI.QueryExecution.resultErrorType result
    Database.DuckDB.FFI.QueryExecution.destroy result
    throwSQLError { message := msg.getD "execute failed", errorType := some errType, query := some templ }
  pure result

-- ────────────────────────────────────────────────────────────────────
-- Row decoding
-- ────────────────────────────────────────────────────────────────────

/-- Decode one row's `Field`s via `FromRow`, throwing `IO.userError` on a
    failed conversion (the same `Ok`-to-`IO`-exception substitution
    `Linen.Database.SQLite.Simple`'s `decodeRow` already documents). -/
private def decodeRow [FromRow r] (fields : Array Field) : IO r :=
  match runFromRow (α := r) fields with
  | .ok a => pure a
  | .errors es => throw (IO.userError s!"row conversion failed: {es}")

/-- Materialize every column of row `rowIdx` within `chunk` into a `Field`,
    given `result` (for column-name lookups) and the chunk's column
    count. -/
private def chunkRowFields (result : Database.DuckDB.FFI.Types.Result) (chunk : DataChunk)
    (numCols : Idx) (rowIdx : Idx) : IO (Array Field) := do
  let mut fields : Array Field := #[]
  for c in [0:numCols.toNat] do
    let colIdx := UInt64.ofNat c
    let vector ← Database.DuckDB.FFI.DataChunk.getVector chunk colIdx
    let value ← Materialize.materializeValue vector rowIdx
    let label ← Database.DuckDB.FFI.QueryExecution.columnName result colIdx
    fields := fields.push { result := value, column := c, columnLabel := label }
  pure fields

/-- Walk every `DataChunk` of `result` via repeated `QueryExecution.
    fetchChunk` calls, folding `action` over each decoded row without ever
    holding more than one chunk's worth of rows at a time. Always destroys
    `result` (and every chunk fetched from it) before returning, even if
    `action` throws. -/
private def streamResult [FromRow row] (result : Database.DuckDB.FFI.Types.Result) (init : α)
    (action : α → row → IO α) : IO α := do
  try
    let numCols ← Database.DuckDB.FFI.QueryExecution.columnCount result
    let mut acc := init
    let mut more := true
    while more do
      match ← Database.DuckDB.FFI.QueryExecution.fetchChunk result with
      | none => more := false
      | some chunk =>
        try
          let size ← Database.DuckDB.FFI.DataChunk.getSize chunk
          for r in [0:size.toNat] do
            let fields ← chunkRowFields result chunk numCols (UInt64.ofNat r)
            acc ← action acc (← decodeRow fields)
        finally
          Database.DuckDB.FFI.DataChunk.destroy chunk
    pure acc
  finally
    Database.DuckDB.FFI.QueryExecution.destroy result

/-- Drain every chunk of `result` without decoding any rows — used by
    `execute`/`execute_`, whose statements are not expected to return any,
    but whose `Result` must still be walked to completion and destroyed. -/
private def drainResult (result : Database.DuckDB.FFI.Types.Result) : IO Unit := do
  try
    let mut more := true
    while more do
      match ← Database.DuckDB.FFI.QueryExecution.fetchChunk result with
      | none => more := false
      | some chunk => Database.DuckDB.FFI.DataChunk.destroy chunk
  finally
    Database.DuckDB.FFI.QueryExecution.destroy result

-- ────────────────────────────────────────────────────────────────────
-- Queries that return results
-- ────────────────────────────────────────────────────────────────────

/-- Run a parameterized query that returns rows, decoding each one via
    `FromRow`. -/
def query [ToRow q] [FromRow r] (conn : Connection) (templ : Query) (params : q) :
    IO (Array r) :=
  withPrepared conn templ fun stmt => do
    bindAll stmt (toRow params)
    let result ← executeStatement templ stmt
    streamResult result #[] (fun acc row => pure (acc.push row))

/-- A version of `query` that performs no parameter substitution. -/
def query_ [FromRow r] (conn : Connection) (templ : Query) : IO (Array r) :=
  withPrepared conn templ fun stmt => do
    let result ← executeStatement templ stmt
    streamResult result #[] (fun acc row => pure (acc.push row))

-- ────────────────────────────────────────────────────────────────────
-- Queries that stream results
-- ────────────────────────────────────────────────────────────────────

/-- Fold over a parameterized query's result rows one chunk at a time,
    without ever materializing the whole result set (unlike `query`). -/
def fold [ToRow params] [FromRow row] (conn : Connection) (templ : Query) (params : params)
    (init : α) (action : α → row → IO α) : IO α :=
  withPrepared conn templ fun stmt => do
    bindAll stmt (toRow params)
    let result ← executeStatement templ stmt
    streamResult result init action

/-- A version of `fold` that performs no parameter substitution. -/
def fold_ [FromRow row] (conn : Connection) (templ : Query) (init : α) (action : α → row → IO α) :
    IO α :=
  withPrepared conn templ fun stmt => do
    let result ← executeStatement templ stmt
    streamResult result init action

-- ────────────────────────────────────────────────────────────────────
-- Statements that do not return results
-- ────────────────────────────────────────────────────────────────────

/-- Run an `INSERT`/`UPDATE`/other statement not expected to return rows. -/
def execute [ToRow q] (conn : Connection) (templ : Query) (params : q) : IO Unit :=
  withPrepared conn templ fun stmt => do
    bindAll stmt (toRow params)
    let result ← executeStatement templ stmt
    drainResult result

/-- A version of `execute` that performs no parameter substitution. -/
def execute_ (conn : Connection) (templ : Query) : IO Unit :=
  withPrepared conn templ fun stmt => do
    let result ← executeStatement templ stmt
    drainResult result

-- ────────────────────────────────────────────────────────────────────
-- Transactions
-- ────────────────────────────────────────────────────────────────────

private def withTransactionKind (conn : Connection) (beginSql commitSql rollbackSql : String)
    (action : IO α) : IO α := do
  execute_ conn beginSql
  try
    let r ← action
    execute_ conn commitSql
    return r
  catch e =>
    execute_ conn rollbackSql
    throw e

/-- Run `action` inside a `BEGIN TRANSACTION`, committing on success and
    rolling back (then re-raising) on any thrown exception. -/
def withTransaction (conn : Connection) (action : IO α) : IO α :=
  withTransactionKind conn "BEGIN TRANSACTION" "COMMIT" "ROLLBACK" action

end Database.DuckDB.Simple
