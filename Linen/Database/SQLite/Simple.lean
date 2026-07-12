/-
  Linen.Database.SQLite.Simple — the public `sqlite-simple` facade

  Module #15 of `docs/imports/sqlite-simple/dependencies.md`, on module #3
  (`Linen.Database.SQLite.Direct`), module #4 (`Linen.Database.SQLite`,
  for `exec`/`step`/`bind`/`SQLData`/`SQLError`), module #5 (`…Types`, for
  `Query`), module #6 (`…Ok`), module #9 (`…Internal`, for `Connection`/
  `Statement`/`currentRowFields`/`openConnection`/`openStatement`), module
  #10 (`…ToField`), module #11 (`…FromField`), module #12 (`…FromRow`), and
  module #13 (`…ToRow`).

  ## Scope

  The dependency plan describes this module's surface as: `open`/`close`/
  `withConnection`, `query`/`query_`/`execute`/`execute_`, `fold`/`fold_`,
  transactions (`withTransaction`, `withSavepoint`), `lastInsertRowId`,
  `changes`, and a pretty-printed `SQLError` — a deliberately smaller cut
  than upstream's full export list. Kept **out of scope**, matching that
  description exactly (not silently dropped):

  - **Named-parameter variants** (`queryNamed`/`executeNamed`/`foldNamed`)
    and **explicit-`RowParser` variants** (`queryWith`/`foldWith`/…): the
    dependency plan's own description of this module never mentions them;
    a caller needing an explicit `RowParser` can already reach
    `Database.SQLite.Simple.runFromRow`/`RowParser.run` directly.
  - **`executeMany`** (bind-and-step a query once per row of a list of
    parameter sets): a thin loop over `execute`, easily written by a caller
    as `params.forM (execute conn templ ·)`, and not in the plan's list.
  - **`setTrace`**: `Linen.Database.SQLite.Bindings`'s own module doc
    already scopes `sqlite3_trace` out of this port (no Lean-closure-from-C
    callback machinery existed when that layer was ported); nothing to
    build on here.
  - **`withImmediateTransaction`/`withExclusiveTransaction`** are *not*
    out of scope — they cost nothing beyond `withTransaction`'s own
    machinery (just a different `BEGIN` variant) and are included below.

  `SQLError` is not redefined here: it (with its `ToString` instance) is
  already `Database.SQLite3.SQLError`, from module #4 — this facade simply
  uses it as-is, satisfying the dependency plan's "pretty-printed `SQLError`"
  without duplication.

  ## Naming deviation

  Upstream's `open :: String -> IO Connection` cannot be ported under that
  name: `open` is a Lean keyword (the `open Namespace` command). Module #9
  (`Internal`) already ported this function as `openConnection`/
  `closeConnection` (chosen for exactly this reason — see that module's own
  doc); this facade re-uses those names as-is rather than introducing a
  second alias, and adds `withConnection` as the bracket-style wrapper
  upstream itself provides.

  ## Design

  - `query`/`query_`/`fold`/`fold_`/`execute`/`execute_` are built directly
    on `openStatement`/`Database.SQLite3.bind`/`Database.SQLite3.step`/
    `currentRowFields`/`closeStatement` (all already ported), wrapped in a
    `try … finally …` so the prepared statement is always finalized even if
    a row's conversion throws — the substitute for upstream's own
    `withStatement`/`withStatementParams` bracketing helpers (themselves not
    separately exported, so not ported as named functions here either).
  - `withTransaction`/`withImmediateTransaction`/`withExclusiveTransaction`
    share one helper (`withTransactionKind`) that issues the right `BEGIN`
    variant, runs the action, and either commits or (on any thrown
    exception) rolls back and re-throws — matching upstream's own
    `mask`/`onException` structure, ported using Lean's `try … catch e =>
    …; throw e` (there is no separate `mask` to port: Lean's `IO` has no
    asynchronous-exception delivery to mask against in the first place).
  - `withSavepoint` generates a fresh name from `Connection`'s
    `connectionTempNameCounter` (already ported in module #9, for exactly
    this purpose) and delegates to the same `withTransactionKind` helper
    with `SAVEPOINT`/`RELEASE`/`ROLLBACK TO` in place of
    `BEGIN`/`COMMIT`/`ROLLBACK`. Upstream increments its counter atomically
    (`atomicModifyIORef'`); this port uses a plain `get`/`set` pair instead
    — a deliberate simplification, since Lean's single-threaded `IO`
    (no implicit concurrency the way GHC's runtime has) never actually
    races two `withSavepoint` calls against each other on the same
    `Connection` the way concurrent Haskell code could.

  ## Haskell source
  - `Database.SQLite.Simple` (`sqlite-simple` package)
-/

import Linen.Database.SQLite.Simple.Internal
import Linen.Database.SQLite.Simple.ToRow
import Linen.Database.SQLite.Simple.FromRow

namespace Database.SQLite.Simple

open Database.SQLite3 (SQLData SQLError)
open Database.SQLite.Simple.Types (Query)

-- ────────────────────────────────────────────────────────────────────
-- Connections
-- ────────────────────────────────────────────────────────────────────

/-- Run `action` with a freshly-opened connection, closing it afterwards
    even if `action` throws (see the module doc for why this is
    `withConnection` rather than upstream's `open`/`close` pair used
    directly — those already exist as `openConnection`/`closeConnection`,
    from module #9). -/
def withConnection (path : String) (action : Connection → IO α) : IO α := do
  let conn ← openConnection path
  try
    action conn
  finally
    closeConnection conn

-- ────────────────────────────────────────────────────────────────────
-- Result statistics
-- ────────────────────────────────────────────────────────────────────

/-- The `rowid` of the most recent successful `INSERT` on this connection. -/
def lastInsertRowId (conn : Connection) : IO Int64 :=
  Database.SQLite3.lastInsertRowId conn.connectionHandle

/-- Rows changed, inserted, or deleted by the most recent statement. -/
def changes (conn : Connection) : IO Int64 :=
  Database.SQLite3.changes conn.connectionHandle

/-- Total rows changed, inserted, or deleted since this connection was
    opened. -/
def totalChanges (conn : Connection) : IO Int64 :=
  Database.SQLite3.totalChanges conn.connectionHandle

-- ────────────────────────────────────────────────────────────────────
-- Queries that return results
-- ────────────────────────────────────────────────────────────────────

/-- Decode one row's `Field`s via `FromRow`, throwing `IO.userError` on a
    failed conversion (the substitute for upstream's `ConversionFailed`/
    `ManyErrors` exceptions — see `Linen.Database.SQLite.Simple.Ok`'s module
    doc for why this port has no open exception hierarchy to throw those
    into). -/
private def decodeRow [FromRow r] (fields : Array Field) : IO r :=
  match runFromRow (α := r) fields with
  | .ok a => pure a
  | .errors es => throw (IO.userError s!"row conversion failed: {es}")

/-- Run `stmt` to completion, decoding every result row via `FromRow`. -/
private def collectRows [FromRow r] (stmt : Statement) : IO (Array r) := do
  let mut out := #[]
  let mut more := true
  while more do
    match ← Database.SQLite3.step stmt.statementHandle with
    | .row =>
      let fields ← currentRowFields stmt
      out := out.push (← decodeRow fields)
    | .done => more := false
  return out

/-- Run a parameterized query that returns rows, decoding each one via
    `FromRow`. -/
def query [ToRow q] [FromRow r] (conn : Connection) (templ : Query) (params : q) :
    IO (Array r) := do
  let stmt ← openStatement conn templ.fromQuery
  try
    Database.SQLite3.bind stmt.statementHandle (toRow params)
    collectRows stmt
  finally
    closeStatement stmt

/-- A version of `query` that performs no parameter substitution. -/
def query_ [FromRow r] (conn : Connection) (templ : Query) : IO (Array r) := do
  let stmt ← openStatement conn templ.fromQuery
  try
    collectRows stmt
  finally
    closeStatement stmt

-- ────────────────────────────────────────────────────────────────────
-- Queries that stream results
-- ────────────────────────────────────────────────────────────────────

/-- Fold over a parameterized query's result rows one at a time, without
    ever materializing the whole result set (unlike `query`). -/
def fold [ToRow params] [FromRow row] (conn : Connection) (templ : Query) (params : params)
    (init : α) (action : α → row → IO α) : IO α := do
  let stmt ← openStatement conn templ.fromQuery
  try
    Database.SQLite3.bind stmt.statementHandle (toRow params)
    let mut acc := init
    let mut more := true
    while more do
      match ← Database.SQLite3.step stmt.statementHandle with
      | .row =>
        let fields ← currentRowFields stmt
        acc ← action acc (← decodeRow fields)
      | .done => more := false
    return acc
  finally
    closeStatement stmt

/-- A version of `fold` that performs no parameter substitution. -/
def fold_ [FromRow row] (conn : Connection) (templ : Query)
    (init : α) (action : α → row → IO α) : IO α := do
  let stmt ← openStatement conn templ.fromQuery
  try
    let mut acc := init
    let mut more := true
    while more do
      match ← Database.SQLite3.step stmt.statementHandle with
      | .row =>
        let fields ← currentRowFields stmt
        acc ← action acc (← decodeRow fields)
      | .done => more := false
    return acc
  finally
    closeStatement stmt

-- ────────────────────────────────────────────────────────────────────
-- Statements that do not return results
-- ────────────────────────────────────────────────────────────────────

/-- Run an `INSERT`/`UPDATE`/other statement not expected to return rows. -/
def execute [ToRow q] (conn : Connection) (templ : Query) (params : q) : IO Unit := do
  let stmt ← openStatement conn templ.fromQuery
  try
    Database.SQLite3.bind stmt.statementHandle (toRow params)
    discard (Database.SQLite3.step stmt.statementHandle)
  finally
    closeStatement stmt

/-- A version of `execute` that performs no parameter substitution. -/
def execute_ (conn : Connection) (templ : Query) : IO Unit := do
  let stmt ← openStatement conn templ.fromQuery
  try
    discard (Database.SQLite3.step stmt.statementHandle)
  finally
    closeStatement stmt

-- ────────────────────────────────────────────────────────────────────
-- Transactions
-- ────────────────────────────────────────────────────────────────────

private def withTransactionKind (conn : Connection) (beginSql commitSql rollbackSql : String)
    (action : IO α) : IO α := do
  Database.SQLite3.exec conn.connectionHandle beginSql
  try
    let r ← action
    Database.SQLite3.exec conn.connectionHandle commitSql
    return r
  catch e =>
    Database.SQLite3.exec conn.connectionHandle rollbackSql
    throw e

/-- Run `action` inside a deferred `BEGIN TRANSACTION`, committing on
    success and rolling back (then re-raising) on any thrown exception. -/
def withTransaction (conn : Connection) (action : IO α) : IO α :=
  withTransactionKind conn "BEGIN TRANSACTION" "COMMIT TRANSACTION" "ROLLBACK TRANSACTION" action

/-- Like `withTransaction`, but with `BEGIN IMMEDIATE TRANSACTION`, which
    acquires the write lock immediately rather than lazily. -/
def withImmediateTransaction (conn : Connection) (action : IO α) : IO α :=
  withTransactionKind conn "BEGIN IMMEDIATE TRANSACTION" "COMMIT TRANSACTION"
    "ROLLBACK TRANSACTION" action

/-- Like `withTransaction`, but with `BEGIN EXCLUSIVE TRANSACTION`, which
    additionally blocks other connections from reading. -/
def withExclusiveTransaction (conn : Connection) (action : IO α) : IO α :=
  withTransactionKind conn "BEGIN EXCLUSIVE TRANSACTION" "COMMIT TRANSACTION"
    "ROLLBACK TRANSACTION" action

/-- Run `action` inside a fresh, uniquely-named `SAVEPOINT`, releasing it on
    success or rolling back to it (then re-raising) on any thrown
    exception. -/
def withSavepoint (conn : Connection) (action : IO α) : IO α := do
  let n ← conn.connectionTempNameCounter.get
  conn.connectionTempNameCounter.set (n + 1)
  let name := s!"sqlite_simple_savepoint_{n}"
  withTransactionKind conn s!"SAVEPOINT '{name}'" s!"RELEASE '{name}'" s!"ROLLBACK TO '{name}'" action

end Database.SQLite.Simple
