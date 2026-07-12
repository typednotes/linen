/-
  Linen.Database.DuckDB.Simple.Internal — `Connection`, `SQLError`, handle
  accessors

  Module #1 of `docs/imports/duckdb-simple/dependencies.md`, on
  `Linen.Database.DuckDB.FFI.{OpenConnect,PreparedStatements,QueryExecution,
  ErrorData,Helpers}`. This is the first module of the new `duckdb-simple`
  import, sitting on top of the already-complete `duckdb-ffi` port
  (`Linen.Database.DuckDB.FFI.*`, 18 modules).

  ## Deviation from upstream

  Checked directly against `Database.DuckDB.Simple.Internal`: upstream's
  version manages *raw* C pointers (`DuckDBDatabase`/`DuckDBConnection`/…)
  behind hand-rolled `bracket`/`alloca`/`poke` calls, because plain
  `Foreign.Ptr` values carry no lifetime discipline of their own — closing a
  connection, or destroying a client context, has to be done by hand exactly
  once. This port's underlying `Linen.Database.DuckDB.FFI.Types` handles
  (`Database`, `Connection`, `ClientContext`, …) are *already* GC-managed
  external objects, each with its own finalizer that idempotently releases
  the DuckDB resource — so the `alloca`/`poke`/`bracket` plumbing upstream
  needs purely to get a raw pointer safely in and out of C has no work to do
  here; this port only needs to track *logical* open/closed state (so a
  caller can't successfully run a query against a `Connection` whose
  `close` has already been called, even though the underlying `Database`/
  `Connection` FFI handles happily outlive that call until the GC catches up).

  Upstream's `Statement` (and its own `StatementState`) is intentionally
  *not* ported here: none of the five `duckdb-simple` modules assigned to
  this batch (`Internal`, `LogicalRep`, `Ok`, `Types`, `FromField`) need it —
  it first becomes necessary for `ToField`/`FromRow`/the top-level facade
  (modules #7/#8/#17), which are out of scope for this batch and will add it
  when ported. Likewise, upstream's `StablePtr`-based delete-callback
  registration helpers (`releaseStablePtrData`, `mkDeleteCallback`) exist
  purely to let a Haskell closure be handed to DuckDB as a `void*` C callback
  for later disposal (used by the not-yet-ported `Logging`/`Copy`/`Function`
  modules) — Lean's FFI has no `StablePtr` equivalent and no closure needs
  disposing via this batch's modules, so this port defers introducing that
  machinery until whichever of `Logging`/`Copy`/`Function` actually needs it
  (a genuine "not yet needed" scope narrowing, not a silent drop of tested
  behaviour — see the module doc convention already used by
  `Linen.Database.DuckDB.FFI.OpenConnect`'s "Configuration" scope note).

  ## Design

  - `Query` wraps a SQL string, exactly mirroring
    `Linen.Database.SQLite.Simple.Types.Query`'s shape (substituting a plain
    `Coe String Query` instance for upstream's `IsString`, per that module's
    own documented rationale) — ported fresh here (not imported) because
    `duckdb-simple` shares no code with `sqlite-simple` (see
    `docs/imports/duckdb-simple/dependencies.md`'s precedence note).
  - `Connection` wraps an `IO.Ref` holding either `none` (closed) or the
    `Database`/`Connection` FFI handle pair (open) — the direct analogue of
    upstream's `IORef ConnectionState`, minus the raw-pointer bracketing
    upstream needs and this port's GC-managed handles don't.
  - `SQLError` is a plain descriptive value (message, optional
    `Database.DuckDB.FFI.Types.ErrorType`, optional offending `Query`).
    Lean's `IO` has no open, extensible exception hierarchy to throw a
    bespoke error type into directly (unlike upstream's
    `Control.Exception.Exception SQLError` instance) — this port's
    `throwSQLError` substitutes by rendering the `SQLError` via its
    `ToString` instance and throwing it as an ordinary `IO.userError`, the
    same substitution `Linen.Database.SQLite.Simple.Ok`'s module doc
    already describes for this codebase's other database ports.
  - `withDatabaseHandle`/`withConnectionHandle`/`withClientContext` are
    bracket-style accessors: the first two just read the `IO.Ref` and throw
    `connectionClosedError` if closed; `withClientContext` additionally
    fetches a fresh `ClientContext` via
    `Linen.Database.DuckDB.FFI.OpenConnect.connectionGetClientContext`, runs
    the action, and destroys it afterwards — the same "acquire, use, release
    early" bracket upstream's version performs, just without the manual
    pointer marshalling.

  ## Haskell source
  - `Database.DuckDB.Simple.Internal` (`duckdb-simple` package, version
    0.1.5.1)
-/

import Linen.Database.DuckDB.FFI.OpenConnect
import Linen.Database.DuckDB.FFI.PreparedStatements
import Linen.Database.DuckDB.FFI.QueryExecution
import Linen.Database.DuckDB.FFI.ErrorData
import Linen.Database.DuckDB.FFI.Helpers

namespace Database.DuckDB.Simple

-- ────────────────────────────────────────────────────────────────────
-- Query
-- ────────────────────────────────────────────────────────────────────

/-- A SQL query string, wrapped to discourage building one by ad hoc string
    concatenation. -/
structure Query where
  fromQuery : String
deriving BEq, Ord, Repr, Inhabited

namespace Query

/-- Build a `Query` from a plain string (upstream's `IsString.fromString`). -/
@[inline] def ofString (s : String) : Query := ⟨s⟩

instance : Coe String Query := ⟨ofString⟩

instance : ToString Query where
  toString q := q.fromQuery

/-- The empty query (upstream's `Monoid` identity). -/
def empty : Query := ⟨""⟩

/-- Concatenate two queries' underlying text (upstream's `Semigroup`
    `(<>)`). -/
def append (a b : Query) : Query := ⟨a.fromQuery ++ b.fromQuery⟩

instance : Append Query := ⟨append⟩

end Query

-- ────────────────────────────────────────────────────────────────────
-- SQLError
-- ────────────────────────────────────────────────────────────────────

/-- An error reported by DuckDB itself, or by this port's own bookkeeping
    (e.g. an operation attempted against a closed `Connection`). -/
structure SQLError where
  /-- A human-readable description of the failure. -/
  message : String
  /-- The DuckDB error classification, if the error originated from a
      `duckdb_error_type`-carrying result. -/
  errorType : Option Database.DuckDB.FFI.Types.ErrorType := none
  /-- The query that triggered the failure, if any. -/
  query : Option Query := none
deriving Repr, Inhabited

instance : ToString SQLError where
  toString e :=
    match e.query with
    | some q => s!"duckdb-simple: {e.message} (query: {q.fromQuery})"
    | none => s!"duckdb-simple: {e.message}"

/-- Throw `err` as a plain `IO` exception, rendering it via its `ToString`
    instance (Lean's `IO` has no open, extensible exception hierarchy to
    throw a bespoke error type into directly — see
    `Linen.Database.SQLite.Simple.Ok`'s module doc for this codebase's
    other database ports' matching substitution). -/
def throwSQLError (err : SQLError) : IO α :=
  throw (IO.userError (toString err))

/-- The shared error value used when an operation targets a closed
    connection. -/
def connectionClosedError : SQLError :=
  { message := "connection is closed" }

/-- Build an `SQLError` reporting a failed registration (e.g. of a callback
    or configuration option); substitutes for upstream's
    `throwRegistrationError`. -/
def registrationError (label : String) : SQLError :=
  { message := s!"{label} failed" }

-- ────────────────────────────────────────────────────────────────────
-- Connection
-- ────────────────────────────────────────────────────────────────────

/-- The live pair of FFI handles backing an open `Connection`. -/
structure ConnectionHandles where
  database : Database.DuckDB.FFI.Types.Database
  connection : Database.DuckDB.FFI.Types.Connection

/-- Tracks the lifetime of a DuckDB database/connection pair. `none` once
    `closeConnection` has been called. -/
structure Connection where
  state : IO.Ref (Option ConnectionHandles)

/-- Open a connection to the database at `path` (`none`, or `":memory:"`,
    for an in-memory database), per
    `Linen.Database.DuckDB.FFI.OpenConnect.openDatabase`/`connect`. -/
def openConnection (path : Option String) : IO Connection := do
  match ← Database.DuckDB.FFI.OpenConnect.openDatabase path with
  | .error msg => throwSQLError (SQLError.mk msg none none)
  | .ok db =>
    match ← Database.DuckDB.FFI.OpenConnect.connect db with
    | .error msg =>
      Database.DuckDB.FFI.OpenConnect.close db
      throwSQLError (SQLError.mk msg none none)
    | .ok conn =>
      let ref ← IO.mkRef (some { database := db, connection := conn })
      pure { state := ref }

/-- Close `conn`, releasing its underlying `Database`/`Connection` handles.
    Subsequent operations against `conn` fail with `connectionClosedError`.
    Idempotent: closing an already-closed connection is a no-op. -/
def closeConnection (conn : Connection) : IO Unit := do
  match ← conn.state.get with
  | none => pure ()
  | some handles =>
    conn.state.set none
    Database.DuckDB.FFI.OpenConnect.disconnect handles.connection
    Database.DuckDB.FFI.OpenConnect.close handles.database

/-- Run `action` against `conn`'s underlying `Database` handle, throwing
    `connectionClosedError` if `conn` has been closed. -/
def withDatabaseHandle (conn : Connection) (action : Database.DuckDB.FFI.Types.Database → IO α) :
    IO α := do
  match ← conn.state.get with
  | none => throwSQLError connectionClosedError
  | some handles => action handles.database

/-- Run `action` against `conn`'s underlying `Connection` handle, throwing
    `connectionClosedError` if `conn` has been closed. -/
def withConnectionHandle
    (conn : Connection) (action : Database.DuckDB.FFI.Types.Connection → IO α) : IO α := do
  match ← conn.state.get with
  | none => throwSQLError connectionClosedError
  | some handles => action handles.connection

/-- Run `action` against a fresh `ClientContext` fetched from `conn`'s
    connection, destroying the context afterwards regardless of whether
    `action` succeeded. -/
def withClientContext
    (conn : Connection) (action : Database.DuckDB.FFI.Types.ClientContext → IO α) : IO α :=
  withConnectionHandle conn fun connHandle => do
    let ctx ← Database.DuckDB.FFI.OpenConnect.connectionGetClientContext connHandle
    try
      action ctx
    finally
      Database.DuckDB.FFI.OpenConnect.destroyClientContext ctx

end Database.DuckDB.Simple
