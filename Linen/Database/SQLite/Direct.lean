/-
  Linen.Database.SQLite.Direct — thin, error-code-checking SQLite3 wrapper

  A slightly higher-level API than `Linen.Database.SQLite.Bindings`:

  - it decodes raw result codes into `Database.SQLite3.Bindings.Types.Error`
    and returns them via `Except Error` instead of leaving callers to check
    a raw `UInt32`;
  - it does no further conversion of `String`/`ByteArray` payloads.

  Upstream's `Database.SQLite3.Direct` additionally exists to avoid forcing
  a `ByteString`↔`Text` (UTF-8 decode/validate) round-trip on every value —
  its `Utf8` newtype is *undecoded* UTF-8 bytes, as distinct from
  `Database.SQLite3`'s real, validated `Text`. Lean's `String` has no such
  split: it is always valid UTF-8 internally, and the C shim
  (`ffi/sqlite3_shim.c`) already produces a Lean `String` directly from
  SQLite's UTF-8 output via `lean_mk_string_from_bytes` — there is no extra
  decode step for this layer to *skip*. So this module uses plain `String`/
  `ByteArray` throughout, the same substitution
  `docs/imports/sqlite-simple/dependencies.md` records for `bytestring`/
  `text` generally (already-ported, and not re-introduced at this layer).

  `Statement` here additionally carries the owning `Database`, so this
  layer (and `Linen.Database.SQLite` above it) can look up a connection's
  `errmsg` for a statement-level error without needing
  `sqlite3_db_handle` — a small, deliberate simplification over upstream's
  `getStatementDatabase`, since the association is already known on the
  Lean side and re-deriving it from SQLite gains nothing.

  ## Haskell source
  - `Database.SQLite3.Direct` (`direct-sqlite` package)
-/

import Linen.Database.SQLite.Bindings

namespace Database.SQLite3.Direct

open Database.SQLite3.Bindings.Types (Error ColumnType StepResult ParamIndex ColumnIndex ColumnCount FuncContext FuncArgs)
open Database.SQLite3.Bindings.Types renaming Database → RawDatabase, Statement → RawStatement

/-- A live (or formerly-live) SQLite database connection. -/
abbrev Database : Type := RawDatabase

/-- A prepared SQL statement, paired with the connection it was prepared
    against (see the module doc for why). -/
structure Statement where
  db  : Database
  raw : RawStatement

/-- The result of a `step`. -/
abbrev Result (α : Type) : Type := Except Error α

private def toResult (rc : UInt32) (a : α) : Result α :=
  if rc == 0 then .ok a else .error (Error.ofUInt32 rc)

private def toStepResult (rc : UInt32) : Result StepResult :=
  match Error.ofUInt32 rc with
  | .row  => .ok .row
  | .done => .ok .done
  | err   => .error err

-- ────────────────────────────────────────────────────────────────────
-- Connection management
-- ────────────────────────────────────────────────────────────────────

/-- Open a database connection. Always returns a `Database`, even for an
    error result, so the caller can retrieve a descriptive message via
    `errmsg`. -/
def open_ (path : String) : IO (Result Database × Database) := do
  let (rc, db) ← Bindings.openRaw path
  return (toResult rc db, db)

/-- Close a database connection. -/
def close (db : Database) : IO (Result Unit) := do
  let rc ← Bindings.closeRaw db
  return toResult rc ()

/-- The most recent error message for this connection. -/
def errmsg (db : @& Database) : IO String := Bindings.errmsg db

/-- Interrupt any pending long-running operation on this connection. -/
def interrupt (db : @& Database) : IO Unit := Bindings.interrupt db

/-- Whether the connection is in autocommit mode. -/
def getAutoCommit (db : @& Database) : IO Bool := Bindings.getAutocommit db

-- ────────────────────────────────────────────────────────────────────
-- Simple query execution
-- ────────────────────────────────────────────────────────────────────

/-- Execute zero or more semicolon-separated statements, discarding any
    result rows and any SQLite-provided error message (see `execMsg` for the
    variant that also returns the message). -/
def exec (db : @& Database) (sql : String) : IO (Result Unit) := do
  let (rc, _msg) ← Bindings.execRaw db sql
  return toResult rc ()

/-- Like `exec`, but also returns the SQLite-provided error message on
    failure (empty string on success). -/
def execMsg (db : @& Database) (sql : String) : IO (Result Unit × String) := do
  let (rc, msg) ← Bindings.execRaw db sql
  return (toResult rc (), msg)

-- ────────────────────────────────────────────────────────────────────
-- Statement management
-- ────────────────────────────────────────────────────────────────────

/-- Compile the first statement in `sql`. If `sql` has no statements,
    succeeds with `none`. -/
def prepare (db : Database) (sql : String) : IO (Result (Option Statement)) := do
  let (rc, rawOpt) ← Bindings.prepareRaw db sql
  if rc == 0 then
    return .ok (rawOpt.map fun raw => { db, raw })
  else
    return .error (Error.ofUInt32 rc)

/-- Advance a statement to its next row (or completion). -/
def step (stmt : Statement) : IO (Result StepResult) := do
  let rc ← Bindings.step stmt.raw
  return toStepResult rc

/-- Reset a statement so it can be `step`ped again from the start. -/
def reset (stmt : Statement) : IO (Result Unit) := do
  let rc ← Bindings.reset stmt.raw
  return toResult rc ()

/-- Destroy a prepared statement. -/
def finalize (stmt : Statement) : IO (Result Unit) := do
  let rc ← Bindings.finalizeRaw stmt.raw
  return toResult rc ()

/-- Set every bound parameter back to `NULL`. -/
def clearBindings (stmt : Statement) : IO Unit := do
  let _ ← Bindings.clearBindings stmt.raw
  return ()

-- ────────────────────────────────────────────────────────────────────
-- Parameter and column information
-- ────────────────────────────────────────────────────────────────────

def bindParameterCount (stmt : Statement) : IO ParamIndex :=
  Bindings.bindParameterCount stmt.raw

def bindParameterName (stmt : Statement) (idx : ParamIndex) : IO (Option String) :=
  Bindings.bindParameterName stmt.raw idx

/-- The index of the named parameter, or `none` if this statement has no
    such parameter. -/
def bindParameterIndex (stmt : Statement) (name : String) : IO (Option ParamIndex) := do
  let idx ← Bindings.bindParameterIndex stmt.raw name
  return if idx == 0 then none else some idx

def columnCount (stmt : Statement) : IO ColumnCount :=
  Bindings.columnCount stmt.raw

def columnName (stmt : Statement) (idx : ColumnIndex) : IO (Option String) :=
  Bindings.columnName stmt.raw idx

-- ────────────────────────────────────────────────────────────────────
-- Binding values to a prepared statement
-- ────────────────────────────────────────────────────────────────────

def bindInt64 (stmt : Statement) (idx : ParamIndex) (v : Int64) : IO (Result Unit) := do
  let rc ← Bindings.bindInt64 stmt.raw idx v
  return toResult rc ()

def bindDouble (stmt : Statement) (idx : ParamIndex) (v : Float) : IO (Result Unit) := do
  let rc ← Bindings.bindDouble stmt.raw idx v
  return toResult rc ()

def bindText (stmt : Statement) (idx : ParamIndex) (v : String) : IO (Result Unit) := do
  let rc ← Bindings.bindText stmt.raw idx v
  return toResult rc ()

def bindBlob (stmt : Statement) (idx : ParamIndex) (v : ByteArray) : IO (Result Unit) := do
  let rc ← Bindings.bindBlob stmt.raw idx v
  return toResult rc ()

def bindNull (stmt : Statement) (idx : ParamIndex) : IO (Result Unit) := do
  let rc ← Bindings.bindNull stmt.raw idx
  return toResult rc ()

-- ────────────────────────────────────────────────────────────────────
-- Reading the result row
-- ────────────────────────────────────────────────────────────────────

def columnType (stmt : Statement) (idx : ColumnIndex) : IO ColumnType := do
  let raw ← Bindings.columnType stmt.raw idx
  return ColumnType.ofUInt32 raw

def columnInt64 (stmt : Statement) (idx : ColumnIndex) : IO Int64 :=
  Bindings.columnInt64 stmt.raw idx

def columnDouble (stmt : Statement) (idx : ColumnIndex) : IO Float :=
  Bindings.columnDouble stmt.raw idx

def columnText (stmt : Statement) (idx : ColumnIndex) : IO String :=
  Bindings.columnText stmt.raw idx

def columnBlob (stmt : Statement) (idx : ColumnIndex) : IO ByteArray :=
  Bindings.columnBlob stmt.raw idx

-- ────────────────────────────────────────────────────────────────────
-- Result statistics
-- ────────────────────────────────────────────────────────────────────

def lastInsertRowId (db : @& Database) : IO Int64 := Bindings.lastInsertRowId db

def changes (db : @& Database) : IO Int64 := Bindings.changes db

def totalChanges (db : @& Database) : IO Int64 := Bindings.totalChanges db

-- ────────────────────────────────────────────────────────────────────
-- User-defined scalar SQL functions (see `Bindings.lean`'s own section of
-- the same name, added for module #16)
-- ────────────────────────────────────────────────────────────────────

/-- Register a scalar SQL function (see `Bindings.createFunctionRaw`). -/
def createFunction (db : Database) (name : String) (nArg : Int32) (deterministic : Bool)
    (f : FuncContext → FuncArgs → UInt32 → IO Unit) : IO (Result Unit) := do
  let rc ← Bindings.createFunctionRaw db name nArg deterministic f
  return toResult rc ()

/-- Remove a scalar SQL function registration (see
    `Bindings.deleteFunctionRaw`). -/
def deleteFunction (db : Database) (name : String) (nArg : Int32) : IO (Result Unit) := do
  let rc ← Bindings.deleteFunctionRaw db name nArg
  return toResult rc ()

def funcArgType (args : @& FuncArgs) (idx : UInt32) : IO ColumnType := do
  let raw ← Bindings.funcArgType args idx
  return ColumnType.ofUInt32 raw

def funcArgInt64 (args : @& FuncArgs) (idx : UInt32) : IO Int64 := Bindings.funcArgInt64 args idx

def funcArgDouble (args : @& FuncArgs) (idx : UInt32) : IO Float := Bindings.funcArgDouble args idx

def funcArgText (args : @& FuncArgs) (idx : UInt32) : IO String := Bindings.funcArgText args idx

def funcArgBlob (args : @& FuncArgs) (idx : UInt32) : IO ByteArray := Bindings.funcArgBlob args idx

def funcResultInt64 (ctx : @& FuncContext) (v : Int64) : IO Unit := Bindings.funcResultInt64 ctx v

def funcResultDouble (ctx : @& FuncContext) (v : Float) : IO Unit := Bindings.funcResultDouble ctx v

def funcResultText (ctx : @& FuncContext) (v : String) : IO Unit := Bindings.funcResultText ctx v

def funcResultBlob (ctx : @& FuncContext) (v : ByteArray) : IO Unit := Bindings.funcResultBlob ctx v

def funcResultNull (ctx : @& FuncContext) : IO Unit := Bindings.funcResultNull ctx

end Database.SQLite3.Direct
