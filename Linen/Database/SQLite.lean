/-
  Linen.Database.SQLite3 — public UTF-8 low-level SQLite3 API

  The user-facing surface of the ported `direct-sqlite` FFI layer: it wraps
  `Linen.Database.SQLite.Direct`'s `Except Error`-returning operations with
  `IO`-throwing ones (`IO.userError`, formatted the same way as
  `Linen.Database.PostgreSQL.LibPQ.execCheck`/`connectCheck`), and adds the
  `SQLData` variant type used to read/write untyped column values.

  This is the layer `Linen.Database.SQLite.Simple` (a future, not-yet-ported
  module — see `docs/imports/sqlite-simple/dependencies.md`, module #15) will
  build its typed row/parameter conversions on top of.

  ## Scope

  Mirrors upstream `Database.SQLite3`, minus the two features already
  scoped out of `Bindings`/`Direct` for lack of a callback-into-Lean
  mechanism in this codebase: `execWithCallback`/`execPrint` (row callback)
  and `setTrace`/`trace`. `interruptibly` (upstream: run an IO action on a
  forked thread so an async exception can `interrupt` it) is also omitted —
  it depends on GHC's asynchronous-exception/`forkIO` semantics, which has
  no Lean `IO` analogue; callers needing to interrupt a long statement can
  call `interrupt` directly from another `IO` thread (`IO.asTask`) around a
  `step` loop.

  ## Haskell source
  - `Database.SQLite3` (`direct-sqlite` package)
-/

import Linen.Database.SQLite.Direct

namespace Database.SQLite3

open Database.SQLite3.Bindings.Types (Error ColumnType StepResult ParamIndex ColumnIndex ColumnCount FuncContext FuncArgs)

/-- A database connection. -/
abbrev Database : Type := Direct.Database

/-- A prepared statement. -/
abbrev Statement : Type := Direct.Statement

/-- An untyped SQLite value, as read from or bound to a column/parameter. -/
inductive SQLData where
  | integer (n : Int64)
  | float (x : Float)
  | text (s : String)
  | blob (b : ByteArray)
  | null
  deriving Inhabited

instance : BEq SQLData where
  beq
    | .integer a, .integer b => a == b
    | .float a, .float b => a == b
    | .text a, .text b => a == b
    | .blob a, .blob b => a == b
    | .null, .null => true
    | _, _ => false

/-- Raised when SQLite reports an error, mirroring
    `Database.SQLite3.SQLError`. -/
structure SQLError where
  error   : Error
  details : String
  context : String
  deriving Inhabited

instance : ToString SQLError where
  toString e := s!"SQLite3 returned {repr e.error} while attempting to perform {e.context}: {e.details}"

private def throwSQLError (details : String) (context : String) (err : Error) : IO α :=
  throw (IO.userError (toString ({ error := err, details, context } : SQLError)))

private def checkDb (db : @& Database) (context : String) : Except Error α → IO α
  | .ok a => pure a
  | .error err => do
    let details ← Direct.errmsg db
    throwSQLError details context err

private def checkStmt (stmt : Statement) (context : String) : Except Error α → IO α
  | .ok a => pure a
  | .error err => do
    let details ← Direct.errmsg stmt.db
    throwSQLError details context err

-- ────────────────────────────────────────────────────────────────────
-- Connection management
-- ────────────────────────────────────────────────────────────────────

/-- Open (or create) a SQLite database at `path`. Throws `SQLError` on
    failure. Pass `":memory:"` for a private in-memory database, or `""`
    for a private on-disk temporary database. -/
def open_ (path : String) : IO Database := do
  let (result, db) ← Direct.open_ path
  match result with
  | .ok _ => return db
  | .error err =>
    let details ← Direct.errmsg db
    throwSQLError details s!"open {path}" err

/-- Close a database connection. Throws `SQLError` on failure. -/
def close (db : Database) : IO Unit := do
  let result ← Direct.close db
  checkDb db "close" result

/-- Execute zero or more semicolon-separated SQL statements, discarding any
    result rows. Throws `SQLError` on failure. -/
def exec (db : Database) (sql : String) : IO Unit := do
  let (result, msg) ← Direct.execMsg db sql
  match result with
  | .ok _ => return ()
  | .error err => throwSQLError msg s!"exec {sql}" err

/-- Interrupt any pending long-running operation on this connection. -/
def interrupt (db : @& Database) : IO Unit := Direct.interrupt db

-- ────────────────────────────────────────────────────────────────────
-- Statement management
-- ────────────────────────────────────────────────────────────────────

/-- Compile the first SQL statement in `sql`. Throws `SQLError` if `sql`
    contains no statements (matching upstream's `fail`) or if compilation
    fails. -/
def prepare (db : Database) (sql : String) : IO Statement := do
  let result ← Direct.prepare db sql
  match result with
  | .ok (some stmt) => return stmt
  | .ok none =>
    throw (IO.userError s!"Database.SQLite3.prepare: empty query string ({sql})")
  | .error err =>
    let details ← Direct.errmsg db
    throwSQLError details s!"prepare {sql}" err

/-- Advance a statement to its next row (or completion). Throws `SQLError`
    on failure. -/
def step (stmt : Statement) : IO StepResult := do
  let result ← Direct.step stmt
  checkStmt stmt "step" result

/-- Reset a statement for re-execution. Never throws (matching upstream:
    a failed prior `step`'s error code is discarded rather than reported
    twice). -/
def reset (stmt : Statement) : IO Unit := discard (Direct.reset stmt)

/-- Destroy a prepared statement. Never throws. -/
def finalize (stmt : Statement) : IO Unit := discard (Direct.finalize stmt)

/-- Set every bound parameter back to `NULL`. -/
def clearBindings (stmt : Statement) : IO Unit := Direct.clearBindings stmt

-- ────────────────────────────────────────────────────────────────────
-- Parameter and column information
-- ────────────────────────────────────────────────────────────────────

def bindParameterCount (stmt : Statement) : IO ParamIndex := Direct.bindParameterCount stmt

def bindParameterName (stmt : Statement) (idx : ParamIndex) : IO (Option String) :=
  Direct.bindParameterName stmt idx

def columnCount (stmt : Statement) : IO ColumnCount := Direct.columnCount stmt

/-- The name of a result column, or `none` if `idx` is out of range. -/
def columnName (stmt : Statement) (idx : ColumnIndex) : IO (Option String) :=
  Direct.columnName stmt idx

-- ────────────────────────────────────────────────────────────────────
-- Binding values to a prepared statement
-- ────────────────────────────────────────────────────────────────────

def bindInt (stmt : Statement) (idx : ParamIndex) (v : Int) : IO Unit := do
  let result ← Direct.bindInt64 stmt idx (Int64.ofInt v)
  checkStmt stmt "bind int" result

def bindInt64 (stmt : Statement) (idx : ParamIndex) (v : Int64) : IO Unit := do
  let result ← Direct.bindInt64 stmt idx v
  checkStmt stmt "bind int64" result

def bindDouble (stmt : Statement) (idx : ParamIndex) (v : Float) : IO Unit := do
  let result ← Direct.bindDouble stmt idx v
  checkStmt stmt "bind double" result

def bindText (stmt : Statement) (idx : ParamIndex) (v : String) : IO Unit := do
  let result ← Direct.bindText stmt idx v
  checkStmt stmt "bind text" result

def bindBlob (stmt : Statement) (idx : ParamIndex) (v : ByteArray) : IO Unit := do
  let result ← Direct.bindBlob stmt idx v
  checkStmt stmt "bind blob" result

def bindNull (stmt : Statement) (idx : ParamIndex) : IO Unit := do
  let result ← Direct.bindNull stmt idx
  checkStmt stmt "bind null" result

/-- Bind an untyped `SQLData` value to a single parameter. -/
def bindSQLData (stmt : Statement) (idx : ParamIndex) : SQLData → IO Unit
  | .integer v => bindInt64 stmt idx v
  | .float v   => bindDouble stmt idx v
  | .text v    => bindText stmt idx v
  | .blob v    => bindBlob stmt idx v
  | .null      => bindNull stmt idx

/-- Bind every parameter of a prepared statement in order (1-based). Throws
    if the number of values doesn't match `bindParameterCount`. -/
def bind (stmt : Statement) (values : Array SQLData) : IO Unit := do
  let n ← bindParameterCount stmt
  if n.toNat ≠ values.size then
    throw (IO.userError
      s!"mismatched parameter count for bind: statement needs {n}, {values.size} given")
  for h : i in [0:values.size] do
    bindSQLData stmt (UInt32.ofNat (i + 1)) values[i]

/-- Bind every named parameter of a prepared statement. Throws if any name
    doesn't match a parameter in the statement, or if the count of
    parameters given doesn't match `bindParameterCount`. -/
def bindNamed (stmt : Statement) (values : Array (String × SQLData)) : IO Unit := do
  let n ← bindParameterCount stmt
  if n.toNat ≠ values.size then
    throw (IO.userError
      s!"mismatched parameter count for bind: statement needs {n}, {values.size} given")
  for (name, value) in values do
    match ← Direct.bindParameterIndex stmt name with
    | some idx => bindSQLData stmt idx value
    | none => throw (IO.userError s!"unknown named parameter {name}")

-- ────────────────────────────────────────────────────────────────────
-- Reading the result row
-- ────────────────────────────────────────────────────────────────────

def columnType (stmt : Statement) (idx : ColumnIndex) : IO ColumnType := Direct.columnType stmt idx

def columnInt64 (stmt : Statement) (idx : ColumnIndex) : IO Int64 := Direct.columnInt64 stmt idx

def columnDouble (stmt : Statement) (idx : ColumnIndex) : IO Float := Direct.columnDouble stmt idx

def columnText (stmt : Statement) (idx : ColumnIndex) : IO String := Direct.columnText stmt idx

def columnBlob (stmt : Statement) (idx : ColumnIndex) : IO ByteArray := Direct.columnBlob stmt idx

/-- Read a single column as an untyped `SQLData`, dispatching on its
    reported `columnType`. -/
def column (stmt : Statement) (idx : ColumnIndex) : IO SQLData := do
  match ← columnType stmt idx with
  | .integer => .integer <$> columnInt64 stmt idx
  | .float   => .float <$> columnDouble stmt idx
  | .text    => .text <$> columnText stmt idx
  | .blob    => .blob <$> columnBlob stmt idx
  | .null    => return .null

/-- Read every column of the current row as `SQLData`. -/
def columns (stmt : Statement) : IO (Array SQLData) := do
  let n ← columnCount stmt
  let mut out := #[]
  for i in [0:n.toNat] do
    out := out.push (← column stmt (UInt32.ofNat i))
  return out

-- ────────────────────────────────────────────────────────────────────
-- Result statistics
-- ────────────────────────────────────────────────────────────────────

/-- The `rowid` of the most recent successful `INSERT`. -/
def lastInsertRowId (db : @& Database) : IO Int64 := Direct.lastInsertRowId db

/-- Rows changed, inserted, or deleted by the most recent statement. -/
def changes (db : @& Database) : IO Int64 := Direct.changes db

/-- Total rows changed, inserted, or deleted since this connection was
    opened. -/
def totalChanges (db : @& Database) : IO Int64 := Direct.totalChanges db

-- ────────────────────────────────────────────────────────────────────
-- User-defined scalar SQL functions (added for module #16 of
-- `docs/imports/sqlite-simple/dependencies.md`,
-- `Linen.Database.SQLite.Simple.Function`; see `Direct.lean`'s section of
-- the same name and `ffi/sqlite3_shim.c` for the underlying machinery)
-- ────────────────────────────────────────────────────────────────────

/-- Read a scalar function callback's `idx`-th argument as an untyped
    `SQLData`, dispatching on its reported type (mirrors `column` above). -/
def funcArgValue (args : FuncArgs) (idx : UInt32) : IO SQLData := do
  match ← Direct.funcArgType args idx with
  | .integer => .integer <$> Direct.funcArgInt64 args idx
  | .float   => .float <$> Direct.funcArgDouble args idx
  | .text    => .text <$> Direct.funcArgText args idx
  | .blob    => .blob <$> Direct.funcArgBlob args idx
  | .null    => return .null

/-- Report a scalar function's result (mirrors `bindSQLData` above). -/
def funcResultValue (ctx : FuncContext) : SQLData → IO Unit
  | .integer v => Direct.funcResultInt64 ctx v
  | .float v   => Direct.funcResultDouble ctx v
  | .text v    => Direct.funcResultText ctx v
  | .blob v    => Direct.funcResultBlob ctx v
  | .null      => Direct.funcResultNull ctx

/-- Register a UTF-8 scalar SQL function of `nArg` arguments. Throws
    `SQLError` on failure (e.g. a name/argument-count SQLite rejects). -/
def createFunction (db : Database) (name : String) (nArg : Int32) (deterministic : Bool)
    (f : FuncContext → FuncArgs → UInt32 → IO Unit) : IO Unit := do
  let result ← Direct.createFunction db name nArg deterministic f
  checkDb db s!"create function {name}" result

/-- Remove a scalar SQL function registration. Throws `SQLError` on
    failure. -/
def deleteFunction (db : Database) (name : String) (nArg : Int32) : IO Unit := do
  let result ← Direct.deleteFunction db name nArg
  checkDb db s!"delete function {name}" result

end Database.SQLite3
