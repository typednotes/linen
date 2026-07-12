/-
  Linen.Database.SQLite.Simple.Internal — `Connection`, `Statement`, `Field`

  Module #9 of `docs/imports/sqlite-simple/dependencies.md`, on module #4
  (`Linen.Database.SQLite`) and module #6 (`Linen.Database.SQLite.Simple.Ok`,
  imported transitively so later modules built on this one can use `Ok`
  without a separate import).

  ## Deviation from upstream

  Checked directly against `Database.SQLite.Simple.Internal`: upstream's
  version of this module declares `Connection`, `ColumnOutOfBounds`, `Field`,
  and the `RowParser` applicative (built on `ReaderT`/`StateT`/`Ok`) — but
  *not* `Statement`, which upstream instead declares in the top-level
  `Database.SQLite.Simple` module (module #15, not yet ported). Per
  `docs/imports/sqlite-simple/dependencies.md`'s own description of this
  module (module #9: "`Connection`, `Statement`, `Field` … and
  connection-open/close bookkeeping"), this port relocates that thin
  `Statement` handle-wrapper here instead, alongside `Connection` — the two
  are equally simple plumbing, and doing so lets a not-yet-ported module
  reuse one instead of needing to wait on the full `Database.SQLite.Simple`
  facade. `RowParser` is *not* ported here: it is `FromRow`'s (module #12)
  supporting machinery, out of this module's scope per the dependency plan,
  and is left for that module's own port. `ColumnOutOfBounds` is also
  dropped: Lean has no open exception hierarchy to throw it into (the same
  substitution `Linen.Database.SQLite.Simple.Ok`'s module doc records for
  upstream's `MonadThrow`); a later `FromRow`/`FromField` port can report an
  out-of-range column directly as an `Ok.fail` message instead.

  ## Design

  - `Connection` wraps the low-level `Database.SQLite3.Database` handle plus
    a mutable counter (`IO.Ref Nat`, substituting upstream's `IORef Word64`)
    used by not-yet-ported code (`withSavepoint`, module #15) to generate
    unique savepoint names.
  - `Statement` pairs a low-level `Database.SQLite3.Statement` with the
    `Connection` it was prepared against — mirroring `Linen.Database.SQLite.
    Direct.Statement`'s own "carry the owning database" design, needed here
    so a failed bind/step can still report `errmsg` without extra plumbing.
  - `Field` is a decoded column value (`Database.SQLite3.SQLData`) plus its
    zero-based column index and (if SQLite reports one) column name, so a
    `FromField` conversion failure (module #11, not yet ported) can name the
    offending column in its error message. Upstream's `Field` carries only
    the value and index, deriving the "declared type" on demand via
    `gettypename`; this port keeps that derivation as `Field.typeName`
    (ported below) rather than a redundant stored field, per this module's
    task description ("a decoded column value plus its declared SQLite type
    … for error messages").

  ## Haskell source
  - `Database.SQLite.Simple.Internal` (`sqlite-simple` package)
-/

import Linen.Database.SQLite
import Linen.Database.SQLite.Simple.Ok

namespace Database.SQLite.Simple

-- ────────────────────────────────────────────────────────────────────
-- Connection
-- ────────────────────────────────────────────────────────────────────

/-- A connection to an open SQLite database. `connectionHandle` gives access
    to the underlying low-level `Database.SQLite3.Database`, for callers
    needing functionality this port doesn't (yet) expose at this level. -/
structure Connection where
  /-- The underlying low-level database handle. -/
  connectionHandle : Database.SQLite3.Database
  /-- A counter used to generate unique temporary names (e.g. for
      `SAVEPOINT`s). -/
  connectionTempNameCounter : IO.Ref Nat

/-- Open a connection to the SQLite database at `path` (see
    `Database.SQLite3.open_` for the accepted forms of `path`, including
    `":memory:"` and `""`). -/
def openConnection (path : String) : IO Connection := do
  let handle ← Database.SQLite3.open_ path
  let counter ← IO.mkRef 0
  return { connectionHandle := handle, connectionTempNameCounter := counter }

/-- Close a connection. -/
def closeConnection (conn : Connection) : IO Unit :=
  Database.SQLite3.close conn.connectionHandle

-- ────────────────────────────────────────────────────────────────────
-- Statement
-- ────────────────────────────────────────────────────────────────────

/-- A prepared statement, together with the connection it was prepared
    against (see the module doc for why `Statement` lives here rather than
    in the not-yet-ported `Database.SQLite.Simple` facade). -/
structure Statement where
  /-- The connection this statement was prepared against. -/
  statementConnection : Connection
  /-- The underlying low-level prepared statement. -/
  statementHandle : Database.SQLite3.Statement

/-- Compile the first SQL statement in `sql` against `conn`. -/
def openStatement (conn : Connection) (sql : String) : IO Statement := do
  let handle ← Database.SQLite3.prepare conn.connectionHandle sql
  return { statementConnection := conn, statementHandle := handle }

/-- Destroy a prepared statement. -/
def closeStatement (stmt : Statement) : IO Unit :=
  Database.SQLite3.finalize stmt.statementHandle

-- ────────────────────────────────────────────────────────────────────
-- Field
-- ────────────────────────────────────────────────────────────────────

/-- A single decoded column value from a result row, together with enough
    positional metadata (its column index, and name if known) for a later
    `FromField` conversion to report a descriptive error. -/
structure Field where
  /-- The decoded column value. -/
  result : Database.SQLite3.SQLData
  /-- The zero-based index of this column within its row. -/
  column : Nat
  /-- The column's declared name, if SQLite reported one for this query. -/
  columnName : Option String := none
deriving Inhabited

namespace Field

/-- The SQLite storage-class name of this field's value (`"INTEGER"`,
    `"FLOAT"`, `"TEXT"`, `"BLOB"`, or `"NULL"`), for use in a conversion
    error message. Matches upstream's `gettypename`. -/
def typeName (f : Field) : String :=
  match f.result with
  | .integer _ => "INTEGER"
  | .float _ => "FLOAT"
  | .text _ => "TEXT"
  | .blob _ => "BLOB"
  | .null => "NULL"

end Field

/-- Read every column of a prepared statement's current result row as
    `Field`s, tagging each with its column index and (if available) name. -/
def currentRowFields (stmt : Statement) : IO (Array Field) := do
  let values ← Database.SQLite3.columns stmt.statementHandle
  let mut out := #[]
  for h : i in [0:values.size] do
    let name ← Database.SQLite3.columnName stmt.statementHandle (UInt32.ofNat i)
    out := out.push { result := values[i], column := i, columnName := name }
  return out

end Database.SQLite.Simple
