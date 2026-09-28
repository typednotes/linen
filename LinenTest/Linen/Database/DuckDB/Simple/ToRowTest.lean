/-
  Tests for `Linen.Database.DuckDB.Simple.ToRow`.

  Exercises the `ToRow` class and its `Unit`/`Only`/tuple (up to arity 7,
  the cutoff this port implements — see
  `Linen/Database/DuckDB/Simple/ToRow.lean`'s module doc)/`Cons` instances
  by checking the rendered `FieldBinding`s' `columnType` tags (pure —
  `FieldBinding` itself isn't `BEq`, since it carries an `IO` closure), then
  binding a `toRow`-rendered row end-to-end against a real prepared
  statement over a real `:memory:` connection.
-/
import Linen.Database.DuckDB.Simple.ToRow
import Linen.Database.DuckDB.Simple.Internal
import Linen.Database.DuckDB.FFI.PreparedStatements
import Linen.Database.DuckDB.FFI.ExecutePrepared

open Database.DuckDB.Simple
open Database.DuckDB.FFI.Types (Type_)
open Database.DuckDB.FFI.PreparedStatements (prepare)
open Database.DuckDB.FFI.ExecutePrepared (execute)

namespace Tests.Database.DuckDB.Simple.ToRow

#guard (toRow ()).map (·.columnType) == (#[] : Array Type_)

#guard (toRow ({ fromOnly := (5 : Int32) } : Only Int32)).map (·.columnType) == #[Type_.integer]
#guard (toRow ({ fromOnly := "hi" } : Only String)).map (·.columnType) == #[Type_.varchar]

#guard (toRow ((1 : Int32), "two")).map (·.columnType) == #[Type_.integer, Type_.varchar]

#guard (toRow ((1 : Int32), "two", (3.0 : Float))).map (·.columnType) ==
  #[Type_.integer, Type_.varchar, Type_.double]

#guard (toRow ((1 : Int32), "two", (3.0 : Float), (4 : Int32))).map (·.columnType) ==
  #[Type_.integer, Type_.varchar, Type_.double, Type_.integer]

-- `Cons`, composing two smaller `ToRow`s to reach beyond arity 7.
#guard
  (toRow ({ car := { fromOnly := (1 : Int32) }, cdr := { fromOnly := "two" } } :
      Cons (Only Int32) (Only String))).map (·.columnType) ==
    #[Type_.integer, Type_.varchar]

-- ────────────────────────────────────────────────────────────────────
-- End-to-end: bind a `toRow`-rendered row against a real prepared statement
-- ────────────────────────────────────────────────────────────────────

#eval show IO Unit from do
  let conn ← openConnection none -- in-memory database

  withConnectionHandle conn fun connHandle => do
    let (state, createStmt) ← prepare connHandle
      "CREATE TABLE person (id INTEGER, name VARCHAR, score DOUBLE)"
    unless state.isSuccess do throw (IO.userError "prepare CREATE TABLE failed")
    let (_, createResult) ← execute createStmt
    Database.DuckDB.FFI.ExecutePrepared.destroy createResult
    Database.DuckDB.FFI.PreparedStatements.destroy createStmt

    let (state, insertStmt) ← prepare connHandle
      "INSERT INTO person (id, name, score) VALUES (?, ?, ?)"
    unless state.isSuccess do throw (IO.userError "prepare INSERT failed")
    let bindings := toRow ((1 : Int32), "Ada", (3.5 : Float))
    for h : i in [0:bindings.size] do
      bindings[i].bind insertStmt (UInt64.ofNat (i + 1))
    let (execState, insertResult) ← execute insertStmt
    unless execState.isSuccess do throw (IO.userError "execute INSERT failed")
    Database.DuckDB.FFI.ExecutePrepared.destroy insertResult
    Database.DuckDB.FFI.PreparedStatements.destroy insertStmt

  closeConnection conn

end Tests.Database.DuckDB.Simple.ToRow
