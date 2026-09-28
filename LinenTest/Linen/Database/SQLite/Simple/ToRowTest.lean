/-
  Tests for `Linen.Database.SQLite.Simple.ToRow`.

  Exercises the `ToRow` class and its `Unit`/`Only`/tuple (up to arity 7,
  the cutoff this port implements — see
  `Linen/Database/SQLite/Simple/ToRow.lean`'s module doc)/`Cons` instances,
  converting Lean values into the flat `Array SQLData` that
  `Database.SQLite3.bind` accepts.
-/
import Linen.Database.SQLite.Simple.ToRow
import Linen.Database.SQLite.Simple.Internal

open Database.SQLite.Simple
open Database.SQLite3 (SQLData)

namespace Tests.Database.SQLite.Simple.ToRow

#guard toRow () == (#[] : Array SQLData)

#guard toRow (Types.Only.mk (5 : Int)) == #[SQLData.integer 5]
#guard toRow (Types.Only.mk "hi") == #[SQLData.text "hi"]

#guard toRow ((1 : Int), "two") == #[SQLData.integer 1, SQLData.text "two"]

#guard toRow ((1 : Int), "two", (3.0 : Float)) ==
  #[SQLData.integer 1, SQLData.text "two", SQLData.float 3.0]

#guard toRow ((1 : Int), "two", (3.0 : Float), (4 : Int)) ==
  #[SQLData.integer 1, SQLData.text "two", SQLData.float 3.0, SQLData.integer 4]

#guard toRow ((1 : Int), "two", (3.0 : Float), (4 : Int), "five") ==
  #[SQLData.integer 1, SQLData.text "two", SQLData.float 3.0, SQLData.integer 4, SQLData.text "five"]

#guard toRow ((1 : Int), "two", (3.0 : Float), (4 : Int), "five", (6 : Int)) ==
  #[SQLData.integer 1, SQLData.text "two", SQLData.float 3.0, SQLData.integer 4,
    SQLData.text "five", SQLData.integer 6]

-- arity 7, the top hand-written tuple instance
#guard toRow ((1 : Int), "two", (3.0 : Float), (4 : Int), "five", (6 : Int), "seven") ==
  #[SQLData.integer 1, SQLData.text "two", SQLData.float 3.0, SQLData.integer 4,
    SQLData.text "five", SQLData.integer 6, SQLData.text "seven"]

-- `Cons`, composing two smaller `ToRow`s to reach beyond arity 7
#guard toRow (Types.Cons.mk (Types.Only.mk (1 : Int)) (Types.Only.mk "two")) ==
  #[SQLData.integer 1, SQLData.text "two"]

#guard toRow (Types.Cons.mk ((1 : Int), "two") ((3.0 : Float), (4 : Int))) ==
  #[SQLData.integer 1, SQLData.text "two", SQLData.float 3.0, SQLData.integer 4]

-- ────────────────────────────────────────────────────────────────────
-- End-to-end: bind a `ToRow`-rendered row against a real `:memory:` database
-- ────────────────────────────────────────────────────────────────────

#eval show IO Unit from do
  let conn ← openConnection ":memory:"
  Database.SQLite3.exec conn.connectionHandle
    "CREATE TABLE person (id INTEGER PRIMARY KEY, name TEXT, score REAL)"

  let insertStmt ← openStatement conn "INSERT INTO person (name, score) VALUES (?, ?)"
  Database.SQLite3.bind insertStmt.statementHandle (toRow ("Ada", (3.5 : Float)))
  match ← Database.SQLite3.step insertStmt.statementHandle with
  | .done => pure ()
  | .row => throw (IO.userError "insert unexpectedly produced a row")
  closeStatement insertStmt

  let selectStmt ← openStatement conn "SELECT name, score FROM person"
  match ← Database.SQLite3.step selectStmt.statementHandle with
  | .row => pure ()
  | .done => throw (IO.userError "expected a first row")

  let fields ← currentRowFields selectStmt
  let some nameField := fields[0]? | throw (IO.userError "missing name field")
  let some scoreField := fields[1]? | throw (IO.userError "missing score field")
  if nameField.result != .text "Ada" then throw (IO.userError "unexpected name value")
  if scoreField.result != .float 3.5 then throw (IO.userError "unexpected score value")

  closeStatement selectStmt
  closeConnection conn

end Tests.Database.SQLite.Simple.ToRow
