/-
  Tests for `Linen.Database.SQLite.Simple.Internal`.

  `Connection`/`Statement`/`Field` are exercised against a real `:memory:`
  database via `Linen.Database.SQLite`'s public low-level API.
-/
import Linen.Database.SQLite.Simple.Internal

open Database.SQLite.Simple

namespace Tests.Database.SQLite.Simple.Internal

#eval show IO Unit from do
  let conn ← openConnection ":memory:"
  Database.SQLite3.exec conn.connectionHandle
    "CREATE TABLE person (id INTEGER PRIMARY KEY, name TEXT, score REAL)"

  let insertStmt ← openStatement conn "INSERT INTO person (name, score) VALUES (?, ?)"
  Database.SQLite3.bind insertStmt.statementHandle #[.text "Ada", .float 3.14]
  match ← Database.SQLite3.step insertStmt.statementHandle with
  | .done => pure ()
  | .row => throw (IO.userError "insert unexpectedly produced a row")
  closeStatement insertStmt

  let insertStmt2 ← openStatement conn "INSERT INTO person (name, score) VALUES (?, ?)"
  Database.SQLite3.bind insertStmt2.statementHandle #[.text "Grace", .null]
  match ← Database.SQLite3.step insertStmt2.statementHandle with
  | .done => pure ()
  | .row => throw (IO.userError "insert unexpectedly produced a row")
  closeStatement insertStmt2

  let selectStmt ← openStatement conn "SELECT name, score FROM person ORDER BY id"
  match ← Database.SQLite3.step selectStmt.statementHandle with
  | .row => pure ()
  | .done => throw (IO.userError "expected a first row")

  let fields ← currentRowFields selectStmt
  if fields.size != 2 then
    throw (IO.userError s!"expected 2 fields, got {fields.size}")

  let some nameField := fields[0]? | throw (IO.userError "missing name field")
  if nameField.column != 0 then
    throw (IO.userError s!"expected column index 0, got {nameField.column}")
  if nameField.columnName != some "name" then
    throw (IO.userError s!"expected column name \"name\", got {repr nameField.columnName}")
  if nameField.result != .text "Ada" then
    throw (IO.userError "unexpected name field value")
  if nameField.typeName != "TEXT" then
    throw (IO.userError s!"expected TEXT, got {nameField.typeName}")

  let some scoreField := fields[1]? | throw (IO.userError "missing score field")
  if scoreField.result != .float 3.14 then
    throw (IO.userError "unexpected score field value")
  if scoreField.typeName != "FLOAT" then
    throw (IO.userError s!"expected FLOAT, got {scoreField.typeName}")

  closeStatement selectStmt
  closeConnection conn

end Tests.Database.SQLite.Simple.Internal
