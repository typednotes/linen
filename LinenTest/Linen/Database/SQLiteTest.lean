/-
  Tests for `Linen.Database.SQLite3`, the public UTF-8 low-level SQLite3 API.
-/
import Linen.Database.SQLite

open Database.SQLite3

namespace Tests.Database.SQLite3

-- Full create/insert/query exercise against a private in-memory database,
--    using the untyped `SQLData`/`bind`/`columns` surface.
#eval show IO Unit from do
  let db ← open_ ":memory:"
  exec db "CREATE TABLE person (id INTEGER PRIMARY KEY, name TEXT, score REAL, photo BLOB)"

  let insertStmt ← prepare db "INSERT INTO person (name, score, photo) VALUES (?, ?, ?)"
  bind insertStmt #[.text "Ada", .float 3.14, .blob (ByteArray.mk #[1, 2, 3])]
  match ← step insertStmt with
  | .done => pure ()
  | .row  => throw (IO.userError "insert unexpectedly produced a row")

  -- Reset (and clear bindings) to re-execute the same prepared statement for
  -- the second row, rather than re-preparing it.
  reset insertStmt
  clearBindings insertStmt
  bind insertStmt #[.text "Grace", .null, .blob ByteArray.empty]
  match ← step insertStmt with
  | .done => pure ()
  | .row  => throw (IO.userError "insert unexpectedly produced a row")
  finalize insertStmt

  let rowId ← lastInsertRowId db
  if rowId != 2 then
    throw (IO.userError s!"expected lastInsertRowId = 2, got {rowId}")
  let ch ← changes db
  if ch != 1 then
    throw (IO.userError s!"expected changes = 1 (last statement only), got {ch}")

  let selectStmt ← prepare db "SELECT name, score, photo FROM person ORDER BY id"
  let n ← columnCount selectStmt
  if n != 3 then
    throw (IO.userError s!"expected 3 result columns, got {n}")
  let name0 ← columnName selectStmt 0
  if name0 != some "name" then
    throw (IO.userError s!"expected column 0 named \"name\", got {repr name0}")

  match ← step selectStmt with
  | .row => pure ()
  | .done => throw (IO.userError "expected a first row")
  let row0 ← columns selectStmt
  if row0 != #[.text "Ada", .float 3.14, .blob (ByteArray.mk #[1, 2, 3])] then
    throw (IO.userError "unexpected first row")

  match ← step selectStmt with
  | .row => pure ()
  | .done => throw (IO.userError "expected a second row")
  let row1 ← columns selectStmt
  if row1 != #[.text "Grace", .null, .blob ByteArray.empty] then
    throw (IO.userError "unexpected second row")

  match ← step selectStmt with
  | .done => pure ()
  | .row => throw (IO.userError "expected no third row")
  finalize selectStmt

  close db

-- `bindNamed` binds parameters by name rather than position.
#eval show IO Unit from do
  let db ← open_ ":memory:"
  exec db "CREATE TABLE t (a INTEGER, b TEXT)"
  let stmt ← prepare db "INSERT INTO t (a, b) VALUES (:a, :b)"
  bindNamed stmt #[(":b", .text "named"), (":a", .integer 9)]
  match ← step stmt with
  | .done => pure ()
  | .row => throw (IO.userError "insert unexpectedly produced a row")
  finalize stmt

  let stmt2 ← prepare db "SELECT a, b FROM t"
  match ← step stmt2 with
  | .row => pure ()
  | .done => throw (IO.userError "expected a row")
  let row ← columns stmt2
  if row != #[.integer 9, .text "named"] then
    throw (IO.userError "unexpected row")
  finalize stmt2
  close db

-- `exec` on invalid SQL raises a `SQLError` (as an `IO.userError`), and
--    `close` on an already-usable connection succeeds without one.
#eval show IO Unit from do
  let db ← open_ ":memory:"
  let threw ←
    try
      exec db "NOT VALID SQL"
      pure false
    catch _ =>
      pure true
  if !threw then
    throw (IO.userError "expected exec on invalid SQL to raise an error")
  close db

end Tests.Database.SQLite3
