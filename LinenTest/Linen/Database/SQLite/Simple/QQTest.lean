/-
  Tests for `Linen.Database.SQLite.Simple.QQ`.

  Exercises the `sql "…"` syntax, confirming it elaborates directly to
  `Query.ofString "…"` with no SQL-specific validation performed (see
  `Linen/Database/SQLite/Simple/QQ.lean`'s module doc for how little
  validation upstream's own quasiquoter performs either).
-/
import Linen.Database.SQLite.Simple.QQ
import Linen.Database.SQLite.Simple.Internal

open Database.SQLite.Simple
open Database.SQLite.Simple.QQ
open Database.SQLite.Simple.Types (Query)

namespace Tests.Database.SQLite.Simple.QQ

#guard (sql "SELECT * FROM t") == Query.ofString "SELECT * FROM t"
#guard (sql "SELECT * FROM t" : Query).fromQuery == "SELECT * FROM t"

-- multi-line, exactly the way upstream's bracket-delimited `[sql| … |]`
-- could span multiple lines
#guard (sql "SELECT a, b
FROM t
WHERE a = 1") == Query.ofString "SELECT a, b\nFROM t\nWHERE a = 1"

-- no SQL-specific validation is performed: nonsense text still elaborates
-- to a `Query`, exactly as upstream's `sqlExp` performs none either
#guard (sql "not even remotely valid SQL") ==
  Query.ofString "not even remotely valid SQL"

-- ────────────────────────────────────────────────────────────────────
-- End-to-end: a `sql`-built `Query` prepares and runs against a real
-- `:memory:` database exactly like any other `Query`
-- ────────────────────────────────────────────────────────────────────

#eval show IO Unit from do
  let conn ← openConnection ":memory:"
  Database.SQLite3.exec conn.connectionHandle (sql "CREATE TABLE t (n INTEGER)").fromQuery

  let insertStmt ← openStatement conn (sql "INSERT INTO t (n) VALUES (42)").fromQuery
  match ← Database.SQLite3.step insertStmt.statementHandle with
  | .done => pure ()
  | .row => throw (IO.userError "insert unexpectedly produced a row")
  closeStatement insertStmt

  let selectStmt ← openStatement conn (sql "SELECT n FROM t").fromQuery
  match ← Database.SQLite3.step selectStmt.statementHandle with
  | .row => pure ()
  | .done => throw (IO.userError "expected a row")
  let fields ← currentRowFields selectStmt
  let some nField := fields[0]? | throw (IO.userError "missing n field")
  if nField.result != .integer 42 then throw (IO.userError "unexpected n value")
  closeStatement selectStmt

  closeConnection conn

end Tests.Database.SQLite.Simple.QQ
