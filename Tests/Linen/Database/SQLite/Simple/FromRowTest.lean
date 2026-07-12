/-
  Tests for `Linen.Database.SQLite.Simple.FromRow`.

  Exercises the `RowParser` applicative and the `FromRow` tuple/`Only`/
  `Cons` instances (arity up to 7, the cutoff this port implements — see
  `Linen/Database/SQLite/Simple/FromRow.lean`'s module doc) against a real
  row's `Field`s, decoded from a `:memory:` database.
-/
import Linen.Database.SQLite.Simple.FromRow

open Database.SQLite.Simple

namespace Tests.Database.SQLite.Simple.FromRow

-- ────────────────────────────────────────────────────────────────────
-- `RowParser` combinators, against hand-built `Field`s
-- ────────────────────────────────────────────────────────────────────

private def mkField (r : Database.SQLite3.SQLData) (col : Nat) : Field :=
  { result := r, column := col }

private def isErr : Ok α → Bool
  | .ok _ => false
  | .errors _ => true

#guard (runFromRow (α := Types.Only Int) #[mkField (.integer 5) 0]) == Ok.ok (Types.Only.mk 5)

#guard (RowParser.run (numFieldsRemaining) #[mkField (.integer 1) 0, mkField (.integer 2) 1] 1)
  == Ok.ok (1, 1)

#guard isErr (RowParser.run (returnRowError "boom" : RowParser Int) #[] 0)

-- running out of columns mid-parse is reported as a failure
#guard isErr (runFromRow (α := Int × Int) #[mkField (.integer 1) 0])

-- ────────────────────────────────────────────────────────────────────
-- Tuple/`Only`/`Cons` instances, against a real row from a `:memory:` DB
-- ────────────────────────────────────────────────────────────────────

#eval show IO Unit from do
  let conn ← openConnection ":memory:"
  Database.SQLite3.exec conn.connectionHandle
    "CREATE TABLE wide (c1 INTEGER, c2 TEXT, c3 REAL, c4 INTEGER, c5 TEXT, c6 INTEGER, c7 TEXT)"

  let insertStmt ← openStatement conn
    "INSERT INTO wide (c1, c2, c3, c4, c5, c6, c7) VALUES (?, ?, ?, ?, ?, ?, ?)"
  Database.SQLite3.bind insertStmt.statementHandle
    #[.integer 1, .text "two", .float 3.0, .integer 4, .text "five", .integer 6, .text "seven"]
  match ← Database.SQLite3.step insertStmt.statementHandle with
  | .done => pure ()
  | .row => throw (IO.userError "insert unexpectedly produced a row")
  closeStatement insertStmt

  let selectStmt ← openStatement conn "SELECT c1, c2, c3, c4, c5, c6, c7 FROM wide"
  match ← Database.SQLite3.step selectStmt.statementHandle with
  | .row => pure ()
  | .done => throw (IO.userError "expected a first row")

  let fields ← currentRowFields selectStmt

  -- arity 2
  match runFromRow (α := Int × String) fields with
  | .ok (1, "two") => pure ()
  | _ => throw (IO.userError "arity-2 decode mismatch")

  -- arity 3
  match runFromRow (α := Int × String × Float) fields with
  | .ok (1, "two", c3) => if c3 != 3.0 then throw (IO.userError "arity-3 decode mismatch")
  | _ => throw (IO.userError "arity-3 decode mismatch")

  -- arity 7 (the top hand-written tuple instance)
  match runFromRow (α := Int × String × Float × Int × String × Int × String) fields with
  | .ok (1, "two", c3, 4, "five", 6, "seven") =>
    if c3 != 3.0 then throw (IO.userError "arity-7 decode mismatch")
  | _ => throw (IO.userError "arity-7 decode mismatch")

  -- `Only`, consuming just the first column
  match runFromRow (α := Types.Only Int) fields with
  | .ok ⟨1⟩ => pure ()
  | _ => throw (IO.userError "Only decode mismatch")

  closeStatement selectStmt
  closeConnection conn

-- `Cons`/`(:.)` composing two tuple-shaped `FromRow`s, against a fresh short row
#eval show IO Unit from do
  let conn ← openConnection ":memory:"
  Database.SQLite3.exec conn.connectionHandle "CREATE TABLE pair (a INTEGER, b TEXT)"
  let insertStmt ← openStatement conn "INSERT INTO pair (a, b) VALUES (?, ?)"
  Database.SQLite3.bind insertStmt.statementHandle #[.integer 10, .text "ten"]
  match ← Database.SQLite3.step insertStmt.statementHandle with
  | .done => pure ()
  | .row => throw (IO.userError "insert unexpectedly produced a row")
  closeStatement insertStmt

  let selectStmt ← openStatement conn "SELECT a, b FROM pair"
  match ← Database.SQLite3.step selectStmt.statementHandle with
  | .row => pure ()
  | .done => throw (IO.userError "expected a first row")
  let fields ← currentRowFields selectStmt

  match runFromRow (α := Types.Cons (Types.Only Int) (Types.Only String)) fields with
  | .ok ⟨⟨10⟩, ⟨"ten"⟩⟩ => pure ()
  | _ => throw (IO.userError "Cons decode mismatch")

  closeStatement selectStmt
  closeConnection conn

end Tests.Database.SQLite.Simple.FromRow
