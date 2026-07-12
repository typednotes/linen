/-
  Tests for `Linen.Database.SQLite.Simple.FromField`.

  Exercises `fromField` against real `Field`s decoded from a `:memory:`
  database (mirroring `Tests.Linen.Database.SQLite.Simple.Internal`'s
  pattern), covering both the success path (decoding a well-typed column)
  and the failure path (a `ResultError`-driven `Ok.errors` when the column's
  storage class doesn't match the target type).
-/
import Linen.Database.SQLite.Simple.FromField

open Database.SQLite.Simple
open Database.SQLite3 (SQLData)

namespace Tests.Database.SQLite.Simple.FromField

-- ────────────────────────────────────────────────────────────────────
-- Pure `ResultError`/`FromField` checks not needing a real database
-- ────────────────────────────────────────────────────────────────────

#guard toString (ResultError.incompatible "INTEGER" "String" "boom") ==
  "incompatible SQL type INTEGER and Lean type String: boom"
#guard toString (ResultError.unexpectedNull "TEXT" "String" "boom") ==
  "unexpected NULL in SQL type TEXT for non-nullable Lean type String: boom"
#guard toString (ResultError.conversionFailed "TEXT" "Int" "boom") ==
  "could not convert SQL type TEXT to Lean type Int: boom"

#guard (fromField (α := SQLData) { result := .integer 42, column := 0 }) ==
  Ok.ok (SQLData.integer 42)

#guard (fromField (α := Option Int) { result := .null, column := 0 }) == Ok.ok none
#guard (fromField (α := Option Int) { result := .integer 7, column := 0 }) == Ok.ok (some 7)

private def isOkNull : Ok Types.Null → Bool
  | .ok .null => true
  | _ => false

private def isErr : Ok α → Bool
  | .ok _ => false
  | .errors _ => true

#guard isOkNull (fromField (α := Types.Null) { result := .null, column := 0 })
#guard isErr (fromField (α := Types.Null) { result := .integer 1, column := 0 })

-- ────────────────────────────────────────────────────────────────────
-- Real round trip via a `:memory:` database
-- ────────────────────────────────────────────────────────────────────

#eval show IO Unit from do
  let conn ← openConnection ":memory:"
  Database.SQLite3.exec conn.connectionHandle
    "CREATE TABLE item (id INTEGER PRIMARY KEY, name TEXT, price REAL, tag TEXT)"

  let insertStmt ← openStatement conn "INSERT INTO item (name, price, tag) VALUES (?, ?, ?)"
  Database.SQLite3.bind insertStmt.statementHandle #[.text "widget", .float 9.5, .null]
  match ← Database.SQLite3.step insertStmt.statementHandle with
  | .done => pure ()
  | .row => throw (IO.userError "insert unexpectedly produced a row")
  closeStatement insertStmt

  let selectStmt ← openStatement conn "SELECT name, price, tag FROM item ORDER BY id"
  match ← Database.SQLite3.step selectStmt.statementHandle with
  | .row => pure ()
  | .done => throw (IO.userError "expected a first row")

  let fields ← currentRowFields selectStmt
  let some nameField := fields[0]? | throw (IO.userError "missing name field")
  let some priceField := fields[1]? | throw (IO.userError "missing price field")
  let some tagField := fields[2]? | throw (IO.userError "missing tag field")

  -- success path: decode each column at its natural type
  match fromField (α := String) nameField with
  | .ok "widget" => pure ()
  | _ => throw (IO.userError "expected name to decode to \"widget\"")

  match fromField (α := Float) priceField with
  | .ok p => if p != 9.5 then throw (IO.userError "expected price to decode to 9.5")
  | .errors _ => throw (IO.userError "expected price to decode successfully")

  match fromField (α := Option String) tagField with
  | .ok none => pure ()
  | _ => throw (IO.userError "expected tag to decode to none")

  -- failure path: a TEXT column decoded as an integer must fail with `Ok.errors`
  match fromField (α := Int) nameField with
  | .errors es =>
    if es.size == 0 then throw (IO.userError "expected a non-empty error list")
  | .ok _ => throw (IO.userError "expected decoding TEXT as Int to fail")

  -- failure path: a `NULL` column decoded as a non-`Option` `String` must fail
  match fromField (α := String) tagField with
  | .errors _ => pure ()
  | .ok _ => throw (IO.userError "expected decoding NULL as String to fail")

  closeStatement selectStmt
  closeConnection conn

end Tests.Database.SQLite.Simple.FromField
