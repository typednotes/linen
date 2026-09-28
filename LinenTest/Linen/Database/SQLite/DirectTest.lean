/-
  Tests for `Linen.Database.SQLite.Direct`.

  Same create/insert/query exercise as `Tests.Database.SQLite3.Bindings`,
  but through the `Except Error`-returning `Direct` layer.
-/
import Linen.Database.SQLite.Direct

open Database.SQLite3.Direct
open Database.SQLite3.Bindings.Types (StepResult)

namespace Tests.Database.SQLite3.Direct

private def expectOk : Result Unit → IO Unit
  | .ok () => pure ()
  | .error err => throw (IO.userError s!"expected .ok, got .error {repr err}")

#eval show IO Unit from do
  let (openResult, db) ← open_ ":memory:"
  expectOk (openResult.map fun _ => ())

  let (createResult, _) ← execMsg db "CREATE TABLE t (a INTEGER, b TEXT)"
  expectOk createResult

  let some stmt ← (do
      let r ← prepare db "INSERT INTO t (a, b) VALUES (?, ?)"
      match r with
      | .ok stmtOpt => pure stmtOpt
      | .error err => throw (IO.userError s!"prepare (insert) failed: {repr err}"))
    | throw (IO.userError "prepare (insert) unexpectedly returned no statement")

  let n ← bindParameterCount stmt
  if n != 2 then
    throw (IO.userError s!"expected 2 bind parameters, got {n}")

  expectOk (← bindInt64 stmt 1 (7 : Int64))
  expectOk (← bindText stmt 2 "world")

  match ← step stmt with
  | .ok .done => pure ()
  | .ok .row => throw (IO.userError "insert step unexpectedly produced a row")
  | .error err => throw (IO.userError s!"insert step failed: {repr err}")

  expectOk (← finalize stmt)

  let some stmt2 ← (do
      let r ← prepare db "SELECT a, b FROM t WHERE a = ?"
      match r with
      | .ok stmtOpt => pure stmtOpt
      | .error err => throw (IO.userError s!"prepare (select) failed: {repr err}"))
    | throw (IO.userError "prepare (select) unexpectedly returned no statement")

  expectOk (← bindInt64 stmt2 1 (7 : Int64))

  match ← step stmt2 with
  | .ok .row => pure ()
  | .ok .done => throw (IO.userError "select step unexpectedly found no row")
  | .error err => throw (IO.userError s!"select step failed: {repr err}")

  let colType ← columnType stmt2 0
  if colType != .integer then
    throw (IO.userError s!"expected column 0 to be .integer, got {repr colType}")

  let a ← columnInt64 stmt2 0
  if a != 7 then
    throw (IO.userError s!"expected a = 7, got {a}")

  let b ← columnText stmt2 1
  if b != "world" then
    throw (IO.userError s!"expected b = \"world\", got {b}")

  expectOk (← finalize stmt2)

  let rowid ← lastInsertRowId db
  if rowid != 1 then
    throw (IO.userError s!"expected lastInsertRowId = 1, got {rowid}")

  expectOk (← close db)

-- A malformed statement reports `.error` with a decoded `Error`, not a raw
--    code, and the connection's `errmsg` is non-empty.
#eval show IO Unit from do
  let (_, db) ← open_ ":memory:"
  match ← prepare db "SELEKT * FROM nope" with
  | .ok _ => throw (IO.userError "expected .error for malformed SQL")
  | .error err =>
    if err.isOk then
      throw (IO.userError s!"expected a non-ok Error, got {repr err}")
    let msg ← errmsg db
    if msg.isEmpty then
      throw (IO.userError "expected a non-empty errmsg after a failed prepare")
  expectOk (← close db)

end Tests.Database.SQLite3.Direct
