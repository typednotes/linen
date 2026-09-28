/-
  Tests for `Linen.Database.SQLite.Bindings`.

  Exercises the raw FFI entry points directly against a private in-memory
  SQLite database, checking raw result codes (`0` = `SQLITE_OK`,
  `100` = `SQLITE_ROW`, `101` = `SQLITE_DONE`).
-/
import Linen.Database.SQLite.Bindings

open Database.SQLite3.Bindings
open Database.SQLite3.Bindings.Types (Database Statement)

namespace Tests.Database.SQLite3.Bindings

-- Full round-trip: open an in-memory database, create a table, insert a
--    row via bound parameters, and read it back.
#eval show IO Unit from do
  let (rcOpen, db) ← openRaw ":memory:"
  if rcOpen != 0 then
    throw (IO.userError s!"open failed with code {rcOpen}")

  let (rcCreate, _msg) ← execRaw db "CREATE TABLE t (a INTEGER, b TEXT)"
  if rcCreate != 0 then
    throw (IO.userError s!"create table failed with code {rcCreate}")

  let (rcPrepare, stmtOpt) ← prepareRaw db "INSERT INTO t (a, b) VALUES (?, ?)"
  if rcPrepare != 0 then
    throw (IO.userError s!"prepare (insert) failed with code {rcPrepare}")
  let some stmt := stmtOpt
    | throw (IO.userError "prepare (insert) unexpectedly returned no statement")

  let n ← bindParameterCount stmt
  if n != 2 then
    throw (IO.userError s!"expected 2 bind parameters, got {n}")

  let rcBindA ← bindInt64 stmt 1 (42 : Int64)
  if rcBindA != 0 then
    throw (IO.userError s!"bind int64 failed with code {rcBindA}")
  let rcBindB ← bindText stmt 2 "hello"
  if rcBindB != 0 then
    throw (IO.userError s!"bind text failed with code {rcBindB}")

  let rcStep ← step stmt
  if rcStep != 101 then
    throw (IO.userError s!"insert step expected SQLITE_DONE (101), got {rcStep}")

  let rcFin ← finalizeRaw stmt
  if rcFin != 0 then
    throw (IO.userError s!"finalize (insert) failed with code {rcFin}")

  let (rcPrepare2, stmtOpt2) ← prepareRaw db "SELECT a, b FROM t"
  if rcPrepare2 != 0 then
    throw (IO.userError s!"prepare (select) failed with code {rcPrepare2}")
  let some stmt2 := stmtOpt2
    | throw (IO.userError "prepare (select) unexpectedly returned no statement")

  let colCount ← columnCount stmt2
  if colCount != 2 then
    throw (IO.userError s!"expected 2 result columns, got {colCount}")

  let colName0 ← columnName stmt2 0
  if colName0 != some "a" then
    throw (IO.userError s!"expected column 0 named \"a\", got {repr colName0}")

  let rcStep2 ← step stmt2
  if rcStep2 != 100 then
    throw (IO.userError s!"select step expected SQLITE_ROW (100), got {rcStep2}")

  let colTypeA ← columnType stmt2 0
  if colTypeA != 1 then  -- SQLITE_INTEGER
    throw (IO.userError s!"expected column 0 type INTEGER (1), got {colTypeA}")
  let a ← columnInt64 stmt2 0
  if a != 42 then
    throw (IO.userError s!"expected a = 42, got {a}")

  let b ← columnText stmt2 1
  if b != "hello" then
    throw (IO.userError s!"expected b = \"hello\", got {b}")

  let rcStep3 ← step stmt2
  if rcStep3 != 101 then
    throw (IO.userError s!"expected no further rows (SQLITE_DONE), got {rcStep3}")

  let rcFin2 ← finalizeRaw stmt2
  if rcFin2 != 0 then
    throw (IO.userError s!"finalize (select) failed with code {rcFin2}")

  let rowid ← lastInsertRowId db
  if rowid != 1 then
    throw (IO.userError s!"expected lastInsertRowId = 1, got {rowid}")

  let ch ← changes db
  if ch != 1 then
    throw (IO.userError s!"expected changes = 1, got {ch}")

  let auto ← getAutocommit db
  if auto != true then
    throw (IO.userError "expected getAutocommit = true outside a transaction")

  let rcClose ← closeRaw db
  if rcClose != 0 then
    throw (IO.userError s!"close failed with code {rcClose}")

-- A malformed statement fails to prepare with a non-zero (`SQLITE_ERROR`)
--    code, and `errmsg` reports a description.
#eval show IO Unit from do
  let (_, db) ← openRaw ":memory:"
  let (rc, stmtOpt) ← prepareRaw db "SELEKT * FROM nope"
  if rc == 0 then
    throw (IO.userError "expected a non-zero result code for malformed SQL")
  if stmtOpt.isSome then
    throw (IO.userError "expected no statement for malformed SQL")
  let msg ← errmsg db
  if msg.isEmpty then
    throw (IO.userError "expected a non-empty errmsg after a failed prepare")
  discard <| closeRaw db

end Tests.Database.SQLite3.Bindings
