/-
  Tests for `Linen.Database.DuckDB.Simple.ToField`.

  Checks each instance's `columnType` tag (pure) and then, for a
  representative sample, actually binds the rendered `FieldBinding` against
  a real `PreparedStatement`'s `SELECT ?` parameter over a real `:memory:`
  connection — `FieldBinding.bind` throws on a genuine `duckdb_bind_*`
  failure (see `ToField.lean`'s `mkBinding`/`checkBindState`), so a bind
  call returning without exception is real evidence the underlying FFI call
  reported success, not just that this port's Lean code type-checks.
-/
import Linen.Database.DuckDB.Simple.ToField
import Linen.Database.DuckDB.Simple.Internal
import Linen.Database.DuckDB.FFI.PreparedStatements

open Database.DuckDB.Simple
open Database.DuckDB.FFI.Types (Type_)
open Database.DuckDB.FFI.PreparedStatements (prepare destroy)

namespace Tests.Database.DuckDB.Simple.ToField

-- `columnType` tags, pure.
#guard (toField true).columnType == Type_.boolean
#guard (toField (5 : Int8)).columnType == Type_.tinyInt
#guard (toField (5 : Int16)).columnType == Type_.smallInt
#guard (toField (5 : Int32)).columnType == Type_.integer
#guard (toField (5 : Int64)).columnType == Type_.bigInt
#guard (toField (-5 : Int)).columnType == Type_.bigInt
#guard (toField (5 : UInt8)).columnType == Type_.uTinyInt
#guard (toField (5 : UInt16)).columnType == Type_.uSmallInt
#guard (toField (5 : UInt32)).columnType == Type_.uInteger
#guard (toField (5 : UInt64)).columnType == Type_.uBigInt
#guard (toField (5 : Nat)).columnType == Type_.uBigInt
#guard (toField (3.5 : Float32)).columnType == Type_.float
#guard (toField (3.5 : Float)).columnType == Type_.double
#guard (toField "hello").columnType == Type_.varchar
#guard (toField (ByteArray.mk #[1, 2, 3])).columnType == Type_.blob
#guard (toField (none : Option Int)).columnType == Type_.sqlNull
#guard (toField (some (7 : Int))).columnType == Type_.bigInt

-- Binding each of a representative sample of instances against a real
-- prepared statement's sole parameter, over a real `:memory:` connection.
private def checkBind [ToField α] (conn : Connection) (value : α) : IO Unit :=
  withConnectionHandle conn fun connHandle => do
    let (state, stmt) ← prepare connHandle "SELECT ?"
    try
      unless state.isSuccess do throw (IO.userError "prepare failed")
      (toField value).bind stmt 1
    finally
      destroy stmt

#eval show IO Unit from do
  let conn ← openConnection none -- in-memory database

  checkBind conn true
  checkBind conn (42 : Int8)
  checkBind conn (42 : Int16)
  checkBind conn (42 : Int32)
  checkBind conn (42 : Int64)
  checkBind conn (-42 : Int)
  checkBind conn (42 : UInt8)
  checkBind conn (42 : UInt16)
  checkBind conn (42 : UInt32)
  checkBind conn (42 : UInt64)
  checkBind conn (42 : Nat)
  checkBind conn (3.5 : Float32)
  checkBind conn (3.5 : Float)
  checkBind conn "hello"
  checkBind conn (ByteArray.mk #[1, 2, 3])
  checkBind conn (none : Option Int)
  checkBind conn (some (7 : Int))

  closeConnection conn

end Tests.Database.DuckDB.Simple.ToField
