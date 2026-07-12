/-
  Tests for `Linen.Database.DuckDB.Simple`.

  Unlike most of this batch's tests (which build `Field`/`FieldValue`s by
  hand, `Materialize` having no live chunk to decode until this very
  module), this is a genuine end-to-end round trip against a real, running
  in-memory DuckDB database: open a connection, create a table, insert rows
  via `execute`, read them back via `query`/`query_`/`fold_` decoding
  through a real `FromRow` instance, exercise `withTransaction`, and close.
-/
import Linen.Database.DuckDB.Simple

open Database.DuckDB.Simple

namespace Tests.Database.DuckDB.Simple

#eval show IO Unit from do
  withConnection none fun conn => do
    -- DDL.
    execute_ conn "CREATE TABLE people(id INTEGER, name VARCHAR)"

    -- Parameterized inserts.
    execute conn "INSERT INTO people VALUES (?, ?)" ((1 : Int32), "Alice")
    execute conn "INSERT INTO people VALUES (?, ?)" ((2 : Int32), "Bob")

    -- `query_`: no parameters, decoded via a real `FromRow (Int32 × String)`
    -- instance.
    let rows : Array (Int32 × String) ← query_ conn "SELECT id, name FROM people ORDER BY id"
    unless rows == #[((1 : Int32), "Alice"), ((2 : Int32), "Bob")] do
      throw (IO.userError s!"query_ returned unexpected rows: {repr rows}")

    -- `query`: parameterized, via `Only`.
    let filtered : Array (Only String) ←
      query conn "SELECT name FROM people WHERE id = ? ORDER BY id" (({ fromOnly := (2 : Int32) } : Only Int32))
    unless filtered == #[({ fromOnly := "Bob" } : Only String)] do
      throw (IO.userError s!"query returned unexpected rows: {repr filtered}")

    -- `execute` an `UPDATE`, confirming the change is visible to a
    -- subsequent `query_`.
    execute conn "UPDATE people SET name = ? WHERE id = ?" ("Alicia", (1 : Int32))
    let updated : Array (Int32 × String) ← query_ conn "SELECT id, name FROM people ORDER BY id"
    unless updated == #[((1 : Int32), "Alicia"), ((2 : Int32), "Bob")] do
      throw (IO.userError s!"post-UPDATE query_ returned unexpected rows: {repr updated}")

    -- `fold_`: streaming accumulation, no parameters.
    let idSum ← fold_ conn "SELECT id, name FROM people ORDER BY id" (0 : Int32)
      (fun (acc : Int32) (row : Int32 × String) => pure (acc + row.1))
    unless idSum == 3 do
      throw (IO.userError s!"fold_ accumulated unexpected id sum: {idSum}")

    -- `fold`: streaming accumulation, parameterized.
    let matchCount ← fold conn "SELECT id FROM people WHERE id >= ? ORDER BY id" (({ fromOnly := (2 : Int32) } : Only Int32))
      (0 : Int32) (fun (acc : Int32) (_row : Only Int32) => pure (acc + 1))
    unless matchCount == 1 do
      throw (IO.userError s!"fold accumulated unexpected match count: {matchCount}")

    -- `withTransaction`: a committed transaction's effects persist.
    withTransaction conn do
      execute_ conn "INSERT INTO people VALUES (3, 'Carol')"
    let afterCommit : Array (Only Int64) ← query_ conn "SELECT COUNT(*) FROM people"
    unless afterCommit == #[({ fromOnly := (3 : Int64) } : Only Int64)] do
      throw (IO.userError s!"expected 3 rows after committed transaction, got {repr afterCommit}")

    -- `withTransaction`: a transaction that throws rolls back.
    let rolledBack ←
      try
        withTransaction conn do
          execute_ conn "INSERT INTO people VALUES (4, 'Dave')"
          throw (IO.userError "deliberate failure to trigger rollback")
        pure false
      catch _ => pure true
    unless rolledBack do throw (IO.userError "expected withTransaction's action to re-throw")
    let afterRollback : Array (Only Int64) ← query_ conn "SELECT COUNT(*) FROM people"
    unless afterRollback == #[({ fromOnly := (3 : Int64) } : Only Int64)] do
      throw (IO.userError
        s!"expected rollback to leave row count at 3, got {repr afterRollback}")

end Tests.Database.DuckDB.Simple
