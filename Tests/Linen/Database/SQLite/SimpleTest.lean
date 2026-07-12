/-
  Tests for `Linen.Database.SQLite.Simple`.

  Exercises the public facade end-to-end against a real `:memory:` database:
  `withConnection`, `execute`/`execute_`, `query`/`query_`, `fold`/`fold_`,
  `withTransaction` (commit and rollback), `withSavepoint`,
  `lastInsertRowId`, and `changes`.
-/
import Linen.Database.SQLite.Simple

open Database.SQLite.Simple
open Database.SQLite.Simple.Types (Only Query)

namespace Tests.Database.SQLite.Simple

#eval show IO Unit from do
  withConnection ":memory:" fun conn => do
    execute_ conn "CREATE TABLE person (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"

    -- `execute` with bound parameters, then `lastInsertRowId`/`changes`
    execute conn "INSERT INTO person (name, age) VALUES (?, ?)" ("Ada", (36 : Int))
    if (← lastInsertRowId conn) != 1 then
      throw (IO.userError "expected the first inserted row's id to be 1")
    if (← changes conn) != 1 then
      throw (IO.userError "expected exactly one row changed by the insert")

    execute conn "INSERT INTO person (name, age) VALUES (?, ?)" ("Grace", (85 : Int))

    -- `query` decodes rows via `FromRow`
    let rows : Array (String × Int) ←
      query conn "SELECT name, age FROM person WHERE age > ? ORDER BY name" (Only.mk (0 : Int))
    if rows != #[("Ada", 36), ("Grace", 85)] then
      throw (IO.userError s!"unexpected query result: {rows}")

    -- `query_` with no parameters
    let names : Array (Only String) ← query_ conn "SELECT name FROM person ORDER BY name"
    if names.map Only.fromOnly != #["Ada", "Grace"] then
      throw (IO.userError s!"unexpected query_ result: {names.map Only.fromOnly}")

    -- `fold` streams rows one at a time
    let total ← fold conn "SELECT age FROM person WHERE age > ?" (Only.mk (0 : Int)) (0 : Int)
      fun acc (row : Only Int) => pure (acc + row.fromOnly)
    if total != 121 then throw (IO.userError s!"unexpected fold total: {total}")

    -- `fold_` with no parameters
    let count ← fold_ conn "SELECT name FROM person" (0 : Int)
      fun acc (_row : Only String) => pure (acc + 1)
    if count != 2 then throw (IO.userError s!"unexpected fold_ count: {count}")

    -- `withTransaction`: a committed transaction's effects persist
    withTransaction conn do
      execute_ conn "INSERT INTO person (name, age) VALUES ('Alan', 41)"
    let countAfterCommit : Array (Only Int) ← query_ conn "SELECT COUNT(*) FROM person"
    if countAfterCommit[0]!.fromOnly != 3 then
      throw (IO.userError "expected the committed row to persist")

    -- `withTransaction`: an exception rolls the transaction back
    let threw ← try
      withTransaction conn do
        execute_ conn "INSERT INTO person (name, age) VALUES ('Rolled', 0)"
        throw (IO.userError "deliberate failure")
      pure false
    catch _ =>
      pure true
    if !threw then throw (IO.userError "expected withTransaction to re-throw")
    let countAfterRollback : Array (Only Int) ← query_ conn "SELECT COUNT(*) FROM person"
    if countAfterRollback[0]!.fromOnly != 3 then
      throw (IO.userError "expected the rolled-back row to be discarded")

    -- `withSavepoint`: same commit/rollback behaviour, nested inside a
    -- transaction
    withTransaction conn do
      withSavepoint conn do
        execute_ conn "INSERT INTO person (name, age) VALUES ('Saved', 1)"
      let savedThrew ← try
        withSavepoint conn do
          execute_ conn "INSERT INTO person (name, age) VALUES ('NotSaved', 2)"
          throw (IO.userError "deliberate failure")
        pure false
      catch _ =>
        pure true
      if !savedThrew then throw (IO.userError "expected withSavepoint to re-throw")
    let finalNames : Array (Only String) ← query_ conn "SELECT name FROM person ORDER BY name"
    if finalNames.map Only.fromOnly != #["Ada", "Alan", "Grace", "Saved"] then
      throw (IO.userError s!"unexpected final names: {finalNames.map Only.fromOnly}")

end Tests.Database.SQLite.Simple
