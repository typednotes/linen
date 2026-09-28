/-
  Tests for `Linen.Database.DuckDB.Simple.Copy`.

  Creates a real table via `Database.DuckDB.FFI.QueryExecution.query`,
  bulk-appends real rows into it via `copyRows`, then confirms the rows
  actually landed by registering a `Database.DuckDB.Simple.Function`-based
  accumulator scalar function and running `SELECT acc(a) FROM t` through
  the same raw-query path — since no `duckdb_fetch_chunk`-equivalent is in
  scope for this port (see `Materialize`'s own module doc), driving the
  appended rows back through a real scalar function is the genuine
  end-to-end confirmation available here, exactly mirroring `Function`'s
  own test design.
-/
import Linen.Database.DuckDB.Simple.Copy
import Linen.Database.DuckDB.Simple.Function
import Linen.Database.DuckDB.Simple.Internal
import Linen.Database.DuckDB.FFI.QueryExecution
import Linen.Database.DuckDB.FFI.LogicalTypes

open Database.DuckDB.Simple
open Database.DuckDB.Simple.Copy
open Database.DuckDB.Simple.Function
open Database.DuckDB.FFI.QueryExecution (query destroy)
open Database.DuckDB.FFI.Types (Type_)
open Database.DuckDB.FFI.LogicalTypes (create)

namespace Tests.Database.DuckDB.Simple.Copy

#eval show IO Unit from do
  let conn ← openConnection none -- in-memory database

  let (createState, createResult) ←
    withConnectionHandle conn fun h => query h "CREATE TABLE t(a BIGINT, b VARCHAR)"
  destroy createResult
  unless createState.isSuccess do throw (IO.userError "CREATE TABLE failed")

  -- Bulk-append real rows via `copyRows`.
  let rows : Array (Array Database.DuckDB.Simple.FieldValue) :=
    #[#[.int64 1, .varchar "one"], #[.int64 2, .varchar "two"], #[.int64 3, .varchar "three"]]
  copyRows conn none "t" rows

  -- Confirm the appended rows landed by registering a real accumulator
  -- scalar function and running it against the table.
  let sum ← IO.mkRef (0 : Int64)
  let count ← IO.mkRef (0 : Nat)
  let bigInt ← create Type_.bigInt
  createFunction conn "linen_copy_test_acc" #[bigInt] bigInt fun args => do
    match args[0]! with
    | .int64 i =>
      sum.modify (· + i)
      count.modify (· + 1)
      pure (.int64 i)
    | _ => pure (.int64 0)

  let (queryState, queryResult) ←
    withConnectionHandle conn fun h => query h "SELECT linen_copy_test_acc(a) FROM t"
  destroy queryResult
  unless queryState.isSuccess do throw (IO.userError "SELECT with accumulator function failed")

  let finalSum ← sum.get
  let finalCount ← count.get
  unless finalCount == 3 do
    throw (IO.userError s!"expected the accumulator to have been called 3 times, saw {finalCount}")
  unless finalSum == 6 do
    throw (IO.userError s!"expected the appended rows to sum to 6, saw {finalSum}")

  closeConnection conn

end Tests.Database.DuckDB.Simple.Copy
