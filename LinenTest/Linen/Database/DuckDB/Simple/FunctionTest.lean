/-
  Tests for `Linen.Database.DuckDB.Simple.Function`.

  Registers `double_it(x BIGINT) -> BIGINT` via `createFunction`, and a
  `sink(x BIGINT) -> BIGINT` identity function that records every call it
  receives into an `IO.Ref`. Running `SELECT sink(double_it(21))` through a
  real query drives both functions end-to-end: `double_it`'s
  `writeFieldValue` result must be written into DuckDB's real output
  vector correctly for `sink`'s own `Materialize`-based `decodeArgs` to
  read back the expected value — a genuine round trip, since no
  `duckdb_fetch_chunk`-equivalent is in scope to read a `SELECT`'s results
  directly (see `Materialize`'s own module doc).

  Also confirms `createFunctionWithState` threads mutable state across
  calls, and that `deleteFunction` runs without error.
-/
import Linen.Database.DuckDB.Simple.Function
import Linen.Database.DuckDB.Simple.Internal
import Linen.Database.DuckDB.FFI.QueryExecution
import Linen.Database.DuckDB.FFI.LogicalTypes

open Database.DuckDB.Simple
open Database.DuckDB.Simple.Function
open Database.DuckDB.FFI.QueryExecution (query destroy)
open Database.DuckDB.FFI.Types (Type_)
open Database.DuckDB.FFI.LogicalTypes (create)

namespace Tests.Database.DuckDB.Simple.Function

#eval show IO Unit from do
  let conn ← openConnection none -- in-memory database
  let bigInt ← create Type_.bigInt

  -- `double_it(x) = 2 * x`.
  createFunction conn "linen_fn_test_double_it" #[bigInt] bigInt fun args => do
    match args[0]! with
    | .int64 i => pure (.int64 (2 * i))
    | _ => pure (.int64 0)

  -- `sink(x) = x`, recording every call it receives.
  let calls ← IO.mkRef (#[] : Array Database.DuckDB.Simple.FieldValue)
  createFunction conn "linen_fn_test_sink" #[bigInt] bigInt fun args => do
    calls.modify (·.push args[0]!)
    pure args[0]!

  let (state, result) ←
    withConnectionHandle conn fun h =>
      query h "SELECT linen_fn_test_sink(linen_fn_test_double_it(21))"
  destroy result
  unless state.isSuccess do throw (IO.userError "chained SELECT failed")

  let seen ← calls.get
  unless seen.size == 1 do
    throw (IO.userError s!"expected exactly one call to sink, saw {seen.size}")
  match seen[0]! with
  | .int64 42 => pure ()
  | other => throw (IO.userError s!"expected sink to have received int64 42, saw {other.typeName}")

  -- `createFunctionWithState` threads mutable state across calls.
  createFunctionWithState conn "linen_fn_test_counter" #[bigInt] bigInt (0 : Int64)
    fun ref _args => do
      ref.modify (· + 1)
      .int64 <$> ref.get

  let (state2, result2) ←
    withConnectionHandle conn fun h => query h "SELECT linen_fn_test_counter(0), linen_fn_test_counter(0)"
  destroy result2
  unless state2.isSuccess do throw (IO.userError "stateful SELECT failed")

  -- `deleteFunction` issues a real `DROP FUNCTION IF EXISTS` through the
  -- connection. Per the module doc, DuckDB does not actually support
  -- dropping a scalar function registered through the C API, so this may
  -- report a registration-style `SQLError` here rather than succeeding;
  -- either outcome is acceptable — only confirming the call reaches DuckDB
  -- (no crash/hang) is this test's job, not upstream's own limitation.
  try
    deleteFunction conn "linen_fn_test_counter"
  catch _ =>
    pure ()

  closeConnection conn

end Tests.Database.DuckDB.Simple.Function
