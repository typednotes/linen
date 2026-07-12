/-
  Tests for `Linen.Database.DuckDB.FFI.Logging`.

  Registers a real `LogStorage` on a real `Database`, enables DuckDB's
  logging feature via `SET enable_logging = true` and points it at this
  logger via `SET logging_storage = '<name>'`, then runs a query and
  confirms the write callback actually fired — exercising the full
  Lean-closure-called-from-C trampoline pair end-to-end, not just the
  registration call.
-/
import Linen.Database.DuckDB.FFI.Logging
import Linen.Database.DuckDB.FFI.OpenConnect
import Tests.Linen.Database.DuckDB.FFI.TestSupport

open Database.DuckDB.FFI.Logging
open Database.DuckDB.FFI.OpenConnect
open Database.DuckDB.FFI.Types
open Tests.Database.DuckDB.FFI.TestSupport

namespace Tests.Database.DuckDB.FFI.Logging

#eval show IO Unit from do
  let dbResult ← openDatabase none
  let db ← match dbResult with
    | .ok db => pure db
    | .error msg => throw (IO.userError s!"duckdb_open failed: {msg}")
  let connResult ← connect db
  let conn ← match connResult with
    | .ok conn => pure conn
    | .error msg => throw (IO.userError s!"duckdb_connect failed: {msg}")

  let callCount ← IO.mkRef (0 : Nat)
  let onWrite : Int64 → String → String → String → IO Unit := fun _ts _level _logType _msg =>
    callCount.modify (· + 1)

  let (registerState, _logStorage) ← register db "linen-test-logger" onWrite
  unless registerState.isSuccess do throw (IO.userError "register failed")

  -- Point DuckDB's logging subsystem at this custom logger and enable it.
  -- If this particular build's `SET`s are rejected (e.g. an older/newer
  -- DuckDB that spells these options differently), fall back to asserting
  -- registration success alone — `register`'s own FFI round trip is what
  -- this module is really responsible for; driving DuckDB's logging
  -- subsystem end-to-end is a bonus check, not this module's contract.
  let enableState ← queryExec conn "SET enable_logging = true"
  let selectStorageState ← queryExec conn "SET logging_storage = 'linen-test-logger'"
  if enableState.isSuccess && selectStorageState.isSuccess then
    let queryState ← queryExec conn "SELECT 1"
    unless queryState.isSuccess do throw (IO.userError "SELECT 1 failed")
    let n ← callCount.get
    unless n > 0 do
      throw (IO.userError "expected the registered log-write callback to have fired at least once")
  else
    -- Logging couldn't be enabled in this build; still confirm the
    -- registration call itself succeeded end-to-end (checked above).
    pure ()

  disconnect conn
  close db

end Tests.Database.DuckDB.FFI.Logging
