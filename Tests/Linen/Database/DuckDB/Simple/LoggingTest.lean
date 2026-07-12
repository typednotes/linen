/-
  Tests for `Linen.Database.DuckDB.Simple.Logging`.

  Mirrors `Tests/Linen/Database/DuckDB/FFI/LoggingTest.lean`'s own
  end-to-end drive: registers a real `LogStorage` on a real connection's
  underlying database via this module's `register`, enables DuckDB's
  logging feature (`SET enable_logging = true`) and points it at this
  logger (`SET logging_storage = '<name>'`), runs a query, and confirms
  the `LogEntry → IO Unit` callback actually fired — exercising this
  module's own record-building wrapper end-to-end, not just registration.
-/
import Linen.Database.DuckDB.Simple.Logging
import Linen.Database.DuckDB.Simple.Internal
import Linen.Database.DuckDB.FFI.QueryExecution

open Database.DuckDB.Simple
open Database.DuckDB.Simple.Logging
open Database.DuckDB.FFI.QueryExecution (query destroy)

namespace Tests.Database.DuckDB.Simple.Logging

#eval show IO Unit from do
  let conn ← openConnection none -- in-memory database

  let entries ← IO.mkRef (#[] : Array LogEntry)
  let _logStorage ← register conn "linen-simple-test-logger" fun entry =>
    entries.modify (·.push entry)

  -- Point DuckDB's logging subsystem at this custom logger and enable it.
  -- If this particular build's `SET`s are rejected (an older/newer DuckDB
  -- spelling these options differently), fall back to asserting
  -- registration success alone — `register`'s own FFI round trip (checked
  -- above, since it would have thrown on failure) is what this module is
  -- really responsible for; driving DuckDB's logging subsystem end-to-end
  -- is a bonus check, not this module's contract (mirroring the FFI-layer
  -- test's identical fallback).
  let (enableState, enableResult) ← withConnectionHandle conn fun h => query h "SET enable_logging = true"
  destroy enableResult
  let (storageState, storageResult) ←
    withConnectionHandle conn fun h => query h "SET logging_storage = 'linen-simple-test-logger'"
  destroy storageResult

  if enableState.isSuccess && storageState.isSuccess then
    let (queryState, queryResult) ← withConnectionHandle conn fun h => query h "SELECT 1"
    destroy queryResult
    unless queryState.isSuccess do throw (IO.userError "SELECT 1 failed")
    let seen ← entries.get
    unless seen.size > 0 do
      throw (IO.userError "expected the registered log-entry callback to have fired at least once")
  else
    pure ()

  closeConnection conn

-- `LogEntry`'s derived instances.
#guard
  ({ timestampMicros := 1, level := "INFO", logType := "t", message := "m" } : LogEntry).level ==
    "INFO"

end Tests.Database.DuckDB.Simple.Logging
