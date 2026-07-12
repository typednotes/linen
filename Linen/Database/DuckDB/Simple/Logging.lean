/-
  Linen.Database.DuckDB.Simple.Logging — log-callback registration

  Module #13 of `docs/imports/duckdb-simple/dependencies.md`, on #1
  (`Linen.Database.DuckDB.Simple.Internal`, for `Connection`/`SQLError`/
  `withDatabaseHandle`/`registrationError`) and
  `Linen.Database.DuckDB.FFI.Logging`.

  ## Design

  `Linen.Database.DuckDB.FFI.Logging` already solves the one genuinely hard
  part of this module — the Lean-closure-called-from-C trampoline pair
  (`linen_duckdb_log_write_trampoline`/`linen_duckdb_log_delete_trampoline`)
  that lets a Lean closure be installed as `duckdb_log_storage`'s
  `extra_data` write callback (see that module's own doc comment for the
  full ownership/lifetime story). This module needs **no new C code**: it
  is a thin `Simple`-package ergonomic wrapper — `register` combines
  `Logging.register`'s own `create`/`setWriteLogEntry`/`setExtraData`/
  `setName`/`duckdb_register_log_storage` sequence with this port's
  `Connection`-taking, `SQLError`-throwing conventions (fetching the
  underlying `Database` handle via `Internal.withDatabaseHandle`, and
  reporting a non-`success` registration `State` via `registrationError`,
  matching `Catalog`/`Config`/`FileSystem`'s identical shape for this
  batch).

  Per the fetched upstream source, `Database.DuckDB.Simple.Logging` bundles
  a log event's fields into one `LogEntry` record (`logEntryTimestamp`,
  `logEntryLevel`, `logEntryType`, `logEntryMessage`) before handing it to
  the user's callback, rather than exposing the four raw callback
  parameters separately — this module does the same, wrapping
  `Database.DuckDB.FFI.Logging.register`'s raw
  `Int64 → String → String → String → IO Unit` callback shape into one
  `LogEntry → IO Unit` callback. Upstream additionally decodes the raw
  timestamp into a `UTCTime`; this port keeps it as the same raw
  microsecond `Int64`
  `Database.DuckDB.FFI.Logging`'s own module doc already documents keeping
  (no `Timestamp`-object-construction path exists at that trampoline
  boundary — see that module's doc comment for why), rather than
  reconstructing a decoded time value only to immediately discard the
  extra precision/structure a caller may not need.

  ## Haskell source
  - `Database.DuckDB.Simple.Logging` (`duckdb-simple` package, version
    0.1.5.1)
-/
import Linen.Database.DuckDB.Simple.Internal
import Linen.Database.DuckDB.FFI.Logging

namespace Database.DuckDB.Simple.Logging

open Database.DuckDB.FFI.Types (LogStorage)
open Database.DuckDB.Simple (Connection SQLError throwSQLError registrationError withDatabaseHandle)

/-- A single log event delivered through DuckDB's log-storage callback (see
    the module doc for why `timestampMicros` is a raw microsecond count
    rather than a decoded time value). -/
structure LogEntry where
  /-- Raw microseconds since the Unix epoch. -/
  timestampMicros : Int64
  /-- The log entry's severity level (e.g. `"INFO"`, `"WARNING"`). -/
  level : String
  /-- The log entry's type/category, as assigned by whatever DuckDB
      subsystem emitted it. -/
  logType : String
  /-- The log entry's message text. -/
  message : String
deriving Repr, Inhabited

/-- Register a named log-storage backend on `conn`'s underlying database,
    invoking `onEntry` for every log event once installed (upstream's
    `registerLogStorage`). Thin wrapper over
    `Database.DuckDB.FFI.Logging.register` — see the module doc: no new
    C-level trampoline is needed here. The returned `LogStorage` handle
    need not be explicitly destroyed once registration succeeds (see
    `Database.DuckDB.FFI.Logging`'s own doc comment on why). -/
def register (conn : Connection) (name : String) (onEntry : LogEntry → IO Unit) :
    IO LogStorage :=
  withDatabaseHandle conn fun db => do
    let (state, logStorage) ←
      Database.DuckDB.FFI.Logging.register db name fun timestampMicros level logType message =>
        onEntry { timestampMicros, level, logType, message }
    match state with
    | .success => pure logStorage
    | .error => throwSQLError (registrationError s!"register log storage \"{name}\"")

end Database.DuckDB.Simple.Logging
