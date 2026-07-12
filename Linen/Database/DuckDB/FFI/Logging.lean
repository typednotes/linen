/-
  Linen.Database.DuckDB.FFI.Logging — custom log-storage registration

  Mirrors Haskell's `Database.DuckDB.FFI.Logging` (the `duckdb-ffi`
  package). Module #11 of `docs/imports/duckdb-ffi/dependencies.md`; depends
  only on `Database.DuckDB.FFI.Types` (module #1), which declares the
  `LogStorage` handle this module builds up and installs.

  This is the one module in this batch that needs a Lean-closure-called-
  from-C trampoline — the same shape of problem
  `Linen.Database.SQLite.Simple.Function`/`ffi/sqlite3_shim.c`'s
  `xFunc`/`xDestroy` pair already solves for `sqlite3_create_function_v2`.
  DuckDB's own logging API is actually a cleaner fit for this than SQLite's:
  `duckdb_log_storage_set_write_log_entry` installs one *fixed* native
  function pointer (which Lean code could never construct itself, since
  Lean has no way to produce a genuine C function pointer value — so this
  port's C shim always installs its own single trampoline,
  `linen_duckdb_log_write_trampoline`, unconditionally), while
  `duckdb_log_storage_set_extra_data` separately stores an opaque
  `void *extra_data` *and* a `duckdb_delete_callback_t` to release it. This
  port stores the registered Lean closure (retained via `lean_inc_ref`)
  directly as `extra_data`, and passes its own `linen_duckdb_log_delete_trampoline`
  (which `lean_dec_ref`s it) as the `delete_callback` — DuckDB then hands
  `extra_data` straight to the write trampoline on every call, no
  `sqlite3_user_data(ctx)`-style secondary lookup needed.

  The write callback's upstream C signature is
  `void (*)(void *extra_data, duckdb_timestamp *timestamp, const char
  *level, const char *log_type, const char *log_message)`. Rather than add
  a `Timestamp`-object-construction path to the trampoline (untested
  territory: hand-building a Lean structure value from raw C to pass into
  `lean_apply_N`, as opposed to this codebase's existing precedent of only
  ever *reading* such values back out at an `@[extern]` boundary), this
  port's Lean-visible callback type takes the timestamp as a plain `Int64`
  of raw microseconds — the same simplification `Database.DuckDB.FFI.Helpers`
  already applies to `isFiniteTimestampSeconds`/`Millis`/`Nanos`, and the
  exact bit pattern the C ABI passes across either way.

  `register` is this module's one ergonomic addition on top of upstream's
  six raw entry points: it sequences `create`/`setWriteLogEntry`/
  `setExtraData`/`setName`/`duckdb_register_log_storage` into a single call,
  the same "combine the raw multi-step lifecycle into one `def`" treatment
  `Linen.Database.SQLite.Simple.Function`'s fixed-arity `createFunction0`..
  `createFunction3` wrappers give `Database.SQLite3.createFunction`.
  **Caveat**: `duckdb.h` does not document whether `duckdb_register_log_storage`
  takes ownership of the `LogStorage` it is passed (i.e. whether the
  database's logger may later free it itself) — following the conservative
  reading, `register` does not explicitly destroy the `LogStorage` after
  registration, and its own GC finalizer is deliberately left as the only
  cleanup path for this specific case (never called explicitly by this
  port once a `LogStorage` has been successfully registered). -/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.Logging

open Database.DuckDB.FFI.Types

/-- `duckdb_create_log_storage`: a new, blank log storage object. The result
    must eventually be destroyed with `destroy` (or let its GC finalizer do
    so) *unless* it is passed to `registerLogStorage` — see this module's
    doc comment. -/
@[extern "linen_duckdb_create_log_storage"]
opaque create : IO LogStorage

/-- `duckdb_destroy_log_storage`: release `logStorage`'s underlying
    resources early. Idempotent. Do not call this after a successful
    `registerLogStorage` (see this module's doc comment). -/
@[extern "linen_duckdb_destroy_log_storage"]
opaque destroy : LogStorage → IO Unit

/-- `duckdb_log_storage_set_write_log_entry`: install this port's fixed
    native write trampoline on `logStorage` (see this module's doc
    comment — there is no Lean-visible function-pointer parameter, since
    Lean cannot construct one). -/
@[extern "linen_duckdb_log_storage_set_write_log_entry"]
opaque setWriteLogEntry : LogStorage → IO Unit

/-- `duckdb_log_storage_set_extra_data`: retain `onWrite` (via
    `lean_inc_ref`) and install it as `logStorage`'s `extra_data`, with this
    port's delete trampoline as the `delete_callback` that releases it once
    `logStorage` itself is destroyed. `onWrite` receives, per log entry: the
    raw microsecond timestamp, the log level, the log type, and the log
    message. -/
@[extern "linen_duckdb_log_storage_set_extra_data"]
opaque setExtraData (logStorage : @& LogStorage)
    (onWrite : Int64 → String → String → String → IO Unit) : IO Unit

/-- `duckdb_log_storage_set_name`: the registration name of `logStorage`. -/
@[extern "linen_duckdb_log_storage_set_name"]
opaque setName (logStorage : @& LogStorage) (name : @& String) : IO Unit

/-- Raw `duckdb_register_log_storage`. -/
@[extern "linen_duckdb_register_log_storage"]
opaque registerLogStorageRaw (database : @& Database) (logStorage : @& LogStorage) : IO UInt32

/-- Register `logStorage` as `database`'s logger backend. -/
def registerLogStorage (database : Database) (logStorage : LogStorage) : IO State :=
  State.ofUInt32 <$> registerLogStorageRaw database logStorage

/-- Build a `LogStorage` named `name` whose write callback is `onWrite`, and
    install it on `database` in one call. See this module's doc comment for
    the ownership caveat on the returned `LogStorage` once registration
    succeeds. -/
def register (database : Database) (name : String)
    (onWrite : Int64 → String → String → String → IO Unit) : IO (State × LogStorage) := do
  let logStorage ← create
  setWriteLogEntry logStorage
  setExtraData logStorage onWrite
  setName logStorage name
  let state ← registerLogStorage database logStorage
  pure (state, logStorage)

end Database.DuckDB.FFI.Logging
