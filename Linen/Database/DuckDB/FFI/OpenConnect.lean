/-
  Linen.Database.DuckDB.FFI.OpenConnect — opening/connecting/closing DuckDB
  databases

  Mirrors Haskell's `Database.DuckDB.FFI.OpenConnect` (the `duckdb-ffi`
  package). Module #2 of `docs/imports/duckdb-ffi/dependencies.md`; depends
  only on `Database.DuckDB.FFI.Types` (module #1).

  Every `@[extern]` declaration below is backed by `ffi/duckdb_shim.c`, whose
  header comment documents this module's one scope reduction: `duckdb_open_ext`
  and `duckdb_get_or_create_from_cache` always pass a NULL `duckdb_config`
  (i.e. "use the default configuration", a real DuckDB-specified behavior),
  since `Database.DuckDB.FFI.Configuration` is not yet ported.

  Where upstream's Haskell binds a `CString` parameter that DuckDB's C API
  treats as nullable (`path` in `duckdb_open`/`duckdb_open_ext`/
  `duckdb_get_or_create_from_cache`: both `NULL` and `":memory:"` mean "open
  an in-memory database"), this port uses `Option String` instead of asking
  every caller to spell out `":memory:"` — `none` passes a real C `NULL`,
  `some path` passes `path`, and both are accepted per `duckdb.h`'s own
  documented contract.
-/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.OpenConnect

open Database.DuckDB.FFI.Types

/-! ── Instance cache ── -/

/-- Create a new database instance cache. Must eventually be released (the
    GC finalizer calls `duckdb_destroy_instance_cache` if `destroyInstanceCache`
    was never called explicitly). The instance cache lets a process that
    (re)opens the same database file multiple times share one underlying
    instance instead of creating a fresh one each time. -/
@[extern "linen_duckdb_create_instance_cache"]
opaque createInstanceCache : IO InstanceCache

/-- Raw `duckdb_get_or_create_from_cache`: creates a new database instance in
    the cache, or retrieves an existing one, returning `(state, database?,
    error?)`. `path := none` (or `:memory:`) opens/retrieves an in-memory
    database. -/
@[extern "linen_duckdb_get_or_create_from_cache"]
opaque getOrCreateFromCacheRaw (cache : @& InstanceCache) (path : @& Option String) :
    IO (UInt32 × Option Database × Option String)

/-- Create a new database instance in `cache`, or retrieve an existing one.
    The resulting `Database` must be closed with `Database.DuckDB.FFI.OpenConnect.close`
    (or let its GC finalizer do so). Returns `.error msg` on failure. -/
def getOrCreateFromCache (cache : InstanceCache) (path : Option String) :
    IO (Except String Database) := do
  let (rc, dbOpt, errOpt) ← getOrCreateFromCacheRaw cache path
  match State.ofUInt32 rc, dbOpt with
  | .success, some db => pure (.ok db)
  | _, _ => pure (.error (errOpt.getD "duckdb_get_or_create_from_cache failed"))

/-- Destroy an instance cache, releasing its memory. Idempotent: calling this
    again (or letting the GC finalizer run afterwards) is a no-op. -/
@[extern "linen_duckdb_destroy_instance_cache"]
opaque destroyInstanceCache : InstanceCache → IO Unit

/-! ── Open / close ── -/

/-- Raw `duckdb_open`: creates a new database or opens an existing database
    file, returning `(state, database?)`. `path := none` (or `:memory:`)
    creates an in-memory database. -/
@[extern "linen_duckdb_open"]
opaque openRaw (path : @& Option String) : IO (UInt32 × Option Database)

/-- Create a new database or open an existing database file at `path` (an
    in-memory database if `path := none`). The resulting `Database` must be
    closed with `close` (or let its GC finalizer do so).

    Named `openDatabase`, not `open` (upstream's name, mirrored by `openRaw`
    above) — `open` is a Lean keyword (the namespace-opening command) and
    can't be used as an ordinary identifier. -/
def openDatabase (path : Option String) : IO (Except String Database) := do
  let (rc, dbOpt) ← openRaw path
  match State.ofUInt32 rc, dbOpt with
  | .success, some db => pure (.ok db)
  | _, _ => pure (.error "duckdb_open failed")

/-- Raw `duckdb_open_ext`: extended version of `duckdb_open`, returning
    `(state, database?, error?)`. Always uses the default configuration (see
    the module doc comment). -/
@[extern "linen_duckdb_open_ext"]
opaque openExtRaw (path : @& Option String) : IO (UInt32 × Option Database × Option String)

/-- Extended version of `open`: creates a new database or opens an existing
    database file at `path`, surfacing the C API's error message on
    failure. Always uses the default configuration (see the module doc
    comment). -/
def openExt (path : Option String) : IO (Except String Database) := do
  let (rc, dbOpt, errOpt) ← openExtRaw path
  match State.ofUInt32 rc, dbOpt with
  | .success, some db => pure (.ok db)
  | _, _ => pure (.error (errOpt.getD "duckdb_open_ext failed"))

/-- Close `database` and deallocate all memory associated with it. Idempotent:
    calling this again (or letting the GC finalizer run afterwards) is a
    no-op. Should be called after you're done with any `Database` obtained
    from `open`/`openExt`/`getOrCreateFromCache`. -/
@[extern "linen_duckdb_close"]
opaque close : Database → IO Unit

/-! ── Connect / disconnect ── -/

/-- Raw `duckdb_connect`: opens a connection to `database`, returning
    `(state, connection?)`. -/
@[extern "linen_duckdb_connect"]
opaque connectRaw (database : @& Database) : IO (UInt32 × Option Connection)

/-- Open a connection to `database`. Connections are required to query the
    database, and store transactional state. The resulting `Connection` must
    be closed with `disconnect` (or let its GC finalizer do so). -/
def connect (database : Database) : IO (Except String Connection) := do
  let (rc, connOpt) ← connectRaw database
  match State.ofUInt32 rc, connOpt with
  | .success, some conn => pure (.ok conn)
  | _, _ => pure (.error "duckdb_connect failed")

/-- Interrupt a running query on `connection`. -/
@[extern "linen_duckdb_interrupt"]
opaque interrupt : Connection → IO Unit

/-- Raw `duckdb_query_progress`: `(percentage, rowsProcessed,
    totalRowsToProcess)`. -/
@[extern "linen_duckdb_query_progress"]
opaque queryProgressRaw (connection : @& Connection) : IO (Float × UInt64 × UInt64)

/-- Get the progress of the query currently running on `connection`. A
    `percentage` of `-1` means no progress is available (e.g. no query is
    currently running). -/
def queryProgress (connection : Connection) : IO QueryProgress := do
  let (percentage, rowsProcessed, totalRowsToProcess) ← queryProgressRaw connection
  pure { percentage, rowsProcessed, totalRowsToProcess }

/-- Close `connection` and deallocate all memory associated with it.
    Idempotent, like `close`. -/
@[extern "linen_duckdb_disconnect"]
opaque disconnect : Connection → IO Unit

/-! ── Client context / Arrow options ── -/

/-- Retrieve `connection`'s client context. Must eventually be released (the
    GC finalizer calls `duckdb_destroy_client_context` if
    `destroyClientContext` was never called explicitly). -/
@[extern "linen_duckdb_connection_get_client_context"]
opaque connectionGetClientContext : Connection → IO ClientContext

/-- Retrieve `connection`'s Arrow options. Must eventually be released
    likewise (`duckdb_destroy_arrow_options`/`destroyArrowOptions`). -/
@[extern "linen_duckdb_connection_get_arrow_options"]
opaque connectionGetArrowOptions : Connection → IO ArrowOptions

/-- The connection id of `context`. -/
@[extern "linen_duckdb_client_context_get_connection_id"]
opaque clientContextGetConnectionId (context : @& ClientContext) : IO UInt64

/-- Destroy `context`, deallocating its memory. Idempotent. -/
@[extern "linen_duckdb_destroy_client_context"]
opaque destroyClientContext : ClientContext → IO Unit

/-- Destroy `arrowOptions`, deallocating its memory. Idempotent. -/
@[extern "linen_duckdb_destroy_arrow_options"]
opaque destroyArrowOptions : ArrowOptions → IO Unit

/-! ── Misc ── -/

/-- The version of the linked DuckDB library (with a dev-version postfix, if
    applicable). -/
@[extern "linen_duckdb_library_version"]
opaque libraryVersion : IO String

/-- Raw `duckdb_get_table_names`: `qualified` as a raw `UInt8` (`0`/`1`). -/
@[extern "linen_duckdb_get_table_names"]
opaque getTableNamesRaw (connection : @& Connection) (query : @& String) (qualified : UInt8) :
    IO Value

/-- The list of (fully qualified, if `qualified`) table names referenced by
    `query`, as a `VARCHAR[]`-typed `Value`. Must eventually be released
    (the GC finalizer calls `duckdb_destroy_value` if never done explicitly).
    Decoding the returned `Value`'s contents is out of scope here — that's
    `Database.DuckDB.FFI.ValueInterface`, one of the 26 excluded modules. -/
def getTableNames (connection : Connection) (query : String) (qualified : Bool) : IO Value :=
  getTableNamesRaw connection query (if qualified then 1 else 0)

end Database.DuckDB.FFI.OpenConnect
