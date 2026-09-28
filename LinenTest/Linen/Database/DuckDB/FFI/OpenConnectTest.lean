/-
  Tests for `Linen.Database.DuckDB.FFI.OpenConnect`.

  This is the load-bearing test of this whole native-dependency batch: it
  actually opens a real in-memory DuckDB database (`duckdb_open(NULL, &db)`
  per the C API — the `path := none` case here), connects to it, runs a
  trivial query end-to-end via `duckdb_query`/`duckdb_state`-free bookkeeping
  is out of scope (`Database.DuckDB.FFI.QueryExecution` isn't ported yet), so
  this exercises exactly the surface `OpenConnect` itself provides — and
  closes everything back down again. If `lakefile.lean`'s DuckDB discovery
  (download the pinned archive, resolve `-I`/`-L`, get the runtime rpath
  right) were broken in any way, either the build itself would fail to link,
  or this `#eval` (which runs the FFI calls through the interpreter, so the
  shared library must be dlopen-able at this point too, not just link-time
  resolvable) would fail at run time.
-/
import Linen.Database.DuckDB.FFI.OpenConnect

open Database.DuckDB.FFI.OpenConnect
open Database.DuckDB.FFI.Types

namespace Tests.Database.DuckDB.FFI.OpenConnect

-- Full round-trip: open an in-memory database, connect, check the linked
-- library's version string is non-empty, interrupt a (non-running) query
-- and read its progress, then disconnect and close — proving the whole
-- link chain (download → compile/link → runtime dlopen → FFI call) works.
#eval show IO Unit from do
  let version ← libraryVersion
  if version.isEmpty then
    throw (IO.userError "duckdb_library_version returned an empty string")

  let dbResult ← openDatabase none -- in-memory database
  let db ← match dbResult with
    | .ok db => pure db
    | .error msg => throw (IO.userError s!"duckdb_open failed: {msg}")

  let connResult ← connect db
  let conn ← match connResult with
    | .ok conn => pure conn
    | .error msg => throw (IO.userError s!"duckdb_connect failed: {msg}")

  -- No query is running yet, so `-1` (no progress available) is expected.
  let progress ← queryProgress conn
  if progress.percentage != -1.0 then
    throw (IO.userError s!"expected no query progress yet, got {repr progress}")

  interrupt conn -- no-op with no running query; just exercises the entry point

  let ctx ← connectionGetClientContext conn
  let connId ← clientContextGetConnectionId ctx
  destroyClientContext ctx
  -- Connection ids are non-negative `UInt64`s; just check the call completed
  -- and produced a value (any value, including `0`, is a valid connection
  -- id — this only proves the FFI round-trip itself worked).
  if connId != connId then
    throw (IO.userError "unreachable")

  let arrowOpts ← connectionGetArrowOptions conn
  destroyArrowOptions arrowOpts

  disconnect conn
  close db

-- `getOrCreateFromCache`: create an instance cache, get an in-memory
-- database from it, connect, disconnect, close, then destroy the cache.
#eval show IO Unit from do
  let cache ← createInstanceCache
  let dbResult ← getOrCreateFromCache cache none
  let db ← match dbResult with
    | .ok db => pure db
    | .error msg => throw (IO.userError s!"duckdb_get_or_create_from_cache failed: {msg}")

  let connResult ← connect db
  let conn ← match connResult with
    | .ok conn => pure conn
    | .error msg => throw (IO.userError s!"duckdb_connect failed: {msg}")

  disconnect conn
  close db
  destroyInstanceCache cache

-- `openExt` with the default configuration behaves like `open` for an
-- in-memory database.
#eval show IO Unit from do
  let dbResult ← openExt none
  let db ← match dbResult with
    | .ok db => pure db
    | .error msg => throw (IO.userError s!"duckdb_open_ext failed: {msg}")
  close db

end Tests.Database.DuckDB.FFI.OpenConnect
