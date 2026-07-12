/-
  Tests for `Linen.Database.DuckDB.FFI.Configuration`.

  Exercises both lifecycles the module documents: the built-in
  `createConfig`/`configCount`/`getConfigFlag`/`setConfig`/`destroyConfig`
  path, and the custom `createConfigOption`/`configOptionSet*`/
  `registerConfigOption`/`destroyConfigOption`/`clientContextGetConfigOption`
  path. `configOptionSetType` needs a real `LogicalType`, which this module
  has no producer for on its own (`Database.DuckDB.FFI.LogicalTypes` is out
  of scope for this batch) — so this test borrows one for free from
  `Database.DuckDB.FFI.Appender.columnType` on a real table column, the same
  cross-module borrowing `DataChunkTest` uses for its own `LogicalType`.
-/
import Linen.Database.DuckDB.FFI.Configuration
import Linen.Database.DuckDB.FFI.Appender
import Linen.Database.DuckDB.FFI.OpenConnect
import Tests.Linen.Database.DuckDB.FFI.TestSupport

open Database.DuckDB.FFI.Configuration
open Database.DuckDB.FFI.Appender
open Database.DuckDB.FFI.OpenConnect
open Database.DuckDB.FFI.Types
open Tests.Database.DuckDB.FFI.TestSupport

namespace Tests.Database.DuckDB.FFI.Configuration

#eval show IO Unit from do
  -- ── Built-in options ──
  let cfgResult ← createConfig
  let cfg ← match cfgResult with
    | .ok cfg => pure cfg
    | .error msg => throw (IO.userError msg)

  let count ← configCount
  if count == 0 then
    throw (IO.userError "expected at least one built-in configuration option")

  let flag0 ← getConfigFlag 0
  match flag0 with
  | .error msg => throw (IO.userError s!"getConfigFlag 0 failed: {msg}")
  | .ok (name, _desc) =>
    if name.isEmpty then
      throw (IO.userError "expected a non-empty built-in option name at index 0")

  -- An out-of-range index must fail.
  let flagOob ← getConfigFlag count
  match flagOob with
  | .ok _ => throw (IO.userError "expected getConfigFlag to fail for an out-of-range index")
  | .error _ => pure ()

  let setState ← setConfig cfg "threads" "4"
  if !setState.isSuccess then
    throw (IO.userError "expected setConfig \"threads\" \"4\" to succeed")

  destroyConfig cfg
  destroyConfig cfg -- idempotent

  -- ── Custom options, using a real LogicalType borrowed from Appender ──
  let dbResult ← openDatabase none
  let db ← match dbResult with
    | .ok db => pure db
    | .error msg => throw (IO.userError s!"duckdb_open failed: {msg}")

  let connResult ← connect db
  let conn ← match connResult with
    | .ok conn => pure conn
    | .error msg => throw (IO.userError s!"duckdb_connect failed: {msg}")

  -- `a`'s column type is `VARCHAR[]`, matching `getTableNames`'s own return
  -- type below exactly — so `configOptionSetDefaultValue` never needs DuckDB
  -- to cast between two unrelated logical types.
  let createState ← queryExec conn "CREATE TABLE cfg_probe(a VARCHAR[])"
  if !createState.isSuccess then
    throw (IO.userError "CREATE TABLE cfg_probe failed")

  let appResult ← create conn none "cfg_probe"
  let appender ← match appResult with
    | .ok app => pure app
    | .error msg => throw (IO.userError msg)

  let listType ← columnType appender 0

  let ctx ← connectionGetClientContext conn

  -- An unknown option name must report an `.invalid` scope.
  let (_bogusValue, bogusScope) ← clientContextGetConfigOption ctx "definitely_not_a_real_option"
  if bogusScope != .invalid then
    throw (IO.userError s!"expected .invalid scope for an unknown option, got {repr bogusScope}")

  let option ← createConfigOption
  configOptionSetName option "linen_test_option"
  configOptionSetType option listType
  let sampleValue ← getTableNames conn "SELECT 1" false
  configOptionSetDefaultValue option sampleValue
  configOptionSetDefaultScope option .session
  configOptionSetDescription option "A custom option registered by ConfigurationTest"

  let regState ← registerConfigOption conn option
  if !regState.isSuccess then
    throw (IO.userError "registerConfigOption failed")

  -- Once registered, the option is known, so its scope must no longer be
  -- `.invalid`.
  let (_regValue, regScope) ← clientContextGetConfigOption ctx "linen_test_option"
  if regScope == .invalid then
    throw (IO.userError "expected a non-invalid scope for the just-registered custom option")

  destroyConfigOption option
  destroyConfigOption option -- idempotent

  destroy appender -- Appender.destroy
  destroyClientContext ctx
  disconnect conn
  close db

end Tests.Database.DuckDB.FFI.Configuration
