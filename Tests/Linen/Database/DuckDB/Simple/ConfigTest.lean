/-
  Tests for `Linen.Database.DuckDB.Simple.Config`.

  Exercises the pre-`open` `Config` object lifecycle
  (`createConfig`/`setConfigOption`/`destroyConfig`) against a real
  configuration handle, confirms `listConfigFlags` reports a real,
  nonempty list of built-in options (including a couple of well-known
  ones), and confirms `getConfigOption` reports a non-`.invalid` scope for
  a known built-in option on a real, live connection — proving each call
  round-trips through the real `duckdb_config`/`duckdb_client_context`
  FFI, not just that this port's Lean code type-checks.
-/
import Linen.Database.DuckDB.Simple.Config
import Linen.Database.DuckDB.Simple.Internal

open Database.DuckDB.Simple
open Database.DuckDB.Simple.Config
open Database.DuckDB.FFI.Types (ConfigOptionScope)

namespace Tests.Database.DuckDB.Simple.Config

#eval show IO Unit from do
  -- Pre-`open` `Config` lifecycle: create, set a couple of options, destroy.
  let config ← createConfig
  setConfigOption config "threads" "4"
  setConfigOption config "access_mode" "READ_WRITE"
  destroyConfig config

  -- `listConfigFlags` reports the real set of built-in options DuckDB
  -- understands.
  let flags ← listConfigFlags
  unless flags.size > 0 do throw (IO.userError "expected at least one built-in config flag")
  unless flags.any (·.name == "threads") do
    throw (IO.userError "expected \"threads\" among the built-in config flags")
  unless flags.any (·.name == "memory_limit") do
    throw (IO.userError "expected \"memory_limit\" among the built-in config flags")

  -- `getConfigOption` against a real, live connection.
  let conn ← openConnection none -- in-memory database
  let threadsValue ← getConfigOption conn "threads"
  if threadsValue.scope == ConfigOptionScope.invalid then
    throw (IO.userError "expected \"threads\" to be a recognized option")

  let unknownValue ← getConfigOption conn "no_such_option_at_all"
  unless unknownValue.scope == ConfigOptionScope.invalid do
    throw (IO.userError "expected an unrecognized option to decode to an invalid scope")

  closeConnection conn

-- `ConfigFlag`'s derived instances.
#guard ({ name := "a", description := "b" } : ConfigFlag) == ({ name := "a", description := "b" } : ConfigFlag)
#guard ({ name := "a", description := "b" } : ConfigFlag).name == "a"

end Tests.Database.DuckDB.Simple.Config
