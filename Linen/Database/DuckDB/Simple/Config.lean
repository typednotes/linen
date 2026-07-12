/-
  Linen.Database.DuckDB.Simple.Config — connection configuration inspection

  Module #11 of `docs/imports/duckdb-simple/dependencies.md`, on #1
  (`Linen.Database.DuckDB.Simple.Internal`, for `Connection`/`SQLError`/
  `withClientContext`) and `Linen.Database.DuckDB.FFI.Configuration`.

  ## Design

  `Linen.Database.DuckDB.FFI.Configuration` already provides the raw
  `duckdb_create_config`/`duckdb_set_config`/`duckdb_config_count`/
  `duckdb_get_config_flag`/`duckdb_client_context_get_config_option` family
  — this module is the `Simple`-package ergonomic wrapper around them,
  matching this batch's shape (`Catalog`'s own module doc describes the
  same pattern): every per-connection call is threaded through a
  `Connection` (fetching and releasing a scratch `ClientContext` via
  `Internal.withClientContext`) rather than requiring the caller to manage
  one by hand, and a failed built-in `duckdb_state`-returning call is
  reported as an `SQLError` via `throwSQLError`/`registrationError` rather
  than upstream's own exception type (see `Internal`'s module doc for why).

  Upstream's own module (per the fetched `duckdb-simple` source) exposes
  exactly two things: `listConfigFlags` (every built-in option DuckDB
  understands) and `getConfigOption` (a connection's current value/scope for
  a named option). This port additionally exposes the pre-`open`
  `Config`-object lifecycle (`createConfig`/`setConfigOption`/
  `destroyConfig`) directly off `Linen.Database.DuckDB.FFI.Configuration`,
  since that lifecycle is otherwise unreachable from `duckdb-simple` (see
  `Internal`'s own module doc: its `openConnection` never threads a `Config`
  through to `duckdb_open_ext`, deferring that wiring to "whoever ports
  `duckdb-simple`'s own config-plumbing layer" — this module).

  ### Deviation: `getConfigOption`'s value is a boxed `Value`, not `String`

  Upstream renders a config option's current value to `Text` via
  `duckdb_get_varchar`. `Linen.Database.DuckDB.FFI.Types.Value`'s own doc
  comment documents that only the bare handle type is in scope for this
  codebase's `duckdb-ffi` port — its full boxed-value decoding API
  (`Database.DuckDB.FFI.ValueInterface`, including `duckdb_get_varchar`) is
  one of that port's excluded modules. This module therefore returns the
  boxed `Value` handle as-is rather than a rendered `String`; a caller
  needing the text form must wait for `ValueInterface` to be ported. This
  is a genuine "the underlying decoding API was never ported" scope gap,
  not a proof-avoidance shortcut — see `AGENTS.md`'s termination-proof rule
  for the distinction this port is careful to keep, and
  `Database.DuckDB.FFI.Types`'s own `ValueHandle` doc comment for the
  original exclusion.

  ## Haskell source
  - `Database.DuckDB.Simple.Config` (`duckdb-simple` package, version
    0.1.5.1)
-/
import Linen.Database.DuckDB.Simple.Internal
import Linen.Database.DuckDB.FFI.Configuration

namespace Database.DuckDB.Simple.Config

open Database.DuckDB.FFI.Types (Value ConfigOptionScope Config)
open Database.DuckDB.Simple (Connection SQLError throwSQLError registrationError withClientContext)

-- ────────────────────────────────────────────────────────────────────
-- Pre-`open` `Config` lifecycle
-- ────────────────────────────────────────────────────────────────────

/-- A fresh, blank configuration object, usable as `duckdb_open_ext`'s
    start-up options (see the module doc: no `duckdb-simple` entry point in
    this batch actually threads the result into `openConnection` yet). Must
    eventually be destroyed with `destroyConfig` (or let its GC finalizer do
    so). -/
def createConfig : IO Config := do
  match ← Database.DuckDB.FFI.Configuration.createConfig with
  | .ok cfg => pure cfg
  | .error msg => throwSQLError { message := msg : SQLError }

/-- Set the built-in or custom option named `name` on `config` to `value`,
    prior to opening a connection with it. -/
def setConfigOption (config : Config) (name value : String) : IO Unit := do
  match ← Database.DuckDB.FFI.Configuration.setConfig config name value with
  | .success => pure ()
  | .error => throwSQLError (registrationError s!"set config option {name}")

/-- Destroy `config`, releasing its native resources early. Idempotent. -/
def destroyConfig (config : Config) : IO Unit :=
  Database.DuckDB.FFI.Configuration.destroyConfig config

-- ────────────────────────────────────────────────────────────────────
-- Inspecting built-in options
-- ────────────────────────────────────────────────────────────────────

/-- A built-in configuration option's name and human-readable description. -/
structure ConfigFlag where
  name : String
  description : String
deriving BEq, Repr, Inhabited

/-- Every built-in configuration option DuckDB understands (upstream's
    `listConfigFlags`). -/
def listConfigFlags : IO (Array ConfigFlag) := do
  let n ← Database.DuckDB.FFI.Configuration.configCount
  let mut flags : Array ConfigFlag := #[]
  for i in [0:n.toNat] do
    match ← Database.DuckDB.FFI.Configuration.getConfigFlag (UInt64.ofNat i) with
    | .ok (name, description) => flags := flags.push { name, description }
    | .error _ => pure ()
  pure flags

-- ────────────────────────────────────────────────────────────────────
-- Inspecting a live connection's option values
-- ────────────────────────────────────────────────────────────────────

/-- The current value and scope of a (built-in or registered custom)
    configuration option, as reported by `getConfigOption` (see the module
    doc's note on why `value` is a boxed `Value`, not a `String`). -/
structure ConfigValue where
  value : Value
  scope : ConfigOptionScope

/-- The current value/scope of the configuration option named `name`, as
    seen from `conn` (upstream's `getConfigOption`). If `name` does not
    identify a known option, the returned scope decodes to `.invalid` (per
    `Linen.Database.DuckDB.FFI.Configuration.clientContextGetConfigOption`'s
    own doc comment). -/
def getConfigOption (conn : Connection) (name : String) : IO ConfigValue :=
  withClientContext conn fun ctx => do
    let (value, scope) ← Database.DuckDB.FFI.Configuration.clientContextGetConfigOption ctx name
    pure { value, scope }

end Database.DuckDB.Simple.Config
