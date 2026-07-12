/-
  Linen.Database.DuckDB.FFI.Configuration — `duckdb_config` creation and
  option management

  Mirrors Haskell's `Database.DuckDB.FFI.Configuration` (the `duckdb-ffi`
  package). Module #5 of `docs/imports/duckdb-ffi/dependencies.md`; depends
  only on `Database.DuckDB.FFI.Types` (module #1).

  Every `@[extern]` declaration below is backed by `ffi/duckdb_shim.c`. Two
  independent lifecycles are ported here, faithfully mirroring upstream:

  - **Built-in options**: `createConfig`/`setConfig`/`destroyConfig` build a
    `duckdb_config` for `duckdb_open_ext`'s startup options; `configCount`/
    `getConfigFlag` enumerate the config keys DuckDB itself understands.
    Note that this port's own `Database.DuckDB.FFI.OpenConnect.openExt`
    doesn't yet thread a `Config` through to `duckdb_open_ext` (it always
    passes `NULL`, i.e. "use the defaults" — see that module's doc comment);
    wiring a real `Config` into `openExt` is left to whoever ports
    `duckdb-simple`'s own config-plumbing layer on top of this module.
  - **Custom options**: `createConfigOption`/the `configOptionSet*` setters/
    `registerConfigOption`/`destroyConfigOption` build and register a
    brand-new named option that `clientContextGetConfigOption` (and
    `getConfigFlag`, once registered) can then see.

  `duckdb_config_count`'s C-side `size_t` is bound as `Idx` (`UInt64`), the
  same substitution `Types.lean` already uses for every other `idx_t`-typed
  quantity in this port, rather than introducing a separate `CSize` type for
  what is, on every platform DuckDB ships for, the same width.
-/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.Configuration

open Database.DuckDB.FFI.Types

/-! ── Built-in configuration options ── -/

/-- Raw `duckdb_create_config`: initializes an empty configuration object,
    returning `(state, config?)`. -/
@[extern "linen_duckdb_create_config"]
opaque createConfigRaw : IO (UInt32 × Option Config)

/-- Initialize an empty configuration object, usable as `duckdb_open_ext`'s
    start-up options. The resulting `Config` must eventually be destroyed
    with `destroyConfig` (or let its GC finalizer do so) — per `duckdb.h`'s
    own doc comment, this is true *even if this call itself fails*, but since
    a failure here (malloc failure only) never produces a `Config` to
    destroy in the first place, that requirement is trivially satisfied by
    this wrapper's `Option` return. -/
def createConfig : IO (Except String Config) := do
  let (rc, cfgOpt) ← createConfigRaw
  match State.ofUInt32 rc, cfgOpt with
  | .success, some cfg => pure (.ok cfg)
  | _, _ => pure (.error "duckdb_create_config failed")

/-- The total number of built-in configuration options DuckDB understands
    (usable as the exclusive upper bound for `getConfigFlag`'s index). Not
    meant to be called in a loop — it internally loops over all the options
    itself. -/
@[extern "linen_duckdb_config_count"]
opaque configCount : IO Idx

/-- Raw `duckdb_get_config_flag`: `(state, name?, description?)`. -/
@[extern "linen_duckdb_get_config_flag"]
opaque getConfigFlagRaw (index : Idx) : IO (UInt32 × Option String × Option String)

/-- A human-readable name and description of the built-in configuration
    option at `index` (between `0` and `configCount`). Fails if `index` is
    out of that range. -/
def getConfigFlag (index : Idx) : IO (Except String (String × String)) := do
  let (rc, nameOpt, descOpt) ← getConfigFlagRaw index
  match State.ofUInt32 rc, nameOpt, descOpt with
  | .success, some name, some desc => pure (.ok (name, desc))
  | _, _, _ => pure (.error s!"duckdb_get_config_flag failed for index {index}")

/-- Set the built-in or custom option named `name` on `config` to `value`.
    Fails if `name` is unrecognized or `value` is invalid for that option;
    upstream's C API surfaces no further detail on *why* in this call
    itself. -/
@[extern "linen_duckdb_set_config"]
opaque setConfigRaw (config : @& Config) (name : @& String) (value : @& String) : IO UInt32

/-- `setConfigRaw`, decoded to a `State`. -/
def setConfig (config : Config) (name : String) (value : String) : IO State :=
  State.ofUInt32 <$> setConfigRaw config name value

/-- Destroy `config`, deallocating all memory associated with it. Idempotent,
    like `Database.DuckDB.FFI.OpenConnect.close`. -/
@[extern "linen_duckdb_destroy_config"]
opaque destroyConfig : Config → IO Unit

/-! ── Custom configuration options ── -/

/-- Create a new custom configuration-option descriptor. Must eventually be
    destroyed with `destroyConfigOption` (or `registerConfigOption`'s GC
    finalizer, if never explicitly destroyed) — mirrors
    `Database.DuckDB.FFI.OpenConnect.createInstanceCache`'s always-succeeds
    contract. -/
@[extern "linen_duckdb_create_config_option"]
opaque createConfigOption : IO ConfigOption

/-- Destroy `option`, releasing its native resources. Idempotent. -/
@[extern "linen_duckdb_destroy_config_option"]
opaque destroyConfigOption : ConfigOption → IO Unit

/-- Set `option`'s name. -/
@[extern "linen_duckdb_config_option_set_name"]
opaque configOptionSetName (option : @& ConfigOption) (name : @& String) : IO Unit

/-- Set `option`'s logical (SQL) type. -/
@[extern "linen_duckdb_config_option_set_type"]
opaque configOptionSetType (option : @& ConfigOption) (type : @& LogicalType) : IO Unit

/-- Set `option`'s default value. -/
@[extern "linen_duckdb_config_option_set_default_value"]
opaque configOptionSetDefaultValue (option : @& ConfigOption) (value : @& Value) : IO Unit

/-- Raw `duckdb_config_option_set_default_scope`. -/
@[extern "linen_duckdb_config_option_set_default_scope"]
opaque configOptionSetDefaultScopeRaw (option : @& ConfigOption) (scope : UInt32) : IO Unit

/-- Set `option`'s default scope (defaults to `.session` if never called). -/
def configOptionSetDefaultScope (option : ConfigOption) (scope : ConfigOptionScope) : IO Unit :=
  configOptionSetDefaultScopeRaw option scope.toUInt32

/-- Set `option`'s human-readable description. -/
@[extern "linen_duckdb_config_option_set_description"]
opaque configOptionSetDescription (option : @& ConfigOption) (description : @& String) : IO Unit

/-- Raw `duckdb_register_config_option`. -/
@[extern "linen_duckdb_register_config_option"]
opaque registerConfigOptionRaw (connection : @& Connection) (option : @& ConfigOption) : IO UInt32

/-- Register the custom option `option` (built via `createConfigOption` and
    the `configOptionSet*` setters above) on `connection`, decoded to a
    `State`. -/
def registerConfigOption (connection : Connection) (option : ConfigOption) : IO State :=
  State.ofUInt32 <$> registerConfigOptionRaw connection option

/-- Raw `duckdb_client_context_get_config_option`: `(value, scope)`. -/
@[extern "linen_duckdb_client_context_get_config_option"]
opaque clientContextGetConfigOptionRaw (context : @& ClientContext) (name : @& String) :
    IO (Value × UInt32)

/-- The current value and scope of the (built-in or registered custom)
    configuration option named `name`, as seen from `context`. If `name`
    does not identify a known option, the returned scope decodes to
    `.invalid` (per `duckdb.h`'s own documented behavior for this call). -/
def clientContextGetConfigOption (context : ClientContext) (name : String) :
    IO (Value × ConfigOptionScope) := do
  let (value, scope) ← clientContextGetConfigOptionRaw context name
  pure (value, ConfigOptionScope.ofUInt32 scope)

end Database.DuckDB.FFI.Configuration
