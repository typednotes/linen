/-
  Linen.Database.DuckDB.FFI.ScalarFunctions — registering user-defined
  scalar functions

  Mirrors Haskell's `Database.DuckDB.FFI.ScalarFunctions` (the `duckdb-ffi`
  package). One of the batch of modules from
  `docs/imports/duckdb-ffi/dependencies.md` depending only on
  `Database.DuckDB.FFI.Types` (module #1).

  **Scope.** Upstream's full scalar-function API also covers the
  bind/init lifecycle (`duckdb_scalar_function_set_bind`/`set_init` and
  their `duckdb_bind_info`/`duckdb_init_info` accessors — per-invocation
  bind-time argument inspection, custom bind/init data, and their own error
  channels). This port excludes that lifecycle: it is not required for a
  basic register-a-function-and-have-it-fire round trip, and every one of
  those entry points needs its own opaque `BindInfo`/`InitInfo` handle
  declared in `Types.lean` purely to support a feature this batch has no
  other consumer for. This mirrors the same kind of documented,
  scope-narrowing precedent already used elsewhere in this import (e.g.
  `Configuration`'s "always pass a NULL `duckdb_config`",
  `Logging`'s "swallow callback errors"). What *is* covered: building a
  function's signature (`create`/`setName`/`setVarargs`/
  `setSpecialHandling`/`setVolatile`/`addParameter`/`setReturnType`),
  installing its native implementation (`setExtraInfo`/`setFunction`),
  registering it (`register`/`registerSet`), and reading its per-call
  input (`inputSize`/`inputColumnCount`/`inputVector` on the callback's
  borrowed `BorrowedDataChunk`).

  **The callback trampoline.** DuckDB itself invokes a function's native
  implementation (`duckdb_scalar_function_t`) directly — Lean cannot
  construct a real C function pointer for `duckdb_scalar_function_set_
  function` to install, so (mirroring
  `Database.DuckDB.FFI.Logging`'s `LogStorage` trampoline pair) this port
  installs one *fixed* native trampoline
  (`linen_duckdb_scalar_function_call_trampoline`) that retrieves the
  actual Lean closure from the function's `extra_data` slot (set via
  `setExtraInfo`, using the same `duckdb_delete_callback_t`-backed
  lifetime management `Logging` already uses) and applies it. The
  callback's input `duckdb_data_chunk` is owned by DuckDB, not by this
  program — see `Types.lean`'s doc comment on `BorrowedDataChunkHandle` for
  why it gets its own non-owning handle rather than reusing `DataChunk`.
  The output `duckdb_vector` is likewise borrowed and reuses
  `Types.Vector` (already non-owning, per its own doc comment). Any
  Lean-side failure inside the callback is silently swallowed, exactly as
  `Logging`'s write callback already documents: DuckDB's callback type is
  `void`-returning, so there is no channel to surface an exception through
  (reporting an in-band SQL error via `duckdb_scalar_function_set_error`
  is part of the excluded bind/init-adjacent error-reporting surface
  above). -/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.ScalarFunctions

open Database.DuckDB.FFI.Types

/-! ── Building a function's signature ── -/

/-- Create a new, empty scalar function. Must eventually be destroyed with
    `destroy` (or let its GC finalizer do so), unless it is added to a
    `ScalarFunctionSet` via `addToSet` (which takes over ownership) or
    registered directly via `register`. -/
@[extern "linen_duckdb_create_scalar_function"]
opaque create : IO ScalarFunction

/-- Destroy `fn`, deallocating all associated memory. -/
@[extern "linen_duckdb_destroy_scalar_function"]
opaque destroy : ScalarFunction → IO Unit

/-- Set `fn`'s SQL name. -/
@[extern "linen_duckdb_scalar_function_set_name"]
opaque setName (fn : @& ScalarFunction) (name : @& String) : IO Unit

/-- Set `fn` to accept variable numbers of arguments, all of logical type
    `ty`. -/
@[extern "linen_duckdb_scalar_function_set_varargs"]
opaque setVarargs (fn : @& ScalarFunction) (ty : @& LogicalType) : IO Unit

/-- Enable `fn`'s "special handling" mode, in which `NULL`/error handling
    of individual argument values is left to `fn`'s own implementation
    rather than DuckDB's default behavior. -/
@[extern "linen_duckdb_scalar_function_set_special_handling"]
opaque setSpecialHandling (fn : @& ScalarFunction) : IO Unit

/-- Mark `fn` as volatile (its result may differ across calls with
    identical arguments), disabling common-subexpression elimination for
    calls to it. -/
@[extern "linen_duckdb_scalar_function_set_volatile"]
opaque setVolatile (fn : @& ScalarFunction) : IO Unit

/-- Add a parameter of logical type `ty` to `fn`'s signature. -/
@[extern "linen_duckdb_scalar_function_add_parameter"]
opaque addParameter (fn : @& ScalarFunction) (ty : @& LogicalType) : IO Unit

/-- Set `fn`'s return type. -/
@[extern "linen_duckdb_scalar_function_set_return_type"]
opaque setReturnType (fn : @& ScalarFunction) (ty : @& LogicalType) : IO Unit

/-! ── Installing the implementation ── -/

/-- Install `onCall` as `fn`'s native implementation, invoked by DuckDB for
    every batch of rows the function is called on: `onCall input output`
    reads argument columns from the borrowed `input` chunk and must write
    exactly `input`'s row count of result values into `output`. Combines
    `duckdb_scalar_function_set_extra_info` (to store the closure) and
    `duckdb_scalar_function_set_function` (to install the fixed
    trampoline) into one ergonomic call, mirroring
    `Database.DuckDB.FFI.Logging.register`'s combined `create`/
    `setWriteLogEntry`/`setExtraData` call. -/
@[extern "linen_duckdb_scalar_function_set_extra_info"]
opaque setOnCall (fn : @& ScalarFunction) (onCall : BorrowedDataChunk → Vector → IO Unit) : IO Unit

/-- Install the fixed native call trampoline on `fn` (see this module's
    doc comment). Must be called after `setOnCall`, and before `register`/
    `addToSet`. -/
@[extern "linen_duckdb_scalar_function_set_function"]
opaque setFunction (fn : @& ScalarFunction) : IO Unit

/-! ── Registration ── -/

/-- Register `fn` on `connection`. -/
@[extern "linen_duckdb_register_scalar_function_raw"]
opaque registerRaw (connection : @& Connection) (fn : @& ScalarFunction) : IO UInt32

/-- Register `fn` on `connection`. -/
def registerScalarFunction (connection : Connection) (fn : ScalarFunction) : IO State := do
  pure (State.ofUInt32 (← registerRaw connection fn))

/-- Build, install, and register a scalar function named `name` with
    parameter types `paramTypes`, return type `returnType`, and native
    implementation `onCall`, in one call. -/
def register (connection : Connection) (name : String) (paramTypes : Array LogicalType)
    (returnType : LogicalType) (onCall : BorrowedDataChunk → Vector → IO Unit) : IO State := do
  let fn ← create
  setName fn name
  for ty in paramTypes do
    addParameter fn ty
  setReturnType fn returnType
  setOnCall fn onCall
  setFunction fn
  registerScalarFunction connection fn

/-! ── Function-overload sets ── -/

/-- Create a new, empty named set of scalar-function overloads. Must
    eventually be destroyed with `destroySet` (or let its GC finalizer do
    so), unless registered via `registerSet` (which takes over ownership
    of the set, but not of the individual functions already added to it —
    see `addToSet`'s doc comment). -/
@[extern "linen_duckdb_create_scalar_function_set"]
opaque createSet (name : @& String) : IO ScalarFunctionSet

/-- Destroy `set`, deallocating all associated memory. -/
@[extern "linen_duckdb_destroy_scalar_function_set"]
opaque destroySet : ScalarFunctionSet → IO Unit

/-- Add `fn` (a fully-built overload with a matching name) to `set`. `set`
    takes ownership of `fn`: it must *not* also be `destroy`ed or
    separately `register`ed. -/
@[extern "linen_duckdb_add_scalar_function_to_set_raw"]
opaque addToSetRaw (set : @& ScalarFunctionSet) (fn : @& ScalarFunction) : IO UInt32

/-- Add `fn` (a fully-built overload with a matching name) to `set`. -/
def addToSet (set : ScalarFunctionSet) (fn : ScalarFunction) : IO State := do
  pure (State.ofUInt32 (← addToSetRaw set fn))

/-- Register all of `set`'s overloads on `connection`. -/
@[extern "linen_duckdb_register_scalar_function_set_raw"]
opaque registerSetRaw (connection : @& Connection) (set : @& ScalarFunctionSet) : IO UInt32

/-- Register all of `set`'s overloads on `connection`. -/
def registerSet (connection : Connection) (set : ScalarFunctionSet) : IO State := do
  pure (State.ofUInt32 (← registerSetRaw connection set))

/-! ── Reading the callback's borrowed input ── -/

/-- The number of rows in a scalar-function callback's borrowed input
    chunk. Not `Database.DuckDB.FFI.DataChunk.getSize`, since that is typed
    to the (owning) `DataChunk` handle, not `BorrowedDataChunk` — see this
    module's doc comment. -/
@[extern "linen_duckdb_scalar_function_input_get_size"]
opaque inputSize (input : @& BorrowedDataChunk) : IO Idx

/-- The number of columns in a scalar-function callback's borrowed input
    chunk. -/
@[extern "linen_duckdb_scalar_function_input_get_column_count"]
opaque inputColumnCount (input : @& BorrowedDataChunk) : IO Idx

/-- The argument column at `colIdx` in a scalar-function callback's
    borrowed input chunk. -/
@[extern "linen_duckdb_scalar_function_input_get_vector"]
opaque inputVector (input : @& BorrowedDataChunk) (colIdx : Idx) : IO Vector

end Database.DuckDB.FFI.ScalarFunctions
