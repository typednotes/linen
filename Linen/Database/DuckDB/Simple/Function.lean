/-
  Linen.Database.DuckDB.Simple.Function — user-defined scalar SQL functions

  Module #15 of `docs/imports/duckdb-simple/dependencies.md`, on #5
  (`Linen.Database.DuckDB.Simple.FromField`, for `FieldValue`), #1
  (`…Internal`, for `Connection`/`SQLError`/`withConnectionHandle`), #6
  (`…Materialize`, for decoding a call's borrowed input columns), #3
  (`…Ok`), plus `Linen.Database.DuckDB.FFI.ScalarFunctions`.

  ## Design

  `Linen.Database.DuckDB.FFI.ScalarFunctions` already solves the one
  genuinely hard part of this module — the Lean-closure-called-from-C
  trampoline (`linen_duckdb_scalar_function_call_trampoline`) that lets a
  Lean closure be installed as a `duckdb_scalar_function`'s native
  implementation (see that module's own doc comment for the full
  ownership/lifetime story, mirroring `Database.DuckDB.FFI.Logging`'s
  `LogStorage` trampoline pair). This module needs **no new C code**: it
  is a thin `Simple`-package ergonomic wrapper — `createFunction` combines
  `ScalarFunctions.register`'s own `create`/`setName`/`addParameter`*/
  `setReturnType`/`setOnCall`/`setFunction`/registration sequence with a
  row-oriented callback shape (`Array FieldValue → IO FieldValue`, one call
  per input row) built on top of `ScalarFunctions`'s own chunk-oriented
  `onCall : BorrowedDataChunk → Vector → IO Unit` callback, decoding each
  input column via `Materialize.materializeValue` and writing the result
  back via this module's own `writeFieldValue`.

  Per the fetched upstream source, `Database.DuckDB.Simple.Function`
  additionally provides `FunctionArg`/`FunctionResult` type classes and a
  recursive, arbitrary-arity `Function` class (`instance Function a`
  overlapping with `instance Function (a -> b)`, resolved only by GHC's
  overlapping-instance extensions) so a plain Haskell function of any
  arity can be registered directly. This is the exact "arity problem" the
  already-ported `Linen.Database.SQLite.Simple.Function`'s own module doc
  describes and resolves for `sqlite-simple`'s identically-shaped
  `Function` class: Lean's typeclass resolution has no notion of one
  instance head being more specific than another, so no direct port is
  possible. Rather than re-deriving that same fixed-arity-cutoff design a
  second time, this port instead exposes the row-oriented
  `Array FieldValue → IO FieldValue` shape directly — `createFunction`'s
  caller decodes its own fixed-arity arguments out of the `Array`
  (`args[0]!`, `args[1]!`, …) the same way `ScalarFunctions.register`'s own
  `onCall` already receives one `BorrowedDataChunk` covering every
  parameter column at once, rather than adding a second layer of
  `createFunction0`..`createFunctionN` wrappers on top.

  `createFunctionWithState` mirrors upstream's own name for the same
  registration but additionally threads a piece of caller-supplied,
  mutable `IO.Ref`-held state through every call — upstream needs a
  bespoke `StablePtr`-based mechanism for this; here it is simply a
  closure capturing an `IO.Ref`, since Lean closures already close over
  their environment.

  ### `writeFieldValue`'s scalar coverage

  Mirrors `Database.DuckDB.Simple.Copy.appendFieldValue`'s identical
  documented gap: `Linen.Database.DuckDB.FFI.Vector` exposes direct
  setters only for `Bool`/`Int32`/`Int64`/`Float`/raw bytes/string
  elements — nothing for `LIST`/`ARRAY`/`MAP`/`STRUCT`/`UNION`/`ENUM`/
  `UUID`/`DECIMAL`/`BIT` (those would need the same unported
  `FieldValue → Value` boxed-value encoder `Copy`'s own module doc already
  flags as out of scope). `writeFieldValue` covers every other
  `FieldValue` constructor (including the fixed-width temporal/`HugeInt`/
  `UHugeInt`/`Interval` structs, written via `Vector.setDataBytes` the same
  way `Materialize.materializeValueFuel` reads them back) and reports the
  rest as an explicit `IO` error.

  `deleteFunction` runs `DROP FUNCTION IF EXISTS <name>` directly through
  `Linen.Database.DuckDB.FFI.QueryExecution.query` — DuckDB's own
  documented limitation (a scalar function registered through the C API
  cannot actually be dropped this way) applies here exactly as it does
  upstream; this port makes no attempt to work around it.

  ## Haskell source
  - `Database.DuckDB.Simple.Function` (`duckdb-simple` package, version
    0.1.5.1) — consulted for scope/intent; the arity-polymorphic `Function`
    class is not portable byte-for-byte per the deviation note above.
-/
import Linen.Database.DuckDB.Simple.FromField
import Linen.Database.DuckDB.Simple.Internal
import Linen.Database.DuckDB.Simple.Materialize
import Linen.Database.DuckDB.Simple.Ok
import Linen.Database.DuckDB.FFI.ScalarFunctions

namespace Database.DuckDB.Simple.Function

open Database.DuckDB.FFI.Types (Idx LogicalType BorrowedDataChunk Vector State)
open Database.DuckDB.Simple (Connection SQLError FieldValue throwSQLError registrationError
  withConnectionHandle)

-- ────────────────────────────────────────────────────────────────────
-- Writing a `FieldValue` result into the output vector
-- ────────────────────────────────────────────────────────────────────

private def bytesOfU8 (b : UInt8) : ByteArray := ⟨#[b]⟩
private def bytesOfI8 (i : Int8) : ByteArray := bytesOfU8 i.toUInt8

private def bytesOfU16LE (w : UInt16) : ByteArray := ⟨#[w.toUInt8, (w >>> 8).toUInt8]⟩
private def bytesOfI16LE (i : Int16) : ByteArray := bytesOfU16LE i.toUInt16

private def bytesOfU32LE (w : UInt32) : ByteArray :=
  ⟨#[w.toUInt8, (w >>> 8).toUInt8, (w >>> 16).toUInt8, (w >>> 24).toUInt8]⟩
private def bytesOfI32LE (i : Int32) : ByteArray := bytesOfU32LE i.toUInt32

private def bytesOfU64LE (w : UInt64) : ByteArray :=
  ⟨#[w.toUInt8, (w >>> 8).toUInt8, (w >>> 16).toUInt8, (w >>> 24).toUInt8,
     (w >>> 32).toUInt8, (w >>> 40).toUInt8, (w >>> 48).toUInt8, (w >>> 56).toUInt8]⟩
private def bytesOfI64LE (i : Int64) : ByteArray := bytesOfU64LE i.toUInt64

/-- Write `value` at row `idx` of `output`, dispatching to the matching
    `Linen.Database.DuckDB.FFI.Vector` setter. Throws an `IO` error for a
    `FieldValue` constructor `Vector` has no direct setter for (see the
    module doc). -/
def writeFieldValue (output : Vector) (idx : Idx) (value : FieldValue) : IO Unit := do
  match value with
  | .null => do
    Database.DuckDB.FFI.Vector.ensureValidityWritable output
    match ← Database.DuckDB.FFI.Vector.getValidity output with
    | some validity => Database.DuckDB.FFI.Validity.setRowInvalid validity idx
    | none => pure ()
  | .boolean b => Database.DuckDB.FFI.Vector.setBool output idx b
  | .int8 i => Database.DuckDB.FFI.Vector.setDataBytes output idx (bytesOfI8 i)
  | .int16 i => Database.DuckDB.FFI.Vector.setDataBytes output (idx * 2) (bytesOfI16LE i)
  | .int32 i => Database.DuckDB.FFI.Vector.setInt32 output idx i
  | .int64 i => Database.DuckDB.FFI.Vector.setInt64 output idx i
  | .uint8 i => Database.DuckDB.FFI.Vector.setDataBytes output idx (bytesOfU8 i)
  | .uint16 i => Database.DuckDB.FFI.Vector.setDataBytes output (idx * 2) (bytesOfU16LE i)
  | .uint32 i => Database.DuckDB.FFI.Vector.setDataBytes output (idx * 4) (bytesOfU32LE i)
  | .uint64 i => Database.DuckDB.FFI.Vector.setDataBytes output (idx * 8) (bytesOfU64LE i)
  | .hugeInt i =>
    Database.DuckDB.FFI.Vector.setDataBytes output (idx * 16)
      ⟨(bytesOfU64LE i.lower).data ++ (bytesOfI64LE i.upper).data⟩
  | .uHugeInt i =>
    Database.DuckDB.FFI.Vector.setDataBytes output (idx * 16)
      ⟨(bytesOfU64LE i.lower).data ++ (bytesOfU64LE i.upper).data⟩
  | .float f => Database.DuckDB.FFI.Vector.setDataBytes output (idx * 4) (bytesOfU32LE f.toBits)
  | .double f => Database.DuckDB.FFI.Vector.setDouble output idx f
  | .varchar s => Database.DuckDB.FFI.Vector.assignStringElement output idx s
  | .blob b => Database.DuckDB.FFI.Vector.assignStringElementLen output idx b
  | .date d => Database.DuckDB.FFI.Vector.setInt32 output idx d.days
  | .time t => Database.DuckDB.FFI.Vector.setInt64 output idx t.micros
  | .timestamp t => Database.DuckDB.FFI.Vector.setInt64 output idx t.micros
  | .interval i =>
    Database.DuckDB.FFI.Vector.setDataBytes output (idx * 16)
      ⟨(bytesOfI32LE i.months).data ++ (bytesOfI32LE i.days).data ++ (bytesOfI64LE i.micros).data⟩
  | other =>
    throw (IO.userError
      s!"Function.writeFieldValue: Vector has no per-cell setter for {other.typeName} \
         values (see the module doc)")

-- ────────────────────────────────────────────────────────────────────
-- Registration
-- ────────────────────────────────────────────────────────────────────

/-- Decode row `idx` of every column in `input` (`Materialize.materializeValue`
    on each), one `FieldValue` per parameter. -/
private def decodeArgs (input : BorrowedDataChunk) (idx : Idx) : IO (Array FieldValue) := do
  let colCount ← Database.DuckDB.FFI.ScalarFunctions.inputColumnCount input
  let mut args : Array FieldValue := #[]
  for c in [0:colCount.toNat] do
    let colVec ← Database.DuckDB.FFI.ScalarFunctions.inputVector input (UInt64.ofNat c)
    args := args.push (← Database.DuckDB.Simple.Materialize.materializeValue colVec idx)
  pure args

/-- Register a scalar SQL function named `name`, with parameter types
    `paramTypes` and return type `returnType`, whose implementation is
    `f`. `f` is called once per input row with that row's decoded
    parameter values, in column order; any exception it throws propagates
    unhandled out of `onCall` (silently swallowed by
    `Linen.Database.DuckDB.FFI.ScalarFunctions`'s own trampoline — see that
    module's doc comment — rather than surfacing as a SQL-level error, no
    in-band error channel being in scope for this batch). -/
def createFunction (connection : Connection) (name : String) (paramTypes : Array LogicalType)
    (returnType : LogicalType) (f : Array FieldValue → IO FieldValue) : IO Unit :=
  withConnectionHandle connection fun connHandle => do
    let state ←
      Database.DuckDB.FFI.ScalarFunctions.register connHandle name paramTypes returnType
        fun input output => do
          let n ← Database.DuckDB.FFI.ScalarFunctions.inputSize input
          for i in [0:n.toNat] do
            let idx := UInt64.ofNat i
            let args ← decodeArgs input idx
            let result ← f args
            writeFieldValue output idx result
    match state with
    | .success => pure ()
    | .error => throwSQLError (registrationError s!"register scalar function \"{name}\"")

/-- Like `createFunction`, but `f` additionally receives a shared, mutable
    `state : IO.Ref σ` threaded across every call (upstream's
    `createFunctionWithState`; see the module doc for why this needs no
    `StablePtr`-based mechanism here). -/
def createFunctionWithState (connection : Connection) (name : String)
    (paramTypes : Array LogicalType) (returnType : LogicalType) (initial : σ)
    (f : IO.Ref σ → Array FieldValue → IO FieldValue) : IO Unit := do
  let state ← IO.mkRef initial
  createFunction connection name paramTypes returnType (f state)

/-- Remove the scalar function named `name` (`DROP FUNCTION IF EXISTS
    <name>`), run directly through
    `Linen.Database.DuckDB.FFI.QueryExecution.query` (see the module doc
    for DuckDB's own documented limitation on dropping a C-API-registered
    function). -/
def deleteFunction (connection : Connection) (name : String) : IO Unit :=
  withConnectionHandle connection fun connHandle => do
    let (state, result) ←
      Database.DuckDB.FFI.QueryExecution.query connHandle s!"DROP FUNCTION IF EXISTS {name}"
    try
      match state with
      | .success => pure ()
      | .error => throwSQLError (registrationError s!"delete scalar function \"{name}\"")
    finally
      Database.DuckDB.FFI.QueryExecution.destroy result

end Database.DuckDB.Simple.Function
