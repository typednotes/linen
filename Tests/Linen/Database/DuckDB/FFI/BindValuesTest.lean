/-
  Tests for `Linen.Database.DuckDB.FFI.BindValues`.

  Prepares a single statement with one *named* parameter per DuckDB physical
  type this module's `bind*` family covers, uses `bindParameterIndex` to
  resolve each name to its (1-indexed) position — exercising that lookup
  extensively, since nothing else in this batch does — then binds each with
  its matching `bind*` function. `bindValue` is exercised too, with a real
  boxed `Value` borrowed from `Database.DuckDB.FFI.OpenConnect.getTableNames`
  (this batch's only `Value` producer, `Database.DuckDB.FFI.ValueInterface`
  being out of scope). Since `Database.DuckDB.FFI.PreparedStatements`/
  `ExecutePrepared` aren't ported either, the returned `duckdb_state` from
  each bind call is this test's only correctness signal — the same
  proxy `AppenderTest`/`ConfigurationTest` use for their own bare-state
  calls.
-/
import Linen.Database.DuckDB.FFI.BindValues
import Linen.Database.DuckDB.FFI.OpenConnect
import Tests.Linen.Database.DuckDB.FFI.TestSupport

open Database.DuckDB.FFI.BindValues
open Database.DuckDB.FFI.OpenConnect
open Database.DuckDB.FFI.Types
open Tests.Database.DuckDB.FFI.TestSupport

namespace Tests.Database.DuckDB.FFI.BindValues

def paramQuery : String :=
  "SELECT $bo::BOOLEAN, $i1::TINYINT, $i2::SMALLINT, $i3::INTEGER, $i4::BIGINT,
          $i5::HUGEINT, $u1::UTINYINT, $u2::USMALLINT, $u3::UINTEGER, $u4::UBIGINT,
          $u5::UHUGEINT, $de::DECIMAL(10,2), $fl::FLOAT, $do::DOUBLE, $dt::DATE,
          $tm::TIME, $ts::TIMESTAMP, $tstz::TIMESTAMPTZ, $iv::INTERVAL,
          $vc::VARCHAR, $vc2::VARCHAR, $bl::BLOB, $nu::INTEGER, $anyv::VARCHAR[]"

/-- Resolve `name`'s (1-indexed) parameter position in `stmt`, failing loudly
    if it isn't found (every name in `paramQuery` above must resolve). -/
def idxOf (stmt : PreparedStatement) (name : String) : IO Idx := do
  let result ← bindParameterIndex stmt name
  match result with
  | .ok idx => pure idx
  | .error msg => throw (IO.userError msg)

#eval show IO Unit from do
  let dbResult ← openDatabase none
  let db ← match dbResult with
    | .ok db => pure db
    | .error msg => throw (IO.userError s!"duckdb_open failed: {msg}")

  let connResult ← connect db
  let conn ← match connResult with
    | .ok conn => pure conn
    | .error msg => throw (IO.userError s!"duckdb_connect failed: {msg}")

  let prepResult ← prepareForTest conn paramQuery
  let stmt ← match prepResult with
    | .ok stmt => pure stmt
    | .error msg => throw (IO.userError msg)

  -- An unknown named parameter must fail to resolve.
  let bogusIdx ← bindParameterIndex stmt "definitely_not_a_param"
  match bogusIdx with
  | .ok _ => throw (IO.userError "expected bindParameterIndex to fail for an unknown name")
  | .error _ => pure ()

  let mut results : Array (String × State) := #[]

  let boIdx ← idxOf stmt "bo"
  results := results.push ("boolean", ← bindBoolean stmt boIdx true)

  let i1Idx ← idxOf stmt "i1"
  results := results.push ("int8", ← bindInt8 stmt i1Idx (-8))
  let i2Idx ← idxOf stmt "i2"
  results := results.push ("int16", ← bindInt16 stmt i2Idx (-16))
  let i3Idx ← idxOf stmt "i3"
  results := results.push ("int32", ← bindInt32 stmt i3Idx (-32))
  let i4Idx ← idxOf stmt "i4"
  results := results.push ("int64", ← bindInt64 stmt i4Idx (-64))
  let i5Idx ← idxOf stmt "i5"
  results := results.push ("hugeint", ← bindHugeInt stmt i5Idx ⟨1, 2⟩)

  let u1Idx ← idxOf stmt "u1"
  results := results.push ("uint8", ← bindUInt8 stmt u1Idx 8)
  let u2Idx ← idxOf stmt "u2"
  results := results.push ("uint16", ← bindUInt16 stmt u2Idx 16)
  let u3Idx ← idxOf stmt "u3"
  results := results.push ("uint32", ← bindUInt32 stmt u3Idx 32)
  let u4Idx ← idxOf stmt "u4"
  results := results.push ("uint64", ← bindUInt64 stmt u4Idx 64)
  let u5Idx ← idxOf stmt "u5"
  results := results.push ("uhugeint", ← bindUHugeInt stmt u5Idx ⟨1, 2⟩)

  let deIdx ← idxOf stmt "de"
  results := results.push ("decimal", ← bindDecimal stmt deIdx ⟨10, 2, ⟨1234, 0⟩⟩)

  let flIdx ← idxOf stmt "fl"
  results := results.push ("float", ← bindFloat stmt flIdx 1.5)
  let doIdx ← idxOf stmt "do"
  results := results.push ("double", ← bindDouble stmt doIdx 2.5)

  let dtIdx ← idxOf stmt "dt"
  results := results.push ("date", ← bindDate stmt dtIdx ⟨19723⟩)
  let tmIdx ← idxOf stmt "tm"
  results := results.push ("time", ← bindTime stmt tmIdx ⟨3600000000⟩)
  let tsIdx ← idxOf stmt "ts"
  results := results.push ("timestamp", ← bindTimestamp stmt tsIdx ⟨1_700_000_000_000_000⟩)
  let tstzIdx ← idxOf stmt "tstz"
  results := results.push ("timestampTz", ← bindTimestampTz stmt tstzIdx ⟨1_700_000_000_000_000⟩)
  let ivIdx ← idxOf stmt "iv"
  results := results.push ("interval", ← bindInterval stmt ivIdx ⟨1, 2, 3⟩)

  let vcIdx ← idxOf stmt "vc"
  results := results.push ("varchar", ← bindVarchar stmt vcIdx "hello")
  let vc2Idx ← idxOf stmt "vc2"
  results := results.push ("varcharLength", ← bindVarcharLength stmt vc2Idx "world")
  let blIdx ← idxOf stmt "bl"
  results := results.push ("blob", ← bindBlob stmt blIdx (String.toUTF8 "blob-bytes"))

  let nuIdx ← idxOf stmt "nu"
  results := results.push ("null", ← bindNull stmt nuIdx)

  let anyvIdx ← idxOf stmt "anyv"
  let sampleValue ← getTableNames conn "SELECT 1" false
  results := results.push ("value", ← bindValue stmt anyvIdx sampleValue)

  for (label, state) in results do
    unless state.isSuccess do throw (IO.userError s!"bind{label} failed")

  -- An out-of-range positional index must fail.
  let oobState ← bindNull stmt 9999
  if oobState.isSuccess then
    throw (IO.userError "expected bindNull to fail for an out-of-range index")

  destroyPreparedForTest stmt
  destroyPreparedForTest stmt -- idempotent

  disconnect conn
  close db

end Tests.Database.DuckDB.FFI.BindValues
