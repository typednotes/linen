/-
  Tests for `Linen.Database.DuckDB.FFI.Helpers`.

  Exercises every one of upstream's 26 entry points directly (no
  `Database`/`Connection` needed — every function in this module is
  self-contained), grouped the same way the module itself is: raw
  memory, UTF-8 validation, `duckdb_string_t` packing/accessors,
  date/time/timestamp conversions and finiteness checks, and
  hugeint/uhugeint/decimal <-> double conversions.
-/
import Linen.Database.DuckDB.FFI.Helpers

open Database.DuckDB.FFI.Helpers
open Database.DuckDB.FFI.Types

namespace Tests.Database.DuckDB.FFI.Helpers

/-! ── Raw memory ── -/

#eval show IO Unit from do
  let mem ← malloc 16
  free mem
  free mem -- idempotent

/-! ── UTF-8 validation ── -/

#eval show IO Unit from do
  let validErr ← validUtf8Check (String.toUTF8 "hello, world")
  match validErr with
  | none => pure ()
  | some _ => throw (IO.userError "expected valid UTF-8 to report no error")
  -- `0xFF` alone is never valid UTF-8.
  let invalidErr ← validUtf8Check (ByteArray.mk #[0xFF])
  match invalidErr with
  | some _ => pure ()
  | none => throw (IO.userError "expected invalid UTF-8 to report an error")

  let n ← vectorSize
  unless n > 0 do throw (IO.userError s!"expected a positive vectorSize, got {n}")

/-! ── `duckdb_string_t` packing/accessors ── -/

#eval show IO Unit from do
  match mkInlinedStringT "hello" with
  | .error msg => throw (IO.userError s!"mkInlinedStringT failed: {msg}")
  | .ok bytes =>
    unless bytes.size == 16 do throw (IO.userError s!"expected a 16-byte image, got {bytes.size}")
    let inlined ← stringIsInlined bytes
    unless inlined do throw (IO.userError "expected a short string to be inlined")
    let len ← stringTLength bytes
    unless len == 5 do throw (IO.userError s!"unexpected stringTLength: {len}")
    let data ← stringTData bytes
    unless data == "hello" do throw (IO.userError s!"unexpected stringTData: {data}")
  match mkInlinedStringT (String.ofList (List.replicate 13 'x')) with
  | .error _ => pure () -- 13 bytes exceeds the 12-byte inlined budget
  | .ok _ => throw (IO.userError "expected mkInlinedStringT to reject a 13-byte string")

/-! ── Date/time/timestamp conversions ── -/

#eval show IO Unit from do
  -- 2024-01-01 (`19723` days after the 1970-01-01 epoch).
  let date : Date := ⟨19723⟩
  let dateStruct ← fromDate date
  unless dateStruct == ({ year := 2024, month := 1, day := 1 } : DateStruct) do
    throw (IO.userError s!"unexpected fromDate: {repr dateStruct}")
  let date' ← toDate dateStruct
  unless date' == date do throw (IO.userError s!"unexpected toDate: {repr date'}")
  let finite ← isFiniteDate date.days
  unless finite do throw (IO.userError "expected 2024-01-01 to be finite")

  -- 01:02:03.000004.
  let time : Time := ⟨1 * 3600000000 + 2 * 60000000 + 3 * 1000000 + 4⟩
  let timeStruct ← fromTime time
  unless timeStruct == ({ hour := 1, min := 2, sec := 3, micros := 4 } : TimeStruct) do
    throw (IO.userError s!"unexpected fromTime: {repr timeStruct}")
  let time' ← toTime timeStruct
  unless time' == time do throw (IO.userError s!"unexpected toTime: {repr time'}")

  let tz ← createTimeTz time.micros 3600
  let tzStruct ← fromTimeTz tz
  unless tzStruct == ({ time := timeStruct, offset := 3600 } : TimeTzStruct) do
    throw (IO.userError s!"unexpected fromTimeTz: {repr tzStruct}")

  let ts : Timestamp := ⟨1_700_000_000_000_000⟩
  let tsStruct ← fromTimestamp ts
  let ts' ← toTimestamp tsStruct
  unless ts' == ts do throw (IO.userError s!"unexpected timestamp round trip: {repr ts'}")
  let tsFinite ← isFiniteTimestamp ts.micros
  unless tsFinite do throw (IO.userError "expected a normal timestamp to be finite")
  let tsSecFinite ← isFiniteTimestampSeconds (ts.micros / 1_000_000)
  unless tsSecFinite do throw (IO.userError "expected isFiniteTimestampSeconds to be true")
  let tsMillisFinite ← isFiniteTimestampMillis (ts.micros / 1_000)
  unless tsMillisFinite do throw (IO.userError "expected isFiniteTimestampMillis to be true")
  let tsNanosFinite ← isFiniteTimestampNanos (ts.micros * 1_000)
  unless tsNanosFinite do throw (IO.userError "expected isFiniteTimestampNanos to be true")

/-! ── Hugeint/uhugeint/decimal <-> double ── -/

#eval show IO Unit from do
  let d ← hugeIntToDouble ⟨100, 0⟩
  unless d == 100.0 do throw (IO.userError s!"unexpected hugeIntToDouble: {d}")
  let h ← doubleToHugeInt 100.0
  unless h == (⟨100, 0⟩ : HugeInt) do throw (IO.userError s!"unexpected doubleToHugeInt: {repr h}")

  let ud ← uHugeIntToDouble ⟨100, 0⟩
  unless ud == 100.0 do throw (IO.userError s!"unexpected uHugeIntToDouble: {ud}")
  let uh ← doubleToUHugeInt 100.0
  unless uh == (⟨100, 0⟩ : UHugeInt) do throw (IO.userError s!"unexpected doubleToUHugeInt: {repr uh}")

  let dec ← doubleToDecimal 12.5 4 1
  unless dec.width == 4 && dec.scale == 1 do throw (IO.userError s!"unexpected decimal shape: {repr dec}")
  let decD ← decimalToDouble dec
  unless decD == 12.5 do throw (IO.userError s!"unexpected decimalToDouble: {decD}")

end Tests.Database.DuckDB.FFI.Helpers
