/-
  Tests for `Linen.Database.SQL.Decoders`.

  `Value` decoders are pure (`Option String → Except String α`) and `Row`
  widths are pure `Nat`s, so both are checked with `#guard`. `Row.decode` and
  the `Result` decoders read a live `PgResult` over FFI, so those are pinned at
  the type level only.
-/
import Linen.Database.SQL.Decoders

open Database.PostgreSQL.LibPQ
open Database.SQL.Session
open Database.SQL.Decoders

namespace Tests.Database.SQL.Decoders

-- Core has no `BEq (Except ε α)`; this local instance lets us compare decoder
-- outcomes (`.ok`/`.error`) directly in `#guard`.
local instance [BEq ε] [BEq α] : BEq (Except ε α) where
  beq
    | .ok a, .ok b => a == b
    | .error a, .error b => a == b
    | _, _ => false

/-! ### Value decoders -/

#guard Value.text.decode (some "hi") == .ok "hi"
#guard Value.text.decode none == .error "unexpected NULL for text column"
#guard Value.int.decode (some "42") == .ok 42
#guard Value.int.decode (some "-7") == .ok (-7)
#guard Value.int.decode (some "x") == .error "invalid integer: x"
#guard Value.int.decode none == .error "unexpected NULL for integer column"
#guard Value.nat.decode (some "9") == .ok 9
#guard Value.nat.decode (some "-1") == .error "invalid natural: -1"

/-! ### Value.bool — accepts t/true/1 and f/false/0 -/

#guard Value.bool.decode (some "t") == .ok true
#guard Value.bool.decode (some "true") == .ok true
#guard Value.bool.decode (some "1") == .ok true
#guard Value.bool.decode (some "f") == .ok false
#guard Value.bool.decode (some "false") == .ok false
#guard Value.bool.decode (some "0") == .ok false
#guard Value.bool.decode (some "maybe") == .error "invalid boolean: maybe"

/-! ### Value.float — hand-rolled parser (exactly-representable literals) -/

#guard Value.float.decode (some "3.5") == .ok 3.5
#guard Value.float.decode (some "-2.0") == .ok (-2.0)
#guard Value.float.decode (some "10") == .ok 10.0
#guard Value.float.decode (some "1.5e2") == .ok 150.0
#guard Value.float.decode (some "abc") == .error "invalid float: abc"

/-! ### Value.nullable / map / rawText -/

#guard (Value.nullable Value.int).decode none == .ok none
#guard (Value.nullable Value.int).decode (some "5") == .ok (some 5)
#guard (Value.nullable Value.int).decode (some "x") == .error "invalid integer: x"
#guard (Value.map (· + 1) Value.int).decode (some "5") == .ok 6
#guard Value.rawText.decode none == .ok none
#guard Value.rawText.decode (some "raw") == .ok (some "raw")

/-! ### Row — widths are pure -/

#guard (Row.column Value.int).width == 1
#guard (Row.seq (Row.column Value.int) (Row.column Value.text)).width == 2
#guard (Row.map (· + 1) (Row.column Value.int)).width == 1
#guard (Row.pair Value.int Value.text).width == 2
#guard (Row.triple Value.int Value.text Value.bool).width == 3

-- width laws (compile-time)
example (v : Value α) : (Row.column v).width = 1 := Row.column_width v
example (a : Value α) (b : Value β) : (Row.pair a b).width = 2 := Row.pair_width a b

-- Row.decode reads a PgResult over FFI — signature only.
example : Row Int := Row.column Value.int
example : Row (Int × String) := Row.pair Value.int Value.text

/-! ### Result decoders — signatures (need a live PgResult) -/

example {α} : Row α → Result (List α) := Result.rowList
example {α} : Row α → Result (Array α) := Result.rowArray
example {α} : Row α → Result α := Result.singleRow
example {α} : Row α → Result (Option α) := Result.maybeRow
example : Result Nat := Result.rowsAffected
example : Result Unit := Result.unit
example {α β} : (α → β) → Result α → Result β := Result.map

end Tests.Database.SQL.Decoders
