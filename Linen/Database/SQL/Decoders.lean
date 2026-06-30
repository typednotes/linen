/-
  Linen.Database.SQL.Decoders — Result set decoders

  Composable decoders for transforming PostgreSQL result sets (`PgResult`)
  into typed Lean values.  Decoders are split into three levels:

  1. **Value** decoders: decode a single column value (`Option String → Except String α`)
  2. **Row** decoders: decode an entire row by combining value decoders
  3. **Result** decoders: decode the entire result set (single row, list, etc.)

  ## Haskell source
  - `Hasql.Decoders` (hasql package)
-/

import Linen.Database.SQL.Session
import Linen.Database.PostgreSQL.LibPQ

namespace Database.SQL.Decoders

open Database.PostgreSQL.LibPQ
open Database.SQL.Session

-- ════════════════════════════════════════════════════════════════════
-- Helpers
-- ════════════════════════════════════════════════════════════════════

private def charToDigit (c : Char) : Nat :=
  c.toNat - '0'.toNat

/-- Parse a string as a `Float`. Handles optional sign, integer part,
    optional decimal part, and optional exponent (`e`/`E`).
    Returns `none` if the string is not a valid float literal. -/
private def parseFloat? (s : String) : Option Float :=
  let chars := s.trimAscii.toString.toList
  if chars.isEmpty then none
  else
    let (negative, rest) := match chars with
      | '-' :: cs => (true, cs)
      | '+' :: cs => (false, cs)
      | cs => (false, cs)
    if rest.isEmpty then none
    else
      let (intDigits, afterInt) := rest.span Char.isDigit
      let (fracDigits, afterFrac) := match afterInt with
        | '.' :: cs => let (fd, r) := cs.span Char.isDigit; (fd, r)
        | cs => ([], cs)
      if intDigits.isEmpty && fracDigits.isEmpty then none
      else
        let parseExp := match afterFrac with
          | 'e' :: rest' | 'E' :: rest' =>
            let (en, ed) := match rest' with
              | '-' :: ds => (true, ds)
              | '+' :: ds => (false, ds)
              | ds => (false, ds)
            let (expDigits, trailing) := ed.span Char.isDigit
            if !trailing.isEmpty || expDigits.isEmpty then none
            else some (en, expDigits.foldl (fun acc c => acc * 10 + charToDigit c) 0)
          | [] => some (false, 0)
          | _ => none
        match parseExp with
        | none => none
        | some (expNeg, ev) =>
          let mantissa := (intDigits ++ fracDigits).foldl (fun acc c => acc * 10 + charToDigit c) 0
          let fracLen := fracDigits.length
          let netExp : Int := (if expNeg then -(ev : Int) else (ev : Int)) - (fracLen : Int)
          let f := if netExp >= 0 then
            Float.ofScientific mantissa false netExp.toNat
          else
            Float.ofScientific mantissa true (-netExp).toNat
          some (if negative then -f else f)

-- ════════════════════════════════════════════════════════════════════
-- Value decoders
-- ════════════════════════════════════════════════════════════════════

/-- A value decoder converts a nullable string column value to a typed result. -/
structure Value (α : Type) where
  decode : Option String → Except String α

namespace Value

/-- Decode a non-nullable text value. -/
def text : Value String :=
  { decode := fun
    | some s => .ok s
    | none => .error "unexpected NULL for text column" }

/-- Decode a non-nullable integer value. -/
def int : Value Int :=
  { decode := fun
    | some s => match s.toInt? with
      | some n => .ok n
      | none => .error s!"invalid integer: {s}"
    | none => .error "unexpected NULL for integer column" }

/-- Decode a non-nullable natural number. -/
def nat : Value Nat :=
  { decode := fun
    | some s => match s.toNat? with
      | some n => .ok n
      | none => .error s!"invalid natural: {s}"
    | none => .error "unexpected NULL for natural column" }

/-- Decode a non-nullable float value. -/
def float : Value Float :=
  { decode := fun
    | some s => match parseFloat? s with
      | some f => .ok f
      | none => .error s!"invalid float: {s}"
    | none => .error "unexpected NULL for float column" }

/-- Decode a non-nullable boolean value. -/
def bool : Value Bool :=
  { decode := fun
    | some "t" | some "true" | some "1" => .ok true
    | some "f" | some "false" | some "0" => .ok false
    | some s => .error s!"invalid boolean: {s}"
    | none => .error "unexpected NULL for boolean column" }

/-- Decode a nullable value. Produces `Option α`. -/
def nullable (inner : Value α) : Value (Option α) :=
  { decode := fun
    | none => .ok none
    | some s => match inner.decode (some s) with
      | .ok a => .ok (some a)
      | .error e => .error e }

/-- Map the output of a value decoder. -/
def map (f : α → β) (v : Value α) : Value β :=
  { decode := fun s => (v.decode s).map f }

/-- Decode any value as raw text (nullable). -/
def rawText : Value (Option String) :=
  { decode := fun s => .ok s }

end Value

-- ════════════════════════════════════════════════════════════════════
-- Row decoders
-- ════════════════════════════════════════════════════════════════════

/-- A row decoder reads values from columns left-to-right, tracking
    the current column index. -/
structure Row (α : Type) where
  /-- Number of columns consumed. -/
  width : Nat
  /-- Decode a row from the result at the given row index, starting at the given column. -/
  decode : PgResult → UInt32 → UInt32 → IO (Except String α)

namespace Row

/-- Decode a single column using a value decoder. -/
def column (v : Value α) : Row α :=
  { width := 1
    decode := fun result rowIdx colIdx => do
      let isNull ← getIsNull result rowIdx colIdx
      let raw ← if isNull then Pure.pure none else some <$> getvalue result rowIdx colIdx
      return v.decode raw }

/-- Combine two row decoders sequentially (decode left columns, then right columns). -/
def seq (ra : Row α) (rb : Row β) : Row (α × β) :=
  { width := ra.width + rb.width
    decode := fun result rowIdx colIdx => do
      match ← ra.decode result rowIdx colIdx with
      | .error e => return .error e
      | .ok a =>
        match ← rb.decode result rowIdx (colIdx + ra.width.toUInt32) with
        | .error e => return .error e
        | .ok b => return .ok (a, b) }

/-- Map the result of a row decoder. -/
def map (f : α → β) (r : Row α) : Row β :=
  { width := r.width
    decode := fun result rowIdx colIdx => do
      match ← r.decode result rowIdx colIdx with
      | .ok a => return .ok (f a)
      | .error e => return .error e }

instance : Functor Row where
  map := Row.map

/-- Convenience: decode two columns into a pair. -/
def pair (a : Value α) (b : Value β) : Row (α × β) :=
  seq (column a) (column b)

/-- Convenience: decode three columns into a triple. -/
def triple (a : Value α) (b : Value β) (c : Value γ) : Row (α × β × γ) :=
  map (fun ((x, y), z) => (x, y, z)) (seq (seq (column a) (column b)) (column c))

-- ────────────────────────────────────────────────────────────────────
-- Row width theorems
-- ────────────────────────────────────────────────────────────────────

/-- `column` always consumes exactly 1 column. -/
theorem column_width (v : Value α) : (column v).width = 1 := rfl

/-- `seq` width is the sum of its constituents' widths. -/
theorem seq_width (ra : Row α) (rb : Row β) : (seq ra rb).width = ra.width + rb.width := rfl

/-- `map` preserves width. -/
theorem map_width (f : α → β) (r : Row α) : (Row.map f r).width = r.width := rfl

/-- `pair` width is always 2. -/
theorem pair_width (a : Value α) (b : Value β) : (pair a b).width = 2 := rfl

/-- `triple` width is always 3. -/
theorem triple_width (a : Value α) (b : Value β) (c : Value γ) : (triple a b c).width = 3 := rfl

end Row

-- ════════════════════════════════════════════════════════════════════
-- Result decoders
-- ════════════════════════════════════════════════════════════════════

/-- A result decoder transforms a complete `PgResult` into a typed value. -/
def Result (α : Type) := PgResult → IO (Except SessionError α)

namespace Result

/-- Decode result rows as a list. -/
def rowList (row : Row α) : Result (List α) := fun pgResult => do
  let nRows ← ntuples pgResult
  let mut results : List α := []
  for i in List.range nRows.toNat do
    match ← row.decode pgResult i.toUInt32 0 with
    | .ok a => results := results ++ [a]
    | .error e => return .error (.resultError e)
  return .ok results

/-- Decode result rows as an array. -/
def rowArray (row : Row α) : Result (Array α) := fun pgResult => do
  let nRows ← ntuples pgResult
  let mut results : Array α := Array.mkEmpty nRows.toNat
  for i in List.range nRows.toNat do
    match ← row.decode pgResult i.toUInt32 0 with
    | .ok a => results := results.push a
    | .error e => return .error (.resultError e)
  return .ok results

/-- Decode exactly one row.  Fails if the result has zero or more than one row. -/
def singleRow (row : Row α) : Result α := fun pgResult => do
  let nRows ← ntuples pgResult
  if nRows == 1 then
    match ← row.decode pgResult 0 0 with
    | .ok a => return .ok a
    | .error e => return .error (.resultError e)
  else
    return .error (.resultError s!"expected exactly 1 row, got {nRows}")

/-- Decode zero or one rows.  Fails if the result has more than one row. -/
def maybeRow (row : Row α) : Result (Option α) := fun pgResult => do
  let nRows ← ntuples pgResult
  if nRows == 0 then
    return .ok none
  else if nRows == 1 then
    match ← row.decode pgResult 0 0 with
    | .ok a => return .ok (some a)
    | .error e => return .error (.resultError e)
  else
    return .error (.resultError s!"expected 0 or 1 rows, got {nRows}")

/-- Decode the number of affected rows (for INSERT/UPDATE/DELETE). -/
def rowsAffected : Result Nat := fun pgResult => do
  let s ← cmdTuples pgResult
  match s.toNat? with
  | some n => return .ok n
  | none => return .ok 0  -- empty string means 0

/-- Ignore the result entirely. -/
def unit : Result Unit := fun _ => return .ok ()

/-- Map the result. -/
def map (f : α → β) (r : Result α) : Result β := fun pgResult => do
  match ← r pgResult with
  | .ok a => return .ok (f a)
  | .error e => return .error e

end Result

end Database.SQL.Decoders
