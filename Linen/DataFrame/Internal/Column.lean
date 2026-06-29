/-
  Linen.DataFrame.Internal.Column — column-level operations

  Construction, type inference, and element-wise transforms on individual
  `Column`s (no DataFrame rectangular invariant needed). All functions are pure
  and total.
-/

import Linen.DataFrame.Internal.Types

namespace DataFrame
namespace Column

/-- Infer the predominant `ColumnType`: the common variant of all non-null
    values, else `mixed` (an all-null or empty array is `mixed`). -/
def inferType (values : Array Value) : ColumnType :=
  let nonNull := values.filter (· != Value.null)
  if nonNull.size == 0 then .mixed
  else
    let first := (nonNull[0]!).columnType
    if nonNull.all (·.columnType == first) then first else .mixed

/-- Smart constructor: build a `Column`, auto-inferring its type. -/
def mk' (name : String) (values : Array Value) : Column :=
  { name := name, values := values, colType := inferType values }

/-- Map a function over every value (result type becomes `mixed`). -/
def mapValues (f : Value → Value) (c : Column) : Column :=
  { name := c.name, values := c.values.map f, colType := .mixed }

/-- Re-infer the column type from its current values (e.g. after `mapValues`). -/
def reInferType (c : Column) : Column :=
  { c with colType := inferType c.values }

/-- Keep the values whose positionally-aligned mask entry is `true` (`zip`
    stops at the shorter of mask/values). -/
def filterByMask (mask : Array Bool) (c : Column) : Column :=
  { name := c.name
    values := (c.values.zip mask).filterMap (fun (v, b) => if b then some v else none)
    colType := c.colType }

/-- Extract numeric values as `Option Float` (non-numeric / null → `none`). -/
def toFloats (c : Column) : Array (Option Float) :=
  c.values.map Value.toFloat?

/-- Render every value as a `String` (via `ToString Value`). -/
def toStrings (c : Column) : Array String :=
  c.values.map toString

/-- Number of null values. -/
def nullCount (c : Column) : Nat :=
  c.values.foldl (fun acc v => if v.isNull then acc + 1 else acc) 0

/-- Number of non-null values. -/
def nonNullCount (c : Column) : Nat :=
  c.size - c.nullCount

/-- The first `n` elements. -/
def take (n : Nat) (c : Column) : Column :=
  { name := c.name, values := c.values.extract 0 n, colType := c.colType }

/-- All but the first `n` elements. -/
def drop (n : Nat) (c : Column) : Column :=
  { name := c.name, values := c.values.extract n c.values.size, colType := c.colType }

/-- Unique values, preserving first-occurrence order. -/
def unique (c : Column) : Array Value :=
  c.values.foldl (fun seen v => if seen.contains v then seen else seen.push v) #[]

end Column
end DataFrame
