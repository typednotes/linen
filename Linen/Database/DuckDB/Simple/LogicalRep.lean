/-
  Linen.Database.DuckDB.Simple.LogicalRep — structured logical-type/value
  representations

  Module #2 of `docs/imports/duckdb-simple/dependencies.md`, on
  `Linen.Database.DuckDB.FFI.LogicalTypes`. No dependency on any other
  `duckdb-simple` module.

  ## Design

  `LogicalTypeRep` is a pure Lean tree mirroring a `duckdb_logical_type`
  descriptor: every DuckDB logical type this batch cares about
  (scalar/`DECIMAL`/`LIST`/`ARRAY`/`MAP`/`STRUCT`/`UNION`/`ENUM`) is either a
  leaf or built from smaller `LogicalTypeRep`s, letting callers build/inspect
  a type tree without holding any live FFI handle. `StructField`/
  `StructValue`/`UnionMemberType`/`UnionValue` are the accompanying
  *materialized-value* shapes `FromField` (module #5) decodes into —
  `StructValue`/`UnionValue` are generic over their payload type `α` so
  `FromField.lean` can specialize them to its own recursive `FieldValue`.

  `logicalTypeToRep`/`logicalTypeFromRep` convert between a live
  `Linen.Database.DuckDB.FFI.Types.LogicalType` handle and a `LogicalTypeRep`
  tree, in each direction:

  - **`logicalTypeToRep`** walks a *runtime* FFI handle, whose nesting depth
    is not visible to Lean's termination checker (each child type is a fresh
    handle fetched via another C call, not a structural subterm of anything
    Lean already knows terminates). Per `AGENTS.md`'s termination rule, this
    is exactly the case calling for a genuine argument rather than a
    fuel-parameter dodge — but no `duckdb.h` API gives an actual maximum
    nesting depth to use as that argument. This port therefore takes the
    same documented approach as other genuinely out-of-scope corners already
    recorded in this codebase (e.g. `Database.DuckDB.FFI.OpenConnect`'s
    "always pass a NULL `duckdb_config`"): recursion is bounded by
    `maxNestingDepth` (a conservative constant, `64`, far beyond any type
    tree a real schema would build), with any *deeper* type tree reported as
    an explicit `IO` error rather than either looping forever or silently
    truncating. `Nat`-indexed recursion on that bound is ordinary structural
    recursion (Lean already accepts it with no proof obligation) — it is
    used here purely as a documented, honest depth cap, not to dodge a
    termination argument that could otherwise be given.
  - **`logicalTypeFromRep`** recurses on an actual, already-finite Lean value
    (`LogicalTypeRep` itself), so it needs no depth cap — but its `.struct`/
    `.union` cases recurse across a `List LogicalTypeRep` of children, which
    needs the mutual-recursion-with-`sizeOf` termination proof this codebase
    already uses for `Linen.CDP.Domains.Media.PlayerError`'s own
    self-referential `cause : List PlayerError` field (see that module's
    `encodePlayerError`/`encodePlayerErrorList`).

  ## Haskell source
  - `Database.DuckDB.Simple.LogicalRep` (`duckdb-simple` package, version
    0.1.5.1)
-/

import Linen.Database.DuckDB.FFI.LogicalTypes

namespace Database.DuckDB.Simple.LogicalRep

open Database.DuckDB.FFI.LogicalTypes
open Database.DuckDB.FFI.Types (Type_ LogicalType)

-- ────────────────────────────────────────────────────────────────────
-- Logical-type tree
-- ────────────────────────────────────────────────────────────────────

/-- A pure Lean description of a `duckdb_logical_type` tree (see the module
    doc). `struct`/`union` carry parallel `names`/`types` lists rather than a
    single list of named pairs, so `logicalTypeFromRep`'s termination proof
    only has to reason about one recursive `List LogicalTypeRep`. -/
inductive LogicalTypeRep where
  /-- Any DuckDB type with no further structure to describe (includes every
      primitive `duckdb_type` other than `DECIMAL`/`LIST`/`ARRAY`/`MAP`/
      `STRUCT`/`UNION`/`ENUM`). -/
  | scalar (ty : Type_)
  /-- A `DECIMAL(width, scale)` type. -/
  | decimal (width scale : UInt8)
  /-- A `LIST` type, from its element type. -/
  | list (elem : LogicalTypeRep)
  /-- A fixed-size `ARRAY` type, from its element type and length. -/
  | array (elem : LogicalTypeRep) (size : UInt64)
  /-- A `MAP` type, from its key and value types. -/
  | map (key value : LogicalTypeRep)
  /-- A `STRUCT` type, from parallel member-name/member-type lists (same
      length). -/
  | struct (names : List String) (types : List LogicalTypeRep)
  /-- A `UNION` type, from parallel member-name/member-type lists (same
      length). -/
  | union (names : List String) (types : List LogicalTypeRep)
  /-- An `ENUM` type, from its dictionary of member names (in dictionary
      order). -/
  | enum (names : List String)
deriving Repr, Inhabited, BEq

-- ────────────────────────────────────────────────────────────────────
-- Materialized struct/union values
-- ────────────────────────────────────────────────────────────────────

/-- A named field within a `STRUCT`-like value. -/
structure StructField (α : Type u) where
  name : String
  value : α
deriving Repr, Inhabited, BEq

/-- A fully materialized `STRUCT` value: one `StructField` per member, in
    declaration order. -/
structure StructValue (α : Type u) where
  fields : Array (StructField α)
deriving Repr, Inhabited, BEq

/-- Look up a `StructValue`'s field by name (linear scan — substitutes for
    upstream's precomputed `Map Text Int` index, which this port does not
    keep: none of this batch's modules need repeated lookups against the
    same `StructValue`, so the O(1)-amortized index buys nothing here). -/
def StructValue.field? (s : StructValue α) (name : String) : Option (StructField α) :=
  s.fields.find? (·.name == name)

/-- A named member within a `UNION` type. -/
structure UnionMemberType where
  name : String
  type : LogicalTypeRep
deriving Repr, Inhabited, BEq

/-- A fully materialized `UNION` value: which member is active (`index`/
    `label`), its decoded `payload`, and the full member list (so a caller
    can inspect the type even though only one member has a value). -/
structure UnionValue (α : Type u) where
  index : UInt16
  label : String
  payload : α
  members : Array UnionMemberType
deriving Repr, Inhabited, BEq

-- ────────────────────────────────────────────────────────────────────
-- FFI handle ↔ `LogicalTypeRep`
-- ────────────────────────────────────────────────────────────────────

/-- The maximum `LogicalTypeRep` nesting depth `logicalTypeToRep` will
    follow before reporting an error — see the module doc. -/
def maxNestingDepth : Nat := 64

/-- Convert a list of child names (fetched by `fetchName i` for `i < count`)
    into plain `String`s, failing if any is unexpectedly absent. -/
private def childNames (count : Nat) (label : String) (fetchName : Nat → IO (Option String)) :
    IO (List String) := do
  let mut out : List String := []
  for i in [0:count] do
    match ← fetchName i with
    | some n => out := n :: out
    | none => throw (IO.userError s!"logicalTypeToRep: {label} name at index {i} is null")
  pure out.reverse

/-- Convert a live `LogicalType` handle into a `LogicalTypeRep` tree, giving
    up with an `IO` error if it nests deeper than `fuel` levels. Structural
    recursion on `fuel` (an ordinary `Nat`), decreasing by exactly one at
    every recursive call — see the module doc for why `fuel` is a genuine,
    documented depth cap rather than a termination dodge. -/
def logicalTypeToRepFuel (fuel : Nat) (ty : LogicalType) : IO LogicalTypeRep := do
  let tid ← getTypeId ty
  match tid with
  | .decimal => do
    let w ← decimalWidth ty
    let s ← decimalScale ty
    pure (.decimal w s)
  | .enum => do
    let dictSize ← enumDictionarySize ty
    let names ← childNames dictSize.toNat "enum dictionary"
      (fun i => enumDictionaryValue ty (UInt64.ofNat i))
    pure (.enum names)
  | .list | .array | .map | .struct | .union =>
    match fuel with
    | 0 => throw (IO.userError "logicalTypeToRep: max nesting depth exceeded")
    | fuel + 1 =>
      match tid with
      | .list => do
        let child ← listTypeChildType ty
        let rep ← logicalTypeToRepFuel fuel child
        destroy child
        pure (.list rep)
      | .array => do
        let child ← arrayTypeChildType ty
        let rep ← logicalTypeToRepFuel fuel child
        destroy child
        let size ← arrayTypeArraySize ty
        pure (.array rep size)
      | .map => do
        let keyTy ← mapTypeKeyType ty
        let keyRep ← logicalTypeToRepFuel fuel keyTy
        destroy keyTy
        let valTy ← mapTypeValueType ty
        let valRep ← logicalTypeToRepFuel fuel valTy
        destroy valTy
        pure (.map keyRep valRep)
      | .struct => do
        let count ← structTypeChildCount ty
        let n := count.toNat
        let names ← childNames n "struct child"
          (fun i => structTypeChildName ty (UInt64.ofNat i))
        let mut types : List LogicalTypeRep := []
        for i in [0:n] do
          let child ← structTypeChildType ty (UInt64.ofNat i)
          let rep ← logicalTypeToRepFuel fuel child
          destroy child
          types := rep :: types
        pure (.struct names types.reverse)
      | .union => do
        let count ← unionTypeMemberCount ty
        let n := count.toNat
        let names ← childNames n "union member"
          (fun i => unionTypeMemberName ty (UInt64.ofNat i))
        let mut types : List LogicalTypeRep := []
        for i in [0:n] do
          let child ← unionTypeMemberType ty (UInt64.ofNat i)
          let rep ← logicalTypeToRepFuel fuel child
          destroy child
          types := rep :: types
        pure (.union names types.reverse)
      | _ => pure (.scalar tid) -- unreachable: guarded by the outer match
  | _ => pure (.scalar tid)

/-- Convert a live `LogicalType` handle into a `LogicalTypeRep` tree, giving
    up with an `IO` error past `maxNestingDepth` levels. -/
def logicalTypeToRep (ty : LogicalType) : IO LogicalTypeRep :=
  logicalTypeToRepFuel maxNestingDepth ty

mutual

/-- Materialize a `LogicalType` handle from a `LogicalTypeRep` tree. The
    result must eventually be destroyed with
    `Linen.Database.DuckDB.FFI.destroy` (or let its GC
    finalizer do so). -/
def logicalTypeFromRep : LogicalTypeRep → IO LogicalType
  | .scalar ty => create ty
  | .decimal width scale => createDecimalType width scale
  | .list elem => do
    let childTy ← logicalTypeFromRep elem
    let result ← createListType childTy
    destroy childTy
    pure result
  | .array elem size => do
    let childTy ← logicalTypeFromRep elem
    let result ← createArrayType childTy size
    destroy childTy
    pure result
  | .map key value => do
    let keyTy ← logicalTypeFromRep key
    let valueTy ← logicalTypeFromRep value
    let result ← createMapType keyTy valueTy
    destroy keyTy
    destroy valueTy
    pure result
  | .struct names types => do
    let childTypes ← logicalTypeFromRepList types
    let result ← createStructType childTypes.toArray names.toArray
    childTypes.forM destroy
    pure result
  | .union names types => do
    let childTypes ← logicalTypeFromRepList types
    let result ← createUnionType childTypes.toArray names.toArray
    childTypes.forM destroy
    pure result
  | .enum names => createEnumType names.toArray
termination_by rep => sizeOf rep
decreasing_by
  all_goals simp_wf <;> omega

/-- Materialize a list of `LogicalType` handles from a list of
    `LogicalTypeRep`s, in order. The caller is responsible for destroying
    the intermediate handles once done with them (`logicalTypeFromRep`'s
    `.struct`/`.union` cases do so immediately after building the enclosing
    type). -/
def logicalTypeFromRepList : List LogicalTypeRep → IO (List LogicalType)
  | [] => pure []
  | rep :: reps => do
    let hd ← logicalTypeFromRep rep
    let tl ← logicalTypeFromRepList reps
    pure (hd :: tl)
termination_by l => sizeOf l
decreasing_by
  all_goals simp_wf <;> omega

end

end Database.DuckDB.Simple.LogicalRep
