/-
  PostgREST.RangeQuery — Range and pagination types

  Types for representing HTTP range-based pagination in PostgREST.
  Supports parsing the `Range` header (e.g., `0-24`) and producing
  the `Content-Range` response header (e.g., `0-24/100`).

  ## Haskell source
  - `PostgREST.RangeQuery` (postgrest package)

  ## Design
  - `NonnegRange` stores a non-negative offset and optional limit
  - `ContentRange` represents the response range with optional total count
  - Parsing is pure and total (returns `Option` on failure)
-/

namespace PostgREST.RangeQuery

-- ────────────────────────────────────────────────────────────────────
-- NonnegRange
-- ────────────────────────────────────────────────────────────────────

/-- A non-negative range with an offset and optional limit.
    $$\text{NonnegRange} = \langle \text{offset} : \mathbb{N},
      \text{limit} : \text{Option}\ \mathbb{N} \rangle$$

    When `rangeLimit` is `none`, the range is unbounded (no limit). -/
structure NonnegRange where
  rangeOffset : Nat
  rangeLimit : Option Nat
  deriving BEq, Repr, Inhabited

instance : ToString NonnegRange where
  toString r :=
    let limitStr := match r.rangeLimit with
      | some l => toString (r.rangeOffset + l - 1)
      | none => ""
    s!"{r.rangeOffset}-{limitStr}"

/-- The unlimited range: offset 0, no limit.
    $$\text{unlimited} = \langle 0, \text{none} \rangle$$ -/
def NonnegRange.unlimited : NonnegRange := ⟨0, none⟩

/-- The number of rows this range requests, or `none` for unbounded.
    $$\text{size}(r) = r.\text{limit}$$ -/
def NonnegRange.size (r : NonnegRange) : Option Nat := r.rangeLimit

/-- Check whether the range is unlimited (no upper bound).
    $$\text{isUnlimited}(r) \iff r.\text{limit} = \text{none}$$ -/
def NonnegRange.isUnlimited (r : NonnegRange) : Bool := r.rangeLimit.isNone

/-- Restrict a range to at most `n` rows.
    $$\text{restrictTo}(r, n) = \langle r.\text{offset},
      \text{some}(\min(r.\text{limit}, n)) \rangle$$ -/
def NonnegRange.restrictTo (r : NonnegRange) (n : Nat) : NonnegRange :=
  { r with rangeLimit := some (match r.rangeLimit with
    | some l => min l n
    | none => n) }

-- ────────────────────────────────────────────────────────────────────
-- Parsing
-- ────────────────────────────────────────────────────────────────────

/-- Parse a range header value of the form `"<low>-<high>"` into a
    `NonnegRange`. Returns `none` if the format is invalid or the
    bounds are non-numeric.

    Examples:
    - `"0-24"` $\to$ `some ⟨0, some 25⟩` (25 rows starting at 0)
    - `"0-"` $\to$ `some ⟨0, none⟩` (unbounded from 0)
    - `"abc"` $\to$ `none`

    $$\text{parseRange}(\texttt{lo-hi}) = \langle \text{lo},
      \text{some}(\text{hi} - \text{lo} + 1) \rangle$$ -/
def parseRange (s : String) : Option NonnegRange :=
  let trimmed := s.trimAscii.toString
  match trimmed.splitOn "-" with
  | [loStr, hiStr] =>
    match loStr.trimAscii.toString.toNat? with
    | none => none
    | some lo =>
      let hiTrimmed := hiStr.trimAscii.toString
      if hiTrimmed.isEmpty then
        some ⟨lo, none⟩
      else
        match hiTrimmed.toNat? with
        | none => none
        | some hi =>
          if hi < lo then none
          else some ⟨lo, some (hi - lo + 1)⟩
  | [singleStr] =>
    match singleStr.trimAscii.toString.toNat? with
    | some n => some ⟨n, some 1⟩
    | none => none
  | _ => none

-- ────────────────────────────────────────────────────────────────────
-- ContentRange
-- ────────────────────────────────────────────────────────────────────

/-- The Content-Range response header representation.
    $$\text{ContentRange} = \langle \text{offset}, \text{limit},
      \text{total}? \rangle$$

    Rendered as `"<offset>-<offset+limit-1>/<total>"` or
    `"<offset>-<offset+limit-1>/*"` when total is unknown. -/
structure ContentRange where
  offset : Nat
  limit : Nat
  total : Option Nat
  /-- When the total is known, offset + limit cannot exceed the total.
      $$\forall t \in \text{total},\; \text{offset} + \text{limit} \leq t$$ -/
  valid : ∀ t, total = some t → offset + limit ≤ t := by
    intro t ht; omega
  deriving Repr

instance : BEq ContentRange where
  beq a b := a.offset == b.offset && a.limit == b.limit && a.total == b.total

instance : Inhabited ContentRange where
  default := ⟨0, 0, none, by intro t ht; simp at ht⟩

/-- Produce a `Content-Range` header value from a `ContentRange`.
    $$\text{contentRangeHeader}(\langle o, l, t \rangle) =
      \texttt{o}\texttt{-}\texttt{(o+l-1)}\texttt{/}\texttt{t|*}$$

    Special case: when `limit = 0`, produces `"*/<total>"` or `"*/*"`. -/
def contentRangeHeader (cr : ContentRange) : String :=
  let totalStr := match cr.total with
    | some t => toString t
    | none => "*"
  if cr.limit == 0 then
    s!"*/{totalStr}"
  else
    let last := cr.offset + cr.limit - 1
    s!"{cr.offset}-{last}/{totalStr}"

instance : ToString ContentRange where
  toString cr := contentRangeHeader cr

/-- Create a `ContentRange` from a `NonnegRange` and an optional total count,
    with a proof that the range fits within the total (when known).
    $$\text{fromNonnegRange}(r, t, h) = \langle r.\text{offset},
      r.\text{limit} \mid 0, t \rangle$$ -/
def ContentRange.fromNonnegRange (r : NonnegRange) (total : Option Nat)
    (h : ∀ t, total = some t → r.rangeOffset + r.rangeLimit.getD 0 ≤ t := by
      intro t ht; omega) : ContentRange :=
  { offset := r.rangeOffset,
    limit := r.rangeLimit.getD 0,
    total := total,
    valid := h }

end PostgREST.RangeQuery
