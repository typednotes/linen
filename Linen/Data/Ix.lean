/-
  Linen.Data.Ix — Index class for array-like range operations

  Provides the `Ix` typeclass mirroring Haskell's `Data.Ix` (not in Lean core),
  with instances for `Nat`, `Int`, `Char`, `Bool`, and products.

  ## Design

  `Ix` abstracts over types that can serve as array indices. Given a pair of
  bounds $(lo, hi)$, `range` enumerates all valid indices, `index` maps an index
  to its position, and `inRange` tests membership.

  The `index` return type carries a proof that the returned position is less
  than `rangeSize bounds`, ensuring array-safe indexing by construction.
-/

namespace Data

/-- Typeclass for indexable types supporting range enumeration.

    $$\text{Ix}(\alpha)$$ provides operations for enumerating and indexing over
    bounded ranges $[lo, hi]$. Corresponds to Haskell's `Data.Ix.Ix`. -/
class Ix (α : Type u) where
  /-- Enumerate all indices in the range $[lo, hi]$. -/
  range : α × α → List α
  /-- The number of indices in the range. -/
  rangeSize : α × α → Nat := fun bounds => (range bounds).length
  /-- Map an index to its zero-based position in the range, bounded by `rangeSize`.
      The returned index carries a proof that it is less than `rangeSize bounds`. -/
  index : (bounds : α × α) → α → Option {n : Nat // n < rangeSize bounds}
  /-- Test whether an index is within the given bounds. -/
  inRange : α × α → α → Bool

-- ── Nat instance ────────────────────────────────

instance : Ix Nat where
  range bounds :=
    let (lo, hi) := bounds
    if lo > hi then []
    else (List.range (hi - lo + 1)).map (· + lo)
  rangeSize bounds :=
    let (lo, hi) := bounds
    if lo > hi then 0 else hi - lo + 1
  index bounds i :=
    let (lo, hi) := bounds
    if h : i >= lo && i <= hi then
      some ⟨i - lo, by
        simp only [Bool.and_eq_true, decide_eq_true_eq] at h
        simp only []; split <;> omega⟩
    else none
  inRange bounds i :=
    let (lo, hi) := bounds
    i >= lo && i <= hi

-- ── Int instance ────────────────────────────────

instance : Ix Int where
  range bounds :=
    let (lo, hi) := bounds
    if lo > hi then []
    else
      let n := (hi - lo + 1).toNat
      (List.range n).map (fun i => lo + Int.ofNat i)
  rangeSize bounds :=
    let (lo, hi) := bounds
    if lo > hi then 0 else (hi - lo + 1).toNat
  index bounds i :=
    let (lo, hi) := bounds
    if h : i >= lo && i <= hi then
      some ⟨(i - lo).toNat, by
        simp only [Bool.and_eq_true, decide_eq_true_eq] at h
        simp only []; split <;> omega⟩
    else none
  inRange bounds i :=
    let (lo, hi) := bounds
    i >= lo && i <= hi

-- ── Char instance ───────────────────────────────

instance : Ix Char where
  range bounds :=
    let (lo, hi) := bounds
    if lo.toNat > hi.toNat then []
    else (List.range (hi.toNat - lo.toNat + 1)).map (fun i => Char.ofNat (lo.toNat + i))
  rangeSize bounds :=
    let (lo, hi) := bounds
    if lo.toNat > hi.toNat then 0 else hi.toNat - lo.toNat + 1
  index bounds c :=
    let (lo, hi) := bounds
    if h : c.toNat >= lo.toNat && c.toNat <= hi.toNat then
      some ⟨c.toNat - lo.toNat, by
        simp only [Bool.and_eq_true, decide_eq_true_eq] at h
        simp only []; split <;> omega⟩
    else none
  inRange bounds c :=
    let (lo, hi) := bounds
    c.toNat >= lo.toNat && c.toNat <= hi.toNat

-- ── Bool instance ───────────────────────────────

private def boolToNat : Bool → Nat
  | false => 0
  | true => 1

instance : Ix Bool where
  range bounds :=
    let (lo, hi) := bounds
    match lo, hi with
    | false, false => [false]
    | false, true  => [false, true]
    | true,  true  => [true]
    | true,  false => []
  rangeSize bounds :=
    let (lo, hi) := bounds
    match lo, hi with
    | false, false => 1
    | false, true  => 2
    | true,  true  => 1
    | true,  false => 0
  index bounds b :=
    let (lo, hi) := bounds
    if h : boolToNat b >= boolToNat lo && boolToNat b <= boolToNat hi then
      some ⟨boolToNat b - boolToNat lo, by
        simp only [Bool.and_eq_true, decide_eq_true_eq] at h
        cases lo <;> cases hi <;> cases b <;> simp_all [boolToNat]⟩
    else none
  inRange bounds b :=
    let (lo, hi) := bounds
    boolToNat b >= boolToNat lo && boolToNat b <= boolToNat hi

-- ── Product instance ────────────────────────────

instance [Ix α] [Ix β] : Ix (α × β) where
  range bounds :=
    let ((loA, loB), (hiA, hiB)) := bounds
    let as := Ix.range (loA, hiA)
    let bs := Ix.range (loB, hiB)
    as.flatMap (fun a => bs.map (fun b => (a, b)))
  rangeSize bounds :=
    let ((loA, loB), (hiA, hiB)) := bounds
    Ix.rangeSize (loA, hiA) * Ix.rangeSize (loB, hiB)
  index bounds pair :=
    let ((loA, loB), (hiA, hiB)) := bounds
    let (a, b) := pair
    match Ix.index (loA, hiA) a, Ix.index (loB, hiB) b with
    | some ia, some ib =>
      let bSize := Ix.rangeSize (loB, hiB)
      some ⟨ia.val * bSize + ib.val, by
        show ia.val * bSize + ib.val < Ix.rangeSize (loA, hiA) * Ix.rangeSize (loB, hiB)
        have hia := ia.2; have hib := ib.2
        have h1 : ia.val * bSize + ib.val < (ia.val + 1) * bSize := by
          rw [Nat.add_mul]; omega
        have h2 : (ia.val + 1) * bSize ≤ Ix.rangeSize (loA, hiA) * bSize :=
          Nat.mul_le_mul_right bSize (by omega)
        exact Nat.lt_of_lt_of_le h1 h2⟩
    | _, _ => none
  inRange bounds pair :=
    let ((loA, loB), (hiA, hiB)) := bounds
    let (a, b) := pair
    Ix.inRange (loA, hiA) a && Ix.inRange (loB, hiB) b

-- ── Proofs ──────────────────────────────────────

/-- `inRange` is consistent with `index`: a value is in range iff `index`
    returns `some` (for the `Nat` instance). -/
theorem Ix.inRange_iff_index_isSome_nat (bounds : Nat × Nat) (x : Nat) :
    Ix.inRange bounds x = (Ix.index bounds x).isSome := by
  simp [Ix.inRange, Ix.index]
  split <;> simp_all

end Data
