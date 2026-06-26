/-
  Linen.Data.List — Extended list operations

  Supplements Lean's built-in `List` with the operations from Haskell's
  `Data.List` that core lacks. Lives in `Data.List'` (primed) to avoid clashing
  with the core `List`.

  Already in core, so **not** re-ported:

  | Haskell        | Lean core                          |
  |----------------|------------------------------------|
  | `nub` / `nubBy`| `List.eraseDups` / `List.eraseDupsBy` |
  | `group`/`groupBy` | `List.splitBy`                  |
  | `scanr`/`scanl`| `List.scanr` / `List.scanl`        |
  | `intercalate`  | `List.intercalate`                 |
  | `deleteBy eq x`| `List.eraseP (eq x)`               |
  | `union`/`intersect` (BEq) | `List.union` / `List.inter` |
  | `genericLength`| `List.length`                      |

  `unfoldr` is **not** provided: it is inherently non-terminating, and this
  library bans both `partial` and fuel parameters (use an explicit bounded
  recursion at the call site instead).
-/

import Linen.Data.List.NonEmpty

namespace Data.List'

-- ── Transposing ─────────────────────────────────

/-- Auxiliary for `transpose`: merge a row into the partial transpose. -/
private def transposeAux : List α → List (List α) → List (List α)
  | [], rs => rs
  | a :: as, [] => [a] :: transposeAux as []
  | a :: as, r :: rs => (a :: r) :: transposeAux as rs

/-- Transpose a list of lists (rows to columns), by structural recursion.
    $$\text{transpose}([[1,2,3],[4,5,6]]) = [[1,4],[2,5],[3,6]]$$ -/
def transpose : List (List α) → List (List α)
  | [] => []
  | l :: ls => transposeAux l (transpose ls)

-- ── Sublists ────────────────────────────────────

/-- All suffixes, from longest to shortest, including `[]`. Returns `NonEmpty`
    since every list has at least the empty suffix.
    $$\text{tails}([1,2,3]) = [[1,2,3],[2,3],[3],[]]$$ -/
def tails : List α → List.NonEmpty (List α)
  | [] => List.NonEmpty.singleton []
  | x :: xs => ⟨x :: xs, (tails xs).toList⟩

/-- All prefixes, from shortest to longest. Returns `NonEmpty`.
    $$\text{inits}([1,2,3]) = [[],[1],[1,2],[1,2,3]]$$ -/
def inits : List α → List.NonEmpty (List α)
  | [] => List.NonEmpty.singleton []
  | x :: xs => ⟨[], (inits xs).toList.map (x :: ·)⟩

/-- All subsequences (power set), including `[]`. $$|\text{subsequences}(l)| = 2^{|l|}$$ -/
def subsequences : List α → List (List α)
  | [] => [[]]
  | x :: xs =>
    let rest := subsequences xs
    rest ++ rest.map (x :: ·)

/-- Insert `x` at every position in `ys`. -/
private def insertEverywhere (x : α) : List α → List (List α)
  | [] => [[x]]
  | y :: ys => (x :: y :: ys) :: (insertEverywhere x ys).map (y :: ·)

/-- All permutations of a list. $$|\text{permutations}(l)| = |l|!$$ -/
def permutations : List α → List (List α)
  | [] => [[]]
  | x :: xs =>
    let perms := permutations xs
    perms.flatMap (insertEverywhere x)

-- ── Accumulating maps ───────────────────────────

/-- Left-to-right map with accumulator.
    $$\text{mapAccumL}(f, s_0, [x_1, \ldots, x_n]) = (s_n, [y_1, \ldots, y_n])$$ -/
def mapAccumL (f : σ → α → σ × β) (init : σ) : List α → σ × List β
  | [] => (init, [])
  | x :: xs =>
    let (s', y) := f init x
    let (s'', ys) := mapAccumL f s' xs
    (s'', y :: ys)

/-- Right-to-left map with accumulator. -/
def mapAccumR (f : σ → α → σ × β) (init : σ) : List α → σ × List β
  | [] => (init, [])
  | x :: xs =>
    let (s', ys) := mapAccumR f init xs
    let (s'', y) := f s' x
    (s'', y :: ys)

-- ── Sorting ─────────────────────────────────────

/-- Sort by a derived key (core's `mergeSort` takes a comparator, not a key). -/
def sortOn [Ord β] (f : α → β) (l : List α) : List α :=
  l.toArray.qsort (fun a b => compare (f a) (f b) == .lt) |>.toList

-- ── Extrema (by custom comparator) ──────────────

/-- Maximum element by a custom comparator, or `none` for empty lists. -/
def maximumBy (cmp : α → α → Ordering) : List α → Option α
  | [] => none
  | x :: xs => some (xs.foldl (fun acc y => if cmp acc y == .lt then y else acc) x)

/-- Minimum element by a custom comparator, or `none` for empty lists. -/
def minimumBy (cmp : α → α → Ordering) : List α → Option α
  | [] => none
  | x :: xs => some (xs.foldl (fun acc y => if cmp acc y == .gt then y else acc) x)

-- ── Set operations (by custom equality) ─────────

/-- List union by a custom equality (core's `List.union` uses `BEq`). -/
def unionBy (eq : α → α → Bool) (xs ys : List α) : List α :=
  xs ++ ys.filter (fun y => !xs.any (eq y))

/-- List intersection by a custom equality (core's `List.inter` uses `BEq`). -/
def intersectBy (eq : α → α → Bool) (xs ys : List α) : List α :=
  xs.filter (fun x => ys.any (eq x))

/-- Insert into a sorted list (by comparator), maintaining order. -/
def insertBy (cmp : α → α → Ordering) (x : α) : List α → List α
  | [] => [x]
  | y :: ys => if cmp x y != .gt then x :: y :: ys else y :: insertBy cmp x ys

-- ── Proofs ──────────────────────────────────────

/-- `tails` produces `length + 1` suffixes. -/
theorem tails_length (l : List α) : (tails l).toList.length = l.length + 1 := by
  induction l with
  | nil => rfl
  | cons _ xs ih =>
    unfold tails
    simp only [List.NonEmpty.toList, List.length_cons]
    have : (tails xs).tail.length + 1 = (tails xs).toList.length := by
      simp [List.NonEmpty.toList]
    omega

/-- `inits` produces `length + 1` prefixes. -/
theorem inits_length (l : List α) : (inits l).toList.length = l.length + 1 := by
  induction l with
  | nil => rfl
  | cons _ xs ih =>
    unfold inits
    simp only [List.NonEmpty.toList, List.length_cons, List.length_map]
    have : (inits xs).tail.length + 1 = (inits xs).toList.length := by
      simp [List.NonEmpty.toList]
    omega

/-- `tails` of the empty list is the singleton `[[]]`. -/
theorem tails_nil : tails ([] : List α) = List.NonEmpty.singleton [] := rfl

/-- `inits` of the empty list is the singleton `[[]]`. -/
theorem inits_nil : inits ([] : List α) = List.NonEmpty.singleton [] := rfl

end Data.List'
