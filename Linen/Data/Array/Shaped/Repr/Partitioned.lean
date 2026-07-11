/-
  Linen.Data.Array.Shaped.Repr.Partitioned — the `Partitioned` array
  representation

  Ported from Haskell's `Data.Array.Repa.Repr.Partitioned` (package `repa`).
  A partitioned array dispatches between two sub-arrays of the same shape
  by region: indices matching the range predicate read from the first
  sub-array, everything else reads from the second (which is expected to
  cover the whole shape, typically an `Undefined` array in stencil use, or
  vice versa when the range covers the interior and the fallback covers a
  border).

  Universe-polymorphic in both `arr1`/`arr2` for the same reason as
  `Cursored` (see `Repr/Cursored.lean`): one of the two nested
  representations may itself be `Cursored`, which lives one universe
  higher than a `Type 0` representation like `Manifest`.

  The `Load` instance (`loadRangeS` on the first sub-array, then `loadS` on
  the second) is dropped along with every other `Load` instance — see
  `Repr/Manifest.lean`'s `computeS`.
-/

import Linen.Data.Array.Shaped.Base

namespace Data.Array.Shaped

/-- A range of indices, given by a lower/upper bound and a membership
    predicate (the predicate is the actual test used; the bounds are
    metadata describing which slice of the shape it covers). -/
structure Range (sh : Type) where
  lo : sh
  hi : sh
  contains : sh → Bool

/-- Check whether an index is within the given range. -/
def Range.inRange {sh} (r : Range sh) (ix : sh) : Bool :=
  r.contains ix

instance [Inhabited sh] : Inhabited (Range sh) where
  default := ⟨default, default, fun _ => false⟩

/-- A partitioned array: dispatches between two sub-arrays of the same
    shape by region. Indices satisfying the range read from the first
    sub-array; everything else reads from the second. -/
structure Partitioned.{u1, u2} (arr1 : Type → Type → Type u1)
    (arr2 : Type → Type → Type u2) (sh e : Type) where
  extent : sh
  range : Range sh
  inRangeArr : arr1 sh e
  fallbackArr : arr2 sh e

instance [Inhabited sh] [Inhabited (arr1 sh e)] [Inhabited (arr2 sh e)] :
    Inhabited (Partitioned arr1 arr2 sh e) where
  default := ⟨default, default, default, default⟩

instance [Source arr1] [Source arr2] : Source (Partitioned arr1 arr2) where
  extent a := a.extent
  linearIndex a i :=
    let ix := Shape.fromIndex a.extent i
    if a.range.inRange ix then index a.inRangeArr ix else index a.fallbackArr ix

end Data.Array.Shaped
