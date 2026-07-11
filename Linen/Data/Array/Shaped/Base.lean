/-
  Linen.Data.Array.Shaped.Base — the `Source` class of readable array
  representations

  Ported from Haskell's `Data.Array.Repa.Base` (package `repa`). Upstream's
  `Source` class declares `Array r sh e` as an *associated data family*: each
  representation tag `r` gets its own concrete constructor. Lean has no
  data-family mechanism, so each representation is instead its own concrete
  type (`Delayed sh e`, `Manifest sh e`, …) and `Source` is parameterized
  directly over that type constructor, dropping the phantom tag `r`.

  `deepSeqArray`/`deepSeqArrays` are dropped: their sole purpose is to force
  strict evaluation under GHC's laziness (documented upstream as a hint to
  the GHC simplifier about unboxing), which has no counterpart in Lean's
  call-by-value semantics — the same reasoning already applied to `Shape`'s
  `deepSeq`.
-/

import Linen.Data.Array.Shaped.Shape

namespace Data.Array.Shaped

/-- Class of array representations that we can read elements from.

    `[Inhabited e]` is required so that `linearIndex` can be given a total
    Lean definition: an out-of-range `Int` is a precondition violation
    upstream too (Haskell's instances simply crash on it), and Lean's
    equivalent of "crash on a violated precondition" is `panic!`, which
    itself requires `Inhabited e` to produce *some* value of the result
    type.

    Universe-polymorphic in `arr`'s result (`Type u` rather than a fixed
    `Type`) so that representations backed by an existential type — e.g.
    `Cursored`, whose cursor type is itself data stored in the array — can
    still be `Source` instances despite living a universe higher. -/
class Source.{u} (arr : Type → Type → Type u) where
  /-- O(1). Take the extent (size) of an array. -/
  extent : {sh e : Type} → [Shape sh] → [Inhabited e] → arr sh e → sh
  /-- O(1). Linear indexing into the underlying, row-major, array representation. -/
  linearIndex : {sh e : Type} → [Shape sh] → [Inhabited e] → arr sh e → Int → e
  /-- O(1). Linear indexing, without bounds checking. -/
  unsafeLinearIndex : {sh e : Type} → [Shape sh] → [Inhabited e] → arr sh e → Int → e :=
    linearIndex

/-- O(1). Shape-polymorphic indexing. -/
def index {arr sh e} [Shape sh] [Inhabited e] [Source arr] (a : arr sh e) (ix : sh) : e :=
  Source.linearIndex a (Shape.toIndex (Source.extent a) ix)

/-- O(1). Shape-polymorphic indexing, without bounds checking. -/
def unsafeIndex {arr sh e} [Shape sh] [Inhabited e] [Source arr] (a : arr sh e) (ix : sh) : e :=
  Source.unsafeLinearIndex a (Shape.toIndex (Source.extent a) ix)

/-- O(n). Convert an array to a list. -/
def toList {arr sh e} [Shape sh] [Inhabited e] [Source arr] (a : arr sh e) : List e :=
  let len := (Shape.size (Source.extent a)).toNat
  (List.range len).map (fun i => Source.unsafeLinearIndex a (Int.ofNat i))

end Data.Array.Shaped
