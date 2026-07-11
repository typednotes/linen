/-
  Linen.Data.Array.Shaped.Repr.Delayed — the `D` (delayed) array representation

  Ported from Haskell's `Data.Array.Repa.Repr.Delayed` (package `repa`).
  A delayed array is just a shape paired with a function from index to
  element — every read recomputes the element. Upstream also gives `D` a
  `Load` instance (`loadS`/`loadP`, backed by `Eval.Chunked`'s GHC-specific
  worker-gang filling loops) so that it can be *materialized*; that
  materialization step is ported as `Manifest.computeS`, which only needs
  `Source`, not a separate `Load` class — see `Repr/Manifest.lean`.
-/

import Linen.Data.Array.Shaped.Base

namespace Data.Array.Shaped

/-- A delayed array: a shape together with a function from index to element. -/
structure Delayed (sh e : Type) where
  extent : sh
  apply : sh → e

instance [Inhabited sh] [Inhabited e] : Inhabited (Delayed sh e) where
  default := ⟨default, fun _ => default⟩

instance : Source Delayed where
  extent a := a.extent
  linearIndex a i := a.apply (Shape.fromIndex a.extent i)

/-- O(1). Wrap a function as a delayed array. -/
def fromFunction {sh e} (sh' : sh) (f : sh → e) : Delayed sh e :=
  ⟨sh', f⟩

/-- O(1). Produce the extent of an array, and a function to retrieve an
    arbitrary element. -/
def toFunction {arr sh e} [Shape sh] [Inhabited e] [Source arr] (a : arr sh e) :
    sh × (sh → e) :=
  (Source.extent a, fun ix => unsafeIndex a ix)

/-- O(1). Delay an array: wrap it as a function from indices to elements, so
    consumers don't need to worry about what the previous representation
    was. -/
def delay {arr sh e} [Shape sh] [Inhabited e] [Source arr] (a : arr sh e) :
    Delayed sh e :=
  ⟨Source.extent a, fun ix => unsafeIndex a ix⟩

end Data.Array.Shaped
