/-
  Linen.Data.Array.Shaped.Repr.Undefined — the `Undefined` array
  representation

  Ported from Haskell's `Data.Array.Repa.Repr.Undefined` (package `repa`).
  An array with a known extent whose elements are never meant to be read —
  normally used as the last partition of a `Partitioned` array, when earlier
  partitions are expected to cover the whole shape. Upstream's `linearIndex`
  is `error`; the Lean equivalent is `panic!`.
-/

import Linen.Data.Array.Shaped.Base

namespace Data.Array.Shaped

/-- An array with a known extent but undefined elements: reading any
    element `panic!`s. -/
structure Undefined (sh e : Type) where
  extent : sh

instance [Inhabited sh] : Inhabited (Undefined sh e) where
  default := ⟨default⟩

instance : Source Undefined where
  extent a := a.extent
  linearIndex _ _ := panic! "Linen.Data.Array.Shaped.Repr.Undefined: array element is undefined"

end Data.Array.Shaped
