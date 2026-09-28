/-
  Tests for `Linen.Data.Array.Shaped.Base` — the `Source` class, `index`,
  `unsafeIndex`, and `toList`.

  Exercised through a minimal local `Source` instance backed by a plain
  `List`, since `Source` itself has no instances until a representation
  (e.g. `Delayed`, ported separately) is defined.
-/
import Linen.Data.Array.Shaped.Base
import Linen.Data.Array.Shaped.Index

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Base

private structure ListArr (sh e : Type) where
  extent : sh
  elems : List e

private instance : Source ListArr where
  extent a := a.extent
  linearIndex a i := a.elems.getD i.toNat default

private def arr : ListArr DIM2 Nat := ⟨ix2 2 3, [1, 2, 3, 4, 5, 6]⟩

#guard index arr (ix2 0 0) == 1
#guard index arr (ix2 1 2) == 6
#guard unsafeIndex arr (ix2 1 0) == 4
#guard toList arr == [1, 2, 3, 4, 5, 6]

end Tests.Data.Array.Shaped.Base
