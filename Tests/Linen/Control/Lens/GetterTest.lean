/-
  Tests for `Linen.Control.Lens.Getter`.
-/
import Linen.Control.Lens.Getter

open Control.Lens Control.Lens.Internal Data.Functor

namespace Tests.Linen.Control.Lens.Getter

structure Point where
  x : Nat
  y : Nat
deriving Repr, BEq

/-- `to Point.x`, viewed as a plain `Getter Point Nat`. -/
def xG : Getter Point Nat := to Point.x

#guard view xG ⟨3, 4⟩ = 3
#guard (⟨3, 4⟩ : Point) ^. xG = 3

/-- `like`: a `Getter` that ignores its argument. -/
def alwaysFive : Getter Point Nat := like 5

#guard view alwaysFive ⟨3, 4⟩ = 5

-- `views` post-processes the focused value before extracting it.
#guard views xG (· * 10) ⟨3, 4⟩ = 30

/-- `iview` recovers the index alongside the value, for an indexed getter
    built directly from `Indexed`: this one always reports the fixed index
    `7` alongside `p.x`. -/
def indexedX : IndexedGetting Nat (Nat × Nat) Point Nat :=
  fun ix p => Const.mk (ix.runIndexed 7 p.x).getConst

#guard iview indexedX ⟨3, 4⟩ = (7, 3)
#guard (⟨3, 4⟩ : Point) ^@. indexedX = (7, 3)

end Tests.Linen.Control.Lens.Getter
