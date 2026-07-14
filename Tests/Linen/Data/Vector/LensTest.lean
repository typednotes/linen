/-
  Tests for `Linen.Data.Vector.Lens`.
-/
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Indexed
import Linen.Control.Lens.Iso
import Linen.Control.Lens.Setter
import Linen.Data.Vector.Lens

open Control.Lens Control.Lens.Internal
open Data.Functor (Contravariant)

namespace Tests.Linen.Data.Vector.Lens

/-- See `Tests.Linen.Data.Set.Lens`'s identical helper. -/
def unitIndexed {S A : Type} (l : Fold S A) : IndexedFold Unit S A :=
  fun {F} [Contravariant F] [Applicative F] {P} [Indexable Unit P] pab s =>
    l (fun a => Indexable.indexed pab () a) s

-- ── `vector` ─────────────────────────────────────
-- (run directly via `withIso`, since `Iso` is genuinely
-- profunctor-polymorphic and does not unify with `view`/`review`'s bare
-- `Getting`/`AReview` shape without an explicit instantiation — see
-- `Linen.Control.Lens.Iso.withIso`'s own doc comment.)

#guard withIso (vector (A := Nat)) (fun sa _ => sa [1, 2, 3]) = #[1, 2, 3]
#guard withIso (vector (A := Nat)) (fun _ bt => bt #[1, 2, 3]) = [1, 2, 3]

-- ── `toArrayOf` ──────────────────────────────────

#guard toArrayOf (unitIndexed folded) [1, 2, 3] = (#[1, 2, 3] : Array Nat)

-- ── `ordinals` ───────────────────────────────────

#guard itoListOf (ordinals [0, 2]) (#[10, 20, 30] : Array Nat) = [(0, 10), (2, 30)]

#guard Id.run (ordinals [0, 2] (F := Id) (P := Indexed Nat)
    (Indexed.mk (fun _ a => a + 1)) (#[10, 20, 30] : Array Nat))
  = #[11, 20, 31]

end Tests.Linen.Data.Vector.Lens
