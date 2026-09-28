/-
  Tests for `Linen.Data.Set.Lens`.
-/
import Linen.Control.Lens.Empty
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Indexed
import Linen.Control.Lens.Prism
import Linen.Control.Lens.Setter
import Linen.Data.Set.Lens

open Control.Lens Control.Lens.Internal
open Data (Set')
open Data.Functor (Contravariant)

namespace Tests.Linen.Data.Set.Lens

/-- A `Fold` trivially widens to an `IndexedFold Unit` — the index carries no
    information, so it is simply ignored via `Indexable.indexed pab ()`.
    (A local test-only helper; `setOf`/`toArrayOf` are the only combinators
    in this batch that need an `IndexedFold Unit`, upstream's own "discard
    the index" convention for `Getting (Set a) s a`.) -/
def unitIndexed {S A : Type} (l : Fold S A) : IndexedFold Unit S A :=
  fun {F} [Contravariant F] [Applicative F] {P} [Indexable Unit P] pab s =>
    l (fun a => Indexable.indexed pab () a) s

-- ── `Ixed` ───────────────────────────────────────

#guard preview (ix 1) (Data.Set'.fromList [1, 2, 3]) = some ()
#guard preview (ix 9) (Data.Set'.fromList [1, 2, 3]) = none

-- ── `At` ─────────────────────────────────────────

#guard ((Data.Set'.fromList ([1, 2, 3] : List Nat)) ^. «at» 1) = some ()
#guard ((Data.Set'.fromList ([1, 2, 3] : List Nat)) ^. «at» 9) = none
#guard ((«at» (9 : Nat) .~ some ()) (Data.Set'.fromList [1, 2, 3])).contains 9
#guard ¬((«at» (1 : Nat) .~ none) (Data.Set'.fromList [1, 2, 3])).contains 1

-- ── `AsEmpty` ────────────────────────────────────
-- (run directly via `withPrism`, matching `Tests.Linen.Control.Lens.
-- EmptyTest`'s own precedent for exercising a `Prism` without a bare-arrow
-- `Getting` bridge.)

#guard withPrism _Empty (fun _ seta =>
  match seta (Data.Set'.fromList ([] : List Nat)) with | .inr () => true | .inl _ => false)
#guard withPrism _Empty (fun _ seta =>
  match seta (Data.Set'.fromList ([1] : List Nat)) with | .inr () => false | .inl _ => true)

-- ── `setmapped` ──────────────────────────────────

#guard Data.Set'.toList' (over setmapped (fun n => n + 1) (Data.Set'.fromList ([1, 2, 3] : List Nat)))
  == [2, 3, 4]

-- ── `setOf` ──────────────────────────────────────

#guard Data.Set'.toList' (setOf (unitIndexed folded) [1, 2, 2, 3]) == [1, 2, 3]

end Tests.Linen.Data.Set.Lens
