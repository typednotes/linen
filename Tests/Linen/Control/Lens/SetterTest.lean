/-
  Tests for `Linen.Control.Lens.Setter`.
-/
import Linen.Control.Lens.Setter

open Control.Lens

namespace Tests.Linen.Control.Lens.Setter

structure Point where
  x : Nat
  y : Nat
deriving Repr, BEq, DecidableEq

/-- A hand-written `Setter'` focused on `Point.x`, built via `sets`. -/
def xS : Setter' Point Nat :=
  sets (fun f p => { p with x := f p.x })

#guard over xS (· + 1) ⟨3, 4⟩ = ⟨4, 4⟩
#guard set xS 10 ⟨3, 4⟩ = ⟨10, 4⟩
#guard set' xS 10 ⟨3, 4⟩ = ⟨10, 4⟩

#guard (xS .~ 10) (⟨3, 4⟩ : Point) = ⟨10, 4⟩
#guard (xS %~ (· + 1)) (⟨3, 4⟩ : Point) = ⟨4, 4⟩
#guard (xS <.~ 10) (⟨3, 4⟩ : Point) = (10, ⟨10, 4⟩)

/-- A `Setter'` focused on an `Option`-valued field, exercising `(?~)`/`(<?~)`. -/
structure Cell where
  value : Option Nat
deriving Repr, BEq, DecidableEq

def valueS : Setter' Cell (Option Nat) :=
  sets (fun f c => { c with value := f c.value })

#guard (valueS ?~ 7) (⟨none⟩ : Cell) = ⟨some 7⟩
#guard (valueS <?~ 7) (⟨none⟩ : Cell) = (7, ⟨some 7⟩)

-- `mapped`: the canonical `Setter` on any `Functor`'s contents.
#guard over (mapped) (· + 1) (some 3 : Option Nat) = some 4
#guard over (mapped) (· * 2) ([1, 2, 3] : List Nat) = [2, 4, 6]

-- `contramapped`: the canonical `Setter` on any `Contravariant` functor's
-- contents. A small `Bool`-valued predicate stands in for
-- `Data.Functor.Predicate` here so the check reduces by `decide` alone
-- (a `Prop`-valued predicate would need to unfold `sets`/`over`/`contramap`
-- under `Decidable` instance search, which only unfolds `@[reducible]`
-- definitions). Contravariance means pre-composing the predicate's argument
-- with `f`, so testing the mapped predicate at `3` after
-- `contramapped (· + 1)` checks the original predicate at `4`.
structure BoolPred (α : Type) where
  runPred : α → Bool

instance : Data.Functor.Contravariant BoolPred where
  contramap f p := ⟨p.runPred ∘ f⟩

#guard (over contramapped (fun n : Nat => n + 1)
    (BoolPred.mk (· = 4) : BoolPred Nat)).runPred 3

end Tests.Linen.Control.Lens.Setter
