/-
  Tests for `Linen.Control.Lens.Type`.

  These are pure type-alias unfolding checks: since every alias here is an
  `abbrev`, the main thing worth illustrating is that the primed/unprimed
  forms unify, and that a concrete `Lens`/`Traversal`/`Setter` value
  typechecks at its alias.
-/
import Linen.Control.Lens.Type

open Control.Lens

namespace Tests.Linen.Control.Lens.Type

/-- A trivial `Lens' α α`, the identity lens: focuses the whole value. -/
def idLens : Lens' α α := fun f a => f a

-- Running the identity lens through `Id` recovers the original value.
-- Stated as `example ... := rfl` rather than `#guard`, since `#guard`'s
-- automatic `Decidable`-to-`Bool` coercion doesn't unfold through `Id` far
-- enough to find `Nat`'s `DecidableEq` instance, even though the equality
-- holds by `rfl` outright (per `AGENTS.md`'s guidance for such cases).
example : ((idLens (F := Id) id 3 : Id Nat) : Nat) = 3 := rfl

/-- `Lens' s a` unifies with `Lens s s a a` definitionally. -/
example (l : Lens' Nat Nat) : Lens Nat Nat Nat Nat := l

-- A `Setter'` built from the identity lens, run through `Id` (which is
-- `Settable`).
example : ((idLens (F := Id) (· + 1) 3 : Id Nat) : Nat) = 4 := rfl

/-- `LensLike' (F := Id) Nat Nat` is exactly `(Nat → Id Nat) → Nat → Id Nat`. -/
example (_f : LensLike' Id Nat Nat) (g : (Nat → Id Nat) → Nat → Id Nat) : True :=
  let _ : LensLike' Id Nat Nat := g
  trivial

end Tests.Linen.Control.Lens.Type
