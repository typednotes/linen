/-
  Tests for `Linen.Control.Profunctor.Unsafe`.

  `Profunctor`'s `dimap`/`lmap`/`rmap` over the `Control.Fun` instance, plus
  the default-implementation laws relating them.
-/
import Linen.Control.Profunctor.Unsafe

open Control

namespace Tests.Control.Profunctor.Unsafe

def inc : Fun Nat Nat := ⟨(· + 1)⟩

#guard (Profunctor.dimap (α := String) (γ := Nat) (δ := Nat)
          String.length (· * 2) inc).apply "abc" == 8
#guard (Profunctor.lmap String.length inc).apply "abc" == 4
#guard (Profunctor.rmap (· * 2) inc).apply 5 == 12

/-! ### dimap in terms of lmap/rmap (default-implementation law) -/

example (h : Fun Nat Nat) (f : String → Nat) (g : Nat → Nat) :
    Profunctor.dimap f g h = Profunctor.lmap f (Profunctor.rmap g h) := rfl

end Tests.Control.Profunctor.Unsafe
