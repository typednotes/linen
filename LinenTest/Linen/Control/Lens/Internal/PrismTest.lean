/-
  Tests for `Linen.Control.Lens.Internal.Prism`.

  `Market Nat Nat Nat Nat`: `Functor`/`Profunctor`/`Choice` instances.
-/
import Linen.Control.Lens.Internal.Prism

open Control Control.Profunctor Control.Lens.Internal

namespace Tests.Control.Lens.Internal.Prism

/-- The `Prism Nat Nat Nat Nat` matching only even numbers (built back via
    `bt`, matched via `seta`), reified as a `Market`. -/
def evens : Market Nat Nat Nat Nat :=
  ⟨id, fun n => if n % 2 == 0 then .inr (n / 2) else .inl n⟩

/-! ### Functor -/

#guard (Functor.map (· + 100) evens).bt 4 == 104
#guard (Functor.map (· + 100) evens).seta 4 == .inr 2
#guard (Functor.map (· + 100) evens).seta 3 == .inl 103

/-! ### Profunctor -/

#guard (Profunctor.rmap (· + 100) evens).bt 4 == 104
#guard (Profunctor.rmap (· + 100) evens).seta 4 == .inr 2
#guard (Profunctor.rmap (· + 100) evens).seta 3 == .inl 103

#guard (Profunctor.lmap (· + 10) evens).seta 4 == .inr 7
#guard (Profunctor.lmap (· + 10) evens).seta 3 == .inl 13

#guard (Profunctor.dimap (· + 10) (· + 100) evens).seta 4 == .inr 7
#guard (Profunctor.dimap (· + 10) (· + 100) evens).seta 3 == .inl 113
#guard (Profunctor.dimap (· + 10) (· + 100) evens).bt 4 == 104

/-! ### Choice -/

#guard (Choice.left' (γ := String) evens).seta (.inl 4) == .inr 2
#guard (Choice.left' (γ := String) evens).seta (.inl 3) == .inl (.inl 3)
#guard (Choice.left' (γ := String) evens).seta (.inr "x") == .inl (.inr "x")
#guard (Choice.left' (γ := String) evens).bt 4 == .inl 4

#guard (Choice.right' (γ := String) evens).seta (.inr 4) == .inr 2
#guard (Choice.right' (γ := String) evens).seta (.inr 3) == .inl (.inr 3)
#guard (Choice.right' (γ := String) evens).seta (.inl "x") == .inl (.inl "x")
#guard (Choice.right' (γ := String) evens).bt 4 == .inr 4

end Tests.Control.Lens.Internal.Prism
