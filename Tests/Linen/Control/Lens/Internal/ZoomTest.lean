/-
  Tests for `Linen.Control.Lens.Internal.Zoom`.

  `Focusing Id (List Nat)`: `Functor`/`Pure`/`Seq` thread the sub-state
  (here, a `List Nat` combined with `++`) alongside the result.
-/
import Linen.Control.Lens.Internal.Zoom

open Control.Lens.Internal

namespace Tests.Control.Lens.Internal.Zoom

/-- `Id α` and `α` are defeq but not syntactically the same type, so `BEq`
    instance search can't see through `Id` on its own; this helper's own
    return type forces the unfolding once, for `#guard`'s sake. -/
def unId {α : Type} (a : Id α) : α := a

def foc1 : Focusing Id (List Nat) Nat := ⟨(([1], 10) : List Nat × Nat)⟩
def foc2 : Focusing Id (List Nat) Nat := ⟨(([2], 20) : List Nat × Nat)⟩

/-! ### `Functor`: maps the result, leaves the state untouched -/

#guard unId (Functor.map (· + 1) foc1).runFocusing == ([1], 11)

/-! ### `Pure`: contributes the empty (`Inhabited`) state -/

#guard unId (Pure.pure 5 : Focusing Id (List Nat) Nat).runFocusing == ([], 5)

/-! ### `Seq`: combines both states with `++`, applies the function -/

#guard unId (Seq.seq (Functor.map (fun a b => a + b) foc1) (fun _ => foc2)).runFocusing ==
  ([1, 2], 30)

end Tests.Control.Lens.Internal.Zoom
