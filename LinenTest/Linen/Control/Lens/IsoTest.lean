/-
  Tests for `Linen.Control.Lens.Iso`.
-/
import Linen.Control.Lens.Iso

open Control.Lens

namespace Tests.Linen.Control.Lens.Iso

/-- A self-inverse `Iso'` between `Bool` and `Bool`, via `not`. -/
def notIso : Iso' Bool Bool := iso not not

#guard withIso notIso (fun sa _ => sa true) = false
#guard withIso notIso (fun sa _ => sa false) = true
#guard withIso notIso (fun _ bt => bt true) = false

/-- An `Iso'` between `Bool × Nat` and `Nat × Bool`, via `Prod.swap`. -/
def swapIso : Iso' (Bool × Nat) (Nat × Bool) := iso Prod.swap Prod.swap

#guard withIso swapIso (fun sa _ => sa (true, 3)) = (3, true)
#guard withIso swapIso (fun _ bt => bt (3, true)) = (true, 3)

-- `«from»` reverses an `Iso`'s two directions.
#guard withIso («from» swapIso) (fun sa _ => sa (3, true)) = (true, 3)

-- `cloneIso` rebuilds a fresh, equivalent `Iso`.
#guard withIso (cloneIso swapIso) (fun sa _ => sa (true, 3)) = (3, true)

-- `under`: conjugating the identity function by an `Iso` recovers the
-- identity (running one direction then immediately back the other).
#guard under swapIso id (3, true) = (3, true)

-- `au`: build an `f a` out of an action producing `f s` from the backward
-- direction.
#guard au swapIso (F := List) (fun bt => [bt (3, true)]) = [(3, true)]

-- `auf`: like `au`, but also maps the backward direction over an `f b`
-- first.
#guard auf swapIso (F := List) (G := List) id [(3, true)] = [(3, true)]

-- `mapping`: lift an `Iso` to act underneath a shared functor (`List` here).
#guard withIso (mapping (F := List) (G := List) swapIso)
    (fun sa _ => sa [(true, 3), (false, 5)]) = [(3, true), (5, false)]

end Tests.Linen.Control.Lens.Iso
