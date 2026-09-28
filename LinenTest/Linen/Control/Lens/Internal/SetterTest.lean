/-
  Tests for `Linen.Control.Lens.Internal.Setter`.

  `Settable Id`: `untainted` recovers the wrapped value; `untaintedDot`/
  `taintedDot` push a `Settable` layer through a `Control.Fun` profunctor.
  `Settable (Compose F G)`: unwraps both layers in turn.
-/
import Linen.Control.Lens.Internal.Setter

open Control Control.Profunctor Data.Functor Control.Lens.Internal

namespace Tests.Control.Lens.Internal.Setter

/-- `Id α` and `α` are defeq but not syntactically the same type, so `BEq`
    instance search can't see through `Id` on its own; this helper's own
    return type forces the unfolding once, for `#guard`'s sake. -/
def unId {α : Type} (a : Id α) : α := a

/-! ### `Settable Id` -/

#guard Settable.untainted (5 : Id Nat) == 5

/-! ### `untaintedDot` / `taintedDot`, over `Control.Fun` -/

def incId : Fun Nat (Id Nat) := ⟨fun n => (n + 1 : Id Nat)⟩
def inc : Fun Nat Nat := ⟨fun n => n + 1⟩

#guard (Settable.untaintedDot incId).apply 5 == 6
#guard unId ((Settable.taintedDot (F := Id) inc).apply 5) == 6

/-! ### `Settable (Compose Id Id)` -/

def cnn : Compose Id Id Nat := ⟨(5 : Id Nat)⟩

#guard Settable.untainted cnn == 5

end Tests.Control.Lens.Internal.Setter
