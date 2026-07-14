/-
  Tests for `Linen.Control.Lens.Internal.Bazaar`.

  `Bazaar`/`BazaarT`: `Functor`, `Applicative`, `Bizarre`, `sell`.
-/
import Linen.Control.Lens.Internal.Bazaar

open Control Control.Lens.Internal

namespace Tests.Control.Lens.Internal.Bazaar

/-- The "visitor" every test runs the reified traversal against: increment
    the visited element, in the identity applicative. -/
def visit : Fun Nat (Id Nat) := ⟨fun a => a + 1⟩

/-! ### `sell` / `runBazaar` -/

def bz : Bazaar Fun Nat Nat Nat := Bazaar.sell 5

example : bz.runBazaar (F := Id) visit = (6 : Id Nat) := rfl

/-! ### `Functor` -/

example : (Functor.map (· * 10) bz).runBazaar (F := Id) visit = (60 : Id Nat) := rfl

/-! ### `Applicative` -/

def bzPure : Bazaar' Fun Nat Nat := Pure.pure 42

example : bzPure.runBazaar (F := Id) visit = (42 : Id Nat) := rfl

def bzAp : Bazaar Fun Nat Nat Nat := (Pure.pure (· + 1) : Bazaar Fun Nat Nat (Nat → Nat)) <*> bz

example : bzAp.runBazaar (F := Id) visit = (7 : Id Nat) := rfl

/-! ### `Bizarre` -/

example : @Bizarre.bazaar Fun (Bazaar Fun) inferInstance _ Id _ Nat Nat Nat visit bz =
    (6 : Id Nat) := rfl

/-! ### `BazaarT` -/

def bt : BazaarT Fun Option Nat Nat Nat := BazaarT.sell 5

example : bt.runBazaarT (F := Id) visit = (6 : Id Nat) := rfl

example : (Functor.map (· + 100) bt).runBazaarT (F := Id) visit = (106 : Id Nat) := rfl

def btAp : BazaarT Fun Option Nat Nat Nat :=
  (Pure.pure (· + 1) : BazaarT Fun Option Nat Nat (Nat → Nat)) <*> bt

example : btAp.runBazaarT (F := Id) visit = (7 : Id Nat) := rfl

example : @Bizarre.bazaar Fun (BazaarT Fun Option) inferInstance _ Id _ Nat Nat Nat visit bt =
    (6 : Id Nat) := rfl

end Tests.Control.Lens.Internal.Bazaar
