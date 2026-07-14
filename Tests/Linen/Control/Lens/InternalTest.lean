/-
  Tests for `Linen.Control.Lens.Internal` (the facade re-exporting every
  `Control.Lens.Internal.*` module in this batch).

  Since the facade's entire content is its `import` list, this test just
  checks a representative name from each re-exported module resolves through
  a bare `import Linen.Control.Lens.Internal`.
-/
import Linen.Control.Lens.Internal
import Linen.Control.Profunctor.Types

open Control Control.Profunctor Data.Functor Control.Lens.Internal

namespace Tests.Control.Lens.Internal

/-! ### `Indexed`, `Profunctor` -/

def ix : Indexed Nat Nat Nat := ⟨fun i a => i + a⟩
#guard (ix.atIndex 10).apply 5 == 15

def wp : WrappedPafb Option Fun Nat Nat := ⟨⟨fun n => some (n + 1)⟩⟩
#guard wp.unwrapPafb.apply 5 == some 6

/-! ### `Context` -/

def c : Context Nat Nat Nat := ⟨(· + 1), 5⟩
#guard Context.extract c == 6

/-! ### `Magma` -/

def tree : Magma Nat Nat Nat Nat := .leaf 0 41
#guard Magma.run tree == 41

/-! ### `Bazaar` -/

def bz : Bazaar Fun Nat Nat Nat := Bazaar.sell 5
example : bz.runBazaar (F := Id) ⟨fun a => (a + 1 : Id Nat)⟩ = (6 : Id Nat) := rfl

/-! ### `Iso` (`Exchange`) -/

def ex : Exchange Nat Nat Nat Nat := ⟨(· + 1), (· * 10)⟩
#guard ex.sa 5 == 6
#guard ex.bt 5 == 50

/-! ### `Prism` (`Market`) -/

def mk : Market Nat Nat Nat Nat := ⟨(· + 1), fun s => if s > 0 then .inr s else .inl 0⟩
#guard mk.bt 5 == 6

/-! ### `Review` (`Reviewable`, `retagged`) -/

instance : Data.Bifunctor Tagged where
  bimap _ g t := ⟨g t.unTagged⟩

example : Reviewable Tagged := inferInstance
#guard (retagged (S := String) (⟨7⟩ : Tagged Bool Nat) : Tagged String Nat).unTagged == 7

/-! ### `Getter` (`noEffect`) -/

/-- A minimal `Contravariant`/`Pure` functor, matching `GetterTest`'s. -/
structure ConstBool (α : Type) where
  val : Bool

instance : Contravariant ConstBool where
  contramap _ c := ⟨c.val⟩

instance : Pure ConstBool where
  pure _ := ⟨true⟩

#guard (noEffect : ConstBool Nat).val == true

/-! ### `Fold` (`Folding`) -/

def fold5 : Folding Option Nat := ⟨some 5⟩
#guard fold5.runFolding == some 5

/-! ### `Setter` (`Settable`) -/

#guard Settable.untainted (5 : Id Nat) == 5

/-! ### `Level` -/

def lvl : Level Nat Nat := .one 0 5
#guard lvl.size == 1

/-! ### `Deque` -/

def dq : Deque Nat := Deque.fromList [1, 2, 3]
#guard dq.toList == [1, 2, 3]

/-! ### `List` (`ordinalNub`) -/

#guard Control.Lens.Internal.ordinalNub 3 [-1, 2, 1, 4, 2, 3] == [2, 1]

/-! ### `Zoom` (`Focusing`) -/

def fq : Focusing Id Nat Nat := ⟨(5, 10)⟩
example : fq.runFocusing = ((5, 10) : Nat × Nat) := rfl

end Tests.Control.Lens.Internal
