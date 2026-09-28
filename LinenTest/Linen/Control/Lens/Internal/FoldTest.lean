/-
  Tests for `Linen.Control.Lens.Internal.Fold`.

  `Folding`: sequences a `Contravariant`/`Applicative` action with `*>`,
  discarding the earlier result — checked against `Option`. The identity
  element (`Inhabited`, via `noEffect`) is checked against the same local
  contravariant/`Pure` functor `GetterTest` uses.
-/
import Linen.Control.Lens.Internal.Fold

open Data.Functor Control.Lens.Internal

namespace Tests.Control.Lens.Internal.Fold

/-! ### `Append`, over `Option` -/

def f1 : Folding Option Nat := ⟨some 1⟩
def f2 : Folding Option Nat := ⟨some 2⟩
def fNone : Folding Option Nat := ⟨none⟩

#guard (f1 ++ f2).runFolding == some 2
#guard (fNone ++ f2).runFolding == none
#guard (f1 ++ fNone).runFolding == none

/-! ### `Inhabited` (the `Append` identity element, via `noEffect`) -/

/-- A minimal `Contravariant`/`Pure` functor, matching `GetterTest`'s. -/
structure ConstBool (α : Type) where
  val : Bool

instance : Contravariant ConstBool where
  contramap _ c := ⟨c.val⟩

instance : Pure ConstBool where
  pure _ := ⟨true⟩

#guard (default : Folding ConstBool Nat).runFolding.val == true

end Tests.Control.Lens.Internal.Fold
