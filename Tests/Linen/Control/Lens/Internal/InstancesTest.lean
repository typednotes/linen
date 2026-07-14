/-
  Tests for `Linen.Control.Lens.Internal.Instances`.

  `Data.Traversable Id` and `Data.Traversable (Data.Functor.Const α)`.
-/
import Linen.Control.Lens.Internal.Instances

open Control.Lens.Internal

namespace Tests.Control.Lens.Internal.Instances

example : Data.Traversable.traverse (T := Id) (fun n => some (n + 1)) (5 : Id Nat) =
    (some 6 : Option Nat) := rfl

#guard (Data.Traversable.traverse (T := Data.Functor.Const String)
  (fun n => some (n + 1)) (⟨"const"⟩ : Data.Functor.Const String Nat)) == some ⟨"const"⟩

end Tests.Control.Lens.Internal.Instances
