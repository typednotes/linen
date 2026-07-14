/-
  Tests for `Linen.Control.Lens.Internal.Review`.

  `Reviewable`, `retagged`, exercised over `Tagged` (given a local
  `Bifunctor` instance here, since `Linen.Control.Profunctor.Types` — which
  already gives `Tagged` a `Profunctor` instance — has no reason of its own
  to also give it a `Bifunctor` one; `Tagged`'s first parameter is exactly
  the phantom "unreadable" slot `Reviewable`/`retagged` are about).
-/
import Linen.Control.Lens.Internal.Review
import Linen.Control.Profunctor.Types

open Control Control.Profunctor Control.Lens.Internal

namespace Tests.Control.Lens.Internal.Review

instance : Data.Bifunctor Tagged where
  bimap _ g t := ⟨g t.unTagged⟩

-- `Reviewable` is a pure constraint synonym with no method of its own; it
-- just certifies that `Tagged` (already `Profunctor` and, per the local
-- instance above, `Bifunctor`) is usable wherever `Reviewable` is required.
example : Reviewable Tagged := inferInstance

#guard (Profunctor.dimap (P := Tagged) (· + 1) (· * 10) ⟨5⟩ : Tagged Nat Nat).unTagged == 50

def t : Tagged Bool Nat := ⟨7⟩

#guard (retagged (S := String) t : Tagged String Nat).unTagged == 7
#guard (retagged (S := Nat) t : Tagged Nat Nat).unTagged == 7

example : (retagged (S := String) t : Tagged String Nat) = ⟨7⟩ := rfl

end Tests.Control.Lens.Internal.Review
