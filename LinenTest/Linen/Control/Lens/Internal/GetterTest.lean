/-
  Tests for `Linen.Control.Lens.Internal.Getter`.

  `noEffect`: the `mempty` equivalent for a `Contravariant` `Applicative`
  functor — checked against a small local contravariant/`Pure` functor
  whose covariant slot is entirely phantom.
-/
import Linen.Control.Lens.Internal.Getter

open Data.Functor Control.Lens.Internal

namespace Tests.Control.Lens.Internal.Getter

/-- A minimal `Contravariant`/`Pure` functor: carries a `Bool`, ignoring its
    (phantom) type parameter entirely — just enough to exercise `noEffect`
    without needing a full `Data.Functor.Const`-style instance. -/
structure ConstBool (α : Type) where
  val : Bool

instance : Contravariant ConstBool where
  contramap _ c := ⟨c.val⟩

instance : Pure ConstBool where
  pure _ := ⟨true⟩

/-! `noEffect` produces the same `ConstBool` no matter which phantom type is
    requested. -/

#guard (noEffect : ConstBool Nat).val == true
#guard (noEffect : ConstBool String).val == true

end Tests.Control.Lens.Internal.Getter
