/-
  Tests for `Linen.Control.Monad.Trans`.

  Covers `lift` (Lean's `monadLift`) over the three transformers it already
  has instances for — `ExceptT`, `ReaderT`, `StateT` — plus the generic
  `lift_pure`/`lift_bind` laws.
-/
import Linen.Control.Monad.Trans

open Control.Monad.Trans

namespace Tests.Control.Monad.Trans

-- `lift` into `ExceptT` wraps the inner value as a success.
#guard (Id.run ((lift (pure 5 : Id Nat) : ExceptT String Id Nat).run)).toOption == some 5

-- `lift` into `ReaderT` ignores the environment.
#guard Id.run ((lift (pure 5 : Id Nat) : ReaderT Nat Id Nat).run 99) == 5

-- `lift` into `StateT` threads the state through unchanged.
#guard Id.run ((lift (pure 5 : Id Nat) : StateT Nat Id Nat).run 7) == (5, 7)

-- Reduction laws (checked at compile time), specialised to each transformer.
example (a : Nat) : (lift (pure a : Id Nat) : ExceptT String Id Nat) = pure a := lift_pure a
example (a : Nat) : (lift (pure a : Id Nat) : ReaderT Nat Id Nat) = pure a := lift_pure a
example (a : Nat) : (lift (pure a : Id Nat) : StateT Nat Id Nat) = pure a := lift_pure a

example (ma : Id Nat) (f : Nat → Id Nat) :
    (lift (ma >>= f) : ExceptT String Id Nat) = lift ma >>= fun a => lift (f a) :=
  lift_bind ma f

end Tests.Control.Monad.Trans
