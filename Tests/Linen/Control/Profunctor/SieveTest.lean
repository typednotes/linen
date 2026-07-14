/-
  Tests for `Linen.Control.Profunctor.Sieve`.

  `Sieve`/`Cosieve` over `Control.Fun`, `Star`/`Costar`, and `Forget`.
-/
import Linen.Control.Profunctor.Sieve
import Linen.Data.Functor

open Control Control.Profunctor Data.Functor

namespace Tests.Control.Profunctor.Sieve

/-! ### Sieve: Control.Fun (via `Id`) -/

def inc : Fun Nat Nat := ⟨(· + 1)⟩

example : (Sieve.sieve inc 5 : Id Nat) = 6 := rfl

/-! ### Sieve: Star -/

def starInc : Star Option Nat Nat := ⟨fun n => some (n + 1)⟩

#guard Sieve.sieve starInc 5 == some 6

/-! ### Sieve: Forget (via `Const`) -/

def forgetLen : Forget Nat String Bool := ⟨String.length⟩

#guard (Sieve.sieve forgetLen "ab" : Const Nat Bool).getConst == 2

/-! ### Cosieve: Control.Fun (via `Id`) -/

example : Cosieve.cosieve inc (5 : Id Nat) = 6 := rfl

/-! ### Cosieve: Costar -/

def costarSum : Costar List Nat Nat := ⟨List.foldl (· + ·) 0⟩

#guard Cosieve.cosieve costarSum [1, 2, 3] == 6

end Tests.Control.Profunctor.Sieve
