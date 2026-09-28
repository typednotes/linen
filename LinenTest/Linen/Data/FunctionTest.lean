/-
  Tests for `Linen.Data.Function`.

  `on` and `applyTo` (the additions); `flip`/`const` already have a stdlib
  spelling, exercised below to document that they aren't re-ported.
-/
import Linen.Data.Function

open Data.Function

namespace Tests.Data.Function

/-! ### applyTo (flip apply) -/

#guard applyTo 5 (· + 1) == 6
#guard applyTo "ab" String.length == 2
#guard applyTo (applyTo 5 (· * 2)) (· + 1) == 11                       -- (5·2) then +1
-- the point-free use: apply one value across a list of functions
#guard ([(· + 1), (· * 2), (· - 3)] : List (Nat → Nat)).map (applyTo 10) == [11, 20, 7]

/-! ### on (combine by a projection) -/

#guard on (· + ·) (· * 2) 3 4 == 14                                    -- (3·2) + (4·2)
#guard on (· == ·) String.length "abc" "xyz" == true                   -- equal lengths

/-! ### Laws (compile-time) -/

example (x : Nat) (f : Nat → Nat) : applyTo x f = f x := applyTo_apply x f
example (f : β → β → γ) (g : α → β) (x y : α) : on f g x y = f (g x) (g y) := on_apply f g x y

/-! ### Already in core — not re-ported -/

#guard flip (· - ·) 1 10 == 9                                          -- 10 − 1
#guard Function.const String 7 "ignored" == 7                          -- type-first `const`

end Tests.Data.Function
