/-
  Tests for `Linen.Control.Lens.Extras`.
-/
import Linen.Control.Lens.Extras

open Control.Lens

namespace Tests.Linen.Control.Lens.Extras

-- `is _Left` matches `Sum.inl` values and rejects `Sum.inr` ones.
#guard is (_Left (A := Nat) (B := Nat) (C := Nat)) (Sum.inl 12) = true
#guard is (_Left (A := Nat) (B := Nat) (C := Nat)) (Sum.inr 12) = false

-- `is _Right` matches `Sum.inr` values and rejects `Sum.inl` ones.
#guard is (_Right (A := Nat) (B := Nat) (C := Nat)) (Sum.inr 12) = true
#guard is (_Right (A := Nat) (B := Nat) (C := Nat)) (Sum.inl 12) = false

-- `is _Just` matches `some` and rejects `none`.
#guard is (_Just (A := Nat) (B := Nat)) (some 3) = true
#guard is (_Just (A := Nat) (B := Nat)) (none : Option Nat) = false

-- `is _Nothing` matches `none` and rejects `some`.
#guard is (_Nothing (A := Nat)) (none : Option Nat) = true
#guard is (_Nothing (A := Nat)) (some 3) = false

end Tests.Linen.Control.Lens.Extras
