/-
  Tests for `Linen.Control.Lens.Wrapped`.
-/
import Linen.Control.Lens.Wrapped

open Control.Lens

namespace Tests.Linen.Control.Lens.Wrapped

-- `_Wrapped'`/`_Unwrapped'` round-trip through each monoid wrapper's single
-- field. (`Dual`/`Sum`/`Product`/`All`/`Any` derive `BEq`, not
-- `DecidableEq`, so wrapper-to-wrapper comparisons below use `==` rather
-- than `=`.)

#guard withIso (_Wrapped' (S := Data.Dual Nat)) (fun sa _ => sa ⟨3⟩) = 3
#guard withIso (_Wrapped' (S := Data.Dual Nat)) (fun _ bt => bt 3) == (⟨3⟩ : Data.Dual Nat)
#guard withIso (_Unwrapped' (S := Data.Dual Nat)) (fun sa _ => sa 3) == (⟨3⟩ : Data.Dual Nat)

#guard withIso (_Wrapped' (S := Data.Sum Nat)) (fun sa _ => sa ⟨5⟩) = 5
#guard withIso (_Wrapped' (S := Data.Sum Nat)) (fun _ bt => bt 5) == (⟨5⟩ : Data.Sum Nat)

#guard withIso (_Wrapped' (S := Data.Product Nat)) (fun sa _ => sa ⟨4⟩) = 4
#guard withIso (_Wrapped' (S := Data.Product Nat)) (fun _ bt => bt 4) == (⟨4⟩ : Data.Product Nat)

#guard withIso (_Wrapped' (S := Data.All)) (fun sa _ => sa ⟨true⟩) = true
#guard withIso (_Wrapped' (S := Data.All)) (fun _ bt => bt false) == (⟨false⟩ : Data.All)

#guard withIso (_Wrapped' (S := Data.Any)) (fun sa _ => sa ⟨false⟩) = false
#guard withIso (_Wrapped' (S := Data.Any)) (fun _ bt => bt true) == (⟨true⟩ : Data.Any)

-- `_Wrapped`/`_Unwrapped`: the polymorphic, type-changing versions, here
-- instantiated at the same wrapper on both sides.

#guard withIso (_Wrapped (S := Data.Sum Nat) (T := Data.Sum Nat)) (fun sa _ => sa ⟨7⟩) = 7
#guard withIso (_Unwrapped (S := Data.Sum Nat) (T := Data.Sum Nat)) (fun sa _ => sa 7)
    == (⟨7⟩ : Data.Sum Nat)

-- `_Wrapping'`/`_Unwrapping'`/`_Wrapping`/`_Unwrapping`: the
-- constructor-pinned convenience forms (the constructor argument is
-- ignored, only its type matters).

#guard withIso (_Wrapping' Data.Sum.mk) (fun sa _ => sa ⟨9⟩) = 9
#guard withIso (_Unwrapping' Data.Sum.mk) (fun sa _ => sa 9) == (⟨9⟩ : Data.Sum Nat)
#guard withIso (_Wrapping (T := Data.Sum Nat) Data.Sum.mk) (fun sa _ => sa ⟨2⟩) = 2
#guard withIso (_Unwrapping (S := Data.Sum Nat) (T := Data.Sum Nat) Data.Sum.mk) (fun sa _ => sa 2)
    == (⟨2⟩ : Data.Sum Nat)

-- `op`: the constructor/deconstructor pair.
#guard op Data.Sum.mk (⟨11⟩ : Data.Sum Nat) = 11
#guard op Data.All.mk (⟨true⟩ : Data.All) = true

-- `ala`/`alaf`: fold a list through a wrapper constructor and unwrap the
-- result — the classic "sum via `Sum`" idiom.

def sumViaAla (xs : List Nat) : Nat :=
  ala (S := Data.Sum Nat) (T := Data.Sum Nat) (F := Id) Data.Sum.mk
    (show (Nat → Data.Sum Nat) → Data.Sum Nat from
      fun f => (xs.map f).foldl (· ++ ·) ⟨0⟩)

#guard sumViaAla [1, 2, 3, 4] = 10

def allViaAla (xs : List Bool) : Bool :=
  ala (S := Data.All) (T := Data.All) (F := Id) Data.All.mk
    (show (Bool → Data.All) → Data.All from
      fun f => (xs.map f).foldl (· ++ ·) ⟨true⟩)

#guard allViaAla [true, true] = true
#guard allViaAla [true, false] = false

def sumOfLengthsViaAlaf (xs : List String) : Nat :=
  alaf (S := Data.Sum Nat) (T := Data.Sum Nat) (F := List) (G := Id) Data.Sum.mk
    (show List (Data.Sum Nat) → Data.Sum Nat from
      fun ts => ts.foldl (· ++ ·) ⟨0⟩)
    (xs.map String.length)

#guard sumOfLengthsViaAlaf ["hello", "world"] = 10

end Tests.Linen.Control.Lens.Wrapped
