/-
  Tests for `Linen.Control.Lens.Prism`.
-/
import Linen.Control.Lens.Prism

open Control.Lens

namespace Tests.Linen.Control.Lens.Prism

-- `prism`/`prism'` build a `Prism` from "build" and "match" functions.

def posPrism : Prism' Int Nat :=
  prism' (fun n : Nat => (n : Int)) (fun i => if i ≥ 0 then some i.toNat else none)

#guard withPrism posPrism (fun bt _ => bt 3) = (3 : Int)
#guard withPrism posPrism (fun _ seta => seta 5) = Sum.inr 5
#guard withPrism posPrism (fun _ seta => seta (-1)) = Sum.inl (-1)

-- `clonePrism` rebuilds an equivalent `Prism`.
#guard withPrism (clonePrism posPrism) (fun bt _ => bt 3) = (3 : Int)

-- `isoAsPrism`: any `Iso` is usable as a `Prism`.
def notIso : Iso' Bool Bool := iso not not
#guard withPrism (isoAsPrism notIso) (fun bt _ => bt true) = false

-- `_Left` / `_Right`: prisms onto the two cases of `Sum`.
#guard withPrism (_Left (A := Nat) (B := Nat) (C := Nat)) (fun bt _ => bt 3) = Sum.inl (3 : Nat)
#guard withPrism (_Left (A := Nat) (B := Nat) (C := Nat)) (fun _ seta => seta (Sum.inl 3)) = Sum.inr 3
#guard withPrism (_Left (A := Nat) (B := Nat) (C := Nat)) (fun _ seta => seta (Sum.inr 7)) = Sum.inl (Sum.inr 7)

#guard withPrism (_Right (A := Nat) (B := Nat) (C := Nat)) (fun bt _ => bt 3) = Sum.inr (3 : Nat)
#guard withPrism (_Right (A := Nat) (B := Nat) (C := Nat)) (fun _ seta => seta (Sum.inr 3)) = Sum.inr 3
#guard withPrism (_Right (A := Nat) (B := Nat) (C := Nat)) (fun _ seta => seta (Sum.inl 7)) = Sum.inl (Sum.inl 7)

-- `_Just` / `_Nothing`: prisms onto the two cases of `Option`.
#guard withPrism (_Just (A := Nat) (B := Nat)) (fun bt _ => bt 3) = some (3 : Nat)
#guard withPrism (_Just (A := Nat) (B := Nat)) (fun _ seta => seta (some 3)) = Sum.inr 3
#guard withPrism (_Just (A := Nat) (B := Nat)) (fun _ seta => seta none) = Sum.inl none

#guard withPrism (_Nothing (A := Nat)) (fun _ seta => seta (none : Option Nat)) = Sum.inr ()
#guard withPrism (_Nothing (A := Nat)) (fun _ seta => seta (some 3)) = Sum.inl (some 3)

-- `only`: a `Prism'` matching exactly one value.
#guard withPrism (only (3 : Nat)) (fun _ seta => seta 3) = Sum.inr ()
#guard withPrism (only (3 : Nat)) (fun _ seta => seta 4) = Sum.inl 4

-- `nearly`: like `only`, but matching an arbitrary predicate.
#guard withPrism (nearly (0 : Nat) (fun n => n % 2 = 0)) (fun _ seta => seta 4) = Sum.inr ()
#guard withPrism (nearly (0 : Nat) (fun n => n % 2 = 0)) (fun _ seta => seta 5) = Sum.inl 5

end Tests.Linen.Control.Lens.Prism
