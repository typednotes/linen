/-
  Tests for `Linen.Control.Lens.Cons`.
-/
import Linen.Control.Lens.Cons
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Setter

open Control.Lens

namespace Tests.Linen.Control.Lens.Cons

-- ── `_Cons` / `_Snoc` directly, via `withPrism` ─
-- (matching `Tests.Linen.Control.Lens.Prism`'s own precedent for exercising
-- a `Prism` without a bare-arrow `Getting`/`Setter` bridge.)

#guard withPrism (_Cons (S := List Nat) (B := Nat)) (fun _ seta => seta [1, 2, 3]) =
  Sum.inr (1, [2, 3])
#guard withPrism (_Cons (S := List Nat) (B := Nat)) (fun _ seta => seta ([] : List Nat)) =
  Sum.inl ([] : List Nat)
#guard withPrism (_Cons (S := List Nat) (B := Nat)) (fun bt _ => bt (1, [2, 3])) = [1, 2, 3]

#guard withPrism (_Snoc (S := List Nat) (B := Nat)) (fun _ seta => seta [1, 2, 3]) =
  Sum.inr ([1, 2], 3)
#guard withPrism (_Snoc (S := List Nat) (B := Nat)) (fun _ seta => seta ([] : List Nat)) =
  Sum.inl ([] : List Nat)
#guard withPrism (_Snoc (S := Array Nat) (B := Nat)) (fun _ seta => seta #[1, 2, 3]) =
  Sum.inr (#[1, 2], 3)
#guard withPrism (_Snoc (S := Array Nat) (B := Nat)) (fun _ seta => seta (#[] : Array Nat)) =
  Sum.inl (#[] : Array Nat)

-- ── `cons` / `uncons` ────────────────────────────

#guard cons 1 [2, 3] = [1, 2, 3]
#guard uncons ([1, 2, 3] : List Nat) = some (1, [2, 3])
#guard uncons ([] : List Nat) = none

-- ── `snoc` / `unsnoc` ────────────────────────────

#guard snoc [1, 2] 3 = [1, 2, 3]
#guard unsnoc ([1, 2, 3] : List Nat) = some ([1, 2], 3)
#guard unsnoc ([] : List Nat) = none

#guard snoc (#[1, 2] : Array Nat) 3 = #[1, 2, 3]
#guard unsnoc (#[1, 2, 3] : Array Nat) = some (#[1, 2], 3)
#guard unsnoc (#[] : Array Nat) = none

-- ── `_head` / `_tail` / `_init` / `_last` ────────

#guard preview _head ([1, 2, 3] : List Nat) = some 1
#guard preview _head ([] : List Nat) = none
#guard preview _tail ([1, 2, 3] : List Nat) = some [2, 3]
#guard preview _tail ([] : List Nat) = none
#guard preview _init ([1, 2, 3] : List Nat) = some [1, 2]
#guard preview _init ([] : List Nat) = none
#guard preview _last ([1, 2, 3] : List Nat) = some 3
#guard preview _last ([] : List Nat) = none

#guard (over _head (· + 10) ([1, 2, 3] : List Nat)) = [11, 2, 3]
#guard (over _head (· + 10) ([] : List Nat)) = []
#guard (over _last (· + 10) ([1, 2, 3] : List Nat)) = [1, 2, 13]

end Tests.Linen.Control.Lens.Cons
