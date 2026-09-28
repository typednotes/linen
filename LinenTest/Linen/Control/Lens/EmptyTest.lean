/-
  Tests for `Linen.Control.Lens.Empty`.
-/
import Linen.Control.Lens.Empty
import Linen.Control.Lens.Review

open Control.Lens

namespace Tests.Linen.Control.Lens.Empty

-- ── `_Empty`, via `withPrism`/`review` ──────────
-- (matching `Tests.Linen.Control.Lens.Prism`'s own precedent for exercising
-- a `Prism` without a bare-arrow `Getting`/`Setter` bridge.)

#guard withPrism (_Empty (A := List Nat)) (fun _ seta => seta ([] : List Nat)) = Sum.inr ()
#guard withPrism (_Empty (A := List Nat)) (fun _ seta => seta [1, 2]) = Sum.inl [1, 2]
#guard review (_Empty (A := List Nat)) () = ([] : List Nat)

#guard withPrism (_Empty (A := Option Nat)) (fun _ seta => seta (none : Option Nat)) = Sum.inr ()
#guard withPrism (_Empty (A := Option Nat)) (fun _ seta => seta (some 1)) = Sum.inl (some 1)
#guard review (_Empty (A := Option Nat)) () = (none : Option Nat)

#guard withPrism (_Empty (A := String)) (fun _ seta => seta "") = Sum.inr ()
#guard withPrism (_Empty (A := String)) (fun _ seta => seta "hi") = Sum.inl "hi"
#guard review (_Empty (A := String)) () = ""

#guard withPrism (_Empty (A := Array Nat)) (fun _ seta => seta (#[] : Array Nat)) = Sum.inr ()
#guard withPrism (_Empty (A := Array Nat)) (fun _ seta => seta #[1, 2]) = Sum.inl #[1, 2]
#guard review (_Empty (A := Array Nat)) () = (#[] : Array Nat)

end Tests.Linen.Control.Lens.Empty
