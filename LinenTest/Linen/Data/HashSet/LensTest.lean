/-
  Tests for `Linen.Data.HashSet.Lens`.
-/
import Linen.Control.Lens.Empty
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Prism
import Linen.Control.Lens.Setter
import Linen.Data.HashSet.Lens
import Std.Data.HashSet

open Control.Lens

namespace Tests.Linen.Data.HashSet.Lens

-- ── `Ixed` ───────────────────────────────────────

#guard preview (ix 1) (Std.HashSet.ofList [1, 2, 3]) = some ()
#guard preview (ix 9) (Std.HashSet.ofList [1, 2, 3]) = none

-- ── `At` ─────────────────────────────────────────

#guard ((Std.HashSet.ofList ([1, 2, 3] : List Nat)) ^. «at» 1) = some ()
#guard ((Std.HashSet.ofList ([1, 2, 3] : List Nat)) ^. «at» 9) = none
#guard (((«at» (9 : Nat) .~ some ()) (Std.HashSet.ofList [1, 2, 3])).contains 9) = true
#guard (((«at» (1 : Nat) .~ none) (Std.HashSet.ofList [1, 2, 3])).contains 1) = false

-- ── `AsEmpty` ────────────────────────────────────

#guard withPrism _Empty (fun _ seta =>
  match seta (Std.HashSet.ofList ([] : List Nat)) with | .inr () => true | .inl _ => false)
#guard withPrism _Empty (fun _ seta =>
  match seta (Std.HashSet.ofList ([1] : List Nat)) with | .inr () => false | .inl _ => true)

-- ── `Each` ───────────────────────────────────────

#guard ((over each (· + 1) (Std.HashSet.ofList ([1, 2, 3] : List Nat))).contains 2) = true
#guard ((over each (· + 1) (Std.HashSet.ofList ([1, 2, 3] : List Nat))).contains 1) = false

end Tests.Linen.Data.HashSet.Lens
