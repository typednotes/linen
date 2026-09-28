/-
  Tests for `Linen.Control.Lens.At`.
-/
import Linen.Control.Lens.At
import Linen.Control.Lens.Fold

open Control.Lens

namespace Tests.Linen.Control.Lens.At

-- ── `Ixed (E → A)` — functions ──────────────────

#guard ((fun n => n + 1) ^? ix 3) = some 4
#guard (ix 3 .~ 100) (fun n => n + 1) 3 = 100
#guard (ix 3 .~ 100) (fun n => n + 1) 2 = 3

-- ── `Ixed (List A)` ─────────────────────────────

#guard ([10, 20, 30] ^? ix 1) = some 20
#guard ([10, 20, 30] ^? ix 5) = none
#guard (ix 1 .~ 99) [10, 20, 30] = [10, 99, 30]
#guard (ix 5 .~ 99) [10, 20, 30] = [10, 20, 30]
#guard (ix 1 %~ (· + 1)) [10, 20, 30] = [10, 21, 30]

-- ── `Ixed (Option A)` ───────────────────────────

#guard ((some 5 : Option Nat) ^? ix ()) = some 5
#guard ((none : Option Nat) ^? ix ()) = none
#guard (ix () .~ 9) (some 5 : Option Nat) = some 9
#guard (ix () .~ 9) (none : Option Nat) = none

-- ── `At (Option A)` — `«at» ()` is the identity lens ──

#guard ((some 5 : Option Nat) ^. «at» ()) = some 5
#guard ((none : Option Nat) ^. «at» ()) = none
#guard («at» () .~ (some 9 : Option Nat)) (some 5 : Option Nat) = some 9
#guard («at» () .~ (none : Option Nat)) (some 5 : Option Nat) = none

-- ── `sans` — delete via `At` ────────────────────

#guard sans () (some 5 : Option Nat) = none
#guard sans () (none : Option Nat) = none

-- ── `ixAt` — `ix` derived from `«at»` alone ───────

#guard ((some 5 : Option Nat) ^? ixAt ()) = ((some 5 : Option Nat) ^? ix ())
#guard ((none : Option Nat) ^? ixAt ()) = ((none : Option Nat) ^? ix ())
#guard (ixAt () .~ 9) (some 5 : Option Nat) = (ix () .~ 9) (some 5 : Option Nat)
#guard (ixAt () .~ 9) (none : Option Nat) = (ix () .~ 9) (none : Option Nat)

end Tests.Linen.Control.Lens.At
