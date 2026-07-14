/-
  Tests for `Linen.Data.Map.Lens`.
-/
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Indexed
import Linen.Control.Lens.Setter
import Linen.Data.Map.Lens

open Control.Lens Control.Lens.Internal
open Data (Map)

namespace Tests.Linen.Data.Map.Lens

-- ── `Ixed` ───────────────────────────────────────

#guard preview (ix "a") (Data.Map.fromList [("a", 1), ("b", 2)]) = some 1
#guard preview (ix "z") (Data.Map.fromList [("a", 1), ("b", 2)]) = none
#guard over (ix "a") (· + 10) (Data.Map.fromList [("a", 1), ("b", 2)])
  == Data.Map.fromList [("a", 11), ("b", 2)]

-- ── `At` ─────────────────────────────────────────

#guard ((Data.Map.fromList [("a", 1)] : Map String Nat) ^. «at» "a") = some 1
#guard ((Data.Map.fromList [("a", 1)] : Map String Nat) ^. «at» "z") = none
#guard (((«at» "b" .~ some 2) (Data.Map.fromList [("a", 1)] : Map String Nat)).find? "b") = some 2
#guard (((«at» "a" .~ none) (Data.Map.fromList [("a", 1)] : Map String Nat)).find? "a") = none

-- ── `toMapOf` ────────────────────────────────────
-- (indexed by list position, via `ifolded : IndexedFold Nat (List A) A`)

#guard toMapOf ifolded [10, 20, 30] == Data.Map.fromList [(0, 10), (1, 20), (2, 30)]

end Tests.Linen.Data.Map.Lens
