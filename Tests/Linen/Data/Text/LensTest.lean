/-
  Tests for `Linen.Data.Text.Lens`.
-/
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Iso
import Linen.Control.Lens.Setter
import Linen.Data.Text.Lens

open Control.Lens

namespace Tests.Linen.Data.Text.Lens

-- ── `packed` / `unpacked` ────────────────────────

#guard withIso packed (fun sa _ => sa "abc") = "abc"
#guard withIso unpacked (fun sa _ => sa "abc") = "abc"

-- ── `Cons` / `Snoc` ──────────────────────────────

#guard cons 'a' "bc" = "abc"
#guard uncons "abc" = some ('a', "bc")
#guard uncons ("" : String) = (none : Option (Char × String))
#guard snoc "ab" 'c' = "abc"
#guard unsnoc "abc" = some ("ab", 'c')
#guard unsnoc ("" : String) = (none : Option (String × Char))

-- ── `Ixed` ───────────────────────────────────────

#guard preview (ix 1) "abc" = some 'b'
#guard preview (ix 9) "abc" = none
#guard over (ix 1) Char.toUpper "abc" = "aBc"

end Tests.Linen.Data.Text.Lens
