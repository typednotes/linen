/-
  Tests for `Linen.Data.Bits.Lens`.
-/
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Indexed
import Linen.Control.Lens.Setter
import Linen.Data.Bits.Lens

open Control.Lens Control.Lens.Internal

namespace Tests.Linen.Data.Bits.Lens

-- ── `bitAt` ──────────────────────────────────────

#guard preview (bitAt 0) (1 : UInt8) = some true
#guard preview (bitAt 1) (1 : UInt8) = some false
#guard set (bitAt 1) true (1 : UInt8) = 3
#guard set (bitAt 0) false (1 : UInt8) = 0
#guard over (bitAt 0) not (1 : UInt8) = 0

-- ── `bits` — an `IndexedTraversal'`, run at `P := Indexed Nat` ──

#guard (itoListOf bits (1 : UInt8)).take 3 = [(0, true), (1, false), (2, false)]

#guard Id.run (bits (F := Id) (P := Indexed Nat) (Indexed.mk (fun _ b => not b)) (0 : UInt8))
  = (255 : UInt8)

end Tests.Linen.Data.Bits.Lens
