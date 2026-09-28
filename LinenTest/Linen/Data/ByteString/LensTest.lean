/-
  Tests for `Linen.Data.ByteString.Lens`.
-/
import Linen.Control.Lens.Empty
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Iso
import Linen.Control.Lens.Prism
import Linen.Control.Lens.Setter
import Linen.Data.ByteString.Lens

open Control.Lens

namespace Tests.Linen.Data.ByteString.Lens

-- ── `packedBytes` / `unpackedBytes` ──────────────
-- (run directly via `withIso`, since `Iso` is genuinely
-- profunctor-polymorphic and does not unify with `view`'s bare `Getting`
-- shape without an explicit instantiation — see `Linen.Control.Lens.Iso.
-- withIso`'s own doc comment.)

#guard withIso packedBytes (fun sa _ => sa [1, 2, 3]) == Data.ByteString.pack [1, 2, 3]
#guard withIso unpackedBytes (fun sa _ => sa (Data.ByteString.pack [1, 2, 3])) = ([1, 2, 3] : List UInt8)

-- ── `packedChars` / `unpackedChars` ──────────────

#guard withIso unpackedChars (fun sa _ => withIso packedChars (fun sa' _ => sa (sa' "abc"))) = "abc"

-- ── `Cons` / `Snoc` ──────────────────────────────

#guard cons (1 : UInt8) (Data.ByteString.pack [2, 3]) == Data.ByteString.pack [1, 2, 3]
#guard uncons (Data.ByteString.pack ([1, 2, 3] : List UInt8)) == some ((1 : UInt8), Data.ByteString.pack [2, 3])
#guard snoc (Data.ByteString.pack ([1, 2] : List UInt8)) (3 : UInt8) == Data.ByteString.pack [1, 2, 3]
#guard unsnoc (Data.ByteString.pack ([1, 2, 3] : List UInt8)) == some (Data.ByteString.pack [1, 2], (3 : UInt8))

-- ── `AsEmpty` ────────────────────────────────────
-- (run directly via `withPrism`, matching `Tests.Linen.Control.Lens.
-- EmptyTest`'s own precedent for exercising a `Prism` without a bare-arrow
-- `Getting` bridge.)

#guard withPrism _Empty (fun _ seta =>
  match seta Data.ByteString.empty with | .inr () => true | .inl _ => false)
#guard withPrism _Empty (fun _ seta =>
  match seta (Data.ByteString.pack ([1] : List UInt8)) with | .inr () => false | .inl _ => true)

-- ── `Ixed` ───────────────────────────────────────

#guard preview (ix 1) (Data.ByteString.pack ([10, 20, 30] : List UInt8)) == some (20 : UInt8)
#guard preview (ix 5) (Data.ByteString.pack ([10, 20, 30] : List UInt8)) = none
#guard over (ix 1) (· + 1) (Data.ByteString.pack ([10, 20, 30] : List UInt8))
  == Data.ByteString.pack [10, 21, 30]

end Tests.Linen.Data.ByteString.Lens
