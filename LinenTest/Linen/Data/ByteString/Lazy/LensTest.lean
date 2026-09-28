/-
  Tests for `Linen.Data.ByteString.Lazy.Lens`.
-/
import Linen.Control.Lens.Empty
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Iso
import Linen.Control.Lens.Prism
import Linen.Control.Lens.Setter
import Linen.Data.ByteString.Lazy.Lens

open Control.Lens
open Data.ByteString.Lazy (LazyByteString)

namespace Tests.Linen.Data.ByteString.Lazy.Lens

-- ── `lazyPackedBytes` / `lazyUnpackedBytes` ──────
-- (run directly via `withIso`, since `Iso` is genuinely
-- profunctor-polymorphic and does not unify with `view`'s bare `Getting`
-- shape without an explicit instantiation — see `Linen.Control.Lens.Iso.
-- withIso`'s own doc comment.)

#guard withIso lazyPackedBytes (fun sa _ => sa [1, 2, 3]) == LazyByteString.pack [1, 2, 3]
#guard withIso lazyUnpackedBytes (fun sa _ => sa (LazyByteString.pack [1, 2, 3])) = ([1, 2, 3] : List UInt8)

-- ── `lazyPackedChars` / `lazyUnpackedChars` ──────

#guard withIso lazyUnpackedChars (fun sa _ =>
  withIso lazyPackedChars (fun sa' _ => sa (sa' "abc"))) = "abc"

-- ── `Cons` / `Snoc` ──────────────────────────────

#guard cons (1 : UInt8) (LazyByteString.pack [2, 3]) == LazyByteString.pack [1, 2, 3]
#guard uncons (LazyByteString.pack ([1, 2, 3] : List UInt8)) == some ((1 : UInt8), LazyByteString.pack [2, 3])
#guard snoc (LazyByteString.pack ([1, 2] : List UInt8)) (3 : UInt8) == LazyByteString.pack [1, 2, 3]
#guard unsnoc (LazyByteString.pack ([1, 2, 3] : List UInt8)) == some (LazyByteString.pack [1, 2], (3 : UInt8))

-- ── `AsEmpty` ────────────────────────────────────
-- (run directly via `withPrism`, matching `Tests.Linen.Control.Lens.
-- EmptyTest`'s own precedent for exercising a `Prism` without a bare-arrow
-- `Getting` bridge.)

#guard withPrism _Empty (fun _ seta =>
  match seta LazyByteString.empty with | .inr () => true | .inl _ => false)
#guard withPrism _Empty (fun _ seta =>
  match seta (LazyByteString.pack ([1] : List UInt8)) with | .inr () => false | .inl _ => true)

-- ── `Ixed` ───────────────────────────────────────

#guard preview (ix 1) (LazyByteString.pack ([10, 20, 30] : List UInt8)) == some (20 : UInt8)
#guard preview (ix 5) (LazyByteString.pack ([10, 20, 30] : List UInt8)) = none
#guard over (ix 1) (· + 1) (LazyByteString.pack ([10, 20, 30] : List UInt8))
  == LazyByteString.pack [10, 21, 30]

end Tests.Linen.Data.ByteString.Lazy.Lens
