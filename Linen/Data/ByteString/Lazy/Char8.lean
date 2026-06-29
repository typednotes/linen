/-
  Linen.Data.ByteString.Lazy.Char8 — character-oriented lazy byte strings

  A Latin-1 `Char` view of `LazyByteString` (`c2w c = c.toNat % 256`,
  `w2c w = Char.ofNat w.toNat`), the lazy analogue of `Data.ByteString.Char8`.
  No new types — thin wrappers over `LazyByteString`. Mirrors Haskell's
  `Data.ByteString.Lazy.Char8`.
-/

import Linen.Data.ByteString.Lazy
import Linen.Data.ByteString.Char8

namespace Data.ByteString.Lazy.Char8

open Data.ByteString.Lazy

/-- Convert a `Char` to a byte (Latin-1 truncation). -/
@[inline] private def c2w (c : Char) : UInt8 := c.toNat.toUInt8

/-- Convert a byte to a `Char` (Latin-1). -/
@[inline] private def w2c (w : UInt8) : Char := Char.ofNat w.toNat

/-- Pack a `String` into a `LazyByteString` (Latin-1 truncation). -/
def pack (s : String) : LazyByteString :=
  LazyByteString.fromStrict (Data.ByteString.Char8.pack s)

/-- Unpack a `LazyByteString` into a `String` (Latin-1 interpretation). -/
def unpack (lbs : LazyByteString) : String :=
  Data.ByteString.Char8.unpack lbs.toStrict

/-- Cons a character to the front. -/
def cons (c : Char) (lbs : LazyByteString) : LazyByteString :=
  LazyByteString.cons (c2w c) lbs

/-- The first character, or `none` if empty. -/
def head? (lbs : LazyByteString) : Option Char :=
  lbs.head?.map w2c

/-- Map a character function over all bytes. -/
def map (f : Char → Char) (lbs : LazyByteString) : LazyByteString :=
  LazyByteString.map (fun w => c2w (f (w2c w))) lbs

/-- Filter by a character predicate. -/
def filter (p : Char → Bool) (lbs : LazyByteString) : LazyByteString :=
  LazyByteString.filter (fun w => p (w2c w)) lbs

/-- Left fold with characters. -/
def foldl (f : β → Char → β) (init : β) (lbs : LazyByteString) : β :=
  LazyByteString.foldl (fun acc w => f acc (w2c w)) init lbs

/-- Right fold with characters. -/
def foldr (f : Char → β → β) (init : β) (lbs : LazyByteString) : β :=
  LazyByteString.foldr (fun w acc => f (w2c w) acc) init lbs

/-- Does a character occur in the lazy byte string? -/
def elem (c : Char) (lbs : LazyByteString) : Bool :=
  LazyByteString.elem (c2w c) lbs

end Data.ByteString.Lazy.Char8
