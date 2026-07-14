/-
  Linen.Data.ByteString.Lazy.Lens — `packedBytes`, `unpackedBytes`,
  `packedChars`, `unpackedChars`, and `Cons`/`Snoc`/`AsEmpty`/`Ixed`
  instances for `Linen.Data.ByteString.Lazy`

  Port of Hackage's `lens-5.3.6`'s `Data.ByteString.Lazy.Lens` (fetched and
  read via Hackage's rendered source), the lazy-`ByteString` counterpart of
  `Linen.Data.ByteString.Lens` — same four isomorphisms and the same
  `Cons`/`Snoc`/`AsEmpty`/`Ixed` instance set, over `Linen.Data.ByteString.
  Lazy`'s `LazyByteString` instead. See that module's doc comment for the
  shared design notes (`Ixed` round-tripping through `List`, `Int` → `Nat`
  narrowing). -/

import Linen.Control.Lens.At
import Linen.Control.Lens.Cons
import Linen.Control.Lens.Empty
import Linen.Data.ByteString.Lazy
import Linen.Data.ByteString.Lazy.Char8

namespace Control.Lens

open Data.ByteString.Lazy (LazyByteString)

-- ── packedBytes / unpackedBytes ─────────────────

/-- `packedBytes :: Iso' [Word8] ByteString` — `iso pack unpack`. -/
@[inline] def lazyPackedBytes : Iso' (List UInt8) LazyByteString :=
  iso LazyByteString.pack LazyByteString.unpack

/-- `unpackedBytes :: Iso' ByteString [Word8]` — `from packedBytes`. -/
@[inline] def lazyUnpackedBytes : Iso' LazyByteString (List UInt8) :=
  «from» lazyPackedBytes

-- ── packedChars / unpackedChars ──────────────────

/-- `packedChars :: Iso' String ByteString` — `iso pack unpack`, over
    `Linen.Data.ByteString.Lazy.Char8`'s Latin1-style conversion. -/
@[inline] def lazyPackedChars : Iso' String LazyByteString :=
  iso Data.ByteString.Lazy.Char8.pack Data.ByteString.Lazy.Char8.unpack

/-- `unpackedChars :: Iso' ByteString String` — `from packedChars`. -/
@[inline] def lazyUnpackedChars : Iso' LazyByteString String :=
  «from» lazyPackedChars

-- ── Cons / Snoc / AsEmpty ────────────────────────

/-- `instance Cons ByteString ByteString Word8 Word8`. -/
instance instConsLazyByteString : Cons LazyByteString UInt8 UInt8 LazyByteString where
  _Cons := prism (fun p => LazyByteString.cons p.1 p.2) (fun s =>
    match LazyByteString.uncons s with
    | some (w, rest) => .inr (w, rest)
    | none => .inl LazyByteString.empty)

/-- `instance Snoc ByteString ByteString Word8 Word8`. -/
instance instSnocLazyByteString : Snoc LazyByteString UInt8 UInt8 LazyByteString where
  _Snoc := prism (fun p => LazyByteString.snoc p.1 p.2) (fun s =>
    -- `LazyByteString` has no dedicated `unsnoc`; fall back to a
    -- `List`-round-trip via `unpack`/`pack`, matching `Ixed`'s own
    -- strategy below.
    match (LazyByteString.unpack s).getLast? with
    | none => .inl LazyByteString.empty
    | some w => .inr (LazyByteString.pack (LazyByteString.unpack s).dropLast, w))

/-- `instance AsEmpty ByteString`. -/
instance instAsEmptyLazyByteString : AsEmpty LazyByteString where
  _Empty := nearly LazyByteString.empty LazyByteString.null

-- ── Ixed ─────────────────────────────────────────

/-- `instance Ixed ByteString` (`Index ByteString = Int`, `IxValue
    ByteString = Word8`, narrowed to `Nat`) — round-trips through
    `unpack`/`pack`, reusing `Linen.Control.Lens.At`'s `ixListGo`. -/
instance instIxedLazyByteString : Ixed LazyByteString Nat UInt8 where
  ix i := fun {F} [Applicative F] (f : UInt8 → F UInt8) (lbs : LazyByteString) =>
    (fun l => LazyByteString.pack l) <$> ixListGo f lbs.unpack i

end Control.Lens
