/-
  Linen.Data.ByteString.Lens — `packedBytes`, `unpackedBytes`,
  `packedChars`, `unpackedChars`, and `Cons`/`Snoc`/`AsEmpty`/`Ixed`
  instances for `Linen.Data.ByteString`

  Port of Hackage's `lens-5.3.6`'s `Data.ByteString.Lens` (`Control.Lens.
  Internal.ByteString` supplies the actual `Cons`/`Snoc`/`AsEmpty`/`Ixed`
  instances upstream, re-exported from `Data.ByteString.Lens`/`Data.
  ByteString.Strict.Lens`; fetched and read via Hackage's rendered
  source). Upstream's real content:

  ```
  packedBytes   :: Iso' [Word8] ByteString
  unpackedBytes :: Iso' ByteString [Word8]
  packedChars   :: Iso' String ByteString
  unpackedChars :: Iso' ByteString String

  instance Cons ByteString ByteString Word8 Word8
  instance Snoc ByteString ByteString Word8 Word8
  instance AsEmpty ByteString
  instance Ixed ByteString  -- Index = Int, IxValue = Word8
  ```

  translated against `Linen.Data.ByteString`'s `ByteString` (a
  `ByteArray`-backed slice type) and `Linen.Data.ByteString.Char8`'s
  `pack`/`unpack` for the `String ↔ ByteString` direction.

  **Deviation (`Ixed`, round-tripping through `List`).** `linen`'s
  `ByteString` has no direct in-place "read/write byte `i`" primitive of its
  own; rather than adding one, `ix` here round-trips through `unpack`/`pack`
  and reuses `Linen.Control.Lens.At`'s already-proven-total `ixListGo`
  helper — the same "no bespoke recursion, delegate to an existing
  structural traversal" strategy `Linen.Control.Lens.Each`'s `instEachArray`
  already uses for `Array`. -/

import Linen.Control.Lens.At
import Linen.Control.Lens.Cons
import Linen.Control.Lens.Empty
import Linen.Data.ByteString
import Linen.Data.ByteString.Char8

namespace Control.Lens

open Data (ByteString)

-- ── packedBytes / unpackedBytes ─────────────────

/-- `packedBytes :: Iso' [Word8] ByteString` — `iso pack unpack`. -/
@[inline] def packedBytes : Iso' (List UInt8) ByteString :=
  iso ByteString.pack ByteString.unpack

/-- `unpackedBytes :: Iso' ByteString [Word8]` — `from packedBytes`. -/
@[inline] def unpackedBytes : Iso' ByteString (List UInt8) :=
  «from» packedBytes

-- ── packedChars / unpackedChars ──────────────────

/-- `packedChars :: Iso' String ByteString` — `iso pack unpack`, over
    `Linen.Data.ByteString.Char8`'s Latin1-style `String ↔ ByteString`
    conversion. -/
@[inline] def packedChars : Iso' String ByteString :=
  iso Data.ByteString.Char8.pack Data.ByteString.Char8.unpack

/-- `unpackedChars :: Iso' ByteString String` — `from packedChars`. -/
@[inline] def unpackedChars : Iso' ByteString String :=
  «from» packedChars

-- ── Cons / Snoc / AsEmpty ────────────────────────

/-- `instance Cons ByteString ByteString Word8 Word8`. -/
instance instConsByteString : Cons ByteString UInt8 UInt8 ByteString where
  _Cons := prism (fun p => ByteString.cons p.1 p.2) (fun s =>
    match ByteString.uncons s with
    | some (w, rest) => .inr (w, rest)
    | none => .inl ByteString.empty)

/-- `instance Snoc ByteString ByteString Word8 Word8`. -/
instance instSnocByteString : Snoc ByteString UInt8 UInt8 ByteString where
  _Snoc := prism (fun p => ByteString.snoc p.1 p.2) (fun s =>
    match ByteString.unsnoc s with
    | some (rest, w) => .inr (rest, w)
    | none => .inl ByteString.empty)

/-- `instance AsEmpty ByteString`. -/
instance instAsEmptyByteString : AsEmpty ByteString where
  _Empty := nearly ByteString.empty (fun bs => bs.len == 0)

-- ── Ixed ─────────────────────────────────────────

/-- `instance Ixed ByteString` (`Index ByteString = Int`, `IxValue ByteString
    = Word8`, narrowed to `Nat` matching this batch's other container `Ixed`
    instances) — round-trips through `unpack`/`pack`, reusing `Linen.
    Control.Lens.At`'s `ixListGo` (see the module doc comment). -/
instance instIxedByteString : Ixed ByteString Nat UInt8 where
  ix i := fun {F} [Applicative F] (f : UInt8 → F UInt8) (bs : ByteString) =>
    (fun l => ByteString.pack l) <$> ixListGo f bs.unpack i

end Control.Lens
