/-
  Linen.Data.ByteString.Builder — efficient byte-string construction

  A `Builder` is a continuation `LazyByteString → LazyByteString`, composed by
  function composition for **O(1) `append`**; execution materialises a
  `LazyByteString` (then optionally a strict `ByteString`). The monoid laws hold
  definitionally (associativity/identity of `∘`). Mirrors
  `Data.ByteString.Builder`.

  The hex encoder uses core `Nat.toDigits 16` (no hand-rolled fuel loop).
-/

import Linen.Data.ByteString.Lazy
import Linen.Data.ByteString.Short

namespace Data.ByteString

/-- A byte-string builder: a continuation `LazyByteString → LazyByteString`.
    Composition gives O(1) concatenation.
    $$\text{Builder} = \text{LazyByteString} \to \text{LazyByteString}$$ -/
structure Builder where
  /-- The continuation function. -/
  run : Lazy.LazyByteString → Lazy.LazyByteString

namespace Builder

/-! ── Core ── -/

/-- The empty builder (identity continuation). -/
@[inline] def empty : Builder := ⟨id⟩

instance : Inhabited Builder := ⟨empty⟩

/-- Append two builders via function composition. O(1). -/
@[inline] def append (a b : Builder) : Builder := ⟨a.run ∘ b.run⟩

instance : Append Builder := ⟨Builder.append⟩

/-! ── Execution ── -/

/-- Execute a builder, producing a lazy byte string. -/
@[inline] def toLazyByteString (b : Builder) : Lazy.LazyByteString :=
  b.run .nil

/-- Execute a builder, producing a strict byte string. -/
@[inline] def toStrictByteString (b : Builder) : ByteString :=
  b.toLazyByteString.toStrict

/-! ── Primitives ── -/

/-- Build a single byte. -/
def singleton (w : UInt8) : Builder :=
  ⟨fun rest => Lazy.LazyByteString.cons w rest⟩

/-- Build from a strict `ByteString`. -/
def byteString (bs : ByteString) : Builder :=
  ⟨fun rest =>
    if bs.null then rest
    else Lazy.LazyByteString.chunk' bs (Thunk.mk fun () => rest)⟩

/-- Build from a lazy `ByteString`. -/
def lazyByteString (lbs : Lazy.LazyByteString) : Builder :=
  ⟨fun rest => lbs ++ rest⟩

/-- Build from a `ShortByteString`. -/
def shortByteString (sbs : ShortByteString) : Builder :=
  byteString (ShortByteString.fromShort sbs)

/-! ── Numeric encodings ── -/

/-- Encode a `UInt8` as a single byte. -/
@[inline] def word8 (w : UInt8) : Builder := singleton w

/-- Encode a 16-bit value, big-endian. -/
def word16BE (v : UInt16) : Builder :=
  singleton (v >>> 8).toUInt8 ++ singleton v.toUInt8

/-- Encode a 16-bit value, little-endian. -/
def word16LE (v : UInt16) : Builder :=
  singleton v.toUInt8 ++ singleton (v >>> 8).toUInt8

/-- Encode a 32-bit value, big-endian. -/
def word32BE (v : UInt32) : Builder :=
  singleton (v >>> 24).toUInt8 ++ singleton (v >>> 16).toUInt8 ++
  singleton (v >>> 8).toUInt8 ++ singleton v.toUInt8

/-- Encode a 32-bit value, little-endian. -/
def word32LE (v : UInt32) : Builder :=
  singleton v.toUInt8 ++ singleton (v >>> 8).toUInt8 ++
  singleton (v >>> 16).toUInt8 ++ singleton (v >>> 24).toUInt8

/-- Encode a 64-bit value, big-endian. -/
def word64BE (v : UInt64) : Builder :=
  singleton (v >>> 56).toUInt8 ++ singleton (v >>> 48).toUInt8 ++
  singleton (v >>> 40).toUInt8 ++ singleton (v >>> 32).toUInt8 ++
  singleton (v >>> 24).toUInt8 ++ singleton (v >>> 16).toUInt8 ++
  singleton (v >>> 8).toUInt8 ++ singleton v.toUInt8

/-- Encode a 64-bit value, little-endian. -/
def word64LE (v : UInt64) : Builder :=
  singleton v.toUInt8 ++ singleton (v >>> 8).toUInt8 ++
  singleton (v >>> 16).toUInt8 ++ singleton (v >>> 24).toUInt8 ++
  singleton (v >>> 32).toUInt8 ++ singleton (v >>> 40).toUInt8 ++
  singleton (v >>> 48).toUInt8 ++ singleton (v >>> 56).toUInt8

/-! ── Text encodings ── -/

/-- Encode a character as a single byte (Latin-1 truncation). -/
def char8 (c : Char) : Builder :=
  singleton c.toNat.toUInt8

/-- Encode a character as UTF-8. -/
def charUtf8 (c : Char) : Builder :=
  let bytes := (String.singleton c).toUTF8
  byteString ⟨bytes, 0, bytes.size, by omega⟩

/-- Encode a string as UTF-8. -/
def stringUtf8 (s : String) : Builder :=
  let bytes := s.toUTF8
  byteString ⟨bytes, 0, bytes.size, by omega⟩

/-! ── Decimal / hex formatting ── -/

/-- Encode an integer as decimal ASCII (e.g. `42 ↦ "42"`). -/
def intDec (n : Int) : Builder :=
  stringUtf8 (toString n)

/-- Encode a natural number as lowercase hexadecimal ASCII (e.g. `255 ↦ "ff"`),
    via core `Nat.toDigits 16`. -/
def wordHex (n : Nat) : Builder :=
  byteString (ByteString.pack ((Nat.toDigits 16 n).map (fun c => c.toNat.toUInt8)))

/-! ── Instances ── -/

instance : ToString Builder where
  toString b := toString b.toStrictByteString

/-! ── Monoid laws (definitional via `∘`) ── -/

/-- Left identity: `empty ++ b = b`. -/
theorem empty_append (b : Builder) : empty ++ b = b := by
  cases b with | mk f => exact congrArg Builder.mk (funext fun _ => rfl)

/-- Right identity: `b ++ empty = b`. -/
theorem append_empty (b : Builder) : b ++ empty = b := by
  cases b with | mk f => exact congrArg Builder.mk (funext fun _ => rfl)

/-- Associativity: `(a ++ b) ++ c = a ++ (b ++ c)`. -/
theorem append_assoc (a b c : Builder) : (a ++ b) ++ c = a ++ (b ++ c) := by
  cases a with | mk f => cases b with | mk g => cases c with | mk h =>
  exact congrArg Builder.mk (funext fun _ => rfl)

end Builder
end Data.ByteString
