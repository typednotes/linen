/-
  Linen.Data.ByteString.Short — short byte strings

  A thin newtype over core `ByteArray`. In Haskell `ShortByteString` uses
  unpinned (GC-friendly) memory; Lean has no pinned/unpinned distinction, so
  this exists for API compatibility and type-safe `toShort`/`fromShort`
  conversions with the slice-based strict `ByteString`. Mirrors
  `Data.ByteString.Short`.
-/

import Linen.Data.ByteString

namespace Data.ByteString

/-- A short byte string backed by a plain `ByteArray`.
    $$\text{ShortByteString} \cong \text{ByteArray}$$ -/
structure ShortByteString where
  /-- The underlying byte array. -/
  data : ByteArray
deriving BEq

instance : Repr ShortByteString where
  reprPrec sbs n := reprPrec sbs.data.toList n

instance : Ord ShortByteString where
  compare a b := compare a.data.toList b.data.toList

instance : Hashable ShortByteString where
  hash sbs := hash sbs.data.toList

namespace ShortByteString

/-- The empty short byte string. -/
@[inline] def empty : ShortByteString := ⟨ByteArray.empty⟩

instance : Inhabited ShortByteString := ⟨empty⟩

/-- Is this short byte string empty? -/
@[inline] def null (sbs : ShortByteString) : Bool := sbs.data.size == 0

/-- The number of bytes. -/
@[inline] def length (sbs : ShortByteString) : Nat := sbs.data.size

/-- Index with a bounds proof. -/
@[inline] def index (sbs : ShortByteString) (i : Nat) (h : i < sbs.data.size) : UInt8 :=
  sbs.data[i]'h

/-- Pack a list of bytes. -/
def pack (ws : List UInt8) : ShortByteString :=
  ⟨ws.foldl (fun a w => a.push w) ByteArray.empty⟩

/-- Unpack into a list of bytes. -/
def unpack (sbs : ShortByteString) : List UInt8 :=
  go 0 sbs.data.size []
where
  go (i : Nat) (remaining : Nat) (acc : List UInt8) : List UInt8 :=
    match remaining with
    | 0 => acc.reverse
    | n + 1 => go (i + 1) n (sbs.data.get! i :: acc)

/-- Convert a strict `ByteString` to a `ShortByteString` (copies the slice). -/
def toShort (bs : ByteString) : ShortByteString :=
  ⟨bs.data.extract bs.off (bs.off + bs.len)⟩

/-- Convert a `ShortByteString` to a strict `ByteString`. O(1). -/
def fromShort (sbs : ShortByteString) : ByteString :=
  ⟨sbs.data, 0, sbs.data.size, by omega⟩

instance : ToString ShortByteString where
  toString sbs := toString (fromShort sbs)

/-- `toShort` preserves length: $|\text{toShort}(bs)| = |bs|$. -/
theorem length_toShort (bs : ByteString) :
    (toShort bs).length = bs.len := by
  unfold toShort length
  simp only [ByteArray.size_extract]
  have hv := bs.valid
  omega

end ShortByteString
end Data.ByteString
