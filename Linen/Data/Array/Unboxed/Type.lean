/-
  Linen.Data.Array.Unboxed.Type — the immutable unboxed array

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Array.Type`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Array/Type.hs),
  module #29 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  An `Array a` is streamly's **immutable, unboxed** array: a frozen `MutArray`
  (#27). Upstream's record

  ```haskell
  data Array a = Array { arrContents :: !MutByteArray , arrStart :: !Int , arrEnd :: !Int }
  ```

  drops `MutArray`'s `arrBound` (an immutable array never grows, so capacity =
  length). Elements are `Unbox`-serialized, read back by `getIndexUnsafe`.

  ## Namespace

  Placed under `Linen.Data.Array.Unboxed.*` (not `Linen.Data.Array.*`) so it
  sits beside the existing `repa`-derived `Linen.Data.Array.Shaped.*` without
  collision — per the plan's namespace decision.

  ## Substitutions / deviations

  - **Frozen `MutByteArray` slice, byte offsets** (`start`, `used`), mirroring
    `MutArray` minus `bound`. Freeze/thaw share the buffer (immutability is by
    API, matching upstream's `unsafeFreeze`/`unsafeThaw`).
  - **Pure**, as in the `MutArray`/`MutByteArray` ports: `fromList`/`toList`/
    `getIndex`/`splice`/slicing are pure; the `Fold`/`Unfold`/`Stream` builders
    keep their monadic carrier.
  - **Deferred as peripheral**: `asPtrUnsafe`/`unsafePinnedAsPtr` (`Ptr` FFI, no
    Lean analogue) and the byte-specialized `Eq`/`Ord` macro instances (the
    generic `Unbox`+`BEq` element-wise instance is kept instead).
-/

import Linen.Data.MutArray.Type
import Linen.Data.Unbox
import Linen.Data.Fold.Type
import Linen.Data.Unfold.Type
import Linen.Data.Stream.Type

namespace Data
namespace Array

open Data (MutByteArray Unbox MutArray)
open Data.Fold (Fold)
open Data.Unfold (Unfold)
open Data.Stream (Stream)

-- ── The immutable unboxed array ─────────────────────────────────────────────

/-- An immutable unboxed array: the frozen byte slice `[start, used)` of a
    backing `MutByteArray`. -/
structure Unboxed (a : Type) where
  /-- Backing byte buffer. -/
  contents : MutByteArray
  /-- Byte offset of the first element (`arrStart`). -/
  start : Nat := 0
  /-- Byte offset just past the last element (`arrEnd`). -/
  used : Nat := 0
  deriving Inhabited

namespace Unboxed

/-- Byte size of a single element. -/
@[inline] def elemBytes (a : Type) [Unbox a] : Nat := Unbox.size (a := a)

-- ── Freeze / thaw ─────────────────────────────────────────────────────────────

/-- View a `MutArray` as an immutable array, sharing the buffer
    (`unsafeFreeze`). -/
@[inline] def unsafeFreeze (arr : MutArray a) : Unboxed a :=
  { contents := arr.contents, start := arr.start, used := arr.used }

/-- View an immutable array as a `MutArray` with no spare capacity
    (`unsafeThaw`). -/
@[inline] def unsafeThaw (arr : Unboxed a) : MutArray a :=
  { contents := arr.contents, start := arr.start, used := arr.used, bound := arr.used }

-- ── Size ──────────────────────────────────────────────────────────────────────

/-- Length in bytes (`arrEnd - arrStart`). -/
@[inline] def byteLength (arr : Unboxed a) : Nat := arr.used - arr.start

/-- Number of elements. -/
@[inline] def length [Unbox a] (arr : Unboxed a) : Nat := byteLength arr / elemBytes a

-- ── Indexing ──────────────────────────────────────────────────────────────────

/-- Read the element at index `i` without a bounds check (`getIndexUnsafe`). -/
@[inline] def unsafeGetIndex [Unbox a] (i : Nat) (arr : Unboxed a) : a :=
  Unbox.peekAt (arr.start + i * elemBytes a) arr.contents

/-- Read the element at index `i`, or `none` if out of range. -/
@[inline] def getIndex [Unbox a] (i : Nat) (arr : Unboxed a) : Option a :=
  if i < length arr then some (unsafeGetIndex i arr) else none

-- ── Slicing ─────────────────────────────────────────────────────────────────

/-- The sub-array of `len` elements from element `off`, no bounds check
    (`unsafeSliceOffLen`). Shares the buffer. -/
@[inline] def unsafeSliceOffLen [Unbox a] (off len : Nat) (arr : Unboxed a) : Unboxed a :=
  let s := arr.start + off * elemBytes a
  { arr with start := s, used := s + len * elemBytes a }

-- ── Lists ───────────────────────────────────────────────────────────────────

/-- All elements, in order. -/
@[inline] def toList [Unbox a] (arr : Unboxed a) : List a :=
  (List.range (length arr)).map (fun i => unsafeGetIndex i arr)

/-- Build from at most `count` elements of a list (`fromListN`). -/
@[inline] def fromListN [Unbox a] (count : Nat) (xs : List a) : Unboxed a :=
  unsafeFreeze (MutArray.fromListN count xs)

/-- Build from a whole list (`fromList`). -/
@[inline] def fromList [Unbox a] (xs : List a) : Unboxed a :=
  unsafeFreeze (MutArray.fromList xs)

-- ── Concatenation ─────────────────────────────────────────────────────────────

/-- Concatenate two arrays into a fresh one (`splice`). -/
@[inline] def splice [Unbox a] (x y : Unboxed a) : Unboxed a :=
  fromList (toList x ++ toList y)

-- ── Streaming ─────────────────────────────────────────────────────────────────

/-- Unfold an array into a stream of its elements (`reader`). -/
@[inline] def reader [Monad m] [Unbox a] : Unfold m (Unboxed a) a where
  step := fun (arr, i) =>
    if i < length arr then pure (.Yield (unsafeGetIndex i arr) (arr, i + 1))
    else pure .Stop
  inject := fun arr => pure (arr, 0)

/-- Stream the elements of an array in order (`read`). -/
@[inline] def read [Monad m] [Unbox a] (arr : Unboxed a) : Stream m a :=
  Stream.unfold reader arr

-- ── Folds ─────────────────────────────────────────────────────────────────────

/-- A fold collecting up to `n` elements into a frozen array (`createOf`). -/
@[inline] def createOf [Monad m] [Unbox a] (n : Nat) : Fold m a (Unboxed a) :=
  unsafeFreeze <$> MutArray.createOf n

/-- A fold collecting all input into a frozen array (`create`). -/
@[inline] def create [Monad m] [Unbox a] : Fold m a (Unboxed a) :=
  unsafeFreeze <$> MutArray.create

-- ── Instances ─────────────────────────────────────────────────────────────────

/-- Element-wise equality (generic `Unbox`+`BEq` instance). -/
instance [Unbox a] [BEq a] : BEq (Unboxed a) where
  beq x y := x.toList == y.toList

/-- `fromList`-style display, as upstream's `Show`. -/
instance [Unbox a] [ToString a] : ToString (Unboxed a) where
  toString arr := s!"fromList {arr.toList}"

/-- `Semigroup`/`Monoid` via `splice`/`empty`. -/
instance [Unbox a] : Append (Unboxed a) where
  append := splice

/-- The empty array. -/
@[inline] def empty : Unboxed a := unsafeFreeze MutArray.empty

end Unboxed
end Array
end Data
