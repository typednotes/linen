/-
  Linen.Data.Array.Unboxed — immutable unboxed-array combinators

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Array`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Array.hs),
  module #30 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  Combinators over the immutable unboxed `Array.Unboxed` of `.Type` (#29):
  building an array from a stream, reverse reads, folds/scans over an array,
  and slicing an array into fixed-length pieces.

  ## Substitutions / deviations

  - **Pure**, matching `.Type`: transforms return a fresh array; the
    stream-consuming builders keep the monadic carrier of `Fold`/`Stream`.
  - **Deferred as peripheral** (consistent with the plan's scoping): the
    `Unicode`/`Serialize` byte-array facades, the `Ptr`/FFI access, and the
    `compact*` stream-of-arrays coalescers (deferred with `MutArray`'s, since
    they need the deferred byte-splitter/`RingArray` machinery). The everyday
    build/read/slice/fold surface is kept.
-/

import Linen.Data.Array.Unboxed.Type
import Linen.Data.Unfold.Type
import Linen.Data.Stream.Type

namespace Data
namespace Array
namespace Unboxed

open Data (Unbox)
open Data.Unfold (Unfold)
open Data.Stream (Stream)

-- ── Building from a stream ────────────────────────────────────────────────────

/-- Collect at most `n` elements of a stream into a fresh array
    (`fromStreamN`). -/
@[inline] unsafe def fromStreamN [Monad m] [Unbox a] (n : Nat) (s : Stream m a) :
    m (Unboxed a) :=
  Stream.fold (createOf n) s

/-- Collect a whole stream into a fresh array (`fromStream`). -/
@[inline] unsafe def fromStream [Monad m] [Unbox a] (s : Stream m a) : m (Unboxed a) :=
  Stream.fold create s

-- ── Reverse read ──────────────────────────────────────────────────────────────

/-- Unfold an array into a stream of its elements in reverse (`readerRev`). -/
@[inline] def readerRev [Monad m] [Unbox a] : Unfold m (Unboxed a) a where
  step := fun (arr, i) =>
    match i with
    | 0 => pure .Stop
    | i + 1 => pure (.Yield (unsafeGetIndex i arr) (arr, i))
  inject := fun arr => pure (arr, length arr)

/-- Stream the elements of an array in reverse (`readRev`). -/
@[inline] def readRev [Monad m] [Unbox a] (arr : Unboxed a) : Stream m a :=
  Stream.unfold readerRev arr

-- ── Folding over an array ─────────────────────────────────────────────────────

/-- Strict left fold over the elements of an array. -/
@[inline] def foldl' [Unbox a] (f : b → a → b) (z : b) (arr : Unboxed a) : b :=
  arr.toList.foldl f z

/-- Is the array empty? -/
@[inline] def null [Unbox a] (arr : Unboxed a) : Bool := length arr == 0

/-- The first element, or `none` if empty (`head`). -/
@[inline] def head [Unbox a] (arr : Unboxed a) : Option a := getIndex 0 arr

/-- The last element, or `none` if empty (`last`). -/
@[inline] def last [Unbox a] (arr : Unboxed a) : Option a :=
  match length arr with
  | 0 => none
  | n + 1 => some (unsafeGetIndex n arr)

-- ── Slicing ───────────────────────────────────────────────────────────────────

/-- Unfold an array into its successive fixed-length sub-arrays of `len`
    elements each, starting at element `from_` (`slicerFromLen`). -/
@[inline] def slicerFromLen [Monad m] [Unbox a] (from_ len : Nat) :
    Unfold m (Unboxed a) (Unboxed a) where
  step := fun (arr, i) =>
    if i + len ≤ length arr then pure (.Yield (unsafeSliceOffLen i len arr) (arr, i + len))
    else pure .Stop
  inject := fun arr => pure (arr, from_)

/-- The sub-array of `len` elements from element `off`, clamped to range
    (`getSlice`). -/
@[inline] def getSlice [Unbox a] (off len : Nat) (arr : Unboxed a) : Unboxed a :=
  let off := min off (length arr)
  let len := min len (length arr - off)
  unsafeSliceOffLen off len arr

end Unboxed
end Array
end Data
