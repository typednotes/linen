/-
  Linen.Data.MutArray — mutable-array combinators

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.MutArray`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/MutArray.hs),
  module #28 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  Combinators over the growable unboxed `MutArray` of `.Type` (#27): building
  an array from a stream, in-place transforms (`modify`/`reverse`), element
  swaps, comparison, and the slice indexers used to cut a big array into
  fixed-length pieces.

  ## Substitutions / deviations

  - **Pure, as in `.Type`.** `fromStream`/`writeN`-style array builders are
    given via the `Fold`s exported from `.Type` (`create`/`createOf`); the
    stream-consuming forms here run those folds. In-place transforms are pure
    `MutArray → MutArray` (`MutByteArray` is a managed value).
  - **`indexerFromLen`/`splitterFromLen`** are ported (the fixed-length slice
    indexers), since slicing a big array into chunks is the module's headline
    use. The `compact*` family (coalescing a *stream of arrays* by byte
    separators / max size) and the `Serialize`/`Ptr` paths are **deferred as
    peripheral** — they belong to the deferred `Unicode`/`Serialize`/FFI
    subtrees the plan already scopes out, and the compaction combinators need
    the deferred `RingArray`/byte-splitter machinery. The everyday
    build/transform/compare surface is kept.
-/

import Linen.Data.MutArray.Type
import Linen.Data.Unfold.Type
import Linen.Data.Stream.Type
import Linen.Data.Stream.Eliminate

namespace Data
namespace MutArray

open Data (Unbox)
open Data.Unfold (Unfold)
open Data.Stream (Stream)

-- ── Building from a stream ────────────────────────────────────────────────────

/-- Collect at most `n` elements of a stream into a fresh array
    (`fromStreamN`/`writeN`). -/
@[inline] unsafe def fromStreamN [Monad m] [Unbox a] (n : Nat) (s : Stream m a) :
    m (MutArray a) :=
  Stream.fold (createOf n) s

/-- Collect a whole stream into a growable array (`fromStream`/`write`). -/
@[inline] unsafe def fromStream [Monad m] [Unbox a] (s : Stream m a) : m (MutArray a) :=
  Stream.fold create s

-- ── In-place transforms ───────────────────────────────────────────────────────

/-- Swap the elements at indices `i` and `j` (a no-op if either is out of
    range). -/
@[inline] def swapIndices [Unbox a] (i j : Nat) (arr : MutArray a) : MutArray a :=
  match getIndex i arr, getIndex j arr with
  | some x, some y => unsafePutIndex j (unsafePutIndex i arr y) x
  | _, _ => arr

/-- Reverse the array in place. -/
@[inline] def reverse [Unbox a] (arr : MutArray a) : MutArray a :=
  let n := length arr
  (List.range (n / 2)).foldl (fun acc i => swapIndices i (n - 1 - i) acc) arr

/-- Map `f` over every element in place (`modify`). -/
@[inline] def modify [Unbox a] (f : a → a) (arr : MutArray a) : MutArray a :=
  (List.range (length arr)).foldl (fun acc i => modifyIndex i f acc) arr

-- ── Comparison ────────────────────────────────────────────────────────────────

/-- Structural element-wise comparison of two arrays (`==` on the live
    elements; `cmp`/`eq` upstream). -/
@[inline] def eq [Unbox a] [BEq a] (x y : MutArray a) : Bool := x.toList == y.toList

-- ── Slice indexers ────────────────────────────────────────────────────────────

/-- Unfold an array into the successive `(offset, length)` byte pairs of its
    fixed-length element slices of `len` elements each (`indexerFromLen`). -/
@[inline] def indexerFromLen [Monad m] [Unbox a] (from_ len : Nat) :
    Unfold m (MutArray a) (Nat × Nat) where
  step := fun (arr, i) =>
    let total := length arr
    if i + len ≤ total then
      let off := (from_ + i) * elemBytes a
      pure (.Yield (off, len * elemBytes a) (arr, i + len))
    else pure .Stop
  inject := fun arr => pure (arr, from_)

/-- Unfold an array into its successive fixed-length sub-arrays of `len`
    elements each (`splitterFromLen`). -/
@[inline] def splitterFromLen [Monad m] [Unbox a] (from_ len : Nat) :
    Unfold m (MutArray a) (MutArray a) where
  step := fun (arr, i) =>
    if i + len ≤ length arr then pure (.Yield (unsafeSliceOffLen i len arr) (arr, i + len))
    else pure .Stop
  inject := fun arr => pure (arr, from_)

end MutArray
end Data
