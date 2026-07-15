/-
  Linen.Data.MutArray.Type — the growable unboxed mutable array

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.MutArray.Type`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/MutArray/Type.hs),
  module #27 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  A `MutArray a` is a *growable unboxed* mutable array: a slice
  `[arrStart, arrEnd)` (in bytes) into a backing `MutByteArray`, whose elements
  are `Unbox`-serialized. `arrBound` is the first invalid byte index of the
  backing buffer — the capacity ceiling below which `snoc` can append without
  reallocating; once it is hit the buffer is reallocated, doubled, and rounded
  up to a power of two.

  ## Representation: `MutByteArray` slice, byte offsets

  Faithful to upstream's record

  ```haskell
  data MutArray a = MutArray
    { arrContents :: !MutByteArray , arrStart :: !Int
    , arrEnd :: !Int , arrBound :: !Int }
  ```

  We keep the same four fields (`arrEnd` → `used`, since `end` is a Lean
  keyword; `arrBound` → `bound`). All offsets are **byte** offsets, so the
  element count is a derived quantity (`length = (used - start) / elemBytes`).
  The `Unbox a` dictionary is a per-operation constraint, not a field (matching
  upstream's non-`DEVBUILD` layout).

  ## Substitutions / deviations

  - **Operations are pure, not `MonadIO`.** Upstream's ops are `MonadIO m => …
    -> m …` because `MutByteArray` is a raw primitive mutated in `IO`. As in the
    already-ported `MutByteArray`/`Unbox` modules, Lean's `MutByteArray` is a
    managed value updated functionally, so the low-level array ops
    (`snoc`/`getIndex`/`putIndex`/`reallocBytes`/slicing/`toList`/`fromList`)
    are pure functions returning the updated array. The `Fold`/`Unfold`/`Stream`
    wrappers (`create`/`createOf`/`reader`/`read`) keep the monadic shape their
    carrier types demand, lifting the pure ops with `pure`.
  - **`unsafeGetIndex`/`unsafeSnoc`/`unsafePutIndex`/`unsafeSliceOffLen` do no
    bounds check** (caller's responsibility, as upstream); their checked
    siblings (`getIndex`/`snocMay`/`putIndex`/`sliceOffLen`) do. `putIndex` is
    a no-op on an out-of-range index rather than throwing (there is no `IO`
    error channel here); `snocMay` returns `none` when the buffer is full.
  - **Pinning is a `MutByteArray` flag.** Reallocation preserves the source
    array's pinned intent, matching upstream's pin-preserving realloc without a
    real pinned heap (see the `MutByteArray` deviations).
  - **Deferred as peripheral** (kept out of this batch, consistent with the
    plan's Tier-3/4 scoping): the `Parser`-driven readers, `readerRev`/
    `readRev` (reverse traversal is easily recovered via `toList.reverse`),
    `growExp`/`rightSize`/`reallocBytesWith` (finer realloc policies over the
    single `reallocBytes` core kept here), `asPtrUnsafe`/`Ptr`-based FFI access
    (no Lean analogue), and the `Serialize`-TH paths. The growable core
    (`snoc`/index/slice/realloc/create/reader) is ported faithfully.
-/

import Linen.Data.Unbox
import Linen.Data.MutByteArray.Type
import Linen.Data.Fold.Type
import Linen.Data.Unfold.Type
import Linen.Data.Stream.Type

namespace Data

open Data (MutByteArray Unbox)
open Data.Fold (Fold)
open Data.Unfold (Unfold)
open Data.Stream (Stream)

-- ── The growable unboxed mutable array ──────────────────────────────────────

/-- A growable unboxed mutable array: the byte slice `[start, used)` of a
    backing `MutByteArray`, with capacity ceiling `bound`. Element (de)coding
    is by the `Unbox` instance supplied per-operation. -/
structure MutArray (a : Type) where
  /-- Backing byte buffer (a superset of the live slice). -/
  contents : MutByteArray
  /-- Byte offset of the first live element (`arrStart`). -/
  start : Nat := 0
  /-- Byte offset just past the last live element (`arrEnd`). -/
  used : Nat := 0
  /-- Byte offset just past the usable buffer — the capacity ceiling
      (`arrBound`). -/
  bound : Nat := 0
  deriving Inhabited

namespace MutArray

/-- Byte size of a single element (`sizeOf` upstream). -/
@[inline] def elemBytes (a : Type) [Unbox a] : Nat := Unbox.size (a := a)

-- ── Size and capacity ───────────────────────────────────────────────────────

/-- Live length of the array in bytes (`arrEnd - arrStart`). -/
@[inline] def byteLength (arr : MutArray a) : Nat := arr.used - arr.start

/-- Number of live elements. -/
@[inline] def length [Unbox a] (arr : MutArray a) : Nat :=
  byteLength arr / elemBytes a

/-- Usable capacity in bytes (`arrBound - arrStart`). -/
@[inline] def byteCapacity (arr : MutArray a) : Nat := arr.bound - arr.start

/-- Usable capacity in elements. -/
@[inline] def capacity [Unbox a] (arr : MutArray a) : Nat :=
  byteCapacity arr / elemBytes a

/-- Unused bytes at the end of the buffer (`arrBound - arrEnd`). -/
@[inline] def bytesFree (arr : MutArray a) : Nat := arr.bound - arr.used

/-- Unused element slots at the end of the buffer. -/
@[inline] def free [Unbox a] (arr : MutArray a) : Nat := bytesFree arr / elemBytes a

-- ── Construction ──────────────────────────────────────────────────────────────

/-- A fresh array with room for `count` elements (unpinned), empty. -/
@[inline] def emptyOf [Unbox a] (count : Nat) : MutArray a :=
  let n := count * elemBytes a
  { contents := MutByteArray.new n, start := 0, used := 0, bound := n }

/-- A fresh *pinned* array with room for `count` elements, empty (`emptyOf'`). -/
@[inline] def emptyOf' [Unbox a] (count : Nat) : MutArray a :=
  let n := count * elemBytes a
  { contents := MutByteArray.newPinned n, start := 0, used := 0, bound := n }

/-- The empty array (zero capacity). -/
@[inline] def empty : MutArray a :=
  { contents := MutByteArray.empty, start := 0, used := 0, bound := 0 }

-- ── Power-of-two growth policy ────────────────────────────────────────────────

/-- Is `n` a power of two (`n > 0` and only one bit set)? -/
@[inline] def isPower2 (n : Nat) : Bool := n != 0 && (n &&& (n - 1)) == 0

/-- Round `n` up to the nearest power of two (`roundUpToPower2`; `≤ 1 ↦ 1`). -/
@[inline] def roundUpToPower2 (n : Nat) : Nat :=
  if n ≤ 1 then 1 else 2 ^ (Nat.log2 (n - 1) + 1)

-- ── Byte copy helper ────────────────────────────────────────────────────────

/-- Copy `len` bytes from `src` at `srcOff` into a copy of `dst` at `dstOff`
    (total: a left fold over the range, no primop). -/
def copyBytes (src : ByteArray) (srcOff : Nat) (dst : ByteArray) (dstOff len : Nat) :
    ByteArray :=
  (List.range len).foldl (fun acc i => acc.set! (dstOff + i) (src.get! (srcOff + i))) dst

-- ── Reallocation ──────────────────────────────────────────────────────────────

/-- Reallocate the backing buffer so its usable capacity is `newCapBytes`
    bytes (but never smaller than the live slice), copying the live slice to
    offset `0`. Preserves the source's pinned intent. -/
def reallocBytes [Unbox a] (newCapBytes : Nat) (arr : MutArray a) : MutArray a :=
  let live := byteLength arr
  let newCap := max newCapBytes live
  let fresh := if arr.contents.pinned then MutByteArray.newPinned newCap
               else MutByteArray.new newCap
  let bytes := copyBytes arr.contents.bytes arr.start fresh.bytes 0 live
  { contents := { fresh with bytes := bytes }, start := 0, used := live, bound := newCap }

/-- Grow the array so it can hold at least `count` more elements without a
    further reallocation, doubling and rounding to a power of two if needed. -/
def growBy [Unbox a] (count : Nat) (arr : MutArray a) : MutArray a :=
  let needed := arr.used + count * elemBytes a
  if needed ≤ arr.bound then arr
  else reallocBytes (roundUpToPower2 (max needed (arr.bound * 2))) arr

-- ── Snoc (append) ─────────────────────────────────────────────────────────────

/-- Append `x` at `used` without any capacity check (caller must ensure room). -/
@[inline] def unsafeSnoc [Unbox a] (arr : MutArray a) (x : a) : MutArray a :=
  { arr with contents := Unbox.pokeAt arr.used arr.contents x,
             used := arr.used + elemBytes a }

/-- Append `x` if there is reserved room, else `none`. -/
@[inline] def snocMay [Unbox a] (arr : MutArray a) (x : a) : Option (MutArray a) :=
  if arr.used + elemBytes a ≤ arr.bound then some (unsafeSnoc arr x) else none

/-- Append `x`, reallocating via `sizer` applied to the current byte capacity
    when the buffer is full. -/
@[inline] def snocWith [Unbox a] (sizer : Nat → Nat) (arr : MutArray a) (x : a) :
    MutArray a :=
  if arr.used + elemBytes a ≤ arr.bound then unsafeSnoc arr x
  else
    let want := max (sizer arr.bound) (byteLength arr + elemBytes a)
    unsafeSnoc (reallocBytes want arr) x

/-- Append `x`, doubling the buffer (rounded to a power of two) when full. -/
@[inline] def snoc [Unbox a] (arr : MutArray a) (x : a) : MutArray a :=
  snocWith (fun cap => roundUpToPower2 (cap * 2)) arr x

-- ── Indexing ──────────────────────────────────────────────────────────────────

/-- Read the element at index `i` without a bounds check. -/
@[inline] def unsafeGetIndex [Unbox a] (i : Nat) (arr : MutArray a) : a :=
  Unbox.peekAt (arr.start + i * elemBytes a) arr.contents

/-- Read the element at index `i`, or `none` if out of range. -/
@[inline] def getIndex [Unbox a] (i : Nat) (arr : MutArray a) : Option a :=
  if i < length arr then some (unsafeGetIndex i arr) else none

/-- Write `x` at index `i` without a bounds check. -/
@[inline] def unsafePutIndex [Unbox a] (i : Nat) (arr : MutArray a) (x : a) : MutArray a :=
  { arr with contents := Unbox.pokeAt (arr.start + i * elemBytes a) arr.contents x }

/-- Write `x` at index `i`; a no-op if `i` is out of range. -/
@[inline] def putIndex [Unbox a] (i : Nat) (arr : MutArray a) (x : a) : MutArray a :=
  if i < length arr then unsafePutIndex i arr x else arr

/-- Apply `f` to the element at index `i` (a no-op if out of range). -/
@[inline] def modifyIndex [Unbox a] (i : Nat) (f : a → a) (arr : MutArray a) : MutArray a :=
  match getIndex i arr with
  | some x => unsafePutIndex i arr (f x)
  | none => arr

-- ── Slicing ─────────────────────────────────────────────────────────────────

/-- The sub-array of `len` elements starting at element `off`, no bounds check
    (`unsafeSliceOffLen`). Shares the backing buffer. -/
@[inline] def unsafeSliceOffLen [Unbox a] (off len : Nat) (arr : MutArray a) : MutArray a :=
  let s := arr.start + off * elemBytes a
  let e := s + len * elemBytes a
  { arr with start := s, used := e, bound := e }

/-- The sub-array of `len` elements from `off`, clamped to the live range. -/
@[inline] def sliceOffLen [Unbox a] (off len : Nat) (arr : MutArray a) : MutArray a :=
  let off := min off (length arr)
  let len := min len (length arr - off)
  unsafeSliceOffLen off len arr

-- ── Lists ───────────────────────────────────────────────────────────────────

/-- All live elements, in order. -/
@[inline] def toList [Unbox a] (arr : MutArray a) : List a :=
  (List.range (length arr)).map (fun i => unsafeGetIndex i arr)

/-- Build an array of at most `count` elements from a list. -/
@[inline] def fromListN [Unbox a] (count : Nat) (xs : List a) : MutArray a :=
  (xs.take count).foldl (fun acc x => unsafeSnoc acc x) (emptyOf count)

/-- Build an array from a whole list. -/
@[inline] def fromList [Unbox a] (xs : List a) : MutArray a :=
  fromListN xs.length xs

-- ── Streaming: reader / read ───────────────────────────────────────────────

/-- Unfold an array into a stream of its elements (`reader`). -/
@[inline] def reader [Monad m] [Unbox a] : Unfold m (MutArray a) a where
  step := fun (arr, i) =>
    if i < length arr then pure (.Yield (unsafeGetIndex i arr) (arr, i + 1))
    else pure .Stop
  inject := fun arr => pure (arr, 0)

/-- Stream the elements of an array in order (`read`). -/
@[inline] def read [Monad m] [Unbox a] (arr : MutArray a) : Stream m a :=
  Stream.unfold reader arr

-- ── Folds: createOf / create ────────────────────────────────────────────────

/-- A fold collecting up to `n` elements into a fresh array (`createOf`). -/
@[inline] def createOf [Monad m] [Unbox a] (n : Nat) : Fold m a (MutArray a) where
  s := MutArray a
  initial := pure (.Partial (emptyOf n))
  step arr x := pure (if length arr < n then .Partial (unsafeSnoc arr x) else .Done arr)
  extract := pure
  final := pure

/-- A fold collecting all input into a growable array (`create`). -/
@[inline] def create [Monad m] [Unbox a] : Fold m a (MutArray a) where
  s := MutArray a
  initial := pure (.Partial empty)
  step arr x := pure (.Partial (snoc arr x))
  extract := pure
  final := pure

-- ── Equality ────────────────────────────────────────────────────────────────

/-- Two arrays are equal when their live element lists are. -/
instance [Unbox a] [BEq a] : BEq (MutArray a) where
  beq x y := x.toList == y.toList

end MutArray
end Data
