/-
  Linen.Data.Array.Lens — `Ixed` for Lean's native `Array`

  Port of Hackage's `lens-5.3.6`'s `Data.Array.Lens` (fetched and read via
  Hackage's rendered source). Upstream's real content is a single instance,
  over GHC's boxed/unboxed `Ix`-indexed arrays:

  ```
  instance Ix i => Ixed (Array i e) where
    ix i f arr
      | inRange (bounds arr) i = f (arr ! i) <&> \e -> arr // [(i,e)]
      | otherwise               = pure arr
  ```

  `linen` has no `Data.Array`-style `Ix`-indexed array type — the closest
  and only array-shaped container is Lean's own native `Array`, which is
  `Nat`-indexed (from `0` to `size - 1`) rather than `Ix`-indexed over an
  arbitrary bounds pair. This module gives `Ixed` for that `Array` directly,
  matching the batch plan's own note that this is a strict simplification
  (a total, contiguous `Nat` index range in place of an arbitrary `Ix`
  range), not a lost capability.

  **Scope note (`TraverseMin`/`TraverseMax`).** Upstream's `Data.Array.Lens`
  additionally instantiates `Control.Lens.Traversal`'s `TraverseMin`/
  `TraverseMax` classes for `Array i e` (focusing on the element at the
  smallest/largest in-range index). `linen` has ported neither class (no
  `Linen.Control.Lens.Traversal.TraverseMin`/`TraverseMax` exists yet
  anywhere in the codebase) — adding them from scratch is out of this
  batch's stated scope (`Ixed`/`At`/`Each`/`Cons`/`Snoc`/`Wrapped`
  instances only), mirroring `Linen.Control.Lens.At`'s own precedent of
  deferring an out-of-scope sibling class (`Contains`) to a later batch.
  Skipped here for the same reason. -/

import Linen.Control.Lens.At

namespace Control.Lens

/-- `instance Ix i => Ixed (Array i e) where …`: Lean's native `Array` is
    `Nat`-indexed from `0` to `arr.size - 1`; writing through `ix i` when `i`
    is in range replaces the element at position `i`, and leaves the array
    untouched when `i` is out of range — exactly upstream's `inRange
    (bounds arr) i` guard, specialized to a `0`-based contiguous range. -/
instance instIxedArray {A : Type u} : Ixed (Array A) Nat A where
  ix i := fun {F} [Applicative F] (f : A → F A) (arr : Array A) =>
    if h : i < arr.size then
      (fun a => arr.set i a h) <$> f (arr[i]'h)
    else
      pure arr

end Control.Lens
