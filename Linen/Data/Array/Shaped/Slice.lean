/-
  Linen.Data.Array.Shaped.Slice — index-space transformation between arrays
  and slices

  Ported from Haskell's `Data.Array.Repa.Slice` (package `repa`). Upstream's
  associated type families `FullShape`/`SliceShape` become `outParam`s on the
  `Slice` class, Lean's usual substitute for a Haskell associated type: given
  the slice-specification type `ss`, they're determined rather than chosen
  freely, exactly like the type families they replace.
-/

import Linen.Data.Array.Shaped.Index

namespace Data.Array.Shaped

/-- Select all indices at a certain position. -/
inductive All : Type where
  | All : All
deriving BEq, Repr, Inhabited

/-- Place holder for any possible shape. -/
inductive Any (sh : Type) : Type where
  | Any : Any sh
deriving BEq, Repr, Inhabited

/-- Class of index types that can map to slices. `full` is the shape of the
    full array, `slice` is the shape of the slice, both determined by the
    slice-specification type `ss` (Haskell's `FullShape ss`/`SliceShape ss`
    type families). -/
class Slice (ss : Type) (full : outParam Type) (slice : outParam Type) where
  /-- Map an index of a full shape onto an index of some slice. -/
  sliceOfFull : ss → full → slice
  /-- Map an index of a slice onto an index of the full shape. -/
  fullOfSlice : ss → slice → full

instance : Slice Z Z Z where
  sliceOfFull _ _ := Z.Z
  fullOfSlice _ _ := Z.Z

instance : Slice (Any sh) sh sh where
  sliceOfFull _ sh := sh
  fullOfSlice _ sh := sh

/-- Fixing this dimension to a concrete index drops it from the slice. -/
instance [Slice sl full slice] : Slice (Snoc sl Int) (Snoc full Int) slice where
  sliceOfFull
    | fsl :. _, ssl :. _ => Slice.sliceOfFull fsl ssl
  fullOfSlice
    | fsl :. n, ssl => Slice.fullOfSlice fsl ssl :. n

/-- Keeping this dimension (`All`) carries it through to the slice. -/
instance [Slice sl full slice] : Slice (Snoc sl All) (Snoc full Int) (Snoc slice Int) where
  sliceOfFull
    | fsl :. All.All, ssl :. s => Slice.sliceOfFull fsl ssl :. s
  fullOfSlice
    | fsl :. All.All, ssl :. s => Slice.fullOfSlice fsl ssl :. s

end Data.Array.Shaped
