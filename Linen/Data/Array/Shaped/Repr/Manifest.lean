/-
  Linen.Data.Array.Shaped.Repr.Manifest — the `Manifest` array representation

  Ported from Haskell's `Data.Array.Repa.Repr.{Unboxed,ForeignPtr,Vector,
  ByteString}` (package `repa`), collapsed into a single representation:
  those four upstream modules differ only in *backing store* (an unboxed
  `Vector`, a raw `ForeignPtr`, a boxed `Vector`, an immutable `ByteString`)
  while their `Source`/`Target` instance logic is structurally identical.
  Lean's persistent `Array e` is the one efficient flat store needed —
  distinguishing "unboxed" from "boxed" from "foreign-pointer-backed" buys
  nothing under Lean's memory model the way it does under GHC's.

  `computeS`/`copyS` are also ported here rather than into a separate
  `Eval.lean`: upstream's `Target` class (dropped, see `Base.lean` and the
  scope notes in `docs/imports/repa/dependencies.md`) was the only reason
  `computeS` needed to live apart from any one representation — it was
  polymorphic in the *destination* representation. With `Target` gone,
  `Manifest` is the only sensible destination, so `computeS`/`copyS` are
  defined directly against the generic `Source` class here.
-/

import Linen.Data.Array.Shaped.Repr.Delayed

namespace Data.Array.Shaped

/-- A manifest array: a shape together with a flat, row-major `Array` of
    elements. -/
structure Manifest (sh e : Type) where
  extent : sh
  elems : Array e
  deriving BEq

instance [Inhabited sh] [Inhabited e] : Inhabited (Manifest sh e) where
  default := ⟨default, #[]⟩

instance : Source Manifest where
  extent a := a.extent
  linearIndex a i := a.elems.getD i.toNat default
  unsafeLinearIndex a i := a.elems.getD i.toNat default

/-- O(n). Convert a list to a manifest array. `panic!`s if the list's length
    does not match the size of the given shape. -/
def Manifest.fromList {sh e} [Shape sh] [Inhabited sh] [Inhabited e] (sh' : sh) (xs : List e) :
    Manifest sh e :=
  if xs.length = (Shape.size sh').toNat then
    ⟨sh', Array.mk xs⟩
  else
    panic! "Linen.Data.Array.Shaped.Repr.Manifest.fromList: provided array shape does not match list length"

/-- O(n). Sequential computation of array elements: materialize any
    `Source` array into a `Manifest`. -/
def computeS {arr sh e} [Shape sh] [Inhabited e] [Source arr] (a : arr sh e) :
    Manifest sh e :=
  ⟨Source.extent a, Array.mk (toList a)⟩

/-- O(n). Sequential copying of a manifest array between representations —
    delays the source, then computes it into a fresh `Manifest`. -/
def copyS {arr sh e} [Shape sh] [Inhabited e] [Source arr] (a : arr sh e) :
    Manifest sh e :=
  computeS (delay a)

/-- O(1). Zip two manifest arrays of identical shape. `panic!`s otherwise. -/
def Manifest.zip {sh a b} [BEq sh] [Inhabited sh] [Inhabited a] [Inhabited b]
    (a1 : Manifest sh a) (a2 : Manifest sh b) : Manifest sh (a × b) :=
  if a1.extent == a2.extent then
    ⟨a1.extent, (a1.elems.zip a2.elems)⟩
  else
    panic! "Linen.Data.Array.Shaped.Repr.Manifest.zip: array shapes not identical"

/-- O(1). Zip three manifest arrays of identical shape. `panic!`s otherwise. -/
def Manifest.zip3 {sh a b c} [BEq sh] [Inhabited sh] [Inhabited a] [Inhabited b] [Inhabited c]
    (a1 : Manifest sh a) (a2 : Manifest sh b) (a3 : Manifest sh c) :
    Manifest sh (a × b × c) :=
  if a1.extent == a2.extent && a1.extent == a3.extent then
    ⟨a1.extent, (a1.elems.zip a2.elems).zip a3.elems |>.map (fun ((x, y), z) => (x, y, z))⟩
  else
    panic! "Linen.Data.Array.Shaped.Repr.Manifest.zip3: array shapes not identical"

/-- O(1). Unzip a manifest array of pairs. -/
def Manifest.unzip {sh a b} [Inhabited a] [Inhabited b] (arr : Manifest sh (a × b)) :
    Manifest sh a × Manifest sh b :=
  (⟨arr.extent, arr.elems.map Prod.fst⟩, ⟨arr.extent, arr.elems.map Prod.snd⟩)

/-- O(1). Unzip a manifest array of triples. -/
def Manifest.unzip3 {sh a b c} [Inhabited a] [Inhabited b] [Inhabited c]
    (arr : Manifest sh (a × b × c)) :
    Manifest sh a × Manifest sh b × Manifest sh c :=
  (⟨arr.extent, arr.elems.map (·.1)⟩,
   ⟨arr.extent, arr.elems.map (·.2.1)⟩,
   ⟨arr.extent, arr.elems.map (·.2.2)⟩)

end Data.Array.Shaped
