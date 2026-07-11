/-
  Linen.Data.Array.Shaped.Operators.Mapping — element-wise map and zipWith

  Ported from Haskell's `Data.Array.Repa.Operators.Mapping` (package `repa`).

  Only `map`/`zipWith`/the arithmetic operators and the `Structured` class are
  ported; the `ByteString`/`ForeignPtr`/`Unboxed` representations are not part
  of this port (see `docs/imports/repa/dependencies.md`), so their dedicated
  `Structured` instances are dropped, as are the `HintSmall`/`HintInterleave`
  representations (never used outside `repa` itself and thus out of scope).
  `Manifest` gets the same `Structured` instance upstream gives those
  materialized representations: it collapses to plain `Delayed`, since there
  is no structure worth preserving in a flat backing array.
-/

import Linen.Data.Array.Shaped.Repr.Cursored
import Linen.Data.Array.Shaped.Repr.Delayed
import Linen.Data.Array.Shaped.Repr.Manifest
import Linen.Data.Array.Shaped.Repr.Partitioned
import Linen.Data.Array.Shaped.Repr.Undefined

namespace Data.Array.Shaped

/-- Apply a worker function to each element of an array, yielding a new
    array with the same extent. -/
def map {arr sh a b} [Shape sh] [Inhabited a] [Inhabited b] [Source arr]
    (f : a → b) (a' : arr sh a) : Delayed sh b :=
  let d := delay a'
  fromFunction d.extent (f ∘ d.apply)

/-- Combine two arrays, element-wise, with a binary operator. If the extent
    of the two array arguments differ, the resulting array's extent is
    their intersection. -/
def zipWith {arr1 arr2 sh a b c} [Shape sh] [Inhabited a] [Inhabited b] [Inhabited c]
    [Source arr1] [Source arr2]
    (f : a → b → c) (a1 : arr1 sh a) (a2 : arr2 sh b) : Delayed sh c :=
  fromFunction (Shape.intersectDim (Source.extent a1) (Source.extent a2))
    (fun ix => f (unsafeIndex a1 ix) (unsafeIndex a2 ix))

/-- Element-wise addition. -/
def addP {arr1 arr2 sh e} [Shape sh] [Inhabited e] [Add e] [Source arr1] [Source arr2]
    (a1 : arr1 sh e) (a2 : arr2 sh e) : Delayed sh e := zipWith (· + ·) a1 a2

/-- Element-wise subtraction. -/
def subP {arr1 arr2 sh e} [Shape sh] [Inhabited e] [Sub e] [Source arr1] [Source arr2]
    (a1 : arr1 sh e) (a2 : arr2 sh e) : Delayed sh e := zipWith (· - ·) a1 a2

/-- Element-wise multiplication. -/
def mulP {arr1 arr2 sh e} [Shape sh] [Inhabited e] [Mul e] [Source arr1] [Source arr2]
    (a1 : arr1 sh e) (a2 : arr2 sh e) : Delayed sh e := zipWith (· * ·) a1 a2

/-- Element-wise division. -/
def divP {arr1 arr2 sh e} [Shape sh] [Inhabited e] [Div e] [Source arr1] [Source arr2]
    (a1 : arr1 sh e) (a2 : arr2 sh e) : Delayed sh e := zipWith (· / ·) a1 a2

infixl:65 " +^ " => addP
infixl:65 " -^ " => subP
infixl:70 " *^ " => mulP
infixl:70 " /^ " => divP

/-- Structured versions of `map` and `zipWith` that preserve the
    representation of cursored and partitioned arrays, instead of collapsing
    everything to a plain `Delayed` array. `TR` names the representation of
    the result.

    The second array argument to `szipWith` (the "plain" operand, matching
    upstream's `arr` in `szipWith :: ... -> Array r sh c -> Array r1 sh a ->
    ...`) is fixed at `Type 0`: every representation actually zipped against
    a structured array in this port (`Delayed`, `Manifest`) lives there, so
    this avoids a second independent universe parameter for no observed
    benefit. -/
class Structured.{u} (arr1 : Type → Type → Type u) where
  /-- The target result representation. -/
  TR : Type → Type → Type u
  /-- Structured `map`. -/
  smap {sh a b} [Shape sh] [Inhabited a] [Inhabited b] :
    (a → b) → arr1 sh a → TR sh b
  /-- Structured `zipWith`. If you have a cursored or partitioned source
      array, use that as the second argument (corresponding to `arr1`). -/
  szipWith {arr2 : Type → Type → Type} {sh a b c}
    [Shape sh] [Inhabited a] [Inhabited b] [Inhabited c] [Source arr2] :
    (c → a → b) → arr2 sh c → arr1 sh a → TR sh b

attribute [reducible] Structured.TR

instance : Structured Delayed where
  TR := Delayed
  smap := map
  szipWith := zipWith

instance : Structured Cursored where
  TR := Cursored
  smap f a := makeCursored a.cursor a.extent a.makeCursor a.shiftCursor (f ∘ a.loadCursor)
  szipWith {arr2} {sh} {a} {b} {c} [Shape sh] [Inhabited a] [Inhabited b] [Inhabited c]
      [Source arr2] f a2 av :=
    makeCursored (sh × av.cursor)
      (Shape.intersectDim (Source.extent a2) av.extent)
      (fun ix => (ix, av.makeCursor ix))
      (fun off (ix, cur) => (Shape.addDim off ix, av.shiftCursor off cur))
      (fun (ix, cur) => f (unsafeIndex a2 ix) (av.loadCursor cur))

instance : Structured Manifest where
  TR := Delayed
  smap := map
  szipWith := zipWith

instance : Structured Undefined where
  TR := Undefined
  smap _ a := ⟨a.extent⟩
  szipWith _ _ a := ⟨a.extent⟩

instance [Structured arr1] [Structured arr2] : Structured (Partitioned arr1 arr2) where
  TR := Partitioned (Structured.TR (arr1 := arr1)) (Structured.TR (arr1 := arr2))
  smap f a := ⟨a.extent, a.range, Structured.smap f a.inRangeArr, Structured.smap f a.fallbackArr⟩
  szipWith f a2 a :=
    ⟨a.extent, a.range, Structured.szipWith f a2 a.inRangeArr, Structured.szipWith f a2 a.fallbackArr⟩

end Data.Array.Shaped
