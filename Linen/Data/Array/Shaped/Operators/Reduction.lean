/-
  Linen.Data.Array.Shaped.Operators.Reduction — folding and summing arrays

  Ported from Haskell's `Data.Array.Repa.Operators.Reduction` (package
  `repa`). Only the sequential (`*S`) variants are ported; the parallel
  (`*P`) variants are GHC-Gang-based splittings of the same sequential
  reduction (see `Repr/Manifest.lean`'s `computeS`/`copyS` note) and have no
  distinct observable behavior under Lean's eager, sequential evaluation.

  `equalsS` is ported as a plain function rather than a `BEq (arr sh a)`
  instance: `Manifest` already derives `BEq` structurally (comparing
  `extent`/`elems` directly), so a second, generic `Source`-based instance
  would create instance ambiguity for `Manifest` without adding any new
  capability.
-/

import Linen.Data.Array.Shaped.Index
import Linen.Data.Array.Shaped.Operators.Mapping
import Linen.Data.Array.Shaped.Repr.Manifest

namespace Data.Array.Shaped

/-- Sequential reduction of the innermost dimension of an arbitrary-rank
    array. Elements are reduced in the order of their indices, from lowest
    to highest; applications of the operator are associated arbitrarily. -/
def fold {arr sh a} [Shape sh] [Inhabited sh] [Inhabited a] [Source arr]
    (f : a → a → a) (z : a) (a' : arr (Snoc sh Int) a) : Manifest sh a :=
  match Source.extent a' with
  | sh' :. n =>
    let n' := n.toNat
    let rows := (Shape.size sh').toNat
    Manifest.fromList sh'
      ((List.range rows).map (fun i =>
        (List.range n').foldl
          (fun acc j => f acc (Source.unsafeLinearIndex a' (Int.ofNat (i * n' + j)))) z))

/-- Sequential reduction of an array of arbitrary rank to a single scalar
    value. Elements are reduced in row-major order; applications of the
    operator are associated arbitrarily. -/
def foldAll {arr sh a} [Shape sh] [Inhabited a] [Source arr]
    (f : a → a → a) (z : a) (a' : arr sh a) : a :=
  (toList a').foldl f z

/-- Sequential sum of the innermost dimension of an array. -/
def sum {arr sh a} [Shape sh] [Inhabited sh] [Inhabited a] [Add a] [OfNat a 0] [Source arr]
    (a' : arr (Snoc sh Int) a) : Manifest sh a :=
  fold (· + ·) 0 a'

/-- Sequential sum of all the elements of an array. -/
def sumAll {arr sh a} [Shape sh] [Inhabited a] [Add a] [OfNat a 0] [Source arr]
    (a' : arr sh a) : a :=
  foldAll (· + ·) 0 a'

/-- Check whether two arrays have the same shape and contain equal elements,
    sequentially. -/
def equalsS {arr1 arr2 sh a} [Shape sh] [BEq sh] [Inhabited a] [BEq a] [Source arr1] [Source arr2]
    (a1 : arr1 sh a) (a2 : arr2 sh a) : Bool :=
  Source.extent a1 == Source.extent a2 &&
    foldAll (· && ·) true (zipWith (· == ·) a1 a2)

end Data.Array.Shaped
