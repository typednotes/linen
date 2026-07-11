/-
  Linen.Data.Array.Shaped.Operators.Selection — filtering a range of indices

  Ported from Haskell's `Data.Array.Repa.Operators.Selection` (package
  `repa`). Upstream only exposes a parallel `selectP`, built on
  `Eval.Selection`'s Gang-based chunking; the chunking has no distinct
  observable behavior under Lean's eager, sequential evaluation (the same
  reasoning as `Repr/Manifest.lean`'s `computeS`/`copyS`), so this port
  provides a single sequential `select`.
-/

import Linen.Data.Array.Shaped.Repr.Manifest
import Linen.Data.Array.Shaped.Index

namespace Data.Array.Shaped

/-- Produce an array by applying a predicate to a range of integers `[0,
    len)`. Where the predicate matchFn, the given function generates the
    corresponding element; indices for which it does not match are
    dropped. -/
def select {a} [Inhabited a] (matchFn : Int → Bool) (produce : Int → a) (len : Int) :
    Manifest DIM1 a :=
  let vals :=
    (List.range len.toNat).filterMap
      (fun i => if matchFn (Int.ofNat i) then some (produce (Int.ofNat i)) else none)
  Manifest.fromList (ix1 (Int.ofNat vals.length)) vals

end Data.Array.Shaped
