/-
  Linen.Data.Array.Shaped — rank-polymorphic, shape-indexed arrays

  ## Haskell equivalent
  `Data.Array.Repa` from https://hackage.haskell.org/package/repa

  ## Design
  Upstream's `Data.Array.Repa` is a re-export facade gathering every module
  of the `repa` package (`Shape`, `Index`, `Slice`, `Base`, every kept
  representation, every operator, `Specialised.Dim2`, `Stencil`) behind a
  single import, plus a QuickCheck `Data.Array.Repa.Arbitrary` import that
  is dropped here per `docs/imports/repa/dependencies.md` (testing-only
  infrastructure; `linen` uses `#guard`, not QuickCheck). This module
  mirrors that shape: every declaration already lives directly in the
  `Data.Array.Shaped` namespace, so — as with `Stencil.lean` — no further
  `export` is needed, only the transitive imports.
-/

import Linen.Data.Array.Shaped.Base
import Linen.Data.Array.Shaped.Index
import Linen.Data.Array.Shaped.Operators.IndexSpace
import Linen.Data.Array.Shaped.Operators.Interleave
import Linen.Data.Array.Shaped.Operators.Mapping
import Linen.Data.Array.Shaped.Operators.Reduction
import Linen.Data.Array.Shaped.Operators.Selection
import Linen.Data.Array.Shaped.Operators.Traversal
import Linen.Data.Array.Shaped.Repr.Cursored
import Linen.Data.Array.Shaped.Repr.Delayed
import Linen.Data.Array.Shaped.Repr.Manifest
import Linen.Data.Array.Shaped.Repr.Partitioned
import Linen.Data.Array.Shaped.Repr.Undefined
import Linen.Data.Array.Shaped.Shape
import Linen.Data.Array.Shaped.Slice
import Linen.Data.Array.Shaped.Specialised.Dim2
import Linen.Data.Array.Shaped.Stencil
