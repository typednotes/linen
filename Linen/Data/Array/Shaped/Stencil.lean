/-
  Linen.Data.Array.Shaped.Stencil — efficient computation of stencil-based
  convolutions

  Ported from Haskell's `Data.Array.Repa.Stencil` (package `repa`), a thin
  aggregator pulling in stencil creation (`Stencil.Base`) and application
  (`Stencil.Dim2`, `Specialised.Dim2`). Every declaration already lives
  directly in the `Data.Array.Shaped` namespace (unlike, e.g., `Data.PDF.Content`'s
  submodules), so no further `export` is needed here.
-/

import Linen.Data.Array.Shaped.Specialised.Dim2
import Linen.Data.Array.Shaped.Stencil.Base
import Linen.Data.Array.Shaped.Stencil.Dim2
