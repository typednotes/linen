/-
  Linen.Graphics.Image.Processing — the processing facade: re-exports
  `Geometric`/`Interpolation`/`Convolution`/`Filter`, plus one genuine
  addition of its own, `pixelGrid`

  ## Haskell equivalent
  `Graphics.Image.Processing` from https://hackage.haskell.org/package/hip
  (module #20 of the `hip` import plan, see `docs/imports/hip/dependencies.md`),
  on `Interface` (#3) and `Processing.Geometric`/`Processing.Interpolation`/
  `Processing.Convolution`/`Processing.Filter` (#14, #13, #17, #18). Read
  directly against the tarball source
  (`hip-1.5.6.0/src/Graphics/Image/Processing.hs`).

  ## Not a pure re-export of everything: `Complex`/`Complex.Fourier`/`Binary`
  are deliberately absent

  The task plan that set up this port anticipated this file re-exporting
  every one of modules #13–#19 (`Interpolation`, `Geometric`,
  `Complex.Fourier`, `Complex`, `Convolution`, `Filter`, `Binary`). Checking
  upstream's actual export list and import block shows this is **not** what
  upstream's `Graphics.Image.Processing` module does: its own `module
  Graphics.Image.Processing (…) where` header lists only `Geometric`,
  `Interpolation`, `Convolution`, `Filter` under `module …` re-export items
  (plus the local `Border(..)`/`pixelGrid`), and its `import` block matches
  exactly — no `import Graphics.Image.Processing.Complex`, `.Complex.Fourier`,
  or `.Binary` appears anywhere in the file. Those three modules are instead
  imported *directly* by the top-level `Graphics.Image` facade (module #27,
  not yet ported at the time of writing) — confirmed by grepping
  `hip-1.5.6.0/src/Graphics/Image.hs`, which has its own `import
  Graphics.Image.Processing.Binary as IP` and `import
  Graphics.Image.Processing.Complex as IP` sitting *alongside* (not routed
  through) its `import Graphics.Image.Processing as IP`. So this port follows
  upstream's actual source, not the plan's anticipatory description: `Binary`/
  `Complex`/`Complex.Fourier` are re-exported by `Linen.Graphics.Image` (#27)
  directly when that module is ported, not by this one.

  ## Re-export strategy

  As with `Linen.Graphics.Image.ColorSpace` (module #12, the precedent for a
  facade in this port), every re-exported sub-module declares its own child
  namespace one level below this file's `Graphics.Image.Processing`
  namespace, and Lean's `import` is already transitive — a plain `import
  Linen.Graphics.Image.Processing` (below) makes every declaration from
  `Geometric`/`Interpolation`/`Convolution`/`Filter` reachable at its fully
  qualified name with no further re-export step needed, unlike Haskell's
  per-module export lists. The `open` statements below exist only so *this
  file's own* definition (`pixelGrid`) can refer to `traverse`/`promote`/etc.
  unqualified.

  ## `Border(..)`

  Upstream re-exports the `Border` constructors as part of this module's own
  export list, even though `Border` itself is declared in `Graphics.Image.
  Interface` (imported, not re-exported, by this file upstream). Lean's
  transitive-import behaviour already makes `Graphics.Image.Interface.Border`
  reachable through this file's own `import Linen.Graphics.Image.Interface`
  (needed anyway for `pixelGrid`'s own body), so no separate re-export
  mechanism is needed here either — matching the `ColorSpace.lean` facade's
  own treatment of transitively-reachable names.

  ## `pixelGrid`

  Upstream's `pixelGrid k img` magnifies `img` by drawing a one-pixel grid
  (mid-grey, `0.5` on every channel) around each original pixel, at a
  magnification factor of `succ (fromIntegral k)` — i.e. a `Word8` argument
  `k` of `0` still produces a magnification of (at least) `1`, since `succ`
  always adds one. Ported directly on `traverse` (already used throughout
  `Interpolation.lean`/`Geometric.lean` for the same "compute new dims, then
  build from an old-index getter" shape), with the same `+1` baked into the
  local `mag` computation (`k.toNat + 1`), `promote`/`Elevator.fromFloat` in
  place of upstream's `promote . fromDouble`, and `%`/`/` on `Int` for
  upstream's `mod`/`div` (both already used the same way, on the same
  nonnegative-by-construction pixel-index type, throughout `Interface.lean`'s
  own `handleBorderIndex`). The `BangPatterns`/`INLINE` pragmas and the
  `ViewPatterns`-based `succ . fromIntegral -> k` argument binder are GHC
  strictness/optimisation/pattern-binding syntax with no Lean counterpart and
  are simply absent from the port; the doctest-only `>>>` example lines are
  dropped along with every other doctest throughout this port (no `readImage`/
  `writeImage` exists in this port to run them against).
-/

import Linen.Graphics.Image.Interface
import Linen.Graphics.Image.Interface.Elevator
import Linen.Graphics.Image.Processing.Geometric
import Linen.Graphics.Image.Processing.Interpolation
import Linen.Graphics.Image.Processing.Convolution
import Linen.Graphics.Image.Processing.Filter

open Graphics.Image.Interface (Pixel ColorSpace traverse promote)
open Graphics.Image.Interface.Elevator (Elevator fromFloat)

namespace Graphics.Image.Processing

/-- Magnify an image by a factor of `k.toNat + 1` and draw a mid-grey
(`0.5` on every channel) one-pixel grid around each original pixel — a
useful inspection tool for zooming into small regions of an image. Upstream's
`pixelGrid`. -/
def pixelGrid {cs e px Components : Type} [Pixel cs e px] [Inhabited px] [Elevator e]
    [ColorSpace cs e Components] (k : UInt8)
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  let mag : Int := (k.toNat : Int) + 1
  traverse img
    (fun (m, n) => (1 + m * mag, 1 + n * mag))
    (fun getPx (i, j) =>
      if i % mag == 0 || j % mag == 0 then
        promote (cs := cs) (e := e) (fromFloat 0.5)
      else
        getPx ((i - 1) / mag, (j - 1) / mag))

end Graphics.Image.Processing
