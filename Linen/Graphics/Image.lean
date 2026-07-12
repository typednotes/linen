/-
  Linen.Graphics.Image — the top-level public facade of the whole `hip`
  library

  ## Haskell equivalent

  `Graphics.Image` from https://hackage.haskell.org/package/hip (module #27
  of the `hip` import plan, see `docs/imports/hip/dependencies.md` — the
  package's own public re-export facade, the last of the "core" chain before
  the three `Image`-dependent processing modules #28–#30). `raw.
  githubusercontent.com/lehins/hip/master/src/Graphics/Image.hs` 404s, as
  with every other module in this import; fetched instead from the 1.5.6.0
  release tarball (`hackage.haskell.org/package/hip-1.5.6.0/hip-1.5.6.0.tar.
  gz`, `src/Graphics/Image.hs`, ~230 lines, read in full).

  ## Confirmed import/re-export set (correcting nothing — module #20's own
  ## finding stands)

  Upstream's own `import` block, read directly off the tarball source:

  ```
  import Graphics.Image.ColorSpace
  import Graphics.Image.IO
  import Graphics.Image.Interface as I hiding (Pixel)
  import Graphics.Image.Types as IP

  import Graphics.Image.Processing as IP
  import Graphics.Image.Processing.Binary as IP
  import Graphics.Image.Processing.Complex as IP
  import Graphics.Image.Processing.Geometric as IP
  #ifndef DISABLE_CHART
  import Graphics.Image.IO.Histogram as IP
  #endif
  ```

  This confirms, rather than corrects, module #20 (`Processing.lean`)'s own
  finding: `Processing.hs` itself re-exports only `Geometric`/
  `Interpolation`/`Convolution`/`Filter`, and it is exactly *here* — the
  top-level `Graphics.Image` facade — that `Processing.Binary` and
  `Processing.Complex` (which pulls in `Processing.Complex.Fourier`
  transitively, per that module's own `import Graphics.Image.Processing.
  Complex.Fourier`) are imported directly, sitting alongside (not routed
  through) the plain `import Graphics.Image.Processing as IP`. The direct
  `import Graphics.Image.Processing.Geometric as IP` is redundant with what
  `Processing as IP` already re-exports (upstream re-imports it a second
  time under the same `IP` qualifier for no additional names); this port
  simply relies on `Linen.Graphics.Image.Processing`'s own transitive
  re-export of `Geometric`, needing no separate import line for it.

  ## Histogram: confirmed excluded, exactly as the CPP guard already signals

  The one Histogram reference in this file, `import Graphics.Image.IO.
  Histogram as IP`, is wrapped in `#ifndef DISABLE_CHART` — upstream's own
  `.cabal` exposes a `disable-chart` flag for users who want to drop the
  `Chart`/`Chart-diagrams`/`diagrams-lib` dependency chain, and defining
  `DISABLE_CHART` (as this whole port does, per `docs/imports/hip/
  dependencies.md`'s scope note) makes this import vanish entirely — it is
  not merely "excluded by policy" here, upstream's own build already omits
  it under the exact configuration this port targets. No other Histogram
  reference (import, export-list item, or otherwise) appears anywhere in
  this file.

  ## Re-export strategy

  As with every other facade in this port (`ColorSpace.lean`, `Processing.
  lean`, `Types.lean`), a plain `import` below is enough to make every name
  from `ColorSpace` (#12), `IO` (#25), `Interface` (#3), `Types` (#26),
  `Processing` (#20, transitively pulling in `Interpolation`/`Geometric`/
  `Convolution`/`Filter`), `Processing.Binary` (#19), and `Processing.Complex`
  (#16, transitively pulling in `Complex.Fourier`, #15) reachable at its full
  name, with no further re-export step needed — Lean's `import` is already
  transitive, unlike Haskell's per-module `module … (…) where` export lists
  that must name every re-exported module explicitly.

  ## Genuine top-level definitions ported here

  Upstream's `Graphics.Image` module is not a pure facade: beyond its
  `module Graphics.Image (…) where` re-export list, it defines a handful of
  small functions of its own directly on `Array`/`BaseArray`. Each is
  accounted for below:

  * `rows`, `cols` — thin wrappers around `dims`, ported directly.
  * `sum`, `product` — `fold (+) 0`/`fold (+) 1` respectively. Upstream's own
    `product` literally reads `fold (+) 1`, using `(+)` rather than `(*)` —
    checked directly against the 1.5.6.0 source, not a transcription slip
    introduced by this port. This looks like an upstream copy-paste bug (the
    result is the same as `sum` when every pixel channel is `≥ 0` and the
    fold starts from `1` instead of `0`, i.e. `product img = sum img +
    pixelCount * 1 - 0`'s multiplicative *name* with additive *behaviour*),
    but "port what upstream ships, not what it should have shipped" applies
    here just as it does everywhere else in this import — so it is
    transcribed literally, `(+)` and all, with this note as the record of
    the discrepancy.
  * `maximum`, `minimum` — `fold max (index00 img) img` / `fold min (index00
    img) img`, needing `Ord (Pixel cs e)` upstream. This port's pixel types
    carry no `Ord`/`Max`/`Min` instance of their own yet (`ColorSpace.X`'s own
    doc-comment already defers `Ord (PixelX e)` for "whichever later module
    actually needs it" — this is that module, for the general `px` case, not
    only `PixelX`): rather than manufacture an instance nobody asked for,
    `maximum`/`minimum` are ported as genuinely `px`-polymorphic functions
    requiring `[Max px]`/`[Min px]` (Lean's direct equivalent of what an
    `Ord`-derived `max`/`min` would provide), ready to use the moment any
    colour space's pixel type picks up such an instance, exactly mirroring
    how `Interface.lean` itself declares whole typeclass hierarchies with no
    instance yet in scope.
  * `normalize` — upstream computes a per-pixel channel maximum/minimum
    (`foldl1Px max`/`min`, wrapped in a scalar `PixelX`), then reduces those
    over the whole image via `maximum`/`minimum` (needing `Ord (Pixel X e)`),
    and finally rescales every channel into `[0, 1]`. This port takes the
    same two-step shape but skips wrapping the per-pixel channel
    reduction in `PixelX` — a `PixelX e` is nothing but a one-field carrier
    for a bare `e`, so reducing directly over a `Manifest DIM2 e` (via the
    already-generic `Interface.map`/`Interface.fold`, neither of which
    constrains its pixel type to be a `Pixel cs e px` instance) reaches the
    same two scalars `l`/`s` with one fewer intermediate wrapper type and
    without needing an `Ord (PixelX e)` instance that, per the point above,
    nothing in this port has defined. This is not a behavioural
    simplification, only a representational shortcut through an
    unconstrained-but-equivalent carrier.
  * `eqTol` — ported directly on `ColorSpace.eqTolPx` (already ported, module
    #12) and `Processing.Binary.toImageBinaryUsing2`/`Processing.Binary.and`
    (already ported, module #19), exactly as upstream composes `IP.and .
    toImageBinaryUsing2 (eqTolPx tol)`.
  * `toLists` — the inverse of `Interface.fromLists`, built directly from
    `dims`/`index`.

  ## Dropped or deferred, all for reasons already established earlier in
  ## this import (nothing new decided here)

  * `makeImageR`, `fromListsR` — both are upstream's `makeImage`/`fromLists`
    with an extra, ignored representation-selector argument (`arr`) bolted
    on purely so a caller can pin down which backend to use. With no
    representation axis left in this port (`Interface.lean`'s own
    representation-collapse decision), there is nothing left for the extra
    argument to select between; `makeImage`/`fromLists` (module #3) already
    cover the same ground with no such parameter to begin with.
  * `exchange`, `(|*|)` — both already dropped at `Interface.lean` itself
    (module #3's own doc-comment, bullets 1 and 5 respectively): `exchange`
    converts an image between representations (none left to convert
    between), and `(|*|)` was declared abstractly in the now-dropped `Array`
    class with every concrete body living in the also-dropped
    `Interface.Vector`/`Interface.Repa` modules — there was no faithful body
    to port there, and still isn't here.
  * `readImageY`, `readImageYA`, `readImageRGB`, `readImageRGBA`,
    `writeImage`, `displayImage` — all thin wrappers around upstream's
    generic `readImage'`/`writeImage`, which `IO.lean`'s own doc-comment
    (module #25) already establishes cannot exist in this port: they need a
    colour-space-generic `Readable (Image cs e) InputFormat`/`Writable (Image
    cs e) OutputFormat` instance family that would in turn need a
    precision-narrowing hook `ColorSpace.lean` never gave `Pixel cs e px`
    (a plain marker class, not a `Functor`-style structure). `displayImage`
    additionally needs the external-viewer machinery `IO.lean` already
    defers as "no GUI/external-process story in this codebase." Both gaps
    are inherited unchanged from module #25, not re-litigated here; a caller
    can still reach the same file at one concrete `(cs, e, format)` triple
    via `Graphics.Image.IO.readImageExact`/`writeImageExact`.
  * `Graphics.Image.IO.Histogram` — excluded per the scope note above; not
    referenced anywhere in this port's `Graphics.Image`.
  * The module-level Haddock ($colorspace) prose, the `>>>` doctest examples
    throughout, and the `BangPatterns`/`INLINE` pragmas are GHC/Haddock-only
    presentation and optimisation directives with no Lean counterpart and
    are dropped along with every other doctest/pragma throughout this port.

  ## Fixture/test naming

  Tests in `Tests/Linen/Graphics/ImageTest.lean` use an `img`-prefix on every
  fixture, to avoid cross-file `Tests` namespace collisions.
-/

import Linen.Graphics.Image.ColorSpace
import Linen.Graphics.Image.IO
import Linen.Graphics.Image.Interface
import Linen.Graphics.Image.Types
import Linen.Graphics.Image.Processing
import Linen.Graphics.Image.Processing.Binary
import Linen.Graphics.Image.Processing.Complex

open Graphics.Image.Interface
  (Pixel ColorSpace Image dims index index00 fold map foldl1Px liftPx)
open Graphics.Image.Interface.Elevator (Elevator)
open Graphics.Image.ColorSpace (eqTolPx)
open Graphics.Image.Processing.Binary (toImageBinaryUsing2 and)

namespace Graphics.Image

-- ── Dimensions ──

/-- Get the number of rows in an image. Upstream's `rows`. -/
def rows {px : Type} (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) : Int :=
  (dims img).1

/-- Get the number of columns in an image. Upstream's `cols`. -/
def cols {px : Type} (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) : Int :=
  (dims img).2

-- ── Reduction ──

/-- Sum all pixels in the image. Upstream's `sum`. -/
def sum {px : Type} [Add px] [OfNat px 0]
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) : px :=
  fold (· + ·) 0 img

/-- Multiply all pixels in the image. Upstream's `product` — see the module
doc-comment for why this is `fold (+) 1`, not `fold (*) 1`, matching
upstream's own apparent bug faithfully. -/
def product {px : Type} [Add px] [OfNat px 1]
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) : px :=
  fold (· + ·) 1 img

/-- Retrieve the biggest pixel from an image. Upstream's `maximum`; see the
module doc-comment for the `Ord (Pixel cs e)` → `[Max px]` substitution. -/
def maximum {px : Type} [Inhabited px] [Max px]
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) : px :=
  fold Max.max (index00 img) img

/-- Retrieve the smallest pixel from an image. Upstream's `minimum`. -/
def minimum {px : Type} [Inhabited px] [Min px]
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) : px :=
  fold Min.min (index00 img) img

/-- Scale all of the pixels of an image to be in the range `[0, 1]`.
Upstream's `normalize`; see the module doc-comment for the `PixelX`-wrapper
shortcut. -/
def normalize {cs e px Components : Type} [Pixel cs e px] [Elevator e] [Inhabited px]
    [ColorSpace cs e Components] [Inhabited e] [Max e] [Min e] [Sub e] [Div e] [Mul e]
    [OfNat e 0] [OfNat e 1] [BEq e] [LT e] [DecidableRel (α := e) (· < ·)]
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  let chanMax : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 e :=
    map (fun p => foldl1Px (cs := cs) (e := e) Max.max p) img
  let chanMin : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 e :=
    map (fun p => foldl1Px (cs := cs) (e := e) Min.min p) img
  let l := fold Max.max (index00 chanMax) chanMax
  let s := fold Min.min (index00 chanMin) chanMin
  if l == s then
    if s < (0 : e) then map (liftPx (cs := cs) (e := e) (· * 0)) img
    else if (1 : e) < s then img
    else img
  else
    map (liftPx (cs := cs) (e := e) (fun v => (v - s) / (l - s))) img

/-- Check whether two images are equal within a tolerance, useful for
comparing images with `Float`/`Float32` precision. Upstream's `eqTol`. -/
def eqTol {cs e px Components : Type} [Pixel cs e px] [Elevator e]
    [ColorSpace cs e Components] [Sub e] [Max e] [Min e] [LE e]
    [DecidableRel (α := e) (· ≤ ·)] (tol : e)
    (img1 img2 : Image cs e) : Bool :=
  and (toImageBinaryUsing2 (cs := cs) (e := e) (eqTolPx (cs := cs) (e := e) tol) img1 img2)

-- ── Conversion ──

/-- Generate a nested list of pixels from an image: the outer list's length
is the number of rows, each inner list's length the number of columns.
Upstream's `toLists`, the inverse of `Interface.fromLists`. -/
def toLists {px : Type} [Inhabited px]
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) : List (List px) :=
  let (m, n) := dims img
  (List.range m.toNat).map
    (fun i => (List.range n.toNat).map (fun j => index img (Int.ofNat i, Int.ofNat j)))

end Graphics.Image
