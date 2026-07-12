/-
  Linen.Graphics.Image.Processing.Ahe — adaptive (local-rank) histogram
  equalization

  ## Haskell equivalent
  `Graphics.Image.Processing.Ahe` from https://hackage.haskell.org/package/hip
  (module #28 of the `hip` import plan, see `docs/imports/hip/
  dependencies.md`), on module #18 (`Linen.Graphics.Image.Processing.Filter`,
  via `Filter`/`Direction`/`correlate`), module #3 (`Linen.Graphics.Image.
  Interface`), module #27 (`Linen.Graphics.Image`, per the dependency list —
  though nothing from that facade beyond names `Interface` already exports is
  actually used, see below) and module #26 (`Linen.Graphics.Image.Types`,
  likewise unused beyond what `Interface`/`ColorSpace.Y` already provide).
  `raw.githubusercontent.com/lehins/hip/master/src/Graphics/Image/Processing/
  Ahe.hs` 404s, as with every other module in this import; fetched instead
  from the 1.5.6.0 release tarball (`hackage.haskell.org/package/
  hip-1.5.6.0/hip-1.5.6.0.tar.gz`, `src/Graphics/Image/Processing/Ahe.hs`,
  read in full — a short, ~55-line module).

  ## Upstream is explicitly experimental

  Upstream's own module Haddock reads verbatim:

  > /__Warning__/ - This module is experimental and likely doesn't work as
  > expected

  This is not this port's own hedge — it is upstream's, and the analysis
  below (several apparent bugs, an unused parameter, and a partial function
  with no validated precondition) corroborates it directly. Per this
  project's "port what upstream ships, not what it should have shipped"
  convention (see e.g. `Linen.Graphics.Image`'s own doc-comment on `product`),
  every one of these oddities is carried over faithfully rather than
  "fixed", with this note as the record of each.

  ## The algorithm actually implemented

  Despite the module's name and Haddock prose ("contrast enhancement in a
  neighborhood region… a characteristic length scale"), upstream's `ahe` is
  **not** the classic tile-based CLAHE algorithm (divide into tiles, build a
  per-tile histogram, equalize each tile, bilinearly interpolate between
  tile results). It is a **local-rank transform**: for every pixel `(x, y)`
  of a Laplacian-preprocessed image, it counts how many pixels in a fixed,
  axis-separable `11×11` neighbourhood (clipped at the image border) are
  strictly less than the centre pixel, then rescales that rank by `255` into
  a `Word16` output pixel. Concretely, reading upstream's `where`-clause
  line by line:

  * `ip = applyFilter (simpleFilter Horizontal Edge) image` — the whole input
    is first passed through `simpleFilter`'s kernel with `Edge` border
    resolution. `simpleFilter`'s kernel is `[[0,-1,0],[-1,4,-1],[0,-1,0]]`,
    the standard discrete Laplacian (a second-derivative/edge-detection
    kernel, not a smoothing one) — despite the call-site comment "Pre-
    processing (Border resolution)" suggesting otherwise; the *border*
    resolution strategy is `Edge`, but the *kernel itself* is a Laplacian,
    checked directly against the source. `simpleFilter`'s `Direction`
    parameter is also dead: both its `Vertical` and `Horizontal` branches
    build the exact same kernel literal (checked directly against the
    source; ported as a literal transcription of that duplication, not
    consolidated away, per the same "port what upstream ships" rule).
  * `var1 = rows ip - 1`, `var2 = cols ip - 1` (and `_widthMax`/`_heightMax`,
    unused underscore-prefixed duplicates of the same two values — dropped
    here as dead bindings with no observable effect, unlike the *used*
    dead-code items below).
  * For every `(x, y)` with `x ∈ [0, var1]`, `y ∈ [0, var2]` (i.e. every pixel
    of `ip`): `neighborhood a maxValue = filter (\a -> a ≥ 0 ∧ a < maxValue)
    [a-5 .. a+5]` is applied once to `x` (bounded by `var1`) and once to `y`
    (bounded by `var2`), and `rank` counts every `(i, j)` in the
    *cross product* of those two 1-D lists (so an axis-aligned, up-to-`11×11`
    window, clipped near the border — not a disc/circle despite "region")
    for which `ip(x, y) > ip(i, j)` (upstream's `I.index ip (x, y) > I.index
    ip (i, j)`, a direct `Pixel Y Double` comparison — ported here as a
    direct comparison of the two pixels' single `.y` field, since `PixelY e`
    has exactly that one field and no `Ord (Pixel Y e)` instance exists
    anywhere in this port to call generically, per `Graphics.Image`'s own
    `maximum`/`minimum`/`normalize` doc-comment note about the same gap).
  * The output pixel at `(x, y)` is `PixelY (fromIntegral (rank * 255))` —
    `rank` is bounded by the neighbourhood's size (at most `11 * 11 = 121`,
    typically fewer near a border), so `rank * 255 ≤ 30855`, comfortably
    inside `Word16`'s `[0, 65535]` range with no truncation ever actually
    occurring in practice, even though `fromIntegral :: Int -> Word16` is, in
    general, a wrapping (not saturating) conversion.
  * Finally, `ahe … = I.map (fmap toWord16) accBin`: `accBin` is *already* a
    `Image arr Y Word16` (built directly above), so this outer `fmap
    toWord16` calls `Elevator.toWord16 : Word16 → Word16` on an
    already-`Word16` value. `Elevator.lean`'s own `instance Elevator UInt16`
    already fixes `toWord16 := id` (module #2's own transcription of
    upstream's `Elevator Word16` instance, which is likewise the identity),
    so this final pass is observably a no-op; this port skips it and returns
    the local-rank image directly, documenting the redundancy rather than
    threading an identity `map` through for its own sake.

  ## The `neighborhoodFactor` parameter: upstream's own dead argument

  `ahe`'s third `Int` parameter, `neighborhoodFactor`, is **never referenced**
  anywhere in upstream's `where`-clause — checked directly against the
  source. The neighbourhood radius actually used is the hardcoded literal
  `5` inside `neighborhood`'s `[a-5 .. a+5]`, entirely independent of any
  argument. This port keeps the parameter (for signature fidelity — a caller
  porting code that calls `ahe img thetaSz distSz factor` should not need to
  drop an argument) but names it `_neighborhoodFactor`, Lean's convention for
  a deliberately unused binder, with this note as the record of why.

  ## `thetaSz`/`distSz`: a partial precondition, not a resizing parameter

  `ahe`'s Haddock labels `thetaSz`/`distSz` "width/height of output image",
  and its type signature end result is `Image arr Y Word16` — suggesting a
  resize. But nothing in the body actually resizes anything: `accBin` is
  allocated via `I.new (thetaSz, distSz)` as a *fresh, independent* mutable
  array, while every write into it, `I.write arr (x, y) …`, is indexed by
  `(x, y)` ranging over `ip`'s own dimensions (`var1`, `var2`), not
  `thetaSz`/`distSz`. In GHC, `MArray`'s `write`/`new` are both bounds-checked
  at runtime: if `thetaSz < rows ip` or `distSz < cols ip`, some write lands
  out of the freshly-allocated array's bounds and throws; if `thetaSz > rows
  ip` or `distSz > cols ip`, the array is allocated correctly but its
  "extra" rows/columns are never written by the loop at all, leaving
  whatever `I.new`'s own initial-fill value is (itself unspecified by hip's
  public API) — i.e. upstream itself has no well-defined behaviour for any
  `(thetaSz, distSz)` other than exactly `(rows ip, cols ip)`. Rather than
  invent a resizing/cropping/padding semantics upstream never specifies (the
  "genuinely out of scope" carve-out for simplifications, not a shortcut
  around a real proof obligation), this port makes the one combination
  upstream's own code path is actually well-defined for — `thetaSz = rows ip
  ∧ distSz = cols ip` — an explicit precondition, `panic!`ing with a
  descriptive message otherwise, following this port's established
  `Border`-adjacent-error convention already used for e.g. `Graphics.Image.
  Processing.Geometric.leftToRight`/`topToBottom`'s dimension-mismatch
  `panic!`s (see that module's doc-comment): "a `panic!` with a descriptive
  message, rather than threading `Except`/`Option` through every caller for
  input that is a programming error."

  ## Fixture/test naming

  Tests in `Tests/Linen/Graphics/Image/Processing/AheTest.lean` use an
  `ahe`-prefix on every fixture, to avoid cross-file `Tests` namespace
  collisions.
-/

import Linen.Graphics.Image.Interface
import Linen.Graphics.Image.ColorSpace.Y
import Linen.Graphics.Image.ColorSpace.X
import Linen.Graphics.Image.Processing.Convolution
import Linen.Graphics.Image.Processing.Filter

open Graphics.Image.Interface (Border Image dims index makeImage fromLists)
open Graphics.Image.ColorSpace.Y (Y PixelY)
open Graphics.Image.ColorSpace.X (X PixelX)
open Graphics.Image.Processing.Convolution (correlate)
open Graphics.Image.Processing.Filter (Filter Direction)

namespace Graphics.Image.Processing.Ahe

-- ── `simpleFilter` — the Laplacian kernel used as `ahe`'s preprocessing pass ──

/-- Supplementary function for applying border resolution and a Laplacian
kernel. Upstream's `simpleFilter`; see the module doc-comment for why both
`Direction` branches build the identical kernel (a faithfully-preserved
upstream duplication, not a bug introduced by this port). -/
def simpleFilter (dir : Direction) (border : Border (PixelY Float)) : Filter Y Float where
  applyFilter img :=
    let kernel : Image X Float :=
      match dir with
      | .vertical =>
        fromLists ([[0, -1, 0], [-1, 4, -1], [0, -1, 0]] : List (List (PixelX Float)))
      | .horizontal =>
        fromLists ([[0, -1, 0], [-1, 4, -1], [0, -1, 0]] : List (List (PixelX Float)))
    correlate border kernel img

-- ── `ahe` — the local-rank transform ──

/-- A clipped, one-dimensional neighbourhood `[a - 5, a + 5]` around `a`,
restricted to `[0, bound)`. Upstream's `neighborhood`. -/
private def neighborhood (a bound : Int) : List Int :=
  (List.range 11).filterMap (fun k =>
    let v := a - 5 + Int.ofNat k
    if v >= 0 && v < bound then some v else none)

/-- `ahe` operates on small, fixed `11×11` "contextual" neighbourhoods of the
image: every pixel is replaced by (a rescaling of) its own local rank among
the pixels in that neighbourhood of a Laplacian-preprocessed image. See the
module doc-comment for the full derivation of this behaviour from upstream's
source, including:

* why this is a local-rank transform rather than tile-based CLAHE;
* why `neighborhoodFactor` is unused (upstream's own dead parameter, kept
  only for signature fidelity);
* why `thetaSz`/`distSz` must equal the (Laplacian-preprocessed) input
  image's own `(rows, cols)` — upstream's code path has no well-defined
  behaviour for any other combination, so this port `panic!`s (upstream's
  `error`, via an out-of-bounds `MArray` write/read) rather than inventing an
  unspecified resizing semantics.

Upstream's `ahe`. -/
def ahe (image : Image Y Float) (thetaSz distSz : Int) (_neighborhoodFactor : Int) :
    Image Y UInt16 :=
  let ip := (simpleFilter .horizontal .edge).applyFilter image
  let (m, n) := dims ip
  if thetaSz != m || distSz != n then
    panic! s!"Graphics.Image.Processing.Ahe.ahe: output dimensions ({thetaSz}, {distSz}) must \
      equal the (Laplacian-preprocessed) input image's dimensions ({m}, {n})"
  else
    makeImage (m, n) (fun (x, y) =>
      let centre := (index ip (x, y)).y
      let rank : Int :=
        (neighborhood x m).foldl
          (fun acc i =>
            (neighborhood y n).foldl
              (fun acc' j => if centre > (index ip (i, j)).y then acc' + 1 else acc')
              acc)
          0
      (⟨(rank * 255).toNat.toUInt16⟩ : PixelY UInt16))

end Graphics.Image.Processing.Ahe
