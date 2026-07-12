/-
  Linen.Graphics.Image.Processing.Hough — the linear Hough transform for
  line detection

  ## Haskell equivalent
  `Graphics.Image.Processing.Hough` from https://hackage.haskell.org/package/hip
  (module #29 of the `hip` import plan, see `docs/imports/hip/
  dependencies.md`), on module #27 (`Linen.Graphics.Image`, per the dependency
  list — though, exactly as module #28 (`Ahe`) notes for itself, nothing from
  that facade beyond what `Interface`/`ColorSpace.Y` already export is
  actually used below), module #3 (`Linen.Graphics.Image.Interface`) and
  module #26 (`Linen.Graphics.Image.Types`, likewise unused beyond `Interface`/
  `ColorSpace.Y`). `raw.githubusercontent.com/lehins/hip/master/src/Graphics/
  Image/Processing/Hough.hs` 404s, as with every other module in this import;
  fetched instead from the 1.5.6.0 release tarball (`hackage.haskell.org/
  package/hip-1.5.6.0/hip-1.5.6.0.tar.gz`, `src/Graphics/Image/Processing/
  Hough.hs`, read in full — a short, ~110-line module).

  ## Upstream is explicitly experimental

  As with module #28 (`Ahe`), upstream's own module Haddock reads verbatim:

  > /__Warning__/ - This module is experimental and likely doesn't work as
  > expected

  Per this port's "port what upstream ships, not what it should have
  shipped" convention, every oddity documented below is carried over
  faithfully rather than "fixed".

  ## The algorithm actually implemented

  Contrary to what a reader might expect from the module's name ("Hough
  Transform... to identify straight lines") and this port's own task
  description, upstream's `hough` does **not** take a binary/edge image —
  its signature is `Image arr Y Double -> Int -> Int -> Image arr Y Word8`,
  operating directly on a single-channel *luma* image, with no thresholding
  or edge-detection step of its own. For every pixel `(x, y)` of the input,
  upstream computes a local forward-difference gradient (`slope`, this
  port's `slope`/inlined loop body):

  * `orig = image (x, y)`, `x' = image (min (x+1) widthMax, y)`,
    `y' = image (x, min (y+1) heightMax)` (clipped at the image border,
    exactly as `Ahe`'s neighbourhood clipping),
  * `gradient = (orig - x', orig - y')`.

  Only pixels with nonzero gradient magnitude (`mag gradient > 0`) cast a
  vote: for every discretized angle `theta ∈ [0, thetaSz]` (`thetaSz + 1`
  angles, not `thetaSz` — checked directly against the loop bound
  `forM_ [0 .. thetaSz]`), the perpendicular distance `ρ = x·cosθ + y·sinθ`
  from the image centre is computed (with `(x, y)` here the *centre-relative*
  coordinates `(xCtr, yCtr) - (px, py)`, not the pixel's own index), rescaled
  into `[0, distSz)` via `distance * distSz / distMax` (`distMax` the image's
  half-diagonal length), and — if the rescaled distance lands in
  `[0, distSz)` — the accumulator cell `(theta, distance)` is incremented.
  The final output pixel at `(θ, ρ)` is `255 - round(accBin[θ,ρ] / max ×
  255)`: a vote-count heatmap rescaled and *inverted* (more votes → darker),
  not a boolean line-detection result.

  `θ`'s range is degrees-then-radians: `theta_ = theta * 360 / thetaSz / 180
  * π`, i.e. `theta` (an index in `[0, thetaSz]`) is first turned into a
  fraction of `360°` via `thetaSz`, then converted to radians — ported
  exactly as written (checked directly against the source), including the
  fact that the denominator is `thetaSz`, not `thetaSz + 1` even though the
  loop actually visits `thetaSz + 1` angle indices.

  ## Accumulator dimensions: one extra row and column, exactly as `Ahe`

  `accBin`'s bounds are `((0,0), (thetaSz, distSz))`, i.e. `(thetaSz+1) ×
  (distSz+1)` cells (checked directly against `newArray`'s bounds argument),
  but the output image is `makeImage (thetaSz, distSz)`, i.e. only
  `thetaSz × distSz` cells (indices `[0, thetaSz) × [0, distSz)`). The
  accumulator's final row (`theta = thetaSz`) and final column (`distance =
  distSz`, which the `distance_ < distSz` guard below never actually writes)
  are therefore allocated and (for the row) written but never read back by
  `hTransform` — the same "extra allocated cells past what's ever read"
  pattern `Ahe`'s own doc-comment documents for its `thetaSz`/`distSz`
  discussion, ported here just as literally: this port's accumulator is
  sized identically, `(thetaSz+1) × (distSz+1)`, with the same cells left
  unread.

  ## `sub`/`dotProduct`/`fromIntegralP`/`mag`: narrowed from upstream's `Num`/
  `Integral` polymorphism to their one instantiation each

  Upstream declares these four as generic helpers (`Num x => ... -> (x, x)`,
  `(Integral x, Num y) => (x,x) -> (y,y)`, `Floating x => (x,x) -> x`), but
  within this module each is only ever called at exactly one concrete type:
  `sub`/`fromIntegralP`'s argument at `Int × Int`, `dotProduct`/`mag`'s
  argument at `Float × Float` (`fromIntegralP`'s result type). This port
  narrows each to its single actual instantiation rather than reproducing
  upstream's unused polymorphism with Lean's narrower per-operator classes
  (`Add`/`Sub`/`Mul`) — the same "port the fragment of genericity actually
  exercised" convention already used for e.g. `Graphics.Image.ColorSpace.
  Binary`'s `toNum`/`fromNum`.

  ## `truncate`: ported via a local `truncateToInt`, reusing the established
  `floatToInt`/`intToFloat`/`piD` local-copy convention

  Haskell's `truncate` rounds toward zero (unlike `floor`, which rounds
  toward `-∞`); Lean's core library has neither a `Float.pi` constant nor a
  toward-zero `Float → Int` conversion, so — following the exact precedent
  `Linen.Graphics.Image.Processing.Geometric`/`Complex.Fourier` already
  establish for `piD` and the integral-`Float`-to-`Int` conversion
  `floatToInt`/`intToFloat` (each module keeps its own private copy rather
  than sharing one, per those modules' own doc-comments) — this module adds
  a third local helper, `truncateToInt`, built directly on top of the same
  `floatToInt`: `if x < 0 then x.ceil else x.floor` first rounds toward zero
  to an already-integral `Float`, then `floatToInt` converts that exactly,
  exactly mirroring how `Geometric.lean` itself builds its own `floor`-based
  helpers on `floatToInt`.

  ## The `maxAcc = 0` case: a well-defined "no votes cast" rescaling, not a
  precondition violation

  If no pixel ever casts a vote (e.g. a uniform/all-off input with zero
  gradient everywhere, so `mag gradient > 0` never holds), `accBin` stays
  all-zero and `F.maximum accBin = 0`, making upstream's `old / maxAcc` a
  `0.0 / 0.0` division that produces a Haskell `Double` `NaN`, which
  `truncate` then applies **undefined, implementation-dependent** behaviour
  to (GHC's `truncate` on `NaN` is documented as unspecified). Unlike
  `Ahe`'s `thetaSz`/`distSz` mismatch (a genuine "no well-defined upstream
  behaviour for any other input" precondition, `panic!`ed there) or
  `Geometric.scale`'s non-positive factor (also `panic!`ed), this
  degenerate case is not a programming-error input — a valid, `panic!`-free
  input to `hough` (e.g. a solid-colour image) leads straight to it, and the
  *intent* of the formula is unambiguous: with every accumulator cell
  exactly `0`, the "vote count as a fraction of the maximum" is `0` for
  every cell, giving every output pixel `255 - round(0 × 255) = 255` (pure
  white — "no line evidence anywhere"). This port makes that reading
  explicit (`if maxAcc == 0 then 255 else …`) rather than either `panic!`ing
  on a non-error input or letting an undefined `Float → Int` conversion on
  `NaN` run, documenting the substitution here as the record of the
  decision.

  ## `thetaSz = 0`: a genuine precondition, `panic!`ed

  Unlike `maxAcc = 0` above, `thetaSz = 0` **is** a genuine precondition
  violation with no well-defined upstream behaviour: `theta_`'s formula
  divides by `thetaSz` directly (`theta * 360 / thetaSz / 180 * pi`), so
  `thetaSz = 0` is a `0`-divisor `0.0 / 0.0 = NaN` at the very first step of
  every iteration (`theta` ranges over `[0, thetaSz] = [0, 0]`, so `theta =
  0` always, making the numerator `0` too), propagating `NaN` through
  `cos`/`sin`/`truncate` with the same undefined-`truncate`-on-`NaN`
  consequence as above, but for *every* accumulator write rather than a
  cleanly-classifiable "no votes" case — there is no faithful, well-defined
  substitute reading available the way there was for `maxAcc = 0`. Following
  this port's established `Ahe`/`Geometric` convention for exactly this
  situation (a `panic!` with a descriptive message, rather than threading
  `Except`/`Option` through every caller for input that is a programming
  error), this port `panic!`s when `thetaSz == 0`.

  ## The final `fmap toWord8`: already a no-op, dropped

  Exactly as `Ahe`'s own final `I.map (fmap toWord16) accBin` is a no-op
  (`toWord16 : Word16 → Word16 = id` once `accBin` is already `Word16`-typed)
  — upstream's `hough`'s final line, `I.map (fmap toWord8) hImage`, is the
  same pattern one level up: `hImage` is already `Image arr Y Word8`
  (built directly above by `makeImage`/`hTransform`), so this final `fmap
  toWord8` calls `Elevator.toWord8 : Word8 → Word8` (the identity, per
  module #2's own `Elevator UInt8` instance) on an already-`UInt8` pixel.
  This port skips it and returns `hTransform`'s image directly, exactly as
  `Ahe` does for its own analogous redundant pass.

  ## Fixture/test naming

  Tests in `Tests/Linen/Graphics/Image/Processing/HoughTest.lean` use a
  `hough`-prefix on every fixture, to avoid cross-file `Tests` namespace
  collisions.
-/

import Linen.Graphics.Image.Interface
import Linen.Graphics.Image.ColorSpace.Y

open Graphics.Image.Interface (Image dims index makeImage)
open Graphics.Image.ColorSpace.Y (Y PixelY)

namespace Graphics.Image.Processing.Hough

-- ── `Float`/`Int` conversion helpers (local copies, see the module doc-comment) ──

/-- Convert an already integral-valued `Float` (e.g. the result of
`Float.floor`/`Float.ceil`) to `Int`, exactly. Local copy of the helper
`Linen.Graphics.Image.Processing.Geometric`/`Complex.Fourier` already use,
each keeping its own copy — see those modules' doc-comments. -/
private def floatToInt (x : Float) : Int :=
  if x < 0 then
    -(Int.ofNat (-x).toUInt64.toNat)
  else
    Int.ofNat x.toUInt64.toNat

/-- Convert an `Int` to `Float`, exactly (within `Int64`'s range, which every
image coordinate/dimension here stays well within). -/
private def intToFloat (n : Int) : Float :=
  if n < 0 then
    -((-n).toNat.toFloat)
  else
    n.toNat.toFloat

/-- Local copy of `Geometric.lean`/`Complex.Fourier.lean`'s own `piD`: Lean's
core library has no `Float.pi` constant. -/
private def piD : Float := 3.14159265358979323846

/-- Convert a `Float` to `Int` by truncating toward zero, matching Haskell's
`truncate` (as opposed to `floor`, which rounds toward `-∞`). Built directly
on `floatToInt` — see the module doc-comment. -/
private def truncateToInt (x : Float) : Int :=
  floatToInt (if x < 0 then x.ceil else x.floor)

-- ── Small geometric helpers (narrowed from upstream's polymorphism, see the module doc-comment) ──

/-- Subtract two coordinate pairs component-wise. Upstream's `sub`, narrowed
to `Int × Int` — the only instantiation used in this module. -/
def sub (p1 p2 : Int × Int) : Int × Int :=
  (p1.1 - p2.1, p1.2 - p2.2)

/-- The dot product (equivalently, the sum of squares when both arguments
coincide) of a pair of coordinates. Upstream's `dotProduct`, narrowed to
`Float × Float` — the only instantiation used. -/
def dotProduct (p1 p2 : Float × Float) : Float :=
  p1.1 * p2.1 + p1.2 * p2.2

/-- Convert an `Int` coordinate pair to a `Float` one. Upstream's
`fromIntegralP`, narrowed to its one instantiation (`Int → Float`). -/
def fromIntegralP (p : Int × Int) : Float × Float :=
  (intToFloat p.1, intToFloat p.2)

/-- The Euclidean magnitude of a coordinate pair. Upstream's `mag`. -/
def mag (p : Float × Float) : Float :=
  Float.sqrt (dotProduct p p)

-- ── `hough` — the linear Hough transform ──

/-- Computes the linear Hough transform, mapping each point `(θ, ρ)` of a
`thetaSz × distSz` accumulator image to a rescaled, inverted vote count:
brighter cells (closer to `255`) are cells with fewer votes, darker cells
more. See the module doc-comment for:

* the full algorithm derivation (a centre-relative forward-difference
  gradient, thresholded to `> 0`, casting one vote per discretized angle
  into a `(θ, ρ)` accumulator);
* why the accumulator is one row and one column larger than the output
  image, exactly as `Ahe`'s own accumulator/output-size discussion;
* why `thetaSz = 0` is `panic!`ed (a genuine division-by-zero precondition)
  while an all-zero accumulator (`maxAcc = 0`) is instead given the
  well-defined "no votes anywhere → pure white" reading.

Upstream's `hough`. -/
def hough (image : Image Y Float) (thetaSz distSz : Int) : Image Y UInt8 :=
  if thetaSz == 0 then
    panic! "Graphics.Image.Processing.Hough.hough: thetaSz must be nonzero \
      (theta_'s formula divides by thetaSz, an undefined division by zero otherwise)"
  else
    let (rows, cols) := dims image
    let widthMax := rows - 1
    let xCtr := widthMax / 2
    let heightMax := cols - 1
    let yCtr := heightMax / 2
    let distMax : Float :=
      Float.sqrt
        (intToFloat
          ((heightMax + 1) * (heightMax + 1) + (widthMax + 1) * (widthMax + 1))) / 2
    let accRows := (thetaSz + 1).toNat
    let accCols := (distSz + 1).toNat
    let accBin : Array Float := Id.run do
      let mut acc := Array.replicate (accRows * accCols) (0.0 : Float)
      for xN in [0:rows.toNat] do
        for yN in [0:cols.toNat] do
          let x := Int.ofNat xN
          let y := Int.ofNat yN
          let orig := (index image (x, y)).y
          let gx := (index image (min (x + 1) widthMax, y)).y
          let gy := (index image (x, min (y + 1) heightMax)).y
          let gradient := (orig - gx, orig - gy)
          if mag gradient > 0 then
            let (xCtrF, yCtrF) := fromIntegralP (sub (xCtr, yCtr) (x, y))
            for thetaN in [0:accRows] do
              let theta := Int.ofNat thetaN
              let theta_ := intToFloat theta * 360 / intToFloat thetaSz / 180 * piD
              let distance := Float.cos theta_ * xCtrF + Float.sin theta_ * yCtrF
              let distanceI := truncateToInt (distance * intToFloat distSz / distMax)
              if distanceI >= 0 && distanceI < distSz then
                let idx := thetaN * accCols + distanceI.toNat
                acc := acc.set! idx (acc.getD idx 0.0 + 1)
      pure acc
    let maxAcc := accBin.foldl max (0.0 : Float)
    makeImage (thetaSz, distSz) (fun (x, y) =>
      let idx := x.toNat * accCols + y.toNat
      let v := accBin.getD idx 0.0
      let l : UInt8 :=
        if maxAcc == 0.0 then
          255
        else
          (255 - truncateToInt (v / maxAcc * 255)).toNat.toUInt8
      (⟨l⟩ : PixelY UInt8))

end Graphics.Image.Processing.Hough
