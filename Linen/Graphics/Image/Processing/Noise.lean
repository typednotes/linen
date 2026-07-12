/-
  Linen.Graphics.Image.Processing.Noise — synthetic salt-and-pepper noise

  ## Haskell equivalent
  `Graphics.Image.Processing.Noise` from https://hackage.haskell.org/package/hip
  (module #30, the final module, of the `hip` import plan, see `docs/imports/
  hip/dependencies.md`), on module #3 (`Linen.Graphics.Image.Interface`) —
  nothing from module #27 (`Linen.Graphics.Image`)/#26 (`Types`) beyond what
  `Interface`/`ColorSpace.Y` already export is actually used, exactly as
  modules #28 (`Ahe`)/#29 (`Hough`) note for themselves. `raw.
  githubusercontent.com/lehins/hip/master/src/Graphics/Image/Processing/
  Noise.hs` 404s, as with every other module in this import; fetched instead
  from the 1.5.6.0 release tarball (`hackage.haskell.org/package/
  hip-1.5.6.0/hip-1.5.6.0.tar.gz`, `src/Graphics/Image/Processing/Noise.hs`,
  read in full — a short, ~55-line module).

  ## RNG precedence check (`AGENTS.md`'s import-precedence rule)

  Upstream threads `System.Random`'s `StdGen`/`RandomGen` explicitly:
  `saltAndPepper`'s own signature takes a `StdGen` argument (`g`), and its
  helper `randomCoords :: StdGen -> Int -> Int -> [(Int,Int)]` calls
  `randomR` on that same generator, producing an updated generator alongside
  each draw — this is already a *pure*, deterministic seed-threading style
  (no hidden global mutable state, `IO`, or `newStdGen`/`getStdGen` calls
  anywhere in this module itself; those only appear in the module's example
  Haddock usage snippet, not in code being ported). Checking `linen` for an
  existing port of `random` (per the precedence rule: stdlib first) finds
  one already: `docs/imports/index.md`'s "covered by the Lean stdlib"
  section records `random` → `Init.Data.Random` (`RandomGen`, `StdGen`,
  `mkStdGen`, `randNat`, `randBool`) as "already a direct port of this same
  Haskell library" — Lean core's `Init.Data.Random.StdGen`/`stdNext`/
  `stdSplit` is a line-for-line port of GHC's own `System.Random` `StdGen`
  algorithm (same magic constants `40014`/`53668`/`12211`/`2147483563`/
  `40692`/`52774`/`3791`/`2147483399`/`2147483562`). This module therefore
  reuses `StdGen`/`RandomGen`/`randNat` directly from `Init.Data.Random`
  (always in scope, part of the `Init` prelude — no `import` line needed)
  rather than inventing a different generator: the same generator, same
  seed-threading convention, no new RNG code at all.

  `randNat g lo hi` (inclusive-inclusive on `Nat`) stands in for upstream's
  `randomR (lo, hi) g :: (Int, StdGen)` — both draw uniformly (up to
  `randNat`'s documented `1 ± 1/1000` fairness bound; GHC's own `Random Int`
  instance is likewise not perfectly uniform) from a closed integer
  interval and thread the generator the same way; since every interval this
  module draws from is non-negative (`[0, widthMax]`/`[0, heightMax]`, image
  dimensions minus one), `Nat` bounds obtained via `Int.toNat` are exact.

  ## `randomCoords`: from upstream's infinite list to a bounded, structurally
  recursive generator

  Upstream's `randomCoords` is a self-referential *infinite* list (`(rnx1,
  rny1) : randomCoords g2 width height`), safe in Haskell only because the
  caller (`take (noiseIntensity + 1) …`) forces just a finite prefix. Lean
  has no infinite `List`, so this port fuses the "generate" and "take a
  bounded prefix" steps into one structurally recursive function,
  `randomCoords`, taking the exact count `n` to generate as a `Nat` and
  recursing on it — the direct, faithful equivalent of upstream's
  `take (noiseIntensity + 1) (randomCoords g …)`, with no behavioural
  difference (both produce exactly `noiseIntensity + 1` coordinate draws, in
  the same order, threading the same generator the same way).

  ## `saltAndPepper`'s per-pixel rule and the "last write wins" `HashMap`

  For each of the `noiseIntensity + 1` random coordinates `(x, y)` (drawn
  with replacement — repeats are possible, exactly as upstream's `take`
  over `randomCoords` allows), upstream's `forM_` body (in `ST`, mutating a
  thawed copy of the input array in place) writes `0` (black) if
  `(x + y) % 2 == 0`, else `1.0` (white) — *not* a coin-flip between salt and
  pepper as the name "salt-and-pepper noise" might suggest, but a
  coordinate-parity rule: this port carries that faithfully, exactly as
  written, rather than "fixing" it to something more traditionally
  salt-and-pepper-like (per this port's established "port what upstream
  ships" convention, see `Hough.lean`'s doc-comment for the same stance on a
  different oddity). Because later writes in upstream's `forM_` sequence
  overwrite earlier ones at the same coordinate (ordinary mutable-array
  semantics), and every coordinate's *written* value depends only on that
  coordinate itself (not on iteration order — `(x + y) % 2` is the same
  value no matter which draw produced it), this port's purely functional
  equivalent is exact without needing to simulate in-place mutation: fold
  the coordinate list into a `Std.HashMap (Int × Int) Float` via repeated
  `insert` (later entries overwrite earlier ones at the same key, matching
  "last write wins"), then build the output image with `makeImage`, looking
  each pixel up in that map and falling back to the original image's pixel
  where no noise coordinate landed.

  `noiseIntensity = round (noiseLevel * fromIntegral widthMax * fromIntegral
  heightMax)` uses Haskell's `RealFrac.round` (round-half-to-even); this port
  uses `Float.round` (round-half-away-from-zero) via the same `floatToInt`
  helper `Linen.Graphics.Image.Processing.Interpolation`/`Geometric`/
  `Complex.Fourier`/`Hough` already establish as each module's own local
  copy (ties are a measure-zero edge case on `Float` products of an
  arbitrary `noiseLevel` and image dimensions, and `Interpolation.lean`
  itself does not distinguish the two rounding conventions either — this
  port follows that same precedent rather than introducing a bespoke
  round-half-to-even implementation nowhere else in this codebase needs).

  ## No `panic!`-worthy precondition here

  Unlike `Hough`'s `thetaSz = 0`/`Ahe`'s mismatched `thetaSz`/`distSz`, every
  input to `saltAndPepper` is well-defined: `noiseIntensity` can be `0`
  (a single coordinate draw, `noiseLevel` at or near `0`) or as large as
  `widthMax * heightMax` (`noiseLevel` at or near `1`) without dividing by
  anything or producing `NaN`; `widthMax`/`heightMax` being `0` (a `1`-row or
  `1`-column image) simply narrows `randNat`'s draw range to the single
  value `0`, which `randNat` (and upstream's `randomR (0, 0)`) both handle
  without error.

  ## Fixture/test naming

  Tests in `Tests/Linen/Graphics/Image/Processing/NoiseTest.lean` use a
  `noise`-prefix on every fixture, to avoid cross-file `Tests` namespace
  collisions.
-/

import Std.Data.HashMap
import Linen.Graphics.Image.Interface
import Linen.Graphics.Image.ColorSpace.Y

open Graphics.Image.Interface (Image dims index makeImage)
open Graphics.Image.ColorSpace.Y (Y PixelY)

namespace Graphics.Image.Processing.Noise

-- ── `Float`/`Int` conversion helpers (local copies, see the module doc-comment) ──

/-- Convert an already integral-valued `Float` (e.g. the result of
`Float.round`) to `Int`, exactly. Local copy of the helper
`Linen.Graphics.Image.Processing.Interpolation`/`Geometric`/`Complex.Fourier`/
`Hough` already use, each keeping its own copy — see those modules'
doc-comments. -/
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

-- ── `randomCoords` — bounded random-coordinate generation ──

/-- Generate exactly `n` uniformly random coordinate pairs in
`[0, width] × [0, height]`, threading the generator `g` through each draw
(`x` drawn before `y` at each step, matching upstream's `randomR (0, width)
a` then `randomR (0, height) g1`). The direct, structurally recursive
equivalent of upstream's `take n (randomCoords g width height)` — see the
module doc-comment for why an infinite self-referential list is replaced by
recursion on the exact count needed. Upstream's `randomCoords`. -/
def randomCoords (g : StdGen) (width height : Int) : Nat → List (Int × Int)
  | 0 => []
  | n + 1 =>
    let (x, g1) := randNat g 0 width.toNat
    let (y, g2) := randNat g1 0 height.toNat
    (Int.ofNat x, Int.ofNat y) :: randomCoords g2 width height n

-- ── `saltAndPepper` — salt-and-pepper (impulse) noise ──

/-- The value written at a noise coordinate: `0` (black) when the coordinate
sum is even, `1.0` (white) otherwise — upstream's `if a mod 2 == 0 then 0
else 1.0` where `a = x + y`, a coordinate-parity rule rather than a
random choice between salt and pepper (see the module doc-comment). -/
private def noiseValue (coord : Int × Int) : Float :=
  if (coord.1 + coord.2) % 2 == 0 then 0.0 else 1.0

/-- Fold a list of noise coordinates into a lookup table from coordinate to
the value that must be written there, later entries overwriting earlier ones
at the same key — the purely functional equivalent of upstream's sequential
in-place mutable-array writes (`I.write arr i px` inside `forM_`), exact
because each coordinate's written value depends only on the coordinate
itself, not on when it was drawn (see the module doc-comment). -/
private def noiseTable (coords : List (Int × Int)) : Std.HashMap (Int × Int) Float :=
  coords.foldl (fun m c => m.insert c (noiseValue c)) {}

/-- Introduces salt-and-pepper (impulse) noise into a single-channel luma
image: at `noiseIntensity + 1` random coordinates (drawn with replacement),
the pixel is overwritten with `0`/`1.0` according to the coordinate-parity
rule documented above; every other pixel is copied unchanged from the input.

`noiseLevel` is the noise intensity, scaled to `(0, 1)` (upstream's own
documented domain — not enforced here, matching upstream, which likewise
performs no range check).

See the module doc-comment for: the RNG-precedence check justifying reuse of
Lean core's `StdGen`/`randNat`; why the "last write wins" semantics of
upstream's in-place mutation is reproduced exactly by folding into a
`HashMap`; and why no input to this function is a `panic!`-worthy
precondition violation. Upstream's `saltAndPepper`. -/
def saltAndPepper (image : Image Y Float) (noiseLevel : Float) (g : StdGen) :
    Image Y Float :=
  let (rows, cols) := dims image
  let widthMax := rows - 1
  let heightMax := cols - 1
  let noiseIntensity :=
    floatToInt (Float.round (noiseLevel * intToFloat widthMax * intToFloat heightMax))
  let coords := randomCoords g widthMax heightMax (noiseIntensity + 1).toNat
  let table := noiseTable coords
  makeImage (rows, cols) (fun (x, y) =>
    match table[(x, y)]? with
    | some v => (⟨v⟩ : PixelY Float)
    | none => index image (x, y))

end Graphics.Image.Processing.Noise
