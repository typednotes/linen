/-
  Linen.Graphics.Image.Processing.Interpolation — sampling an image at a
  non-integer coordinate

  ## Haskell equivalent
  `Graphics.Image.Processing.Interpolation` from
  https://hackage.haskell.org/package/hip (module #13 of the `hip` import
  plan, see `docs/imports/hip/dependencies.md`). Upstream defines the
  `Interpolation` class plus three strategies: `Nearest`, `Bilinear`, and
  `Bicubic`.

  ## The `Interpolation` class

  Upstream declares `interpolate` with a per-call `ColorSpace cs e`
  constraint, not a constraint on the `Interpolation` instance itself:

  ```haskell
  class Interpolation method where
    interpolate :: ColorSpace cs e =>
                   method -> Border (Pixel cs e) -> (Int, Int)
                -> ((Int, Int) -> Pixel cs e) -> (Double, Double) -> Pixel cs e
  ```

  This is ported literally as a class whose single field quantifies over
  `cs`/`e`/`px`/`Components` itself, mirroring the `[[Pixel cs e px] [Elevator
  e] [ColorSpace cs e Components]]` binder pattern `Linen.Graphics.Image.
  Interface`'s own `foldrPx`/`foldlPx`/`foldl1Px`/`toListPx` already use for
  the same reason (`px`/`Components` are `outParam`s that need to be mentioned
  explicitly at the call site to unify with whichever `ColorSpace` instance is
  in scope).

  One deviation: `Bilinear`/`Bicubic` need pixel arithmetic (`f10 - f00`,
  `iPx * (...)`, `f00 + ...`). Upstream gets this for free because
  `Graphics.Image.Interface` derives a generic `Num (Pixel cs e)` instance
  from `liftPx`/`liftPx2`/`promote` for *every* `ColorSpace cs e`. Per
  `Interface.lean`'s own doc-comment, this port defers that generic `Num`
  derivation to individual colour-space modules instead (Lean's `Num` splits
  into `Add`/`Sub`/`Mul`/…, so which pieces make sense is a per-colour-space
  choice) — meaning not every `ColorSpace cs e` in scope is guaranteed to carry
  an `Add`/`Sub`/`Mul` instance on its pixel type `px`. Rather than depend on
  that, `interpolate`'s signature adds explicit `[Add e] [Sub e] [Mul e]`
  constraints on the *component* type `e` instead, and all pixel arithmetic
  below is written directly in terms of `liftPx2` (channel-wise, exactly what
  a derived `Num (Pixel cs e)` instance would have done). Since every
  component type ported so far (`UInt8`/`UInt16`/`UInt32`/`UInt64`/`Int`/
  `Float`/`Float32`) already has `Add`/`Sub`/`Mul` from the Lean standard
  library, this adds no real burden at any call site — it only makes explicit
  a constraint upstream's design leaves implicit.

  ## `Nearest`/`Bilinear`/`Bicubic`

  Ported as literal transcriptions of upstream's three `Interpolation`
  instances:

  * `Nearest` rounds `(i, j)` to the nearest integer pair and looks up that
    one pixel through `handleBorderIndex` — 1 lookup, no arithmetic, so it
    needs none of the `Add`/`Sub`/`Mul` machinery above (even though the
    class as a whole requires it).
  * `Bilinear` looks up the 4 integer neighbours surrounding `(i, j)`
    (`(i0,j0)`, `(i1,j0)`, `(i0,j1)`, `(i1,j1)` with `i1 = i0+1`, `j1 = j0+1`)
    and blends them with the fractional parts of `i`/`j`, exactly upstream's
    `fi0 + jPx*(fi1-fi0)` where `fi0 = f00 + iPx*(f10-f00)`, `fi1 = f01 +
    iPx*(f11-f01)`.
  * `Bicubic a` looks up the 16 integer neighbours in the 4×4 block
    surrounding `(i, j)` (offsets `-1, 0, 1, 2` in both axes), weights each by
    upstream's Keys/Mitchell-style cubic kernel `weight`, and normalises by
    the sum of the 16 weights. Upstream writes all 16 lookups out by hand;
    this port instead folds over the 4×4 offset grid (still a fixed, bounded
    16-term traversal — no new termination machinery, just `List.foldl`/
    `List.flatMap` over two 4-element literal lists in place of 16 named
    `let`s) since the two are behaviourally identical and the fold reads more
    directly as "sum the 16 weighted neighbours".

  Every strategy performs a *fixed* number of neighbour lookups per call (1,
  4, and 16 respectively) — none of them recurse, so no termination argument
  is needed beyond what `handleBorderIndex`/`liftPx2`/`List.foldl` already
  provide.

  ## `Double` → `Int` conversion

  Upstream's `round`/`floor` (from `RealFrac`) produce an `Int` directly.
  Lean's `Float.round`/`Float.floor` instead produce another (now
  integral-valued) `Float`, and Lean's core library only exposes
  `Float.toUInt64` (unsigned, truncating) to get back to a fixed-width
  integer — no signed `Float → Int` conversion. Since sampling coordinates
  can be negative near a border, this module adds a small private helper
  `floatToInt` that converts an *already integral* `Float` to `Int` by
  splitting on sign and going through `Float.toUInt64` on the (non-negative)
  magnitude — exact for any integral input, which `.floor`/`.round`'s result
  always is here.
-/

import Linen.Graphics.Image.Interface

open Graphics.Image.Interface
  (Pixel ColorSpace AlphaSpace Border handleBorderIndex promote liftPx2)
open Graphics.Image.Interface.Elevator (Elevator fromFloat)

namespace Graphics.Image.Processing.Interpolation

-- ── `Double` → `Int` conversion ──

/-- Convert an already integral-valued `Float` (e.g. the result of
`Float.floor`/`Float.round`) to `Int`, exactly. See the module doc-comment for
why Lean needs this helper where Haskell's `round`/`floor` give an `Int`
directly. -/
private def floatToInt (x : Float) : Int :=
  if x < 0 then
    -(Int.ofNat (-x).toUInt64.toNat)
  else
    Int.ofNat x.toUInt64.toNat

/-- Convert an `Int` to `Float`, exactly (for any value within `Int64`'s
range, which every image coordinate/offset here stays well within). Lean's
core library has no direct `Int.toFloat`; this goes through `Int.toNat` on
the (non-negative) magnitude, mirroring `floatToInt` above. -/
private def intToFloat (n : Int) : Float :=
  if n < 0 then
    -((-n).toNat.toFloat)
  else
    n.toNat.toFloat

-- ── The `Interpolation` class ──

/-- Implementation for an interpolation method: constructing a new pixel from
information about neighbouring pixels of an image, at a real-valued `(i, j)`
location. Upstream's `Interpolation`. See the module doc-comment for the
`[Add e] [Sub e] [Mul e]` constraints, added here in place of upstream's
generic derived `Num (Pixel cs e)` instance. -/
class Interpolation (method : Type) where
  /-- Construct a new pixel by using information from neighbouring pixels.
  Upstream's `interpolate`. -/
  interpolate {cs e px Components : Type} [Pixel cs e px] [Elevator e]
      [ColorSpace cs e Components] [Add e] [Sub e] [Mul e]
      (m : method)
      -- Border resolution strategy.
      (border : Border px)
      -- Image dimensions: `m` rows and `n` columns.
      (mn : Int × Int)
      -- Lookup function that returns a pixel at the `i`th and `j`th location.
      (getPx : Int × Int → px)
      -- Real-valued `i` and `j` index.
      (ij : Float × Float) : px

export Interpolation (interpolate)

-- ── `Nearest` — nearest-neighbour interpolation ──

/-- Nearest-neighbour interpolation method. Upstream's `Nearest`. -/
inductive Nearest where
  /-- The (only) nearest-neighbour strategy. -/
  | nearest
deriving Repr

instance : Interpolation Nearest where
  interpolate _ border mn getPx ij :=
    let (i, j) := ij
    handleBorderIndex border mn getPx (floatToInt i.round, floatToInt j.round)

-- ── `Bilinear` — bilinear interpolation ──

/-- Bilinear interpolation method. Upstream's `Bilinear`. -/
inductive Bilinear where
  /-- The (only) bilinear strategy. -/
  | bilinear
deriving Repr

instance : Interpolation Bilinear where
  interpolate {cs e px Components} [Pixel cs e px] [Elevator e] [ColorSpace cs e Components]
      [Add e] [Sub e] [Mul e] _ border mn getPx ij :=
    let (i, j) := ij
    let getPx' := handleBorderIndex border mn getPx
    let i0 := floatToInt i.floor
    let j0 := floatToInt j.floor
    let i1 := i0 + 1
    let j1 := j0 + 1
    let iPx := promote (cs := cs) (e := e) (fromFloat (i - intToFloat i0))
    let jPx := promote (cs := cs) (e := e) (fromFloat (j - intToFloat j0))
    let f00 := getPx' (i0, j0)
    let f10 := getPx' (i1, j0)
    let f01 := getPx' (i0, j1)
    let f11 := getPx' (i1, j1)
    let add2 := liftPx2 (cs := cs) (e := e) (· + ·)
    let sub2 := liftPx2 (cs := cs) (e := e) (· - ·)
    let mul2 := liftPx2 (cs := cs) (e := e) (· * ·)
    let fi0 := add2 f00 (mul2 iPx (sub2 f10 f00))
    let fi1 := add2 f01 (mul2 iPx (sub2 f11 f01))
    add2 fi0 (mul2 jPx (sub2 fi1 fi0))

-- ── `Bicubic` — bicubic interpolation ──

/-- Bicubic interpolation method: the parameter is usually set between `-0.5`
and `-1.0`. Upstream's `Bicubic`. -/
structure Bicubic where
  /-- The kernel's sharpness parameter. -/
  a : Float
deriving Repr

/-- Upstream's Keys/Mitchell-style cubic convolution kernel weight, as a
function of the (unsigned) distance `x` from the sample point. -/
private def bicubicWeight (a x : Float) : Float :=
  let x' := x.abs
  let x2' := x' * x'
  if x' <= 1 then
    ((a + 2) * x' - (a + 3)) * x2' + 1
  else if x' < 2 then
    a * ((x2' - 5 * x' + 8) * x' - 4)
  else
    0

instance : Interpolation Bicubic where
  interpolate {cs e px Components} [Pixel cs e px] [Elevator e] [ColorSpace cs e Components]
      [Add e] [Sub e] [Mul e] m border mn getPx ij :=
    let (i, j) := ij
    let a := m.a
    let getPx' := handleBorderIndex border mn getPx
    let i1 := floatToInt i.floor
    let j1 := floatToInt j.floor
    -- The 4×4 neighbourhood is `{i1-1, i1, i1+1, i1+2} × {j1-1, j1, j1+1, j1+2}`.
    let offsets : List Int := [-1, 0, 1, 2]
    let weightsX := offsets.map (fun di => bicubicWeight a (intToFloat (i1 + di) - i))
    let weightsY := offsets.map (fun dj => bicubicWeight a (intToFloat (j1 + dj) - j))
    -- The 16 `(pixel, weight)` terms of the 4×4 neighbourhood.
    let terms : List (px × Float) :=
      (offsets.zip weightsX).flatMap (fun (di, wx) =>
        (offsets.zip weightsY).map (fun (dj, wy) =>
          (getPx' (i1 + di, j1 + dj), wx * wy)))
    let w := (terms.map Prod.snd).foldl (· + ·) 0
    let add2 := liftPx2 (cs := cs) (e := e) (· + ·)
    let mul2 := liftPx2 (cs := cs) (e := e) (· * ·)
    let promote' := promote (cs := cs) (e := e)
    let zeroPx := promote' (fromFloat (0 : Float))
    let sumPx := terms.foldl (fun acc (p, wt) => add2 acc (mul2 p (promote' (fromFloat wt)))) zeroPx
    mul2 sumPx (promote' (fromFloat (1 / w)))

end Graphics.Image.Processing.Interpolation
