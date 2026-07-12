/-
  Linen.Graphics.Image.ColorSpace.Complex — complex-valued pixels, over any
  existing colour space

  ## Haskell equivalent
  `Graphics.Image.ColorSpace.Complex` from
  https://hackage.haskell.org/package/hip (module #9 of the `hip` import
  plan, see `docs/imports/hip/dependencies.md`). Unlike `Y`/`RGB`/`HSI`/
  `CMYK`/`YCbCr` (modules #4–#8), this module introduces **no new
  colour-space tag**. Checked directly against the tarball source
  (`hip-1.5.6.0/src/Graphics/Image/ColorSpace/Complex.hs`): upstream instead
  re-exports GHC's `Data.Complex.Complex` and defines nine pixel-level
  operations (`(+:)`, `realPart`, `imagPart`, `mkPolar`, `cis`, `polar`,
  `magnitude`, `phase`, `conjugate`) that work over `Pixel cs (Complex e)`
  for **any** colour space `cs` that already has a `Pixel cs e` instance —
  i.e. `Pixel cs e` becomes complex-valued simply by instantiating its
  existing, `e`-polymorphic definition with `e := Complex e'`. There is no
  `ComplexPixel`/bespoke wrapper type upstream, and none is introduced here.

  ## `Data.Complex` reuse

  `Linen.Data.Complex` (`Linen/Data/Complex.lean`) already ports Haskell's
  `Data.Complex` shape as a plain structure `Complex α` with `re`/`im`
  fields, generic addition/subtraction/multiplication/negation, `conjugate`,
  and `magnitudeSquared`. This module reuses that structure and its
  arithmetic directly — no complex-number arithmetic is reinvented here; see
  below for the one deliberate, narrowly-scoped addition (`magnitude`/
  `phase`/`mkPolar`/`cis`, which need concrete floating-point `sqrt`/`atan2`/
  `cos`/`sin` that a type generic over an arbitrary `α` cannot provide, so
  they are *not* added to `Linen.Data.Complex` itself).

  ## Why complex-valued pixels need no new `Pixel`/`ColorSpace` instance

  `Linen.Graphics.Image.Interface.Elevator`'s own doc-comment (module #2)
  explicitly defers `Elevator (Complex e)` to this module:

  > `Elevator (Complex e)` discards the imaginary part and elevates the real
  > part alone. … this instance is postponed to module #9 … where
  > complex-valued pixels are actually introduced.

  That deferred instance is defined below. Once it exists, every colour
  space ported so far — `Y`/`RGB`/`HSI`/`CMYK`/`YCbCr` — already declares its
  `ColorSpace` instance generically over `[Elevator e]` (e.g. `instance
  [Elevator e] : ColorSpace RGB e (e × e × e)`), so Lean's instance search
  automatically resolves `ColorSpace RGB (Complex Float) _`, `Pixel Y (Complex
  UInt8) (PixelY (Complex UInt8))`, etc. with **no further code**: this is
  exactly the mechanism upstream's own `Pixel cs (Complex e)` genericity
  relies on (a data-family instance parameterised over `e`, here a Lean
  class instance parameterised over `e` the same way). This module therefore
  contributes only the one instance the existing colour spaces are missing
  (`Elevator (Complex e)`) plus the pixel-level complex-arithmetic functions
  below — no `ComplexPixel`/new tag/new `Pixel`/`ColorSpace` instance per
  colour space is needed or would be faithful to upstream.

  ## `Elevator (Complex e)`

  Ported exactly as `Interface/Elevator.lean`'s deferral describes: the
  imaginary part is discarded and the real part alone is elevated, via the
  underlying `Elevator e` instance. `fromFloat` recovers a zero imaginary
  part with `Elevator.fromFloat 0` (rather than requiring a separate
  `OfNat e 0`/`Zero e` constraint) since every `Elevator e` instance already
  provides `fromFloat : Float → e`, and `fromFloat 0` is `0` in every
  concrete instance (`UInt*`, `Int`, `Float32`, `Float`).

  ## `buildPx` — this port's stand-in for upstream's `Applicative (Pixel cs)`

  Upstream's `(+:)`, `realPart`, `imagPart`, `mkPolar`, `cis`, `polar`,
  `magnitude`, `phase`, `conjugate` are all one-line `liftA`/`liftA2`s through
  `Applicative (Pixel cs)` — generic over *every* colour space at once.
  `Linen.Graphics.Image.Interface`'s own doc-comment already explains why
  this port has no such generic `Functor`/`Applicative (Pixel cs)`: `Pixel`
  is a plain marker class (no data family to hang a higher-kinded instance
  off), and `liftPx`/`liftPx2`/`promote` are `ColorSpace` fields instead —
  but those fields only combine pixels that already share one component
  type, not (as `(+:)`/`realPart` need) two *different* component types
  (`e` and `Complex e`) of the same colour space.

  This module closes that gap with one small generic helper, `buildPx`,
  built directly from `ColorSpace`'s existing `channels`/`getPxC`/`setPxC`/
  `promote` (the same primitives `Interface.lean`'s `foldrPx`/`foldlPx`/
  `toListPx` are already built from): it constructs a pixel of colour space
  `cs` over component type `e` from a plain function `cs → e`, needing
  nothing colour-space-specific. Every pixel-level function below is
  `buildPx` applied to a `cs → e`/`cs → Complex e` function built from
  `getPxC` on its argument pixel(s) — a faithful, if differently-shaped,
  transcription of upstream's `liftA`/`liftA2`.

  ## Weaker constraints than upstream for `(+:)`/`realPart`/`imagPart`/`conjugate`

  Upstream types `realPart`/`imagPart`/`conjugate` (and `(+:)`'s codomain)
  with a blanket `RealFloat e` constraint, even though their bodies are pure
  field projections/negation with no actual floating-point operation. This
  port uses the strictly weaker constraint each operation actually needs:
  `[Elevator e]` (needed only because `ColorSpace`/`Pixel` themselves require
  it) for `(+:)`/`realPart`/`imagPart`, plus `[Neg e]` for `conjugate` (reusing
  `Linen.Data.Complex.conjugate`, which itself only needs `[Neg α]`). This
  lets these four operations apply to *any* `Elevator` component type, not
  just `Float`/`Float32` — a deliberate, documented relaxation, not a
  simplification of behaviour.

  ## `magnitude`/`phase`/`mkPolar`/`cis`/`polar` — specialised to `Float`/`Float32`

  These five upstream operations genuinely need `sqrt`/`atan2`/`cos`/`sin`,
  which no Lean type generic over an arbitrary `Elevator e` can supply (the
  same reason `Y`/`RGB`/`HSI`/`CMYK`/`YCbCr` never port a generic
  `Floating (Pixel cs e)`). Following `Interface/Elevator.lean`'s own
  dual-precision convention (`clamp01`/`clamp01F32`, `squashTo1`/
  `squashTo1F32`, `stretch`/`stretchF32`), this module defines each of these
  five, plus the small scalar helpers they're built from
  (`magnitudeOf`/`phaseOf`/`mkPolarOf`/`cisOf`), twice: once for `Complex
  Float` (double precision) and once for `Complex Float32` (single
  precision) — the same two floating component types `Interface/
  Elevator.lean` itself instantiates `Elevator` for. The scalar helpers are
  new, small additions local to this module (not added to the generic
  `Linen.Data.Complex`, which is deliberately unconstrained on its
  component type `α` and so cannot host a `sqrt`/`atan2`-based definition).
-/

import Linen.Graphics.Image.Interface
import Linen.Data.Complex

open Graphics.Image.Interface (Pixel ColorSpace AlphaSpace channels getPxC setPxC promote)
open Graphics.Image.Interface.Elevator (Elevator)
open Data (Complex)

namespace Graphics.Image.ColorSpace.Complex

-- ── `Elevator (Complex e)` — picking up `Interface.Elevator`'s deferral ──

/-- Discards the imaginary part and elevates the real part alone, via the
underlying `Elevator e` instance. Upstream's deferred `Elevator (Complex e)`
instance, ported here per `Interface/Elevator.lean`'s own doc-comment — see
the module doc-comment above for the full rationale. -/
instance [Elevator e] : Elevator (Complex e) where
  toWord8 z := Elevator.toWord8 z.re
  toWord16 z := Elevator.toWord16 z.re
  toWord32 z := Elevator.toWord32 z.re
  toWord64 z := Elevator.toWord64 z.re
  toFloat32 z := Elevator.toFloat32 z.re
  toFloat z := Elevator.toFloat z.re
  fromFloat x := ⟨Elevator.fromFloat x, Elevator.fromFloat 0⟩

-- ── `buildPx` — construct a pixel from a per-channel function ──

/-- Build a pixel of colour space `cs` over component type `e` from a
function computing each channel's value. This port's stand-in for upstream's
`Applicative (Pixel cs)`-based `liftA`/`liftA2` — see the module doc-comment
for why this is needed and how it is built from `ColorSpace`'s existing
`channels`/`setPxC`/`promote`.

`cs`/`e`/`px`/`Components` are repeated as explicit binders, exactly as
`Interface.lean`'s own `foldrPx`/`foldlPx`/`toListPx` do, since they are
`outParam`s that must be unified with whichever `Pixel`/`ColorSpace`
instance is in scope at each call site. -/
def buildPx {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] (f : cs → e) : px :=
  (channels (cs := cs) (e := e)).foldl (fun acc c => setPxC acc c (f c))
    (promote (cs := cs) (e := e) (Elevator.fromFloat 0))

-- ── Rectangular form: `(+:)`, `realPart`, `imagPart` ──

/-- Construct a complex pixel from two pixels representing the real and
imaginary parts, channel by channel. Upstream's `(+:)`.

```
#eval mkComplexPx (PixelY.mk 4) (PixelY.mk 7)
-- PixelY.mk ⟨4, 7⟩
```
-/
def mkComplexPx {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components]
    {pxC : Type} [Pixel cs (Complex e) pxC] [Elevator (Complex e)]
    {ComponentsC : Type} [ColorSpace cs (Complex e) ComponentsC]
    (p1 p2 : px) : pxC :=
  buildPx (cs := cs) (e := Complex e)
    (fun c => (⟨getPxC (cs := cs) (e := e) p1 c, getPxC (cs := cs) (e := e) p2 c⟩ : Complex e))

/-- Extract the real part of a complex pixel, channel by channel. Upstream's
`realPart`. -/
def realPartPx {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components]
    {pxC : Type} [Pixel cs (Complex e) pxC] [Elevator (Complex e)]
    {ComponentsC : Type} [ColorSpace cs (Complex e) ComponentsC]
    (pz : pxC) : px :=
  buildPx (cs := cs) (e := e) (fun c => (getPxC (cs := cs) (e := Complex e) pz c).re)

/-- Extract the imaginary part of a complex pixel, channel by channel.
Upstream's `imagPart`. -/
def imagPartPx {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components]
    {pxC : Type} [Pixel cs (Complex e) pxC] [Elevator (Complex e)]
    {ComponentsC : Type} [ColorSpace cs (Complex e) ComponentsC]
    (pz : pxC) : px :=
  buildPx (cs := cs) (e := e) (fun c => (getPxC (cs := cs) (e := Complex e) pz c).im)

-- ── Conjugate ──

/-- The conjugate of a complex pixel, channel by channel. Upstream's
`conjugate`. Reuses `Linen.Data.Complex.conjugate` directly. -/
def conjugatePx {cs e : Type} [Elevator e] [Neg e]
    {pxC : Type} [Pixel cs (Complex e) pxC] [Elevator (Complex e)]
    {ComponentsC : Type} [ColorSpace cs (Complex e) ComponentsC]
    (pz : pxC) : pxC :=
  buildPx (cs := cs) (e := Complex e)
    (fun c => Data.Complex.conjugate (getPxC (cs := cs) (e := Complex e) pz c))

-- ── Scalar polar-form helpers, specialised to `Float`/`Float32` ──
-- See the module doc-comment for why these are new, narrowly-scoped
-- additions local to this module rather than to `Linen.Data.Complex`.

/-- The nonnegative magnitude of a complex number (double precision). -/
def magnitudeOf (z : Complex Float) : Float :=
  Float.sqrt (Data.Complex.magnitudeSquared z)

/-- `Float32` counterpart of `magnitudeOf`. -/
def magnitudeOfF32 (z : Complex Float32) : Float32 :=
  Float32.sqrt (Data.Complex.magnitudeSquared z)

/-- The phase of a complex number, in the range `(-π, π]`; `0` if the
magnitude is `0` (double precision). -/
def phaseOf (z : Complex Float) : Float :=
  if z.re == 0 && z.im == 0 then 0 else Float.atan2 z.im z.re

/-- `Float32` counterpart of `phaseOf`. -/
def phaseOfF32 (z : Complex Float32) : Float32 :=
  if z.re == 0 && z.im == 0 then 0 else Float32.atan2 z.im z.re

/-- Form a complex number from polar components of magnitude and phase
(double precision). -/
def mkPolarOf (r θ : Float) : Complex Float :=
  ⟨r * Float.cos θ, r * Float.sin θ⟩

/-- `Float32` counterpart of `mkPolarOf`. -/
def mkPolarOfF32 (r θ : Float32) : Complex Float32 :=
  ⟨r * Float32.cos θ, r * Float32.sin θ⟩

/-- A complex number with magnitude `1` and phase `θ` (double precision). -/
def cisOf (θ : Float) : Complex Float :=
  ⟨Float.cos θ, Float.sin θ⟩

/-- `Float32` counterpart of `cisOf`. -/
def cisOfF32 (θ : Float32) : Complex Float32 :=
  ⟨Float32.cos θ, Float32.sin θ⟩

-- ── Polar form, lifted to pixels (double precision) ──

/-- The nonnegative magnitude of a complex pixel, channel by channel.
Upstream's `magnitude` (double precision). -/
def magnitudePx {cs : Type} {px : Type} [Pixel cs Float px] {Components : Type}
    [ColorSpace cs Float Components]
    {pxC : Type} [Pixel cs (Complex Float) pxC]
    {ComponentsC : Type} [ColorSpace cs (Complex Float) ComponentsC]
    (pz : pxC) : px :=
  buildPx (cs := cs) (e := Float)
    (fun c => magnitudeOf (getPxC (cs := cs) (e := Complex Float) pz c))

/-- The phase of a complex pixel, channel by channel. Upstream's `phase`
(double precision). -/
def phasePx {cs : Type} {px : Type} [Pixel cs Float px] {Components : Type}
    [ColorSpace cs Float Components]
    {pxC : Type} [Pixel cs (Complex Float) pxC]
    {ComponentsC : Type} [ColorSpace cs (Complex Float) ComponentsC]
    (pz : pxC) : px :=
  buildPx (cs := cs) (e := Float)
    (fun c => phaseOf (getPxC (cs := cs) (e := Complex Float) pz c))

/-- A complex pixel's `(magnitude, phase)` pair. Upstream's `polar` (double
precision). -/
def polarPx {cs : Type} {px : Type} [Pixel cs Float px] {Components : Type}
    [ColorSpace cs Float Components]
    {pxC : Type} [Pixel cs (Complex Float) pxC]
    {ComponentsC : Type} [ColorSpace cs (Complex Float) ComponentsC]
    (pz : pxC) : px × px :=
  (magnitudePx (cs := cs) pz, phasePx (cs := cs) pz)

/-- Form a complex pixel from polar components of magnitude and phase,
channel by channel. Upstream's `mkPolar` (double precision). -/
def mkPolarPx {cs : Type} {px : Type} [Pixel cs Float px] {Components : Type}
    [ColorSpace cs Float Components]
    {pxC : Type} [Pixel cs (Complex Float) pxC] [Elevator (Complex Float)]
    {ComponentsC : Type} [ColorSpace cs (Complex Float) ComponentsC]
    (pr pθ : px) : pxC :=
  buildPx (cs := cs) (e := Complex Float)
    (fun c => mkPolarOf (getPxC (cs := cs) (e := Float) pr c) (getPxC (cs := cs) (e := Float) pθ c))

/-- A complex pixel with magnitude `1` and phase `θ` (per channel), channel
by channel. Upstream's `cis` (double precision). -/
def cisPx {cs : Type} {px : Type} [Pixel cs Float px] {Components : Type}
    [ColorSpace cs Float Components]
    {pxC : Type} [Pixel cs (Complex Float) pxC] [Elevator (Complex Float)]
    {ComponentsC : Type} [ColorSpace cs (Complex Float) ComponentsC]
    (pθ : px) : pxC :=
  buildPx (cs := cs) (e := Complex Float) (fun c => cisOf (getPxC (cs := cs) (e := Float) pθ c))

-- ── Polar form, lifted to pixels (single precision) ──

/-- `Float32` counterpart of `magnitudePx`. -/
def magnitudePxF32 {cs : Type} {px : Type} [Pixel cs Float32 px] {Components : Type}
    [ColorSpace cs Float32 Components]
    {pxC : Type} [Pixel cs (Complex Float32) pxC]
    {ComponentsC : Type} [ColorSpace cs (Complex Float32) ComponentsC]
    (pz : pxC) : px :=
  buildPx (cs := cs) (e := Float32)
    (fun c => magnitudeOfF32 (getPxC (cs := cs) (e := Complex Float32) pz c))

/-- `Float32` counterpart of `phasePx`. -/
def phasePxF32 {cs : Type} {px : Type} [Pixel cs Float32 px] {Components : Type}
    [ColorSpace cs Float32 Components]
    {pxC : Type} [Pixel cs (Complex Float32) pxC]
    {ComponentsC : Type} [ColorSpace cs (Complex Float32) ComponentsC]
    (pz : pxC) : px :=
  buildPx (cs := cs) (e := Float32)
    (fun c => phaseOfF32 (getPxC (cs := cs) (e := Complex Float32) pz c))

/-- `Float32` counterpart of `polarPx`. -/
def polarPxF32 {cs : Type} {px : Type} [Pixel cs Float32 px] {Components : Type}
    [ColorSpace cs Float32 Components]
    {pxC : Type} [Pixel cs (Complex Float32) pxC]
    {ComponentsC : Type} [ColorSpace cs (Complex Float32) ComponentsC]
    (pz : pxC) : px × px :=
  (magnitudePxF32 (cs := cs) pz, phasePxF32 (cs := cs) pz)

/-- `Float32` counterpart of `mkPolarPx`. -/
def mkPolarPxF32 {cs : Type} {px : Type} [Pixel cs Float32 px] {Components : Type}
    [ColorSpace cs Float32 Components]
    {pxC : Type} [Pixel cs (Complex Float32) pxC] [Elevator (Complex Float32)]
    {ComponentsC : Type} [ColorSpace cs (Complex Float32) ComponentsC]
    (pr pθ : px) : pxC :=
  buildPx (cs := cs) (e := Complex Float32)
    (fun c =>
      mkPolarOfF32 (getPxC (cs := cs) (e := Float32) pr c) (getPxC (cs := cs) (e := Float32) pθ c))

/-- `Float32` counterpart of `cisPx`. -/
def cisPxF32 {cs : Type} {px : Type} [Pixel cs Float32 px] {Components : Type}
    [ColorSpace cs Float32 Components]
    {pxC : Type} [Pixel cs (Complex Float32) pxC] [Elevator (Complex Float32)]
    {ComponentsC : Type} [ColorSpace cs (Complex Float32) ComponentsC]
    (pθ : px) : pxC :=
  buildPx (cs := cs) (e := Complex Float32)
    (fun c => cisOfF32 (getPxC (cs := cs) (e := Float32) pθ c))

end Graphics.Image.ColorSpace.Complex
