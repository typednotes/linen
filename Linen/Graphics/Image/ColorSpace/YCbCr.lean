/-
  Linen.Graphics.Image.ColorSpace.YCbCr — three-channel luma/blue-difference-
  chroma/red-difference-chroma colour space, plus its alpha-carrying
  counterpart

  ## Haskell equivalent
  `Graphics.Image.ColorSpace.YCbCr` from https://hackage.haskell.org/package/hip
  (module #8 of the `hip` import plan, see `docs/imports/hip/dependencies.md`).
  Upstream's module defines two colour spaces together — `YCbCr` (luma,
  blue-difference chroma, red-difference chroma) and `YCbCrA` (`YCbCr` with an
  alpha channel) — since `YCbCrA`'s `AlphaSpace` instance needs `YCbCr` as its
  `Opaque` counterpart; this port keeps both in one module for the same
  reason, exactly mirroring the `Y`/`YA`, `RGB`/`RGBA`, `HSI`/`HSIA`,
  `CMYK`/`CMYKA` pattern already established by modules #4–#7. Confirmed
  directly against the tarball source (`hip-1.5.6.0/src/Graphics/Image/
  ColorSpace/YCbCr.hs`): `YCbCrA` does exist as a genuine fourth-channel
  alpha variant on top of the already-3-channel `YCbCr`.

  This module follows exactly the pattern established by
  `Linen.Graphics.Image.ColorSpace.Y` (module #4), `RGB` (module #5), `HSI`
  (module #6), and `CMYK` (module #7) — see those modules' doc-comments for
  the detailed rationale behind every generic decision below; only points
  specific to `YCbCr`/`YCbCrA` are repeated here.

  ## `PixelYCbCr8` (`Linen.Codec.Picture.Types`) is *not* reused here

  This codebase already has a `PixelYCbCr8` structure, ported from
  `JuicyPixels`' `Codec.Picture.Types` as part of the earlier `JuicyPixels`
  import (see `Linen/Codec/Picture/Types.lean`). Per `AGENTS.md`'s
  Hackage-import precedence rule and the reuse-check convention already
  applied to `colour` in `dependencies.md`, this reuse was checked and
  rejected, for the same reason `dependencies.md`'s own precedence-check
  section gives for `colour`/`JuicyPixels` generally:

  * `PixelYCbCr8` is a **fixed, 8-bit-per-component** structure (`y cb cr :
    Pixel8`, i.e. `UInt8`) with a `Pixel PixelYCbCr8 Pixel8` instance tied to
    `Linen.Codec.Picture.Types`'s own `ColorSpaceConvertible`/`ColorPlane`
    class hierarchy — it exists purely so `JuicyPixels`' concrete PNG/JPEG
    codec machinery has one committed-to component width to decode/encode
    against.
  * hip's `YCbCr`/`Pixel YCbCr e` (this module) is generic over **any**
    `Elevator e` (so `Double`, `Float`, `UInt16`, `UInt8`, …), tied instead to
    `Graphics.Image.Interface`'s `Pixel`/`ColorSpace`/`AlphaSpace` classes —
    the same component-type-polymorphic abstraction `Y`/`RGB`/`HSI`/`CMYK`
    already established, deliberately decoupled from any one file format's
    fixed bit width.

  These are genuinely distinct abstractions serving different layers (a
  fixed-width codec pixel vs. a precision-polymorphic image-processing
  pixel), exactly as `dependencies.md`'s precedence-check note already found
  for `RGB`/`CMYK`/etc. against their `JuicyPixels` counterparts — so this is
  new work, not a reuse, consistent with every prior colour-space module in
  this plan. (`Linen.Codec.Picture.Types.PixelYCbCr8` does, incidentally,
  already carry a concrete `RGB ↔ YCbCr` conversion via
  `ColorSpaceConvertible` — using the same JPEG/full-range formula recorded
  below — but that instance is specific to the fixed-8-bit codec pixel type
  and not reusable for the `Elevator`-polymorphic `PixelYCbCr e` here.)

  ## `YCbCr`/`YCbCrA` as colour-space tags

  Upstream's `YCbCr`/`YCbCrA` are small enumerations (`data YCbCr = LumaYCbCr
  | CBlueYCbCr | CRedYCbCr`, `data YCbCrA = LumaYCbCrA | CBlueYCbCrA |
  CRedYCbCrA | AlphaYCbCrA`) used only as `ColorSpace` type-tags and as the
  argument to `getPxC`/`setPxC`/`mapPxC`. They are ported directly as
  inductive types with one constructor per channel (`YCbCr.luma`/`.cb`/`.cr`,
  `YCbCrA.luma`/`.cb`/`.cr`/`.alpha`), each with `deriving BEq, Repr,
  Inhabited` standing in for upstream's `Eq, Show` — `Enum, Bounded, Typeable`
  are simplified away exactly as in `Y`/`RGB`/`HSI`/`CMYK`. Upstream's
  `LumaYCbCr`/`CBlueYCbCr`/`CRedYCbCr` constructor names are spelled out as
  `.luma`/`.cb`/`.cr` here for readability, matching upstream's own
  doc-comments (`-- ^ Luma component`, `-- ^ Blue difference chroma
  component`, `-- ^ Red difference chroma component`).

  ## `PixelYCbCr`/`PixelYCbCrA` as concrete pixel types

  Upstream's `Pixel YCbCr e`/`Pixel YCbCrA e` are data-family instances (`data
  instance Pixel YCbCr e = PixelYCbCr !e !e !e` / `data instance Pixel YCbCrA
  e = PixelYCbCrA !e !e !e !e`); as in `Y`/`RGB`/`HSI`/`CMYK`, `Pixel` here is
  a plain marker class, so `PixelYCbCr`/`PixelYCbCrA` are declared directly
  as structures with three/four `e`-typed fields.

  ## Deferred/dropped upstream machinery

  Exactly the same deferrals as `Y`'s/`RGB`'s/`HSI`'s/`CMYK`'s doc-comments
  describe, applied here: `Show (Pixel YCbCr e)`/`Show (Pixel YCbCrA e)`'s
  bespoke `<YCbCr:(y|b|r)>`-style rendering and `Storable (Pixel YCbCr e)`/
  `Storable (Pixel YCbCrA e)` are dropped (structural `Repr` from `deriving`
  covers `Show`'s role; `Storable` is GHC FFI machinery with no Lean
  counterpart); `Functor (Pixel YCbCr)`/`Applicative (Pixel YCbCr)`/
  `Foldable (Pixel YCbCr)` (and the `YCbCrA` counterparts) are redundant
  wrappers around `liftPx`/`liftPx2`/`promote`/`foldlPx`, which this port
  defines directly as `ColorSpace` fields instead; not ported.

  ## `Num`/`Fractional`-style arithmetic on `PixelYCbCr`

  As in `Y`/`RGB`/`HSI`/`CMYK`, upstream's generic `instance ColorSpace cs e
  => Num (Pixel cs e)` (`Graphics/Image/Interface.hs`) applies uniformly to
  *every* colour space, `YCbCr` included, with no special-casing. This port
  instantiates the same component-wise `Add`/`Sub`/`Mul`/`Div`/`Neg`/`OfNat`
  on `PixelYCbCr e` that `Y`/`RGB`/`HSI`/`CMYK` already established,
  conditional on the matching instance for `e` — the same mechanical
  derivation, carrying the same caveat upstream itself carries (arithmetic
  operates blindly on all three channels; it is the caller's responsibility
  to know whether that is meaningful for a given operation, same as
  upstream).

  `abs`/`signum`/`Floating` are not ported, for the same reason `Y`/`RGB`/
  `HSI`/`CMYK` give (no generic Lean stdlib target for an arbitrary component
  type `e`).

  `PixelYCbCrA e`'s arithmetic is not ported, for the same reason
  `PixelYA e`/`PixelRGBA e`/`PixelHSIA e`/`PixelCMYKA e`'s isn't: nothing in
  this port currently needs alpha-aware arithmetic. The same
  `liftPx2`/`liftPx`/`promote`-based pattern used for `PixelYCbCr` below
  would apply unchanged if a later module needs it.

  ## `RGB ↔ YCbCr` conversion — deferred to module #12, not ported here

  hip's actual `RGB → YCbCr` and `YCbCr → RGB` conversions are **not**
  defined in upstream's `Graphics/Image/ColorSpace/YCbCr.hs` at all (checked
  directly against the tarball source: that file contains only the tag
  inductives/`Pixel`/`ColorSpace`/`AlphaSpace` instances ported above, no
  colour-mixing arithmetic). Exactly as with `RGB ↔ HSI`/`RGB ↔ CMYK` (see
  `HSI.lean`/`CMYK.lean`'s doc-comments for the full precedent and
  reasoning), the conversion instead lives in `Graphics/Image/ColorSpace.hs`
  (module #12 in the plan, `Linen.Graphics.Image.ColorSpace`), as `ToRGB
  YCbCr`/`ToYCbCr RGB` instances among the full
  `ToY`/`ToRGB`/`ToHSI`/`ToCMYK`/`ToYCbCr` conversion matrix — every other
  colour space's `ToYCbCr`/`ToRGB` instance for `YCbCr` simply routes through
  `RGB` (e.g. `ToYCbCr HSI e` is `toPixelYCbCr . toPixelRGB`, `ToHSI YCbCr e`
  is `toPixelHSI . toPixelRGB`), so `RGB ↔ YCbCr` is the one genuinely new
  formula module #12 needs here, exactly analogous to how `RGB ↔ HSI`/`RGB ↔
  CMYK` were the one new formula in each of those modules.

  For module #12's future author, the exact formulas found in upstream's
  `Graphics/Image/ColorSpace.hs` (`hip-1.5.6.0`) are recorded here so no
  numerical detail is lost between now and then. This is the standard
  full-range (JPEG-style, not studio/limited-range broadcast) BT.601 luma/
  chroma transform. Both operate on `Double` components (upstream promotes
  via `toDouble`/`Elevator` first, and clamps every result to `[0, 1]` via
  `clamp01`):

  `RGB → YCbCr` (upstream's `toPixelYCbCr (fmap toDouble -> PixelRGB r g b) =
  PixelYCbCr y cb cr`):
  ```
  y  = clamp01 (        0.299   * r +   0.587   * g +   0.114   * b)
  cb = clamp01 (0.5 - 0.168736 * r - 0.331264 * g +   0.5     * b)
  cr = clamp01 (0.5 +   0.5     * r - 0.418688 * g - 0.081312 * b)
  ```

  `YCbCr → RGB` (upstream's `toPixelRGB (fmap toDouble -> PixelYCbCr y cb cr)
  = PixelRGB r g b`):
  ```
  r = clamp01 (y                        +   1.402  * (cr - 0.5))
  g = clamp01 (y - 0.34414 * (cb - 0.5) - 0.71414 * (cr - 0.5))
  b = clamp01 (y +   1.772  * (cb - 0.5))
  ```
-/

import Linen.Graphics.Image.Interface

open Graphics.Image.Interface (Pixel ColorSpace AlphaSpace)
open Graphics.Image.Interface.Elevator (Elevator)

namespace Graphics.Image.ColorSpace.YCbCr

-- ── `YCbCr` — plain luma/blue-difference-chroma/red-difference-chroma ──

/-- Luma/blue-difference-chroma/red-difference-chroma colour space, upstream's
`YCbCr` (`LumaYCbCr`/`CBlueYCbCr`/`CRedYCbCr`, here `YCbCr.luma`/`.cb`/`.cr`).
-/
inductive YCbCr where
  /-- The luma component (commonly denoted `Y'`). -/
  | luma
  /-- The blue-difference chroma component. -/
  | cb
  /-- The red-difference chroma component. -/
  | cr
deriving BEq, Repr, Inhabited

/-- A YCbCr pixel: three `e`-typed components. Upstream's `data instance
Pixel YCbCr e = PixelYCbCr !e !e !e`. -/
structure PixelYCbCr (e : Type) where
  /-- The luma component. -/
  y : e
  /-- The blue-difference chroma component. -/
  cb : e
  /-- The red-difference chroma component. -/
  cr : e
deriving BEq, Repr, Inhabited

instance : Pixel YCbCr e (PixelYCbCr e) where

instance [Elevator e] : ColorSpace YCbCr e (e × e × e) where
  channels := [YCbCr.luma, YCbCr.cb, YCbCr.cr]
  toComponents px := (px.y, px.cb, px.cr)
  fromComponents | (y, cb, cr) => ⟨y, cb, cr⟩
  promote x := ⟨x, x, x⟩
  getPxC px
    | .luma => px.y
    | .cb => px.cb
    | .cr => px.cr
  setPxC px
    | .luma => fun y => ⟨y, px.cb, px.cr⟩
    | .cb => fun cb => ⟨px.y, cb, px.cr⟩
    | .cr => fun cr => ⟨px.y, px.cb, cr⟩
  mapPxC f px := ⟨f .luma px.y, f .cb px.cb, f .cr px.cr⟩
  liftPx f px := ⟨f px.y, f px.cb, f px.cr⟩
  liftPx2 f px1 px2 := ⟨f px1.y px2.y, f px1.cb px2.cb, f px1.cr px2.cr⟩
  foldlPx2 f z px1 px2 := f (f (f z px1.y px2.y) px1.cb px2.cb) px1.cr px2.cr

-- ── `YCbCrA` — luma/blue-difference-chroma/red-difference-chroma with alpha ──

/-- Luma/blue-difference-chroma/red-difference-chroma-with-alpha colour
space, upstream's `YCbCrA` (`LumaYCbCrA`/`CBlueYCbCrA`/`CRedYCbCrA`/
`AlphaYCbCrA`, here `YCbCrA.luma`/`.cb`/`.cr`/`.alpha`). -/
inductive YCbCrA where
  /-- The luma component. -/
  | luma
  /-- The blue-difference chroma component. -/
  | cb
  /-- The red-difference chroma component. -/
  | cr
  /-- The alpha component. -/
  | alpha
deriving BEq, Repr, Inhabited

/-- A YCbCrA pixel: four `e`-typed components. Upstream's `data instance
Pixel YCbCrA e = PixelYCbCrA !e !e !e !e`. -/
structure PixelYCbCrA (e : Type) where
  /-- The luma component. -/
  y : e
  /-- The blue-difference chroma component. -/
  cb : e
  /-- The red-difference chroma component. -/
  cr : e
  /-- The alpha component. -/
  a : e
deriving BEq, Repr, Inhabited

instance : Pixel YCbCrA e (PixelYCbCrA e) where

instance [Elevator e] : ColorSpace YCbCrA e (e × e × e × e) where
  channels := [YCbCrA.luma, YCbCrA.cb, YCbCrA.cr, YCbCrA.alpha]
  toComponents px := (px.y, px.cb, px.cr, px.a)
  fromComponents | (y, cb, cr, a) => ⟨y, cb, cr, a⟩
  promote x := ⟨x, x, x, x⟩
  getPxC px
    | .luma => px.y
    | .cb => px.cb
    | .cr => px.cr
    | .alpha => px.a
  setPxC px
    | .luma => fun y => ⟨y, px.cb, px.cr, px.a⟩
    | .cb => fun cb => ⟨px.y, cb, px.cr, px.a⟩
    | .cr => fun cr => ⟨px.y, px.cb, cr, px.a⟩
    | .alpha => fun a => ⟨px.y, px.cb, px.cr, a⟩
  mapPxC f px := ⟨f .luma px.y, f .cb px.cb, f .cr px.cr, f .alpha px.a⟩
  liftPx f px := ⟨f px.y, f px.cb, f px.cr, f px.a⟩
  liftPx2 f px1 px2 :=
    ⟨f px1.y px2.y, f px1.cb px2.cb, f px1.cr px2.cr, f px1.a px2.a⟩
  foldlPx2 f z px1 px2 :=
    f (f (f (f z px1.y px2.y) px1.cb px2.cb) px1.cr px2.cr) px1.a px2.a

instance : AlphaSpace YCbCrA e YCbCr where
  getAlpha px := px.a
  addAlpha a px := ⟨px.y, px.cb, px.cr, a⟩
  dropAlpha px := ⟨px.y, px.cb, px.cr⟩

-- ── Component-wise arithmetic on `PixelYCbCr` ──

instance [Add e] : Add (PixelYCbCr e) where
  add px1 px2 := ⟨px1.y + px2.y, px1.cb + px2.cb, px1.cr + px2.cr⟩

instance [Sub e] : Sub (PixelYCbCr e) where
  sub px1 px2 := ⟨px1.y - px2.y, px1.cb - px2.cb, px1.cr - px2.cr⟩

instance [Mul e] : Mul (PixelYCbCr e) where
  mul px1 px2 := ⟨px1.y * px2.y, px1.cb * px2.cb, px1.cr * px2.cr⟩

instance [Div e] : Div (PixelYCbCr e) where
  div px1 px2 := ⟨px1.y / px2.y, px1.cb / px2.cb, px1.cr / px2.cr⟩

instance [Neg e] : Neg (PixelYCbCr e) where
  neg px := ⟨-px.y, -px.cb, -px.cr⟩

instance [OfNat e n] : OfNat (PixelYCbCr e) n where
  ofNat := ⟨OfNat.ofNat n, OfNat.ofNat n, OfNat.ofNat n⟩

end Graphics.Image.ColorSpace.YCbCr
