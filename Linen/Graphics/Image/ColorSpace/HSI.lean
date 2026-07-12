/-
  Linen.Graphics.Image.ColorSpace.HSI — three-channel hue/saturation/intensity
  colour space, plus its alpha-carrying counterpart

  ## Haskell equivalent
  `Graphics.Image.ColorSpace.HSI` from https://hackage.haskell.org/package/hip
  (module #6 of the `hip` import plan, see `docs/imports/hip/dependencies.md`).
  Upstream's module defines two colour spaces together — `HSI` (plain
  hue/saturation/intensity) and `HSIA` (`HSI` with an alpha channel) — since
  `HSIA`'s `AlphaSpace` instance needs `HSI` as its `Opaque` counterpart; this
  port keeps both in one module for the same reason.

  This module follows exactly the pattern established by
  `Linen.Graphics.Image.ColorSpace.Y` (module #4) and
  `Linen.Graphics.Image.ColorSpace.RGB` (module #5) — see those modules'
  doc-comments for the detailed rationale behind every generic decision below;
  only points specific to `HSI`/`HSIA` are repeated here.

  ## `HSI`/`HSIA` as colour-space tags

  Upstream's `HSI`/`HSIA` are small enumerations (`data HSI = HueHSI | SatHSI
  | IntHSI`, `data HSIA = HueHSIA | SatHSIA | IntHSIA | AlphaHSIA`) used only
  as `ColorSpace` type-tags and as the argument to `getPxC`/`setPxC`/`mapPxC`.
  They are ported directly as inductive types with one constructor per
  channel (`HSI.hue`/`HSI.sat`/`HSI.int`, `HSIA.hue`/`HSIA.sat`/`HSIA.int`/
  `HSIA.alpha`), each with `deriving BEq, Repr, Inhabited` standing in for
  upstream's `Eq, Show` — `Enum, Bounded, Typeable` are simplified away
  exactly as in `Y`/`RGB`/`Interface`.

  ## `PixelHSI`/`PixelHSIA` as concrete pixel types

  Upstream's `Pixel HSI e`/`Pixel HSIA e` are data-family instances (`data
  instance Pixel HSI e = PixelHSI !e !e !e` / `data instance Pixel HSIA e =
  PixelHSIA !e !e !e !e`); as in `Y`/`RGB`, `Pixel` here is a plain marker
  class, so `PixelHSI`/`PixelHSIA` are declared directly as structures with
  three/four `e`-typed fields.

  ## Deferred/dropped upstream machinery

  Exactly the same deferrals as `Y`'s/`RGB`'s doc-comments describe, applied
  here: `Show (Pixel HSI e)`/`Show (Pixel HSIA e)`'s bespoke `<HSI:(h|s|i)>`-
  style rendering and `Storable (Pixel HSI e)`/`Storable (Pixel HSIA e)` are
  dropped (structural `Repr` from `deriving` covers `Show`'s role; `Storable`
  is GHC FFI machinery with no Lean counterpart); `Functor (Pixel HSI)`/
  `Applicative (Pixel HSI)`/`Foldable (Pixel HSI)` (and the `HSIA`
  counterparts) are redundant wrappers around `liftPx`/`liftPx2`/`promote`/
  `foldlPx`, which this port defines directly as `ColorSpace` fields instead;
  not ported.

  ## `Num`/`Fractional`-style arithmetic on `PixelHSI`

  As in `Y`/`RGB`, upstream's `instance ColorSpace cs e => Num (Pixel cs e)`
  (`Graphics/Image/Interface.hs`) is defined once, generically, for *every*
  colour space — including `HSI` — with no special-casing to exclude it
  despite `HSI`'s hue channel being an angle rather than an independent linear
  channel (component-wise `+` on two hues does not correspond to any
  meaningful physical operation, e.g. it does not "average" or "rotate" a
  hue the way one might expect; upstream simply inherits whatever
  `liftPx2 (+)` happens to produce). Being faithful to upstream's own choice
  not to withhold this instance for `HSI`, this port instantiates the same
  component-wise `Add`/`Sub`/`Mul`/`Div`/`Neg`/`OfNat` on `PixelHSI e` that
  `Y`/`RGB` already established, conditional on the matching instance for
  `e` — exactly the same mechanical derivation, carrying the same caveat
  upstream itself carries (arithmetic operates blindly on all three channels;
  it is the caller's responsibility to know whether that is meaningful for a
  given operation, same as upstream).

  `abs`/`signum`/`Floating` are not ported, for the same reason `Y`/`RGB`
  give (no generic Lean stdlib target for an arbitrary component type `e`).

  `PixelHSIA e`'s arithmetic is not ported, for the same reason
  `PixelYA e`/`PixelRGBA e`'s isn't: nothing in this port currently needs
  alpha-aware arithmetic. The same `liftPx2`/`liftPx`/`promote`-based pattern
  used for `PixelHSI` below would apply unchanged if a later module needs it.

  ## `RGB ↔ HSI` conversion — deferred to module #12, not ported here

  hip's actual `RGB → HSI` and `HSI → RGB` conversions are **not** defined in
  upstream's `Graphics/Image/ColorSpace/HSI.hs` at all (checked directly
  against the tarball source: that file contains only the tag
  inductives/`Pixel`/`ColorSpace`/`AlphaSpace` instances ported above, no
  trigonometry, no `ToRGB`/`ToHSI` instance). The conversion instead lives in
  `Graphics/Image/ColorSpace.hs` (module #12 in the plan,
  `Linen.Graphics.Image.ColorSpace`), which declares generic `ToY`/`ToRGB`/
  `ToHSI`/`ToCMYK`/`ToYCbCr` classes and gives a conversion instance from
  *every* colour space to *every other* — e.g. `instance Elevator e => ToRGB
  HSI e`, `instance Elevator e => ToHSI RGB e`, but also `instance Elevator e
  => ToHSI Y e`, `instance Elevator e => ToHSI YCbCr e` (routed through
  `RGB`), `instance Elevator e => ToHSI CMYK e` (likewise), and the symmetric
  `ToRGB`/`ToY`/`ToCMYK`/`ToYCbCr` instances for `HSI`/`HSIA`. This is exactly
  the generalized-across-many-colour-spaces case flagged as a reason *not* to
  invent an ad-hoc conversion here: module #3's `Interface.lean` deliberately
  declares no `ToXxx`-style conversion classes (its own doc-comment covers
  only `Pixel`/`ColorSpace`/`AlphaSpace`/indexing), and the topological plan
  already places the *whole* conversion matrix at module #12, after every
  individual colour space (`Y`, `RGB`, `HSI`, `CMYK`, `YCbCr`, `Complex`, `X`,
  `Binary`) it converts between has been ported. Porting just `RGB ↔ HSI`
  here, ahead of that, would both duplicate work module #12 must redo anyway
  and pre-empt the design of the `ToXxx` class hierarchy before the other
  colour spaces it must also cover (`CMYK`, `YCbCr`, …) exist.

  For module #12's future author, the exact formulas found in upstream's
  `Graphics/Image/ColorSpace.hs` (`hip-1.5.6.0`) are recorded here so no
  numerical detail is lost between now and then. Both operate on `Double`
  components (upstream promotes via `toDouble`/`Elevator` first) and treat hue
  as a *fraction* of a full turn, `h ∈ [0, 1)` representing an angle `h·2π`:

  `RGB → HSI` (upstream's `toPixelHSI (fmap toDouble -> PixelRGB r g b) =
  PixelHSI h s i`):
  ```
  x = (2r - g - b) / 2.449489742783178       -- 2.449489742783178 = √6
  y = (g - b) / 1.4142135623730951           -- 1.4142135623730951 = √2
  h' = atan2 y x
  h  = (if h' < 0 then h' + 2π else h') / (2π)
  i  = (r + g + b) / 3
  s  = if i == 0 then 0 else 1 - min r (min g b) / i
  ```

  `HSI → RGB` (upstream's `toPixelRGB (fmap toDouble -> PixelHSI h' s i) =
  getRGB (h'·2π)`, i.e. hue is first turned back into radians):
  ```
  is     = i * s
  second = i - is
  getFirst a b = i + is * cos a / cos b
  getThird v1 v2 = i + 2*is + v1 - v2
  getRGB h
    | h < 0        = error (out of range)
    | h < 2π/3     = let r = getFirst h (π/3 - h); b = second; g = getThird b r
    | h < 4π/3     = let g = getFirst (h - 2π/3) (h + π); r = second; b = getThird r g
    | h < 2π       = let b = getFirst (h - 4π/3) (2π - π/3 - h); g = second; r = getThird g b
    | otherwise    = error (out of range)
  ```
  (the three branches partition the hue circle into thirds, one per primary
  colour being the channel nearest its peak).
-/

import Linen.Graphics.Image.Interface

open Graphics.Image.Interface (Pixel ColorSpace AlphaSpace)
open Graphics.Image.Interface.Elevator (Elevator)

namespace Graphics.Image.ColorSpace.HSI

-- ── `HSI` — plain hue/saturation/intensity ──

/-- Hue/saturation/intensity colour space, upstream's `HSI` (`HueHSI`/
`SatHSI`/`IntHSI`, here `HSI.hue`/`HSI.sat`/`HSI.int`). -/
inductive HSI where
  /-- The hue channel. -/
  | hue
  /-- The saturation channel. -/
  | sat
  /-- The intensity channel. -/
  | int
deriving BEq, Repr, Inhabited

/-- An HSI pixel: three `e`-typed components. Upstream's `data instance Pixel
HSI e = PixelHSI !e !e !e`. -/
structure PixelHSI (e : Type) where
  /-- The hue component. -/
  h : e
  /-- The saturation component. -/
  s : e
  /-- The intensity component. -/
  i : e
deriving BEq, Repr, Inhabited

instance : Pixel HSI e (PixelHSI e) where

instance [Elevator e] : ColorSpace HSI e (e × e × e) where
  channels := [HSI.hue, HSI.sat, HSI.int]
  toComponents px := (px.h, px.s, px.i)
  fromComponents | (h, s, i) => ⟨h, s, i⟩
  promote x := ⟨x, x, x⟩
  getPxC px
    | .hue => px.h
    | .sat => px.s
    | .int => px.i
  setPxC px
    | .hue => fun h => ⟨h, px.s, px.i⟩
    | .sat => fun s => ⟨px.h, s, px.i⟩
    | .int => fun i => ⟨px.h, px.s, i⟩
  mapPxC f px := ⟨f .hue px.h, f .sat px.s, f .int px.i⟩
  liftPx f px := ⟨f px.h, f px.s, f px.i⟩
  liftPx2 f px1 px2 := ⟨f px1.h px2.h, f px1.s px2.s, f px1.i px2.i⟩
  foldlPx2 f z px1 px2 := f (f (f z px1.h px2.h) px1.s px2.s) px1.i px2.i

-- ── `HSIA` — hue/saturation/intensity with an alpha channel ──

/-- Hue/saturation/intensity-with-alpha colour space, upstream's `HSIA`
(`HueHSIA`/`SatHSIA`/`IntHSIA`/`AlphaHSIA`, here `HSIA.hue`/`HSIA.sat`/
`HSIA.int`/`HSIA.alpha`). -/
inductive HSIA where
  /-- The hue channel. -/
  | hue
  /-- The saturation channel. -/
  | sat
  /-- The intensity channel. -/
  | int
  /-- The alpha channel. -/
  | alpha
deriving BEq, Repr, Inhabited

/-- An HSIA pixel: four `e`-typed components. Upstream's `data instance Pixel
HSIA e = PixelHSIA !e !e !e !e`. -/
structure PixelHSIA (e : Type) where
  /-- The hue component. -/
  h : e
  /-- The saturation component. -/
  s : e
  /-- The intensity component. -/
  i : e
  /-- The alpha component. -/
  a : e
deriving BEq, Repr, Inhabited

instance : Pixel HSIA e (PixelHSIA e) where

instance [Elevator e] : ColorSpace HSIA e (e × e × e × e) where
  channels := [HSIA.hue, HSIA.sat, HSIA.int, HSIA.alpha]
  toComponents px := (px.h, px.s, px.i, px.a)
  fromComponents | (h, s, i, a) => ⟨h, s, i, a⟩
  promote x := ⟨x, x, x, x⟩
  getPxC px
    | .hue => px.h
    | .sat => px.s
    | .int => px.i
    | .alpha => px.a
  setPxC px
    | .hue => fun h => ⟨h, px.s, px.i, px.a⟩
    | .sat => fun s => ⟨px.h, s, px.i, px.a⟩
    | .int => fun i => ⟨px.h, px.s, i, px.a⟩
    | .alpha => fun a => ⟨px.h, px.s, px.i, a⟩
  mapPxC f px := ⟨f .hue px.h, f .sat px.s, f .int px.i, f .alpha px.a⟩
  liftPx f px := ⟨f px.h, f px.s, f px.i, f px.a⟩
  liftPx2 f px1 px2 :=
    ⟨f px1.h px2.h, f px1.s px2.s, f px1.i px2.i, f px1.a px2.a⟩
  foldlPx2 f z px1 px2 :=
    f (f (f (f z px1.h px2.h) px1.s px2.s) px1.i px2.i) px1.a px2.a

instance : AlphaSpace HSIA e HSI where
  getAlpha px := px.a
  addAlpha a px := ⟨px.h, px.s, px.i, a⟩
  dropAlpha px := ⟨px.h, px.s, px.i⟩

-- ── Component-wise arithmetic on `PixelHSI` ──

instance [Add e] : Add (PixelHSI e) where
  add px1 px2 := ⟨px1.h + px2.h, px1.s + px2.s, px1.i + px2.i⟩

instance [Sub e] : Sub (PixelHSI e) where
  sub px1 px2 := ⟨px1.h - px2.h, px1.s - px2.s, px1.i - px2.i⟩

instance [Mul e] : Mul (PixelHSI e) where
  mul px1 px2 := ⟨px1.h * px2.h, px1.s * px2.s, px1.i * px2.i⟩

instance [Div e] : Div (PixelHSI e) where
  div px1 px2 := ⟨px1.h / px2.h, px1.s / px2.s, px1.i / px2.i⟩

instance [Neg e] : Neg (PixelHSI e) where
  neg px := ⟨-px.h, -px.s, -px.i⟩

instance [OfNat e n] : OfNat (PixelHSI e) n where
  ofNat := ⟨OfNat.ofNat n, OfNat.ofNat n, OfNat.ofNat n⟩

end Graphics.Image.ColorSpace.HSI
