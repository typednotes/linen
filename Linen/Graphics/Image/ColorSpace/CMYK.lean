/-
  Linen.Graphics.Image.ColorSpace.CMYK — four-channel cyan/magenta/yellow/key
  (black) colour space, plus its alpha-carrying counterpart

  ## Haskell equivalent
  `Graphics.Image.ColorSpace.CMYK` from https://hackage.haskell.org/package/hip
  (module #7 of the `hip` import plan, see `docs/imports/hip/dependencies.md`).
  Upstream's module defines two colour spaces together — `CMYK` (plain
  cyan/magenta/yellow/key) and `CMYKA` (`CMYK` with an alpha channel) — since
  `CMYKA`'s `AlphaSpace` instance needs `CMYK` as its `Opaque` counterpart;
  this port keeps both in one module for the same reason. Confirmed directly
  against the tarball source (`hip-1.5.6.0/src/Graphics/Image/ColorSpace/
  CMYK.hs`): `CMYKA` does exist as a genuine fifth-channel alpha variant on
  top of the already-4-channel `CMYK`, exactly mirroring the `Y`/`YA`,
  `RGB`/`RGBA`, `HSI`/`HSIA` pattern already established by modules #4–#6.

  This module follows exactly the pattern established by
  `Linen.Graphics.Image.ColorSpace.Y` (module #4), `RGB` (module #5), and
  `HSI` (module #6) — see those modules' doc-comments for the detailed
  rationale behind every generic decision below; only points specific to
  `CMYK`/`CMYKA` are repeated here.

  ## `CMYK`/`CMYKA` as colour-space tags

  Upstream's `CMYK`/`CMYKA` are small enumerations (`data CMYK = CyanCMYK |
  MagCMYK | YelCMYK | KeyCMYK`, `data CMYKA = CyanCMYKA | MagCMYKA | YelCMYKA
  | KeyCMYKA | AlphaCMYKA`) used only as `ColorSpace` type-tags and as the
  argument to `getPxC`/`setPxC`/`mapPxC`. They are ported directly as
  inductive types with one constructor per channel (`CMYK.cyan`/`.magenta`/
  `.yellow`/`.black`, `CMYKA.cyan`/`.magenta`/`.yellow`/`.black`/`.alpha`),
  each with `deriving BEq, Repr, Inhabited` standing in for upstream's `Eq,
  Show` — `Enum, Bounded, Typeable` are simplified away exactly as in
  `Y`/`RGB`/`HSI`. Upstream's `KeyCMYK`/`KeyCMYKA` constructor name ("key" is
  the printing-industry term for black, hence "CMYK") is spelled out as
  `.black` here for readability, matching upstream's own doc-comment
  (`-- ^ Key (Black)`) rather than the abbreviated constructor name.

  ## `PixelCMYK`/`PixelCMYKA` as concrete pixel types

  Upstream's `Pixel CMYK e`/`Pixel CMYKA e` are data-family instances (`data
  instance Pixel CMYK e = PixelCMYK !e !e !e !e` / `data instance Pixel CMYKA
  e = PixelCMYKA !e !e !e !e !e`); as in `Y`/`RGB`/`HSI`, `Pixel` here is a
  plain marker class, so `PixelCMYK`/`PixelCMYKA` are declared directly as
  structures with four/five `e`-typed fields.

  ## Deferred/dropped upstream machinery

  Exactly the same deferrals as `Y`'s/`RGB`'s/`HSI`'s doc-comments describe,
  applied here: `Show (Pixel CMYK e)`/`Show (Pixel CMYKA e)`'s bespoke
  `<CMYK:(c|m|y|k)>`-style rendering and `Storable (Pixel CMYK e)`/
  `Storable (Pixel CMYKA e)` are dropped (structural `Repr` from `deriving`
  covers `Show`'s role; `Storable` is GHC FFI machinery with no Lean
  counterpart); `Functor (Pixel CMYK)`/`Applicative (Pixel CMYK)`/
  `Foldable (Pixel CMYK)` (and the `CMYKA` counterparts) are redundant
  wrappers around `liftPx`/`liftPx2`/`promote`/`foldlPx`, which this port
  defines directly as `ColorSpace` fields instead; not ported.

  ## `Num`/`Fractional`-style arithmetic on `PixelCMYK`

  As in `Y`/`RGB`/`HSI`, upstream's generic `instance ColorSpace cs e => Num
  (Pixel cs e)` (`Graphics/Image/Interface.hs`) applies uniformly to *every*
  colour space, `CMYK` included, with no special-casing. This port
  instantiates the same component-wise `Add`/`Sub`/`Mul`/`Div`/`Neg`/`OfNat`
  on `PixelCMYK e` that `Y`/`RGB`/`HSI` already established, conditional on
  the matching instance for `e` — the same mechanical derivation, carrying
  the same caveat upstream itself carries (arithmetic operates blindly on all
  four channels; it is the caller's responsibility to know whether that is
  meaningful for a given operation, same as upstream).

  `abs`/`signum`/`Floating` are not ported, for the same reason `Y`/`RGB`/
  `HSI` give (no generic Lean stdlib target for an arbitrary component type
  `e`).

  `PixelCMYKA e`'s arithmetic is not ported, for the same reason
  `PixelYA e`/`PixelRGBA e`/`PixelHSIA e`'s isn't: nothing in this port
  currently needs alpha-aware arithmetic. The same `liftPx2`/`liftPx`/
  `promote`-based pattern used for `PixelCMYK` below would apply unchanged if
  a later module needs it.

  ## `RGB ↔ CMYK` conversion — deferred to module #12, not ported here

  hip's actual `RGB → CMYK` and `CMYK → RGB` conversions are **not** defined
  in upstream's `Graphics/Image/ColorSpace/CMYK.hs` at all (checked directly
  against the tarball source: that file contains only the tag
  inductives/`Pixel`/`ColorSpace`/`AlphaSpace` instances ported above, no
  colour-mixing arithmetic). Exactly as with `RGB ↔ HSI` (see `HSI.lean`'s
  doc-comment for the full precedent and reasoning), the conversion instead
  lives in `Graphics/Image/ColorSpace.hs` (module #12 in the plan,
  `Linen.Graphics.Image.ColorSpace`), as `ToRGB CMYK`/`ToCMYK RGB` instances
  among the full `ToY`/`ToRGB`/`ToHSI`/`ToCMYK`/`ToYCbCr` conversion matrix —
  every other colour space's `ToCMYK`/`ToRGB` instance for `CMYK` simply
  routes through `RGB` (e.g. `ToCMYK HSI e` is `toPixelCMYK . toPixelRGB`),
  so `RGB ↔ CMYK` is the one genuinely new formula module #12 needs, exactly
  analogous to how `RGB ↔ HSI` was the one new formula there. Porting it
  here, ahead of module #12's design of the `ToXxx` class hierarchy, would
  duplicate work and pre-empt that design before the other colour spaces it
  must also cover exist.

  For module #12's future author, the exact formulas found in upstream's
  `Graphics/Image/ColorSpace.hs` (`hip-1.5.6.0`) are recorded here so no
  numerical detail is lost between now and then. Both operate on `Double`
  components (upstream promotes via `toDouble`/`Elevator` first):

  `RGB → CMYK` (upstream's `toPixelCMYK (fmap toDouble -> PixelRGB r g b) =
  PixelCMYK c m y k`):
  ```
  k = 1 - max r (max g b)
  c = (1 - r - k) / (1 - k)
  m = (1 - g - k) / (1 - k)
  y = (1 - b - k) / (1 - k)
  ```
  (division by zero when `k = 1`, i.e. pure black, is left exactly as
  upstream leaves it — no special-cased guard in the Haskell source either).

  `CMYK → RGB` (upstream's `toPixelRGB (fmap toDouble -> PixelCMYK c m y k) =
  PixelRGB r g b`):
  ```
  r = (1 - c) * (1 - k)
  g = (1 - m) * (1 - k)
  b = (1 - y) * (1 - k)
  ```
-/

import Linen.Graphics.Image.Interface

open Graphics.Image.Interface (Pixel ColorSpace AlphaSpace)
open Graphics.Image.Interface.Elevator (Elevator)

namespace Graphics.Image.ColorSpace.CMYK

-- ── `CMYK` — plain cyan/magenta/yellow/key(black) ──

/-- Cyan/magenta/yellow/key(black) colour space, upstream's `CMYK`
(`CyanCMYK`/`MagCMYK`/`YelCMYK`/`KeyCMYK`, here `CMYK.cyan`/`.magenta`/
`.yellow`/`.black`). -/
inductive CMYK where
  /-- The cyan channel. -/
  | cyan
  /-- The magenta channel. -/
  | magenta
  /-- The yellow channel. -/
  | yellow
  /-- The key (black) channel. -/
  | black
deriving BEq, Repr, Inhabited

/-- A CMYK pixel: four `e`-typed components. Upstream's `data instance Pixel
CMYK e = PixelCMYK !e !e !e !e`. -/
structure PixelCMYK (e : Type) where
  /-- The cyan component. -/
  c : e
  /-- The magenta component. -/
  m : e
  /-- The yellow component. -/
  y : e
  /-- The key (black) component. -/
  k : e
deriving BEq, Repr, Inhabited

instance : Pixel CMYK e (PixelCMYK e) where

instance [Elevator e] : ColorSpace CMYK e (e × e × e × e) where
  channels := [CMYK.cyan, CMYK.magenta, CMYK.yellow, CMYK.black]
  toComponents px := (px.c, px.m, px.y, px.k)
  fromComponents | (c, m, y, k) => ⟨c, m, y, k⟩
  promote x := ⟨x, x, x, x⟩
  getPxC px
    | .cyan => px.c
    | .magenta => px.m
    | .yellow => px.y
    | .black => px.k
  setPxC px
    | .cyan => fun c => ⟨c, px.m, px.y, px.k⟩
    | .magenta => fun m => ⟨px.c, m, px.y, px.k⟩
    | .yellow => fun y => ⟨px.c, px.m, y, px.k⟩
    | .black => fun k => ⟨px.c, px.m, px.y, k⟩
  mapPxC f px := ⟨f .cyan px.c, f .magenta px.m, f .yellow px.y, f .black px.k⟩
  liftPx f px := ⟨f px.c, f px.m, f px.y, f px.k⟩
  liftPx2 f px1 px2 := ⟨f px1.c px2.c, f px1.m px2.m, f px1.y px2.y, f px1.k px2.k⟩
  foldlPx2 f z px1 px2 :=
    f (f (f (f z px1.c px2.c) px1.m px2.m) px1.y px2.y) px1.k px2.k

-- ── `CMYKA` — cyan/magenta/yellow/key(black) with an alpha channel ──

/-- Cyan/magenta/yellow/key(black)-with-alpha colour space, upstream's
`CMYKA` (`CyanCMYKA`/`MagCMYKA`/`YelCMYKA`/`KeyCMYKA`/`AlphaCMYKA`, here
`CMYKA.cyan`/`.magenta`/`.yellow`/`.black`/`.alpha`). -/
inductive CMYKA where
  /-- The cyan channel. -/
  | cyan
  /-- The magenta channel. -/
  | magenta
  /-- The yellow channel. -/
  | yellow
  /-- The key (black) channel. -/
  | black
  /-- The alpha channel. -/
  | alpha
deriving BEq, Repr, Inhabited

/-- A CMYKA pixel: five `e`-typed components. Upstream's `data instance
Pixel CMYKA e = PixelCMYKA !e !e !e !e !e`. -/
structure PixelCMYKA (e : Type) where
  /-- The cyan component. -/
  c : e
  /-- The magenta component. -/
  m : e
  /-- The yellow component. -/
  y : e
  /-- The key (black) component. -/
  k : e
  /-- The alpha component. -/
  a : e
deriving BEq, Repr, Inhabited

instance : Pixel CMYKA e (PixelCMYKA e) where

instance [Elevator e] : ColorSpace CMYKA e (e × e × e × e × e) where
  channels := [CMYKA.cyan, CMYKA.magenta, CMYKA.yellow, CMYKA.black, CMYKA.alpha]
  toComponents px := (px.c, px.m, px.y, px.k, px.a)
  fromComponents | (c, m, y, k, a) => ⟨c, m, y, k, a⟩
  promote x := ⟨x, x, x, x, x⟩
  getPxC px
    | .cyan => px.c
    | .magenta => px.m
    | .yellow => px.y
    | .black => px.k
    | .alpha => px.a
  setPxC px
    | .cyan => fun c => ⟨c, px.m, px.y, px.k, px.a⟩
    | .magenta => fun m => ⟨px.c, m, px.y, px.k, px.a⟩
    | .yellow => fun y => ⟨px.c, px.m, y, px.k, px.a⟩
    | .black => fun k => ⟨px.c, px.m, px.y, k, px.a⟩
    | .alpha => fun a => ⟨px.c, px.m, px.y, px.k, a⟩
  mapPxC f px :=
    ⟨f .cyan px.c, f .magenta px.m, f .yellow px.y, f .black px.k, f .alpha px.a⟩
  liftPx f px := ⟨f px.c, f px.m, f px.y, f px.k, f px.a⟩
  liftPx2 f px1 px2 :=
    ⟨f px1.c px2.c, f px1.m px2.m, f px1.y px2.y, f px1.k px2.k, f px1.a px2.a⟩
  foldlPx2 f z px1 px2 :=
    f (f (f (f (f z px1.c px2.c) px1.m px2.m) px1.y px2.y) px1.k px2.k) px1.a px2.a

instance : AlphaSpace CMYKA e CMYK where
  getAlpha px := px.a
  addAlpha a px := ⟨px.c, px.m, px.y, px.k, a⟩
  dropAlpha px := ⟨px.c, px.m, px.y, px.k⟩

-- ── Component-wise arithmetic on `PixelCMYK` ──

instance [Add e] : Add (PixelCMYK e) where
  add px1 px2 := ⟨px1.c + px2.c, px1.m + px2.m, px1.y + px2.y, px1.k + px2.k⟩

instance [Sub e] : Sub (PixelCMYK e) where
  sub px1 px2 := ⟨px1.c - px2.c, px1.m - px2.m, px1.y - px2.y, px1.k - px2.k⟩

instance [Mul e] : Mul (PixelCMYK e) where
  mul px1 px2 := ⟨px1.c * px2.c, px1.m * px2.m, px1.y * px2.y, px1.k * px2.k⟩

instance [Div e] : Div (PixelCMYK e) where
  div px1 px2 := ⟨px1.c / px2.c, px1.m / px2.m, px1.y / px2.y, px1.k / px2.k⟩

instance [Neg e] : Neg (PixelCMYK e) where
  neg px := ⟨-px.c, -px.m, -px.y, -px.k⟩

instance [OfNat e n] : OfNat (PixelCMYK e) n where
  ofNat := ⟨OfNat.ofNat n, OfNat.ofNat n, OfNat.ofNat n, OfNat.ofNat n⟩

end Graphics.Image.ColorSpace.CMYK
