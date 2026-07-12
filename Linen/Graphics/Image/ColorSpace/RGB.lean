/-
  Linen.Graphics.Image.ColorSpace.RGB — three-channel red/green/blue colour
  space, plus its alpha-carrying counterpart

  ## Haskell equivalent
  `Graphics.Image.ColorSpace.RGB` from https://hackage.haskell.org/package/hip
  (module #5 of the `hip` import plan, see `docs/imports/hip/dependencies.md`).
  Upstream's module defines two colour spaces together — `RGB` (plain
  red/green/blue) and `RGBA` (`RGB` with an alpha channel) — since `RGBA`'s
  `AlphaSpace` instance needs `RGB` as its `Opaque` counterpart; this port
  keeps both in one module for the same reason.

  This module follows exactly the pattern established by
  `Linen.Graphics.Image.ColorSpace.Y` (module #4) — see that module's
  doc-comment for the detailed rationale behind every decision below; only
  points specific to `RGB`/`RGBA` are repeated here.

  ## `RGB`/`RGBA` as colour-space tags

  Upstream's `RGB`/`RGBA` are small enumerations (`data RGB = RedRGB |
  GreenRGB | BlueRGB`, `data RGBA = RedRGBA | GreenRGBA | BlueRGBA |
  AlphaRGBA`) used only as `ColorSpace` type-tags and as the argument to
  `getPxC`/`setPxC`/`mapPxC`. They are ported directly as inductive types
  with one constructor per channel, named to match upstream in spirit
  (`RGB.red`/`RGB.green`/`RGB.blue`, `RGBA.red`/`RGBA.green`/`RGBA.blue`/
  `RGBA.alpha`), each with `deriving BEq, Repr, Inhabited` standing in for
  upstream's `Eq, Show` — `Enum, Bounded, Typeable` are simplified away
  exactly as in `Y`/`Interface` (`Enum`/`Bounded` replaced by the explicit
  `channels` field, `Typeable` dropped).

  ## `PixelRGB`/`PixelRGBA` as concrete pixel types

  Upstream's `Pixel RGB e`/`Pixel RGBA e` are data-family instances
  (`data instance Pixel RGB e = PixelRGB !e !e !e` / `data instance Pixel
  RGBA e = PixelRGBA !e !e !e !e`); as in `Y`, `Pixel` here is a plain marker
  class, so `PixelRGB`/`PixelRGBA` are declared directly as structures with
  three/four `e`-typed fields.

  ## Deferred/dropped upstream machinery

  Exactly the same deferrals as `Y`'s doc-comment describes, applied here:

  * `Show (Pixel RGB e)`/`Show (Pixel RGBA e)`'s bespoke `<RGB:(r|g|b)>`-style
    rendering, and `Storable (Pixel RGB e)`/`Storable (Pixel RGBA e)` — the
    `deriving Repr` above already gives a structural `Repr` instance, and
    `Storable` is GHC FFI machinery with no Lean counterpart, dropped
    per the package-wide scope note in `dependencies.md`.
  * `Functor (Pixel RGB)`/`Applicative (Pixel RGB)`/`Foldable (Pixel RGB)`
    (and the `RGBA` counterparts) — redundant wrappers around `liftPx`/
    `liftPx2`/`promote`/`foldlPx`, which this port defines directly as
    `ColorSpace` fields instead of via these instances; not ported, same
    reasoning as `Y`.

  ## `Num`/`Fractional`-style arithmetic on `PixelRGB`

  As in `Y`, the generic `Num (Pixel cs e)`/`Fractional (Pixel cs e)`
  instance upstream derives from `liftPx`/`liftPx2`/`promote` is instantiated
  here component-wise via `Add`/`Sub`/`Mul`/`Div`/`Neg`/`OfNat`, conditional
  on the matching instance for `e`. `abs`/`signum`/`Floating` are not
  ported, for the same reason `Y` gives (no generic Lean stdlib target for an
  arbitrary component type `e`).

  `PixelRGBA e`'s arithmetic is not ported, for the same reason `PixelYA e`'s
  isn't: nothing in this port currently needs alpha-aware arithmetic, and
  plain component-wise `+`/`*` on an alpha channel is rarely the intended
  compositing semantics. The same `liftPx2`/`liftPx`/`promote`-based pattern
  used for `PixelRGB` below would apply unchanged if a later module needs it.
-/

import Linen.Graphics.Image.Interface

open Graphics.Image.Interface (Pixel ColorSpace AlphaSpace)
open Graphics.Image.Interface.Elevator (Elevator)

namespace Graphics.Image.ColorSpace.RGB

-- ── `RGB` — plain red/green/blue ──

/-- Red/green/blue colour space, upstream's `RGB` (`RedRGB`/`GreenRGB`/
`BlueRGB`, here `RGB.red`/`RGB.green`/`RGB.blue`). -/
inductive RGB where
  /-- The red channel. -/
  | red
  /-- The green channel. -/
  | green
  /-- The blue channel. -/
  | blue
deriving BEq, Repr, Inhabited

/-- An RGB pixel: three `e`-typed components. Upstream's `data instance Pixel
RGB e = PixelRGB !e !e !e`. -/
structure PixelRGB (e : Type) where
  /-- The red component. -/
  r : e
  /-- The green component. -/
  g : e
  /-- The blue component. -/
  b : e
deriving BEq, Repr, Inhabited

instance : Pixel RGB e (PixelRGB e) where

instance [Elevator e] : ColorSpace RGB e (e × e × e) where
  channels := [RGB.red, RGB.green, RGB.blue]
  toComponents px := (px.r, px.g, px.b)
  fromComponents | (r, g, b) => ⟨r, g, b⟩
  promote x := ⟨x, x, x⟩
  getPxC px
    | .red => px.r
    | .green => px.g
    | .blue => px.b
  setPxC px
    | .red => fun r => ⟨r, px.g, px.b⟩
    | .green => fun g => ⟨px.r, g, px.b⟩
    | .blue => fun b => ⟨px.r, px.g, b⟩
  mapPxC f px := ⟨f .red px.r, f .green px.g, f .blue px.b⟩
  liftPx f px := ⟨f px.r, f px.g, f px.b⟩
  liftPx2 f px1 px2 := ⟨f px1.r px2.r, f px1.g px2.g, f px1.b px2.b⟩
  foldlPx2 f z px1 px2 := f (f (f z px1.r px2.r) px1.g px2.g) px1.b px2.b

-- ── `RGBA` — red/green/blue with an alpha channel ──

/-- Red/green/blue-with-alpha colour space, upstream's `RGBA` (`RedRGBA`/
`GreenRGBA`/`BlueRGBA`/`AlphaRGBA`, here `RGBA.red`/`RGBA.green`/
`RGBA.blue`/`RGBA.alpha`). -/
inductive RGBA where
  /-- The red channel. -/
  | red
  /-- The green channel. -/
  | green
  /-- The blue channel. -/
  | blue
  /-- The alpha channel. -/
  | alpha
deriving BEq, Repr, Inhabited

/-- An RGBA pixel: four `e`-typed components. Upstream's `data instance Pixel
RGBA e = PixelRGBA !e !e !e !e`. -/
structure PixelRGBA (e : Type) where
  /-- The red component. -/
  r : e
  /-- The green component. -/
  g : e
  /-- The blue component. -/
  b : e
  /-- The alpha component. -/
  a : e
deriving BEq, Repr, Inhabited

instance : Pixel RGBA e (PixelRGBA e) where

instance [Elevator e] : ColorSpace RGBA e (e × e × e × e) where
  channels := [RGBA.red, RGBA.green, RGBA.blue, RGBA.alpha]
  toComponents px := (px.r, px.g, px.b, px.a)
  fromComponents | (r, g, b, a) => ⟨r, g, b, a⟩
  promote x := ⟨x, x, x, x⟩
  getPxC px
    | .red => px.r
    | .green => px.g
    | .blue => px.b
    | .alpha => px.a
  setPxC px
    | .red => fun r => ⟨r, px.g, px.b, px.a⟩
    | .green => fun g => ⟨px.r, g, px.b, px.a⟩
    | .blue => fun b => ⟨px.r, px.g, b, px.a⟩
    | .alpha => fun a => ⟨px.r, px.g, px.b, a⟩
  mapPxC f px := ⟨f .red px.r, f .green px.g, f .blue px.b, f .alpha px.a⟩
  liftPx f px := ⟨f px.r, f px.g, f px.b, f px.a⟩
  liftPx2 f px1 px2 :=
    ⟨f px1.r px2.r, f px1.g px2.g, f px1.b px2.b, f px1.a px2.a⟩
  foldlPx2 f z px1 px2 :=
    f (f (f (f z px1.r px2.r) px1.g px2.g) px1.b px2.b) px1.a px2.a

instance : AlphaSpace RGBA e RGB where
  getAlpha px := px.a
  addAlpha a px := ⟨px.r, px.g, px.b, a⟩
  dropAlpha px := ⟨px.r, px.g, px.b⟩

-- ── Component-wise arithmetic on `PixelRGB` ──

instance [Add e] : Add (PixelRGB e) where
  add px1 px2 := ⟨px1.r + px2.r, px1.g + px2.g, px1.b + px2.b⟩

instance [Sub e] : Sub (PixelRGB e) where
  sub px1 px2 := ⟨px1.r - px2.r, px1.g - px2.g, px1.b - px2.b⟩

instance [Mul e] : Mul (PixelRGB e) where
  mul px1 px2 := ⟨px1.r * px2.r, px1.g * px2.g, px1.b * px2.b⟩

instance [Div e] : Div (PixelRGB e) where
  div px1 px2 := ⟨px1.r / px2.r, px1.g / px2.g, px1.b / px2.b⟩

instance [Neg e] : Neg (PixelRGB e) where
  neg px := ⟨-px.r, -px.g, -px.b⟩

instance [OfNat e n] : OfNat (PixelRGB e) n where
  ofNat := ⟨OfNat.ofNat n, OfNat.ofNat n, OfNat.ofNat n⟩

end Graphics.Image.ColorSpace.RGB
