/-
  Linen.Graphics.Image.ColorSpace.Y — single-channel luma colour space, plus
  its alpha-carrying counterpart

  ## Haskell equivalent
  `Graphics.Image.ColorSpace.Y` from https://hackage.haskell.org/package/hip
  (module #4 of the `hip` import plan, see `docs/imports/hip/dependencies.md`).
  Upstream's module defines two colour spaces together — `Y` (plain luma) and
  `YA` (luma with an alpha channel) — since `YA`'s `AlphaSpace` instance
  needs `Y` as its `Opaque` counterpart; this port keeps both in one module
  for the same reason.

  This is the very first concrete `Pixel`/`ColorSpace` instance in the port,
  establishing the pattern every later `ColorSpace.*` module (`RGB`, `HSI`,
  `CMYK`, `YCbCr`, …) follows. See `Linen.Graphics.Image.Interface` (module
  #3) for the class shapes instantiated below.

  ## `Y`/`YA` as colour-space tags

  Upstream's `Y`/`YA` are nullary/small enumerations (`data Y = LumaY`, `data
  YA = LumaYA | AlphaYA`) used only as `ColorSpace` type-tags and as the
  argument to `getPxC`/`setPxC`/`mapPxC`. They are ported directly as
  inductive types with one constructor per channel, named to match upstream
  (`Y.luma`, `YA.luma`, `YA.alpha`), each with `deriving BEq, Repr, Inhabited`
  standing in for upstream's `Eq, Show` (its `Enum, Bounded, Typeable` are
  simplified away exactly as `Interface`'s own doc-comment describes: `Enum`/
  `Bounded` are replaced by the explicit `channels` field, `Typeable` has no
  Lean counterpart).

  ## `PixelY`/`PixelYA` as concrete pixel types

  Upstream's `Pixel Y e`/`Pixel YA e` are data-family instances (`newtype
  Pixel Y e = PixelY e` / `data Pixel YA e = PixelYA !e !e`); per
  `Interface`'s own doc-comment, `Pixel` here is a plain marker class relating
  a colour space and component type to an ordinary structure, so `PixelY`/
  `PixelYA` are declared directly as structures with one/two `e`-typed
  fields.

  ## Deferred/dropped upstream machinery

  * `Storable (Pixel Y e)`/`Storable (Pixel YA e)` — dropped, per the
    package-wide scope note in `dependencies.md` (GHC FFI machinery with no
    Lean counterpart, same treatment as throughout `repa`/`netpbm`/
    `JuicyPixels`).
  * `Functor (Pixel Y)`/`Applicative (Pixel Y)`/`Foldable (Pixel Y)`/`Monad
    (Pixel Y)` (and the `YA` counterparts minus `Monad`, which upstream itself
    doesn't define for `YA`) — these exist upstream purely so `liftPx`/
    `liftPx2`/`promote`/`foldlPx` can be defined *as* `fmap`/`liftA2`/`pure`/
    `foldl'`. Since this port defines `liftPx`/`liftPx2`/`promote`/`foldlPx`
    directly as `ColorSpace` fields instead (no data family to hang a
    `Functor`/`Applicative`/`Foldable`/`Monad` instance off), these
    higher-kinded instances would be redundant wrappers around functionality
    `ColorSpace` already exposes, so they are not ported.

  ## `Num`/`Fractional`/`Floating`-style arithmetic on `PixelY`

  `Interface`'s own doc-comment defers the generic `Num (Pixel cs e)`/
  `Fractional (Pixel cs e)`/`Floating (Pixel cs e)` instances upstream derives
  from `liftPx`/`liftPx2`/`promote` (see `Graphics/Image/Interface.hs`,
  `instance ColorSpace cs e => Num (Pixel cs e)` and its `Fractional`/
  `Floating` neighbours) to "the concrete colour-space modules", i.e. here.
  Lean's stdlib splits `Num` into `Add`/`Sub`/`Mul`/`Neg`/`OfNat`, with `/`
  living on `Div` and no single `Floating` counterpart at all, so each is
  instantiated individually, component-wise via `liftPx2`/`liftPx`/`promote`,
  conditional on the matching instance for `e`:

  * `Add`, `Sub`, `Mul`, `Neg`, `Div` on `PixelY e` — direct counterparts of
    upstream's `(+)`/`(-)`/`(*)`/(`negate`, via `Neg`)/`(/)`.
  * `OfNat (PixelY e) n` (for every `e` with `OfNat e n`) — the counterpart of
    upstream's `fromInteger = promote . fromInteger`.
  * `abs`/`signum` (upstream's remaining two `Num` methods) and the whole of
    `Floating` (`pi`/`exp`/`log`/`sin`/`cos`/…) are **not** ported: Lean's
    stdlib has no polymorphic `Abs`/`Signum` or `Floating` typeclass spanning
    arbitrary numeric types the way Haskell's `Num`/`Floating` do, so there is
    no generic target to instantiate against for an arbitrary component type
    `e`. A future module that needs, say, `Float`-specific transcendental
    functions on a `PixelY Float` can call `Float`'s own functions directly
    through `liftPx`, without needing a typeclass instance for it.

  `PixelYA e`'s arithmetic is not ported for the same reason upstream itself
  doesn't derive it: `Num (Pixel cs e)` upstream is defined once, generically,
  for *every* `ColorSpace cs e` including `YA` — but nothing in this port's
  `Y`/`YA` test or later `ColorSpace.*` modules currently needs alpha-aware
  arithmetic, and an alpha channel participating in ordinary `+`/`*` the same
  way a colour channel does is rarely the intended semantics (compositing
  needs alpha-*weighted* arithmetic, not plain component-wise operations).
  Should a later module need it, the same `liftPx2`/`liftPx`/`promote`-based
  pattern used for `PixelY` below applies unchanged.
-/

import Linen.Graphics.Image.Interface

open Graphics.Image.Interface (Pixel ColorSpace AlphaSpace)
open Graphics.Image.Interface.Elevator (Elevator)

namespace Graphics.Image.ColorSpace.Y

-- ── `Y` — plain luma ──

/-- Luma (brightness) colour space, upstream's `Y` (single constructor
`LumaY`, here `Y.luma`). -/
inductive Y where
  | luma
deriving BEq, Repr, Inhabited

/-- A luma pixel: a single `e`-typed component. Upstream's `newtype instance
Pixel Y e = PixelY e`. -/
structure PixelY (e : Type) where
  /-- The luma component. -/
  y : e
deriving BEq, Repr, Inhabited

instance : Pixel Y e (PixelY e) where

instance [Elevator e] : ColorSpace Y e e where
  channels := [Y.luma]
  toComponents px := px.y
  fromComponents c := ⟨c⟩
  promote x := ⟨x⟩
  getPxC px _ := px.y
  setPxC _ _ y := ⟨y⟩
  mapPxC f px := ⟨f Y.luma px.y⟩
  liftPx f px := ⟨f px.y⟩
  liftPx2 f px1 px2 := ⟨f px1.y px2.y⟩
  foldlPx2 f z px1 px2 := f z px1.y px2.y

-- ── `YA` — luma with an alpha channel ──

/-- Luma-with-alpha colour space, upstream's `YA` (`LumaYA`/`AlphaYA`, here
`YA.luma`/`YA.alpha`). -/
inductive YA where
  /-- The luma channel. -/
  | luma
  /-- The alpha channel. -/
  | alpha
deriving BEq, Repr, Inhabited

/-- A luma-with-alpha pixel: two `e`-typed components. Upstream's `data
instance Pixel YA e = PixelYA !e !e`. -/
structure PixelYA (e : Type) where
  /-- The luma component. -/
  y : e
  /-- The alpha component. -/
  a : e
deriving BEq, Repr, Inhabited

instance : Pixel YA e (PixelYA e) where

instance [Elevator e] : ColorSpace YA e (e × e) where
  channels := [YA.luma, YA.alpha]
  toComponents px := (px.y, px.a)
  fromComponents | (y, a) => ⟨y, a⟩
  promote x := ⟨x, x⟩
  getPxC px
    | .luma => px.y
    | .alpha => px.a
  setPxC px
    | .luma => fun y => ⟨y, px.a⟩
    | .alpha => fun a => ⟨px.y, a⟩
  mapPxC f px := ⟨f .luma px.y, f .alpha px.a⟩
  liftPx f px := ⟨f px.y, f px.a⟩
  liftPx2 f px1 px2 := ⟨f px1.y px2.y, f px1.a px2.a⟩
  foldlPx2 f z px1 px2 := f (f z px1.y px2.y) px1.a px2.a

instance : AlphaSpace YA e Y where
  getAlpha px := px.a
  addAlpha a px := ⟨px.y, a⟩
  dropAlpha px := ⟨px.y⟩

-- ── Component-wise arithmetic on `PixelY` ──

instance [Add e] : Add (PixelY e) where
  add px1 px2 := ⟨px1.y + px2.y⟩

instance [Sub e] : Sub (PixelY e) where
  sub px1 px2 := ⟨px1.y - px2.y⟩

instance [Mul e] : Mul (PixelY e) where
  mul px1 px2 := ⟨px1.y * px2.y⟩

instance [Div e] : Div (PixelY e) where
  div px1 px2 := ⟨px1.y / px2.y⟩

instance [Neg e] : Neg (PixelY e) where
  neg px := ⟨-px.y⟩

instance [OfNat e n] : OfNat (PixelY e) n where
  ofNat := ⟨OfNat.ofNat n⟩

end Graphics.Image.ColorSpace.Y
