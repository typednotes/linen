/-
  Linen.Graphics.Image.ColorSpace — the colour-space facade: re-exports every
  individual colour space (#4–#11) and defines the cross-colour-space
  conversion matrix (`ToY`/`ToRGB`/`ToHSI`/`ToCMYK`/`ToYCbCr`, plus their
  alpha-carrying `…A` counterparts)

  ## Haskell equivalent
  `Graphics.Image.ColorSpace` from https://hackage.haskell.org/package/hip
  (module #12 of the `hip` import plan, see `docs/imports/hip/dependencies.md`),
  on `Interface` (#3) and every colour-space module `Y`/`RGB`/`HSI`/`CMYK`/
  `YCbCr`/`Complex`/`X`/`Binary` (#4–#11). Read directly against the tarball
  source (`hip-1.5.6.0/src/Graphics/Image/ColorSpace.hs`).

  ## Re-export strategy

  Unlike `Linen.Codec.Picture` (whose sub-modules all declare `namespace
  Codec.Picture` themselves, so a plain `import` already exposes every name
  under that one shared namespace with no further aliasing — see that
  module's own doc-comment), every colour-space module ported so far declares
  its *own* child namespace (`Graphics.Image.ColorSpace.Y`, `.RGB`, `.HSI`,
  …), one level below this file's own `Graphics.Image.ColorSpace` namespace.
  Lean's `import` is already transitive — a plain `import
  Linen.Graphics.Image.ColorSpace` (below) makes every declaration from every
  one of those sub-modules reachable at its fully-qualified name (e.g.
  `Graphics.Image.ColorSpace.RGB.RGB`, `Graphics.Image.ColorSpace.Binary.on`)
  with **no explicit re-export step needed**, unlike Haskell's per-module
  export lists which must name every re-exported identifier explicitly. This
  file's `open` statements below exist only to let *this file's own*
  definitions refer to `RGB`/`PixelRGB`/`on`/etc. unqualified; they are not
  themselves part of the port (a caller of this library is free to `open`
  the same namespaces, or not, exactly as with any other Lean import). This
  file's own new contribution — matching upstream's own file, whose export
  list is otherwise almost entirely re-exports plus the `ToXxx` classes below
  — is exactly the conversion-class hierarchy and the `toImageXxx`/
  binary-conversion/`eqTolPx` functions defined below.

  ## The `ToXxx` conversion matrix

  Upstream declares one class per target colour space — `ToY`, `ToYA`,
  `ToRGB`, `ToRGBA`, `ToHSI`, `ToHSIA`, `ToCMYK`, `ToCMYKA`, `ToYCbCr`,
  `ToYCbCrA` — each with one instance for every already-ported *source*
  colour space (`X`, `Y`, `YA`, `RGB`, `RGBA`, `HSI`, `HSIA`, `CMYK`, `CMYKA`,
  `YCbCr`, `YCbCrA`, as applicable — see below for which source spaces each
  target actually supports). No further colour space is referenced anywhere
  in upstream's own export list or instance set — in particular, upstream
  does **not** define `ToXYZ`/`ToLab`/any colour space beyond the eight
  already ported in #4–#11, so there is nothing left out of scope on that
  front.

  Every class carries `ColorSpace cs e` (ported here as the `[Elevator e]
  [ColorSpace cs e Components]` pair, per `Interface`'s own `outParam`
  threading convention already used throughout this port — `px`/`Components`
  repeated as explicit binders, exactly as `Interface.lean`'s own `foldrPx`/
  `foldlPx`/`toListPx` and `Complex.lean`'s `buildPx` already do) as its
  superclass constraint, and every conversion target is fixed to `Float`
  (Lean's 64-bit float, this port's `Double` counterpart — see
  `Interface.Elevator`'s own type-mapping table), exactly mirroring
  upstream's `Pixel cs e -> Pixel Y Double`/`Pixel RGB Double`/etc. shape:
  there is no dependence on a *target* precision parameter anywhere upstream,
  only the *source* `e` varies.

  ### Declaration order: `ToRGB`/`ToRGBA` first, unlike upstream's file order

  Upstream declares `ToY`/`ToYA` before `ToRGB`/`ToRGBA` in the source file,
  relying on GHC's whole-module mutual visibility (`ToY HSI e`'s body calls
  `toPixelRGB`, defined only later in the same file). Lean requires
  declare-before-use, so this port reorders the five families to
  `ToRGB`/`ToRGBA`, then `ToY`/`ToYA`, `ToHSI`/`ToHSIA`, `ToCMYK`/`ToCMYKA`,
  `ToYCbCr`/`ToYCbCrA` — the one order consistent with upstream's own
  dependency shape, since **every** non-`RGB` colour space's `ToY`/`ToHSI`/
  `ToCMYK`/`ToYCbCr` instance that isn't a direct formula routes through
  `toPixelRGB` (checked directly against every instance body below), while
  `ToRGB` itself never calls back into `ToY`/`ToHSI`/`ToCMYK`/`ToYCbCr`. This
  is a pure declaration-order adaptation with no change in behaviour or
  formula versus upstream.

  ### Formula provenance

  * `RGB ↔ HSI`, `RGB ↔ CMYK`, `RGB ↔ YCbCr` — the three formulas already
    transcribed verbatim into `HSI.lean`/`CMYK.lean`/`YCbCr.lean`'s own
    doc-comments (each deferred exactly here, to this module, by name) are
    used as-is; re-derived from those doc-comments, not re-fetched from
    upstream a second time, per the task's own instruction.
  * `RGB → Y` (`Y' = 0.299·R' + 0.587·G' + 0.114·B'`, the standard BT.601
    luma weights) is a **freshly-sourced** formula: neither `Y.lean` nor any
    other module's doc-comment records it (`Y.lean` needed no conversion
    formula of its own, unlike `HSI`/`CMYK`/`YCbCr`, since `Y` has no
    `RGB`-mixing arithmetic to defer), so it is taken directly from this
    module's own upstream source (`toPixelY`'s `RGB` instance in
    `ColorSpace.hs`, quoted in this doc-comment's instance-by-instance
    commentary below) rather than from any other module's doc-comment.
  * Every other instance (`ToY YA/RGBA/HSIA/CMYKA/YCbCrA/YCbCr(A)`, all of
    `ToYA`/`ToRGBA`/`ToHSIA`/`ToCMYKA`/`ToYCbCrA`, the `CMYK`/`HSI`/`YCbCr`
    "identity" instances on themselves, …) is either a direct field
    projection/pattern-match, an `addAlpha`/`dropAlpha` composition, or a
    composition through `toPixelRGB` — none of these introduce new numerical
    formulas beyond the three above, exactly mirroring upstream's own
    instance bodies.

  ### Restricted source spaces

  Upstream restricts several instances to a *specific* component type rather
  than any `Elevator e`, and this port carries that restriction forward
  faithfully rather than generalising it away:

  * `ToRGB X Bit`, `ToYA X Bit`, `ToRGBA X Bit` are defined **only** for
    `e := Bit`, not for a general `Elevator e` — checked directly against
    upstream (`instance ToRGB X Bit where …`, no `Elevator e =>` context, in
    contrast to `instance Elevator e => ToY X e where …` a few lines above in
    the same file). `ToHSI X e`, `ToCMYK X e`, `ToYCbCr X e` (and their `…A`
    counterparts) are **not defined at all** for any `e` — `X` converts only
    to `Y` (any `Elevator e`) and to `RGB`/`YA`/`RGBA` (`Bit` only); this is
    an upstream design choice (`X`'s own module doc-comment's claim that `X`
    "is not convertible to or from" is, per this file's own instance list,
    only true of `HSI`/`CMYK`/`YCbCr`, not of `Y`/`RGB`), not a simplification
    introduced by this port.

  ## `toWord8I`/`toWord16I`/`toWord32I`/`toFloatI`/`toDoubleI`/`toWord8Px` —
  ## out of scope, architectural limitation

  Upstream's six precision-changing *image*/*pixel* functions are generic
  over **any** colour space at once via a `Functor (Pixel cs)` constraint
  (`toWord8I = I.map (fmap toWord8)`, `toWord8Px = fmap toWord8`): they change
  a pixel's *component* type (`e → Word8`/`Float`/`Double`/…) while leaving
  its colour space `cs` fixed, for whichever `cs` the caller picks.
  `Interface.lean`'s own doc-comment already explains why this port has no
  such generic `Functor (Pixel cs)`: `Pixel cs e px` is a plain marker class
  relating a *fixed* triple `(cs, e, px)` (`px` an `outParam` of `cs`/`e`
  together), with no data-family/higher-kinded structure to hang a
  component-type-changing `Functor` instance off — the same limitation
  `Complex.lean`'s `buildPx` was introduced to work around for one single
  colour space's `e ↔ Complex e` pair, at the cost of one bespoke helper per
  color space using it. Reproducing that generically for *every* colour space
  at once — which is exactly what `toWord8I`/`toWord8Px` need — is out of
  scope for this module: it isn't a numerical formula this module could
  faithfully port, but a whole new generic-`Functor`-equivalent abstraction
  the rest of this port's architecture does not provide anywhere. Any future
  caller needing a component-precision change on a *known, concrete* colour
  space can already do so directly with that colour space's own `liftPx`
  applied to the target `Elevator`'s conversion method (e.g. `Interface.map
  (Interface.liftPx (cs := RGB) (e := e) Elevator.toWord8) img`), exactly the
  pattern `Complex.lean`'s per-colour-space helpers already use — there is
  simply no single generic definition of it spanning every colour space at
  once, matching the same limitation already documented for `Interface.lean`
  itself.

  ## Binary conversions and `eqTolPx`

  `toPixelBinary`/`fromPixelBinary`/`toImageBinary`/`fromImageBinary` and
  `eqTolPx` are ported directly below, generic over any `ColorSpace cs e`
  (needing, respectively, `[BEq px] [OfNat px 0]` for the `px == 0` test, and
  `[Sub e] [Max e] [Min e] [LE e] [DecidableRel (α := e) (· ≤ ·)]` for the
  tolerance comparison — the exact fragments of upstream's `Eq`/`Num`/`Ord`
  each function's body actually needs, following the same class-splitting
  convention `Interface.Elevator`/`X.lean`/`Binary.lean` already use
  throughout this port).
-/

import Linen.Graphics.Image.Interface
import Linen.Graphics.Image.Interface.Elevator
import Linen.Graphics.Image.ColorSpace.Y
import Linen.Graphics.Image.ColorSpace.RGB
import Linen.Graphics.Image.ColorSpace.HSI
import Linen.Graphics.Image.ColorSpace.CMYK
import Linen.Graphics.Image.ColorSpace.YCbCr
import Linen.Graphics.Image.ColorSpace.Complex
import Linen.Graphics.Image.ColorSpace.X
import Linen.Graphics.Image.ColorSpace.Binary

open Graphics.Image.Interface
  (Pixel ColorSpace AlphaSpace getAlpha addAlpha dropAlpha promote getPxC setPxC foldlPx2)
open Graphics.Image.Interface.Elevator (Elevator clamp01)
open Graphics.Image.ColorSpace.Y (Y YA PixelY PixelYA)
open Graphics.Image.ColorSpace.RGB (RGB RGBA PixelRGB PixelRGBA)
open Graphics.Image.ColorSpace.HSI (HSI HSIA PixelHSI PixelHSIA)
open Graphics.Image.ColorSpace.CMYK (CMYK CMYKA PixelCMYK PixelCMYKA)
open Graphics.Image.ColorSpace.YCbCr (YCbCr YCbCrA PixelYCbCr PixelYCbCrA)
open Graphics.Image.ColorSpace.X (X PixelX)
open Graphics.Image.ColorSpace.Binary (Bit on off isOn)

namespace Graphics.Image.ColorSpace

/-- `π`, as a `Float` literal. No `Float.pi` constant exists in Lean's core
`Float` library (unlike, e.g., `Float.atan2`/`Float.cos`/`Float.sin`, which do
exist and are used below and in `Complex.lean`), so it is defined locally
here — the one place in this port that needs it. -/
private def pi : Float := 3.14159265358979323846

-- ══════════════════════════════════════════════════════════════════════════
-- `ToRGB`/`ToRGBA` — declared first; see the module doc-comment for why.
-- ══════════════════════════════════════════════════════════════════════════

/-- Conversion to the `RGB` colour space. Upstream's `ToRGB`. -/
class ToRGB (cs e : Type) {px : outParam Type} [Pixel cs e px] {Components : outParam Type}
    [Elevator e] [ColorSpace cs e Components] where
  /-- Convert a pixel to an `RGB` pixel (double precision). Upstream's
  `toPixelRGB`. -/
  toPixelRGB : px → PixelRGB Float

export ToRGB (toPixelRGB)

/-- Convert an image to an `RGB` image (double precision). Upstream's
`toImageRGB`. -/
def toImageRGB {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [ToRGB cs e]
    (img : Graphics.Image.Interface.Image cs e) : Graphics.Image.Interface.Image RGB Float :=
  Graphics.Image.Interface.map (toPixelRGB (cs := cs) (e := e)) img

instance : ToRGB X Bit where
  toPixelRGB px := promote (cs := RGB) (e := Float) (Elevator.toFloat px.x)

instance [Elevator e] : ToRGB Y e where
  toPixelRGB px := promote (cs := RGB) (e := Float) (Elevator.toFloat px.y)

instance [Elevator e] : ToRGB YA e where
  toPixelRGB px := toPixelRGB (cs := Y) (e := e) (dropAlpha (cs := YA) (e := e) px)

instance [Elevator e] : ToRGB RGB e where
  toPixelRGB px := ⟨Elevator.toFloat px.r, Elevator.toFloat px.g, Elevator.toFloat px.b⟩

instance [Elevator e] : ToRGB RGBA e where
  toPixelRGB px := toPixelRGB (cs := RGB) (e := e) (dropAlpha (cs := RGBA) (e := e) px)

/-- Computes `HSI → RGB` per the formula recorded in `HSI.lean`'s
doc-comment. -/
instance [Elevator e] : ToRGB HSI e where
  toPixelRGB px :=
    let h' := Elevator.toFloat px.h
    let s := Elevator.toFloat px.s
    let i := Elevator.toFloat px.i
    let h := h' * 2 * pi
    let is := i * s
    let second := i - is
    let getFirst (a b : Float) : Float := i + is * Float.cos a / Float.cos b
    let getThird (v1 v2 : Float) : Float := i + 2 * is + v1 - v2
    if h < 0 then
      panic! s!"Graphics.Image.ColorSpace.toPixelRGB: HSI pixel is not properly scaled, Hue: {h'}"
    else if h < 2 * pi / 3 then
      let r := getFirst h (pi / 3 - h)
      let b := second
      let g := getThird b r
      (⟨r, g, b⟩ : PixelRGB Float)
    else if h < 4 * pi / 3 then
      let g := getFirst (h - 2 * pi / 3) (h + pi)
      let r := second
      let b := getThird r g
      (⟨r, g, b⟩ : PixelRGB Float)
    else if h < 2 * pi then
      let b := getFirst (h - 4 * pi / 3) (2 * pi - pi / 3 - h)
      let g := second
      let r := getThird g b
      (⟨r, g, b⟩ : PixelRGB Float)
    else
      panic! s!"Graphics.Image.ColorSpace.toPixelRGB: HSI pixel is not properly scaled, Hue: {h'}"

instance [Elevator e] : ToRGB HSIA e where
  toPixelRGB px := toPixelRGB (cs := HSI) (e := e) (dropAlpha (cs := HSIA) (e := e) px)

/-- Computes `YCbCr → RGB` per the formula recorded in `YCbCr.lean`'s
doc-comment. -/
instance [Elevator e] : ToRGB YCbCr e where
  toPixelRGB px :=
    let y := Elevator.toFloat px.y
    let cb := Elevator.toFloat px.cb
    let cr := Elevator.toFloat px.cr
    ⟨clamp01 (y + 1.402 * (cr - 0.5)),
     clamp01 (y - 0.34414 * (cb - 0.5) - 0.71414 * (cr - 0.5)),
     clamp01 (y + 1.772 * (cb - 0.5))⟩

instance [Elevator e] : ToRGB YCbCrA e where
  toPixelRGB px := toPixelRGB (cs := YCbCr) (e := e) (dropAlpha (cs := YCbCrA) (e := e) px)

/-- Computes `CMYK → RGB` per the formula recorded in `CMYK.lean`'s
doc-comment. -/
instance [Elevator e] : ToRGB CMYK e where
  toPixelRGB px :=
    let c := Elevator.toFloat px.c
    let m := Elevator.toFloat px.m
    let y := Elevator.toFloat px.y
    let k := Elevator.toFloat px.k
    ⟨(1 - c) * (1 - k), (1 - m) * (1 - k), (1 - y) * (1 - k)⟩

instance [Elevator e] : ToRGB CMYKA e where
  toPixelRGB px := toPixelRGB (cs := CMYK) (e := e) (dropAlpha (cs := CMYKA) (e := e) px)

/-- Conversion to `RGBA` (`RGB` with an alpha channel) from another colour
space with an alpha channel. Upstream's `ToRGBA`; `toPixelRGBA`'s default
body is upstream's `addAlpha 1 . toPixelRGB`. -/
class ToRGBA (cs e : Type) {px : outParam Type} [Pixel cs e px] {Components : outParam Type}
    [Elevator e] [ColorSpace cs e Components] [ToRGB cs e] where
  /-- Convert a pixel to an `RGBA` pixel (double precision). Upstream's
  `toPixelRGBA`. -/
  toPixelRGBA : px → PixelRGBA Float := fun p =>
    let q := toPixelRGB (cs := cs) (e := e) p
    ⟨q.r, q.g, q.b, 1⟩

export ToRGBA (toPixelRGBA)

/-- Convert an image to an `RGBA` image (double precision). Upstream's
`toImageRGBA`. -/
def toImageRGBA {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [ToRGB cs e] [ToRGBA cs e]
    (img : Graphics.Image.Interface.Image cs e) : Graphics.Image.Interface.Image RGBA Float :=
  Graphics.Image.Interface.map (toPixelRGBA (cs := cs) (e := e)) img

instance : ToRGBA X Bit where
instance [Elevator e] : ToRGBA Y e where
instance [Elevator e] : ToRGBA YA e where
  toPixelRGBA px :=
    let rgb := toPixelRGB (cs := Y) (e := e) (dropAlpha (cs := YA) (e := e) px)
    ⟨rgb.r, rgb.g, rgb.b, Elevator.toFloat (getAlpha (cs := YA) (e := e) px)⟩
instance [Elevator e] : ToRGBA RGB e where
instance [Elevator e] : ToRGBA RGBA e where
  toPixelRGBA px :=
    ⟨Elevator.toFloat px.r, Elevator.toFloat px.g, Elevator.toFloat px.b, Elevator.toFloat px.a⟩
instance [Elevator e] : ToRGBA HSI e where
instance [Elevator e] : ToRGBA HSIA e where
  toPixelRGBA px :=
    let rgb := toPixelRGB (cs := HSI) (e := e) (dropAlpha (cs := HSIA) (e := e) px)
    ⟨rgb.r, rgb.g, rgb.b, Elevator.toFloat (getAlpha (cs := HSIA) (e := e) px)⟩
instance [Elevator e] : ToRGBA CMYK e where
instance [Elevator e] : ToRGBA CMYKA e where
  toPixelRGBA px :=
    let rgb := toPixelRGB (cs := CMYK) (e := e) (dropAlpha (cs := CMYKA) (e := e) px)
    ⟨rgb.r, rgb.g, rgb.b, Elevator.toFloat (getAlpha (cs := CMYKA) (e := e) px)⟩
instance [Elevator e] : ToRGBA YCbCr e where
instance [Elevator e] : ToRGBA YCbCrA e where
  toPixelRGBA px :=
    let rgb := toPixelRGB (cs := YCbCr) (e := e) (dropAlpha (cs := YCbCrA) (e := e) px)
    ⟨rgb.r, rgb.g, rgb.b, Elevator.toFloat (getAlpha (cs := YCbCrA) (e := e) px)⟩

-- ══════════════════════════════════════════════════════════════════════════
-- `ToY`/`ToYA`
-- ══════════════════════════════════════════════════════════════════════════

/-- Conversion to the Luma (`Y`) colour space. Upstream's `ToY`. -/
class ToY (cs e : Type) {px : outParam Type} [Pixel cs e px] {Components : outParam Type}
    [Elevator e] [ColorSpace cs e Components] where
  /-- Convert a pixel to a Luma pixel (double precision). Upstream's
  `toPixelY`. -/
  toPixelY : px → PixelY Float

export ToY (toPixelY)

/-- Convert an image to a Luma image (double precision). Upstream's
`toImageY`. -/
def toImageY {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [ToY cs e]
    (img : Graphics.Image.Interface.Image cs e) : Graphics.Image.Interface.Image Y Float :=
  Graphics.Image.Interface.map (toPixelY (cs := cs) (e := e)) img

instance [Elevator e] : ToY X e where
  toPixelY px := ⟨Elevator.toFloat px.x⟩

instance [Elevator e] : ToY Y e where
  toPixelY px := ⟨Elevator.toFloat px.y⟩

instance [Elevator e] : ToY YA e where
  toPixelY px := ⟨Elevator.toFloat px.y⟩

/-- Computes Luma: `Y' = 0.299·R' + 0.587·G' + 0.114·B'` (BT.601 weights).
Freshly sourced from upstream's own `ToY RGB` instance — see the module
doc-comment's "Formula provenance" section. -/
instance [Elevator e] : ToY RGB e where
  toPixelY px :=
    ⟨0.299 * Elevator.toFloat px.r + 0.587 * Elevator.toFloat px.g + 0.114 * Elevator.toFloat px.b⟩

instance [Elevator e] : ToY RGBA e where
  toPixelY px := toPixelY (cs := RGB) (e := e) (dropAlpha (cs := RGBA) (e := e) px)

instance [Elevator e] : ToY HSI e where
  toPixelY px := toPixelY (cs := RGB) (e := Float) (toPixelRGB (cs := HSI) (e := e) px)

instance [Elevator e] : ToY HSIA e where
  toPixelY px := toPixelY (cs := HSI) (e := e) (dropAlpha (cs := HSIA) (e := e) px)

instance [Elevator e] : ToY CMYK e where
  toPixelY px := toPixelY (cs := RGB) (e := Float) (toPixelRGB (cs := CMYK) (e := e) px)

instance [Elevator e] : ToY CMYKA e where
  toPixelY px :=
    toPixelY (cs := RGB) (e := Float) (toPixelRGB (cs := CMYK) (e := e)
      (dropAlpha (cs := CMYKA) (e := e) px))

instance [Elevator e] : ToY YCbCr e where
  toPixelY px := ⟨Elevator.toFloat px.y⟩

instance [Elevator e] : ToY YCbCrA e where
  toPixelY px := ⟨Elevator.toFloat px.y⟩

/-- Conversion to `YA` (Luma with an alpha channel) from another colour space
with an alpha channel. Upstream's `ToYA`; `toPixelYA`'s default body is
upstream's `addAlpha 1 . toPixelY`. -/
class ToYA (cs e : Type) {px : outParam Type} [Pixel cs e px] {Components : outParam Type}
    [Elevator e] [ColorSpace cs e Components] [ToY cs e] where
  /-- Convert a pixel to a `YA` pixel (double precision). Upstream's
  `toPixelYA`. -/
  toPixelYA : px → PixelYA Float := fun p => ⟨(toPixelY (cs := cs) (e := e) p).y, 1⟩

export ToYA (toPixelYA)

/-- Convert an image to a `YA` image (double precision). Upstream's
`toImageYA`. -/
def toImageYA {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [ToY cs e] [ToYA cs e]
    (img : Graphics.Image.Interface.Image cs e) : Graphics.Image.Interface.Image YA Float :=
  Graphics.Image.Interface.map (toPixelYA (cs := cs) (e := e)) img

instance : ToYA X Bit where
instance [Elevator e] : ToYA Y e where
instance [Elevator e] : ToYA YA e where
  toPixelYA px := ⟨Elevator.toFloat px.y, Elevator.toFloat px.a⟩
instance [Elevator e] : ToYA RGB e where
instance [Elevator e] : ToYA RGBA e where
  toPixelYA px :=
    ⟨(toPixelY (cs := RGB) (e := e) (dropAlpha (cs := RGBA) (e := e) px)).y,
     Elevator.toFloat (getAlpha (cs := RGBA) (e := e) px)⟩
instance [Elevator e] : ToYA HSI e where
instance [Elevator e] : ToYA HSIA e where
  toPixelYA px :=
    ⟨(toPixelY (cs := HSI) (e := e) (dropAlpha (cs := HSIA) (e := e) px)).y,
     Elevator.toFloat (getAlpha (cs := HSIA) (e := e) px)⟩
instance [Elevator e] : ToYA CMYK e where
instance [Elevator e] : ToYA CMYKA e where
  toPixelYA px :=
    ⟨(toPixelY (cs := CMYK) (e := e) (dropAlpha (cs := CMYKA) (e := e) px)).y,
     Elevator.toFloat (getAlpha (cs := CMYKA) (e := e) px)⟩
instance [Elevator e] : ToYA YCbCr e where
instance [Elevator e] : ToYA YCbCrA e where
  toPixelYA px :=
    ⟨(toPixelY (cs := YCbCr) (e := e) (dropAlpha (cs := YCbCrA) (e := e) px)).y,
     Elevator.toFloat (getAlpha (cs := YCbCrA) (e := e) px)⟩

-- ══════════════════════════════════════════════════════════════════════════
-- `ToHSI`/`ToHSIA`
-- ══════════════════════════════════════════════════════════════════════════

/-- Conversion to the `HSI` colour space. Upstream's `ToHSI`. -/
class ToHSI (cs e : Type) {px : outParam Type} [Pixel cs e px] {Components : outParam Type}
    [Elevator e] [ColorSpace cs e Components] where
  /-- Convert a pixel to an `HSI` pixel (double precision). Upstream's
  `toPixelHSI`. -/
  toPixelHSI : px → PixelHSI Float

export ToHSI (toPixelHSI)

/-- Convert an image to an `HSI` image (double precision). Upstream's
`toImageHSI`. -/
def toImageHSI {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [ToHSI cs e]
    (img : Graphics.Image.Interface.Image cs e) : Graphics.Image.Interface.Image HSI Float :=
  Graphics.Image.Interface.map (toPixelHSI (cs := cs) (e := e)) img

instance [Elevator e] : ToHSI Y e where
  toPixelHSI px := ⟨0, 0, Elevator.toFloat px.y⟩

instance [Elevator e] : ToHSI YA e where
  toPixelHSI px := toPixelHSI (cs := Y) (e := e) (dropAlpha (cs := YA) (e := e) px)

/-- Computes `RGB → HSI` per the formula recorded in `HSI.lean`'s
doc-comment. -/
instance [Elevator e] : ToHSI RGB e where
  toPixelHSI px :=
    let r := Elevator.toFloat px.r
    let g := Elevator.toFloat px.g
    let b := Elevator.toFloat px.b
    let x := (2 * r - g - b) / 2.449489742783178
    let y := (g - b) / 1.4142135623730951
    let h' := Float.atan2 y x
    let h := (if h' < 0 then h' + 2 * pi else h') / (2 * pi)
    let i := (r + g + b) / 3
    let s := if i == 0 then 0 else 1 - min r (min g b) / i
    ⟨h, s, i⟩

instance [Elevator e] : ToHSI RGBA e where
  toPixelHSI px := toPixelHSI (cs := RGB) (e := e) (dropAlpha (cs := RGBA) (e := e) px)

instance [Elevator e] : ToHSI HSI e where
  toPixelHSI px := ⟨Elevator.toFloat px.h, Elevator.toFloat px.s, Elevator.toFloat px.i⟩

instance [Elevator e] : ToHSI HSIA e where
  toPixelHSI px := toPixelHSI (cs := HSI) (e := e) (dropAlpha (cs := HSIA) (e := e) px)

instance [Elevator e] : ToHSI YCbCr e where
  toPixelHSI px := toPixelHSI (cs := RGB) (e := Float) (toPixelRGB (cs := YCbCr) (e := e) px)

instance [Elevator e] : ToHSI YCbCrA e where
  toPixelHSI px := toPixelHSI (cs := YCbCr) (e := e) (dropAlpha (cs := YCbCrA) (e := e) px)

instance [Elevator e] : ToHSI CMYK e where
  toPixelHSI px := toPixelHSI (cs := RGB) (e := Float) (toPixelRGB (cs := CMYK) (e := e) px)

instance [Elevator e] : ToHSI CMYKA e where
  toPixelHSI px := toPixelHSI (cs := CMYK) (e := e) (dropAlpha (cs := CMYKA) (e := e) px)

/-- Conversion to `HSIA` (`HSI` with an alpha channel) from another colour
space with an alpha channel. Upstream's `ToHSIA`. -/
class ToHSIA (cs e : Type) {px : outParam Type} [Pixel cs e px] {Components : outParam Type}
    [Elevator e] [ColorSpace cs e Components] [ToHSI cs e] where
  /-- Convert a pixel to an `HSIA` pixel (double precision). Upstream's
  `toPixelHSIA`. -/
  toPixelHSIA : px → PixelHSIA Float := fun p =>
    let q := toPixelHSI (cs := cs) (e := e) p
    ⟨q.h, q.s, q.i, 1⟩

export ToHSIA (toPixelHSIA)

/-- Convert an image to an `HSIA` image (double precision). Upstream's
`toImageHSIA`. -/
def toImageHSIA {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [ToHSI cs e] [ToHSIA cs e]
    (img : Graphics.Image.Interface.Image cs e) : Graphics.Image.Interface.Image HSIA Float :=
  Graphics.Image.Interface.map (toPixelHSIA (cs := cs) (e := e)) img

instance [Elevator e] : ToHSIA Y e where
instance [Elevator e] : ToHSIA YA e where
  toPixelHSIA px :=
    let q := toPixelHSI (cs := Y) (e := e) (dropAlpha (cs := YA) (e := e) px)
    ⟨q.h, q.s, q.i, Elevator.toFloat (getAlpha (cs := YA) (e := e) px)⟩
instance [Elevator e] : ToHSIA RGB e where
instance [Elevator e] : ToHSIA RGBA e where
  toPixelHSIA px :=
    let q := toPixelHSI (cs := RGB) (e := e) (dropAlpha (cs := RGBA) (e := e) px)
    ⟨q.h, q.s, q.i, Elevator.toFloat (getAlpha (cs := RGBA) (e := e) px)⟩
instance [Elevator e] : ToHSIA HSI e where
instance [Elevator e] : ToHSIA HSIA e where
  toPixelHSIA px :=
    ⟨Elevator.toFloat px.h, Elevator.toFloat px.s, Elevator.toFloat px.i, Elevator.toFloat px.a⟩
instance [Elevator e] : ToHSIA CMYK e where
instance [Elevator e] : ToHSIA CMYKA e where
  toPixelHSIA px :=
    let q := toPixelHSI (cs := CMYK) (e := e) (dropAlpha (cs := CMYKA) (e := e) px)
    ⟨q.h, q.s, q.i, Elevator.toFloat (getAlpha (cs := CMYKA) (e := e) px)⟩
instance [Elevator e] : ToHSIA YCbCr e where
instance [Elevator e] : ToHSIA YCbCrA e where
  toPixelHSIA px :=
    let q := toPixelHSI (cs := YCbCr) (e := e) (dropAlpha (cs := YCbCrA) (e := e) px)
    ⟨q.h, q.s, q.i, Elevator.toFloat (getAlpha (cs := YCbCrA) (e := e) px)⟩

-- ══════════════════════════════════════════════════════════════════════════
-- `ToCMYK`/`ToCMYKA`
-- ══════════════════════════════════════════════════════════════════════════

/-- Conversion to the `CMYK` colour space. Upstream's `ToCMYK`. -/
class ToCMYK (cs e : Type) {px : outParam Type} [Pixel cs e px] {Components : outParam Type}
    [Elevator e] [ColorSpace cs e Components] where
  /-- Convert a pixel to a `CMYK` pixel (double precision). Upstream's
  `toPixelCMYK`. -/
  toPixelCMYK : px → PixelCMYK Float

export ToCMYK (toPixelCMYK)

/-- Convert an image to a `CMYK` image (double precision). Upstream's
`toImageCMYK`. -/
def toImageCMYK {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [ToCMYK cs e]
    (img : Graphics.Image.Interface.Image cs e) : Graphics.Image.Interface.Image CMYK Float :=
  Graphics.Image.Interface.map (toPixelCMYK (cs := cs) (e := e)) img

/-- Computes `RGB → CMYK` per the formula recorded in `CMYK.lean`'s
doc-comment. -/
instance [Elevator e] : ToCMYK RGB e where
  toPixelCMYK px :=
    let r := Elevator.toFloat px.r
    let g := Elevator.toFloat px.g
    let b := Elevator.toFloat px.b
    let k := 1 - max r (max g b)
    ⟨(1 - r - k) / (1 - k), (1 - g - k) / (1 - k), (1 - b - k) / (1 - k), k⟩

instance [Elevator e] : ToCMYK RGBA e where
  toPixelCMYK px := toPixelCMYK (cs := RGB) (e := e) (dropAlpha (cs := RGBA) (e := e) px)

instance [Elevator e] : ToCMYK Y e where
  toPixelCMYK px := toPixelCMYK (cs := RGB) (e := Float) (toPixelRGB (cs := Y) (e := e) px)

instance [Elevator e] : ToCMYK YA e where
  toPixelCMYK px := toPixelCMYK (cs := Y) (e := e) (dropAlpha (cs := YA) (e := e) px)

instance [Elevator e] : ToCMYK HSI e where
  toPixelCMYK px := toPixelCMYK (cs := RGB) (e := Float) (toPixelRGB (cs := HSI) (e := e) px)

instance [Elevator e] : ToCMYK HSIA e where
  toPixelCMYK px := toPixelCMYK (cs := HSI) (e := e) (dropAlpha (cs := HSIA) (e := e) px)

instance [Elevator e] : ToCMYK CMYK e where
  toPixelCMYK px :=
    ⟨Elevator.toFloat px.c, Elevator.toFloat px.m, Elevator.toFloat px.y, Elevator.toFloat px.k⟩

instance [Elevator e] : ToCMYK CMYKA e where
  toPixelCMYK px := toPixelCMYK (cs := CMYK) (e := e) (dropAlpha (cs := CMYKA) (e := e) px)

instance [Elevator e] : ToCMYK YCbCr e where
  toPixelCMYK px := toPixelCMYK (cs := RGB) (e := Float) (toPixelRGB (cs := YCbCr) (e := e) px)

instance [Elevator e] : ToCMYK YCbCrA e where
  toPixelCMYK px := toPixelCMYK (cs := YCbCr) (e := e) (dropAlpha (cs := YCbCrA) (e := e) px)

/-- Conversion to `CMYKA` (`CMYK` with an alpha channel) from another colour
space with an alpha channel. Upstream's `ToCMYKA`. -/
class ToCMYKA (cs e : Type) {px : outParam Type} [Pixel cs e px] {Components : outParam Type}
    [Elevator e] [ColorSpace cs e Components] [ToCMYK cs e] where
  /-- Convert a pixel to a `CMYKA` pixel (double precision). Upstream's
  `toPixelCMYKA`. -/
  toPixelCMYKA : px → PixelCMYKA Float := fun p =>
    let q := toPixelCMYK (cs := cs) (e := e) p
    ⟨q.c, q.m, q.y, q.k, 1⟩

export ToCMYKA (toPixelCMYKA)

/-- Convert an image to a `CMYKA` image (double precision). Upstream's
`toImageCMYKA`. -/
def toImageCMYKA {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [ToCMYK cs e] [ToCMYKA cs e]
    (img : Graphics.Image.Interface.Image cs e) : Graphics.Image.Interface.Image CMYKA Float :=
  Graphics.Image.Interface.map (toPixelCMYKA (cs := cs) (e := e)) img

instance [Elevator e] : ToCMYKA Y e where
instance [Elevator e] : ToCMYKA YA e where
  toPixelCMYKA px :=
    let q := toPixelCMYK (cs := Y) (e := e) (dropAlpha (cs := YA) (e := e) px)
    ⟨q.c, q.m, q.y, q.k, Elevator.toFloat (getAlpha (cs := YA) (e := e) px)⟩
instance [Elevator e] : ToCMYKA RGB e where
instance [Elevator e] : ToCMYKA RGBA e where
  toPixelCMYKA px :=
    let q := toPixelCMYK (cs := RGB) (e := e) (dropAlpha (cs := RGBA) (e := e) px)
    ⟨q.c, q.m, q.y, q.k, Elevator.toFloat (getAlpha (cs := RGBA) (e := e) px)⟩
instance [Elevator e] : ToCMYKA HSI e where
instance [Elevator e] : ToCMYKA HSIA e where
  toPixelCMYKA px :=
    let q := toPixelCMYK (cs := HSI) (e := e) (dropAlpha (cs := HSIA) (e := e) px)
    ⟨q.c, q.m, q.y, q.k, Elevator.toFloat (getAlpha (cs := HSIA) (e := e) px)⟩
instance [Elevator e] : ToCMYKA CMYK e where
instance [Elevator e] : ToCMYKA CMYKA e where
  toPixelCMYKA px :=
    ⟨Elevator.toFloat px.c, Elevator.toFloat px.m, Elevator.toFloat px.y, Elevator.toFloat px.k,
     Elevator.toFloat px.a⟩
instance [Elevator e] : ToCMYKA YCbCr e where
instance [Elevator e] : ToCMYKA YCbCrA e where
  toPixelCMYKA px :=
    let q := toPixelCMYK (cs := YCbCr) (e := e) (dropAlpha (cs := YCbCrA) (e := e) px)
    ⟨q.c, q.m, q.y, q.k, Elevator.toFloat (getAlpha (cs := YCbCrA) (e := e) px)⟩

-- ══════════════════════════════════════════════════════════════════════════
-- `ToYCbCr`/`ToYCbCrA`
-- ══════════════════════════════════════════════════════════════════════════

/-- Conversion to the `YCbCr` colour space. Upstream's `ToYCbCr`. -/
class ToYCbCr (cs e : Type) {px : outParam Type} [Pixel cs e px] {Components : outParam Type}
    [Elevator e] [ColorSpace cs e Components] where
  /-- Convert a pixel to a `YCbCr` pixel (double precision). Upstream's
  `toPixelYCbCr`. -/
  toPixelYCbCr : px → PixelYCbCr Float

export ToYCbCr (toPixelYCbCr)

/-- Convert an image to a `YCbCr` image (double precision). Upstream's
`toImageYCbCr`. -/
def toImageYCbCr {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [ToYCbCr cs e]
    (img : Graphics.Image.Interface.Image cs e) : Graphics.Image.Interface.Image YCbCr Float :=
  Graphics.Image.Interface.map (toPixelYCbCr (cs := cs) (e := e)) img

/-- Computes `RGB → YCbCr` per the formula recorded in `YCbCr.lean`'s
doc-comment. -/
instance [Elevator e] : ToYCbCr RGB e where
  toPixelYCbCr px :=
    let r := Elevator.toFloat px.r
    let g := Elevator.toFloat px.g
    let b := Elevator.toFloat px.b
    ⟨clamp01 (0.299 * r + 0.587 * g + 0.114 * b),
     clamp01 (0.5 - 0.168736 * r - 0.331264 * g + 0.5 * b),
     clamp01 (0.5 + 0.5 * r - 0.418688 * g - 0.081312 * b)⟩

instance [Elevator e] : ToYCbCr RGBA e where
  toPixelYCbCr px := toPixelYCbCr (cs := RGB) (e := e) (dropAlpha (cs := RGBA) (e := e) px)

instance [Elevator e] : ToYCbCr Y e where
  toPixelYCbCr px := toPixelYCbCr (cs := RGB) (e := Float) (toPixelRGB (cs := Y) (e := e) px)

instance [Elevator e] : ToYCbCr YA e where
  toPixelYCbCr px := toPixelYCbCr (cs := Y) (e := e) (dropAlpha (cs := YA) (e := e) px)

instance [Elevator e] : ToYCbCr HSI e where
  toPixelYCbCr px := toPixelYCbCr (cs := RGB) (e := Float) (toPixelRGB (cs := HSI) (e := e) px)

instance [Elevator e] : ToYCbCr HSIA e where
  toPixelYCbCr px := toPixelYCbCr (cs := HSI) (e := e) (dropAlpha (cs := HSIA) (e := e) px)

instance [Elevator e] : ToYCbCr YCbCr e where
  toPixelYCbCr px := ⟨Elevator.toFloat px.y, Elevator.toFloat px.cb, Elevator.toFloat px.cr⟩

instance [Elevator e] : ToYCbCr YCbCrA e where
  toPixelYCbCr px := toPixelYCbCr (cs := YCbCr) (e := e) (dropAlpha (cs := YCbCrA) (e := e) px)

instance [Elevator e] : ToYCbCr CMYK e where
  toPixelYCbCr px := toPixelYCbCr (cs := RGB) (e := Float) (toPixelRGB (cs := CMYK) (e := e) px)

instance [Elevator e] : ToYCbCr CMYKA e where
  toPixelYCbCr px := toPixelYCbCr (cs := CMYK) (e := e) (dropAlpha (cs := CMYKA) (e := e) px)

/-- Conversion to `YCbCrA` (`YCbCr` with an alpha channel) from another
colour space with an alpha channel. Upstream's `ToYCbCrA`. -/
class ToYCbCrA (cs e : Type) {px : outParam Type} [Pixel cs e px] {Components : outParam Type}
    [Elevator e] [ColorSpace cs e Components] [ToYCbCr cs e] where
  /-- Convert a pixel to a `YCbCrA` pixel (double precision). Upstream's
  `toPixelYCbCrA`. -/
  toPixelYCbCrA : px → PixelYCbCrA Float := fun p =>
    let q := toPixelYCbCr (cs := cs) (e := e) p
    ⟨q.y, q.cb, q.cr, 1⟩

export ToYCbCrA (toPixelYCbCrA)

/-- Convert an image to a `YCbCrA` image (double precision). Upstream's
`toImageYCbCrA`. -/
def toImageYCbCrA {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [ToYCbCr cs e] [ToYCbCrA cs e]
    (img : Graphics.Image.Interface.Image cs e) : Graphics.Image.Interface.Image YCbCrA Float :=
  Graphics.Image.Interface.map (toPixelYCbCrA (cs := cs) (e := e)) img

instance [Elevator e] : ToYCbCrA Y e where
instance [Elevator e] : ToYCbCrA YA e where
  toPixelYCbCrA px :=
    let q := toPixelYCbCr (cs := Y) (e := e) (dropAlpha (cs := YA) (e := e) px)
    ⟨q.y, q.cb, q.cr, Elevator.toFloat (getAlpha (cs := YA) (e := e) px)⟩
instance [Elevator e] : ToYCbCrA RGB e where
instance [Elevator e] : ToYCbCrA RGBA e where
  toPixelYCbCrA px :=
    let q := toPixelYCbCr (cs := RGB) (e := e) (dropAlpha (cs := RGBA) (e := e) px)
    ⟨q.y, q.cb, q.cr, Elevator.toFloat (getAlpha (cs := RGBA) (e := e) px)⟩
instance [Elevator e] : ToYCbCrA HSI e where
instance [Elevator e] : ToYCbCrA HSIA e where
  toPixelYCbCrA px :=
    let q := toPixelYCbCr (cs := HSI) (e := e) (dropAlpha (cs := HSIA) (e := e) px)
    ⟨q.y, q.cb, q.cr, Elevator.toFloat (getAlpha (cs := HSIA) (e := e) px)⟩
instance [Elevator e] : ToYCbCrA CMYK e where
instance [Elevator e] : ToYCbCrA CMYKA e where
  toPixelYCbCrA px :=
    let q := toPixelYCbCr (cs := CMYK) (e := e) (dropAlpha (cs := CMYKA) (e := e) px)
    ⟨q.y, q.cb, q.cr, Elevator.toFloat (getAlpha (cs := CMYKA) (e := e) px)⟩
instance [Elevator e] : ToYCbCrA YCbCr e where
instance [Elevator e] : ToYCbCrA YCbCrA e where
  toPixelYCbCrA px :=
    ⟨Elevator.toFloat px.y, Elevator.toFloat px.cb, Elevator.toFloat px.cr,
     Elevator.toFloat px.a⟩

-- ══════════════════════════════════════════════════════════════════════════
-- Binary conversions
-- ══════════════════════════════════════════════════════════════════════════

/-- Convert a pixel of any colour space to a `Binary` pixel: `on` if the
pixel is exactly zero on every channel, `off` otherwise. Upstream's
`toPixelBinary`. -/
def toPixelBinary {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [BEq px] [OfNat px 0] (p : px) : PixelX Bit :=
  if p == (0 : px) then on else off

/-- Convert a `Binary` pixel to a Luma pixel: `on` (foreground) becomes
`Word8`'s minimum (black), `off` (background) becomes `Word8`'s maximum
(white) — matching the grayscale-inversion convention documented in this
module's own upstream export list (binary black is `1`, luma black is `0`).
Upstream's `fromPixelBinary`. -/
def fromPixelBinary (b : PixelX Bit) : PixelY UInt8 :=
  ⟨if isOn b then 0 else 255⟩

/-- Convert an image of any colour space to a `Binary` image. Upstream's
`toImageBinary`. -/
def toImageBinary {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [BEq px] [OfNat px 0]
    (img : Graphics.Image.Interface.Image cs e) : Graphics.Image.Interface.Image X Bit :=
  Graphics.Image.Interface.map (toPixelBinary (cs := cs) (e := e)) img

/-- Convert a `Binary` image to a Luma image. Upstream's `fromImageBinary`. -/
def fromImageBinary (img : Graphics.Image.Interface.Image X Bit) :
    Graphics.Image.Interface.Image Y UInt8 :=
  Graphics.Image.Interface.map fromPixelBinary img

/-- Check whether two pixels are equal within a tolerance, channel by
channel — useful for comparing pixels with `Float`/`Float32` precision.
Ported against the narrower `[Sub e] [Max e] [Min e] [LE e] [DecidableRel (α
:= e) (· ≤ ·)]` fragment of upstream's `Ord e` — the exact pieces `max`/`min`/
`-`/`≤` actually need, following the same class-splitting convention used
throughout this port. Upstream's `eqTolPx`. -/
def eqTolPx {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [Sub e] [Max e] [Min e] [LE e]
    [DecidableRel (α := e) (· ≤ ·)] (tol : e) (p1 p2 : px) : Bool :=
  foldlPx2 (cs := cs) (e := e) (fun acc x1 x2 => acc && decide (max x1 x2 - min x1 x2 ≤ tol)) true
    p1 p2

end Graphics.Image.ColorSpace
