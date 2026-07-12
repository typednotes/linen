/-
  Linen.Graphics.Image.ColorSpace.X — a generic, "unlabeled" single-channel
  colour space used as a building block, distinct from luma `Y`

  ## Haskell equivalent
  `Graphics.Image.ColorSpace.X` from https://hackage.haskell.org/package/hip
  (module #10 of the `hip` import plan, see `docs/imports/hip/dependencies.md`,
  which lists it as depending on `Utils` (#1) alongside the baseline
  `Interface` (#3) dependency every colour-space module shares).

  ## Why `X` exists distinct from `Y`

  Upstream's own module doc-comment (a `-- ^` comment directly above `data X =
  X` in `X.hs`) states the rationale directly:

  > This is a single channel colorspace, that is designed to separate Gray
  > level values from other types of colorspace, hence it is not convertible
  > to or from, but rather is here to allow operation on arbitrary single
  > channel images. If you are looking for a true grayscale colorspace
  > `Graphics.Image.ColorSpace.Luma.Y` should be used instead.

  In other words: `Y` (module #4, `Linen.Graphics.Image.ColorSpace.Y`) carries
  *luma* semantics — a channel that specifically means "brightness" and
  participates in colour-space conversions to/from `RGB`/`HSI`/`YCbCr`/etc.
  `X` carries *no* semantic meaning at all; it is a bare, unlabeled single
  component used as scratch carrier wherever code needs to peel off or
  reassemble one arbitrary channel of some other colour space (see
  `toPixelsX`/`fromPixelsX`/`toImagesX`/`fromImagesX`/`squashWith`/
  `squashWith2` below), or — per `dependencies.md`'s module #11 note — as the
  carrier type `Binary`'s `Bool`-valued pixels build on. Because it has no
  semantic meaning, upstream is explicit that `X` is deliberately **not**
  given any `ColorSpaceConvertible` instances (unlike `Y`, which converts
  to/from `RGB` etc.) — this port carries that same absence forward: no
  colour-conversion function to/from `X` is defined here, matching upstream's
  `X.hs`, which likewise defines none.

  ## `X`/`PixelX` as a colour-space tag and pixel type

  Following the exact precedent of `Y`/`PixelY` in `Linen.Graphics.Image.
  ColorSpace.Y` (module #4, the first concrete `ColorSpace` instance and the
  pattern every later module follows) and `Interface`'s own doc-comment on why
  `Pixel`/`ColorSpace` are ordinary classes with `outParam`s rather than data
  families: upstream's `data X = X` (a single-constructor, single-inhabitant
  type used purely as a `ColorSpace` tag) becomes the inductive `X` with one
  constructor `X.x`; upstream's `newtype instance Pixel X e = PixelX { getX ::
  e }` becomes the structure `PixelX e` with one field `x`. `Eq, Enum,
  Bounded, Show, Typeable` on upstream's `X` are simplified exactly as
  `Y`/`Interface`'s own doc-comments describe: `Eq` → `deriving BEq`, `Show` →
  `deriving Repr`, `Enum`/`Bounded` → the explicit `channels` field, `Typeable`
  dropped (no Lean counterpart).

  ## No alpha variant

  Unlike `Y` (which upstream pairs with `YA`, `RGB` with an implicit alpha
  extension, etc.), upstream's `X.hs` defines **no** `XA` counterpart at all —
  checked directly against the source: the module's export list is exactly
  `X(..), Pixel(..), toPixelsX, toImagesX, fromPixelsX, fromImagesX,
  squashWith, squashWith2`, with no `XA` anywhere. This matches the rationale
  above: `X` is purely an internal single-channel carrier for `Binary` and the
  channel-separation helpers below, not a user-facing colour space that would
  ever need a transparency variant. So this port likewise defines no `XA`/
  `AlphaSpace` instance.

  ## Deferred/dropped upstream machinery

  * `Ord (Pixel X e)` — upstream derives `Ord` (alongside `Eq`) for `Pixel X
    e`, unlike `Y`'s `PixelY`/`PixelYA` (which derive only `Eq`). No other
    `ColorSpace.*` module ported so far needed a total pixel order, and `e`
    itself is only known to be an `Elevator`/have the individual arithmetic
    classes below, none of which imply `LE`/`LT`; adding an `Ord (PixelX e)`
    instance here would need `[Ord e]` threaded in for no current call site,
    so it is left for whichever later module (if any) actually needs it.
  * A custom `Show (Pixel X e)` instance (`show (PixelX g) = "<X:("++show
    g++")>"`) — superseded by the derived `Repr`, per the package-wide
    `Show`→`Repr` simplification already used throughout `Y`/`RGB`/etc.
  * `Functor (Pixel X)`/`Applicative (Pixel X)`/`Foldable (Pixel X)`/`Monad
    (Pixel X)` and `liftPx = fmap`/`liftPx2 = liftA2`/`foldlPx = foldl'` — for
    the exact reason `Y.lean`'s doc-comment gives for the same instances on
    `Pixel Y`: since `liftPx`/`liftPx2`/`foldlPx`/`promote` are `ColorSpace`
    fields here rather than derived from a higher-kinded instance on a data
    family, these wrapper instances would be redundant and are not ported.
  * `Storable (Pixel X e)` — dropped, per the package-wide scope note in
    `dependencies.md` (GHC FFI machinery with no Lean counterpart).

  ## `Num`/`Fractional`-style arithmetic on `PixelX`

  Following `Y.lean`'s precedent (`Interface`'s own doc-comment defers the
  generic `Num (Pixel cs e)`/`Fractional (Pixel cs e)` instances to the
  concrete colour-space modules), `Add`/`Sub`/`Mul`/`Div`/`Neg`/`OfNat` are
  instantiated component-wise on `PixelX e`, conditional on the matching
  instance for `e` — even though upstream's `X.hs` itself does not define
  these (only `Y`'s `Pixel cs e` gets them generically, via the dropped
  `ColorSpace cs e => Num (Pixel cs e)` instance in `Interface.hs`, which
  every concrete `cs` — including `X` — inherits upstream). Porting them here
  keeps `PixelX` usable the same way upstream's generic instance made it
  usable, and gives `Binary` (module #11, built directly on `X`) the same
  component-wise arithmetic to draw on if it needs it. `abs`/`signum` and any
  `Floating` operations are not ported, for the same reason `Y.lean` omits
  them: no polymorphic `Abs`/`Signum`/`Floating` class spans an arbitrary
  component type `e` in Lean's stdlib.

  ## Channel-separation helpers

  Upstream's `toPixelsX`/`fromPixelsX` operate purely at the *pixel* level
  (via `foldrPx`/`setPxC`/`promote`, all already generic `ColorSpace`
  operations from `Interface`), so they port directly as plain functions
  polymorphic over any `ColorSpace cs e Components` instance — no `Image`
  involved. `fromPixelsX`'s seed `promote 0` needs a concrete `OfNat e 0`
  instance (upstream's `0` relies on `Num e`, which Lean's stdlib splits, per
  `Y.lean`'s and `Interface`'s note on that same split).

  `toImagesX`/`fromImagesX`/`squashWith`/`squashWith2` operate at the *image*
  level. Because `Interface`'s representation-collapse decision already made
  `Image cs e` a concrete `Manifest`-backed type (not the abstract `Image arr
  cs e` upstream's versions are polymorphic over), these port directly against
  that concrete type using `Interface.map`/`Interface.zipWith`/
  `Interface.dims`/`Interface.makeImage`, with `channels` standing in for
  upstream's `enumFrom minBound` (the same `Enum`/`Bounded` → `channels`
  substitution `Interface`'s doc-comment already documents). `squashWith2`'s
  `PixelX .:! foldlPx2 f a` is ported using `Graphics.Image.Utils.compose₂!`
  — the one place this module actually needs `Utils`, matching
  `dependencies.md`'s note that `X` depends on module #1.

  `fromImagesX`'s accumulator seed upstream is the literal `0`, resolved via
  a generic `Num (Image arr cs e)` instance (itself derived from `Num (Pixel
  cs e)`, per `Interface`'s own doc-comment deferring *that* instance
  entirely — no `Image`-level `Num` instance has been ported for any colour
  space in this port so far). Rather than fabricate one just for this call
  site, this port seeds the accumulator directly from the first image's own
  dimensions instead (`makeImage (dims img0) (fun _ => promote 0)`), which is
  observationally identical to upstream whenever the list is non-empty (every
  subsequent `zipWith` requires matching dimensions anyway, so upstream's
  scalar `0` would only ever survive unused for an empty list). An empty list
  therefore `panic!`s here rather than silently returning a degenerate 1×1
  zero image the way upstream's `0` literal would — a minor, documented
  deviation for an input shape no real call site produces.
-/

import Linen.Graphics.Image.Interface
import Linen.Graphics.Image.Utils

open Graphics.Image.Interface
  (Pixel ColorSpace channels toComponents fromComponents promote getPxC setPxC mapPxC liftPx
    liftPx2 foldlPx2 foldrPx)
open Graphics.Image.Interface.Elevator (Elevator)

namespace Graphics.Image.ColorSpace.X

-- ── `X` — a generic, unlabeled single channel ──

/-- A generic, semantically-unlabeled single-channel colour space, upstream's
`X` (single constructor `X`, here `X.x`). Deliberately not convertible to or
from any other colour space — see the module doc-comment. -/
inductive X where
  | x
deriving BEq, Repr, Inhabited

/-- A single-channel pixel with no attached semantics. Upstream's `newtype
instance Pixel X e = PixelX { getX :: e }`. -/
structure PixelX (e : Type) where
  /-- The single component. -/
  x : e
deriving BEq, Repr, Inhabited

instance : Pixel X e (PixelX e) where

instance [Elevator e] : ColorSpace X e e where
  channels := [X.x]
  toComponents px := px.x
  fromComponents c := ⟨c⟩
  promote v := ⟨v⟩
  getPxC px _ := px.x
  setPxC _ _ v := ⟨v⟩
  mapPxC f px := ⟨f X.x px.x⟩
  liftPx f px := ⟨f px.x⟩
  liftPx2 f px1 px2 := ⟨f px1.x px2.x⟩
  foldlPx2 f z px1 px2 := f z px1.x px2.x

-- ── Component-wise arithmetic on `PixelX` ──

instance [Add e] : Add (PixelX e) where
  add px1 px2 := ⟨px1.x + px2.x⟩

instance [Sub e] : Sub (PixelX e) where
  sub px1 px2 := ⟨px1.x - px2.x⟩

instance [Mul e] : Mul (PixelX e) where
  mul px1 px2 := ⟨px1.x * px2.x⟩

instance [Div e] : Div (PixelX e) where
  div px1 px2 := ⟨px1.x / px2.x⟩

instance [Neg e] : Neg (PixelX e) where
  neg px := ⟨-px.x⟩

instance [OfNat e n] : OfNat (PixelX e) n where
  ofNat := ⟨OfNat.ofNat n⟩

-- ── Channel-separation helpers ──

/-- Separate a pixel into a list of single-channel `X` pixels, one per
channel, in channel order. Upstream's `toPixelsX`. -/
def toPixelsX {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] (p : px) : List (PixelX e) :=
  foldrPx (cs := cs) (e := e) (fun c acc => (⟨c⟩ : PixelX e) :: acc) [] p

/-- Combine a list of `(channel, X-pixel)` pairs into a pixel of colour space
`cs`, setting each named channel from its paired `X` pixel's single
component. The starting pixel is seeded via `promote 0`, requiring `[OfNat e
0]` in place of upstream's `Num e` — see the module doc-comment. Upstream's
`fromPixelsX`. -/
def fromPixelsX {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [OfNat e 0] (xs : List (cs × PixelX e)) : px :=
  xs.foldl (fun p (c, v) => setPxC p c v.x) (promote (cs := cs) (e := e) (0 : e))

/-- Separate an image into a list of images with `X` pixels, one per channel
of the source colour space, in channel order. Upstream's `toImagesX`
(`enumFrom minBound` becomes `channels`, per `Interface`'s `Enum`/`Bounded` →
`channels` substitution). -/
def toImagesX {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [Inhabited e]
    (img : Graphics.Image.Interface.Image cs e) : List (Graphics.Image.Interface.Image X e) :=
  (channels (cs := cs) (e := e)).map
    (fun ch => Graphics.Image.Interface.map (fun p => (⟨getPxC p ch⟩ : PixelX e)) img)

/-- Combine a list of `(channel, X-image)` pairs into an image of colour space
`cs`, setting each named channel of every pixel from the corresponding `X`
image at the same location. `panic!`s on an empty list — see the module
doc-comment for why this deviates from upstream's `0`-seeded `fromXs`.
Upstream's `fromImagesX`. -/
def fromImagesX {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [OfNat e 0] [Inhabited px]
    (xs : List (cs × Graphics.Image.Interface.Image X e)) :
    Graphics.Image.Interface.Image cs e :=
  match xs with
  | [] => panic! "Graphics.Image.ColorSpace.X.fromImagesX: empty list of images"
  | (_, img0) :: _ =>
    let base := Graphics.Image.Interface.makeImage (Graphics.Image.Interface.dims img0)
      (fun _ => (promote (cs := cs) (e := e) (0 : e) : px))
    xs.foldl
      (fun img (c, xi) =>
        Graphics.Image.Interface.zipWith (fun p (v : PixelX e) => setPxC p c v.x) img xi)
      base

/-- Apply a left fold to every pixel of an image, collapsing it to a
single-channel `X` image. Upstream's `squashWith`. -/
def squashWith {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] {b : Type} (f : b → e → b) (a : b)
    (img : Graphics.Image.Interface.Image cs e) : Graphics.Image.Interface.Image X b :=
  Graphics.Image.Interface.map
    (fun p => (⟨Graphics.Image.Interface.foldlPx (cs := cs) (e := e) f a p⟩ : PixelX b)) img

/-- Combination of `zipWith` and a simultaneous left fold on two pixels at the
same location, collapsing two images of the same colour space into a single
`X` image. Upstream's `squashWith2`; upstream's `PixelX .:! foldlPx2 f a`
becomes `Graphics.Image.Utils.compose₂!` applied to the `PixelX` constructor
and `foldlPx2 f a` — the one place this module needs `Utils`. -/
def squashWith2 {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] {b : Type} [Inhabited b] (f : b → e → e → b) (a : b)
    (img1 img2 : Graphics.Image.Interface.Image cs e) : Graphics.Image.Interface.Image X b :=
  Graphics.Image.Interface.zipWith
    (Graphics.Image.Utils.compose₂! (fun v => (⟨v⟩ : PixelX b)) (foldlPx2 (cs := cs) (e := e) f a))
    img1 img2

end Graphics.Image.ColorSpace.X
