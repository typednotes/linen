/-
  Linen.Graphics.Image.IO.Base — shared reader/writer typeclasses and
  pixel-precision normalisation used by every concrete image-format backend

  ## Haskell equivalent

  `Graphics.Image.IO.Base` from https://hackage.haskell.org/package/hip
  (module #21 of the `hip` import plan, see `docs/imports/hip/dependencies.md`
  — the first module of the `IO` sub-tree). Fetched from the 1.5.6.0 release
  tarball (`raw.githubusercontent.com/lehins/hip/master/…` 404s: the
  repository's default branch has moved on since that release).

  ## What upstream actually is

  Confirming the task brief's own guess: this module is *entirely*
  types/typeclasses, with no concrete file-format logic at all — exactly the
  "base/shared-types module that the format-specific glue modules build on"
  the dependency plan anticipated. Its whole body is:

  - `Convertible cs e` — a class picking, for one of four canonical
    "presentation" pixel formats (`Y`/`YA`/`RGB`/`RGBA`, always at double
    precision), which of `toImageY`/`toImageYA`/`toImageRGB`/`toImageRGBA`
    (module #12, `Linen.Graphics.Image.ColorSpace`) normalises an arbitrary
    source image into it. Used by format backends that only know how to
    write one canonical pixel layout (e.g. always emit 8-bit RGB) regardless
    of the `Image`'s actual colour space.
  - `Seq f` — a zero-cost newtype tag marking a format as supporting
    sequences of images (e.g. animated GIF), so `Readable`/`Writable`/
    `ImageFormat` instances can be given for `Seq GifFormat` distinctly from
    plain `GifFormat`.
  - `ImageFormat format` — associates a format tag type with its own
    per-format save-option type (upstream's `data family SaveOption
    format`) and a file-extension vocabulary (`ext`/`exts`/`isFormat`).
  - `Readable img format` / `Writable img format` — decode/encode a
    concrete `img` type against one format, parameterised by that format's
    own `SaveOption` type.
  - `ComplexWritable` plus the one instance built on it: any format that can
    write a real-valued image can also write a complex-valued image, by
    laying the real and imaginary parts side by side (`leftToRight`) and
    writing that.

  This module reuses `Linen.Codec.Picture.*`/`Linen.Graphics.Netpbm` only in
  the loose sense the dependency plan describes: it does not import either
  directly (upstream's own `Base.hs` doesn't import `JuicyPixels`/`netpbm`
  either — only `Graphics.Image.ColorSpace`/`Interface`/`Processing.Complex`/
  `Processing.Geometric`), but it is precisely the class hierarchy that
  modules #22/#23 (`IO.Formats.JuicyPixels`/`IO.Formats.Netpbm`) will give
  concrete `ImageFormat`/`Readable`/`Writable` instances for, wiring each
  format's `decode`/`encode` to `Linen.Codec.Picture`'s/`Linen.Graphics.
  Netpbm`'s already-ported decoders/encoders. No mismatch was found between
  upstream's abstraction here and that reuse plan: `Readable`/`Writable`'s
  `decode : format → ByteArray → Except String img` / `encode : format →
  List SaveOption → img → ByteArray` shapes are already how
  `Linen.Codec.Picture`'s own `decodeImage`/`Encoder`-family functions and
  `Linen.Graphics.Netpbm.parsePPM` are shaped (byte-array in, `Except`/plain
  value out) — the marshalling modules #22/#23 will need is purely at the
  pixel-array level (hip's `Image cs e` ↔ `Codec.Picture`'s `DynamicImage`/
  `Netpbm`'s `PPM`), not at this module's decode/encode-signature level.

  ## `ByteString`/`ByteString.Lazy` → `ByteArray`

  Upstream's `decode`/`encode` are typed over `Data.ByteString.ByteString`
  (strict, input) and `Data.ByteString.Lazy.ByteString` (lazy, output) —
  purely a GHC memory-strategy distinction with no bearing on either
  function's actual behaviour. Following `Linen.Codec.Picture`'s own
  established convention throughout (`decodeImage`, every `save*Image`, …),
  both collapse to Lean's plain `ByteArray`.

  ## `Convertible`: a class whose method is itself polymorphic

  Upstream's `class Convertible cs e where convert :: (ToYA cs' e', ToRGBA
  cs' e', Array arr cs' e', Array arr cs e) => Image arr cs' e' -> Image arr
  cs e` has a method (`convert`) universally quantified over a *second*,
  method-local pair `cs'`/`e'` not among the class's own parameters — every
  instance shares one constraint set (the union `ToYA cs' e', ToRGBA cs' e'`
  of what any of the four instances' bodies actually needs) even though a
  given instance's body only calls one of `toImageY`/`toImageYA`/
  `toImageRGB`/`toImageRGBA`. This ports directly: `cs'`/`e'`/their `Pixel`/
  `ColorSpace` witnesses become the method's own (rather than the class's)
  implicit/instance binders, mirroring exactly how upstream's method
  signature — not its class head — carries the `cs'`/`e'` quantification.

  ## `ComplexWritable`: inlined rather than named

  Upstream's `type ComplexWritable format arr cs e = (...)` is a
  *constraint* type synonym bundling six class constraints purely so the one
  instance below can state them with a shorter name — it has no other use
  site and carries no data of its own. Lean has no direct counterpart for a
  bare named conjunction of instance-implicit constraints (unlike a
  `Prop`-valued `abbrev`, these are `Type`-valued classes), so, following the
  same "flatten a single-use dispatch/constraint helper directly into its
  one call site" simplification `Linen.Codec.Picture.Saving`'s own
  doc-comment already applies to `Decimable`, the six constraints are
  inlined directly into the `Writable (Image cs (Complex Float)) format`
  instance's own signature instead of being named separately.

  ## `RealFloat e` → `Float`

  Upstream's `ComplexWritable`/its instance are generic over any `RealFloat
  e`. Per `Linen.Graphics.Image.Processing.Complex`'s and `.Complex.Fourier`'s
  own already-established simplification (`Interface.lean` provides no
  generic `Floating (Pixel cs e)` to underwrite a `RealFloat e`-polymorphic
  image operation), `e` is specialised to `Float` here too — the same
  precision `realPartImg`/`imagPartImg`/`leftToRight`'s callers already use
  throughout that module.

  ## `Applicative (Pixel cs)`

  Upstream's `ComplexWritable` also constrains `Applicative (Pixel cs)`, used
  transitively inside `leftToRight`'s callees for pixel-shape bookkeeping in
  the original `Vector`/`Repa`-backed representations. With the
  representation collapse already carried out in `Interface.lean` (see that
  module's doc-comment), `leftToRight`/`realPartImg`/`imagPartImg` need no
  `Applicative` witness at all — their ported signatures are already
  constraint-complete without it, so it is dropped as vestigial, the same
  treatment `Interface.lean` gives every other GHC representation-dispatch
  constraint.
-/

import Linen.Graphics.Image.ColorSpace
import Linen.Graphics.Image.Processing.Complex
import Linen.Graphics.Image.Processing.Geometric

open Graphics.Image.Interface (Pixel ColorSpace Image)
open Graphics.Image.Interface.Elevator (Elevator)
open Graphics.Image.ColorSpace (ToY ToYA ToRGB ToRGBA toImageY toImageYA toImageRGB toImageRGBA)
open Graphics.Image.ColorSpace.Y (Y YA)
open Graphics.Image.ColorSpace.RGB (RGB RGBA)
open Graphics.Image.Processing.Complex (realPartImg imagPartImg)
open Graphics.Image.Processing.Geometric (leftToRight)
open Data (Complex)

namespace Graphics.Image.IO.Base

-- ── `Convertible` — normalise an arbitrary image to one of four canonical pixel layouts ──

/-- Normalise an arbitrary image to a canonical presentation pixel format
`Image cs e` (one of `Y`/`YA`/`RGB`/`RGBA` at double precision, per the
instances below). Upstream's `Convertible cs e`; see the module doc-comment
for why `convert`'s `cs'`/`e'` binders live on the method, not the class. -/
class Convertible (cs e : Type) {px : outParam Type} [Pixel cs e px] where
  /-- Convert an arbitrary source image into this instance's canonical
  target format. Upstream's `convert`. -/
  convert : {cs' e' px' : Type} → [Pixel cs' e' px'] → [Elevator e'] →
    {Components' : Type} → [ColorSpace cs' e' Components'] →
    [ToY cs' e'] → [ToYA cs' e'] → [ToRGB cs' e'] → [ToRGBA cs' e'] →
    Image cs' e' → Image cs e

export Convertible (convert)

instance : Convertible Y Float where
  convert img := toImageY img

instance : Convertible YA Float where
  convert img := toImageYA img

instance : Convertible RGB Float where
  convert img := toImageRGB img

instance : Convertible RGBA Float where
  convert img := toImageRGBA img

-- ── `Seq` — tag marking a format as supporting a sequence of images ──

/-- Wrapper distinguishing a format's "single image" instances from its
"sequence of images" ones (e.g. a static vs. an animated GIF). Upstream's
`newtype Seq f = Seq f`. Note: this name shares its bare identifier with
Lean core's own `Seq` (the `<*>`-flavoured applicative-sequencing class) —
callers outside this namespace should refer to it as
`Graphics.Image.IO.Base.Seq` to avoid ambiguity, exactly as this module's own
tests do. -/
structure Seq (format : Type) where
  /-- The wrapped format tag. Upstream's implicit `Seq`-constructor field
  (no accessor is named upstream either). -/
  unSeq : format

-- ── `ImageFormat` — a format tag's file-extension vocabulary and save-option type ──

/-- A recognisable image file format, with its own per-format save-option
type `SaveOption` (upstream's `data family SaveOption format`, ported as an
`outParam` associated type per the class-family convention already
established throughout this port, e.g. `Components` in `Interface.
ColorSpace`) and file-extension vocabulary. Upstream's `ImageFormat`. -/
class ImageFormat (format : Type) (SaveOption : outParam Type) where
  /-- The default file extension for this format, e.g. `".png"`. Upstream's
  `ext`. -/
  ext : format → String
  /-- Every file extension commonly used for this format, e.g. `[".jpeg",
  ".jpg"]`. Upstream's `exts`; defaults to the singleton `[ext f]`. -/
  exts : format → List String := fun f => [ext f]
  /-- Whether a file extension corresponds to this format, e.g.
  `isFormat ".png" .png == true`. Upstream's `isFormat`; defaults to
  membership in `exts`. -/
  isFormat : String → format → Bool := fun e f => (exts f).contains e

export ImageFormat (ext exts isFormat)

-- ── `Readable`/`Writable` — decode/encode a concrete image type against one format ──

/-- A format `img` can be decoded from. Upstream's `Readable`. -/
class Readable (img format : Type) {SaveOption : outParam Type} [ImageFormat format SaveOption] where
  /-- Decode an image from a byte string, in this format. Upstream's
  `decode`. -/
  decode : format → ByteArray → Except String img

export Readable (decode)

/-- A format `img` can be encoded to. Upstream's `Writable`. -/
class Writable (img format : Type) {SaveOption : outParam Type} [ImageFormat format SaveOption] where
  /-- Encode an image into a byte string, in this format, with the given
  save options. Upstream's `encode`. -/
  encode : format → List SaveOption → img → ByteArray

export Writable (encode)

-- ── Writing complex images: real part left of imaginary part ──

/-- Any format able to write a real-valued image can also write a
complex-valued one, by placing the real part to the left of the imaginary
part (`leftToRight`) and writing that combined image. Upstream's
`ComplexWritable`-constrained `Writable (Image arr cs (Complex e)) format`
instance; see the module doc-comment for why `ComplexWritable` itself is
inlined here rather than named, and why `e` is specialised to `Float`. -/
instance {cs format px pxC Components ComponentsC SaveOption : Type}
    [Pixel cs Float px] [Inhabited px] [ColorSpace cs Float Components]
    [Pixel cs (Complex Float) pxC] [ColorSpace cs (Complex Float) ComponentsC]
    [ImageFormat format SaveOption] [Writable (Image cs Float) format] :
    Writable (Image cs (Complex Float)) format where
  encode format opts imgC :=
    encode format opts (leftToRight (realPartImg imgC) (imagPartImg imgC))

end Graphics.Image.IO.Base
