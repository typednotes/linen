/-!
  Port of `Codec.Picture.Types` from the `JuicyPixels` package
  (see `docs/imports/JuicyPixels/dependencies.md`, module 1 of 29).

  ## Simplifications specific to this module

  - Upstream distinguishes a persistent `Image a` from a mutable, `ST`-region
    `MutableImage s a`, with `freezeImage`/`thawImage`/`unsafeFreezeImage`/
    `unsafeThawImage` converting between them, and a "safe" (bounds-checked)
    vs. "unsafe" pixel-access API (`pixelAt`/`readPixel`/`writePixel` vs.
    `unsafePixelAt`/`unsafeReadPixel`/`unsafeWritePixel`) — the latter existing
    purely to let callers skip GHC's bounds-check overhead in hot loops. Lean
    has no `ST`-region distinction to mirror (there is only ever one, pure,
    persistent `Image`, exactly as `Linen.Data.Array.Shaped.Repr.Manifest`
    collapses several Haskell backing-store representations into one
    `Array`-backed structure with plain pure functions), and `Array.get!`/
    `Array.set!` already give bounds-checked access with no separate "unsafe"
    twin needed. So `MutableImage`, freeze/thaw, and the unsafe pixel-access
    functions are all dropped; `Image.getPixel`/`Image.setPixel` below do the
    job of every one of those upstream names at once.
  - Upstream's `Pixel` class uses an associated type family
    `type PixelBaseComponent a`. A first attempt at porting this ported
    `Component` as a same-named class field (`Component : Type`), mirroring
    how `Linen.Data.Array.Shaped.Base.Source` exposes `extent`/`linearIndex`
    — but unlike those, nothing outside `Pixel` itself ever needs to
    type-class-search *for* something keyed on `Component α`: callers always
    already know a concrete component type once they know a concrete `α`.
    A bare class field turned out to be the wrong tool regardless: Lean's
    typeclass search indexes goals by their head symbol *before* unfolding
    class-field projections, so a goal like `BEq (Component PixelRGB8)`
    never gets far enough to see it is really `BEq Pixel8` and fails outright
    — this bit every `Component α`-keyed lookup (extracting/quantizing a
    single component, `Inhabited`/`BEq` on components, etc). The fix is
    Lean's standard pattern for this exact situation: an `outParam` second
    type parameter (`Pixel α Component`, as `GetElem.Elem` and `HAdd`'s
    output type do), so `Component` is unified to a concrete type as soon as
    `Pixel α Component` itself is resolved, never left as an unresolved
    projection for some *other* class to choke on.
  - Upstream's `pixelBaseIndex`/`mutablePixelBaseIndex` compute a flat-array
    offset that callers combine with `unsafeIndexM`/`VS.unsafeIndex`
    themselves. This Lean port instead exposes `Pixel.toComponents`/
    `Pixel.fromComponents` (a pixel's fixed-size component array, and its
    inverse), and `Image.getPixel`/`Image.setPixel` use those together with
    `componentCount` to do the offset arithmetic once, internally.
  - `Storable`/`NFData` instances (GHC FFI layout and strictness-forcing) are
    dropped, per the package-wide note in `dependencies.md`.
  - `PixelF`/`PixelRGBF` are ported using Lean's `Float32`, a genuine 32-bit
    float type with full arithmetic/comparison/conversion support — bit-width
    faithful to Haskell's `Float`, unlike Lean's default 64-bit `Float`.
  - The `GenST`/`Traversal` lens-style pixel-traversal machinery
    (`imagePixels`, `imageIPixels`, `writePx`, `freezeGenST`) exists upstream
    only to let `lens`-style code compose over the `ST`-based `MutableImage`;
    with `MutableImage` gone, `pixelMap`/`pixelMapXY`/`pixelFold` (already
    upstream, kept below) cover the same ground with plain functions, so the
    lens machinery itself is dropped.
  - `ColorConvertible`/`ColorSpaceConvertible`/`ColorPlane` upstream carry a
    `Pixel a`/`Pixel b` superclass constraint, but never use the associated
    component type in their own methods; that constraint is dropped here as
    unused machinery (it never wards off an otherwise ill-typed instance,
    since `promotePixel`/`convertPixel`/`toComponentIndex` don't mention
    `Component` at all).
-/

namespace Codec.Picture

-- ── Component base types ──

/-- An 8-bit pixel component. -/
abbrev Pixel8 := UInt8

/-- A 16-bit pixel component. -/
abbrev Pixel16 := UInt16

/-- A 32-bit pixel component. -/
abbrev Pixel32 := UInt32

/-- A floating-point (32-bit) pixel component. -/
abbrev PixelF := Float32

-- ── The `Pixel` class ──

/-- A pixel type: a fixed-size tuple of `Component` values, with the
    conversions and combinators every codec needs to move between whole
    pixels and their flat component representation. `Component` is an
    `outParam` (see the module doc-comment) so it is always resolved to a
    concrete type as soon as a concrete `α` is known. -/
class Pixel (α : Type) (Component : outParam Type) where
  /-- The neutral/default component value (upstream's implicit zero-fill). -/
  zero : Component
  /-- Number of components per pixel (e.g. `3` for RGB, `4` for RGBA). -/
  componentCount : Nat
  /-- Decompose a pixel into its `componentCount` components, in storage
      order. -/
  toComponents : α → Array Component
  /-- Reconstruct a pixel from a `componentCount`-length component array
      (as produced by `toComponents`). -/
  fromComponents : Array Component → α
  /-- Combine two pixels component-wise; the combining function is also
      given the component's index (`0`-based). -/
  mixWith : (Nat → Component → Component → Component) → α → α → α
  /-- Like `mixWith`, but the last component (alpha) is combined with a
      separate function. Defaults to `mixWith` when there is no alpha. -/
  mixWithAlpha : (Nat → Component → Component → Component) →
      (Component → Component → Component) → α → α → α :=
    fun f _ => mixWith f
  /-- A pixel's opacity component (its alpha, or fully-opaque if none). -/
  pixelOpacity : α → Component
  /-- Apply a function to every component of a pixel. -/
  colorMap : (Component → Component) → α → α := fun f p =>
    fromComponents (toComponents p |>.map f)

/-- A pixel type that genuinely carries a transparency (alpha) component,
    as opposed to `pixelOpacity` returning a synthetic constant. `DropAlpha`
    is an `outParam` for the same reason `Pixel`'s `Component` is. -/
class TransparentPixel (α : Type) {Component : outParam Type} [Pixel α Component]
    (DropAlpha : outParam Type) where
  /-- Replace a pixel's alpha component. -/
  setOpacity : Component → α → α
  /-- Drop the alpha component, keeping only the opaque colour. -/
  dropAlphaLayer : α → DropAlpha

-- ── Concrete pixel types ──

/-- Grey-level pixel with an alpha channel, 8 bits per component. -/
structure PixelYA8 where
  y : Pixel8
  a : Pixel8
deriving BEq, Repr, Inhabited

/-- Grey-level pixel with an alpha channel, 16 bits per component. -/
structure PixelYA16 where
  y : Pixel16
  a : Pixel16
deriving BEq, Repr, Inhabited

/-- RGB pixel, 8 bits per component. -/
structure PixelRGB8 where
  r : Pixel8
  g : Pixel8
  b : Pixel8
deriving BEq, Repr, Inhabited

/-- RGB pixel, 16 bits per component. -/
structure PixelRGB16 where
  r : Pixel16
  g : Pixel16
  b : Pixel16
deriving BEq, Repr, Inhabited

/-- RGB pixel with floating-point components. -/
structure PixelRGBF where
  r : PixelF
  g : PixelF
  b : PixelF
deriving BEq, Repr, Inhabited

/-- RGB pixel with an alpha channel, 8 bits per component. -/
structure PixelRGBA8 where
  r : Pixel8
  g : Pixel8
  b : Pixel8
  a : Pixel8
deriving BEq, Repr, Inhabited

/-- RGB pixel with an alpha channel, 16 bits per component. -/
structure PixelRGBA16 where
  r : Pixel16
  g : Pixel16
  b : Pixel16
  a : Pixel16
deriving BEq, Repr, Inhabited

/-- YCbCr pixel (JPEG's native colour space), 8 bits per component. -/
structure PixelYCbCr8 where
  y : Pixel8
  cb : Pixel8
  cr : Pixel8
deriving BEq, Repr, Inhabited

/-- CMYK pixel, 8 bits per component. -/
structure PixelCMYK8 where
  c : Pixel8
  m : Pixel8
  y : Pixel8
  k : Pixel8
deriving BEq, Repr, Inhabited

/-- CMYK pixel, 16 bits per component. -/
structure PixelCMYK16 where
  c : Pixel16
  m : Pixel16
  y : Pixel16
  k : Pixel16
deriving BEq, Repr, Inhabited

/-- YCbCr pixel with an extra black (`K`) component, as used by some Adobe
    JPEG files. -/
structure PixelYCbCrK8 where
  y : Pixel8
  cb : Pixel8
  cr : Pixel8
  k : Pixel8
deriving BEq, Repr, Inhabited

-- ── `Pixel` instances ──

private def opaque8 : Pixel8 := 255
private def opaque16 : Pixel16 := 65535
private def opaque32 : Pixel32 := 4294967295
private def opaqueF : PixelF := 1.0

instance : Pixel Pixel8 Pixel8 where
  zero := 0
  componentCount := 1
  toComponents p := #[p]
  fromComponents c := c.getD 0 0
  mixWith f p1 p2 := f 0 p1 p2
  pixelOpacity _ := opaque8

instance : Pixel Pixel16 Pixel16 where
  zero := 0
  componentCount := 1
  toComponents p := #[p]
  fromComponents c := c.getD 0 0
  mixWith f p1 p2 := f 0 p1 p2
  pixelOpacity _ := opaque16

instance : Pixel Pixel32 Pixel32 where
  zero := 0
  componentCount := 1
  toComponents p := #[p]
  fromComponents c := c.getD 0 0
  mixWith f p1 p2 := f 0 p1 p2
  pixelOpacity _ := opaque32

instance : Pixel PixelF PixelF where
  zero := 0.0
  componentCount := 1
  toComponents p := #[p]
  fromComponents c := c.getD 0 0.0
  mixWith f p1 p2 := f 0 p1 p2
  pixelOpacity _ := opaqueF

instance : Pixel PixelYA8 Pixel8 where
  zero := 0
  componentCount := 2
  toComponents p := #[p.y, p.a]
  fromComponents c := ⟨c.getD 0 0, c.getD 1 0⟩
  mixWith f p1 p2 := ⟨f 0 p1.y p2.y, f 1 p1.a p2.a⟩
  mixWithAlpha f fa p1 p2 := ⟨f 0 p1.y p2.y, fa p1.a p2.a⟩
  pixelOpacity p := p.a

instance : Pixel PixelYA16 Pixel16 where
  zero := 0
  componentCount := 2
  toComponents p := #[p.y, p.a]
  fromComponents c := ⟨c.getD 0 0, c.getD 1 0⟩
  mixWith f p1 p2 := ⟨f 0 p1.y p2.y, f 1 p1.a p2.a⟩
  mixWithAlpha f fa p1 p2 := ⟨f 0 p1.y p2.y, fa p1.a p2.a⟩
  pixelOpacity p := p.a

instance : Pixel PixelRGB8 Pixel8 where
  zero := 0
  componentCount := 3
  toComponents p := #[p.r, p.g, p.b]
  fromComponents c := ⟨c.getD 0 0, c.getD 1 0, c.getD 2 0⟩
  mixWith f p1 p2 := ⟨f 0 p1.r p2.r, f 1 p1.g p2.g, f 2 p1.b p2.b⟩
  pixelOpacity _ := opaque8

instance : Pixel PixelRGB16 Pixel16 where
  zero := 0
  componentCount := 3
  toComponents p := #[p.r, p.g, p.b]
  fromComponents c := ⟨c.getD 0 0, c.getD 1 0, c.getD 2 0⟩
  mixWith f p1 p2 := ⟨f 0 p1.r p2.r, f 1 p1.g p2.g, f 2 p1.b p2.b⟩
  pixelOpacity _ := opaque16

instance : Pixel PixelRGBF PixelF where
  zero := 0.0
  componentCount := 3
  toComponents p := #[p.r, p.g, p.b]
  fromComponents c := ⟨c.getD 0 0.0, c.getD 1 0.0, c.getD 2 0.0⟩
  mixWith f p1 p2 := ⟨f 0 p1.r p2.r, f 1 p1.g p2.g, f 2 p1.b p2.b⟩
  pixelOpacity _ := opaqueF

instance : Pixel PixelYCbCr8 Pixel8 where
  zero := 0
  componentCount := 3
  toComponents p := #[p.y, p.cb, p.cr]
  fromComponents c := ⟨c.getD 0 0, c.getD 1 0, c.getD 2 0⟩
  mixWith f p1 p2 := ⟨f 0 p1.y p2.y, f 1 p1.cb p2.cb, f 2 p1.cr p2.cr⟩
  pixelOpacity _ := opaque8

instance : Pixel PixelRGBA8 Pixel8 where
  zero := 0
  componentCount := 4
  toComponents p := #[p.r, p.g, p.b, p.a]
  fromComponents c := ⟨c.getD 0 0, c.getD 1 0, c.getD 2 0, c.getD 3 0⟩
  mixWith f p1 p2 := ⟨f 0 p1.r p2.r, f 1 p1.g p2.g, f 2 p1.b p2.b, f 3 p1.a p2.a⟩
  mixWithAlpha f fa p1 p2 :=
    ⟨f 0 p1.r p2.r, f 1 p1.g p2.g, f 2 p1.b p2.b, fa p1.a p2.a⟩
  pixelOpacity p := p.a

instance : Pixel PixelRGBA16 Pixel16 where
  zero := 0
  componentCount := 4
  toComponents p := #[p.r, p.g, p.b, p.a]
  fromComponents c := ⟨c.getD 0 0, c.getD 1 0, c.getD 2 0, c.getD 3 0⟩
  mixWith f p1 p2 := ⟨f 0 p1.r p2.r, f 1 p1.g p2.g, f 2 p1.b p2.b, f 3 p1.a p2.a⟩
  mixWithAlpha f fa p1 p2 :=
    ⟨f 0 p1.r p2.r, f 1 p1.g p2.g, f 2 p1.b p2.b, fa p1.a p2.a⟩
  pixelOpacity p := p.a

instance : Pixel PixelCMYK8 Pixel8 where
  zero := 0
  componentCount := 4
  toComponents p := #[p.c, p.m, p.y, p.k]
  fromComponents c := ⟨c.getD 0 0, c.getD 1 0, c.getD 2 0, c.getD 3 0⟩
  mixWith f p1 p2 := ⟨f 0 p1.c p2.c, f 1 p1.m p2.m, f 2 p1.y p2.y, f 3 p1.k p2.k⟩
  pixelOpacity _ := opaque8

instance : Pixel PixelCMYK16 Pixel16 where
  zero := 0
  componentCount := 4
  toComponents p := #[p.c, p.m, p.y, p.k]
  fromComponents c := ⟨c.getD 0 0, c.getD 1 0, c.getD 2 0, c.getD 3 0⟩
  mixWith f p1 p2 := ⟨f 0 p1.c p2.c, f 1 p1.m p2.m, f 2 p1.y p2.y, f 3 p1.k p2.k⟩
  pixelOpacity _ := opaque16

instance : Pixel PixelYCbCrK8 Pixel8 where
  zero := 0
  componentCount := 4
  toComponents p := #[p.y, p.cb, p.cr, p.k]
  fromComponents c := ⟨c.getD 0 0, c.getD 1 0, c.getD 2 0, c.getD 3 0⟩
  mixWith f p1 p2 :=
    ⟨f 0 p1.y p2.y, f 1 p1.cb p2.cb, f 2 p1.cr p2.cr, f 3 p1.k p2.k⟩
  pixelOpacity _ := opaque8

instance : TransparentPixel PixelYA8 Pixel8 where
  setOpacity a p := { p with a }
  dropAlphaLayer p := p.y

instance : TransparentPixel PixelYA16 Pixel16 where
  setOpacity a p := { p with a }
  dropAlphaLayer p := p.y

instance : TransparentPixel PixelRGBA8 PixelRGB8 where
  setOpacity a p := { p with a }
  dropAlphaLayer p := ⟨p.r, p.g, p.b⟩

instance : TransparentPixel PixelRGBA16 PixelRGB16 where
  setOpacity a p := { p with a }
  dropAlphaLayer p := ⟨p.r, p.g, p.b⟩

-- ── `ColorConvertible` / `ColorSpaceConvertible` ──

/-- A pixel type that can be losslessly widened into another (e.g. grey to
    RGB, or 8-bit to 16-bit). -/
class ColorConvertible (α β : Type) where
  promotePixel : α → β

/-- A pixel type that can be converted into another, possibly with loss (a
    colour-space change, e.g. RGB to YCbCr or CMYK). -/
class ColorSpaceConvertible (α β : Type) where
  convertPixel : α → β

instance : ColorConvertible Pixel8 Pixel16 where
  promotePixel p := p.toUInt16 * 257

instance : ColorConvertible Pixel8 Pixel32 where
  promotePixel p := p.toUInt32 * 16843009

instance : ColorConvertible Pixel8 PixelF where
  promotePixel p := p.toFloat32 / 255.0

instance : ColorConvertible Pixel8 PixelYA8 where
  promotePixel p := ⟨p, opaque8⟩

instance : ColorConvertible Pixel8 PixelRGB8 where
  promotePixel p := ⟨p, p, p⟩

instance : ColorConvertible Pixel8 PixelRGBA8 where
  promotePixel p := ⟨p, p, p, opaque8⟩

instance : ColorConvertible PixelYA8 PixelRGB8 where
  promotePixel p := ⟨p.y, p.y, p.y⟩

instance : ColorConvertible PixelYA8 PixelRGBA8 where
  promotePixel p := ⟨p.y, p.y, p.y, p.a⟩

instance : ColorConvertible PixelRGB8 PixelRGBA8 where
  promotePixel p := ⟨p.r, p.g, p.b, opaque8⟩

instance : ColorConvertible PixelRGB8 PixelRGB16 where
  promotePixel p := ⟨(ColorConvertible.promotePixel p.r : Pixel16),
    ColorConvertible.promotePixel p.g, ColorConvertible.promotePixel p.b⟩

instance : ColorConvertible PixelRGBA8 PixelRGBA16 where
  promotePixel p := ⟨(ColorConvertible.promotePixel p.r : Pixel16),
    ColorConvertible.promotePixel p.g, ColorConvertible.promotePixel p.b,
    ColorConvertible.promotePixel p.a⟩

/-- $Y' = 0.299 R' + 0.587 G' + 0.114 B'$, the ITU-R BT.601 luma weights
    JPEG uses to convert RGB to YCbCr. -/
instance : ColorSpaceConvertible PixelRGB8 PixelYCbCr8 where
  convertPixel p :=
    let r := p.r.toFloat32
    let g := p.g.toFloat32
    let b := p.b.toFloat32
    let y := 0.299 * r + 0.587 * g + 0.114 * b
    let cb := 128.0 - 0.168736 * r - 0.331264 * g + 0.5 * b
    let cr := 128.0 + 0.5 * r - 0.418688 * g - 0.081312 * b
    ⟨y.toUInt8, cb.toUInt8, cr.toUInt8⟩

instance : ColorSpaceConvertible PixelYCbCr8 PixelRGB8 where
  convertPixel p :=
    let y := p.y.toFloat32
    let cb := p.cb.toFloat32 - 128.0
    let cr := p.cr.toFloat32 - 128.0
    let r := y + 1.402 * cr
    let g := y - 0.344136 * cb - 0.714136 * cr
    let b := y + 1.772 * cb
    ⟨r.toUInt8, g.toUInt8, b.toUInt8⟩

instance : ColorSpaceConvertible PixelCMYK8 PixelRGB8 where
  convertPixel p :=
    let conv (x k : Pixel8) : Pixel8 :=
      (255 - x).toUInt32 * (255 - k).toUInt32 / 255 |>.toUInt8 |> (255 - ·)
    ⟨conv p.c p.k, conv p.m p.k, conv p.y p.k⟩

instance : ColorSpaceConvertible PixelRGB8 PixelCMYK8 where
  convertPixel p :=
    let k := min (255 - p.r) (min (255 - p.g) (255 - p.b))
    let conv (x : Pixel8) : Pixel8 :=
      if k == 255 then 0
      else ((255 - x).toUInt32 - k.toUInt32) * 255 / (255 - k).toUInt32 |>.toUInt8
    ⟨conv p.r, conv p.g, conv p.b, k⟩

-- ── `ColorPlane` (named accessors into a pixel's components) ──

/-- A tag identifying one named component ("plane") of a pixel type, e.g.
    `PlaneRed` for `PixelRGB8.r`. Each `Pixel` type that has such a plane
    provides a `ColorPlane` instance mapping the tag to its `0`-based
    component index. -/
class ColorPlane (α plane : Type) where
  toComponentIndex : Nat

inductive PlaneRed
inductive PlaneGreen
inductive PlaneBlue
inductive PlaneAlpha
inductive PlaneLuma
inductive PlaneCr
inductive PlaneCb
inductive PlaneCyan
inductive PlaneMagenta
inductive PlaneYellow
inductive PlaneBlack

instance : ColorPlane PixelRGB8 PlaneRed where toComponentIndex := 0
instance : ColorPlane PixelRGB8 PlaneGreen where toComponentIndex := 1
instance : ColorPlane PixelRGB8 PlaneBlue where toComponentIndex := 2
instance : ColorPlane PixelRGB16 PlaneRed where toComponentIndex := 0
instance : ColorPlane PixelRGB16 PlaneGreen where toComponentIndex := 1
instance : ColorPlane PixelRGB16 PlaneBlue where toComponentIndex := 2
instance : ColorPlane PixelRGBA8 PlaneRed where toComponentIndex := 0
instance : ColorPlane PixelRGBA8 PlaneGreen where toComponentIndex := 1
instance : ColorPlane PixelRGBA8 PlaneBlue where toComponentIndex := 2
instance : ColorPlane PixelRGBA8 PlaneAlpha where toComponentIndex := 3
instance : ColorPlane PixelRGBA16 PlaneRed where toComponentIndex := 0
instance : ColorPlane PixelRGBA16 PlaneGreen where toComponentIndex := 1
instance : ColorPlane PixelRGBA16 PlaneBlue where toComponentIndex := 2
instance : ColorPlane PixelRGBA16 PlaneAlpha where toComponentIndex := 3
instance : ColorPlane PixelYA8 PlaneLuma where toComponentIndex := 0
instance : ColorPlane PixelYA8 PlaneAlpha where toComponentIndex := 1
instance : ColorPlane PixelYA16 PlaneLuma where toComponentIndex := 0
instance : ColorPlane PixelYA16 PlaneAlpha where toComponentIndex := 1
instance : ColorPlane PixelYCbCr8 PlaneLuma where toComponentIndex := 0
instance : ColorPlane PixelYCbCr8 PlaneCb where toComponentIndex := 1
instance : ColorPlane PixelYCbCr8 PlaneCr where toComponentIndex := 2
instance : ColorPlane PixelCMYK8 PlaneCyan where toComponentIndex := 0
instance : ColorPlane PixelCMYK8 PlaneMagenta where toComponentIndex := 1
instance : ColorPlane PixelCMYK8 PlaneYellow where toComponentIndex := 2
instance : ColorPlane PixelCMYK8 PlaneBlack where toComponentIndex := 3
instance : ColorPlane PixelCMYK16 PlaneCyan where toComponentIndex := 0
instance : ColorPlane PixelCMYK16 PlaneMagenta where toComponentIndex := 1
instance : ColorPlane PixelCMYK16 PlaneYellow where toComponentIndex := 2
instance : ColorPlane PixelCMYK16 PlaneBlack where toComponentIndex := 3

/-- A pixel type from which a perceptual luma (greyscale) value can be
    computed directly, without a full colour-space conversion. -/
class LumaPlaneExtractable (α : Type) {Component : outParam Type} [Pixel α Component] where
  computeLuma : α → Component

instance : LumaPlaneExtractable Pixel8 where computeLuma p := p
instance : LumaPlaneExtractable Pixel16 where computeLuma p := p
instance : LumaPlaneExtractable Pixel32 where computeLuma p := p
instance : LumaPlaneExtractable PixelF where computeLuma p := p

instance : LumaPlaneExtractable PixelRGBF where
  computeLuma p := 0.3 * p.r + 0.59 * p.g + 0.11 * p.b

instance : LumaPlaneExtractable PixelRGBA8 where
  computeLuma p :=
    ((p.r.toUInt32 * 6969 + p.g.toUInt32 * 23434 + p.b.toUInt32 * 2365) / 32768).toUInt8

instance : LumaPlaneExtractable PixelYCbCr8 where
  computeLuma p := p.y

-- ── `Image` ──

/-- A rectangular, row-major image whose pixels are stored as a single flat
    array of components (an `Image` collapses upstream's separate
    persistent/mutable image types — see the module doc-comment). -/
structure Image (α : Type) {Component : outParam Type} [Pixel α Component] where
  width : Nat
  height : Nat
  /-- `componentCount α * width * height` components, row-major. -/
  data : Array Component

/-- The number of components between the start of one pixel and the next,
    i.e. the row stride measured in components. -/
def Image.stride [Pixel α Component] (img : @Image α Component _) : Nat :=
  Pixel.componentCount α * img.width

/-- Read the pixel at `(x, y)`, `0`-indexed from the top-left. Out-of-range
    coordinates read as `Pixel.zero`-filled components. -/
def Image.getPixel [Pixel α Component] (img : @Image α Component _) (x y : Nat) : α :=
  let n := Pixel.componentCount α
  let base := n * (y * img.width + x)
  Pixel.fromComponents (Array.ofFn (n := n) fun i => img.data.getD (base + i) (Pixel.zero (α := α)))

/-- Replace the pixel at `(x, y)` with `p`, returning the updated image.
    Out-of-range coordinates leave the image unchanged. -/
def Image.setPixel [Pixel α Component] (img : @Image α Component _) (x y : Nat) (p : α) :
    @Image α Component _ :=
  let n := Pixel.componentCount α
  let base := n * (y * img.width + x)
  let comps := Pixel.toComponents p
  { img with
    data := (List.range n).foldl (fun d i => d.set! (base + i) (comps.getD i (Pixel.zero (α := α))))
      img.data }

/-- Build an image by computing each pixel from its coordinates. -/
def generateImage [Pixel α Component] (f : Nat → Nat → α) (width height : Nat) :
    @Image α Component _ :=
  let n := Pixel.componentCount α
  Id.run do
    let mut data := Array.mkEmpty (n * width * height)
    for y in [0:height] do
      for x in [0:width] do
        data := data ++ Pixel.toComponents (f x y)
    pure { width, height, data }

/-- Apply `f` to every pixel of an image. -/
def pixelMap [Pixel α CA] [Pixel β CB] (f : α → β) (img : @Image α CA _) : @Image β CB _ :=
  generateImage (fun x y => f (img.getPixel x y)) img.width img.height

/-- Apply `f` to every pixel of an image, also given its coordinates. -/
def pixelMapXY [Pixel α CA] [Pixel β CB] (f : Nat → Nat → α → β) (img : @Image α CA _) :
    @Image β CB _ :=
  generateImage (fun x y => f x y (img.getPixel x y)) img.width img.height

/-- Fold over every pixel of an image, row-major, top-to-bottom. -/
def pixelFold [Pixel α Component] (f : β → Nat → Nat → α → β) (init : β)
    (img : @Image α Component _) : β :=
  Id.run do
    let mut acc := init
    for y in [0:img.height] do
      for x in [0:img.width] do
        acc := f acc x y (img.getPixel x y)
    pure acc

/-- Extract a single named component ("plane") from every pixel of an
    image, producing a single-component image of the same size (e.g.
    `extractComponent (plane := PlaneRed) rgbImage`). -/
def extractComponent [Pixel α Component] [Pixel Component Component]
    [ColorPlane α plane] (img : @Image α Component _) : @Image Component Component _ :=
  let idx := ColorPlane.toComponentIndex (α := α) (plane := plane)
  generateImage
    (fun x y => (Pixel.toComponents (img.getPixel x y)).getD idx (Pixel.zero (α := Component)))
    img.width img.height

/-- A simple RGB8 palette: `palette.getPixel i 0` is the RGB colour for index
    `i`. -/
abbrev Palette := Image PixelRGB8

-- ── `DynamicImage` and paletted images ──

/-- A concrete pixel type, tagged so a decoder can return whichever format
    an input image actually used without the caller knowing it up front. -/
inductive DynamicImage where
  | y8 (img : Image Pixel8)
  | y16 (img : Image Pixel16)
  | y32 (img : Image Pixel32)
  | yF (img : Image PixelF)
  | ya8 (img : Image PixelYA8)
  | ya16 (img : Image PixelYA16)
  | rgb8 (img : Image PixelRGB8)
  | rgb16 (img : Image PixelRGB16)
  | rgbF (img : Image PixelRGBF)
  | rgba8 (img : Image PixelRGBA8)
  | rgba16 (img : Image PixelRGBA16)
  | ycbcr8 (img : Image PixelYCbCr8)
  | cmyk8 (img : Image PixelCMYK8)
  | cmyk16 (img : Image PixelCMYK16)

/-- Apply a polymorphic function to whichever concrete image a
    `DynamicImage` holds, without unwrapping the tag yourself. -/
def dynamicMap {β : Type} (f : {α Component : Type} → [Pixel α Component] → @Image α Component _ → β) :
    DynamicImage → β
  | .y8 img => f img
  | .y16 img => f img
  | .y32 img => f img
  | .yF img => f img
  | .ya8 img => f img
  | .ya16 img => f img
  | .rgb8 img => f img
  | .rgb16 img => f img
  | .rgbF img => f img
  | .rgba8 img => f img
  | .rgba16 img => f img
  | .ycbcr8 img => f img
  | .cmyk8 img => f img
  | .cmyk16 img => f img

/-- A colour-mapped ("indexed") image: an RGB8 palette plus an image of
    palette indices (matching upstream's `Palette'`, minus the `Metadata`
    payload — carried forward once `Codec.Picture.Metadata` is ported,
    module 5). -/
structure PalettedImage where
  /-- The indexed image itself; each pixel is a palette index. -/
  indexedImage : Image Pixel8
  /-- `palette.getPixel i 0` is the RGB colour for index `i`. -/
  palette : Image PixelRGB8
  /-- Whether the palette carries per-entry transparency. -/
  hasAlpha : Bool

/-- Expand a paletted image into a true-colour `PixelRGB8` image. -/
def palettedToTrueColor (img : PalettedImage) : Image PixelRGB8 :=
  generateImage (fun x y => img.palette.getPixel (img.indexedImage.getPixel x y).toNat 0)
    img.indexedImage.width img.indexedImage.height

end Codec.Picture
