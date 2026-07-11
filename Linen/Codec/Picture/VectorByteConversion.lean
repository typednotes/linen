import Linen.Codec.Picture.Types

/-!
  Port of `Codec.Picture.VectorByteConversion` from the `JuicyPixels` package
  (see `docs/imports/JuicyPixels/dependencies.md`, module 2 of 29).

  Upstream reinterprets a `Vector Word8`'s backing pointer as a `ByteString`
  (`toByteString`/`blitVector`), and builds an `Image px` directly over a raw
  `ForeignPtr Word8` (`imageFromUnsafePtr`), all without copying. Lean's
  `Array UInt8` and `ByteArray` are genuinely different representations with
  no such pointer-aliasing trick available, so every conversion below is an
  explicit, linear-time copy instead — a representation-level difference,
  not an observable one (see `dependencies.md`'s "Scope and simplifications"
  for the full argument).
-/

namespace Codec.Picture

/-- Copy an array of byte components into a `ByteArray`. -/
def toByteArray (v : Array UInt8) : ByteArray :=
  v.foldl (fun acc b => acc.push b) ByteArray.empty

/-- Inverse of `toByteArray`. -/
def ofByteArray (b : ByteArray) : Array UInt8 :=
  Id.run do
    let mut out := Array.mkEmpty b.size
    for i in [0:b.size] do
      out := out.push (b.get! i)
    pure out

/-- Build an image directly from a flat buffer of raw bytes, `componentCount`
    per pixel, row-major — upstream's `imageFromUnsafePtr`, restricted (as
    upstream is, via `PixelBaseComponent px ~ Word8`) to pixel types whose
    component type is itself a byte. The caller is responsible for `bytes`
    holding exactly `width * height * componentCount α` bytes, same as
    upstream. -/
def imageFromByteArray [Pixel α Pixel8] (width height : Nat) (bytes : ByteArray) :
    @Image α Pixel8 _ :=
  { width, height, data := ofByteArray bytes }

end Codec.Picture
