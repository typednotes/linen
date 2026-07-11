# `JuicyPixels` — dependency plan

Upstream: https://hackage.haskell.org/package/JuicyPixels (version 3.3.9)

`JuicyPixels` is a large codec suite (~14,300 lines across 30 modules):
pixel/image types and colorspace conversions, plus independent encoders/
decoders for PNG, JPEG (baseline + progressive), GIF, BMP, TGA, TIFF, and
Radiance HDR. Ported in the topological order below, one module (or small
cluster of mutually-dependent modules) at a time, each with its own
`Tests/` counterpart and its own `lake build` before moving to the next.

Namespace note: "Codec" and "Picture" name a general subject area, not
Haskell/GHC itself, so no Lean-ification rename is needed (same reasoning
as `Graphics.Netpbm`) — the package is ported as `Linen.Codec.Picture.*`.

## Module list (topological order)

Foundational (pixel/image core, no dependency on any format codec):

1. `Codec.Picture.Types` → `Linen.Codec.Picture.Types` — `Image`/
   `MutableImage`/`DynamicImage`, the `Pixel`/`ColorConvertible`/
   `ColorSpaceConvertible`/`ColorPlane`/`LumaPlaneExtractable`/
   `TransparentPixel` classes, and all concrete pixel types (`PixelYA8`,
   `PixelRGB8`, `PixelYCbCr8`, `PixelCMYK8`, ... and their 16-bit/float
   variants).
2. `Codec.Picture.VectorByteConversion` → `Linen.Codec.Picture.VectorByteConversion`
   — `Vector Word8` ↔ `ByteString` reinterpretation; ported as `Array UInt8`
   ↔ `ByteArray` conversion (a straight copy, since Lean's `Array`/
   `ByteArray` share no storage-layout trick to exploit as the unsafe
   pointer cast upstream does — see Scope and simplifications).
3. `Codec.Picture.InternalHelper` → `Linen.Codec.Picture.InternalHelper` —
   small `ByteString`/file-loading helpers.
4. `Codec.Picture.BitWriter` → `Linen.Codec.Picture.BitWriter` — MSB-first
   bit-level reader/writer built on `Types`, used by both the JPEG and PNG
   (and GIF LZW) codecs.
5. `Codec.Picture.Metadata.Exif` → `Linen.Codec.Picture.Metadata.Exif` — Exif
   tag vocabulary and TIFF-embedded Exif directory parsing; no dependency on
   `Metadata` (upstream's `Metadata.hs` in fact imports `Metadata.Exif` for
   its `ExifTag`/`ExifData` types, the reverse of what this list originally
   said, so `Exif` is ported first).
6. `Codec.Picture.Metadata` → `Linen.Codec.Picture.Metadata` — the generic
   `Metadatas`/`Keys`/`Elem` map attached to decoded images, on `Metadata.Exif`.
7. `Codec.Picture.ColorQuant` → `Linen.Codec.Picture.ColorQuant` — median-cut
   color quantization (GIF/PNG palette generation), on `Types`.

Format codecs (each independent of the others; only depends on the
foundational group above, plus `Linen.Data.Compression.Zlib` for PNG/TIFF):

8. `Codec.Picture.Bitmap` → `Linen.Codec.Picture.Bitmap` — BMP.
9. `Codec.Picture.Tga` → `Linen.Codec.Picture.Tga` — TGA.
10. `Codec.Picture.HDR` → `Linen.Codec.Picture.HDR` — Radiance HDR.
11. `Codec.Picture.Png.Internal.Type` → `Linen.Codec.Picture.Png.Internal.Type`
    — PNG chunk structure, `ChunkSignature`, filter types.
12. `Codec.Picture.Png.Internal.Metadata` →
    `Linen.Codec.Picture.Png.Internal.Metadata` — PNG ancillary-chunk ↔
    `Metadata` conversion, on #11 and #5.
13. `Codec.Picture.Png.Internal.Export` →
    `Linen.Codec.Picture.Png.Internal.Export` — image → PNG chunk encoding,
    on #11.
14. `Codec.Picture.Png` → `Linen.Codec.Picture.Png` — top-level PNG
    decode/encode, on #11–#13 plus zlib inflate/deflate.
15. `Codec.Picture.Tiff.Internal.Types` →
    `Linen.Codec.Picture.Tiff.Internal.Types` — TIFF IFD/tag structure.
16. `Codec.Picture.Tiff.Internal.Metadata` →
    `Linen.Codec.Picture.Tiff.Internal.Metadata` — TIFF tag ↔ `Metadata`
    conversion, on #15, #6.
17. `Codec.Picture.Tiff` → `Linen.Codec.Picture.Tiff` — top-level TIFF
    decode/encode, on #15–#16.
18. `Codec.Picture.Gif.Internal.LZW` → `Linen.Codec.Picture.Gif.Internal.LZW`
    — GIF LZW decompression.
19. `Codec.Picture.Gif.Internal.LZWEncoding` →
    `Linen.Codec.Picture.Gif.Internal.LZWEncoding` — GIF LZW compression.
20. `Codec.Picture.Gif` → `Linen.Codec.Picture.Gif` — top-level GIF
    decode/encode, on #18–#19, #7 (palette quantization on encode).
21. `Codec.Picture.Jpg.Internal.DefaultTable` →
    `Linen.Codec.Picture.Jpg.Internal.DefaultTable` — standard JPEG
    quantization/Huffman tables (data only, no other module dependency).
22. `Codec.Picture.Jpg.Internal.Types` →
    `Linen.Codec.Picture.Jpg.Internal.Types` — JPEG marker/segment/scan
    structure, on `Types`, `BitWriter`.
23. `Codec.Picture.Jpg.Internal.Common` →
    `Linen.Codec.Picture.Jpg.Internal.Common` — shared decode helpers, on
    #22.
24. `Codec.Picture.Jpg.Internal.FastDct` / `Codec.Picture.Jpg.Internal.FastIdct`
    → `Linen.Codec.Picture.Jpg.Internal.FastDct` /
    `Linen.Codec.Picture.Jpg.Internal.FastIdct` — integer DCT/IDCT, on
    `Types` only.
25. `Codec.Picture.Jpg.Internal.Metadata` →
    `Linen.Codec.Picture.Jpg.Internal.Metadata` — JFIF/Exif ↔ `Metadata`,
    on #5, #6.
26. `Codec.Picture.Jpg.Internal.Progressive` →
    `Linen.Codec.Picture.Jpg.Internal.Progressive` — progressive-scan JPEG
    decoding, on #21–#24.
27. `Codec.Picture.Jpg` → `Linen.Codec.Picture.Jpg` — top-level JPEG
    decode/encode, on #21–#26.

Facades (depend on every format codec above):

28. `Codec.Picture.Saving` → `Linen.Codec.Picture.Saving` — format-agnostic
    "save with extension" dispatch, on #8, #10, #14, #17, #20, #27.
29. `Codec.Picture` → `Linen.Codec.Picture` — the package's public
    re-export facade, on all of the above.

## External dependencies

Checked against the Hackage-import precedence rule in `AGENTS.md` before
porting anything:

- `bytestring`, `vector`, `primitive`, `containers`, `mtl`, `transformers` —
  already covered, same as `netpbm`/`hip` (see the "covered by the Lean
  stdlib or an existing port" list in `docs/imports/index.md`).
- `zlib` — already ported as `Linen.Data.Compression.Zlib`; PNG/TIFF's
  deflate-compressed streams are decoded/encoded through it directly, no
  fresh Hackage import needed.
- `binary` — `Std.Internal.Parsec` / `Std.Internal.Parsec.ByteArray`, same
  substitution as `netpbm`.
- `deepseq` — dropped; see `docs/imports/index.md`'s existing note (no
  equivalent notion in an eager language).

## Scope and simplifications

- `Codec.Picture.ConvGraph` is not a real module — it is a doc-comment-only
  file referenced via `Install-Includes` purely to embed a graphviz diagram
  of pixel-conversion functions in the generated Haddocks. It carries no
  code and is dropped.
- `Codec.Picture.VectorByteConversion`'s upstream implementation reinterprets
  a `Vector Word8`'s backing pointer as a `ByteString` (and back) via
  `unsafeCoerce`-style pointer casts, to avoid a copy. Lean's `Array UInt8`
  and `ByteArray` are genuinely different representations with no such
  aliasing available (nor would it be safe to fabricate), so the Lean port
  does an explicit element-wise copy — a representation-level implementation
  detail, not an observable behavior change (the upstream module's only
  contract is "the bytes read back equal the bytes written in").
- As with `netpbm`, `Storable`/`NFData` instances (present throughout
  `Types`, `MutableImage`, `DynamicImage`) are GHC FFI/strictness machinery
  dropped entirely; laziness-forcing (`deepseq`) has no meaning for Lean's
  eager evaluation.
- Any further simplification uncovered while porting a specific module will
  be documented in that module's own doc-comment, following the pattern
  already established in `Linen/Graphics/Netpbm.lean` and the `repa` port.
