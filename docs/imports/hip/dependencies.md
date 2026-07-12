# `hip` — dependency plan

Upstream: https://hackage.haskell.org/package/hip (version 1.5.6.0, the
latest published), source at https://github.com/lehins/hip.

**Scope note:** per the decision recorded in `docs/imports/index.md`,
`Graphics.Image.IO.Histogram` is excluded, and with it the
`Chart`/`Chart-diagrams` dependency it alone pulls in. This is not an
improvised cut: upstream's own `.cabal` already gates exactly this module
behind a `disable-chart` flag (`if !flag(disable-chart) Exposed-Modules:
Graphics.Image.IO.Histogram`) for users who want a lighter dependency
footprint — this port simply builds as if that flag were set. No other
module imports `Graphics.Image.IO.Histogram`, so dropping it changes nothing
about the rest of the topological order below.

An edge **A → B** means *module A imports module B*, so **B must be built
before A**. Derived from each module's own `import Graphics.Image....`
lines (checked directly against the tarball source, not guessed from
Haddock re-export lists).

Namespace note: "Graphics.Image" names a general subject area (image
processing), not Haskell/GHC itself, so no Lean-ification rename is
needed — ported as `Linen.Graphics.Image.*`, mirroring `Linen.Graphics.Netpbm`
and `Linen.Codec.Picture.*`.

## Precedence check (`AGENTS.md`'s Hackage-import precedence rule)

Before listing hip's own modules as new work, each was checked against: (1)
the Lean stdlib, (2) what `linen` already ported from `colour`, `repa`,
`netpbm`, and `JuicyPixels`, in that order.

- **`repa`'s multi-representation layer is already collapsed to one
  `Manifest` representation** in `Linen.Data.Array.Shaped.Repr.Manifest`
  (see `docs/imports/repa/dependencies.md`'s scope note: unboxed/`ForeignPtr`/
  boxed-vector reprs `U`/`F`/`V` collapse into `Manifest`, `HintSmall`/
  `HintInterleave` are dropped as no-ops with no worker-gang to schedule
  against). hip's own representation-selection layer
  (`Graphics.Image.Interface.Vector.{Generic,Unboxing,Storable,Unboxed}`,
  `Graphics.Image.Interface.Vector`, `Graphics.Image.Interface.Repa.{Generic,
  Storable,Unboxed}`, `Graphics.Image.Interface.Repa`) exists **purely** to
  let a user pick which of GHC's `Data.Vector.Storable` / `Data.Vector.Unboxed`
  / `repa` backing stores an `Image` uses — the same representation-choice
  problem `repa` itself had, already solved the same way. These 8 modules
  therefore need **no separate port**: every concrete image type in the
  eventual `Linen.Graphics.Image.*` port is backed directly by
  `Linen.Data.Array.Shaped` (reusing `Manifest`) or, where a flat 1-D pixel
  buffer suffices, `Linen.Data.Vector` — whichever the consuming module
  (`Types`, `IO.Formats.*`) needs, with no `VS`/`RS`/`RU` choice exposed.
- **`JuicyPixels` and `netpbm` are already ported** as `Linen.Codec.Picture.*`
  and `Linen.Graphics.Netpbm` respectively. `Graphics.Image.IO.Formats.
  JuicyPixels` and `Graphics.Image.IO.Formats.Netpbm` import exactly those
  two packages (`Codec.Picture`/`Codec.Picture.ColorQuant`/`Codec.Picture.Gif`/
  `Codec.Picture.Jpg` and `Graphics.Netpbm`) to do the actual file decode/
  encode — the eventual port of these two glue modules reuses
  `Linen.Codec.Picture.*` / `Linen.Graphics.Netpbm` for that work rather than
  re-porting any codec logic; only the (de)marshalling between hip's `Image`
  type and the already-ported pixel/image types is new.
- **`colour` is already ported** as `Linen.Data.Colour.*`, but checking each
  of hip's own `Graphics.Image.ColorSpace.*` modules' imports directly
  against the tarball source shows **none of them import `Data.Colour`** —
  hip's `ColorSpace`/`Pixel` classes (in `Graphics.Image.Interface`) are a
  separate, `Elevator`-component-polymorphic abstraction bespoke to hip
  (arity-1..4 channel tuples over a class-constrained component type `e`),
  unrelated to `colour`'s device-independent `Colour`/`AlphaColour` types or
  to `JuicyPixels`' fixed 8-/16-bit/float `PixelRGB8`-style types. So these
  are genuinely new work, not a reuse — `colour` remains a prerequisite of
  `hip` only in the loose sense of "an earlier step in the same
  image-processing dependency chain" (per `docs/imports/index.md`), not
  because any hip module actually imports it.
- **Lean stdlib**: `array`, `base`, `bytestring`, `containers` (n/a, hip
  itself doesn't use it), `deepseq`, `directory`, `filepath`, `primitive`,
  `process`, `random`, `temporary`, `vector` — all already substituted
  exactly as documented in `docs/imports/index.md`'s "`hip` dependencies
  covered by the Lean stdlib or an existing port" list (this hip import is
  what that list was written in anticipation of).

## Module list (topological order)

Foundational (component/pixel/array core, no dependency on any other hip
module):

1. `Graphics.Image.Utils` → `Linen.Graphics.Image.Utils` — **new port.**
   Small numeric/list helpers with no `Graphics.Image.*` dependency.
2. `Graphics.Image.Interface.Elevator` → `Linen.Graphics.Image.Interface.Elevator`
   — **new port.** The `Elevator` class (precision-changing conversions
   between pixel component types, e.g. `Double ↔ Word8`, with range
   scaling) and `clamp01`. No existing `linen` module provides this —
   `Codec.Picture.Types`'s pixel-conversion machinery (`ColorConvertible`)
   converts between fixed *pixel* types, not generically between *component*
   types, so it doesn't cover this.
3. `Graphics.Image.Interface` → `Linen.Graphics.Image.Interface` — **new
   port**, on #2. The central `Pixel`/`ColorSpace`/`AlphaSpace`/`BaseArray`/
   `Array`/`MArray` classes tying a pixel type and a component type to an
   image's array representation, plus generic indexing (`index`,
   `borderIndex`, `Border`, …). Backed directly by `Linen.Data.Array.Shaped`
   (reusing `Manifest`) rather than a fresh representation-polymorphic
   layer — see the precedence-check note above.

`Interface.Vector.{Generic,Unboxing,Storable,Unboxed}`,
`Interface.Vector`, `Interface.Repa.{Generic,Storable,Unboxed}`,
`Interface.Repa` (8 modules) — **reuse existing `Linen.Data.Array.Shaped` /
`Linen.Data.Vector`, no separate port.** See precedence-check note above.

Color spaces (each depends only on `Interface`, plus `Utils` for `X`):

4. `Graphics.Image.ColorSpace.Y` → `Linen.Graphics.Image.ColorSpace.Y` —
   **new port.** Single-channel luma color space.
5. `Graphics.Image.ColorSpace.RGB` → `Linen.Graphics.Image.ColorSpace.RGB` —
   **new port.** 3-channel RGB.
6. `Graphics.Image.ColorSpace.HSI` → `Linen.Graphics.Image.ColorSpace.HSI` —
   **new port.** 3-channel hue/saturation/intensity.
7. `Graphics.Image.ColorSpace.CMYK` → `Linen.Graphics.Image.ColorSpace.CMYK`
   — **new port.** 4-channel cyan/magenta/yellow/black.
8. `Graphics.Image.ColorSpace.YCbCr` → `Linen.Graphics.Image.ColorSpace.YCbCr`
   — **new port.** 3-channel luma/chroma.
9. `Graphics.Image.ColorSpace.Complex` → `Linen.Graphics.Image.ColorSpace.Complex`
   — **new port.** Complex-valued pixels (on `Data.Complex`, already Lean
   stdlib-covered via a plain structure), used by the FFT processing
   modules.
10. `Graphics.Image.ColorSpace.X` → `Linen.Graphics.Image.ColorSpace.X` —
    **new port**, on #1. Single "extra channel" color space used as the
    binary-image pixel type's carrier.
11. `Graphics.Image.ColorSpace.Binary` → `Linen.Graphics.Image.ColorSpace.Binary`
    — **new port**, on #10. Bit-valued (`Bool`-backed) binary pixels for
    thresholding/morphology.
12. `Graphics.Image.ColorSpace` → `Linen.Graphics.Image.ColorSpace` —
    **new port**, on #3–#11 (facade re-exporting every color space plus
    `Interface.Elevator`).

Processing (each independent of the others except where noted; all on
`Interface`, most also on `ColorSpace`):

13. `Graphics.Image.Processing.Interpolation` →
    `Linen.Graphics.Image.Processing.Interpolation` — **new port**, on #3.
    Nearest-neighbour/bilinear pixel interpolation.
14. `Graphics.Image.Processing.Geometric` →
    `Linen.Graphics.Image.Processing.Geometric` — **new port**, on #13.
    Rotation/scaling/translation/cropping.
15. `Graphics.Image.Processing.Complex.Fourier` →
    `Linen.Graphics.Image.Processing.Complex.Fourier` — **new port**, on #9,
    #14. FFT/inverse-FFT.
16. `Graphics.Image.Processing.Complex` →
    `Linen.Graphics.Image.Processing.Complex` — **new port**, on #9, #15.
    Complex-image conversions built on the FFT.
17. `Graphics.Image.Processing.Convolution` →
    `Linen.Graphics.Image.Processing.Convolution` — **new port**, on #1,
    #12, #14. Kernel convolution/correlation.
18. `Graphics.Image.Processing.Filter` →
    `Linen.Graphics.Image.Processing.Filter` — **new port**, on #12, #17.
    Named filter kernels (Sobel, Gaussian, Laplacian, …) built on
    convolution.
19. `Graphics.Image.Processing.Binary` →
    `Linen.Graphics.Image.Processing.Binary` — **new port**, on #1, #12,
    #17. Binary-image morphology (erode/dilate/open/close) on `ColorSpace.
    Binary`.
20. `Graphics.Image.Processing` → `Linen.Graphics.Image.Processing` —
    **new port**, on #3, #13, #14, #17, #18 (facade).

IO (bridges hip's `Image` type to the already-ported codec libraries):

21. `Graphics.Image.IO.Base` → `Linen.Graphics.Image.IO.Base` — **new port**,
    on #12, #16, #14. Common reader/writer types and the pixel-precision
    auto-conversion helpers shared by every format backend.
22. `Graphics.Image.IO.Formats.JuicyPixels` →
    `Linen.Graphics.Image.IO.Formats.JuicyPixels` — **new port** (glue
    only), on #12, #3, #21, plus **reuse `Linen.Codec.Picture.*`** for the
    actual PNG/JPEG/GIF/BMP/TIFF/TGA/HDR decode/encode. Only the
    hip-`Image` ↔ `Linen.Codec.Picture` pixel-array marshalling is new.
23. `Graphics.Image.IO.Formats.Netpbm` →
    `Linen.Graphics.Image.IO.Formats.Netpbm` — **new port** (glue only),
    on #12, #3, #21, plus **reuse `Linen.Graphics.Netpbm`** for the actual
    PNM/PGM/PPM decode. Only the hip-`Image` ↔ `Linen.Graphics.Netpbm`
    marshalling is new.
24. `Graphics.Image.IO.Formats` → `Linen.Graphics.Image.IO.Formats` — **new
    port**, on #3, #21–#23 (format-dispatch facade).
25. `Graphics.Image.IO` → `Linen.Graphics.Image.IO` — **new port**, on #12,
    #3, `Interface.Vector`(reused, see above), #21, #24. Top-level
    read/write/display entry points. `Graphics.Image.IO.Histogram` is
    excluded per the scope note above and is *not* part of this module's
    port (its one importer, dropped along with it).

Facades (depend on everything above):

26. `Graphics.Image.Types` → `Linen.Graphics.Image.Types` — **new port**,
    on #12, #3, `Interface.Vector`/`Interface.Repa` (reused), #24. Concrete
    type aliases for every (color space × precision × representation)
    combination hip exposes — collapses to (color space × precision)
    aliases only, since the representation axis no longer exists (see
    precedence-check note).
27. `Graphics.Image` → `Linen.Graphics.Image` — **new port**, on #12, #25,
    #3, #26, #20, #19, #16, #14. The package's public re-export facade.
    (Upstream also imports `Graphics.Image.IO.Histogram` here only under the
    `disable-chart`-off branch; with the flag effectively always "on" for
    this port, that import is simply absent.)

Processing modules that depend on the `Image` facade itself (must come
after #27):

28. `Graphics.Image.Processing.Ahe` → `Linen.Graphics.Image.Processing.Ahe`
    — **new port**, on #18, #3, #27, #26. Adaptive histogram equalization.
29. `Graphics.Image.Processing.Hough` →
    `Linen.Graphics.Image.Processing.Hough` — **new port**, on #27, #3,
    #26. Hough-transform line detection.
30. `Graphics.Image.Processing.Noise` →
    `Linen.Graphics.Image.Processing.Noise` — **new port**, on #3, #27,
    #26. Synthetic image noise generation.

## Excluded

- `Graphics.Image.IO.Histogram` — excluded per the scope note above (and
  with it, `Chart`/`Chart-diagrams`/the transitive `diagrams-lib`/
  `diagrams-svg`/`SVGFonts` 2D-vector-graphics EDSL). Decided with the user
  2026-07-11.

## Tally

- 30 modules in hip's own module tree (excluding `Graphics.Image.IO.
  Histogram`), of which:
  - **21 are genuinely new work** (#1, #2, #3, #4–#12, #13–#20, #21–#27
    minus the 8 reused-representation modules already excluded from the
    count, plus #28–#30) — see the numbered list above for the exact set.
  - **8 modules need no separate port**, fully covered by the already-
    ported `repa`: `Interface.Vector.{Generic,Unboxing,Storable,Unboxed}`,
    `Interface.Vector`, `Interface.Repa.{Generic,Storable,Unboxed}`,
    `Interface.Repa`.
  - Within the 21 new-work modules, 2 (`IO.Formats.JuicyPixels`,
    `IO.Formats.Netpbm`) are glue-only and reuse `Linen.Codec.Picture.*` /
    `Linen.Graphics.Netpbm` for all actual codec work.

## External dependencies

Checked against the Hackage-import precedence rule in `AGENTS.md` before
porting anything (see the precedence-check section above for the detailed
reasoning):

- `array`, `base`, `bytestring`, `deepseq`, `directory`, `filepath`,
  `primitive`, `process`, `random`, `temporary`, `vector` — already
  substituted, per `docs/imports/index.md`'s existing note.
- `colour` — already ported as `Linen.Data.Colour.*`, but not actually a
  dependency of any hip module by import (see precedence-check note); listed
  by `docs/imports/index.md` as a prerequisite in the loose "earlier in the
  same chain" sense only.
- `repa` — already ported as `Linen.Data.Array.Shaped.*`; hip's own
  representation-selection layer built on it collapses away entirely (see
  above).
- `JuicyPixels` — already ported as `Linen.Codec.Picture.*`; reused directly
  by `IO.Formats.JuicyPixels`.
- `netpbm` — already ported as `Linen.Graphics.Netpbm`; reused directly by
  `IO.Formats.Netpbm`.
- `Chart`, `Chart-diagrams` — dropped entirely, per the scope note (only
  reachable through the excluded `IO.Histogram`).

## Scope and simplifications

- As with `repa` and `netpbm`, `Storable`/`Unbox`/`NFData`/`Typeable`
  instances threaded throughout `Interface`, the `ColorSpace.*` pixel types,
  and `Types` are GHC FFI/strictness/reflection machinery with no Lean
  counterpart and are dropped entirely.
- Any further simplification uncovered while porting a specific module will
  be documented in that module's own doc-comment, following the pattern
  already established in `Linen/Graphics/Netpbm.lean`, the `repa` port, and
  `Linen/Codec/Picture/*.lean`.
