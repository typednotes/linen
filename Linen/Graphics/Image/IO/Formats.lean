/-
  Linen.Graphics.Image.IO.Formats — the format-dispatch facade tying
  together every JuicyPixels-backed (#22) and Netpbm-backed (#23) format tag

  ## Haskell equivalent

  `Graphics.Image.IO.Formats` from https://hackage.haskell.org/package/hip
  (module #24 of the `hip` import plan, see `docs/imports/hip/dependencies.md`).
  Fetched from the 1.5.6.0 release tarball
  (`raw.githubusercontent.com/lehins/hip/master/…` 404s, same as every other
  module in this sub-tree's own note); the full source is 146 lines, read in
  full.

  ## Re-export strategy

  As with `Linen.Graphics.Image.ColorSpace`'s own doc-comment: Lean's
  `import` is already transitive, so a plain `import
  Linen.Graphics.Image.IO.Formats` (below) makes every declaration from
  `IO.Formats.JuicyPixels` (#22) and `IO.Formats.Netpbm` (#23) — `BMP`,
  `GIF`, `HDR`, `JPG`, `PNG`, `TGA`, `TIF`, `PBM`, `PGM`, `PPM` and every
  `Readable`/`Writable` instance on them — reachable at its fully-qualified
  name, with no explicit re-export step needed, unlike upstream's `module
  Graphics.Image.IO.Formats (module Graphics.Image.IO.Formats.JuicyPixels,
  module Graphics.Image.IO.Formats.Netpbm, …)` export list. This file's
  `open` statements below exist only so *this file's own* definitions can
  refer to `BMP`/`ext`/etc. unqualified.

  ## Genuine new content: `InputFormat`/`OutputFormat`

  Beyond the re-export, upstream declares two tag enumerations spanning
  every concrete format above:

  * `InputFormat` — every format the library can *read*: `InputBMP`,
    `InputGIF`, `InputHDR`, `InputJPG`, `InputPNG`, `InputTIF`, `InputPNM`
    (netpbm's `PBM`/`PGM`/`PPM` collapsed to one `.ppm`-preferring tag, since
    `decode`/`ext` can't tell PBM/PGM/PPM apart from the *format name* alone
    — only from the file's own magic number, which is `Graphics.Netpbm`'s
    job, already handled inside `decodeFirst`/`parsePPM`), `InputTGA`.
  * `OutputFormat` — every format the library can *write*: the same seven,
    minus `InputPNM` (netpbm has no encoder at all — see `IO.Formats.
    Netpbm`'s own doc-comment — so there is nothing for `OutputFormat` to
    dispatch to; upstream's own `OutputFormat` likewise has no `OutputPNM`).

  Both port directly as an `inductive` with an `ImageFormat` instance whose
  `ext`/`exts` delegate to the corresponding concrete tag's own `ImageFormat`
  instance (`ext InputBMP = ext BMP`, …) — this needs nothing beyond the
  `ext`/`exts` dispatch already given to every format tag in #22/#23, so it
  is ported in full, field for field, against upstream's own `InputFormat`/
  `OutputFormat` definitions. Upstream's `data SaveOption InputFormat`/`data
  SaveOption OutputFormat` are empty data families (no constructors, exactly
  like `BMP`/`HDR`/`PNG`/`TGA`/`TIF`/`PBM`/`PGM`/`PPM`'s own `SaveOption`),
  so both port as `Empty` here too, matching that existing convention.

  ## Out of scope: `AllReadable`/`AllWritable`/the `Readable … InputFormat`/
  ## `Writable … OutputFormat` dispatch instances, and `ComplexWritable`

  Upstream also gives:
  - `type AllReadable arr cs = (Readable (Image arr cs Double) BMP, …,
    Readable (Image arr cs Double) PPM)` and one instance `AllReadable arr cs
    => Readable (Image arr cs Double) InputFormat`, dispatching `decode` to
    whichever concrete format the tag names.
  - The mirror `AllWritable`/`Writable (Image arr cs Double) OutputFormat`.
  - A re-export of `ComplexWritable` (defined in `IO.Base`, module #21).

  All three are built entirely on the *generic, canonical-`Double`-precision*
  `Readable`/`Writable (Image arr cs Double) <Format>` instance family — the
  one `IO.Formats.JuicyPixels`'s own doc-comment ("Scope: concrete
  bit-depth-matching instances only, not the generic … layer") already
  documents as **not ported**, for `BMP`/`GIF`/`HDR`/`JPG`/`PNG`/`TGA`/`TIF`,
  because it needs a colour-space-generic `toWord8I`/`toWord16I`/`toFloatI`
  precision-narrowing transform that `Linen.Graphics.Image.ColorSpace`'s own
  doc-comment already documents as out of scope for this port's whole
  architecture (`Pixel cs e px` has no `Functor`-style structure to hang a
  component-type-changing, colour-space-generic transform off). `IO.Formats.
  Netpbm` ported that generic family for `PPM` alone (decode has no such
  obstruction there — see that module's own doc-comment), but `AllReadable`/
  `Readable … InputFormat` needs it for *all eight* formats simultaneously
  (`BMP` through `PPM`) — so, exactly as `IO.Formats.JuicyPixels` already
  decided for the seven formats it backs, porting these three declarations
  here would either reproduce the missing generic-`Functor`-equivalent
  abstraction (out of scope, already decided twice over) or hand-declare an
  instance whose hypotheses can never actually be discharged anywhere in
  this codebase as it stands today (no `Readable (Image cs Float) BMP`/
  `GIF`/`HDR`/`JPG`/`PNG`/`TGA`/`TIF` exists for any `cs`, only the concrete
  bit-depth-matching instances `IO.Formats.JuicyPixels` already gives) —
  dead, permanently-unsatisfiable code, not a faithful port of working
  upstream functionality. This is not a new simplification local to this
  module: it is the same already-accepted architectural limitation `IO.
  Formats.JuicyPixels` and `Linen.Graphics.Image.ColorSpace` each document in
  their own doc-comments, simply inherited here rather than re-litigated. A
  future caller needing `InputFormat`/`OutputFormat`-level dispatch on a
  *specific* colour space can already narrow by hand at the call site
  (`match fmt with | .bmp => decode BMP.mk bytes | …`), exactly the pattern
  those two doc-comments both already recommend for the underlying
  precision-narrowing gap.

  `ComplexWritable` itself carries nothing further to re-export beyond what
  `IO.Base` already ported: that module's own doc-comment already explains
  it was *inlined* directly into its one instance's signature rather than
  named (Lean has no direct counterpart for a bare named conjunction of
  instance-implicit constraints), so there is no separate `ComplexWritable`
  identifier anywhere in this port for this file to re-export.
-/

import Linen.Graphics.Image.IO.Formats.JuicyPixels
import Linen.Graphics.Image.IO.Formats.Netpbm

open Graphics.Image.IO.Base (ImageFormat ext exts)
open Graphics.Image.IO.Formats.JuicyPixels (BMP GIF HDR JPG PNG TGA TIF)
open Graphics.Image.IO.Formats.Netpbm (PBM PGM PPM)

namespace Graphics.Image.IO.Formats

/-- Every image format this library can read. Netpbm's `PBM`/`PGM`/`PPM` are
collapsed to one `pnm` tag (upstream's `InputPNM`), matching upstream:
`decode`/`ext` can't distinguish them by tag alone, only `Graphics.Netpbm`'s
own magic-number sniffing (already inside `decodeFirst`) can. Upstream's
`InputFormat`. -/
inductive InputFormat where
  | bmp | gif | hdr | jpg | png | tif | pnm | tga
deriving Repr, Inhabited, BEq

instance : ImageFormat InputFormat Empty where
  ext fmt := match fmt with
    | .bmp => ext BMP.mk
    | .gif => ext GIF.mk
    | .hdr => ext HDR.mk
    | .jpg => ext JPG.mk
    | .png => ext PNG.mk
    | .tif => ext TIF.mk
    | .pnm => ext PPM.mk
    | .tga => ext TGA.mk
  exts fmt := match fmt with
    | .bmp => exts BMP.mk
    | .gif => exts GIF.mk
    | .hdr => exts HDR.mk
    | .jpg => exts JPG.mk
    | .png => exts PNG.mk
    | .tif => exts TIF.mk
    | .pnm => [ext PBM.mk, ext PGM.mk, ext PPM.mk]
    | .tga => exts TGA.mk

/-- Every image format this library can write. Netpbm has no encoder at all
(see `IO.Formats.Netpbm`'s own doc-comment), so there is no `pnm` tag here,
matching upstream's `OutputFormat` exactly (it also has no `OutputPNM`).
Upstream's `OutputFormat`. -/
inductive OutputFormat where
  | bmp | gif | hdr | jpg | png | tif | tga
deriving Repr, Inhabited, BEq

instance : ImageFormat OutputFormat Empty where
  ext fmt := match fmt with
    | .bmp => ext BMP.mk
    | .gif => ext GIF.mk
    | .hdr => ext HDR.mk
    | .jpg => ext JPG.mk
    | .png => ext PNG.mk
    | .tif => ext TIF.mk
    | .tga => ext TGA.mk
  exts fmt := match fmt with
    | .bmp => exts BMP.mk
    | .gif => exts GIF.mk
    | .hdr => exts HDR.mk
    | .jpg => exts JPG.mk
    | .png => exts PNG.mk
    | .tif => exts TIF.mk
    | .tga => exts TGA.mk

end Graphics.Image.IO.Formats
