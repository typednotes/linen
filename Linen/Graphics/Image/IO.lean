/-
  Linen.Graphics.Image.IO — the top-level file-I/O facade: format-guessing
  read/write wrappers built on `IO.Base`'s `Readable`/`Writable` and
  `IO.Formats`'s `InputFormat`/`OutputFormat` tags

  ## Haskell equivalent

  `Graphics.Image.IO` from https://hackage.haskell.org/package/hip (module
  #25 of the `hip` import plan, see `docs/imports/hip/dependencies.md` — the
  **last** module of the `IO` sub-tree, modules #21–#25). Fetched from the
  1.5.6.0 release tarball (`raw.githubusercontent.com/lehins/hip/master/…`
  404s, same as every other module in this sub-tree's own note); the full
  source is ~230 lines, read in full.

  ## Histogram: confirmed absent from this module

  Unlike the eventual `Graphics.Image` facade (module #27, whose own
  doc-comment already notes it imports `Graphics.Image.IO.Histogram` only
  under the `disable-chart`-off cabal branch), **this module's own source
  contains no reference to `Graphics.Image.IO.Histogram` at all** — not in
  its import list, not in its export list, not behind any `#if`/CPP guard.
  Its only re-export is `module Graphics.Image.IO.Formats` (module #24,
  already ported). So the Histogram exclusion decided for this whole import
  (see `dependencies.md`'s scope note: dropped along with the
  `Chart`/`Chart-diagrams`/`diagrams-lib` chain it alone pulls in) requires
  no action inside this specific module beyond simply not adding an import
  that upstream itself never had here.

  ## `readImage`/`readImage'`/`writeImage`: deferred, an inherited
  ## architectural gap, not a new one

  Upstream's `readImage`/`writeImage` are polymorphic over *any* colour
  space/precision pair `(cs, e)`, guessing a file's format from its
  extension and dispatching through the generic instances `Readable (Image
  VS cs e) InputFormat` / `Writable (Image VS cs e) OutputFormat`. Those two
  generic instance families are exactly the `AllReadable`/`AllWritable`
  dispatch layer `Linen.Graphics.Image.IO.Formats`'s own doc-comment (module
  #24) already documents as **out of scope**: they need a colour-space-
  generic `toWord8I`/`toWord16I`/`toFloatI`-style precision-narrowing
  transform that `Linen.Graphics.Image.ColorSpace`'s own doc-comment already
  establishes this port's `Pixel cs e px` abstraction has no hook for (a
  plain marker class relating one *fixed* triple, not a `Functor`-style
  structure). With no `Readable (Image cs e) InputFormat` / `Writable (Image
  cs e) OutputFormat` instance ever given anywhere in this port (by design,
  not oversight), `readImage`/`readImage'`/`writeImage` cannot be given
  their upstream signatures here: doing so would either reproduce the
  missing abstraction (already decided twice over, out of scope) or declare
  a function whose constraint can never be discharged — dead code, not a
  faithful port. This is the same gap already accepted at module #24,
  simply propagating one level up rather than being re-litigated here. A
  caller needing this capability today can already narrow "guess a format,
  then decode" by hand at one concrete colour space at a time, using
  `guessFormat`/`readImageExact` below plus a `match` over the guessed tag
  (`match fmt with | .bmp => readImageExact
  (Graphics.Image.IO.Formats.JuicyPixels.BMP.mk) path | …`) — exactly the
  pattern module #24's own doc-comment already recommends for this same
  underlying limitation.

  ## `readImageExact`/`readImageExact'`/`writeImageExact`: ported directly

  These three need nothing beyond the already-ported, non-generic
  `Readable img format` / `Writable img format` classes (module #21), so
  they port faithfully. File I/O below follows this codebase's established
  idiom throughout the `hip`/`JuicyPixels` imports (`Linen.Codec.Picture`'s
  own `readAndDecode`/`writeBitmap`-style wrappers): `IO.FS.readBinFile`/
  `IO.FS.writeBinFile` directly on the `ByteArray` `decode`/`encode` already
  operate over (`IO.Base`'s own doc-comment: both collapse `ByteString`/
  `ByteString.Lazy` distinctions to a plain `ByteArray`, so no
  `Data.ByteString`-level conversion step is needed here at all, unlike
  `IO.Formats.JuicyPixels`'s own `toByteArray` helper, which exists only to
  bridge `Codec.Picture`'s own `Data.ByteString`-returning encoders), and
  `try ... catch e => pure (.error (toString e))` for the read side.
  `readImageExact'` unwraps the `Except` with `IO.ofExcept` (`String` has a
  `ToString` instance, satisfying `IO.ofExcept`'s constraint), throwing an
  `IO` exception on decode failure exactly as upstream's `either error id`
  does.

  ## `guessFormat`: a pure helper taking an explicit format list, not a
  ## reflected `Enum`

  Upstream's `guessFormat :: (ImageFormat f, Enum f) => FilePath -> Maybe f`
  reflects over *every* value of `f` via `enumFrom . toEnum $ 0` — GHC's
  derived `Enum` typeclass, letting the same one-line definition work for
  any `ImageFormat` instance with no further input. Lean has no derived
  `Enum`/`Bounded`-style reflection over an inductive's constructors, so
  this ports as a function taking the candidate list explicitly
  (`allFormats : List format`), with `allInputFormats`/`allOutputFormats`
  below supplying that list for this module's own two format tags — the
  same one-time enumeration `IO.Formats`'s own `InputFormat`/`OutputFormat`
  `ext`/`exts` `match`es already write out by hand for the same reason (no
  derived `Enum` to iterate). `System.FilePath.extension` (Lean core) is
  used in place of upstream's `System.FilePath.takeExtension` — the former
  returns the extension *without* its leading dot (`("picture.jpg" :
  FilePath).extension = some "jpg"`), so a dot is prepended before the
  `isFormat` lookup to match `ImageFormat.ext`/`exts`'s own dotted
  convention (`".jpg"`); both apply `String.toLower`, mirroring upstream's
  `Data.Char.toLower`.

  ## Deferred: `ExternalViewer`/`displayImage`/the viewer helpers — no GUI or
  ## external-process story in this codebase

  Upstream's `ExternalViewer`/`displayImage`/`displayImageUsing`/
  `displayImageFile`/`defaultViewer`/`eogViewer`/`fehViewer`/
  `gpicviewViewer`/`gimpViewer` write an image to a temporary file and then
  `System.Process.readProcess` an external GUI application (`eog`, `feh`,
  `gimp`, the OS's `xdg-open`/`open`/`explorer.exe`) to display it,
  optionally in a forked thread (`Control.Concurrent.forkIO`). `linen` has
  no windowing/GUI story and no established "launch an external interactive
  viewer" primitive anywhere in this codebase — this is the same category of
  gap `Linen.Codec.Picture`'s own doc-comment already accepts for upstream's
  FFI-level `withImage`/`imageFromUnsafePtr` (deferred there as "no
  meaningful translation," not silently dropped): there is nothing to build
  a faithful, testable Lean counterpart out of, since the entire point of
  these nine declarations is to hand control to a separate, platform-
  specific interactive program outside this library's process. Deferred in
  full, following that same precedent, rather than stubbed with a body that
  could never actually display anything.

  ## Fixture/test naming

  Following this whole sub-tree's own convention (`jp`/`pnm` prefixes in
  `IO.Formats.JuicyPixels`/`.Netpbm`'s own test files), tests in
  `Tests/Linen/Graphics/Image/IOTest.lean` use an `io`-prefix on every
  fixture, to avoid cross-file `Tests` namespace collisions.
-/

import Linen.Graphics.Image.ColorSpace
import Linen.Graphics.Image.IO.Base
import Linen.Graphics.Image.IO.Formats

open Graphics.Image.IO.Base (ImageFormat Readable Writable ext exts isFormat decode encode)
open Graphics.Image.IO.Formats (InputFormat OutputFormat)

namespace Graphics.Image.IO

-- ── Format enumerations (Lean has no derived `Enum` to reflect over) ──

/-- Every `InputFormat` tag, in the same order `IO.Formats`'s own `ext`/`exts`
`match`es enumerate them. Stands in for upstream's `enumFrom . toEnum $ 0`
at this one format type; see the module doc-comment. -/
def allInputFormats : List InputFormat := [.bmp, .gif, .hdr, .jpg, .png, .tif, .pnm, .tga]

/-- Every `OutputFormat` tag, in the same order `IO.Formats`'s own `ext`/
`exts` `match`es enumerate them. -/
def allOutputFormats : List OutputFormat := [.bmp, .gif, .hdr, .jpg, .png, .tif, .tga]

-- ── `guessFormat` — pick a format tag from a file's extension ──

/-- Guess an image format from `path`'s file extension, preferring the first
match among `allFormats` (upstream's `guessFormat`; see the module
doc-comment for why the candidate list is an explicit argument rather than a
reflected `Enum` range, and for the dotted-extension/lowercasing
convention). Returns `none` if `path` has no extension or no candidate
format's `isFormat` recognises it. -/
def guessFormat {format SaveOption : Type} [ImageFormat format SaveOption]
    (allFormats : List format) (path : System.FilePath) : Option format :=
  match path.extension with
  | none => none
  | some e => allFormats.find? (isFormat s!".{e.toLower}" ·)

-- ── `readImageExact`/`readImageExact'` — decode a file against one known format ──

/-- Read `path` and decode it against the given `format`, turning any `IO`
exception raised while reading into an `Except`-level error, matching this
whole sub-tree's own file-IO idiom (upstream's `readImageExact`). -/
def readImageExact {img format SaveOption : Type} [ImageFormat format SaveOption]
    [Readable img format] (format : format) (path : System.FilePath) :
    IO (Except String img) := do
  try
    let bytes ← IO.FS.readBinFile path
    pure (decode format bytes)
  catch e => pure (.error (toString e))

/-- `readImageExact`, throwing an `IO` exception on decode failure instead of
returning an `Except` (upstream's `readImageExact'`). -/
def readImageExact' {img format SaveOption : Type} [ImageFormat format SaveOption]
    [Readable img format] (format : format) (path : System.FilePath) : IO img := do
  IO.ofExcept (← readImageExact format path)

-- ── `writeImageExact` — encode an image against one known format ──

/-- Encode `img` against the given `format` and write it to `path` (upstream's
`writeImageExact`). -/
def writeImageExact {img format SaveOption : Type} [ImageFormat format SaveOption]
    [Writable img format] (format : format) (opts : List SaveOption)
    (path : System.FilePath) (img : img) : IO Unit :=
  IO.FS.writeBinFile path (encode format opts img)

end Graphics.Image.IO
