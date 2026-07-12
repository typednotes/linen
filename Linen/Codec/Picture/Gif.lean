import Linen.Codec.Picture.Types
import Linen.Codec.Picture.Metadata
import Linen.Codec.Picture.ColorQuant
import Linen.Codec.Picture.Gif.Internal.LZW
import Linen.Codec.Picture.Gif.Internal.LZWEncoding
import Linen.Data.ByteString.Builder

/-!
  Port of `Codec.Picture.Gif` (top-level) from the `JuicyPixels` package (see
  `docs/imports/JuicyPixels/dependencies.md`, module 20 of 29): whole-file GIF
  parsing and writing — the header, logical screen descriptor, optional
  global colour table, the block stream (image descriptors with optional
  local colour tables and LZW-compressed pixel data, graphic control
  extensions, application/comment/plain-text extensions), the trailer, and
  the top-level `decodeGif`/`decodeGifWithMetadata`/
  `decodeGifWithPaletteAndMetadata`/`decodeGifImages`/`getDelaysGifImages`/
  `encodeGifImage`/`encodeGifImages`/`encodeGifImageWithPalette`/
  `encodeComplexGifImage` entry points, built on module 18
  (`Linen.Codec.Picture.Gif.Internal.LZW`, decode), module 19
  (`Linen.Codec.Picture.Gif.Internal.LZWEncoding`, encode) and module 7
  (`Linen.Codec.Picture.ColorQuant`, palette quantisation on encode).

  ## Animation-support scope

  `Linen.Codec.Picture.Types` has no multi-frame/animation image type at
  all (confirmed by reading every constructor of `DynamicImage` and every
  field of `Image`/`PalettedImage`) — GIF is the first format in this
  library that can hold more than one frame per file. Rather than either
  (a) silently only ever exposing the first frame, or (b) inventing new
  animation infrastructure elsewhere in `Types.lean` (out of scope for this
  module, and not requested), this port takes the middle path upstream
  itself uses: `List DynamicImage` already captures "a sequence of
  independent, fully-composited images" with no new type needed, so:

  - `decodeGif`/`decodeGifWithMetadata`/`decodeGifWithPaletteAndMetadata`
    decode **only the first frame** (matching upstream's own
    `decodeFirstGifImage`/`decodeGif` family), returned as a single
    `Sum DynamicImage PalettedImage` (this codebase's established
    multi-format-result shape, per `Linen.Codec.Picture.Tiff`).
  - `decodeGifImages` decodes and **fully composites every frame** of an
    animated GIF (disposal methods, transparency substitution, canvas
    placement against the logical screen) into a `List DynamicImage`,
    matching upstream's `decodeGifImages`.
  - `getDelaysGifImages` returns each frame's delay (in hundredths of a
    second), matching upstream's `getDelaysGifImages`.

  No "single frame vs list of frames" type-level distinction is invented;
  callers who want one frame call the `decodeGif*` family, callers who want
  every frame call `decodeGifImages`.

  ## Block / sub-block traversal termination strategy

  A GIF block stream and each block's LZW-data sub-block chain are both
  self-delimiting but *unboundedly* long from the type checker's point of
  view: a block's own leading tag byte says what kind of block follows and
  a sub-block's own leading length byte says how many data bytes follow it,
  with `0x00` (sub-blocks) or the trailer byte `0x3B` (blocks) the only
  built-in stopping conditions — nothing about a single step *syntactically*
  bounds how many steps a malicious or corrupt stream could demand. As with
  `Linen.Codec.Picture.Tiff`'s `parseIfdChainAux` (module 17) this port's
  termination argument is that **the file has finitely many bytes**, not
  fuel picked out of thin air: `parseDataBlocksAux`/`parseGifBlocksAux` both
  take a `fuel : Nat` matched via the `n + 1` / `0` pattern (so Lean accepts
  the recursion as plain structural recursion on `fuel`, no
  `termination_by`/`decreasing_by` needed) seeded as `bytes.length + 1` by
  their callers — a genuine upper bound, since every sub-block/block
  consumes at least one byte (a length byte or a tag byte) before it can
  possibly recurse again. Running out of fuel is reported as an `Except`
  error, never silently truncated.

  ## Deviation: `LogicalScreenDescriptor`'s corrected field order

  Upstream's own `Binary LogicalScreenDescriptor` instance is internally
  inconsistent: `put` writes the aspect-ratio byte before the background
  colour index, but `get` reads the background colour index before the
  aspect-ratio byte (the latter is the order the GIF87a/89a spec actually
  requires) — i.e. upstream's own encoder writes a spec-noncompliant byte
  order that upstream's own decoder does not agree with (this is a genuine
  bug in the source project's `Binary` instance, not a misreading on this
  port's part — checked directly against the fetched source). This port
  writes its own encode and decode functions from scratch rather than
  sharing one `Binary`-style method pair, so faithfully reproducing the bug
  would mean *deliberately* introducing the same swap into this port's own
  encoder — breaking this module's own round-trip tests and producing
  spec-noncompliant files for no one's benefit. Both `putLogicalScreenDescriptor`
  and `getLogicalScreenDescriptor` here consistently use the spec-correct
  order (background index, then aspect ratio), matching upstream's `get`.

  ## Simplification: `Option Nat` instead of an `Int`-sentinel for transparency

  Upstream threads a `GifDelay`/transparent-colour-index style `Int` value
  through several places using the sentinel `300` to mean "no transparent
  colour" (a value that can never be a real palette index, since indices are
  always `< 256`). This port uses `Option Nat` throughout instead
  (`GraphicControlExtension.transparentColorIndex : Nat` alongside a
  separate `transparentFlag : Bool`, and `GifFrame.transparent : Option
  Nat`), avoiding magic-number encoding while representing exactly the same
  information.

  ## Simplification: dropped discarded compositing state, dropped write-to-file IO

  Upstream's `gifAnimationApplyer`'s scan state is a 3-tuple `(palette,
  control, image)` whose `palette` component is threaded through every step
  but never actually read by the scanning function itself (confirmed by
  close reading: every step pattern-matches it only as `_`). This port's
  `compositeGifFrame` drops that unused component from its own state
  representation. Separately, upstream's `writeGifImage`/`writeGifImages`/
  `writeGifImageWithPalette`/`writeComplexGifImage` (trivial `IO`
  file-writers wrapping the corresponding pure `encode*` function) are
  dropped, matching the precedent already set by `Linen.Codec.Picture.Png`'s
  own module doc-comment for the same four-function shape: callers needing
  file I/O can trivially wrap the pure `encode*` functions themselves.

  ## Purity

  Unlike `Linen.Codec.Picture.Png` (module 14), GIF's only compression
  scheme is LZW (modules 18/19), which — like every TIFF compression scheme
  this library supports — needs no zlib/`IO` dependency. This module is
  therefore fully pure (`Except String α`, no `IO`), following
  `Linen.Codec.Picture.Tiff`'s precedent rather than `Png`'s.

  ## `ColorQuant`'s `palettizeWithAlpha` cross-reference

  `Linen.Codec.Picture.ColorQuant`'s own module doc-comment defers porting
  upstream's `palettizeWithAlpha`/`alphaToBlack`/`alphaTo255` helpers "until
  `Linen.Codec.Picture.Gif` exists," since they mention `GifFrame`-shaped
  types. Having now read the full upstream `Codec/Picture/Gif.hs` in detail,
  none of `palettizeWithAlpha`/`alphaToBlack`/`alphaTo255` are actually
  referenced anywhere in that file's body — they are consumed elsewhere in
  upstream's own package (its `Codec.Picture.Saving` module, which this
  port's dependency list places after this one). This module therefore does
  **not** port them; that remains a follow-up for whichever later module
  actually needs them (`Codec.Picture.Saving`), not a gap in this module.

  ## Encode totality

  Upstream's `encodeGifImage :: Image Pixel8 -> L.ByteString` is a *total*
  signature achieved by an internal `error` call on a branch upstream's own
  comment admits is unreachable in practice (an ordinary Haskell partial
  function dressed up as total). This port's `encodeGifImage` instead
  returns `Except String Data.ByteString`, matching every validating
  `encode*` function in this module (and matching this codebase's blanket
  "no `partial`, no `sorry`" rule, which rules out reproducing an internal
  `error`/`panic!` call standing in for a proof of unreachability).
-/

namespace Codec.Picture

open Data.ByteString (Builder)

-- ── Byte-level decoding primitives ──

/-- Read a single byte. -/
private def gifReadU8 (bytes : List UInt8) : Except String (UInt8 × List UInt8) :=
  match bytes with
  | b :: rest => .ok (b, rest)
  | [] => .error "Unexpected end of GIF stream"

/-- Read `n` bytes from the front of `bytes`. -/
private def gifReadBytesFixed (n : Nat) (bytes : List UInt8) :
    Except String (List UInt8 × List UInt8) :=
  if n ≤ bytes.length then .ok (bytes.take n, bytes.drop n)
  else .error "Unexpected end of GIF stream"

/-- Read a little-endian 16-bit value (every multi-byte GIF field is
    little-endian). -/
private def gifReadU16LE (bytes : List UInt8) : Except String (UInt16 × List UInt8) := do
  let (b0, r1) ← gifReadU8 bytes
  let (b1, r2) ← gifReadU8 r1
  pure (b0.toUInt16 ||| (b1.toUInt16 <<< 8), r2)

private def pushBytes (b : Builder) (bytes : List UInt8) : Builder :=
  bytes.foldl (fun acc c => acc ++ Builder.singleton c) b

/-- Write a list of raw bytes. -/
private def putBytes (bytes : List UInt8) : Builder := pushBytes Builder.empty bytes

/-- Write an ASCII string as raw bytes (GIF signatures/application
    identifiers are always plain ASCII). -/
private def putAscii (s : String) : Builder :=
  putBytes (s.toList.map (fun c => UInt8.ofNat c.toNat))

-- ── Sub-block traversal (LZW-data sub-blocks) ──

/-- Read one `0x00`-terminated chain of length-prefixed sub-blocks
    (upstream's `parseDataBlocks`), collecting every data byte into a single
    flat list. See the module doc-comment for why `fuel` genuinely bounds
    this traversal. -/
private def parseDataBlocksAux : Nat → List UInt8 → Except String (List UInt8 × List UInt8)
  | 0, _ => .error "GIF sub-block chain exceeded its fuel bound (malformed stream)"
  | fuel + 1, bytes => do
    let (len, r1) ← gifReadU8 bytes
    if len == 0 then
      pure ([], r1)
    else
      let (chunk, r2) ← gifReadBytesFixed len.toNat r1
      let (rest, r3) ← parseDataBlocksAux fuel r2
      pure (chunk ++ rest, r3)

/-- Read a `0x00`-terminated chain of length-prefixed sub-blocks. -/
private def parseDataBlocks (bytes : List UInt8) : Except String (List UInt8 × List UInt8) :=
  parseDataBlocksAux (bytes.length + 1) bytes

/-- Chunk `bytes` into `≤ 255`-byte length-prefixed sub-blocks, terminated by
    a `0x00` byte (upstream's `putDataBlocks`). Bounded recursion on the
    remaining byte count, mirroring the decode side's fuel argument. -/
private def putDataBlocksAux : Nat → List UInt8 → Builder
  | 0, _ => Builder.singleton 0
  | fuel + 1, bytes =>
    if bytes.isEmpty then
      Builder.singleton 0
    else
      let chunk := bytes.take 255
      let rest := bytes.drop 255
      Builder.singleton (UInt8.ofNat chunk.length) ++ putBytes chunk ++ putDataBlocksAux fuel rest

private def putDataBlocks (bytes : List UInt8) : Builder :=
  putDataBlocksAux (bytes.length + 1) bytes

/-- Skip (without decoding) a `0x00`-terminated chain of sub-blocks, e.g. for
    extension blocks whose payload this port doesn't otherwise interpret
    (upstream's `skipSubDataBlocks`). -/
private def skipSubDataBlocksAux : Nat → List UInt8 → Except String (List UInt8)
  | 0, _ => .error "GIF sub-block chain exceeded its fuel bound (malformed stream)"
  | fuel + 1, bytes => do
    let (len, r1) ← gifReadU8 bytes
    if len == 0 then pure r1
    else
      let (_chunk, r2) ← gifReadBytesFixed len.toNat r1
      skipSubDataBlocksAux fuel r2

private def skipSubDataBlocks (bytes : List UInt8) : Except String (List UInt8) :=
  skipSubDataBlocksAux (bytes.length + 1) bytes

-- ── `LogicalScreenDescriptor` ──

/-- The fixed-size record following the 6-byte signature at the start of
    every GIF file. -/
structure LogicalScreenDescriptor where
  screenWidth : UInt16
  screenHeight : UInt16
  hasGlobalMap : Bool
  colorResolution : Nat
  isColorTableSorted : Bool
  /-- `2 ^ colorTableSize` entries in the global colour table, when present. -/
  colorTableSize : Nat
  backgroundIndex : UInt8
  deriving BEq, Repr

/-- See the module doc-comment: this uses the spec-correct field order
    (background index, then aspect ratio), matching upstream's `get` rather
    than its inconsistent `put`. -/
private def getLogicalScreenDescriptor (bytes : List UInt8) :
    Except String (LogicalScreenDescriptor × List UInt8) := do
  let (w, r1) ← gifReadU16LE bytes
  let (h, r2) ← gifReadU16LE r1
  let (packed, r3) ← gifReadU8 r2
  let (bg, r4) ← gifReadU8 r3
  let (_aspectRatio, r5) ← gifReadU8 r4
  pure ({ screenWidth := w, screenHeight := h
          hasGlobalMap := (packed &&& 0x80) != 0
          colorResolution := (((packed >>> 4) &&& 0x07).toNat) + 1
          isColorTableSorted := (packed &&& 0x08) != 0
          colorTableSize := ((packed &&& 0x07).toNat) + 1
          backgroundIndex := bg }, r5)

private def putLogicalScreenDescriptor (d : LogicalScreenDescriptor) : Builder :=
  let globalField : UInt8 := if d.hasGlobalMap then 0x80 else 0
  let sortedField : UInt8 := if d.isColorTableSorted then 0x08 else 0
  let resField : UInt8 := (UInt8.ofNat (d.colorResolution - 1) &&& 0x07) <<< 4
  let sizeField : UInt8 := UInt8.ofNat (d.colorTableSize - 1) &&& 0x07
  let packed := globalField ||| sortedField ||| resField ||| sizeField
  Builder.word16LE d.screenWidth ++ Builder.word16LE d.screenHeight ++
    Builder.singleton packed ++ Builder.singleton d.backgroundIndex ++ Builder.singleton 0

-- ── Colour table (palette) ──

/-- Read `2 ^ bits` consecutive RGB triples as a `Palette`. -/
private def getPalette (bits : Nat) (bytes : List UInt8) : Except String (Palette × List UInt8) := do
  let size := 2 ^ bits
  let (rgb, rest) ← gifReadBytesFixed (size * 3) bytes
  pure ({ width := size, height := 1, data := rgb.toArray }, rest)

/-- Write a palette as `2 ^ bits` consecutive RGB triples, zero-padding up to
    that count if the palette itself has fewer entries. -/
private def putPalette (bits : Nat) (p : Palette) : Builder :=
  let want := 2 ^ bits
  let padding := (want - p.width) * 3
  pushBytes Builder.empty p.data.toList ++ putBytes (List.replicate padding (0 : UInt8))

-- ── `ImageDescriptor` ──

structure ImageDescriptor where
  left : UInt16
  top : UInt16
  width : UInt16
  height : UInt16
  hasLocalMap : Bool
  isInterlaced : Bool
  isImgDescriptorSorted : Bool
  localColorTableSize : Nat
  deriving BEq, Repr

private def getImageDescriptor (bytes : List UInt8) : Except String (ImageDescriptor × List UInt8) := do
  let (left, r1) ← gifReadU16LE bytes
  let (top, r2) ← gifReadU16LE r1
  let (width, r3) ← gifReadU16LE r2
  let (height, r4) ← gifReadU16LE r3
  let (packed, r5) ← gifReadU8 r4
  pure ({ left, top, width, height
          hasLocalMap := (packed &&& 0x80) != 0
          isInterlaced := (packed &&& 0x40) != 0
          isImgDescriptorSorted := (packed &&& 0x20) != 0
          localColorTableSize := ((packed &&& 0x07).toNat) + 1 }, r5)

private def putImageDescriptor (d : ImageDescriptor) : Builder :=
  let localField : UInt8 := if d.hasLocalMap then 0x80 else 0
  let interlacedField : UInt8 := if d.isInterlaced then 0x40 else 0
  let sortedField : UInt8 := if d.isImgDescriptorSorted then 0x20 else 0
  let sizeField : UInt8 := UInt8.ofNat (d.localColorTableSize - 1) &&& 0x07
  let packed := localField ||| interlacedField ||| sortedField ||| sizeField
  Builder.singleton 0x2C ++ Builder.word16LE d.left ++ Builder.word16LE d.top ++
    Builder.word16LE d.width ++ Builder.word16LE d.height ++ Builder.singleton packed

-- ── `GifDisposalMethod` ──

/-- How the previous frame's canvas area should be treated before drawing
    the next frame. -/
inductive GifDisposalMethod where
  | unspecified
  | doNotDispose
  | restoreBackground
  | restorePrevious
  deriving BEq, Repr, Inhabited

private def disposalMethodOfCode (c : UInt8) : GifDisposalMethod :=
  match c with
  | 1 => .doNotDispose
  | 2 => .restoreBackground
  | 3 => .restorePrevious
  | _ => .unspecified

private def codeOfDisposalMethod : GifDisposalMethod → UInt8
  | .unspecified => 0
  | .doNotDispose => 1
  | .restoreBackground => 2
  | .restorePrevious => 3

-- ── `GraphicControlExtension` ──

structure GraphicControlExtension where
  disposalMethod : GifDisposalMethod
  userInputFlag : Bool
  transparentFlag : Bool
  /-- Hundredths of a second. -/
  delay : UInt16
  transparentColorIndex : UInt8
  deriving BEq, Repr, Inhabited

/-- Decode a graphic control extension's body (the block-introducer `0x21`
    and label `0xF9` bytes are consumed by the caller before this runs). -/
private def getGraphicControlExtension (bytes : List UInt8) :
    Except String (GraphicControlExtension × List UInt8) := do
  let (_blockSize, r1) ← gifReadU8 bytes
  let (packed, r2) ← gifReadU8 r1
  let (delay, r3) ← gifReadU16LE r2
  let (idx, r4) ← gifReadU8 r3
  let (_terminator, r5) ← gifReadU8 r4
  pure ({ disposalMethod := disposalMethodOfCode ((packed >>> 2) &&& 0x07)
          userInputFlag := (packed &&& 0x02) != 0
          transparentFlag := (packed &&& 0x01) != 0
          delay, transparentColorIndex := idx }, r5)

private def putGraphicControlExtension (e : GraphicControlExtension) : Builder :=
  let disposalField : UInt8 := (codeOfDisposalMethod e.disposalMethod &&& 0x07) <<< 2
  let userField : UInt8 := if e.userInputFlag then 0x02 else 0
  let transField : UInt8 := if e.transparentFlag then 0x01 else 0
  let packed := disposalField ||| userField ||| transField
  Builder.singleton 0x21 ++ Builder.singleton 0xF9 ++ Builder.singleton 4 ++
    Builder.singleton packed ++ Builder.word16LE e.delay ++
    Builder.singleton e.transparentColorIndex ++ Builder.singleton 0

-- ── `GifImage` (one frame's image descriptor + optional local palette + LZW data) ──

structure GifImage where
  descriptor : ImageDescriptor
  localPalette : Option Palette
  lzwMinCodeSize : UInt8
  imageData : List UInt8

/-- Decode one image block's body; the leading image-separator byte `0x2C`
    is consumed by the caller (`parseGifBlocksAux`) before this runs. -/
private def getGifImage (bytes : List UInt8) : Except String (GifImage × List UInt8) := do
  let (desc, r1) ← getImageDescriptor bytes
  let (localPalette, r2) ←
    if desc.hasLocalMap then do
      let (p, r) ← getPalette desc.localColorTableSize r1
      pure (some p, r)
    else pure (none, r1)
  let (minCodeSize, r3) ← gifReadU8 r2
  let (imageData, r4) ← parseDataBlocks r3
  pure ({ descriptor := desc, localPalette, lzwMinCodeSize := minCodeSize, imageData }, r4)

private def putGifImage (img : GifImage) : Builder :=
  putImageDescriptor img.descriptor ++
    (match img.localPalette with
     | some p => putPalette img.descriptor.localColorTableSize p
     | none => Builder.empty) ++
    Builder.singleton img.lzwMinCodeSize ++
    putDataBlocks img.imageData

-- ── `Block` (one entry of the block stream) ──

/-- One block of the GIF block stream (upstream's `Block`); comment and
    plain-text extension payloads are skipped rather than decoded (this
    port has no metadata/text-overlay representation for them, matching
    upstream's own `decodeAllGifImages`, which likewise never surfaces
    their content). -/
inductive Block where
  | graphicControl (e : GraphicControlExtension)
  | image (img : GifImage)
  | other

private def isTrailer (b : UInt8) : Bool := b == 0x3B

/-- Read one block stream (upstream's `parseGifBlocks`), stopping at the
    trailer byte `0x3B`. See the module doc-comment for the fuel bound. -/
private def parseGifBlocksAux : Nat → List UInt8 → Except String (List Block)
  | 0, _ => .error "GIF block stream exceeded its fuel bound (malformed stream)"
  | fuel + 1, bytes => do
    match bytes with
    | [] => .error "Unexpected end of GIF stream (missing trailer)"
    | tag :: rest =>
      if isTrailer tag then
        pure []
      else if tag == 0x2C then do
        let (img, r) ← getGifImage rest
        let more ← parseGifBlocksAux fuel r
        pure (.image img :: more)
      else if tag == 0x21 then do
        -- Extension introducer: dispatch on the label byte.
        let (label, r1) ← gifReadU8 rest
        if label == 0xF9 then do
          let (gce, r2) ← getGraphicControlExtension r1
          let more ← parseGifBlocksAux fuel r2
          pure (.graphicControl gce :: more)
        else do
          -- Application/comment/plain-text extensions: for a plain-text
          -- extension the label is immediately followed by a fixed 12-byte
          -- text-grid record before the usual sub-block chain; for
          -- application/comment extensions the sub-block chain follows the
          -- label directly.
          let r2 ← if label == 0x01 then (gifReadBytesFixed 12 r1).map (·.2) else pure r1
          let r3 ← skipSubDataBlocks r2
          let more ← parseGifBlocksAux fuel r3
          pure (.other :: more)
      else
        .error s!"Unknown GIF block tag {tag}"

private def parseGifBlocks (bytes : List UInt8) : Except String (List Block) :=
  parseGifBlocksAux (bytes.length + 1) bytes

/-- Pair each image block with the graphic control extension immediately
    preceding it, if any (upstream's `associateDescr`). -/
private def associateDescr : List Block → List (Option GraphicControlExtension × GifImage) :=
  let rec go (pending : Option GraphicControlExtension) : List Block →
      List (Option GraphicControlExtension × GifImage)
    | [] => []
    | .graphicControl e :: rest => go (some e) rest
    | .image img :: rest => (pending, img) :: go none rest
    | .other :: rest => go pending rest
  go none

-- ── `GifHeader` ──

private def gif87aSig : List UInt8 := [0x47, 0x49, 0x46, 0x38, 0x37, 0x61]
private def gif89aSig : List UInt8 := [0x47, 0x49, 0x46, 0x38, 0x39, 0x61]

structure GifHeader where
  screenDescriptor : LogicalScreenDescriptor
  globalMap : Option Palette

private def getGifHeader (bytes : List UInt8) : Except String (GifHeader × List UInt8) := do
  let (sig, r1) ← gifReadBytesFixed 6 bytes
  if sig != gif87aSig ∧ sig != gif89aSig then
    .error "Not a GIF file (bad signature)"
  else do
    let (screenDescriptor, r2) ← getLogicalScreenDescriptor r1
    let (globalMap, r3) ←
      if screenDescriptor.hasGlobalMap then do
        let (p, r) ← getPalette screenDescriptor.colorTableSize r2
        pure (some p, r)
      else pure (none, r2)
    pure ({ screenDescriptor, globalMap }, r3)

private def putGifHeader (multiFrame : Bool) (h : GifHeader) : Builder :=
  putBytes (if multiFrame then gif89aSig else gif87aSig) ++
    putLogicalScreenDescriptor h.screenDescriptor ++
    (match h.globalMap with
     | some p => putPalette h.screenDescriptor.colorTableSize p
     | none => Builder.empty)

/-- Decode a whole GIF file into its header and every `(graphic control,
    image)` pair (upstream's `decode` for `GifFile`, minus the looping
    application-extension, which this port's decode side never needs to
    surface). -/
private def parseGif (bytes : ByteArray) : Except String (GifHeader × List (Option GraphicControlExtension × GifImage)) := do
  let all := bytes.toList
  let (header, rest) ← getGifHeader all
  let blocks ← parseGifBlocks rest
  pure (header, associateDescr blocks)

-- ── `GifLooping` (encode-side application extension) ──

/-- How many times an animated GIF's frame sequence should repeat. -/
inductive GifLooping where
  /-- No Netscape looping-application extension is written at all. -/
  | never
  /-- Loop forever. -/
  | forever
  /-- Loop the given number of extra times (`0` also means "forever," per
      the Netscape extension's own convention). -/
  | repeatN (count : UInt16)

private def putLoopingRepeat (count : UInt16) : Builder :=
  Builder.singleton 0x21 ++ Builder.singleton 0xFF ++ Builder.singleton 11 ++
    putAscii "NETSCAPE2.0" ++ Builder.singleton 3 ++ Builder.singleton 1 ++
    Builder.word16LE count ++ Builder.singleton 0

private def putLooping : GifLooping → Builder
  | .never => Builder.empty
  | .forever => putLoopingRepeat 0
  | .repeatN count => putLoopingRepeat count

-- ── Decoding a single frame's pixels ──

/-- Deinterlace order: GIF's 4-pass interlacing visits rows `0, 8, 16, …`,
    then `4, 12, 20, …`, then `2, 6, 10, …`, then every odd row (upstream's
    `gifInterlacingIndices`/`deinterlaceGifImage`), each pass covering the
    whole image width. This produces the list of *source* row indices in
    the order they physically appear in the LZW-decompressed byte stream,
    for a canvas of `height` rows. -/
private def gifInterlacingIndices (height : Nat) : Array Nat :=
  Id.run do
    let mut out := #[]
    for r in [0:height:8] do out := out.push r
    for r in [4:height:8] do out := out.push r
    for r in [2:height:4] do out := out.push r
    for r in [1:height:2] do out := out.push r
    pure out

/-- Undo GIF interlacing: `raw` holds `width * height` palette-index bytes
    in interlaced row order; return them in top-to-bottom row order. -/
private def deinterlaceIndices (width height : Nat) (raw : Array UInt8) : Array UInt8 :=
  let order := gifInterlacingIndices height
  Id.run do
    let mut out := Array.mkEmpty (width * height)
    out := Array.replicate (width * height) (0 : UInt8)
    for pass in [0:order.size] do
      let destRow := order[pass]!
      for x in [0:width] do
        out := out.set! (destRow * width + x) (raw.getD (pass * width + x) 0)
    pure out

/-- Decode one frame's palette-index pixel grid: LZW-decompress its image
    data and, if interlaced, undo the interlacing (upstream's
    `decodeImage`). -/
private def decodeGifFrameIndices (img : GifImage) : Except String (Image Pixel8) := do
  let width := img.descriptor.width.toNat
  let height := img.descriptor.height.toNat
  let decoded ← decodeLzw img.lzwMinCodeSize.toNat (ByteArray.mk img.imageData.toArray)
  let raw := if decoded.size ≥ width * height then decoded.extract 0 (width * height)
             else decoded ++ Array.replicate (width * height - decoded.size) 0
  let ordered := if img.descriptor.isInterlaced then deinterlaceIndices width height raw else raw
  pure { width, height, data := ordered }

-- ── Palette substitution (indices → RGB/RGBA) ──

/-- Substitute a paletted image's indices for RGB colours, using `palette`
    (upstream's `substituteColors`). -/
private def substituteColors (palette : Palette) (img : Image Pixel8) : Image PixelRGB8 :=
  generateImage (fun x y => palette.getPixel (img.getPixel x y).toNat 0) img.width img.height

/-- Substitute a paletted image's indices for RGBA colours, treating
    `transparent` (if present) as a fully-transparent index (upstream's
    `substituteColorsWithTransparency`). -/
private def substituteColorsWithTransparency (palette : Palette) (transparent : Option Nat)
    (img : Image Pixel8) : Image PixelRGBA8 :=
  generateImage (fun x y =>
    let idx := (img.getPixel x y).toNat
    let rgb := palette.getPixel idx 0
    if transparent == some idx then
      { r := rgb.r, g := rgb.g, b := rgb.b, a := 0 }
    else
      ColorConvertible.promotePixel rgb) img.width img.height

/-- Resolve which palette (local, else global) and transparency setting
    apply to a single frame, then produce its true-colour pixels (RGBA if
    the frame declares a transparent index, RGB otherwise). -/
private def decodeFrameToDynamic (globalMap : Option Palette)
    (control : Option GraphicControlExtension) (img : GifImage) :
    Except String DynamicImage := do
  let palette ← match img.localPalette, globalMap with
    | some p, _ => pure p
    | none, some p => pure p
    | none, none => .error "GIF frame has no local or global colour table"
  let indices ← decodeGifFrameIndices img
  match control with
  | some gce =>
    if gce.transparentFlag then
      pure (.rgba8 (substituteColorsWithTransparency palette (some gce.transparentColorIndex.toNat) indices))
    else
      pure (.rgb8 (substituteColors palette indices))
  | none => pure (.rgb8 (substituteColors palette indices))

-- ── Whole-canvas compositing (for `decodeGifImages`) ──

/-- Extract the RGBA8 image underlying a `DynamicImage` produced by
    `decodeFrameToDynamic` (always `.rgb8` or `.rgba8`). -/
private def toRgba8 : DynamicImage → Image PixelRGBA8
  | .rgb8 img => pixelMap ColorConvertible.promotePixel img
  | .rgba8 img => img
  | _ => generateImage (fun _ _ => (⟨0, 0, 0, 0⟩ : PixelRGBA8)) 0 0

/-- Draw `frame` onto `canvas` at its declared offset, honouring
    transparency (a fully-transparent source pixel leaves the canvas pixel
    untouched — GIF's compositing rule). -/
private def drawFrameOnto (canvas : Image PixelRGBA8) (offsetX offsetY : Nat)
    (frame : Image PixelRGBA8) : Image PixelRGBA8 :=
  pixelMapXY (fun x y bg =>
    if x < offsetX ∨ y < offsetY then bg
    else
      let fx := x - offsetX
      let fy := y - offsetY
      if fx < frame.width ∧ fy < frame.height then
        let fp := frame.getPixel fx fy
        if fp.a == 0 then bg else fp
      else bg) canvas

/-- Fill a canvas with `0,0,0,0` (fully transparent) of the given size,
    upstream's disposal-to-background behaviour. -/
private def blankCanvas (width height : Nat) : Image PixelRGBA8 :=
  generateImage (fun _ _ => (⟨0, 0, 0, 0⟩ : PixelRGBA8)) width height

/-- Fold state for `decodeAllGifImages`: the previous frame's control
    extension (needed to know how to dispose of it) and the composited
    canvas so far (upstream's `gifAnimationApplyer`, minus its unused
    "current palette" state component — see the module doc-comment). -/
private structure CompositeState where
  prevControl : Option GraphicControlExtension
  canvas : Image PixelRGBA8

/-- Composite one more frame onto the running canvas, applying the
    *previous* frame's disposal method first (upstream's
    `gifAnimationApplyer`). -/
private def compositeGifFrame (screenWidth screenHeight : Nat) (globalMap : Option Palette)
    (st : CompositeState) (control : Option GraphicControlExtension) (img : GifImage) :
    Except String CompositeState := do
  let disposed :=
    match st.prevControl with
    | some gce =>
      match gce.disposalMethod with
      | .restoreBackground => blankCanvas screenWidth screenHeight
      | _ => st.canvas
    | none => st.canvas
  let frameDyn ← decodeFrameToDynamic globalMap control img
  let frameRgba := toRgba8 frameDyn
  let composited := drawFrameOnto disposed img.descriptor.left.toNat img.descriptor.top.toNat frameRgba
  pure { prevControl := control, canvas := composited }

/-- Monadic left-scan over `Except String`: like `List.scanl` but each step
    may fail, and the running list of intermediate states (not just the
    final one) is collected — needed since every composited canvas along
    the way is itself one output animation frame. No stdlib equivalent was
    found (`List.foldlM` collects only the final accumulator). -/
private def gifScanlM {α β : Type} (f : β → α → Except String β) (init : β) :
    List α → Except String (List β)
  | [] => pure [init]
  | a :: rest => do
    let b ← f init a
    let more ← gifScanlM f b rest
    pure (init :: more)

/-- Decode and fully composite every frame of a GIF (upstream's
    `decodeAllGifImages`). -/
private def decodeAllGifImages (header : GifHeader)
    (frames : List (Option GraphicControlExtension × GifImage)) : Except String (List DynamicImage) := do
  let width := header.screenDescriptor.screenWidth.toNat
  let height := header.screenDescriptor.screenHeight.toNat
  let init : CompositeState := { prevControl := none, canvas := blankCanvas width height }
  let states ← gifScanlM
    (fun st (c, img) => compositeGifFrame width height header.globalMap st c img) init frames
  pure ((states.drop 1).map (fun st => .rgba8 st.canvas))

-- ── Top-level decode entry points ──

/-- Decode only the first frame of a GIF file (upstream's
    `decodeFirstGifImage`), as whichever of `PalettedImage`/`DynamicImage`
    is cheapest to produce: a `PalettedImage` when the frame declares no
    transparency (its palette indices are exactly its pixel data), else a
    true-colour `DynamicImage` (transparency has already been substituted
    into RGBA). -/
private def decodeFirstFrame (header : GifHeader)
    (frames : List (Option GraphicControlExtension × GifImage)) :
    Except String (Sum DynamicImage PalettedImage) := do
  match frames with
  | [] => .error "GIF file has no image frames"
  | (control, img) :: _ => do
    let palette ← match img.localPalette, header.globalMap with
      | some p, _ => pure p
      | none, some p => pure p
      | none, none => .error "GIF frame has no local or global colour table"
    let indices ← decodeGifFrameIndices img
    let isTransparent := (control.map (·.transparentFlag)).getD false
    if isTransparent then do
      let some gce := control | .error "unreachable"
      pure (.inl (.rgba8 (substituteColorsWithTransparency palette (some gce.transparentColorIndex.toNat) indices)))
    else
      pure (.inr { indexedImage := indices, palette, hasAlpha := false })

-- The following three functions use explicit `match`, not `do`-notation,
-- deliberately: `Metadatas` lives in `Type 1` (it has an existential field),
-- so a `Except String (_ × Metadatas)` result can't be built inside the same
-- `do` block as the `Type 0` decode data that feeds it (mirroring
-- `Linen.Codec.Picture.Tiff`'s `decodeTiffWithPaletteAndMetadata`/
-- `decodeTiffWithMetadata`, which hit the identical universe wrinkle).
private def decodeGifCore (input : ByteArray) :
    Except String (Sum DynamicImage PalettedImage × Nat × Nat) :=
  match parseGif input with
  | .error e => .error e
  | .ok (header, frames) =>
    match decodeFirstFrame header frames with
    | .error e => .error e
    | .ok img =>
      .ok (img, header.screenDescriptor.screenWidth.toNat, header.screenDescriptor.screenHeight.toNat)

def decodeGifWithPaletteAndMetadata (input : ByteArray) :
    Except String (Sum DynamicImage PalettedImage × Metadatas) :=
  match decodeGifCore input with
  | .error e => .error e
  | .ok (img, w, h) => .ok (img, basicMetadata .gif w h)

def decodeGifWithMetadata (input : ByteArray) : Except String (DynamicImage × Metadatas) :=
  match decodeGifWithPaletteAndMetadata input with
  | .error e => .error e
  | .ok (.inl dyn, m) => .ok (dyn, m)
  | .ok (.inr pal, m) => .ok (.rgb8 (palettedToTrueColor pal), m)

def decodeGif (input : ByteArray) : Except String DynamicImage :=
  match decodeGifWithMetadata input with
  | .error e => .error e
  | .ok (img, _) => .ok img

/-- Decode and fully composite every frame of an animated (or single-frame)
    GIF file into a `List DynamicImage` (upstream's `decodeGifImages`). See
    the module doc-comment for why this is this port's animation-support
    surface. -/
def decodeGifImages (input : ByteArray) : Except String (List DynamicImage) := do
  let (header, frames) ← parseGif input
  decodeAllGifImages header frames

/-- Each frame's delay, in hundredths of a second (`0` if the frame has no
    graphic control extension), matching upstream's `getDelaysGifImages`. -/
def getDelaysGifImages (input : ByteArray) : Except String (List Nat) := do
  let (_header, frames) ← parseGif input
  pure (frames.map (fun (c, _) => (c.map (·.delay.toNat)).getD 0))

/-- A 256-entry greyscale palette (upstream's `greyPalette`), useful as a
    fallback global colour table. -/
def greyPalette : Palette :=
  generateImage (fun x _ => let v := UInt8.ofNat x; (⟨v, v, v⟩ : PixelRGB8)) 256 1

-- ── Encoding ──

/-- Delay before the next frame, in hundredths of a second. -/
abbrev GifDelay := Nat

/-- One frame to encode: its own palette-index pixel data, an optional
    local palette (falling back to a shared global palette when absent),
    an optional transparent index, and its display delay. -/
structure GifFrame where
  /-- Offset of this frame's canvas within the logical screen. -/
  left : Nat := 0
  top : Nat := 0
  localPalette : Option Palette := none
  transparent : Option Nat := none
  disposal : GifDisposalMethod := .unspecified
  delay : GifDelay := 0
  pixels : Image Pixel8

/-- A full animated-GIF encoding request (upstream's `GifEncode`). -/
structure GifEncode where
  screenWidth : Nat
  screenHeight : Nat
  palette : Option Palette := none
  background : Option Nat := none
  looping : GifLooping := .never
  frames : List GifFrame

-- ── Encode-side validation (upstream's `check*` family) ──

private def inBounds16 (n : Nat) : Bool := 0 < n ∧ n ≤ 0xFFFF

private def checkImageSizes (spec : GifEncode) : Except String Unit :=
  if !(inBounds16 spec.screenWidth ∧ inBounds16 spec.screenHeight) then
    .error "GIF screen size out of bounds"
  else if spec.frames.any (fun f => !(inBounds16 f.pixels.width ∧ inBounds16 f.pixels.height)) then
    .error "GIF frame size out of bounds"
  else pure ()

private def checkImagesInBounds (spec : GifEncode) : Except String Unit :=
  if spec.frames.any (fun f =>
      f.left + f.pixels.width > spec.screenWidth ∨ f.top + f.pixels.height > spec.screenHeight) then
    .error "GIF frame extends past the logical screen"
  else pure ()

private def isValidPalette (p : Palette) : Bool := p.height == 1 ∧ 0 < p.width ∧ p.width ≤ 256

private def checkPaletteValidity (spec : GifEncode) : Except String Unit :=
  if (spec.palette.map (fun p => !isValidPalette p)).getD false then
    .error "GIF global palette has an invalid size"
  else if spec.frames.any (fun f => (f.localPalette.map (fun p => !isValidPalette p)).getD false) then
    .error "GIF frame has a local palette with an invalid size"
  else pure ()

/-- Is `idx` a valid index into a frame's applicable palette (local, else
    global)? Mirrors upstream's `checkIndexInPalette` pattern-match
    priority exactly: a present local palette always wins over the global
    one. -/
private def checkIndexInPalette (global localPal : Option Palette) (idx : Nat) : Bool :=
  match global, localPal with
  | _, some p => idx < p.width
  | some p, none => idx < p.width
  | none, none => false

private def checkIndexAbsentFromPalette (spec : GifEncode) : Except String Unit :=
  if spec.frames.any (fun f =>
      f.pixels.data.any (fun px => !checkIndexInPalette spec.palette f.localPalette px.toNat)) then
    .error "GIF frame uses a palette index outside its colour table"
  else pure ()

private def checkBackground (spec : GifEncode) : Except String Unit :=
  match spec.background with
  | none => pure ()
  | some bg => if checkIndexInPalette spec.palette none bg then pure ()
               else .error "GIF background index outside the global colour table"

private def checkTransparencies (spec : GifEncode) : Except String Unit :=
  if spec.frames.any (fun f =>
      match f.transparent with
      | none => false
      | some t => !checkIndexInPalette spec.palette f.localPalette t) then
    .error "GIF frame's transparent index outside its colour table"
  else pure ()

/-- Smallest `k` with `2 ^ k ≥ p.width` (upstream's `computeColorTableSize`),
    bounded to `1 .. 8` since a valid palette (checked by
    `checkPaletteValidity`) never exceeds `256` entries. -/
private def computeColorTableSize (p : Palette) : Nat :=
  Id.run do
    for k in [1:9] do
      if 2 ^ k ≥ p.width then return k
    return 8

-- ── Encode-side block assembly ──

private def buildImageDescriptor (f : GifFrame) : ImageDescriptor :=
  { left := f.left.toUInt16, top := f.top.toUInt16
    width := f.pixels.width.toUInt16, height := f.pixels.height.toUInt16
    hasLocalMap := f.localPalette.isSome
    isInterlaced := false
    isImgDescriptorSorted := false
    localColorTableSize := (f.localPalette.map computeColorTableSize).getD 1 }

private def buildGraphicControlExtension (f : GifFrame) : GraphicControlExtension :=
  { disposalMethod := f.disposal
    userInputFlag := false
    transparentFlag := f.transparent.isSome
    delay := f.delay.toUInt16
    transparentColorIndex := (f.transparent.getD 0).toUInt8 }

/-- The LZW minimum code size for a colour table with `2 ^ bits` entries
    (at least `2`, matching upstream's implicit `max 2` via GIF's own rule
    that the minimum code size is never `< 2`). -/
private def minCodeSizeForBits (bits : Nat) : Nat := max 2 bits

private def buildFrameBlocks (globalPalette : Option Palette) (f : GifFrame) : Builder :=
  let hasControl := f.transparent.isSome ∨ f.disposal != .unspecified ∨ f.delay != 0
  let controlBuilder := if hasControl then putGraphicControlExtension (buildGraphicControlExtension f)
                         else Builder.empty
  let effectiveBits := (f.localPalette.orElse (fun _ => globalPalette)).map computeColorTableSize |>.getD 8
  let minCodeSize := minCodeSizeForBits effectiveBits
  let lzwBytes := lzwEncode minCodeSize f.pixels.data
  let descriptor := buildImageDescriptor f
  let gifImg : GifImage :=
    { descriptor, localPalette := f.localPalette, lzwMinCodeSize := UInt8.ofNat minCodeSize
      imageData := lzwBytes.toList }
  controlBuilder ++ putGifImage gifImg

/-- Validate and encode a full `GifEncode` spec into a complete GIF file
    (upstream's `encodeComplexGifImage`). -/
def encodeComplexGifImage (spec : GifEncode) : Except String Data.ByteString := do
  if spec.frames.isEmpty then .error "GIF encoding needs at least one frame"
  checkImageSizes spec
  checkImagesInBounds spec
  checkPaletteValidity spec
  checkBackground spec
  checkTransparencies spec
  checkIndexAbsentFromPalette spec
  let multiFrame := spec.frames.length > 1
  let screenDescriptor : LogicalScreenDescriptor :=
    { screenWidth := spec.screenWidth.toUInt16, screenHeight := spec.screenHeight.toUInt16
      hasGlobalMap := spec.palette.isSome
      colorResolution := 8
      isColorTableSorted := false
      colorTableSize := (spec.palette.map computeColorTableSize).getD 8
      backgroundIndex := (spec.background.getD 0).toUInt8 }
  let header : GifHeader := { screenDescriptor, globalMap := spec.palette }
  let framesBuilder := (spec.frames.map (buildFrameBlocks spec.palette)).foldl (· ++ ·) Builder.empty
  let out := putGifHeader multiFrame header ++ putLooping spec.looping ++ framesBuilder ++
    Builder.singleton 0x3B
  pure out.toStrictByteString

/-- Encode several already-paletted frames sharing one global colour table
    into an animated GIF (upstream's `encodeGifImages`). Each `Image Pixel8`
    is quantised from `img` via `Linen.Codec.Picture.ColorQuant.palettize`
    to obtain both the shared palette and each frame's indices. -/
def encodeGifImages (looping : GifLooping) (delay : GifDelay) (imgs : List (Image PixelRGB8)) :
    Except String Data.ByteString :=
  match imgs with
  | [] => .error "GIF encoding needs at least one frame"
  | first :: _ =>
    let (_, palette) := palettize defaultPaletteOptions first
    let frames := imgs.map (fun img =>
      let (indexed, _) := palettize defaultPaletteOptions img
      ({ pixels := indexed, delay, disposal := .doNotDispose } : GifFrame))
    encodeComplexGifImage
      { screenWidth := first.width, screenHeight := first.height, palette := some palette
        looping, frames }

/-- Encode a single true-colour image as a one-frame GIF, quantising its
    own palette (upstream's `encodeGifImage`). -/
def encodeGifImage (img : Image PixelRGB8) : Except String Data.ByteString :=
  encodeGifImages .never 0 [img]

/-- Encode a single already-paletted image with an explicit palette
    (upstream's `encodeGifImageWithPalette`). -/
def encodeGifImageWithPalette (img : Image Pixel8) (palette : Palette) : Except String Data.ByteString :=
  encodeComplexGifImage
    { screenWidth := img.width, screenHeight := img.height, palette := some palette
      frames := [{ pixels := img }] }

end Codec.Picture
