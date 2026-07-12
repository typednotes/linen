import Linen.Codec.Picture.Types
import Linen.Codec.Picture.Metadata
import Linen.Data.ByteString.Builder

/-!
  Port of `Codec.Picture.HDR` from the `JuicyPixels` package (see
  `docs/imports/JuicyPixels/dependencies.md`, module 10 of 29). Decoding and
  encoding of the Radiance/RGBE (`.hdr`/`.pic`) high-dynamic-range format.

  ## Design

  - The Radiance header is line-oriented ASCII text (a run of `key=value` /
    comment / blank lines, terminated by a blank line), not a fixed binary
    layout like `Bitmap.lean`/`Tga.lean`'s headers, so it is parsed with a
    small hand-rolled, `Except`-returning recursive scanner over `List UInt8`
    rather than `Std.Internal.Parsec.ByteArray.Parser`. Every scanner below
    recurses by directly matching `[]` / `c :: rest` (or an explicit `Nat`
    counter) and calling itself on the matched tail, so Lean's structural
    recursion check accepts each of them with no `termination_by` at all —
    the same idea as `Tga.lean`'s explicit-argument fix, pushed one step
    further by never introducing a `List.drop`-computed remainder.

  - Upstream's info lines are collected into a `Vector (String, String)` but
    are only ever consulted for one purpose: extracting the mandatory
    `FORMAT=` value (nothing downstream, including `put`'s header-encoder,
    reads the rest), so this port scans for `FORMAT=` directly and discards
    every other line rather than building and threading a list of pairs.

  - `RadianceFormat` (`rgbe`/`xyze`) is parsed and validated exactly as
    upstream does, but — faithfully mirroring upstream's own
    `decodeRadiancePicture`, which always interprets scanline bytes as RGBE
    regardless of the declared format — it is not actually used to pick a
    decoding path.

  - The size line's two numbers are signed (`-Y <h> +X <w>`), but
    `decodeRadiancePicture` only ever consumes their absolute value (no
    row/column flip is ever applied, unlike `Bitmap.lean`'s signed-height
    convention); this port keeps the sign only long enough to discard it.

  - Upstream threads an `ST s`/`STVector` scratch buffer through its
    old-style and new-style scanline decoders, plus an unsafe backward
    buffer read (`copyPrevColor`) for the old-style run marker's "repeat the
    previous pixel" step. Both decoders are ported as plain structural
    recursion over an immutable `Array UInt8`; the old-style decoder
    replaces the unsafe backward read with an explicit `lastColor`
    accumulator (defaulting to `⟨0,0,0,0⟩` for the edge case of a run marker
    with no preceding pixel — a case upstream does not handle safely either).

  - `encodeRLENewStyleHDR`'s per-channel run-length encoder is ported as a
    single bounded `for` loop (`Id.run`) rather than recursion, since its
    read index advances by exactly one input byte per step regardless of
    which branch is taken — a shape `Std.Range` iteration handles with no
    termination proof, while still reproducing upstream's exact
    run/copy/127-cap state machine.

  - `writeHDR`/`writeRLENewStyleHDR` (trivial `IO` file-writers) are dropped,
    matching this library's convention of leaving file I/O to the caller.
-/

namespace Codec.Picture

open Data.ByteString (Builder)

-- ── `RGBE` ──

/-- One packed Radiance pixel: three 8-bit mantissa channels sharing a common
    8-bit exponent `e`. -/
structure RGBE where
  r : UInt8
  g : UInt8
  b : UInt8
  e : UInt8
  deriving BEq, Inhabited

/-- $2^{e - 136}$, the scale factor `RGBE.toFloat`/`RGBE.ofFloat` share
    (`136 = 128 + 8`, matching upstream's `encodeFloat 1 (e - (128+8))`). -/
private def rgbeScale (e : UInt8) : Float32 :=
  Float32.exp2 (e.toNat.toFloat32 - 136.0)

/-- Convert a packed `RGBE` pixel to a floating-point RGB triple. -/
def RGBE.toFloat (c : RGBE) : PixelRGBF :=
  let f := rgbeScale c.e
  { r := c.r.toFloat32 * f, g := c.g.toFloat32 * f, b := c.b.toFloat32 * f }

/-- Convert a floating-point RGB triple to a packed `RGBE` pixel.

    Upstream uses Haskell's `exponent`/`significand` (`d = significand d *
    2 ^ exponent d`, `0.5 ≤ significand d < 1`); `Float32` has no such
    primitive, so this is computed algebraically instead: for `d > 0`,
    `exponent d = ⌊log₂ d⌋ + 1`, and since `significand d = d / 2 ^ exponent
    d`, upstream's `coeff = significand d * 255.9999 / d` simplifies to
    `255.9999 / 2 ^ exponent d`. -/
def RGBE.ofFloat (p : PixelRGBF) : RGBE :=
  let d := max p.r (max p.g p.b)
  if d ≤ (1e-32 : Float32) then { r := 0, g := 0, b := 0, e := 0 }
  else
    let exponent := (Float32.log2 d).floor + 1
    let coeff := (255.9999 : Float32) / Float32.exp2 exponent
    let fix (v : Float32) : UInt8 := (max (0 : Float32) (min (255 : Float32) ((v * coeff).floor))).toUInt8
    { r := fix p.r, g := fix p.g, b := fix p.b, e := (exponent + 128).toUInt8 }

-- ── `RadianceFormat` ──

/-- The two pixel encodings a Radiance file's `FORMAT=` line may declare
    (validated on decode, but not otherwise used — see the module
    doc-comment). -/
inductive RadianceFormat where
  | rgbe
  | xyze
  deriving BEq

private def asciiBytes (s : String) : List UInt8 := s.toList.map (fun c => c.toNat.toUInt8)

private def radianceFormatOfBytes (bytes : List UInt8) : Except String RadianceFormat :=
  if bytes == asciiBytes "32-bit_rle_rgbe" then .ok .rgbe
  else if bytes == asciiBytes "32-bit_rle_xyze" then .ok .xyze
  else .error "Unrecognized radiance format"

private def stringOfRadianceFormat : RadianceFormat → String
  | .rgbe => "32-bit_rle_rgbe"
  | .xyze => "32-bit_rle_xyze"

-- ── Header parsing ──

/-- Split `line` at its first `'='` (`61`); `none` if there is none. -/
private def splitAtEquals : List UInt8 → Option (List UInt8 × List UInt8)
  | [] => none
  | (61 : UInt8) :: rest => some ([], rest)
  | c :: rest =>
      match splitAtEquals rest with
      | none => none
      | some (k, v) => some (c :: k, v)

/-- Scan header lines (each terminated by `'\n'` = `10`), skipping `'#'`-
    (`35`) prefixed comments, recording the `FORMAT=` value if seen, and
    stopping at the first blank line. Returns the declared format's raw
    bytes and the bytes following the blank line. -/
private def scanHeaderLines : List UInt8 → List UInt8 → Option (List UInt8) → Except String (List UInt8 × List UInt8)
  | [], _, _ => .error "Unexpected end of radiance header"
  | (10 : UInt8) :: rest, lineAcc, fmt =>
      let line := lineAcc.reverse
      if line.isEmpty then
        match fmt with
        | some f => .ok (f, rest)
        | none => .error "No radiance format specified"
      else if line.head! == (35 : UInt8) then
        scanHeaderLines rest [] fmt
      else
        match splitAtEquals line with
        | some (k, v) => scanHeaderLines rest [] (if k == asciiBytes "FORMAT" then some v else fmt)
        | none => scanHeaderLines rest [] fmt
  | c :: rest, lineAcc, fmt => scanHeaderLines rest (c :: lineAcc) fmt

/-- Consume digit characters (`'0'..'9'`, `48..57`) into `acc`, stopping at
    (and discarding) the first non-digit. -/
private def scanDigits (acc : Nat) : List UInt8 → (Nat × List UInt8)
  | [] => (acc, [])
  | c :: rest => if c.toNat ≥ 48 ∧ c.toNat ≤ 57 then scanDigits (acc * 10 + (c.toNat - 48)) rest else (acc, rest)

/-- Parse one signed size field: a sign (`'+'`/`'-'`), an axis letter
    (`'X'`/`'Y'`), a space, then one or more digits and a discarded
    terminator. -/
private def scanSizeNum : List UInt8 → Except String (Int × List UInt8)
  | sign :: axis :: (32 : UInt8) :: d :: rest =>
      if (sign == (43 : UInt8) ∨ sign == (45 : UInt8)) ∧ (axis == (88 : UInt8) ∨ axis == (89 : UInt8)) then
        let (n, rest') := scanDigits 0 (d :: rest)
        .ok ((if sign == (45 : UInt8) then -(n : Int) else (n : Int)), rest')
      else .error "Invalid radiance size declaration"
  | _ => .error "Invalid radiance size declaration"

private def radianceSignature : List UInt8 := asciiBytes "#?RADIANCE\n"

/-- Parse a Radiance file's header, returning its declared format, its
    (unsigned) height and width, and the remaining pixel-data bytes. -/
private def decodeRadianceHeader (bytes : List UInt8) : Except String (RadianceFormat × Nat × Nat × List UInt8) :=
  if bytes.take radianceSignature.length != radianceSignature then .error "Invalid radiance file signature"
  else
    match scanHeaderLines (bytes.drop radianceSignature.length) [] none with
    | .error e => .error e
    | .ok (fmtBytes, rest0) =>
        match radianceFormatOfBytes fmtBytes with
        | .error e => .error e
        | .ok format =>
            match scanSizeNum rest0 with
            | .error e => .error e
            | .ok (h, rest1) =>
                match scanSizeNum rest1 with
                | .error e => .error e
                | .ok (w, rest2) => .ok (format, h.natAbs, w.natAbs, rest2)

-- ── Pixel-data decoding ──

/-- Write `n` copies of `color` (4 bytes each) into `arr` at consecutive
    4-byte slots starting at `writeIndex`. -/
private def writeRunRGBE (arr : Array UInt8) (writeIndex n : Nat) (color : RGBE) : Array UInt8 :=
  Id.run do
    let mut a := arr
    for i in [0:n] do
      let idx := writeIndex + i
      if 4 * idx + 3 < a.size then
        a := ((a.set! (4*idx) color.r).set! (4*idx+1) color.g).set! (4*idx+2) color.b |>.set! (4*idx+3) color.e
    pure a

/-- Write `n` copies of `v` into `arr` at consecutive positions starting at
    `writeIndex`. -/
private def writeRepeated1 (arr : Array UInt8) (writeIndex n : Nat) (v : UInt8) : Array UInt8 :=
  Id.run do
    let mut a := arr
    for i in [0:n] do
      let idx := writeIndex + i
      if idx < a.size then a := a.set! idx v
    pure a

/-- Old-style RLE: sequential `RGBE` quads, where `(1,1,1,e)` repeats the
    previous pixel `e <<< shift` times (`shift` accumulates by `8` across
    consecutive run markers, resetting after any literal quad). Every branch
    matches at least the leading 4-byte quad and recurses on the resulting
    tail, so this is accepted as structural recursion with no explicit
    termination proof. -/
private def decodeOldStyleAux (width : Nat) :
    List UInt8 → Nat → Nat → RGBE → Array UInt8 → (Array UInt8 × List UInt8)
  | r :: g :: b :: e :: rest, writeIndex, shift, lastColor, arr =>
      if writeIndex ≥ width then (arr, r :: g :: b :: e :: rest)
      else if r == 1 ∧ g == 1 ∧ b == 1 then
        let count := min (e.toNat <<< shift) (width - writeIndex)
        decodeOldStyleAux width rest (writeIndex + count) (shift + 8) lastColor (writeRunRGBE arr writeIndex count lastColor)
      else
        let color : RGBE := { r, g, b, e }
        decodeOldStyleAux width rest (writeIndex + 1) 0 color (writeRunRGBE arr writeIndex 1 color)
  | rest, _, _, _, arr => (arr, rest)

private def decodeOldStyleScanline (width : Nat) (bytes : List UInt8) : (Array UInt8 × List UInt8) :=
  decodeOldStyleAux width bytes 0 0 default (Array.replicate (width * 4) (0 : UInt8))

/-- New-style per-channel RLE: a code byte `> 128` is a run of `code &&&
    0x7F` copies of the following value byte; a code byte `≤ 128` is a
    literal run of `code` raw bytes. Every branch consumes at least one
    element of the matched list and recurses on its tail, so this too is
    accepted as structural recursion with no explicit termination proof. -/
private def decodePlaneAux (width : Nat) :
    List UInt8 → Nat → Nat → Array UInt8 → (Array UInt8 × List UInt8)
  | b :: rest, writeIndex, n + 1, arr =>
      let arr' := if writeIndex < arr.size then arr.set! writeIndex b else arr
      decodePlaneAux width rest (writeIndex + 1) n arr'
  | code :: rest, writeIndex, 0, arr =>
      if writeIndex ≥ width then (arr, code :: rest)
      else if code.toNat > 128 then
        match rest with
        | val :: rest' =>
            let count := min ((code &&& 0x7F).toNat) (width - writeIndex)
            decodePlaneAux width rest' (writeIndex + count) 0 (writeRepeated1 arr writeIndex count val)
        | [] => (arr, [])
      else decodePlaneAux width rest writeIndex code.toNat arr
  | [], _, _, arr => (arr, [])

/-- Recombine four independently-decoded `width`-length planes into an
    interleaved `RGBE` byte stream. -/
private def interleavePlanes (width : Nat) (rP gP bP eP : Array UInt8) : Array UInt8 :=
  Id.run do
    let mut out := Array.mkEmpty (width * 4)
    for i in [0:width] do
      out := out |>.push (rP.getD i 0) |>.push (gP.getD i 0) |>.push (bP.getD i 0) |>.push (eP.getD i 0)
    pure out

/-- Decode one scanline, auto-detecting old- vs. new-style RLE from its
    leading 4-byte marker (`r = g = 2`, with the declared width packed into
    `b`,`e`). A new-style marker whose declared width mismatches the image's
    real `width` is a genuine decode error, propagated rather than silently
    truncated or padded. -/
private def decodeScanline (width : Nat) (bytes : List UInt8) : Except String (Array UInt8 × List UInt8) :=
  match bytes with
  | r :: g :: b :: e :: rest =>
      if r == (2 : UInt8) ∧ g == (2 : UInt8) then
        let lineLen := (b.toNat <<< 8) ||| e.toNat
        if lineLen ≠ width then .error "Invalid scanline size"
        else
          let (rP, rest1) := decodePlaneAux width rest 0 0 (Array.replicate width (0 : UInt8))
          let (gP, rest2) := decodePlaneAux width rest1 0 0 (Array.replicate width (0 : UInt8))
          let (bP, rest3) := decodePlaneAux width rest2 0 0 (Array.replicate width (0 : UInt8))
          let (eP, rest4) := decodePlaneAux width rest3 0 0 (Array.replicate width (0 : UInt8))
          .ok (interleavePlanes width rP gP bP eP, rest4)
      else .ok (decodeOldStyleScanline width bytes)
  | _ => .ok (decodeOldStyleScanline width bytes)

/-- Append one decoded scanline's interleaved `RGBE` bytes to `acc` as `r,g,b`
    float triples. -/
private def appendScanlineFloats (width : Nat) (rgbeBytes : Array UInt8) (acc : Array Float32) : Array Float32 :=
  Id.run do
    let mut a := acc
    for x in [0:width] do
      let base := x * 4
      let c : RGBE :=
        { r := rgbeBytes.getD base 0, g := rgbeBytes.getD (base+1) 0,
          b := rgbeBytes.getD (base+2) 0, e := rgbeBytes.getD (base+3) 0 }
      let p := c.toFloat
      a := a |>.push p.r |>.push p.g |>.push p.b
    pure a

/-- Decode `n` remaining scanlines (a genuine "scanlines left" countdown
    initialized from the image's real `height`, not an arbitrary fuel
    bound), accumulating each one's floating-point pixel triples. -/
private def decodeAllScanlines (width : Nat) : Nat → List UInt8 → Array Float32 → Except String (Array Float32)
  | 0, _, acc => .ok acc
  | n + 1, bytes, acc =>
      match decodeScanline width bytes with
      | .error e => .error e
      | .ok (rgbeBytes, rest) => decodeAllScanlines width n rest (appendScanlineFloats width rgbeBytes acc)

private def decodeHDRPixels (width height : Nat) (bytes : List UInt8) : Except String (Image PixelRGBF) :=
  match decodeAllScanlines width height bytes #[] with
  | .error e => .error e
  | .ok data => .ok { width, height, data }

-- ── Decoding pipeline ──

/-- Decode a Radiance HDR file into a floating-point `DynamicImage`, plus its
    metadata. -/
def decodeHDRWithMetadata (input : ByteArray) : Except String (DynamicImage × Metadatas) :=
  match decodeRadianceHeader input.toList with
  | .error e => .error e
  | .ok (_format, height, width, rest) =>
      match decodeHDRPixels width height rest with
      | .error e => .error e
      | .ok img => .ok (.rgbF img, basicMetadata .hdr width height)

/-- Decode a Radiance HDR file into a floating-point `DynamicImage`. -/
def decodeHDR (input : ByteArray) : Except String DynamicImage :=
  match decodeHDRWithMetadata input with
  | .error e => .error e
  | .ok (img, _) => .ok img

-- ── Encoding ──

private def headerBuilder (format : RadianceFormat) (width height : Nat) : Builder :=
  Builder.stringUtf8 "#?RADIANCE\n" ++
  Builder.stringUtf8 "FORMAT=" ++ Builder.stringUtf8 (stringOfRadianceFormat format) ++ Builder.stringUtf8 "\n\n" ++
  Builder.stringUtf8 "-Y " ++ Builder.intDec (height : Int) ++ Builder.stringUtf8 " +X " ++ Builder.intDec (width : Int) ++
  Builder.stringUtf8 "\n"

private def builderOfByteArray (b : Array UInt8) : Builder :=
  b.foldl (fun acc byte => acc ++ Builder.word8 byte) Builder.empty

/-- An image's raw, uncompressed `RGBE` pixel bytes, row-major. -/
private def rgbeBytesOfImage (img : Image PixelRGBF) : Array UInt8 :=
  Id.run do
    let mut out := Array.mkEmpty (img.width * img.height * 4)
    for y in [0:img.height] do
      for x in [0:img.width] do
        let c := RGBE.ofFloat (img.getPixel x y)
        out := out |>.push c.r |>.push c.g |>.push c.b |>.push c.e
    pure out

/-- Encode an image as an uncompressed Radiance HDR file. -/
def encodeRawHDR (img : Image PixelRGBF) : Data.ByteString :=
  (headerBuilder .rgbe img.width img.height ++ builderOfByteArray (rgbeBytesOfImage img)).toStrictByteString

/-- Encode an image as a Radiance HDR file (alias for `encodeRawHDR`,
    matching upstream's `encodeHDR`). -/
def encodeHDR (img : Image PixelRGBF) : Data.ByteString := encodeRawHDR img

/-- Run-length encode a single scanline channel, mirroring upstream's
    `encodeScanlineColor` state machine: a repeat run (≥4 identical values)
    is emitted as one or more `(count|0x80, value)` tokens capped at `127`
    per token, and any other stretch of values is emitted as `(count, ...)`
    literal tokens, also capped at `127`. `idx` advances by exactly one
    input element every iteration regardless of branch, so this is a single
    bounded `for` loop rather than recursion. -/
private def encodeChannelRLE (vec : Array UInt8) : Array UInt8 :=
  Id.run do
    let n := vec.size
    if n == 0 then return #[]
    let mut out : Array UInt8 := #[]
    let mut run : Nat := 1
    let mut cpy : Nat := 0
    for idx in [1:n] do
      let val := vec[idx]!
      let prev := vec[idx - 1]!
      if run == 127 then
        out := out |>.push ((127 : UInt8) ||| 0x80) |>.push prev
        run := 1; cpy := 0
      else if cpy == 127 then
        out := (out.push (127 : UInt8)) ++ vec.extract (idx - 127) idx
        run := 1; cpy := 0
      else if run > 0 then
        if val == prev then run := run + 1
        else if run < 4 then
          cpy := run + 1
          run := 0
        else
          out := out |>.push (run.toUInt8 ||| 0x80) |>.push prev
          run := 1; cpy := 0
      else
        if val == prev then
          out := (out.push (cpy - 1).toUInt8) ++ vec.extract (idx - cpy) (idx - 1)
          run := 2; cpy := 0
        else cpy := cpy + 1
    if run > 0 then
      out := out |>.push (run.toUInt8 ||| 0x80) |>.push vec[n - 1]!
    else
      out := (out.push cpy.toUInt8) ++ vec.extract (n - cpy) n
    pure out

private def channelOfScanline (img : Image PixelRGBF) (y : Nat) (extract : RGBE → UInt8) : Array UInt8 :=
  Id.run do
    let mut out := Array.mkEmpty img.width
    for x in [0:img.width] do
      out := out.push (extract (RGBE.ofFloat (img.getPixel x y)))
    pure out

private def encodeScanlineRLE (img : Image PixelRGBF) (y : Nat) : Array UInt8 :=
  let marker : Array UInt8 := #[2, 2, (img.width >>> 8).toUInt8, (img.width &&& 0xFF).toUInt8]
  marker ++ encodeChannelRLE (channelOfScanline img y RGBE.r) ++ encodeChannelRLE (channelOfScanline img y RGBE.g) ++
    encodeChannelRLE (channelOfScanline img y RGBE.b) ++ encodeChannelRLE (channelOfScanline img y RGBE.e)

/-- Encode an image as a new-style-RLE-compressed Radiance HDR file. -/
def encodeRLENewStyleHDR (img : Image PixelRGBF) : Data.ByteString :=
  let body : Array UInt8 := Id.run do
    let mut out := #[]
    for y in [0:img.height] do
      out := out ++ encodeScanlineRLE img y
    pure out
  (headerBuilder .rgbe img.width img.height ++ builderOfByteArray body).toStrictByteString

end Codec.Picture
