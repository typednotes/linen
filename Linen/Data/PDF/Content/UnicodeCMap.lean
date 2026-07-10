/-
  Data.PDF.Content.UnicodeCMap — `ToUnicode` CMap parsing

  Ports `Pdf.Content.UnicodeCMap` from Hackage's `pdf-toolbox-content`
  (https://github.com/Yuras/pdf-toolbox, `content/lib/Pdf/Content/UnicodeCMap.hs`,
  fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/content/lib/Pdf/Content/UnicodeCMap.hs`),
  module 9 of the `pdf-toolbox-content` import documented in
  `docs/imports/PdfToolboxContent/dependencies.md`.

  A font dictionary can carry a `"ToUnicode"` entry, referencing a stream
  holding a *Unicode CMap*: a small PostScript-flavoured mini-language
  (defined by Adobe's technical note #5411) describing which byte sequences
  (glyph codes) map to which Unicode text. This module parses the three
  constructs that matter for text extraction — `begincodespacerange`,
  `beginbfchar`, `beginbfrange` — and the two lookups built on top of them.

  ## `MonadFail`/`Except` (Scope note)

  Upstream's `fromHex` is typed against a generic `Fail.MonadFail m`,
  flagged in its own source as `-- XXX: wtf?!` — upstream itself considers
  this smelly. Per `docs/imports/PdfToolboxContent/dependencies.md`'s scope
  note, this is committed here to the concrete `Parser` monad's own `fail`
  (which underlies `Except String` via `Parser.run`) rather than reproduced
  as a polymorphic-monad signature: a legitimate simplification of an
  acknowledged code smell, not a weakening of behaviour — the sole caller
  already instantiates it at `Parser` anyway.

  ## Backtracking and termination

  Built directly against `Std.Internal.Parsec.ByteArray.Parser`, following
  the conventions already established in
  `Data.PDF.Core.Parsers.Object`/`Data.PDF.Core.Parsers.Util`: every
  alternative that can consume input before possibly failing is wrapped in
  `attempt` before `<|>` (see that module's "Backtracking" note).

  `skipTillParser` (search forward for a marker keyword, upstream's
  `P.choice [p, P.anyChar >> skipTillParser p]`) and `times`/`hexPairs`/
  `utf16beToChars` below are not visibly structurally decreasing to Lean's
  termination checker as written in `attoparsec`; each is given an explicit
  `fuel`/count parameter that decreases by exactly one every recursive step
  (a directly structural recursion needing no `termination_by`), seeded
  from a genuine bound already available at the call site — `it
  .remainingBytes` for `skipTillParser` (exactly `Data.PDF.Core.Parsers
  .Object`'s own `parseObjectFuel` pattern: a well-formed search can never
  need more steps than there are bytes left) and the parsed repeat count
  `n`/list lengths elsewhere.

  ## Combining maps from repeated blocks

  A CMap can contain more than one `beginbfchar`/`beginbfrange` block.
  Upstream combines their `Map Int Text` contributions with `Map.union`,
  whose *left* argument's keys win on a duplicate code — and combines
  `charsParser`'s block-by-block results themselves with `Map.union`, again
  left-biased, so an **earlier** block's entry for a code wins over a
  later block's. `Std.HashMap.ofList` instead lets a *later* list entry
  overwrite an earlier one for the same key, so wherever this port needs
  "earlier wins", it builds the combined association list in
  earlier-preferred order and then calls `Std.HashMap.ofList` on its
  `.reverse` — the later (correctly-losing) entries land first and get
  overwritten by the earlier (correctly-winning) ones, exactly inverting
  `ofList`'s last-wins default into the needed first-wins order.
-/
import Linen.Data.PDF.Core.Parsers.Util
import Linen.Data.ByteString
import Linen.Data.Word8
import Linen.Data.Text
import Std.Internal.Parsec.ByteArray
import Std.Data.HashMap

namespace Data.PDF.Content.UnicodeCMap

open Std.Internal.Parsec Std.Internal.Parsec.ByteArray
open Data.PDF.Core.Parsers.Util (skipSpace)

/-! ── The `UnicodeCMap` type ── -/

/-- A parsed Unicode CMap. Mirrors upstream's `UnicodeCMap` record. -/
structure UnicodeCMap where
  /-- The codespace ranges declared by `begincodespacerange`/
      `endcodespacerange`, as `(low, high)` byte-string pairs — used by
      `nextGlyph` to work out how many bytes make up the next glyph code. -/
  codeRanges : List (Data.ByteString × Data.ByteString)
  /-- Individual glyph-code → text mappings, from `beginbfchar`/`endbfchar`
      blocks (and any `beginbfrange`/`endbfrange` block using the
      hex-*array* form). -/
  chars : Std.HashMap Nat Data.Text
  /-- Contiguous glyph-code ranges, from `beginbfrange`/`endbfrange` blocks
      using the single-hex-string form: `(start, end, firstChar)`, where
      glyph code `start + k` maps to `Char.ofNat (firstChar.toNat + k)`. -/
  ranges : List (Nat × Nat × Char)
deriving Repr

/-! ── Byte/code helpers ── -/

/-- Interpret a `ByteString` as a big-endian natural number. Mirrors
    upstream's `toCode`. -/
def toCode (bs : Data.ByteString) : Nat :=
  bs.unpack.foldl (fun acc b => acc * 256 + b.toNat) 0

/-- Split a list of UTF-16 code units into (16-bit) surrogate-decoded
    `Char`s. Structural recursion on the list; a high surrogate is only
    combined with the *immediately following* element (still a subterm of
    the original list), everything else copies through unchanged. -/
private def utf16UnitsToChars : List UInt16 → List Char
  | [] => []
  | [u] => [Char.ofNat u.toNat]
  | hi :: lo :: rest =>
    if 0xD800 ≤ hi && hi ≤ 0xDBFF && 0xDC00 ≤ lo && lo ≤ 0xDFFF then
      let cp := 0x10000 + ((hi.toNat - 0xD800) <<< 10) + (lo.toNat - 0xDC00)
      Char.ofNat cp :: utf16UnitsToChars rest
    else
      Char.ofNat hi.toNat :: utf16UnitsToChars (lo :: rest)

/-- Pack raw bytes, two at a time, into big-endian UTF-16 code units. A
    trailing odd byte is dropped (this only arises from malformed input;
    upstream's `Text.decodeUtf16BE` on an odd-length `ByteString` is itself
    documented upstream as "undefined" for that case). -/
private def bytesToUtf16Units : List UInt8 → List UInt16
  | [] => []
  | [_] => []
  | b0 :: b1 :: rest => (b0.toUInt16 <<< 8 ||| b1.toUInt16) :: bytesToUtf16Units rest

/-- Decode a big-endian UTF-16 `ByteString` into `Text`. Mirrors upstream's
    `Text.decodeUtf16BE`, used here (rather than added to
    `Data.Text.Encoding`) because it is the sole consumer of a strict
    big-endian (as opposed to platform/BOM-dependent) UTF-16 decoder in
    `linen` so far. -/
def decodeUtf16BE (bs : Data.ByteString) : Data.Text :=
  Data.Text.pack (utf16UnitsToChars (bytesToUtf16Units bs.unpack))

/-! ── Low-level combinators ── -/

/-- Repeatedly try `p`, skipping one byte at a time on failure, until `p`
    succeeds or the input is exhausted. Mirrors upstream's
    `skipTillParser p = P.choice [p, P.anyChar >> skipTillParser p]`. See
    the module doc-comment's "Backtracking and termination" section for why
    `fuel` here is a genuine bound, not an arbitrary cap. -/
private def skipTillFuel (p : Parser α) : Nat → Parser α
  | 0 => p
  | fuel + 1 => attempt p <|> (any *> skipTillFuel p fuel)

/-- `skipTillParser p` — see `skipTillFuel`. -/
def skipTillParser (p : Parser α) : Parser α := fun it => skipTillFuel p it.remainingBytes it

/-- Run `p` exactly `n` times, collecting the results in order. Mirrors
    upstream's `Control.Monad.replicateM`. -/
private def times (n : Nat) (p : Parser α) : Parser (Array α) :=
  match n with
  | 0 => pure #[]
  | n + 1 => do
    let x ← p
    let xs ← times n p
    pure (#[x] ++ xs)

/-! ── Hex strings/arrays ── -/

/-- Hex-digit value of an ASCII hex-digit byte (undefined for anything
    else — always guarded by `Data.Word8.isHexDigit` at call sites). -/
private def hexVal (b : UInt8) : UInt8 :=
  if b ≥ '0'.toUInt8 && b ≤ '9'.toUInt8 then b - '0'.toUInt8
  else if b ≥ 'a'.toUInt8 && b ≤ 'f'.toUInt8 then b - 'a'.toUInt8 + 10
  else b - 'A'.toUInt8 + 10

/-- Pair up hex digits into bytes; `none` on an odd digit count. Mirrors
    upstream's `fromHex`/`Data.ByteString.Base16.decode`, which also fails
    (rather than padding) on an odd-length hex string. -/
private def hexPairs : List UInt8 → Option (List UInt8)
  | [] => some []
  | [_] => none
  | d1 :: d2 :: rest =>
    match hexPairs rest with
    | some bytes => some ((hexVal d1 * 16 + hexVal d2) :: bytes)
    | none => none

/-- Parse a `<...>` hex string. Embedded spaces are stripped before
    decoding (mirroring upstream's `ByteString.filter (/= 32)`); any other
    non-hex byte inside the angle brackets causes the subsequent
    `skipByte '>'` to fail, and an odd hex-digit count fails explicitly —
    both cases mirror `fromHex` failing on malformed input. -/
def parseHex : Parser Data.ByteString := do
  skipByte '<'.toUInt8
  let raw ← many (satisfy (fun b => Data.Word8.isHexDigit b || b == ' '.toUInt8))
  skipByte '>'.toUInt8
  let digits := raw.toList.filter (· != ' '.toUInt8)
  match hexPairs digits with
  | some bytes => pure (Data.ByteString.pack bytes)
  | none => fail "fromHex: odd number of hex digits"

/-- Parse a `[<...> <...> ...]` array of hex strings. Mirrors upstream's
    `parseHexArray`. -/
def parseHexArray : Parser (Array Data.ByteString) := do
  skipByte '['.toUInt8
  let items ← many (attempt (skipSpace *> parseHex))
  skipSpace
  skipByte ']'.toUInt8
  pure items

/-! ── `beginbfchar`/`endbfchar` blocks ── -/

/-- Parse a single `beginbfchar ... endbfchar` block into its
    glyph-code → text pairs, in the order they appear. Mirrors upstream's
    `charsParser'`. -/
def charsParser' : Parser (List (Nat × Data.Text)) := do
  let n ← skipTillParser (do
    let n ← digits
    skipSpace
    skipString "beginbfchar"
    pure n)
  let pairs ← times n (do
    skipSpace
    let i ← parseHex
    skipSpace
    let j ← parseHex
    pure (toCode i, decodeUtf16BE j))
  pure pairs.toList

/-- Parse every `beginbfchar ... endbfchar` block in the CMap, combining
    them into a single map (earlier blocks win on a duplicate code — see
    the module doc-comment's "Combining maps" section). Mirrors upstream's
    `charsParser`. -/
def charsParser : Parser (Std.HashMap Nat Data.Text) := do
  let blocks ← many (attempt charsParser')
  let combined := blocks.toList.foldr (· ++ ·) []
  pure (Std.HashMap.ofList combined.reverse)

/-! ── `beginbfrange`/`endbfrange` blocks ── -/

/-- Parse a single `beginbfrange ... endbfrange` block. Each entry is
    either `<start> <end> <dst>` (a contiguous range starting at `dst`'s
    first decoded character) or `<start> <end> [<dst0> <dst1> ...]` (an
    explicit per-code array, contributing individual chars instead).
    Mirrors upstream's `rangesParser'`. -/
def rangesParser' : Parser (List (Nat × Nat × Char) × List (Nat × Data.Text)) := do
  let n ← skipTillParser (do
    let n ← digits
    skipSpace
    skipString "beginbfrange"
    pure n)
  go n [] []
where
  go : Nat → List (Nat × Nat × Char) → List (Nat × Data.Text) →
      Parser (List (Nat × Nat × Char) × List (Nat × Data.Text))
    | 0, rs, cs => pure (rs.reverse, cs.reverse)
    | count + 1, rs, cs => do
      skipSpace
      let i ← toCode <$> parseHex
      skipSpace
      let j ← toCode <$> parseHex
      skipSpace
      (attempt (do
        let h ← parseHex
        match (decodeUtf16BE h).toList with
        | [] => fail "Can't decode range"
        | c :: _ => go count ((i, j, c) :: rs) cs)) <|> do
        let hs ← parseHexArray
        let cs' := (List.range' i (j + 1 - i)).zip (hs.toList.map decodeUtf16BE)
        go count rs (cs'.reverse ++ cs)

/-- Parse every `beginbfrange ... endbfrange` block in the CMap, combining
    ranges by simple concatenation (block order preserved) and array-form
    char contributions the same left-biased way as `charsParser` (see the
    module doc-comment). Mirrors upstream's `rangesParser`. -/
def rangesParser : Parser (List (Nat × Nat × Char) × Std.HashMap Nat Data.Text) := do
  let blocks ← many (attempt rangesParser')
  let ranges := blocks.toList.foldr (fun (rs, _) acc => rs ++ acc) []
  let chars := blocks.toList.foldr (fun (_, cs) acc => cs ++ acc) []
  pure (ranges, Std.HashMap.ofList chars.reverse)

/-! ── `begincodespacerange`/`endcodespacerange` ── -/

/-- Parse the `begincodespacerange ... endcodespacerange` block declaring
    which byte-length glyph codes are in use. Mirrors upstream's
    `codeRangesParser`. -/
def codeRangesParser : Parser (List (Data.ByteString × Data.ByteString)) := do
  let n ← skipTillParser (do
    let n ← digits
    skipSpace
    skipString "begincodespacerange"
    pure n)
  let pairs ← times n (do
    skipSpace
    let i ← parseHex
    skipSpace
    let j ← parseHex
    pure (i, j))
  pure pairs.toList

/-! ── Top-level parsing ── -/

/-- Parse the whole content of a `ToUnicode` CMap stream. Mirrors
    upstream's `parseUnicodeCMap`, which runs the three block parsers
    independently over the same input (`attoparsec`'s `parseOnly`) and
    combines `charsParser`'s and `rangesParser`'s char maps with
    upstream's left-biased `Map.union` (`cs` — from `beginbfchar` — wins
    over `crs` — the array-form contributions from `beginbfrange` — on a
    duplicate code; see the module doc-comment's "Combining maps"
    section). -/
def parseUnicodeCMap (cmap : Data.ByteString) : Except String UnicodeCMap := do
  let codeRanges ← (codeRangesParser.run cmap.data).mapError ("CMap code ranges: " ++ ·)
  let cs ← (charsParser.run cmap.data).mapError ("CMap chars: " ++ ·)
  let (ranges, crs) ← (rangesParser.run cmap.data).mapError ("CMap ranges: " ++ ·)
  let combined := cs.toList ++ crs.toList
  pure { codeRanges := codeRanges, chars := Std.HashMap.ofList combined.reverse, ranges := ranges }

/-! ── Lookups ── -/

/-- Does `glyph` fall (inclusively) within codespace range `(start, end)`,
    both of `glyph`'s length? Mirrors upstream's `inRange` (inside
    `unicodeCMapNextGlyph`). -/
private def inCodeRange (glyph : Data.ByteString) (range : Data.ByteString × Data.ByteString) :
    Bool :=
  glyph.length == range.1.length
    && compare glyph range.1 != Ordering.lt
    && compare glyph range.2 != Ordering.gt

/-- Try successively longer glyph-code prefixes (1 to 4 bytes) of `str`
    against `cmap`'s codespace ranges. `fuel` bounds the number of
    lengths tried; called with `fuel = 4` (matching upstream's `go 5 _ =
    Nothing`, i.e. lengths `1..4` are tried and `5` gives up). -/
private def nextGlyphFuel (cmap : UnicodeCMap) (str : Data.ByteString) (n : Nat) :
    Nat → Option (Nat × Data.ByteString)
  | 0 => none
  | fuel + 1 =>
    let glyph := str.take n
    if glyph.length != n then none
    else if cmap.codeRanges.any (inCodeRange glyph) then some (toCode glyph, str.drop n)
    else nextGlyphFuel cmap str (n + 1) fuel

/-- Take the next glyph code off the front of `str`, returning it together
    with the unconsumed remainder. Mirrors upstream's
    `unicodeCMapNextGlyph`. -/
def unicodeCMapNextGlyph (cmap : UnicodeCMap) (str : Data.ByteString) :
    Option (Nat × Data.ByteString) :=
  nextGlyphFuel cmap str 1 4

/-- Convert a glyph code to text, consulting `chars` first and then
    `ranges` — note a single glyph can decode to more than one character
    (e.g. ligatures). Mirrors upstream's `unicodeCMapDecodeGlyph`. -/
def unicodeCMapDecodeGlyph (cmap : UnicodeCMap) (glyph : Nat) : Option Data.Text :=
  match cmap.chars[glyph]? with
  | some txt => some txt
  | none =>
    match cmap.ranges.filter (fun (start, stop, _) => start ≤ glyph && glyph ≤ stop) with
    | [(start, _, c)] => some (Data.Text.singleton (Char.ofNat (c.val.toNat + (glyph - start))))
    | _ => none

end Data.PDF.Content.UnicodeCMap
