/-
  Data.PDF.Core.Parsers.XRef — parsers for the cross-reference table/trailer

  Ports `Pdf.Core.Parsers.XRef` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox, `core/lib/Pdf/Core/Parsers/XRef.hs`),
  module 9 of the `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  ## `startXRef`'s scan-and-take-last search

  Upstream's `startXRef` repeatedly scans forward for the next occurrence of
  `"startxref"`, each time followed by a decimal offset and `"%%EOF"`, using
  `many` to collect *every* such occurrence and then returning the *last*
  one — because a PDF that has been incrementally updated may contain
  several `startxref` markers (one per update), and only the final one, at
  the very end of the file, is authoritative. That scan-loop is itself
  unbounded a priori, so — mirroring the fuel technique used throughout
  `Parsers.Object` (see that module's doc-comment) — it is parameterised by
  a `fuel : Nat` seeded from the parser's remaining input: each iteration of
  the scan consumes at least the bytes up to and including one
  `"startxref"` occurrence, so `fuel` can never run out before a
  well-formed input's last marker is found.

  Upstream's `Int64` offsets are ported as `Nat` throughout, matching the
  substitution already established by `Data.PDF.Core.IO.Buffer` and
  `Data.PDF.Core.Object` (a file offset is never negative).
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Parsers.Object
import Linen.Data.PDF.Core.Parsers.Util
import Std.Internal.Parsec.ByteArray

namespace Data.PDF.Core.Parsers.XRef

open Std.Internal.Parsec Std.Internal.Parsec.ByteArray
open Data.PDF.Core.Object Data.PDF.Core.Parsers.Util Data.PDF.Core.Parsers.Object

/-- Read the current iterator without consuming anything (see
    `Parsers.Object.getIt`, reimplemented locally since it is private
    there). -/
private def getIt : Parser ByteArray.Iterator := fun it => .success it it

/-- Skip forward, byte by byte, until immediately before the next
    `"startxref"` occurrence (or fail if none remains in `fuel` bytes),
    mirroring upstream's `manyTill anyChar (string "startxref")`. -/
private def skipToStartXRef : Nat → Parser Unit
  | 0 => fail "startXRef: \"startxref\" not found"
  | fuel + 1 =>
    attempt (skipString "startxref") <|> (any *> skipToStartXRef fuel)

/-- One `"startxref"` occurrence (already positioned anywhere in the file),
    followed by its offset and the `"%%EOF"` marker. Mirrors upstream's
    deliberate choice of whitespace skipper at each point: the
    comment-aware, local `skipSpace` between `"startxref"` and the offset
    (upstream's unqualified `skipSpace`), but a bare run of whitespace bytes
    (upstream's `P.skipSpace`, attoparsec's own, comment-*unaware* skipper)
    between the offset and `"%%EOF"` — deliberately *not* the comment-aware
    `skipSpace` there, since (besides not matching upstream) a `%`-comment
    that runs all the way to the true end of the buffer with no terminating
    `\r`/`\n` would trip `Std.Internal.Parsec.ByteArray.skipWhile`'s
    documented not-even-zero-matches-tolerated eof behaviour, and
    `"%%EOF"` is very often the last bytes of a resident PDF buffer. -/
private def startXRefOnce (fuel : Nat) : Parser Nat := do
  skipToStartXRef fuel
  skipSpace
  let offset ← digits
  skipWhile isSpaceByte
  skipString "%%EOF"
  pure offset

/-- Collect every `startxref ... %%EOF` occurrence from the current position
    onward, returning them in order found. See the module doc-comment for
    the `fuel` termination argument. -/
private def collectStartXRefs : Nat → Array Nat → Parser (Array Nat)
  | 0, acc => pure acc
  | fuel + 1, acc =>
    attempt (do
      let it ← getIt
      let off ← startXRefOnce it.remainingBytes
      collectStartXRefs fuel (acc.push off))
    <|> pure acc

/-- Offset of the very last xref table/stream in the file (there may be
    several, from successive incremental updates — only the last is
    authoritative). Call this only when positioned near the end of the
    file (e.g. the file's last ~1KB): scanning from the very start would be
    correct but wasteful. Mirrors upstream's `startXRef`. -/
def startXRef : Parser Nat := do
  let it ← getIt
  let all ← collectStartXRefs it.remainingBytes #[]
  match all.back? with
  | some off => pure off
  | none => fail "Trailer not found"

/-- When positioned at an xref *table* (not an xref *stream*), succeeds and
    leaves the input positioned at the first subsection header; otherwise
    fails. Mirrors upstream's `tableXRef`. -/
def tableXRef : Parser Unit := do
  skipString "xref"
  endOfLine

/-- Parse a subsection header `start count`, returning the first object
    index and the number of entries in the subsection; leaves the input
    positioned at the first entry. -/
def parseSubsectionHeader : Parser (Nat × Nat) := do
  let start ← digits
  skipSpace
  let count ← digits
  endOfLine
  pure (start, count)

/-- Parse the trailer dictionary located immediately after an xref table.
    Input should be positioned at the `"trailer"` keyword. -/
def parseTrailerAfterTable : Parser Dict := do
  skipSpace
  skipString "trailer"
  endOfLine
  skipSpace
  parseDict

/-- Parse one xref table entry: `offset generation char`, where `char` is
    `'n'` (in use) or `'f'` (free). Returns `(offset, generation, isFree)`. -/
def parseTableEntry : Parser (Nat × Nat × Bool) := do
  let offset ← digits
  skipSpace
  let generation ← digits
  skipSpace
  let c ← any
  if c == 'n'.toUInt8 then
    pure (offset, generation, false)
  else if c == 'f'.toUInt8 then
    pure (offset, generation, true)
  else
    fail s!"error parsing XRef table entry: unknown char: {Char.ofNat c.toNat}"

end Data.PDF.Core.Parsers.XRef
