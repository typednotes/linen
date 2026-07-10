/-
  Data.PDF.Core.Parsers.Util — shared low-level PDF parsing combinators

  Ports `Pdf.Core.Parsers.Util` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox,
  `core/lib/Pdf/Core/Parsers/Util.hs`), module 3 of the `pdf-toolbox-core`
  import documented in `docs/imports/PdfToolboxCore/dependencies.md`.

  Upstream builds these on `attoparsec`; `linen` has no bespoke
  `attoparsec` port (per `docs/imports/PdfToolboxCore/dependencies.md`'s
  external-dependency note, `attoparsec` maps directly onto Lean's own
  `Std.Internal.Parsec`), so these combinators are written directly against
  `Std.Internal.Parsec.ByteArray.Parser`.
-/
import Std.Internal.Parsec.ByteArray

namespace Data.PDF.Core.Parsers.Util

open Std.Internal.Parsec ByteArray

/-- Is `b` an ASCII space/tab/newline/carriage-return/form-feed/vertical-tab
    byte? Matches upstream's (`attoparsec`'s) `isSpace_w8` predicate:
    `w == 32 || w - 9 ≤ 4` (i.e. bytes 9–13, plus 32). -/
def isSpaceByte (b : UInt8) : Bool :=
  b == 32 || (b ≥ 9 && b ≤ 13)

/-- Like `Std.Internal.Parsec.ByteArray.skipWhile`, but treats running off
    the end of input as success rather than as an `.eof` error.

    `Std.Internal.Parsec.ByteArray.skipWhile` is written for a *streaming*
    input, where hitting the end of the currently-available bytes while the
    predicate still holds could mean "there might be more matching bytes
    once more input arrives" — so it reports `.eof` even though every byte
    it did see matched the predicate. `linen`'s PDF parsers, here as
    elsewhere, always run against a complete in-memory `ByteArray`, so there
    is no "more might arrive": reaching the end while the predicate held for
    every byte seen simply means the skip is done. Without this, a bare run
    of trailing whitespace (or a trailing `%`-comment with no terminating
    newline) at the very end of a byte array would make `skipSpace` fail
    instead of succeeding having skipped everything, which is what every
    caller here (and upstream's own `attoparsec`-based `skipWhile`, which has
    no such EOF caveat) actually needs. -/
def skipWhileToEnd (pred : UInt8 → Bool) : Parser Unit :=
  fun it =>
    match skipWhile pred it with
    | .error rem .eof => .success rem ()
    | r => r

/-- In a PDF file, an end-of-line is `"\n"`, `"\r"`, or `"\r\n"` — and a run
    of spaces (`0x20`) before it is allowed and skipped. Mirrors upstream's
    `endOfLine`, which tries `attoparsec`'s `endOfLine` (accepting `"\n"` or
    `"\r\n"`) and falls back to a bare `"\r"`. -/
def endOfLine : Parser Unit := do
  skipWhileToEnd (· == ' '.toUInt8)
  skipByte '\n'.toUInt8 <|> (skipByte '\r'.toUInt8 *> (skipByte '\n'.toUInt8 <|> pure ()))

/-- Skip a `%`-introduced PDF comment, up to (but not including) the next
    end-of-line byte — or, if the comment runs to the end of input with no
    terminating newline, to the end of input. -/
def skipComment : Parser Unit := do
  skipByte '%'.toUInt8
  skipWhileToEnd (fun b => b != '\r'.toUInt8 && b != '\n'.toUInt8)

/-- Skip whitespace, interspersed with any number of `%` comments. Mirrors
    upstream's `skipSpace = P.skipSpace *> many (skipComment *> P.skipSpace)`. -/
def skipSpace : Parser Unit := do
  skipWhileToEnd isSpaceByte
  let _ ← many (skipComment *> skipWhileToEnd isSpaceByte)

end Data.PDF.Core.Parsers.Util
