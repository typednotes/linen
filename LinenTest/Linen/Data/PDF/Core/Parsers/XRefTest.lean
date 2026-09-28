/-
  Tests for `Linen.Data.PDF.Core.Parsers.XRef`.
-/
import Linen.Data.PDF.Core.Parsers.XRef

open Data.PDF.Core.Parsers.XRef
open Std.Internal.Parsec ByteArray

private def bytes (s : String) : ByteArray := String.toUTF8 s

namespace Tests.Data.PDF.Core.Parsers.XRef

-- `startXRef` finds a single `startxref ... %%EOF` marker.
#guard match Parser.run startXRef (bytes "anything...startxref\n123\n%%EOF") with
  | .ok off => off == 123
  | .error _ => false

-- `startXRef` returns the *last* marker when several are present (from
-- successive incremental updates).
#guard match Parser.run startXRef
    (bytes "...startxref\n222\n%%EOF...blah\nstartxref\n123\n%%EOF") with
  | .ok off => off == 123
  | .error _ => false

-- `startXRef` fails when no marker is present at all.
#guard match Parser.run startXRef (bytes "nothing here") with
  | .ok _ => false
  | .error _ => true

-- `tableXRef` succeeds right at `"xref"` and fails otherwise.
#guard match Parser.run tableXRef (bytes "xref\n") with
  | .ok _ => true
  | .error _ => false
#guard match Parser.run tableXRef (bytes "not xref") with
  | .ok _ => false
  | .error _ => true

-- `parseSubsectionHeader` reads the first index and entry count.
#guard match Parser.run parseSubsectionHeader (bytes "0 6\n") with
  | .ok (start, count) => start == 0 && count == 6
  | .error _ => false

-- `parseTrailerAfterTable` parses the dictionary right after `"trailer"`.
#guard match Parser.run parseTrailerAfterTable (bytes "trailer\n<< /Size 6 >>") with
  | .ok _ => true
  | .error _ => false

-- `parseTableEntry` distinguishes in-use (`n`) from free (`f`) entries.
#guard match Parser.run parseTableEntry (bytes "0000000017 00000 n") with
  | .ok (off, gen, free) => off == 17 && gen == 0 && !free
  | .error _ => false
#guard match Parser.run parseTableEntry (bytes "0000000000 65535 f") with
  | .ok (_, _, free) => free
  | .error _ => false

-- An unrecognised third field is a parse error.
#guard match Parser.run parseTableEntry (bytes "0000000000 00000 x") with
  | .ok _ => false
  | .error _ => true

end Tests.Data.PDF.Core.Parsers.XRef
