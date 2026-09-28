/-
  Tests for `Linen.Data.PDF.Core.Parsers.Util`.
-/
import Linen.Data.PDF.Core.Parsers.Util

open Data.PDF.Core.Parsers.Util
open Std.Internal.Parsec ByteArray

private def bytes (s : String) : ByteArray := String.toUTF8 s

namespace Tests.Data.PDF.Core.Parsers.Util

-- `isSpaceByte` accepts the ASCII space and the tab/newline/CR/FF/VT range.
#guard isSpaceByte 32
#guard isSpaceByte 9
#guard isSpaceByte 13
#guard !isSpaceByte 65 -- 'A'

-- `endOfLine` accepts a bare `\n`.
#guard (Parser.run endOfLine (bytes "\n")).isOk

-- `endOfLine` accepts `\r\n`.
#guard (Parser.run endOfLine (bytes "\r\n")).isOk

-- `endOfLine` accepts a bare `\r`.
#guard (Parser.run endOfLine (bytes "\r")).isOk

-- `endOfLine` skips leading spaces before the newline.
#guard (Parser.run endOfLine (bytes "   \n")).isOk

-- `skipComment` consumes a `%`-comment up to (but not including) the
-- terminating newline.
#guard match Parser.run (skipComment *> pbyte '\n'.toUInt8) (bytes "%a comment\n") with
  | .ok c => c == '\n'.toUInt8
  | .error _ => false

-- `skipSpace` skips whitespace interspersed with comments, stopping right
-- before the first non-whitespace, non-comment byte.
#guard match Parser.run (skipSpace *> pbyte 'x'.toUInt8) (bytes "  % hi\n  x") with
  | .ok c => c == 'x'.toUInt8
  | .error _ => false

-- `skipSpace` is fine with no whitespace/comments at all.
#guard match Parser.run (skipSpace *> pbyte 'x'.toUInt8) (bytes "x") with
  | .ok c => c == 'x'.toUInt8
  | .error _ => false

end Tests.Data.PDF.Core.Parsers.Util
