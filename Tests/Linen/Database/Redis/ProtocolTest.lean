import Linen.Database.Redis.Protocol

/-!
  Tests for `Linen.Database.Redis.Protocol`.
-/

open Database.Redis.Protocol
open Std.Internal.Parsec.ByteArray (Parser)

/-- `Except String Reply` has no `BEq` instance (its `Error` case is just a
    `String`), so tests compare parse results through this helper instead. -/
def isReplyOk (expected : Reply) : Except String Reply → Bool
  | .ok r => r == expected
  | .error _ => false

/-- Like `isReplyOk`, but for a parse that is expected to fail. -/
def isParseError : Except String Reply → Bool
  | .ok _ => false
  | .error _ => true

-- `renderRequest` renders a command as a RESP multi-bulk array.
#guard renderRequest ["SET".toUTF8, "k".toUTF8, "v".toUTF8]
  == "*3\r\n$3\r\nSET\r\n$1\r\nk\r\n$1\r\nv\r\n".toUTF8

-- Rendering an empty request is just the (zero-length) array header.
#guard renderRequest [] == "*0\r\n".toUTF8

-- `reply` parses a single-line (`+`) reply.
#guard isReplyOk (Reply.singleLine "OK".toUTF8) (Parser.run reply "+OK\r\n".toUTF8)

-- `reply` parses an error (`-`) reply.
#guard isReplyOk (Reply.error "ERR bad".toUTF8) (Parser.run reply "-ERR bad\r\n".toUTF8)

-- `reply` parses an integer (`:`) reply, including negative values.
#guard isReplyOk (Reply.integer 42) (Parser.run reply ":42\r\n".toUTF8)
#guard isReplyOk (Reply.integer (-7)) (Parser.run reply ":-7\r\n".toUTF8)

-- `reply` parses a bulk (`$`) reply.
#guard isReplyOk (Reply.bulk (some "hello".toUTF8)) (Parser.run reply "$5\r\nhello\r\n".toUTF8)

-- `reply` parses the null bulk reply `$-1\r\n`.
#guard isReplyOk (Reply.bulk none) (Parser.run reply "$-1\r\n".toUTF8)

-- `reply` parses a multi-bulk (`*`) reply, recursing into its elements.
#guard isReplyOk (Reply.multiBulk (some [Reply.singleLine "OK".toUTF8, Reply.integer 1]))
  (Parser.run reply "*2\r\n+OK\r\n:1\r\n".toUTF8)

-- `reply` parses the null multi-bulk reply `*-1\r\n`.
#guard isReplyOk (Reply.multiBulk none) (Parser.run reply "*-1\r\n".toUTF8)

-- `reply` parses nested multi-bulk replies (an array of arrays).
#guard isReplyOk (Reply.multiBulk (some [Reply.multiBulk (some [Reply.integer 9])]))
  (Parser.run reply "*1\r\n*1\r\n:9\r\n".toUTF8)

-- An empty multi-bulk reply parses to an empty (non-null) list.
#guard isReplyOk (Reply.multiBulk (some [])) (Parser.run reply "*0\r\n".toUTF8)

-- An unrecognised reply-type tag is a parse error.
#guard isParseError (Parser.run reply "?nope\r\n".toUTF8)
