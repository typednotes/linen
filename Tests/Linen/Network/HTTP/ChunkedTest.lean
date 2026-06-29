/-
  Tests for `Linen.Network.HTTP.Chunked`.

  `ByteArray`s are compared via `.toList` against the UTF-8 bytes of the
  expected wire string.
-/
import Linen.Network.HTTP.Chunked

open Network.HTTP.Chunked

namespace Tests.Network.HTTP.Chunked

/-! ### single chunk: `<hex-len>\r\n<data>\r\n` -/

#guard (chunkedTransferEncoding "Wiki".toUTF8).toList == "4\r\nWiki\r\n".toUTF8.toList
#guard (chunkedTransferEncoding "pedia".toUTF8).toList == "5\r\npedia\r\n".toUTF8.toList
-- a 16-byte payload exercises a multi-digit hex length ("10")
#guard (chunkedTransferEncoding "0123456789abcdef".toUTF8).toList
        == "10\r\n0123456789abcdef\r\n".toUTF8.toList

/-! ### empty input produces no chunk -/

#guard (chunkedTransferEncoding ByteArray.empty).size == 0

/-! ### terminator -/

#guard chunkedTransferTerminator.toList == "0\r\n\r\n".toUTF8.toList
#guard chunkedTransferTerminator.toList == [48, 13, 10, 13, 10]

/-! ### full bodies -/

#guard (encodeChunked ["Wiki".toUTF8, "pedia".toUTF8]).toList
        == "4\r\nWiki\r\n5\r\npedia\r\n0\r\n\r\n".toUTF8.toList
#guard (encodeChunked []).toList == chunkedTransferTerminator.toList
-- empty chunks are skipped, so they don't prematurely terminate the body
#guard (encodeChunked [ByteArray.empty, "ok".toUTF8]).toList
        == "2\r\nok\r\n0\r\n\r\n".toUTF8.toList

end Tests.Network.HTTP.Chunked
