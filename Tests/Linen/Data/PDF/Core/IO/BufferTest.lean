/-
  Tests for `Linen.Data.PDF.Core.IO.Buffer`.

  `Buffer` is entirely `IO`-based (cursor state lives in an `IO.Ref`), so its
  behaviour is checked with `#eval`, following the pattern used for other
  `IO`-based ports (e.g. `Tests/Linen/Crypto/Zlib/FFITest.lean`).
-/
import Linen.Data.PDF.Core.IO.Buffer

open Data.PDF.Core.IO.Buffer

namespace Tests.Data.PDF.Core.IO.Buffer

-- `fromBytes` reads the whole content in a single `read` (see the module
-- doc-comment: unlike `fromHandle`, `fromBytes` never chunks), then reports
-- end-of-source as `none`.
#eval show IO Unit from do
  let buf ← fromBytes (String.toUTF8 "hello world")
  let some chunk ← buf.read | throw (IO.userError "expected a first chunk")
  unless chunk == String.toUTF8 "hello world" do
    throw (IO.userError s!"unexpected first chunk: {chunk.toList}")
  let .none ← buf.read | throw (IO.userError "expected end-of-source")
  pure ()

-- `size`/`tell`/`seek` report/move the cursor correctly.
#eval show IO Unit from do
  let buf ← fromBytes (String.toUTF8 "abcdef")
  let sz ← buf.size
  unless sz == 6 do
    throw (IO.userError s!"unexpected size: {sz}")
  buf.seek 3
  let t ← buf.tell
  unless t == 3 do
    throw (IO.userError s!"unexpected tell after seek: {t}")
  let some rest ← buf.read | throw (IO.userError "expected data after seek")
  unless rest == String.toUTF8 "def" do
    throw (IO.userError s!"unexpected data after seek: {rest.toList}")

-- `back` rewinds the cursor by exactly the given number of bytes.
#eval show IO Unit from do
  let buf ← fromBytes (String.toUTF8 "abcdef")
  let some _ ← buf.read | throw (IO.userError "expected initial read")
  buf.back 2
  let t ← buf.tell
  unless t == 4 do
    throw (IO.userError s!"unexpected tell after back: {t}")

-- `back` saturates at `0` rather than underflowing (see the module
-- doc-comment).
#eval show IO Unit from do
  let buf ← fromBytes (String.toUTF8 "ab")
  buf.back 100
  let t ← buf.tell
  unless t == 0 do
    throw (IO.userError s!"unexpected tell after over-large back: {t}")

-- `toInputStream` adapts `read`/`back` directly onto
-- `Data.PDF.Stream.InputStream`'s `_read`/`_unRead`.
#eval show IO Unit from do
  let buf ← fromBytes (String.toUTF8 "stream contents")
  let stream := toInputStream buf
  let chunks ← Data.PDF.Stream.toList stream
  unless chunks == [String.toUTF8 "stream contents"] do
    throw (IO.userError s!"unexpected chunks: {chunks.map ByteArray.toList}")

-- `dropExactly` discards `n` bytes from an input stream.
#eval show IO Unit from do
  let stream ← Data.PDF.Stream.fromByteString (String.toUTF8 "0123456789")
  dropExactly 4 stream
  let rest ← Data.PDF.Stream.readExactly 6 stream
  unless rest == String.toUTF8 "456789" do
    throw (IO.userError s!"unexpected remainder after dropExactly: {rest.toList}")

end Tests.Data.PDF.Core.IO.Buffer
