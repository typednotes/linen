/-
  Tests for `Linen.Data.PDF.Core.Stream`.

  Every function here is `IO`-returning (parsing against a resident buffer,
  draining/inflating a stream), so this is checked with `#eval`, following
  `Tests/Linen/Crypto/Zlib/FFITest.lean`'s pattern — a thrown error fails
  the build.
-/
import Linen.Data.PDF.Core.Stream

open Data.PDF.Core.Object Data.PDF.Core.Stream

private def bytes (s : String) : ByteArray := String.toUTF8 s

private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

-- The raw zlib/RFC 1950 bytes for `zlib.compress(b"streamed content")`.
private def streamedContentZlib : ByteArray :=
  ByteArray.mk #[120, 156, 43, 46, 41, 74, 77, 204, 77, 77, 81, 72, 206, 207,
    43, 73, 205, 43, 1, 0, 54, 222, 6, 113]

private def streamedContent : ByteArray := bytes "streamed content"

namespace Tests.Data.PDF.Core.Stream

-- `readStream` parses a `Stream` object's dictionary and computes its
-- payload's absolute offset (here, relative to the caller-supplied `off`,
-- taken as `0`): right after the `"stream\n"` keyword, which is 53 bytes
-- into `"5 0 obj\n<< /Length 24 /Filter /FlateDecode >>\nstream\n"`.
#eval show IO Unit from do
  let is ← Data.PDF.Stream.fromByteString
    (bytes "5 0 obj\n<< /Length 24 /Filter /FlateDecode >>\nstream\n" ++ streamedContentZlib)
  let s ← readStream is 0
  unless s.offset == 53 do
    throw (IO.userError s!"unexpected stream offset: {s.offset}")
  match s.dict.get? (mkName "Filter") with
  | some (.name n) => unless n == mkName "FlateDecode" do
      throw (IO.userError "unexpected /Filter value")
  | _ => throw (IO.userError "missing or malformed /Filter entry")

-- `rawStreamContent` reads a stream's still-filtered bytes given its length
-- and offset, leaving the filter chain untouched.
#eval show IO Unit from do
  let buf ← Data.PDF.Core.IO.Buffer.fromBytes
    (bytes "garbage" ++ streamedContentZlib ++ bytes "trailing")
  let raw ← rawStreamContent buf streamedContentZlib.size 7
  let chunks ← Data.PDF.Stream.toList raw
  let out := chunks.foldl (· ++ ·) ByteArray.empty
  unless out == streamedContentZlib do
    throw (IO.userError s!"rawStreamContent mismatch: got {out.toList}")

-- `decodeStream` applies the stream's named filter chain (`/FlateDecode`,
-- with no `/DecodeParms`) to already-raw content, recovering the original.
#eval show IO Unit from do
  let s := Stream.mk' (Std.HashMap.ofList [(mkName "Filter", Object.name (mkName "FlateDecode"))]) 0
  let is ← Data.PDF.Stream.fromByteString streamedContentZlib
  let out ← decodeStream knownFilters s is
  let chunks ← Data.PDF.Stream.toList out
  let bytes := chunks.foldl (· ++ ·) ByteArray.empty
  unless bytes == streamedContent do
    throw (IO.userError s!"decodeStream mismatch: got {bytes.toList}")

-- `decodedStreamContent` composes raw-read, decryption (identity here), and
-- decoding into one call.
#eval show IO Unit from do
  let buf ← Data.PDF.Core.IO.Buffer.fromBytes streamedContentZlib
  let s := Stream.mk' (Std.HashMap.ofList [(mkName "Filter", Object.name (mkName "FlateDecode"))]) 0
  let out ← decodedStreamContent buf knownFilters pure streamedContentZlib.size s
  let chunks ← Data.PDF.Stream.toList out
  let bytes := chunks.foldl (· ++ ·) ByteArray.empty
  unless bytes == streamedContent do
    throw (IO.userError s!"decodedStreamContent mismatch: got {bytes.toList}")

-- `decodeStream` throws when the dictionary names a filter this port
-- doesn't know about.
#eval show IO Unit from do
  let s := Stream.mk' (Std.HashMap.ofList [(mkName "Filter", Object.name (mkName "LZWDecode"))]) 0
  let is ← Data.PDF.Stream.fromByteString streamedContentZlib
  let result ← try
      let _ ← decodeStream knownFilters s is
      pure true
    catch _ =>
      pure false
  unless !result do
    throw (IO.userError "expected decodeStream on an unknown filter to fail")

end Tests.Data.PDF.Core.Stream
