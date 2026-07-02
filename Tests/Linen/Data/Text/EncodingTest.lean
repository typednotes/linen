/-
  Tests for `Linen.Data.Text.Encoding` — UTF-8 encoding/decoding for `Text`.
-/
import Linen.Data.Text.Encoding

open Data (ByteString)
open Data.Text.Encoding

namespace Tests.Data.Text.Encoding

/-! ### Encoding -/

#guard (encodeUtf8 "hi").unpack == [0x68, 0x69]
#guard (encodeUtf8 "é").unpack == [0xC3, 0xA9]
#guard (encodeUtf8 "€").unpack == [0xE2, 0x82, 0xAC]
#guard (encodeUtf8 "").unpack == []

/-- The Unicode replacement character U+FFFD, as a one-character `Text`. -/
private def replacement : Data.Text := String.singleton (Char.ofNat 0xFFFD)

/-- `Except` has no stdlib `BEq` instance, so compare decoded results by pattern
    match instead of introducing a one-off instance just for these tests. -/
private def isOkWith (r : Except UnicodeError Data.Text) (t : Data.Text) : Bool :=
  match r with
  | .ok t' => t' == t
  | .error _ => false

/-! ### Strict decoding -/

#guard isOkWith (decodeUtf8' (ByteString.pack [0x68, 0x69])) "hi"
#guard isOkWith (decodeUtf8' (ByteString.pack [0xC3, 0xA9])) "é"
#guard isOkWith (decodeUtf8' (ByteString.pack [0xE2, 0x82, 0xAC])) "€"
#guard !(decodeUtf8' (ByteString.pack [0xFF])).isOk
#guard !(decodeUtf8' (ByteString.pack [0xC3])).isOk

/-! ### Round-trip -/

#guard isOkWith (decodeUtf8' (encodeUtf8 "hello, world! 你好")) "hello, world! 你好"

/-! ### Lenient decoding -/

#guard decodeUtf8With lenientDecode (ByteString.pack [0x68, 0x69]) == "hi"
#guard decodeUtf8With lenientDecode (ByteString.pack [0xFF, 0x68]) == replacement ++ "h"
#guard decodeUtf8Lenient (ByteString.pack [0xFF, 0x68]) == replacement ++ "h"

/-! ### Strict-handler decoding skips (rather than fails on) invalid bytes -/

#guard decodeUtf8With strictDecode (ByteString.pack [0xFF, 0x68]) == "h"

/-! ### Latin-1 decoding -/

#guard decodeLatin1 (ByteString.pack [0x68, 0x69]) == "hi"
#guard decodeLatin1 (ByteString.pack [0xE9]) == String.singleton (Char.ofNat 0xE9)

end Tests.Data.Text.Encoding
