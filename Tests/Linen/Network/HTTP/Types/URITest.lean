/-
  Tests for `Linen.Network.HTTP.Types.URI`.

  Query-string parsing/rendering and percent-encoding are pure, so behaviour is
  checked with `#guard`, heavily via round-trips.
-/
import Linen.Network.HTTP.Types.URI

open Network.HTTP.Types

namespace Tests.Network.HTTP.Types.URI

/-! ### parseQuery -/

#guard parseQuery "?a=1&b=2" == [("a", some "1"), ("b", some "2")]
#guard parseQuery "a=1&b=2" == [("a", some "1"), ("b", some "2")]   -- leading '?' optional
#guard parseQuery "a=1&b" == [("a", some "1"), ("b", none)]          -- value-less key
#guard parseQuery "" == ([] : Query)
#guard parseQuery "?" == ([] : Query)

/-! ### renderQuery -/

#guard renderQuery [("a", some "1"), ("b", some "2")] == "?a=1&b=2"
#guard renderQuery [("a", some "1"), ("b", none)] == "?a=1&b"
#guard renderQuery [] == ""

-- Round-trip: parse (render q) = q.
#guard parseQuery (renderQuery [("x", some "1"), ("y", none), ("z", some "3")])
        == [("x", some "1"), ("y", none), ("z", some "3")]

/-! ### urlEncode -/

#guard urlEncode "hello world" == "hello%20world"
#guard urlEncode "a-b_c.d~e" == "a-b_c.d~e"        -- unreserved chars pass through
#guard urlEncode "a+b" == "a%2Bb"                  -- '+' is encoded (not a space)
#guard urlEncode "100%" == "100%25"
-- Encoding is by Unicode codepoint, not UTF-8 byte: 'é' = U+00E9 ⇒ %E9.
#guard urlEncode "café" == "caf%E9"

/-! ### urlDecode -/

#guard urlDecode "hello%20world" == "hello world"
#guard urlDecode "a+b" == "a b"                    -- '+' decodes to space
#guard urlDecode "%2B" == "+"
#guard urlDecode "%7e" == "~"                      -- lowercase hex accepted

/-! ### Round-trips (decode ∘ encode) -/

#guard urlDecode (urlEncode "a b&c=d") == "a b&c=d"
#guard urlDecode (urlEncode "path/to/file?x=1") == "path/to/file?x=1"
#guard urlDecode (urlEncode "a b+c") == "a b+c"    -- '+' survives (encoded as %2B)

end Tests.Network.HTTP.Types.URI
