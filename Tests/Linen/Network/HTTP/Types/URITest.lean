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

/-! ### encodeQueryComponent

  Stricter than `urlEncode`, and UTF-8 correct where `urlEncode` is not. -/

#guard encodeQueryComponent "a-b_c.d~e" == "a-b_c.d~e"   -- the unreserved set, untouched
#guard encodeQueryComponent "a b" == "a%20b"             -- space is %20, never '+'
#guard encodeQueryComponent "a/b" == "a%2Fb"             -- '/' is reserved, so escaped
#guard encodeQueryComponent "a=b&c" == "a%3Db%26c"
#guard encodeQueryComponent "" == ""
-- Encoded from UTF-8 bytes, unlike `urlEncode`, which encodes the code point.
#guard encodeQueryComponent "café" == "caf%C3%A9"
#guard urlEncode "café" == "caf%E9"                      -- the difference, side by side
-- Hex digits are uppercase, as RFC 3986 §2.1 prefers and signing schemes require.
#guard encodeQueryComponent "\t" == "%09"

/-! ### canonicalQuery

  Deterministic: sorted, strictly encoded, no leading `?`. This is what makes a
  query signable, and what `renderQuery` deliberately is not. -/

#guard canonicalQuery [] == ""
#guard canonicalQuery [("a", some "1")] == "a=1"
#guard canonicalQuery [("b", some "2"), ("a", some "1")] == "a=1&b=2"   -- sorted by name
#guard canonicalQuery [("x", none)] == "x="                             -- valueless
#guard canonicalQuery [("a", some "z"), ("a", some "b")] == "a=b&a=z"   -- ties break on value

-- Order-independence is the whole point: any permutation gives one string.
#guard canonicalQuery [("a", some "1"), ("b", some "2")]
     == canonicalQuery [("b", some "2"), ("a", some "1")]

-- No leading '?', unlike `renderQuery`.
#guard renderQuery [("a", some "1")] == "?a=1"
#guard canonicalQuery [("a", some "1")] == "a=1"

-- The AWS Query-protocol example: already sorted, and left alone.
#guard canonicalQuery [("Action", some "ListUsers"), ("Version", some "2010-05-08")]
     == "Action=ListUsers&Version=2010-05-08"

-- Names and values are both encoded, so a literal '=' or '&' inside a component
-- cannot be confused with the separators.
#guard canonicalQuery [("a b", some "c&d")] == "a%20b=c%26d"

/-! ### Signatures -/

example : String → String := encodeQueryComponent
example : Query → String := canonicalQuery

end Tests.Network.HTTP.Types.URI
