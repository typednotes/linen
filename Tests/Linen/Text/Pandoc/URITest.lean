/-
  Tests for `Linen.Text.Pandoc.URI`.
-/
import Linen.Text.Pandoc.URI

namespace Tests.Linen.Text.Pandoc.URI

open _root_.Linen.Text.Pandoc

-- ── escapeURI ─────────────────────────────────────────────────────────

-- spaces and the punctuation set get percent-encoded
#guard URI.escapeURI "a b" == "a%20b"
#guard URI.escapeURI "a<b>c" == "a%3Cb%3Ec"
-- ordinary URL characters are left intact
#guard URI.escapeURI "http://example.com/path?x=1" == "http://example.com/path?x=1"

-- ── isURI ─────────────────────────────────────────────────────────────

#guard URI.isURI "http://example.com" == true
#guard URI.isURI "https://example.com/a/b" == true
#guard URI.isURI "mailto:me@example.com" == true
#guard URI.isURI "not a uri" == false
#guard URI.isURI "just-text" == false
-- unknown scheme rejected
#guard URI.isURI "florb://x" == false

-- ── pBase64DataURI ────────────────────────────────────────────────────

-- "Man" base64-encodes to "TWFu"
#guard (URI.pBase64DataURI "data:text/plain;base64,TWFu").isSome == true
#guard ((URI.pBase64DataURI "data:text/plain;base64,TWFu").map (·.2)) == some "text/plain"
#guard URI.isURI "data:text/plain;base64,TWFu" == true
#guard (URI.pBase64DataURI "data:text/plain,notbase64").isNone == true

end Tests.Linen.Text.Pandoc.URI
