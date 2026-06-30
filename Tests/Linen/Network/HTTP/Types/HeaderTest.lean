/-
  Tests for `Linen.Network.HTTP.Types.Header`.

  Header names are case-insensitive (`CI String`), so the standard-name
  constants and their case-insensitive equality are checked with `#guard`.
-/
import Linen.Network.HTTP.Types.Header

open Network.HTTP.Types Data

namespace Tests.Network.HTTP.Types.Header

/-! ### Standard header-name constants -/

#guard hContentType == CI.mk' "Content-Type"
#guard hHost == CI.mk' "Host"
#guard hSetCookie == CI.mk' "Set-Cookie"
#guard hWWWAuthenticate == CI.mk' "WWW-Authenticate"

/-! ### Case-insensitive name equality -/

-- The whole point of `HeaderName = CI String`: comparison ignores case.
#guard hContentType == CI.mk' "content-type"
#guard hContentType == CI.mk' "CONTENT-TYPE"
#guard hAccept == CI.mk' "accept"
#guard (hContentType == hContentLength) == false
-- …but the original (display) casing is preserved.
#guard hContentType.original == "Content-Type"

/-! ### Header / header-list shapes -/

def hs : RequestHeaders := [(hContentType, "text/html"), (hContentLength, "42")]

#guard hs.length == 2
#guard (hs.lookup hContentType) == some "text/html"
-- Lookup is case-insensitive via the `CI` key.
#guard (hs.lookup (CI.mk' "CONTENT-LENGTH")) == some "42"
#guard (hs.lookup (CI.mk' "x-absent")) == none

end Tests.Network.HTTP.Types.Header
