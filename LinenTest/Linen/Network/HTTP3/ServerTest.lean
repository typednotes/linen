/-
  Tests for `Linen.Network.HTTP3.Server`.

  `H3Request`/`H3Response` are pure data and checked with `#guard`. The
  operational functions (`sendResponse`, `handleRequestStream`,
  `handleConnection`) all take a `Network.QUIC.QUICStream` or `Connection`,
  neither of which is constructible outside `Linen/Network/QUIC/Connection.lean`
  (its constructor is private) — so, matching hale's own upstream (which has
  no test file for this module either), those functions cannot be exercised
  here.
-/
import Linen.Network.HTTP3.Server

open Network.HTTP3

namespace Tests.Network.HTTP3.Server

#guard (({ method := "GET", path := "/", scheme := "https", authority := "example.com", headers := [], readBody := pure ByteArray.empty } : H3Request)).method == "GET"

#guard (({ method := "GET", path := "/", scheme := "https", authority := "example.com", headers := [("x-foo", "bar")], readBody := pure ByteArray.empty } : H3Request)).headers == [("x-foo", "bar")]

#guard (({ status := 200, headers := [], body := ByteArray.empty } : H3Response)).status == 200

#guard (({ status := 404, headers := [("content-type", "text/plain")], body := ByteArray.empty } : H3Response)).headers == [("content-type", "text/plain")]

end Tests.Network.HTTP3.Server
