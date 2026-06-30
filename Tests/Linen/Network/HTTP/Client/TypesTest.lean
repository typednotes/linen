/-
  Tests for `Linen.Network.HTTP.Client.Types`.

  `Request`/`Response` are records (`Connection` carries IO callbacks), so the
  pure pieces — defaults, version, and the `Response` accessors — are checked
  with `#guard`; `Connection` is pinned at the type level.
-/
import Linen.Network.HTTP.Client.Types

open Network.HTTP.Client Network.HTTP.Types Data

namespace Tests.Network.HTTP.Client.Types

/-! ### HttpVersion -/

#guard http11.major == 1
#guard http11.minor == 1
#guard toString http11 == "HTTP/1.1"

/-! ### Request defaults -/

def req : Request := { method := .standard .GET, host := "example.com", port := 443 }

#guard req.path == "/"            -- default
#guard req.queryString == ""      -- default
#guard req.isSecure == false      -- default
#guard req.body == none           -- default
#guard req.host == "example.com"
#guard req.port == 443
#guard req.headers == ([] : RequestHeaders)

/-! ### Response accessors -/

def resp : Response :=
  { statusCode := status200
    headers := [(hContentLength, "42"), (hContentType, "text/html")]
    body := "hi".toUTF8 }

#guard resp.findHeader hContentType == some "text/html"
#guard resp.findHeader hContentLength == some "42"
-- Header lookup is case-insensitive (HeaderName = CI String).
#guard resp.findHeader (CI.mk' "CONTENT-TYPE") == some "text/html"
#guard resp.findHeader hServer == none
#guard resp.contentLength == some 42
#guard resp.isSuccess == true

-- A 404 response is not a success.
#guard ({ statusCode := status404, headers := [], body := ByteArray.empty } : Response).isSuccess == false
-- contentLength is none when the header is absent.
#guard ({ statusCode := status200, headers := [], body := ByteArray.empty } : Response).contentLength == none

/-! ### Connection — signature (carries IO callbacks) -/

example : (Nat → IO ByteArray) → (ByteArray → IO Unit) → IO Unit → Bool → Connection := Connection.mk

end Tests.Network.HTTP.Client.Types
