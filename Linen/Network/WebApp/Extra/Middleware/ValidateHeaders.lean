/-
  Linen.Network.WebApp.Extra.Middleware.ValidateHeaders — validate response
  headers

  Ensures response headers conform to HTTP specifications. Ports Hale's
  `Network.Wai.Middleware.ValidateHeaders`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types

/-- Check that a header value contains no invalid characters (CR, LF, NUL).
    HTTP headers must not contain control characters. -/
private def isValidHeaderValue (v : String) : Bool :=
  !v.any fun c => c == '\r' || c == '\n' || c == '\x00'

/-- Validate all response headers. If any header contains invalid
    characters, replace the response with 500 Internal Server Error.
    $$\text{validateHeaders} : \text{Middleware}$$ -/
def validateHeaders : Middleware :=
  fun app req respond =>
    app req fun resp =>
      let hdrs := resp.headers
      if hdrs.all (fun (_, v) => isValidHeaderValue v) then
        respond resp
      else
        respond (.responseBuilder status500 [] "Invalid response headers".toUTF8)

end Network.WebApp.Extra.Middleware
