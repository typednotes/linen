/-
  Linen.Network.WebApp.Extra.Middleware.Autohead — automatically handle
  HEAD requests

  Converts HEAD requests to GET and strips the response body. Ports
  `Network.Wai.Middleware.Autohead`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types

/-- Convert HEAD requests to GET, passing the result through but stripping
    the body.
    $$\text{autohead} : \text{Middleware}$$ -/
def autohead : Middleware :=
  fun app req respond =>
    if req.requestMethod == .standard .HEAD then
      app { req with requestMethod := .standard .GET } fun resp =>
        respond (.responseBuilder resp.status resp.headers ByteArray.empty)
    else
      app req respond

end Network.WebApp.Extra.Middleware
