/-
  Linen.Network.WebApp.Extra.Middleware.Local — restrict access to localhost

  Ports `Network.Wai.Middleware.Local`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types

/-- Only allow requests from localhost. Returns 403 Forbidden for remote
    clients.
    $$\text{localOnly} : \text{Middleware}$$ -/
def localOnly : Middleware :=
  fun app req respond =>
    let host := req.remoteHost.host
    if host == "127.0.0.1" || host == "::1" || host == "localhost" then
      app req respond
    else
      AppM.respond respond (.responseBuilder status403 [] "Forbidden: localhost only".toUTF8)

end Network.WebApp.Extra.Middleware
