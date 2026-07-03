/-
  Linen.Network.WebApp.Extra.Middleware.ForceSSL — redirect HTTP to HTTPS

  Ports Hale's `Network.Wai.Middleware.ForceSSL`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types

/-- Redirect non-secure requests to HTTPS.
    $$\text{forceSSL} : \text{Middleware}$$ -/
def forceSSL : Middleware :=
  fun app req respond =>
    if req.isSecure then
      app req respond
    else
      let host := req.requestHeaderHost.getD "localhost"
      let url := "https://" ++ host ++ req.rawPathInfo ++ req.rawQueryString
      AppM.respond respond (.responseBuilder status301 [(hLocation, url)] ByteArray.empty)

/-- Secure requests pass through the `forceSSL` middleware unchanged.
    $$\forall\, \text{req},\; \text{req.isSecure} = \text{true} \implies \text{forceSSL}(\text{app}, \text{req}) = \text{app}(\text{req})$$ -/
theorem forceSSL_secure (app : Application) (req : Request)
    (respond : Response → Control.Concurrent.Green.Green ResponseReceived)
    (h : req.isSecure = true) :
    forceSSL app req respond = app req respond := by
  simp [forceSSL, h]

end Network.WebApp.Extra.Middleware
