/-
  Linen.Network.WebApp.Extra.Middleware.Approot — application root detection

  Detects the application root URL from headers or configuration. Ports
   `Network.Wai.Middleware.Approot`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types
open Data (CI)

/-- Pass-through middleware — the approot is computed from the request by
    application code using `getApprootFromRequest`, not stashed by this
    middleware itself.
    $$\text{approotMiddleware} : (\text{Request} \to \text{String}) \to \text{Middleware}$$ -/
def approotMiddleware (_getApproot : Request → String) : Middleware :=
  fun app req respond =>
    app req respond

/-- Get the approot from a request, considering `X-Forwarded-Proto` and
    `Host`. -/
def getApprootFromRequest (req : Request) : String :=
  let proto := if req.isSecure then "https" else
    match req.requestHeaders.find? (fun (n, _) => n == CI.mk' "X-Forwarded-Proto") with
    | some (_, v) => v
    | none => "http"
  let host := req.requestHeaderHost.getD "localhost"
  proto ++ "://" ++ host

end Network.WebApp.Extra.Middleware
