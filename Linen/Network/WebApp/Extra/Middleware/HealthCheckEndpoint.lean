/-
  Linen.Network.WebApp.Extra.Middleware.HealthCheckEndpoint — empty health
  check endpoint

  Adds a health check endpoint at the specified path that returns 200 OK.
  Ports Hale's `Network.Wai.Middleware.HealthCheckEndpoint`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types

/-- Add a health check endpoint at the given path. Returns 200 OK with an
    empty body without calling the inner app.
    $$\text{healthCheck} : \text{String} \to \text{Middleware}$$ -/
def healthCheck (path : String := "/_health") : Middleware :=
  fun app req respond =>
    if req.rawPathInfo == path then
      AppM.respond respond (.responseBuilder status200 [] ByteArray.empty)
    else
      app req respond

/-- Non-health-check requests pass through unchanged.
    $$\text{req.rawPathInfo} \ne \text{path} \implies \text{healthCheck}(\text{path})(\text{app}, \text{req}) = \text{app}(\text{req})$$ -/
theorem healthCheck_passthrough (path : String) (app : Application) (req : Request)
    (respond : Response → Control.Concurrent.Green.Green ResponseReceived)
    (h : (req.rawPathInfo == path) = false) :
    healthCheck path app req respond = app req respond := by
  simp [healthCheck, h]

end Network.WebApp.Extra.Middleware
