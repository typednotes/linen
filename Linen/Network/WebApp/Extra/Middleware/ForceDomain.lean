/-
  Linen.Network.WebApp.Extra.Middleware.ForceDomain — redirect to a
  canonical domain

  Ports `Network.Wai.Middleware.ForceDomain`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types

/-- Redirect to a canonical domain if the Host header doesn't match.
    $$\text{forceDomain} : (\text{String} \to \text{Option String}) \to \text{Middleware}$$ -/
def forceDomain (checkDomain : String → Option String) : Middleware :=
  fun app req respond =>
    match req.requestHeaderHost >>= checkDomain with
    | some newHost =>
      let scheme := if req.isSecure then "https://" else "http://"
      let url := scheme ++ newHost ++ req.rawPathInfo ++ req.rawQueryString
      AppM.respond respond (.responseBuilder status301 [(hLocation, url)] ByteArray.empty)
    | none => app req respond

end Network.WebApp.Extra.Middleware
