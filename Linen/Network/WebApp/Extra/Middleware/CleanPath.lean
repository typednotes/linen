/-
  Linen.Network.WebApp.Extra.Middleware.CleanPath — normalize URL paths

  Removes double slashes and trailing slashes from request paths,
  redirecting to the canonical form. Ports Hale's
  `Network.Wai.Middleware.CleanPath`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types

/-- Remove duplicate/trailing slashes from paths, redirecting to the clean
    path with 301 if needed.
    $$\text{cleanPath} : \text{Middleware}$$ -/
def cleanPath : Middleware :=
  fun app req respond =>
    let path := req.rawPathInfo
    let cleaned := cleanPathStr path
    if cleaned != path && !path.isEmpty then
      let url := cleaned ++ req.rawQueryString
      AppM.respond respond (.responseBuilder status301 [(hLocation, url)] ByteArray.empty)
    else
      app req respond
where
  cleanPathStr (s : String) : String :=
    let parts := s.splitOn "/"
    let nonEmpty := parts.filter (!·.isEmpty)
    "/" ++ "/".intercalate nonEmpty

end Network.WebApp.Extra.Middleware
