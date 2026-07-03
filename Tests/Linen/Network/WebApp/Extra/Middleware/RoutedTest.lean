import Linen.Network.WebApp.Extra.Middleware.Routed
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.Routed`

    Coverage: `routed`/`routedPrefix` apply a middleware only to matching
    requests, plus the `routed_true`/`routed_false` identity laws proved in
    the source module. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.Routed

def echoPathApp : Application :=
  fun req respond => AppM.respond respond (responseLBS status200 [] req.rawPathInfo)

def tagHeader : Data.CI String := Data.CI.mk' "X-Tagged"

def tag : Middleware :=
  fun app req respond => app req fun resp => respond (resp.mapResponseHeaders ((tagHeader, "yes") :: ·))

#eval show IO Unit from do
  let resp ← get (routedPrefix "/api" tag echoPathApp) "/api/widgets"
  unless resp.simpleHeaders.any (fun (n, _) => n == tagHeader) do
    throw (IO.userError "expected tagging middleware applied under /api")

#eval show IO Unit from do
  let resp ← get (routedPrefix "/api" tag echoPathApp) "/other"
  unless !resp.simpleHeaders.any (fun (n, _) => n == tagHeader) do
    throw (IO.userError "expected no tagging outside /api")

example (middle : Middleware) : routed (fun _ => true) middle = middle := routed_true middle
example (middle : Middleware) : routed (fun _ => false) middle = (id : Middleware) := routed_false middle

end Tests.Network.WebApp.Extra.Middleware.Routed
