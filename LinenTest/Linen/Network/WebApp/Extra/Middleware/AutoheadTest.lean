import Linen.Network.WebApp.Extra.Middleware.Autohead
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.Autohead`

    Coverage: a HEAD request is converted to GET and gets its body
    stripped; other methods pass through unchanged. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.Autohead

def bodyApp : Application :=
  fun req respond =>
    AppM.respond respond (responseLBS status200 [] s!"method={req.requestMethod}")

#eval show IO Unit from do
  let resp ← runSession (autohead bodyApp) { method := .standard .HEAD }
  unless resp.simpleBody.isEmpty do
    throw (IO.userError "expected empty body for HEAD request")

#eval show IO Unit from do
  let resp ← get (autohead bodyApp) "/"
  unless !resp.simpleBody.isEmpty do
    throw (IO.userError "expected non-empty body for GET request")

end Tests.Network.WebApp.Extra.Middleware.Autohead
