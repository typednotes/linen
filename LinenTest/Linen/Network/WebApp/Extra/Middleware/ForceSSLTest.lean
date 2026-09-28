import Linen.Network.WebApp.Extra.Middleware.ForceSSL
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.ForceSSL`

    Coverage: insecure requests are redirected to `https://`; secure
    requests pass through (also proved as `forceSSL_secure` by `rfl` in the
    source module). -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.ForceSSL

def okApp : Application :=
  fun _req respond => AppM.respond respond (responseLBS status200 [] "ok")

#eval show IO Unit from do
  let resp ← runSession (forceSSL okApp) { path := "/x", isSecure := false }
  unless resp.simpleStatus.statusCode == 301 do
    throw (IO.userError s!"expected 301, got {resp.simpleStatus.statusCode}")
  unless resp.simpleHeaders.any (fun (n, v) => n == hLocation && v == "https://localhost/x") do
    throw (IO.userError s!"unexpected Location header: {resp.simpleHeaders}")

#eval show IO Unit from do
  let resp ← runSession (forceSSL okApp) { path := "/x", isSecure := true }
  unless String.fromUTF8! resp.simpleBody == "ok" do
    throw (IO.userError "expected passthrough for secure request")

example (app : Application) (req : Network.WebApp.Request)
    (respond : Response → Control.Concurrent.Green.Green ResponseReceived)
    (h : req.isSecure = true) :
    forceSSL app req respond = app req respond := forceSSL_secure app req respond h

end Tests.Network.WebApp.Extra.Middleware.ForceSSL
