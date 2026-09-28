import Linen.Network.WebApp.Extra.Middleware.CleanPath
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.CleanPath`

    Coverage: paths with duplicate slashes are 301-redirected to the
    cleaned path; already-clean paths pass through unchanged. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.CleanPath

def echoPathApp : Application :=
  fun req respond => AppM.respond respond (responseLBS status200 [] req.rawPathInfo)

#eval show IO Unit from do
  let resp ← get (cleanPath echoPathApp) "/foo//bar"
  unless resp.simpleStatus.statusCode == 301 do
    throw (IO.userError s!"expected 301, got {resp.simpleStatus.statusCode}")
  unless resp.simpleHeaders.any (fun (n, v) => n == hLocation && v == "/foo/bar") do
    throw (IO.userError "expected Location: /foo/bar")

#eval show IO Unit from do
  let resp ← get (cleanPath echoPathApp) "/foo/bar"
  unless String.fromUTF8! resp.simpleBody == "/foo/bar" do
    throw (IO.userError s!"expected passthrough, got {String.fromUTF8! resp.simpleBody}")

end Tests.Network.WebApp.Extra.Middleware.CleanPath
