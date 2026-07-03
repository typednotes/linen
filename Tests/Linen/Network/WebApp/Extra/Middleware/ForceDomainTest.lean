import Linen.Network.WebApp.Extra.Middleware.ForceDomain
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.ForceDomain`

    Coverage: redirect to the canonical domain when `checkDomain` matches
    the request's `Host`, passthrough otherwise. `Network.WebApp.Extra.Test`
    always simulates `Host: localhost`, so `checkDomain` is keyed on that. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.ForceDomain

def okApp : Application :=
  fun _req respond => AppM.respond respond (responseLBS status200 [] "ok")

#eval show IO Unit from do
  let mw := forceDomain (fun h => if h == "localhost" then some "canonical.example.com" else none)
  let resp ← get (mw okApp) "/path"
  unless resp.simpleStatus.statusCode == 301 do
    throw (IO.userError s!"expected 301, got {resp.simpleStatus.statusCode}")
  unless resp.simpleHeaders.any (fun (n, v) => n == hLocation && v == "http://canonical.example.com/path") do
    throw (IO.userError s!"unexpected Location header: {resp.simpleHeaders}")

#eval show IO Unit from do
  let mw := forceDomain (fun h => if h == "other.example.com" then some "x" else none)
  let resp ← get (mw okApp) "/"
  unless String.fromUTF8! resp.simpleBody == "ok" do
    throw (IO.userError "expected passthrough when Host doesn't match")

end Tests.Network.WebApp.Extra.Middleware.ForceDomain
