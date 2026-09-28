import Linen.Network.WebApp.Extra.Middleware.HealthCheckEndpoint
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.HealthCheckEndpoint`

    Coverage: the configured path short-circuits with an empty 200; other
    paths pass through (also proved as `healthCheck_passthrough` by `rfl`
    in the source module). -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.HealthCheckEndpoint

def okApp : Application :=
  fun _req respond => AppM.respond respond (responseLBS status200 [] "app-response")

#eval show IO Unit from do
  let resp ← get (healthCheck "/_health" okApp) "/_health"
  unless resp.simpleStatus.statusCode == 200 do
    throw (IO.userError s!"expected 200, got {resp.simpleStatus.statusCode}")
  unless resp.simpleBody.isEmpty do
    throw (IO.userError "expected empty body from health check")

#eval show IO Unit from do
  let resp ← get (healthCheck "/_health" okApp) "/other"
  unless String.fromUTF8! resp.simpleBody == "app-response" do
    throw (IO.userError "expected passthrough for non-health-check path")

end Tests.Network.WebApp.Extra.Middleware.HealthCheckEndpoint
