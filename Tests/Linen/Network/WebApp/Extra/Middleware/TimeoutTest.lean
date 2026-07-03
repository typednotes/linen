import Linen.Network.WebApp.Extra.Middleware.Timeout
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.Timeout`

    Coverage: an app that responds well within the deadline gets its normal
    response through; an app slower than the deadline gets pre-empted with
    a 503. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.Timeout

def fastApp : Application :=
  fun _req respond => AppM.respondIO respond (pure (responseLBS status200 [] "fast"))

def slowApp : Application :=
  fun _req respond => AppM.respondIO respond (do IO.sleep 300; pure (responseLBS status200 [] "slow"))

#eval show IO Unit from do
  let resp ← get (timeout 200 fastApp) "/"
  unless String.fromUTF8! resp.simpleBody == "fast" do
    throw (IO.userError s!"expected the fast app's response, got {String.fromUTF8! resp.simpleBody}")

#eval show IO Unit from do
  let resp ← get (timeout 50 slowApp) "/"
  unless resp.simpleStatus.statusCode == 503 do
    throw (IO.userError s!"expected 503 on timeout, got {resp.simpleStatus.statusCode}")

end Tests.Network.WebApp.Extra.Middleware.Timeout
