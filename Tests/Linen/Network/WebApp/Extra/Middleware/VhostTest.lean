import Linen.Network.WebApp.Extra.Middleware.Vhost
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.Vhost`

    Coverage: a matching `Host` entry routes to its application; when no
    entry matches, the fallback application handles the request.
    `Network.WebApp.Extra.Test` always simulates `Host: localhost`, so the
    matching case is keyed on that. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.Vhost

def siteApp : Application :=
  fun _req respond => AppM.respond respond (responseLBS status200 [] "site")

def fallbackApp : Application :=
  fun _req respond => AppM.respond respond (responseLBS status200 [] "fallback")

#eval show IO Unit from do
  let resp ← get (vhost [("localhost", siteApp)] fallbackApp) "/"
  unless String.fromUTF8! resp.simpleBody == "site" do
    throw (IO.userError s!"expected the localhost vhost app, got {String.fromUTF8! resp.simpleBody}")

#eval show IO Unit from do
  let resp ← get (vhost [("other.example.com", siteApp)] fallbackApp) "/"
  unless String.fromUTF8! resp.simpleBody == "fallback" do
    throw (IO.userError s!"expected the fallback app, got {String.fromUTF8! resp.simpleBody}")

end Tests.Network.WebApp.Extra.Middleware.Vhost
