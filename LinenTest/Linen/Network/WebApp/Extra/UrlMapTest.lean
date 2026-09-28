import Linen.Network.WebApp.Extra.UrlMap
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.UrlMap`

    Coverage: prefix dispatch (with the prefix stripped from `rawPathInfo`
    and `pathInfo` before delegating), and fallback when no route matches. -/

open Network.WebApp Network.WebApp.Extra Network.WebApp.Extra.Test Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.UrlMap

/-- Echoes back the (already-stripped) path it was routed with. -/
def echoPathApp : Application :=
  fun req respond => AppM.respondIO respond (pure (responseLBS status200 [] req.rawPathInfo))

def fallbackApp : Application :=
  fun _req respond => AppM.respondIO respond (pure (responseLBS status200 [] "fallback"))

def routedApp : Application :=
  urlMap [("/api", echoPathApp)] fallbackApp

#eval show IO Unit from do
  let resp ← get routedApp "/api/widgets"
  unless String.fromUTF8! resp.simpleBody == "/widgets" do
    throw (IO.userError s!"expected /widgets, got {String.fromUTF8! resp.simpleBody}")

#eval show IO Unit from do
  let resp ← get routedApp "/other"
  unless String.fromUTF8! resp.simpleBody == "fallback" do
    throw (IO.userError s!"expected fallback, got {String.fromUTF8! resp.simpleBody}")

end Tests.Network.WebApp.Extra.UrlMap
