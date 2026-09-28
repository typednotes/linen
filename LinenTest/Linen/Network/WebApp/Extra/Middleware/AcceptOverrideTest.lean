import Linen.Network.WebApp.Extra.Middleware.AcceptOverride
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.AcceptOverride`

    Coverage: overriding the `Accept` header from an `_accept` query
    parameter, and passthrough when the parameter is absent. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.AcceptOverride

def echoAccept : Application :=
  fun req respond =>
    AppM.respond respond
      (responseLBS status200 [] ((requestHeader hAccept req).getD "none"))

def routedApp : Application := acceptOverride echoAccept

#eval show IO Unit from do
  let resp ← get routedApp "/?_accept=application/json"
  unless String.fromUTF8! resp.simpleBody == "application/json" do
    throw (IO.userError s!"expected overridden Accept, got {String.fromUTF8! resp.simpleBody}")

#eval show IO Unit from do
  let resp ← get routedApp "/"
  unless String.fromUTF8! resp.simpleBody == "none" do
    throw (IO.userError s!"expected no Accept header, got {String.fromUTF8! resp.simpleBody}")

end Tests.Network.WebApp.Extra.Middleware.AcceptOverride
