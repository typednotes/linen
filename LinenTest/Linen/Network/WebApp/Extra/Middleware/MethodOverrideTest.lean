import Linen.Network.WebApp.Extra.Middleware.MethodOverride
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.MethodOverride`

    Coverage: an `_method` query parameter overrides the request method;
    its absence leaves the method unchanged. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.MethodOverride

def echoMethodApp : Application :=
  fun req respond => AppM.respond respond (responseLBS status200 [] (toString req.requestMethod))

#eval show IO Unit from do
  let resp ← get (methodOverride echoMethodApp) "/?_method=DELETE"
  unless String.fromUTF8! resp.simpleBody == toString (Method.standard .DELETE) do
    throw (IO.userError s!"expected DELETE, got {String.fromUTF8! resp.simpleBody}")

#eval show IO Unit from do
  let resp ← get (methodOverride echoMethodApp) "/"
  unless String.fromUTF8! resp.simpleBody == toString (Method.standard .GET) do
    throw (IO.userError s!"expected unchanged GET, got {String.fromUTF8! resp.simpleBody}")

end Tests.Network.WebApp.Extra.Middleware.MethodOverride
