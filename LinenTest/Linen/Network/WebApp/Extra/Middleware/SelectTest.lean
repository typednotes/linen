import Linen.Network.WebApp.Extra.Middleware.Select
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.Select`

    Coverage: `select` dispatches to the chosen middleware, falls through
    to the app when `none`, plus the `select_none` identity law proved in
    the source module. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.Select

def plainApp : Application :=
  fun _req respond => AppM.respond respond (responseLBS status200 [] "plain")

def upper : Middleware :=
  fun app req respond => app req fun resp => respond (resp.mapResponseStatus (fun _ => status201))

def chooser : Request → Option Middleware :=
  fun req => if req.rawPathInfo == "/special" then some upper else none

#eval show IO Unit from do
  let resp ← get (select chooser plainApp) "/special"
  unless resp.simpleStatus.statusCode == 201 do
    throw (IO.userError s!"expected 201 for /special, got {resp.simpleStatus.statusCode}")

#eval show IO Unit from do
  let resp ← get (select chooser plainApp) "/normal"
  unless resp.simpleStatus.statusCode == 200 do
    throw (IO.userError s!"expected 200 for /normal, got {resp.simpleStatus.statusCode}")

example : select (fun _ => (none : Option Middleware)) = (id : Middleware) := select_none

end Tests.Network.WebApp.Extra.Middleware.Select
