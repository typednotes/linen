import Linen.Network.WebApp.Extra.Middleware.Jsonp
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.Jsonp`

    Coverage: JSON responses are wrapped in the `callback` query parameter
    when present; other responses/requests pass through unchanged. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.Jsonp

def jsonApp : Application :=
  fun _req respond =>
    AppM.respond respond (responseLBS status200 [(hContentType, "application/json")] "{\"a\":1}")

#eval show IO Unit from do
  let resp ← get (jsonp jsonApp) "/?callback=handle"
  unless String.fromUTF8! resp.simpleBody == "handle({\"a\":1})" do
    throw (IO.userError s!"expected JSONP wrapping, got {String.fromUTF8! resp.simpleBody}")
  unless resp.simpleHeaders.any (fun (n, v) => n == hContentType && v == "application/javascript") do
    throw (IO.userError "expected Content-Type rewritten to application/javascript")

#eval show IO Unit from do
  let resp ← get (jsonp jsonApp) "/"
  unless String.fromUTF8! resp.simpleBody == "{\"a\":1}" do
    throw (IO.userError "expected passthrough when no callback parameter")

end Tests.Network.WebApp.Extra.Middleware.Jsonp
