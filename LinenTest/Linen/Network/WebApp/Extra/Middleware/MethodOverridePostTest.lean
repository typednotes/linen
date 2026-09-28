import Linen.Network.WebApp.Extra.Middleware.MethodOverridePost
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.MethodOverridePost`

    Coverage: a POST body's `_method` parameter overrides the method, and
    the consumed chunk is replayed so the body is still fully readable;
    non-POST requests pass through unchanged. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.MethodOverridePost

def echoApp : Application :=
  fun req respond =>
    AppM.respondIO respond do
      let body ← Network.WebApp.strictRequestBody req
      pure (responseLBS status200 [] s!"{req.requestMethod}:{String.fromUTF8! body}")

#eval show IO Unit from do
  let resp ← post (methodOverridePost echoApp) "/" "_method=PATCH&a=1".toUTF8 "application/x-www-form-urlencoded"
  unless String.fromUTF8! resp.simpleBody == s!"{Method.standard .PATCH}:_method=PATCH&a=1" do
    throw (IO.userError s!"unexpected response: {String.fromUTF8! resp.simpleBody}")

#eval show IO Unit from do
  let resp ← get echoApp "/"
  unless String.fromUTF8! resp.simpleBody == s!"{Method.standard .GET}:" do
    throw (IO.userError s!"unexpected passthrough for GET: {String.fromUTF8! resp.simpleBody}")

end Tests.Network.WebApp.Extra.Middleware.MethodOverridePost
