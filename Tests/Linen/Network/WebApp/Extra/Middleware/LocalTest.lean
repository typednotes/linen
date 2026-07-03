import Linen.Network.WebApp.Extra.Middleware.Local
import Linen.Network.WebApp.Extra.Test
import Linen.Control.Concurrent.Green

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.Local`

    Coverage: localhost clients pass through; remote clients get 403. The
    remote-client case needs a custom `remoteHost`, which
    `Network.WebApp.Extra.Test`'s `SRequest` doesn't expose (it always
    simulates `127.0.0.1`), so this runs `Request`s directly via `Green.block`
    following the pattern in `WebApp.Static.ApplicationTest`. -/

open Network.WebApp Network.WebApp.Extra.Middleware
open Network.HTTP.Types
open Control.Concurrent.Green

namespace Tests.Network.WebApp.Extra.Middleware.Local

def okApp : Application :=
  fun _req respond => AppM.respond respond (responseLBS status200 [] "ok")

/-- Run `app` against `req`, capturing the `Response` it produces. -/
def runApp (app : Application) (req : Request) : IO Response := do
  let tok ← Std.CancellationToken.new
  let captured ← IO.mkRef (none : Option Response)
  let respond : Response → Green ResponseReceived := fun resp =>
    (do captured.set (some resp); pure ResponseReceived.done : IO ResponseReceived)
  let _ ← Green.block (app req respond).run tok
  match ← captured.get with
  | some resp => pure resp
  | none => throw (IO.userError "app: respond was never called")

#eval show IO Unit from do
  let resp ← runApp (localOnly okApp) { defaultRequest with remoteHost := ⟨"127.0.0.1", 0⟩ }
  unless resp.status.statusCode == 200 do
    throw (IO.userError s!"expected 200 for localhost, got {resp.status.statusCode}")

#eval show IO Unit from do
  let resp ← runApp (localOnly okApp) { defaultRequest with remoteHost := ⟨"203.0.113.5", 0⟩ }
  unless resp.status.statusCode == 403 do
    throw (IO.userError s!"expected 403 for remote client, got {resp.status.statusCode}")

end Tests.Network.WebApp.Extra.Middleware.Local
