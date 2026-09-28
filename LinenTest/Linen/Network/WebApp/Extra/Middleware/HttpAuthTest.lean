import Linen.Network.WebApp.Extra.Middleware.HttpAuth
import Linen.Network.WebApp.Extra.Test
import Linen.Data.Base64

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.HttpAuth`

    Coverage: `Basic` auth with correct/incorrect/absent credentials. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.HttpAuth

def okApp : Application :=
  fun _req respond => AppM.respond respond (responseLBS status200 [] "secret-page")

def check : CheckCreds := fun user pass => pure (user == "admin" && pass == "hunter2")

def basicHeader (creds : String) : String :=
  "Basic " ++ Data.Base64.encode creds.toUTF8

#eval show IO Unit from do
  let resp ← runSession (basicAuth check "Restricted" okApp) { headers := [(hAuthorization, basicHeader "admin:hunter2")] }
  unless String.fromUTF8! resp.simpleBody == "secret-page" do
    throw (IO.userError "expected access with correct credentials")

#eval show IO Unit from do
  let resp ← runSession (basicAuth check "Restricted" okApp) { headers := [(hAuthorization, basicHeader "admin:wrong")] }
  unless resp.simpleStatus.statusCode == 401 do
    throw (IO.userError s!"expected 401 for wrong password, got {resp.simpleStatus.statusCode}")

#eval show IO Unit from do
  let resp ← runSession (basicAuth check "Restricted" okApp) {}
  unless resp.simpleStatus.statusCode == 401 do
    throw (IO.userError "expected 401 with no Authorization header")
  unless resp.simpleHeaders.any (fun (n, _) => n == hWWWAuthenticate) do
    throw (IO.userError "expected a WWW-Authenticate challenge header")

end Tests.Network.WebApp.Extra.Middleware.HttpAuth
