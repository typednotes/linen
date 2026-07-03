import Linen.Network.WebApp.Extra.Middleware.RequestLogger.JSON
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.RequestLogger.JSON`

    Coverage: `formatJSON` renders the expected JSON log line, and `logJSON`
    builds a middleware that passes the wrapped app's response through
    unchanged. -/

open Network.WebApp Network.WebApp.Extra.Test
open Network.WebApp.Extra.Middleware.RequestLogger
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.RequestLogger.JSON

#guard formatJSON { defaultRequest with rawPathInfo := "/x", requestHeaderUserAgent := some "curl" } status200
  == "{\"method\":\"GET\",\"path\":\"/x\",\"status\":200,\"host\":\"0.0.0.0\",\"userAgent\":\"curl\"}"

def okApp : Application :=
  fun _req respond => AppM.respond respond (responseLBS status200 [] "handled")

#eval show IO Unit from do
  let mw ← logJSON
  let resp ← get (mw okApp) "/"
  unless String.fromUTF8! resp.simpleBody == "handled" do
    throw (IO.userError "expected logJSON to pass the response through")

end Tests.Network.WebApp.Extra.Middleware.RequestLogger.JSON
