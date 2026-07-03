import Linen.Network.WebApp.Extra.Middleware.AddHeaders
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.AddHeaders`

    Coverage: headers are appended to the wrapped app's response, plus the
    `addHeaders_nil_*` identity laws proved in the source module. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.AddHeaders

def plainApp : Application :=
  fun _req respond => AppM.respond respond (responseLBS status200 [] "hi")

def extraHeader : Data.CI String := Data.CI.mk' "X-Test"

#eval show IO Unit from do
  let resp ← get (addHeaders [(extraHeader, "yes")] plainApp) "/"
  unless resp.simpleHeaders.any (fun (n, v) => n == extraHeader && v == "yes") do
    throw (IO.userError "expected X-Test header to be added")

example (s : Status) (h : ResponseHeaders) (b : ByteArray) :
    (Response.responseBuilder s h b).mapResponseHeaders (· ++ ([] : ResponseHeaders))
      = .responseBuilder s h b := addHeaders_nil_builder s h b

end Tests.Network.WebApp.Extra.Middleware.AddHeaders
