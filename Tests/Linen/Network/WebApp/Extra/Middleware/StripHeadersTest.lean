import Linen.Network.WebApp.Extra.Middleware.StripHeaders
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.StripHeaders`

    Coverage: named headers are removed from responses; others are kept. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.StripHeaders

def secretHeader : Data.CI String := Data.CI.mk' "X-Secret"

def taggedApp : Application :=
  fun _req respond =>
    AppM.respond respond (.responseBuilder status200 [(secretHeader, "shh"), (hContentType, "text/plain")] ByteArray.empty)

#eval show IO Unit from do
  let resp ← get (stripHeaders [secretHeader] taggedApp) "/"
  unless !resp.simpleHeaders.any (fun (n, _) => n == secretHeader) do
    throw (IO.userError "expected X-Secret to be stripped")
  unless resp.simpleHeaders.any (fun (n, _) => n == hContentType) do
    throw (IO.userError "expected Content-Type to be kept")

end Tests.Network.WebApp.Extra.Middleware.StripHeaders
