import Linen.Network.WebApp.Extra.Middleware.ValidateHeaders
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.ValidateHeaders`

    Coverage: responses with valid header values pass through; a header
    value containing a control character (CRLF) is replaced with a 500. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.ValidateHeaders

def customHeader : Data.CI String := Data.CI.mk' "X-Custom"

def goodApp : Application :=
  fun _req respond => AppM.respond respond (.responseBuilder status200 [(customHeader, "clean")] "ok".toUTF8)

def badApp : Application :=
  fun _req respond => AppM.respond respond (.responseBuilder status200 [(customHeader, "evil\r\nInjected: true")] "ok".toUTF8)

#eval show IO Unit from do
  let resp ← get (validateHeaders goodApp) "/"
  unless resp.simpleStatus.statusCode == 200 do
    throw (IO.userError s!"expected 200 for valid headers, got {resp.simpleStatus.statusCode}")

#eval show IO Unit from do
  let resp ← get (validateHeaders badApp) "/"
  unless resp.simpleStatus.statusCode == 500 do
    throw (IO.userError s!"expected 500 for CRLF-injected header, got {resp.simpleStatus.statusCode}")

end Tests.Network.WebApp.Extra.Middleware.ValidateHeaders
