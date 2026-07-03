import Linen.Network.WebApp.Extra.Middleware.Approot
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.Approot`

    Coverage: `approotMiddleware` passes every request through unchanged,
    and `getApprootFromRequest` derives `scheme://host` from `isSecure` /
    `X-Forwarded-Proto` and the `Host` header. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types
open Data (CI)

namespace Tests.Network.WebApp.Extra.Middleware.Approot

def plainApp : Application :=
  fun _req respond => AppM.respond respond (responseLBS status200 [] "unchanged")

#eval show IO Unit from do
  let resp ← get (approotMiddleware (fun _ => "ignored") plainApp) "/"
  unless String.fromUTF8! resp.simpleBody == "unchanged" do
    throw (IO.userError "expected approotMiddleware to pass the request through unchanged")

#guard getApprootFromRequest { defaultRequest with isSecure := true, requestHeaderHost := some "example.com" }
  == "https://example.com"
#guard getApprootFromRequest { defaultRequest with requestHeaderHost := some "example.com" }
  == "http://example.com"
#guard getApprootFromRequest { defaultRequest with
    requestHeaders := [(CI.mk' "X-Forwarded-Proto", "https")], requestHeaderHost := some "example.com" }
  == "https://example.com"
#guard getApprootFromRequest defaultRequest == "http://localhost"

end Tests.Network.WebApp.Extra.Middleware.Approot
