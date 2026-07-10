import Linen.Network.WebApp.Extra.Middleware.Gzip
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.Gzip`

    Coverage: with actual compression deferred (matching the upstream
    project's own TODO), `gzip` is a behavioral no-op for both
    gzip-accepting and non-accepting clients — this test documents that
    passthrough contract so a future compression implementation can't
    silently change the case it doesn't yet handle. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types
open Data (CI)

namespace Tests.Network.WebApp.Extra.Middleware.Gzip

def textApp : Application :=
  fun _req respond => AppM.respond respond (responseLBS status200 [(hContentType, "text/plain")] "hello world")

#eval show IO Unit from do
  let resp ← runSession (gzip {} textApp) { headers := [(CI.mk' "Accept-Encoding", "gzip, deflate")] }
  unless String.fromUTF8! resp.simpleBody == "hello world" do
    throw (IO.userError "expected unchanged body when client accepts gzip (compression deferred)")

#eval show IO Unit from do
  let resp ← runSession (gzip {} textApp) {}
  unless String.fromUTF8! resp.simpleBody == "hello world" do
    throw (IO.userError "expected unchanged body when client doesn't accept gzip")

end Tests.Network.WebApp.Extra.Middleware.Gzip
