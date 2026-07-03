import Linen.Network.WebApp.Extra.Middleware.CombineHeaders
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.CombineHeaders`

    Coverage: duplicate response headers with the same name are merged into
    a single header with comma-joined values. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.CombineHeaders

def multiHeader : Data.CI String := Data.CI.mk' "X-Multi"

def dupHeadersApp : Application :=
  fun _req respond =>
    AppM.respond respond (.responseBuilder status200 [(multiHeader, "a"), (multiHeader, "b")] ByteArray.empty)

#eval show IO Unit from do
  let resp ← get (combineHeaders dupHeadersApp) "/"
  unless resp.simpleHeaders == [(multiHeader, "a, b")] do
    throw (IO.userError s!"expected a single combined header, got {resp.simpleHeaders}")

end Tests.Network.WebApp.Extra.Middleware.CombineHeaders
