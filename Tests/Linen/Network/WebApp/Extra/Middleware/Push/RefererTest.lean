import Linen.Network.WebApp.Extra.Middleware.Push.Referer
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.Push.pushOnReferer`

    Coverage: a request for a static resource carrying a `Referer` header
    teaches the manager the page → resource association; a later request for
    that page gets a `Link: <resource>; rel=preload` header injected. An
    unseen page gets no `Link` header at all. -/

open Network.WebApp Network.WebApp.Extra.Middleware.Push Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.Push.Referer

def okApp : Application :=
  fun _req respond => AppM.respond respond (responseLBS status200 [] "ok")

#eval show IO Unit from do
  let mw ← pushOnReferer
  let app := mw okApp
  -- Requesting the static resource with a Referer pointing at "/" teaches the
  -- manager that "/" should push "/app.css".
  let _ ← runSession app { path := "/app.css", headers := [(hReferer, "/")] }
  let resp ← get app "/"
  unless resp.simpleHeaders.any (fun (n, v) => n == Data.CI.mk' "Link" && v == "</app.css>; rel=preload") do
    throw (IO.userError s!"expected a Link preload header, got {resp.simpleHeaders}")

#eval show IO Unit from do
  let mw ← pushOnReferer
  let app := mw okApp
  let resp ← get app "/"
  unless resp.simpleHeaders.all (fun (n, _) => n != Data.CI.mk' "Link") do
    throw (IO.userError s!"expected no Link header for an unseen page, got {resp.simpleHeaders}")

end Tests.Network.WebApp.Extra.Middleware.Push.Referer
