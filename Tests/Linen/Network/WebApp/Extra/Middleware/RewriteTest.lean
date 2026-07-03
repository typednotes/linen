import Linen.Network.WebApp.Extra.Middleware.Rewrite
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.Rewrite`

    Coverage: a custom `RewriteRule` rewrites path segments/headers, and
    `rewritePrefix` strips/replaces a raw-path prefix. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.Rewrite

def echoPathApp : Application :=
  fun req respond => AppM.respond respond (responseLBS status200 [] req.rawPathInfo)

def dropFirst : RewriteRule :=
  fun segments headers => (segments.drop 1, headers)

#eval show IO Unit from do
  let resp ← get (rewrite dropFirst echoPathApp) "/skip/keep/me"
  unless String.fromUTF8! resp.simpleBody == "/keep/me" do
    throw (IO.userError s!"expected /keep/me, got {String.fromUTF8! resp.simpleBody}")

#eval show IO Unit from do
  let resp ← get (rewritePrefix "/old" "/new" echoPathApp) "/old/thing"
  unless String.fromUTF8! resp.simpleBody == "/new/thing" do
    throw (IO.userError s!"expected /new/thing, got {String.fromUTF8! resp.simpleBody}")

#eval show IO Unit from do
  let resp ← get (rewritePrefix "/old" "/new" echoPathApp) "/other"
  unless String.fromUTF8! resp.simpleBody == "/other" do
    throw (IO.userError "expected passthrough when prefix doesn't match")

end Tests.Network.WebApp.Extra.Middleware.Rewrite
