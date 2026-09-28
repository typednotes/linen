import Linen.Network.WebApp.Extra.Middleware.RequestLogger
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.RequestLogger`

    Coverage: `logRequests`/`logStdoutDev` build a middleware that passes
    the wrapped app's response through unchanged, for both output formats.
    (The formatting helpers themselves are `private`, and the underlying
    `LoggerSet` buffers by message count rather than flushing per request,
    so this exercises the public passthrough contract rather than the
    buffered log content.) -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Middleware.RequestLogger

def okApp : Application :=
  fun _req respond => AppM.respond respond (responseLBS status200 [] "handled")

#eval show IO Unit from do
  let mw ← logRequests { outputFormat := .apache, destination := .callback (fun _ => pure ()) }
  let resp ← get (mw okApp) "/"
  unless String.fromUTF8! resp.simpleBody == "handled" do
    throw (IO.userError "expected apache-format logRequests to pass the response through")

#eval show IO Unit from do
  let mw ← logRequests { outputFormat := .dev, destination := .callback (fun _ => pure ()) }
  let resp ← get (mw okApp) "/"
  unless String.fromUTF8! resp.simpleBody == "handled" do
    throw (IO.userError "expected dev-format logRequests to pass the response through")

#eval show IO Unit from do
  let mw ← logStdoutDev
  let resp ← get (mw okApp) "/"
  unless resp.simpleStatus.statusCode == 200 do
    throw (IO.userError "expected logStdoutDev to pass the response through")

end Tests.Network.WebApp.Extra.Middleware.RequestLogger
