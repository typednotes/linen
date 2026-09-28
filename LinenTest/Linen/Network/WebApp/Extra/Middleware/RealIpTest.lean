import Linen.Network.WebApp.Extra.Middleware.RealIp
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.RealIp`

    Coverage: `X-Forwarded-For` (leftmost/client IP, trimmed) takes
    precedence over `X-Real-IP`; absent either header, the original
    `remoteHost` is kept. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types
open Data (CI)

namespace Tests.Network.WebApp.Extra.Middleware.RealIp

def echoIpApp : Application :=
  fun req respond => AppM.respond respond (responseLBS status200 [] req.remoteHost.host)

#eval show IO Unit from do
  let resp ← runSession (realIp echoIpApp) { headers := [(CI.mk' "X-Forwarded-For", "203.0.113.9, 10.0.0.1")] }
  unless String.fromUTF8! resp.simpleBody == "203.0.113.9" do
    throw (IO.userError s!"expected leftmost X-Forwarded-For IP, got {String.fromUTF8! resp.simpleBody}")

#eval show IO Unit from do
  let resp ← runSession (realIp echoIpApp) { headers := [(CI.mk' "X-Real-IP", "198.51.100.7")] }
  unless String.fromUTF8! resp.simpleBody == "198.51.100.7" do
    throw (IO.userError s!"expected X-Real-IP, got {String.fromUTF8! resp.simpleBody}")

#eval show IO Unit from do
  let resp ← runSession (realIp echoIpApp) {}
  unless String.fromUTF8! resp.simpleBody == "127.0.0.1" do
    throw (IO.userError "expected unchanged simulated remoteHost")

end Tests.Network.WebApp.Extra.Middleware.RealIp
