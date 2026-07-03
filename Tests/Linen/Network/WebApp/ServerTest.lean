/-
  Tests for `Linen.Network.WebApp.Server` (package aggregator + `run`).

  `run` blocks forever accepting connections (its underlying `acceptLoop`
  is a `while true` loop), so it is pinned at the type level rather than
  invoked — the same convention used for `Network.HTTP2.Server.run`.
-/
import Linen.Network.WebApp.Server

open Network.WebApp.Server
open Network.WebApp (Application)

namespace Tests.Network.WebApp.Server

example : UInt16 → Application → IO Unit := run

end Tests.Network.WebApp.Server
