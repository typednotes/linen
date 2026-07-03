/-
  Linen.Network.WebApp.Server — HTTP server

  A WAI-style HTTP server, renamed from Haskell's `Warp` to `Server` per this
  project's naming convention (matching `Network.HTTP2.Server`,
  `Network.HTTP3.Server`, `Network.QUIC.Server`).

  Ports Hale's `Network.Wai.Handler.Warp` (package root).
-/
import Linen.Network.WebApp.Server.Types
import Linen.Network.WebApp.Server.Internal
import Linen.Network.WebApp.Server.Settings
import Linen.Network.WebApp.Server.Request
import Linen.Network.WebApp.Server.Response
import Linen.Network.WebApp.Server.Run
import Linen.Network.WebApp.Server.WithApplication

namespace Network.WebApp.Server

/-- Run a web application on the given port with default settings.
    $$\text{run} : \text{UInt16} \to \text{Application} \to \text{IO Unit}$$ -/
def run (port : UInt16) (app : Network.WebApp.Application) : IO Unit :=
  runSettings { defaultSettings with settingsPort := port } app

end Network.WebApp.Server
