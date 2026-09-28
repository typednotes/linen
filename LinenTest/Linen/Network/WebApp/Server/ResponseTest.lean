/-
  Tests for `Linen.Network.WebApp.Server.Response`.

  `sendResponse`/`sendResponseEL` need a live connected socket, so their IO
  entry points are pinned at the type level. The pure rendering helpers are
  checked with `#guard`.
-/
import Linen.Network.WebApp.Server.Response

open Network.WebApp.Server
open Network.HTTP.Types
open Network.Socket
open Control.Concurrent.Green (Green)

namespace Tests.Network.WebApp.Server.Response

/-! ### Status-line / header rendering (pure) -/

#guard renderStatusLine http11 status200 == "HTTP/1.1 200 OK\r\n"
#guard renderStatusLineBytes http11 status200 == "HTTP/1.1 200 OK\r\n".toUTF8

#guard renderHeaders [(hContentLength, "5")] == "Content-Length: 5\r\n"
#guard renderHeaders [] == ""
#guard renderHeadersBytes [(hContentLength, "5")] == "Content-Length: 5\r\n".toUTF8

/-! ### IO handlers — signatures (need a live connected socket) -/

example : Socket .connected → Settings → Network.WebApp.Request → Network.WebApp.Response →
    Green Network.WebApp.ResponseReceived := sendResponse
example : Socket .connected → Settings → Network.WebApp.Request → Network.WebApp.Response →
    EventDispatcher → Green Network.WebApp.ResponseReceived := sendResponseEL

end Tests.Network.WebApp.Server.Response
