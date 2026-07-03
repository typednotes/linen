/-
  Tests for `Linen.Network.WebApp.Server.Request`.
-/
import Linen.Network.WebApp.Server.Request

open Network.WebApp.Server
open Network.HTTP.Types

namespace Tests.Network.WebApp.Server.Request

/-! ### `parseHttpVersion` -/

#guard parseHttpVersion "HTTP/1.1" == some http11
#guard parseHttpVersion "HTTP/1.0" == some http10
#guard parseHttpVersion "HTTP/0.9" == some http09
#guard parseHttpVersion "HTTP/2.0" == some http20
#guard parseHttpVersion "HTTP/3.7" == some ⟨3, 7⟩
#guard parseHttpVersion "bogus" == none

example : parseHttpVersion "HTTP/1.1" = some http11 := parseHttpVersion_http11
example : parseRequestLine "" = none := parseRequestLine_empty

/-! ### `parseRequestLine` -/

#guard parseRequestLine "GET /path?q=1 HTTP/1.1" ==
  some (.standard .GET, "/path", "?q=1", http11)
#guard parseRequestLine "POST / HTTP/1.0" == some (.standard .POST, "/", "", http10)
#guard parseRequestLine "" == none
#guard parseRequestLine "GET /only-two-fields" == none

/-! ### `parseHeaderLine` / `parseHeaders` -/

#guard parseHeaderLine "Content-Type: text/html" ==
  some (Data.CI.mk' "Content-Type", "text/html")
#guard parseHeaderLine "no-colon-here" == none

#guard parseHeaders ["Host: example.com", "X-Test: 1"] ==
  [(Data.CI.mk' "Host", "example.com"), (Data.CI.mk' "X-Test", "1")]
#guard parseHeaders ["not-a-header"] == []

/-! ### IO entry points — signatures (need a live buffered socket reader) -/

example : Network.Socket.FFI.RecvBuffer → IO (String × List String) := recvHeaders
example : Network.Socket.FFI.RecvBuffer → Network.Socket.SockAddr →
    IO (Option Network.WebApp.Request) := parseRequest

end Tests.Network.WebApp.Server.Request
