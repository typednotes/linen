/-
  Examples.Server — `Network.WebApp.Server`, the concurrent HTTP/1.1 engine
  ported from Hale's `Warp` (renamed to fit this project's naming
  convention: `Network.Wai.Handler.Warp` → `Network.WebApp.Server`).

  Where `Examples.WebApp` drives its `Application` over a hand-rolled,
  single-request-at-a-time loopback transport, this example drives the
  exact same `demoApplication` over the *real* server:
  `Network.WebApp.Server.withApplication` spins up `Server.Run`'s
  `EventDispatcher`-backed accept loop on an OS-assigned port, hands back
  the actual bound port, runs `action`, and tears the server down again --
  no sockets, no request/response framing, no accept loop to hand-roll.
  That the very same `Application` value works unmodified against both
  transports is `Network.WebApp`'s whole point.

  Requests below carry `Connection: close`, matching
  `Server.WithApplicationTest`'s convention: the real server defaults to
  HTTP/1.1 keep-alive, and `Examples.WebApp.sendRequest`'s client reads
  until EOF, so without it the client would block waiting for a connection
  the server has no reason to close.

  Args: (none) -- runs a few round trips and exits non-zero on any mismatch
-/
import Linen.Network.WebApp.Server.WithApplication
import Examples.WebApp

open Network.WebApp.Server

namespace Examples.Server

def demoRoundTrip : IO Bool := do
  IO.println "── Network.WebApp.Server: the real HTTP/1.1 engine (née Warp) ──"
  withApplication (pure Examples.WebApp.demoApplication) fun port => do
    IO.println s!"  server listening on 127.0.0.1:{port}"

    let (echoStatus, echoBody) ← Examples.WebApp.sendRequest port "POST" "/anything"
      "Content-Type: text/plain\r\nConnection: close\r\n" "hello server"
    IO.println s!"  POST /anything -> {echoStatus} {echoBody}"

    let (healthStatus, healthBody) ← Examples.WebApp.sendRequest port "GET" "/health"
      "Connection: close\r\n" ""
    IO.println s!"  GET /health -> {healthStatus} {healthBody}"

    pure (echoStatus == 200 && echoBody == "hello server" &&
          healthStatus == 200 && healthBody == "healthy")

def run (_args : List String) : IO Unit := do
  if ← demoRoundTrip then
    IO.println "\nserver demo done · all checks passed"
  else
    throw (IO.userError "server demo done · some checks failed")

end Examples.Server
