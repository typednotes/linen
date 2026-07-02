/-
  Examples.HTTPClient — `Network.HTTP.Client`'s connection/request/response/
  redirect layer end-to-end.

  Stands up a tiny hand-rolled HTTP/1.1 server over a real loopback socket
  (`Network.Socket.Blocking`, the same pattern as `Examples.Recv`/`TLS`) and
  drives it with the client stack:

  * a plain request against `/hello` via `Client.connectPlain` +
    `Client.performRequest` — the low-level connect/send/receive path;
  * a request against `/redirect` via `Client.execute`, which follows the
    server's `302 Found` → `/final` hop automatically
    (`Client.executeWithRedirects`).

  Args: (none) -- runs the round trip and exits non-zero on any mismatch
-/
import Linen.Network.HTTP.Client.Connection
import Linen.Network.HTTP.Client.Redirect
import Linen.Network.HTTP.Client.Response
import Linen.Network.Socket.Blocking

open Network.Socket
open Network.Socket.Blocking
open Network.HTTP.Client
open Network.HTTP.Types

namespace Examples.HTTPClient

/-- Read from `conn` until the full request-line + headers block
(terminated by a blank line) has been buffered, returning it as a string.
Good enough for the header-only `GET` requests this demo's client sends. -/
def readRequestHeaders (conn : Socket .connected) : IO String := do
  let mut buf := ByteArray.empty
  let mut done := false
  while !done do
    let chunk ← Blocking.recv conn
    if chunk.isEmpty then
      done := true
    else
      buf := buf ++ chunk
      if ((String.fromUTF8! buf).splitOn "\r\n\r\n").length > 1 then
        done := true
  return String.fromUTF8! buf

/-- The request path from a raw HTTP/1.1 request-line-and-headers block. -/
def requestPath (raw : String) : String :=
  let requestLine := (raw.splitOn "\r\n").getD 0 ""
  (requestLine.splitOn " ").getD 1 "/"

/-- A minimal routing table: `/redirect` bounces to `/final`, everything else
(including `/final`) answers 200 with a path-specific body. -/
def respondFor (path : String) : String :=
  if path == "/redirect" then
    "HTTP/1.1 302 Found\r\nLocation: /final\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
  else
    let body := if path == "/final" then "redirected!" else "hello, http client!"
    s!"HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {body.utf8ByteSize}\r\nConnection: close\r\n\r\n{body}"

/-- Serve exactly `n` sequential connections, replying per `respondFor` and
returning the path each one asked for, in order. -/
def serveRequests (server : Socket .listening) (n : Nat) : IO (List String) := do
  let mut seen : List String := []
  for _ in [0:n] do
    let (conn, _peer) ← Blocking.accept server
    let raw ← readRequestHeaders conn
    let path := requestPath raw
    Blocking.sendAll conn (respondFor path).toUTF8
    let _ ← close conn
    seen := path :: seen
  pure seen.reverse

def demoRoundTrip : IO Bool := do
  IO.println "── Network.HTTP.Client: connect/request/response + redirects ──"
  let server ← listenTCP "127.0.0.1" 0
  let addr ← getSockName server
  IO.println s!"  server listening on 127.0.0.1:{addr.port}"
  -- three connections total: one plain GET, two for the redirect hop
  let serverTask ← IO.asTask (prio := .dedicated) (serveRequests server 3)

  -- Low-level: connectPlain + performRequest.
  let plainConn ← connectPlain "127.0.0.1" addr.port
  let plainReq : Request :=
    { method := .standard .GET, host := "127.0.0.1", port := addr.port, path := "/hello" }
  let plainResp ← performRequest plainConn plainReq
  plainConn.connClose
  IO.println s!"  GET /hello -> {plainResp.statusCode.statusCode} {String.fromUTF8! plainResp.body}"

  -- High-level: execute follows the 302 -> /final redirect automatically.
  let redirectReq : Request :=
    { method := .standard .GET, host := "127.0.0.1", port := addr.port, path := "/redirect" }
  let redirectResp ← execute redirectReq
  IO.println s!"  GET /redirect -> {redirectResp.statusCode.statusCode} {String.fromUTF8! redirectResp.body}"

  let _ ← close server
  let seenPaths ←
    match serverTask.get with
    | .ok paths => pure paths
    | .error e => throw e
  IO.println s!"  server saw, in order: {seenPaths}"

  pure (plainResp.statusCode.statusCode == 200 && plainResp.body == "hello, http client!".toUTF8 &&
        redirectResp.statusCode.statusCode == 200 && redirectResp.body == "redirected!".toUTF8 &&
        seenPaths == ["/hello", "/redirect", "/final"])

def run (_args : List String) : IO Unit := do
  if ← demoRoundTrip then
    IO.println "\nhttpclient demo done · all checks passed"
  else
    throw (IO.userError "httpclient demo done · some checks failed")

end Examples.HTTPClient
