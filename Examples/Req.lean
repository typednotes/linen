/-
  Examples.Req — `Network.HTTP.Req`'s type-safe client end-to-end.

  Stands up the same kind of tiny hand-rolled HTTP/1.1 server as
  `Examples.HTTPClient` (this time reading full request bodies too, since a
  `POST` is exercised) and drives it through `req`/`runReq`:

  * `req GET.mk (http "127.0.0.1" /: "items") NoReqBody.mk bsResponse` —
    a body-less `GET`;
  * `req POST.mk (http "127.0.0.1" /: "items") (ReqBodyBs.mk ...) bsResponse`
    — a `POST` carrying a `ReqBodyBs` payload, echoed back by the server.

  Both calls type-check only because `HttpBodyAllowed` accepts
  `(NoBody, NoBody)` and `(YesBody, YesBody)`; swapping in a body for the
  `GET` (`req GET.mk url (ReqBodyBs.mk ..) ..`) would fail to compile with
  "failed to synthesize HttpBodyAllowed .NoBody .YesBody" — there is no
  such instance (see `Req.lean`'s `HttpBodyAllowed` section).

  Args: (none) -- runs the round trip and exits non-zero on any mismatch
-/
import Linen.Network.HTTP.Req
import Linen.Network.Socket.Blocking

open Network.Socket
open Network.Socket.Blocking
open Network.HTTP.Req

namespace Examples.Req

/-- Read a full HTTP/1.1 request (request-line + headers + body, sized by a
`Content-Length` header if present) off `conn`, returning the request-line's
method/path and the body bytes. -/
def readRequest (conn : Socket .connected) : IO (String × String × ByteArray) := do
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
  let full := String.fromUTF8! buf
  let headerPart := (full.splitOn "\r\n\r\n").getD 0 ""
  let alreadyBuffered := buf.extract (headerPart.toUTF8.size + 4) buf.size
  let requestLine := (headerPart.splitOn "\r\n").getD 0 ""
  let method := (requestLine.splitOn " ").getD 0 "GET"
  let path := (requestLine.splitOn " ").getD 1 "/"
  let contentLength :=
    ((headerPart.splitOn "\r\n").findSome? fun line =>
      if line.toLower.startsWith "content-length:" then
        match line.splitOn ":" with
        | _ :: rest => (String.trimAscii (":".intercalate rest)).toString.toNat?
        | [] => none
      else none).getD 0
  let mut body := alreadyBuffered
  let mut eof := false
  while body.size < contentLength && !eof do
    let chunk ← Blocking.recv conn
    if chunk.isEmpty then eof := true
    else body := body ++ chunk
  return (method, path, body.extract 0 (min contentLength body.size))

/-- `GET` gets a fixed body; `POST` echoes back whatever body it received. -/
def respondFor (method : String) (body : ByteArray) : String :=
  if method == "POST" then
    let respBody := "received: " ++ String.fromUTF8! body
    s!"HTTP/1.1 201 Created\r\nContent-Type: text/plain\r\nContent-Length: {respBody.utf8ByteSize}\r\nConnection: close\r\n\r\n{respBody}"
  else
    let respBody := "get demo"
    s!"HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {respBody.utf8ByteSize}\r\nConnection: close\r\n\r\n{respBody}"

/-- Serve exactly `n` sequential connections, returning each one's
`(method, path)` in order. -/
def serveRequests (server : Socket .listening) (n : Nat) : IO (List (String × String)) := do
  let mut seen : List (String × String) := []
  for _ in [0:n] do
    let (conn, _peer) ← Blocking.accept server
    let (method, path, body) ← readRequest conn
    Blocking.sendAll conn (respondFor method body).toUTF8
    let _ ← close conn
    seen := (method, path) :: seen
  pure seen.reverse

/-- Both requests, run in the `Req` monad against the port the server picked. -/
def demoRequests (serverPort : UInt16) : Req (BsResponse × BsResponse) := do
  let getResp ← req GET.mk (http "127.0.0.1" /: "items") NoReqBody.mk bsResponse
    (port serverPort)
  let postResp ← req POST.mk (http "127.0.0.1" /: "items") (ReqBodyBs.mk "payload".toUTF8) bsResponse
    (port serverPort)
  pure (getResp, postResp)

def demoRoundTrip : IO Bool := do
  IO.println "── Network.HTTP.Req: type-safe req/runReq over a loopback server ──"
  let server ← listenTCP "127.0.0.1" 0
  let addr ← getSockName server
  IO.println s!"  server listening on 127.0.0.1:{addr.port}"
  let serverTask ← IO.asTask (prio := .dedicated) (serveRequests server 2)

  let (getResp, postResp) ← runReq defaultHttpConfig (demoRequests addr.port)
  IO.println s!"  GET /items -> {getResp.responseStatus.statusCode} {String.fromUTF8! getResp.responseBody}"
  IO.println s!"  POST /items -> {postResp.responseStatus.statusCode} {String.fromUTF8! postResp.responseBody}"

  let _ ← close server
  let seenRequests ←
    match serverTask.get with
    | .ok reqs => pure reqs
    | .error e => throw e
  IO.println s!"  server saw, in order: {seenRequests}"

  pure (getResp.responseStatus.statusCode == 200 && getResp.responseBody == "get demo".toUTF8 &&
        postResp.responseStatus.statusCode == 201 &&
        postResp.responseBody == "received: payload".toUTF8 &&
        seenRequests == [("GET", "/items"), ("POST", "/items")])

def run (_args : List String) : IO Unit := do
  if ← demoRoundTrip then
    IO.println "\nreq demo done · all checks passed"
  else
    throw (IO.userError "req demo done · some checks failed")

end Examples.Req
