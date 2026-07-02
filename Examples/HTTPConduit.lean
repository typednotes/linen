/-
  Examples.HTTPConduit — `Network.HTTP.Client.Conduit` and `Network.HTTP.Simple`
  end-to-end.

  Reuses `Examples.HTTPClient`'s tiny loopback HTTP/1.1 server and drives it
  with the streaming/simple layers instead of the raw connect/request API:

  * `Simple.parseUrl!`/`Simple.httpBS` — parse a `http://host:port/path`
    string straight into a `Request` and fetch it in one call;
  * `Client.Conduit.withResponse` — connect, send, hand the parsed
    `Response` to a callback, and close automatically;
  * `Client.Conduit.httpSource` — stream the response body as `ByteArray`
    chunks through a conduit pipeline (`.| sinkList`), capturing the parsed
    `Response` alongside the collected body via an `IO.Ref`.

  `httpSource` is `unsafe` (it builds on `ConduitT`, `unsafe` throughout this
  library — see `Data.Conduit`'s docstring), so this module and its `run` are
  `unsafe` too, same as `Examples.Conduit`.

  Args: (none) -- runs the round trip and exits non-zero on any mismatch
-/
import Linen.Network.HTTP.Client.Conduit
import Linen.Network.HTTP.Simple
import Linen.Network.Socket.Blocking
import Linen.Data.Conduit.Combinators

open Network.Socket
open Network.Socket.Blocking
open Network.HTTP.Client
open Network.HTTP.Client.Conduit
open Network.HTTP.Simple
open Network.HTTP.Types
open Data.Conduit
open Data.Conduit.Combinators

namespace Examples.HTTPConduit

/-- Read from `conn` until the full request-line + headers block has been
buffered. Identical in spirit to `Examples.HTTPClient.readRequestHeaders`,
duplicated here so this module stands on its own. -/
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

/-- Every request gets the same fixed 200 response, tagged with the request
count so each of this demo's three requests gets a distinguishable body. -/
def respondFor (n : Nat) : String :=
  let body := s!"chunk #{n}"
  s!"HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {body.utf8ByteSize}\r\nConnection: close\r\n\r\n{body}"

/-- Serve exactly `n` sequential connections, replying with `respondFor` in
request order (1-indexed). -/
def serveRequests (server : Socket .listening) (n : Nat) : IO Unit := do
  for i in [0:n] do
    let (conn, _peer) ← Blocking.accept server
    let _ ← readRequestHeaders conn
    Blocking.sendAll conn (respondFor (i + 1)).toUTF8
    let _ ← close conn

/-- Stream `req`'s response body through a conduit pipeline, capturing the
parsed `Response` (body field emptied, per `httpSource`'s contract) via an
`IO.Ref` alongside the chunks collected downstream by `sinkList`. -/
unsafe def streamedRequest (req : Request) : IO (Response × ByteArray) := do
  let respRef ← IO.mkRef (none : Option Response)
  let chunks ←
    runConduit
      ((do let r ← httpSource req
           liftConduit (respRef.set (some r)))
        .| sinkList
        : ConduitT PEmpty PEmpty IO (List ByteArray))
  let some resp ← respRef.get | throw (IO.userError "httpSource: no response captured")
  pure (resp, chunks.foldl (· ++ ·) ByteArray.empty)

unsafe def demoRoundTrip : IO Bool := do
  IO.println "── Network.HTTP.Client.Conduit / Network.HTTP.Simple ──"
  let server ← listenTCP "127.0.0.1" 0
  let addr ← getSockName server
  IO.println s!"  server listening on 127.0.0.1:{addr.port}"
  let serverTask ← IO.asTask (prio := .dedicated) (serveRequests server 3)

  -- Simple: parseUrl! + httpBS in one shot.
  let simpleReq ← parseUrl! s!"http://127.0.0.1:{addr.port}/one"
  let simpleResp ← httpBS simpleReq
  IO.println s!"  Simple.httpBS -> {simpleResp.statusCode.statusCode} {String.fromUTF8! simpleResp.body}"

  -- Conduit.withResponse: callback-scoped connection.
  let withResponseReq : Request :=
    { method := .standard .GET, host := "127.0.0.1", port := addr.port, path := "/two" }
  let withResponseBody ← withResponse withResponseReq (fun resp => pure resp.body)
  IO.println s!"  Conduit.withResponse -> {String.fromUTF8! withResponseBody}"

  -- Conduit.httpSource: response body streamed through `.| sinkList`.
  let streamReq : Request :=
    { method := .standard .GET, host := "127.0.0.1", port := addr.port, path := "/three" }
  let (streamResp, streamBody) ← streamedRequest streamReq
  IO.println s!"  Conduit.httpSource -> {streamResp.statusCode.statusCode} {String.fromUTF8! streamBody}"

  let _ ← close server
  match serverTask.get with
  | .ok _ => pure ()
  | .error e => throw e

  pure (simpleResp.statusCode.statusCode == 200 && simpleResp.body == "chunk #1".toUTF8 &&
        withResponseBody == "chunk #2".toUTF8 &&
        streamResp.statusCode.statusCode == 200 && streamBody == "chunk #3".toUTF8)

unsafe def run (_args : List String) : IO Unit := do
  if ← demoRoundTrip then
    IO.println "\nhttpconduit demo done · all checks passed"
  else
    throw (IO.userError "httpconduit demo done · some checks failed")

end Examples.HTTPConduit
