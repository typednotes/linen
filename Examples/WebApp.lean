/-
  Examples.WebApp — `Network.WebApp`'s `Application`/`Middleware`/`AppM`
  machinery end-to-end.

  Stands up the same kind of hand-rolled loopback HTTP/1.1 server as
  `Examples.HTTPClient`/`Examples.Req`, but this time the server's request
  handler *is* a `Network.WebApp.Application`: raw bytes off the socket are
  parsed into a `Request`, dispatched through the application via
  `Green.block`, and the resulting `Response` is serialized back.

  The demo application (`demoApplication`) composes:

  * `echoApp` — the base `Application`: reads the body via
    `strictRequestBody` and echoes it back with the original `Content-Type`;
  * `ifRequest (·.pathInfo == ["health"]) healthCheckMiddleware` — routes
    `/health` to a fixed "healthy" response, bypassing `echoApp` entirely;
  * `modifyResponse (addHeader hServer ..)` — tags every response
    (whichever branch produced it) with a `Server` header,

  all via `composeMiddleware`. That every branch through the composed
  application both compiles and responds exactly once is `AppM`'s
  compile-time guarantee, not something this demo needs to test separately.

  Args: (none) -- runs a few round trips and exits non-zero on any mismatch
-/
import Linen.Network.WebApp
import Linen.Network.Socket.Blocking
import Linen.Network.Sendfile

open Network.Socket
open Network.Socket.Blocking
open Network.WebApp
open Network.HTTP.Types
open Control.Concurrent.Green
open Data (CI)

namespace Examples.WebApp

-- ── The demo application ──

/-- Always responds "healthy", ignoring whatever application it wraps. -/
def healthCheckMiddleware : Middleware :=
  fun _app _req respond => AppM.respondIO respond (pure (responseLBS status200 [] "healthy"))

/-- Echoes the request body back with its original `Content-Type` (or
    `text/plain` if none was given). -/
def echoApp : Application :=
  fun req respond =>
    AppM.respondIO respond do
      let body ← strictRequestBody req
      let ct := (requestHeader hContentType req).getD "text/plain"
      pure (responseLBS status200 [(hContentType, ct)] (String.fromUTF8! body))

/-- `echoApp`, with `/health` routed to a fixed response and a `Server`
    header stamped on every response — built entirely from `Middleware`
    combinators (`composeMiddleware`, `ifRequest`, `modifyResponse`,
    `addHeader`). -/
def demoApplication : Application :=
  composeMiddleware
    (modifyResponse (addHeader hServer "linen-webapp-demo"))
    (ifRequest (fun req => req.pathInfo == ["health"]) healthCheckMiddleware)
    echoApp

-- ── A hand-rolled HTTP/1.1 server driving `demoApplication` ──

/-- Read a full HTTP/1.1 request off `conn` and parse it into a
    `Network.WebApp.Request`. Mirrors `Examples.Req.readRequest`'s
    header/body framing, but builds the richer `Request` record instead of
    just `(method, path, body)`. -/
def readRequest (conn : Socket .connected) (remote : SockAddr) : IO Request := do
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
  let target := (requestLine.splitOn " ").getD 1 "/"
  let (path, query) :=
    match target.splitOn "?" with
    | p :: q :: _ => (p, "?" ++ q)
    | _ => (target, "")
  let headerLines := (headerPart.splitOn "\r\n").drop 1
  let headers : RequestHeaders :=
    headerLines.filterMap fun line =>
      match line.splitOn ":" with
      | name :: rest@(_ :: _) =>
        some (CI.mk' name, (String.trimAscii (":".intercalate rest)).toString)
      | _ => none
  let lookupHeader (n : HeaderName) : Option String :=
    headers.find? (fun h => h.1 == n) |>.map (·.2)
  let contentLength := (lookupHeader hContentLength).bind (·.toNat?) |>.getD 0
  let mut body := alreadyBuffered
  let mut eof := false
  while body.size < contentLength && !eof do
    let chunk ← Blocking.recv conn
    if chunk.isEmpty then eof := true
    else body := body ++ chunk
  let bodyBytes := body.extract 0 (min contentLength body.size)
  let bodyServed ← IO.mkRef false
  let bodyChunk : IO ByteArray := do
    if ← bodyServed.get then
      pure ByteArray.empty
    else
      bodyServed.set true
      pure bodyBytes
  pure { defaultRequest with
    requestMethod := parseMethod method
    httpVersion := http11
    rawPathInfo := path
    rawQueryString := query
    requestHeaders := headers
    remoteHost := remote
    pathInfo := (path.splitOn "/").filter (· != "")
    queryString := parseQuery query
    requestBody := bodyChunk
    requestBodyLength := .knownLength bodyBytes.size
    requestHeaderHost := lookupHeader hHost
    requestHeaderReferer := lookupHeader hReferer
    requestHeaderUserAgent := lookupHeader hUserAgent }

/-- Write a status line + headers + `Content-Length` framing to `conn`. -/
def writeHeaders (conn : Socket .connected) (status : Status)
    (headers : ResponseHeaders) (bodyLength : Nat) : IO Unit := do
  let headerLines := headers.foldl (init := "") fun acc (n, v) => acc ++ s!"{n.original}: {v}\r\n"
  let statusLine := s!"HTTP/1.1 {status.statusCode} {status.statusMessage}\r\n"
  let framing := s!"Content-Length: {bodyLength}\r\nConnection: close\r\n"
  Blocking.sendAll conn (statusLine ++ headerLines ++ framing ++ "\r\n").toUTF8

/-- Serialize a `Response` and write it to `conn`.

    Handles `.responseBuilder` (a body already built in memory) by sending
    it directly, and `.responseFile` (as produced by
    `Network.WebApp.Static`) via `Sendfile.sendFile` — the same zero-copy
    path a real server would use. `.responseStream`/`.responseRaw` are out
    of scope for this hand-rolled demo server, since neither
    `demoApplication` nor `Network.WebApp.Static.static` produce them. -/
def writeResponse (conn : Socket .connected) (resp : Response) : IO Unit := do
  match resp with
  | .responseBuilder status headers body =>
    writeHeaders conn status headers body.size
    Blocking.sendAll conn body
  | .responseFile status headers path part =>
    let size ← match part with
      | some p => pure p.count
      | none => (·.byteSize.toNat) <$> System.FilePath.metadata path
    writeHeaders conn status headers size
    Network.Sendfile.sendFile conn path part
  | _ => throw (IO.userError "writeResponse: demo server only supports responseBuilder/responseFile")

/-- Run `app` against one accepted connection, driving it via `Green.block`
    the same way `Tests.Network.WebApp.Static.Application.runStatic` does. -/
def serveOne (app : Application) (conn : Socket .connected) (remote : SockAddr) : IO Unit := do
  let req ← readRequest conn remote
  let tok ← Std.CancellationToken.new
  let captured ← IO.mkRef (none : Option Response)
  let respond : Response → Green ResponseReceived := fun resp =>
    (do captured.set (some resp); pure ResponseReceived.done : IO ResponseReceived)
  let _ ← Green.block ((app req respond).run) tok
  match ← captured.get with
  | some resp => writeResponse conn resp
  | none => throw (IO.userError "demoApplication never called respond")

/-- Serve exactly `n` sequential connections. -/
def serveRequests (app : Application) (server : Socket .listening) (n : Nat) : IO Unit := do
  for _ in [0:n] do
    let (conn, peer) ← Blocking.accept server
    serveOne app conn peer
    let _ ← close conn
    pure ()

/-- Send one raw HTTP/1.1 request over a fresh connection to `port` and
    return `(status, body)`. -/
def sendRequest (port : UInt16) (method : String) (path : String)
    (headers : String) (body : String) : IO (Nat × String) := do
  let client ← socket .inet .stream
  let conn ← Blocking.connect client { host := "127.0.0.1", port := port }
  let req := s!"{method} {path} HTTP/1.1\r\nHost: 127.0.0.1\r\n{headers}Content-Length: {body.utf8ByteSize}\r\n\r\n{body}"
  Blocking.sendAll conn req.toUTF8
  let mut buf := ByteArray.empty
  let mut done := false
  while !done do
    let chunk ← Blocking.recv conn
    if chunk.isEmpty then done := true
    else buf := buf ++ chunk
  let _ ← close conn
  let full := String.fromUTF8! buf
  let headerPart := (full.splitOn "\r\n\r\n").getD 0 ""
  let respBody := (full.splitOn "\r\n\r\n").getD 1 ""
  let statusCode := ((headerPart.splitOn "\r\n").getD 0 "").splitOn " " |>.getD 1 "0"
  pure (statusCode.toNat?.getD 0, respBody)

def demoRoundTrip : IO Bool := do
  IO.println "── Network.WebApp: Application/Middleware/AppM over a loopback server ──"
  let server ← listenTCP "127.0.0.1" 0
  let addr ← getSockName server
  IO.println s!"  server listening on 127.0.0.1:{addr.port}"
  let serverTask ← IO.asTask (prio := .dedicated) (serveRequests demoApplication server 2)

  let (echoStatus, echoBody) ← sendRequest addr.port "POST" "/anything"
    "Content-Type: text/plain\r\n" "hello webapp"
  IO.println s!"  POST /anything -> {echoStatus} {echoBody}"

  let (healthStatus, healthBody) ← sendRequest addr.port "GET" "/health" "" ""
  IO.println s!"  GET /health -> {healthStatus} {healthBody}"

  match serverTask.get with
  | .ok _ => pure ()
  | .error e => throw e
  let _ ← close server

  pure (echoStatus == 200 && echoBody == "hello webapp" &&
        healthStatus == 200 && healthBody == "healthy")

def run (_args : List String) : IO Unit := do
  if ← demoRoundTrip then
    IO.println "\nwebapp demo done · all checks passed"
  else
    throw (IO.userError "webapp demo done · some checks failed")

end Examples.WebApp
