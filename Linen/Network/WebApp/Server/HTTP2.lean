/-
  Linen.Network.WebApp.Server.HTTP2 — serving a `Network.WebApp.Application`
  over HTTP/2

  Bridges `Network.HTTP2.serve` (the protocol engine) and the WebApp
  interface, as Warp's HTTP/2 layer does for WAI: each HTTP/2 request becomes
  a `Network.WebApp.Request` (with `httpVersion = http20`, the body read from
  the stream as the application asks for it), and each `Response` is written
  to the stream — `responseBuilder` and `responseFile` with their
  `content-length`, `responseStream` as DATA frames as the application
  produces them.

  `responseRaw` takes a connection over, which a multiplexed HTTP/2 stream
  cannot give; its fallback response is sent instead, as Warp does.
-/
import Linen.Network.HTTP2.Server
import Linen.Network.WebApp
import Linen.Network.HTTP.Types.Header
import Linen.Network.HTTP.Types.Method
import Linen.Network.HTTP.Types.URI
import Linen.Network.HTTP.Types.Version
import Linen.Network.Sendfile
import Linen.Network.WebApp.Server.Settings
import Linen.Network.WebApp.Server.Response
import Linen.Network.WebApp.Server.Run

namespace Network.WebApp.Server

open Network.HTTP.Types
open Network.WebApp
open Control.Concurrent.Green (Green)

/-- The `Network.WebApp.Request` for an HTTP/2 request. `:authority` stands
    in for `Host` when there is no `host` field (§8.3.1). -/
def http2Request (req : Network.HTTP2.Request) (remoteAddr : Network.Socket.SockAddr)
    (isSecure : Bool) : Network.WebApp.Request :=
  let (rawPath, rawQuery) := match req.path.splitOn "?" with
    | [p] => (p, "")
    | p :: rest => (p, "?" ++ "?".intercalate rest)
    | [] => ("", "")
  let fields := match req.authority with
    | some a => if req.headers.any (·.1 == "host") then req.headers else ("host", a) :: req.headers
    | none => req.headers
  let headers : RequestHeaders := fields.map fun (n, v) => (Data.CI.mk' n, v)
  let find (name : String) := fields.lookup name
  { requestMethod := parseMethod req.method
    httpVersion := http20
    rawPathInfo := rawPath
    rawQueryString := rawQuery
    requestHeaders := headers
    isSecure
    remoteHost := remoteAddr
    pathInfo := (rawPath.splitOn "/").filter (!·.isEmpty)
    queryString := parseQuery rawQuery
    requestBody := req.body
    vault := Data.Vault.empty
    requestBodyLength := match req.contentLength with
      | some n => .knownLength n
      | none => .chunkedBody
    requestHeaderHost := find "host"
    requestHeaderRange := find "range"
    requestHeaderReferer := find "referer"
    requestHeaderUserAgent := find "user-agent" }

/-- The response fields to send: the application's, plus `Server` when the
    settings ask for it, plus `extra` fields the application did not set. -/
private def withAutoFields (settings : Settings) (extra : List (String × String))
    (headers : ResponseHeaders) : List (String × String) :=
  let own := headers.map fun (n, v) => (n.original, v)
  let has (name : String) := own.any (·.1.toLower == name)
  let own := if settings.settingsAddServerHeader && !has "server" then
      own ++ [("server", settings.settingsServerName)] else own
  own ++ extra.filter (fun (n, _) => !has n)

/-- Write `resp` to an HTTP/2 stream. -/
def sendHttp2Response (responder : Network.HTTP2.Responder) (settings : Settings) :
    Response → Green ResponseReceived
  | .responseBuilder status headers body => do
    (responder.respond status.statusCode
      (withAutoFields settings [("content-length", toString body.size)] headers) body.isEmpty : IO _)
    unless body.isEmpty do
      (responder.write body : IO _)
      (responder.finish : IO _)
    pure .done
  | .responseFile status headers path part => do
    let length ← (fileBodyLength path part : IO _)
    (responder.respond status.statusCode
      (withAutoFields settings [("content-length", toString length)] headers) (length == 0) : IO _)
    if length > 0 then
      (Network.Sendfile.sendFileWith responder.write path part : IO _)
      (responder.finish : IO _)
    pure .done
  | .responseStream status headers body => do
    (responder.respond status.statusCode (withAutoFields settings [] headers) false : IO _)
    (body responder.write (pure ()) : IO _)
    (responder.finish : IO _)
    pure .done
  | .responseRaw _ fallback => sendHttp2Response responder settings fallback

/-- The `Network.HTTP2.Handler` that runs `app` for each stream. -/
def http2Handler (settings : Settings) (app : Application) (remoteAddr : Network.Socket.SockAddr)
    (isSecure : Bool) : Network.HTTP2.Handler := fun req responder => do
  let _ ← (app (http2Request req remoteAddr isSecure) fun resp =>
    sendHttp2Response responder settings resp).run

/-- Serve HTTP/2 on a transport whose next bytes are the client preface
    (after ALPN chose `h2`): `Network.HTTP2.serve`, with the transport's
    reads and writes, `settingsTimeout` for body and window waits, and each
    stream's request answered by `app`.
    $$\text{serveHttp2} : \text{HttpTransport} \to \text{SockAddr} \to \text{Settings} \to \text{Application} \to \text{Green Unit}$$ -/
def serveHttp2 (t : HttpTransport) (remoteAddr : Network.Socket.SockAddr) (settings : Settings)
    (app : Application) : Green Unit := do
  let buffered ← (t.reader.unread : IO _)
  let pending ← (IO.mkRef buffered : IO _)
  let transport : Network.HTTP2.Transport := {
    -- Bytes the HTTP/1.1 reader buffered come first (none after ALPN).
    recv := do
      let early ← (pending.modifyGet fun b => (b, ByteArray.empty) : IO _)
      if early.isEmpty then t.nextChunk else return some early
    send := t.sink.sendIO }
  Network.HTTP2.serve transport (http2Handler settings app remoteAddr t.isSecure)
    { timeoutMillis := settings.timeoutMillis }

end Network.WebApp.Server
