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
import Linen.Network.WebApp.Server.Transport
import Linen.Data.Base64

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
    (after ALPN, prior-knowledge detection, or h2c Upgrade): `Network.HTTP2.serve`, with the transport's
    reads and writes, `settingsTimeout` for body and window waits, and each
    stream's request answered by `app`. An upgrade seeds completed stream 1
    and applies HTTP2-Settings before any response DATA.
    $$\text{serveHttp2} : \text{HttpTransport} \to \text{SockAddr} \to \text{Settings} \to \text{Application} \to \text{Green Unit}$$ -/
def serveHttp2 (t : HttpTransport) (remoteAddr : Network.Socket.SockAddr) (settings : Settings)
    (app : Application) (upgrade : Option Network.HTTP2.UpgradeRequest := none) : Green Unit := do
  let buffered ← (t.reader.unread : IO _)
  let pending ← (IO.mkRef buffered : IO _)
  let transport : Network.HTTP2.Transport := {
    -- Bytes the HTTP/1.1 reader buffered come first (none after ALPN).
    recv := do
      let early ← (pending.modifyGet fun b => (b, ByteArray.empty) : IO _)
      if early.isEmpty then t.nextChunk else return some early
    send := t.sink.sendIO }
  Network.HTTP2.serve transport (http2Handler settings app remoteAddr t.isSecure)
    { timeoutMillis := settings.timeoutMillis } upgrade

-- ── Legacy HTTP/1.1 Upgrade (RFC 7540 §3.2) ─────────────────────────

/-- Every comma-separated token of a header, across repeated field lines,
    trimmed and lowercased. Connection options are case-insensitive. -/
def headerTokens (name : String) (headers : RequestHeaders) : List String :=
  headers.filter (fun (n, _) => n.original.toLower == name) |>.flatMap fun (_, value) =>
    (value.splitOn ",").map (fun s => s.trimAscii.toString.toLower)

/-- Decode an unpadded base64url HTTP2-Settings payload. A SETTINGS payload
    is a multiple of six bytes, so its encoding has no partial quartet.
    Padding, the standard base64 alphabet, and whitespace are rejected. -/
def decodeHttp2Settings (value : String) : Option (List (Network.HTTP2.SettingsKeyId × UInt32)) := do
  if !value.all (fun c => c.isAlphanum && c.toNat < 128 || c == '-' || c == '_') then none
  else
    let bytes ← Data.Base64.decode (value.replace "-" "+" |>.replace "_" "/")
    let params ← Network.HTTP2.decodeSettingsPayload bytes
    match Network.HTTP2.validatePeerSettings params with
    | .ok () => some params
    | .error _ => none

/-- A valid h2c upgrade's peer settings, `none` for an ordinary request, or
    an error for a malformed h2c attempt. Exactly one HTTP2-Settings field
    and both Connection options are mandatory. `h2` is never an upgrade
    token, and HTTP/1.0 cannot switch to h2c. -/
def h2cSettings (req : Network.WebApp.Request) :
    Except String (Option (List (Network.HTTP2.SettingsKeyId × UInt32))) := do
  if req.httpVersion != http11 || !(headerTokens "upgrade" req.requestHeaders).contains "h2c" then
    return none
  let options := headerTokens "connection" req.requestHeaders
  unless options.contains "upgrade" && options.contains "http2-settings" && !options.contains "close" do
    throw "h2c requires Connection: Upgrade, HTTP2-Settings"
  match req.requestHeaders.filter (fun (n, _) => n.original.toLower == "http2-settings") with
  | [(_, value)] =>
    let some params := decodeHttp2Settings value | throw "invalid HTTP2-Settings"
    return some params
  | _ => throw "h2c requires exactly one HTTP2-Settings field"

/-- Convert the upgrade request to stream 1, stripping hop-by-hop fields
    (including those named by Connection). Its body is already fully read,
    as RFC 7540 requires before switching protocols. -/
def h2cRequest (req : Network.WebApp.Request) (body : IO ByteArray) (length : Nat) :
    Network.HTTP2.Request :=
  let options := headerTokens "connection" req.requestHeaders
  let fields := req.requestHeaders.filterMap fun (n, v) =>
    let name := n.original.toLower
    if Network.HTTP2.connectionSpecificFields.contains name || options.contains name ||
        name == "http2-settings" || name == "content-length" || (name == "te" && v != "trailers") then none
    else some (name, v)
  let connect := toString req.requestMethod == "CONNECT"
  {
    streamId := 1, method := toString req.requestMethod,
    scheme := if connect then none else some "http",
    authority := if connect then some req.rawPathInfo else req.requestHeaderHost,
    path := if connect then "" else req.rawPathInfo ++ req.rawQueryString,
    headers := fields ++ [("content-length", toString length)], contentLength := some length, body }

/-- Accept an h2c upgrade. The HTTP/1.1 body is spooled to a temporary file
    before the 101 response, using bounded memory even for large uploads.
    The file is opened for reading and unlinked before the engine starts;
    its handle remains alive for stream 1's reader, including during teardown.
    Expect: 100-continue is honoured before waiting for the upload. -/
def serveHttp2Upgrade (t : HttpTransport) (remoteAddr : Network.Socket.SockAddr)
    (settings : Settings) (app : Application) (req : Network.WebApp.Request)
    (params : List (Network.HTTP2.SettingsKeyId × UInt32)) : Green Unit := do
  if (headerTokens "expect" req.requestHeaders).contains "100-continue" then
    t.sink.send "HTTP/1.1 100 Continue\r\n\r\n".toUTF8
  let (file, path) ← (IO.FS.createTempFile : IO _)
  try
    let mut length := 0
    repeat
      let bytes ← (req.requestBody : IO _)
      if bytes.isEmpty then break
      (file.write bytes : IO _)
      length := length + bytes.size
    (file.flush : IO _)
    let input ← (IO.FS.Handle.mk path .read : IO _)
    (IO.FS.removeFile path : IO _)
    t.sink.send "HTTP/1.1 101 Switching Protocols\r\nConnection: Upgrade\r\nUpgrade: h2c\r\n\r\n".toUTF8
    serveHttp2 t remoteAddr settings app (some {
      request := h2cRequest req (input.read 16384) length,
      settings := params })
  finally
    try (IO.FS.removeFile path : IO _) catch _ => pure ()

end Network.WebApp.Server
