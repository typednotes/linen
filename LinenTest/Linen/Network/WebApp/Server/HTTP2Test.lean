/-
  Tests for `Linen.Network.WebApp.Server.HTTP2`.

  The mapping from an HTTP/2 request to a `Network.WebApp.Request`, and every
  kind of `Response` written to a recording `Network.HTTP2.Responder`. The
  bridge end to end — over TLS, with ALPN, against our own client and curl —
  is in `Network.WebApp.Server.TLSTest`.
-/
import Linen.Network.WebApp.Server.HTTP2
import Linen.Network.WebApp.Server.WithApplication
import Linen.Network.Socket.Blocking

open Network.WebApp.Server
open Network.WebApp
open Network.HTTP.Types
open Control.Concurrent.Green (Green)

namespace Tests.Network.WebApp.Server.HTTP2

private def h2req (path : String) (headers : List (String × String) := [])
    (authority : Option String := some "example.com") : Network.HTTP2.Request :=
  { streamId := 1, method := "POST", scheme := some "https", authority, path, headers,
    contentLength := some 3, body := pure "abc".toUTF8 }

private def addr : Network.Socket.SockAddr := { host := "127.0.0.1", port := 1 }

/-! ### `http2Request` -/

#guard (http2Request (h2req "/a/b?x=1&y") addr true).rawPathInfo == "/a/b"
#guard (http2Request (h2req "/a/b?x=1&y") addr true).rawQueryString == "?x=1&y"
#guard (http2Request (h2req "/a/b?x=1&y") addr true).pathInfo == ["a", "b"]
#guard (http2Request (h2req "/") addr true).httpVersion == http20
#guard (http2Request (h2req "/") addr true).isSecure
#guard (http2Request (h2req "/") addr true).requestMethod == .standard .POST
#guard (http2Request (h2req "/") addr true).requestBodyLength == .knownLength 3
-- `:authority` stands in for Host…
#guard (http2Request (h2req "/") addr true).requestHeaderHost == some "example.com"
-- …unless a `host` field was sent too.
#guard (http2Request (h2req "/" [("host", "other")]) addr true).requestHeaderHost == some "other"
#guard (http2Request (h2req "/" [] none) addr true).requestHeaderHost == none
#guard (http2Request (h2req "/" [("user-agent", "t"), ("referer", "r")]) addr true).requestHeaderUserAgent
  == some "t"
#guard (http2Request { h2req "/" with contentLength := none } addr true).requestBodyLength == .chunkedBody

/-! ### `sendHttp2Response` over a recording responder -/

/-- What `sendHttp2Response` does with `resp`: the calls it makes, in order. -/
private def record (resp : Response) : IO (List String) := do
  let log ← IO.mkRef ([] : List String)
  let responder : Network.HTTP2.Responder := {
    respond := fun status headers endStream =>
      log.modify (· ++ [s!"respond {status} {headers} end={endStream}"])
    write := fun bytes => log.modify (· ++ [s!"write {String.fromUTF8! bytes}"])
    finish := log.modify (· ++ ["finish"]) }
  let settings : Settings := { defaultSettings with settingsAddServerHeader := false }
  let _ ← Green.block (sendHttp2Response responder settings resp) (← Std.CancellationToken.new)
  log.get

#eval do
  let got ← record (.responseBuilder status200 [(Data.CI.mk' "X-A", "1")] "hi".toUTF8)
  unless got == ["respond 200 [(X-A, 1), (content-length, 2)] end=false", "write hi", "finish"] do
    throw (IO.userError s!"builder: {got}")

-- An empty body ends the stream with the headers.
#eval do
  let got ← record (.responseBuilder status204 [] ByteArray.empty)
  unless got == ["respond 204 [(content-length, 0)] end=true"] do throw (IO.userError s!"empty: {got}")

-- A file: its content-length, then its bytes.
#eval do
  let (h, path) ← IO.FS.createTempFile
  h.putStr "0123456789"
  h.flush
  let got ← record (.responseFile status200 [] path.toString (some ⟨2, 3⟩))
  IO.FS.removeFile path
  unless got == ["respond 200 [(content-length, 3)] end=false", "write 234", "finish"] do
    throw (IO.userError s!"file: {got}")

-- A stream: each write as it is produced, then the end.
#eval do
  let got ← record (.responseStream status200 [] fun write _ => do write "a".toUTF8; write "b".toUTF8)
  unless got == ["respond 200 [] end=false", "write a", "write b", "finish"] do
    throw (IO.userError s!"stream: {got}")

-- `responseRaw` cannot take over a multiplexed stream: its fallback is sent.
#eval do
  let got ← record (.responseRaw (fun _ _ => pure ()) (.responseBuilder status500 [] "no".toUTF8))
  unless got == ["respond 500 [(content-length, 2)] end=false", "write no", "finish"] do
    throw (IO.userError s!"raw: {got}")

-- The Server field, when the settings ask for it.
#eval show IO Unit from do
  let log ← IO.mkRef ([] : List (String × String))
  let responder : Network.HTTP2.Responder := {
    respond := fun _ headers _ => log.set headers
    write := fun _ => pure ()
    finish := pure () }
  let token ← Std.CancellationToken.new
  let send := sendHttp2Response responder defaultSettings (.responseBuilder status200 [] ByteArray.empty)
  let _ ← Green.block send token
  unless (← log.get).lookup "server" == some defaultSettings.settingsServerName do
    throw (IO.userError "no server field")

end Tests.Network.WebApp.Server.HTTP2

namespace Tests.Network.WebApp.Server.H2c

open Network.HTTP2

-- ── Upgrade validation and request conversion ──────────────────────

private def upgradeHeaders : RequestHeaders :=
  [(Data.CI.mk' "Upgrade", "h2c"), (hConnection, "Upgrade, HTTP2-Settings"),
   (Data.CI.mk' "HTTP2-Settings", "")]

private def request (headers : RequestHeaders) : Network.WebApp.Request :=
  { http2Request {
      streamId := 1, method := "GET", scheme := some "http", authority := some "x",
      path := "/p?q=1", headers := [], contentLength := some 0, body := pure ByteArray.empty }
      { host := "127.0.0.1", port := 1 } false with httpVersion := http11, requestHeaders := headers }

#guard (h2cSettings (request upgradeHeaders)).toOption == some (some [])
#guard (h2cSettings (request [(Data.CI.mk' "Upgrade", "h2")])).toOption == some none
#guard (h2cSettings (request [])).toOption == some none
#guard (h2cSettings { request upgradeHeaders with httpVersion := http10 }).toOption == some none
#guard (h2cSettings (request (upgradeHeaders ++ [(Data.CI.mk' "HTTP2-Settings", "")]))).toOption.isNone
#guard (h2cSettings (request (upgradeHeaders.filter (·.1 != hConnection)))).toOption.isNone
#guard (h2cSettings (request (upgradeHeaders.filter (fun (n, _) => n != Data.CI.mk' "HTTP2-Settings")))).toOption.isNone
#guard (h2cSettings (request [(Data.CI.mk' "Upgrade", "websocket, h2c"),
    (hConnection, "keep-alive, UpGrAdE"), (hConnection, "HtTp2-SeTtInGs"),
    (Data.CI.mk' "HTTP2-Settings", "")])).toOption == some (some [])
#guard decodeHttp2Settings "AAMAAABkAAQAAP__" == some [(.maxConcurrentStreams, 100), (.initialWindowSize, 65535)]
#guard decodeHttp2Settings "AAQAAAAA" == some [(.initialWindowSize, 0)]
#guard decodeHttp2Settings "AAQAAAAA=" == none
#guard decodeHttp2Settings "AAMAAABkAAQAAP//" == none
#guard decodeHttp2Settings "AA" == none
#guard decodeHttp2Settings "AAAA" == none
#guard decodeHttp2Settings "AAIAAAAC" == none  -- ENABLE_PUSH = 2
#guard decodeHttp2Settings "AAT_____" == none  -- window > 2^31-1
#guard decodeHttp2Settings "AAUAAAAA" == none  -- MAX_FRAME_SIZE = 0
#guard decodeHttp2Settings "AAQ AAAA" == none

private def converted := h2cRequest (request (upgradeHeaders ++
  [(hConnection, "X-Hop"), (Data.CI.mk' "X-Hop", "secret"), (hTransferEncoding, "chunked"),
   (Data.CI.mk' "X-End", "value")])) (pure "abc".toUTF8) 3

#guard converted.streamId == 1
#guard converted.path == "/p?q=1"
#guard converted.scheme == some "http"
#guard converted.contentLength == some 3
#guard converted.headers == [("x-end", "value"), ("content-length", "3")]
#guard defaultSettings.settingsHttp2

-- ── Real TCP connections, both server modes ────────────────────────

-- A deadline during detection ends the connection instead of starting a
-- second full HTTP/1.1 head timeout. This counts reads, avoiding a brittle
-- wall-clock assertion on busy CI hosts. Covers idle and partial prefaces.
#eval show IO Unit from do
  for opening in ["", "P", "PRI * HTTP/2.0\r\n\r\n"] do
    let reader ← ByteSource.buffered (pure ByteArray.empty)
    reader.feed opening.toUTF8
    let waits ← IO.mkRef 0
    let transport : HttpTransport := {
      nextChunk := do (waits.modify (· + 1) : IO _); return none
      reader
      sink := {
        send := fun _ => throw (IO.userError "response after detection timed out")
        sendIO := fun _ => throw (IO.userError "response after detection timed out")
        sendFile := fun _ _ => pure (), rawRecv := pure ByteArray.empty, rawSend := fun _ => pure () } }
    let app : Application := fun _ respond => AppM.respondIO respond
      (throw (IO.userError "application called after timeout"))
    Green.block (serveHttp transport { host := "127.0.0.1", port := 0 } defaultSettings app)
      (← Std.CancellationToken.new)
    unless (← waits.get) == 1 do throw (IO.userError "detection restarted the timeout")

private inductive Mode where
  | blocking
  | eventLoop
deriving Repr

private def withServer (mode : Mode) (app : Application) (client : UInt16 → IO α)
    (settings : Network.WebApp.Server.Settings := defaultSettings) : IO α := do
  match mode with
  | .eventLoop => withApplicationSettings settings (pure app) client
  | .blocking =>
    let server ← Network.Socket.listenTCP "127.0.0.1" 0
    let port := (← Network.Socket.getSockName server).port
    let stop ← Std.CancellationToken.new
    let loop ← IO.asTask (prio := .dedicated) (acceptLoopUntil server settings app stop)
    try client port
    finally
      stop.cancel .cancel
      let wake ← Network.Socket.Blocking.connect (← Network.Socket.socket .inet .stream)
        { host := "127.0.0.1", port }
      let _ ← Network.Socket.close wake
      let _ ← IO.wait loop
      let _ ← Network.Socket.close server

private def bothModes (test : Mode → IO Unit) : IO Unit := do
  for mode in [Mode.blocking, .eventLoop] do
    try test mode catch e => throw (IO.userError s!"h2c {repr mode}: {e}")

private def echoApp : Application := fun req respond => AppM.respondIO respond do
  let mut length := 0
  repeat
    let bytes ← req.requestBody
    if bytes.isEmpty then break
    length := length + bytes.size
  let extra := if req.rawPathInfo == "/big" then String.ofList (List.replicate 1000000 'd') else ""
  pure (responseLBS status200 []
    (s!"{req.requestMethod} {req.rawPathInfo}{req.rawQueryString} {req.httpVersion} secure={req.isSecure} n={length};" ++ extra))

private structure Client where
  sock : Network.Socket.Socket .connected
  reader : BufferedSource

private def withClient (port : UInt16) (action : Client → IO α) : IO α := do
  let sock ← Network.Socket.Blocking.connect (← Network.Socket.socket .inet .stream)
    { host := "127.0.0.1", port }
  Network.Socket.setNonBlocking sock
  let reader ← ByteSource.buffered (Network.Socket.Blocking.recv sock 16384 3000)
  try action { sock, reader }
  finally let _ ← Network.Socket.close sock

private def Client.write (c : Client) (bytes : ByteArray) : IO Unit :=
  Network.Socket.Blocking.sendAll c.sock bytes 3000

private def Client.exact (c : Client) (n : Nat) : IO ByteArray := do
  let mut got := ByteArray.empty
  while got.size < n do
    let bytes ← c.reader.source.readN (n - got.size)
    if bytes.isEmpty then throw (IO.userError s!"EOF with {n - got.size} bytes missing")
    got := got ++ bytes
  return got

private def Client.frame (c : Client) : IO Frame := do
  let some h := decodeFrameHeader (← c.exact 9) | throw (IO.userError "bad frame header")
  return { header := h, payload := ← c.exact h.payloadLength.toNat }

private def Client.preface (c : Client) : IO Unit :=
  c.write (connectionPreface ++ encodeFrame (buildSettingsFrame []))

private def requestFrame (id : Nat) (path : String) : ByteArray :=
  encodeFrame (buildHeadersFrame (StreamId.fromWire id.toUInt32)
    (HPACK.encodeHeadersStatic [(":method", "GET"), (":scheme", "http"),
      (":authority", "x"), (":path", path)]) true true)

private def Client.responses (c : Client) (ids : List Nat) : IO (List (Nat × String)) := do
  let mut bodies : List (Nat × String) := ids.map (·, "")
  let mut done : List Nat := []
  while done.length < ids.length do
    let f ← c.frame
    let id := f.header.streamId.val.toNat
    if f.header.frameType == .goaway || f.header.frameType == .rstStream then
      throw (IO.userError s!"unexpected {repr f.header.frameType} on {id}")
    if ids.contains id && (f.header.frameType == .headers || f.header.frameType == .data) then
      if f.header.frameType == .data then
        bodies := bodies.map fun (sid, body) =>
          (sid, if sid == id then body ++ String.fromUTF8! f.payload else body)
      if FrameFlags.test f.header.flags FrameFlags.endStream then done := id :: done
  return bodies

private def upgradeHead (path : String) (extra : String := "") (settings : String := "") : String :=
  s!"POST {path} HTTP/1.1\r\nHost: x\r\nConnection: Upgrade, HTTP2-Settings\r\nUpgrade: h2c\r\nHTTP2-Settings: {settings}\r\n{extra}\r\n"

private def Client.switching (c : Client) : IO Unit := do
  let some (line, _) ← recvHeadersFrom c.reader.source | throw (IO.userError "no 101 head")
  unless line == "HTTP/1.1 101 Switching Protocols" do throw (IO.userError s!"upgrade: {line}")

-- A byte-at-a-time preface, including the blank line at byte 18; then two
-- streams sent in one packet. The detector must neither parse PRI as HTTP/1
-- nor drop frames already buffered behind the preface.
#eval bothModes fun mode => withServer mode echoApp fun port => withClient port fun c => do
  for b in connectionPreface.toList do
    c.write (ByteArray.empty.push b)
    IO.sleep 1
  c.write (encodeFrame (buildSettingsFrame []) ++ requestFrame 1 "/one" ++ requestFrame 3 "/two")
  let bodies ← c.responses [1, 3]
  unless bodies.lookup 1 == some "GET /one HTTP/2.0 secure=false n=0;" &&
      bodies.lookup 3 == some "GET /two HTTP/2.0 secure=false n=0;" do
    throw (IO.userError s!"prior knowledge: {bodies}")

-- Upgrade preserves stream 1's body and query, then serves stream 3 too.
-- Body plus preface plus frames in one write exercise the buffered handoff.
#eval bothModes fun mode => withServer mode echoApp fun port => withClient port fun c => do
  c.write ((upgradeHead "/up?q=1" "Content-Length: 3\r\n").toUTF8 ++ "abc".toUTF8 ++
    connectionPreface ++ encodeFrame (buildSettingsFrame []) ++ requestFrame 3 "/next")
  c.switching
  let first ← c.frame
  unless first.header.frameType == .settings && !FrameFlags.test first.header.flags FrameFlags.ack do
    throw (IO.userError "server preface must be SETTINGS")
  let bodies ← c.responses [1, 3]
  unless bodies.lookup 1 == some "POST /up?q=1 HTTP/2.0 secure=false n=3;" &&
      bodies.lookup 3 == some "GET /next HTTP/2.0 secure=false n=0;" do
    throw (IO.userError s!"upgrade: {bodies}")

-- Chunked upgrade bodies are decoded before switching; an Expect client
-- receives 100 before sending the body and 101 only after the last chunk.
#eval bothModes fun mode => withServer mode echoApp fun port => withClient port fun c => do
  c.write (upgradeHead "/chunk" "Transfer-Encoding: chunked\r\nExpect: 100-continue\r\n").toUTF8
  let some (line, _) ← recvHeadersFrom c.reader.source | throw (IO.userError "missing 100")
  unless line == "HTTP/1.1 100 Continue" do throw (IO.userError s!"expect: {line}")
  c.write "3\r\nabc\r\n".toUTF8
  unless (← Network.Socket.poll c.sock .read 30) matches .timeout do
    throw (IO.userError "101 sent before the entire HTTP/1.1 body arrived")
  c.write "2\r\nde\r\n0\r\nX-Trailer: ignored\r\n\r\n".toUTF8
  c.switching
  c.preface
  unless (← c.responses [1]).lookup 1 == some "POST /chunk HTTP/2.0 secure=false n=5;" do
    throw (IO.userError "chunked upgrade body lost")

-- Header SETTINGS take effect before stream 1 responds. Its zero send
-- window prevents DATA until WINDOW_UPDATE, while PING proves the reader
-- remains live. The header gets no explicit ACK: only the preface does.
#eval bothModes fun mode => withServer mode echoApp fun port => withClient port fun c => do
  c.write (upgradeHead "/window" "" "AAQAAAAA").toUTF8
  c.switching
  c.preface
  c.write (encodeFrame (buildPingFrame "12345678".toUTF8))
  let mut ackCount := 0
  repeat
    let f ← c.frame
    if f.header.frameType == .settings && FrameFlags.test f.header.flags FrameFlags.ack then
      ackCount := ackCount + 1
    if f.header.frameType == .data then throw (IO.userError "ignored HTTP2-Settings window")
    if f.header.frameType == .ping then break
  unless ackCount == 1 do throw (IO.userError s!"header settings got an ACK: {ackCount}")
  c.write (encodeFrame (buildWindowUpdateFrame (StreamId.fromWire 1) 1000))
  unless (← c.responses [1]).lookup 1 == some "POST /window HTTP/2.0 secure=false n=0;" do
    throw (IO.userError "window did not reopen")

-- Invalid settings never switch protocols or reach the application.
#eval bothModes fun mode => withServer mode echoApp fun port => withClient port fun c => do
  c.write (upgradeHead "/invalid" "" "AAIAAAAC").toUTF8
  let some (line, _) ← recvHeadersFrom c.reader.source | throw (IO.userError "no rejection")
  unless line == "HTTP/1.1 400 Bad Request" do throw (IO.userError s!"bad settings: {line}")

-- Disabling h2c leaves Upgrade requests on HTTP/1.1 and rejects a direct
-- HTTP/2 preface. ALPN's TLS setting is independent.
#eval bothModes fun mode => withServer mode echoApp (settings := { defaultSettings with settingsHttp2 := false })
    fun port => do
  withClient port fun c => do
    c.write (upgradeHead "/off").toUTF8
    let some (line, _) ← recvHeadersFrom c.reader.source | throw (IO.userError "no HTTP/1.1 fallback")
    unless line == "HTTP/1.1 200 OK" do throw (IO.userError s!"disabled: {line}")
  withClient port fun c => do
    c.preface
    unless (← c.reader.readSome).isEmpty do throw (IO.userError "disabled server answered PRI")

-- Interoperability: curl uses HTTP/1.1 Upgrade for --http2, and the direct
-- preface for --http2-prior-knowledge. Large transfers exercise both routes.
#eval bothModes fun mode => do
  let version ← IO.Process.output { cmd := "curl", args := #["--version"] }
  unless version.exitCode == 0 && (version.stdout.splitOn "HTTP2").length > 1 do
    IO.println "curl with HTTP/2 not found: skipping h2c curl interop"
    return
  withServer mode echoApp fun port => do
    for option in ["--http2", "--http2-prior-knowledge"] do
      let curl (args : Array String) : IO String := do
        let out ← IO.Process.output { cmd := "curl", args := #["-sS", option, "--max-time", "30"] ++ args }
        unless out.exitCode == 0 do throw (IO.userError s!"curl {option}: {out.stderr}")
        return out.stdout
      let url := s!"http://127.0.0.1:{port}"
      let got ← curl #[url ++ "/q?a=b"]
      unless got == "GET /q?a=b HTTP/2.0 secure=false n=0;" do
        throw (IO.userError s!"curl GET {option}: {got}")
      let (file, path) ← IO.FS.createTempFile
      try
        file.putStr (String.ofList (List.replicate 3000000 'u'))
        file.flush
        let got ← curl #["--data-binary", "@" ++ path.toString, url ++ "/up"]
        unless got == "POST /up HTTP/2.0 secure=false n=3000000;" do
          throw (IO.userError s!"curl upload {option}: {got}")
      finally IO.FS.removeFile path
      let got ← curl #["-w", "%{http_version} %{size_download}", "-o", "/dev/null", url ++ "/big"]
      unless got == s!"2 {1000000 + "GET /big HTTP/2.0 secure=false n=0;".length}" do
        throw (IO.userError s!"curl download {option}: {got}")
      -- Curl's connection counters pin one reused connection,
      -- and exercise stream 3 after the upgraded stream 1.
      let got ← curl #["-w", "connections=%{num_connects};", url ++ "/1", url ++ "/2", url ++ "/3"]
      unless got == "GET /1 HTTP/2.0 secure=false n=0;connections=1;GET /2 HTTP/2.0 secure=false n=0;connections=0;GET /3 HTTP/2.0 secure=false n=0;connections=0;" do
        throw (IO.userError s!"curl reuse {option}: {got}")
      let got ← curl #["--parallel", "-w", "\nconnections:%{num_connects}\n",
        url ++ "/1", url ++ "/2", url ++ "/3", url ++ "/4", url ++ "/5"]
      for i in [1, 2, 3, 4, 5] do
        unless (got.splitOn s!"GET /{i} HTTP/2.0 secure=false n=0;").length > 1 do
          throw (IO.userError s!"curl multiplex {option}: /{i} missing")
      let connections := (got.splitOn "\nconnections:").drop 1 |>.map fun piece =>
        (((piece.splitOn "\n").headD "").toNat?).getD 100
      unless connections.length == 5 && connections.foldl (· + ·) 0 == 1 do
        throw (IO.userError s!"curl multiplex {option} used {connections} connections")

-- Optional external conformance check, run in both modes against the same
-- cleartext listener as real clients. H2SPEC names a pinned h2spec binary;
-- ordinary lake test still runs all of the socket/protocol/curl tests above.
#eval bothModes fun mode => do
  if let some h2spec ← IO.getEnv "H2SPEC" then
    withServer mode echoApp fun port => do
      let out ← IO.Process.output {
        cmd := h2spec, args := #["-h", "127.0.0.1", "-p", toString port, "--timeout", "5"] }
      unless out.exitCode == 0 do
        throw (IO.userError s!"h2spec {repr mode}: {out.stdout}\n{out.stderr}")
      IO.println s!"h2spec {repr mode}: {out.stdout.splitOn "\n" |>.filter (fun s => (s.splitOn "tests").length > 1)}"

end Tests.Network.WebApp.Server.H2c
