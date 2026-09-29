/-
  Tests for `Linen.Network.HTTP2.Server`.

  The engine runs over an in-memory transport (`Pipe`), driven by a scripted
  client that writes raw frames and reads the server's back, so each protocol
  rule is exercised exactly: the opening exchange, requests and bodies,
  multiplexing, flow control in both directions, and the errors RFC 9113
  assigns (stream resets and GOAWAY). Interop with real clients (curl, and
  the h2spec conformance suite, which passes all 146 cases) is checked over
  TLS in `Network.WebApp.Server.TLSTest`.

  Every wait is bounded, so a protocol bug fails the build instead of
  hanging it.
-/
import Linen.Network.HTTP2.Server

open Network.HTTP2
open Control.Concurrent.Green (Green)

namespace Tests.Network.HTTP2.Server

/-! ### Pure pieces -/

private def reqHead (fields : List (String × String)) : Option String :=
  match parseRequestHead 1 fields with
  | .ok _ => none
  | .error why => some why

private def getReq : List (String × String) :=
  [(":method", "GET"), (":scheme", "https"), (":path", "/x"), (":authority", "h")]

#guard reqHead getReq == none
#guard (parseRequestHead 1 (getReq ++ [("content-length", "12")])).toOption.map (·.contentLength) == some (some 12)
#guard (parseRequestHead 1 getReq).toOption.map (·.path) == some "/x"
-- Malformed requests (§8.1.1, §8.2, §8.3).
#guard (reqHead [(":scheme", "https"), (":path", "/")]).isSome              -- no :method
#guard (reqHead [(":method", "GET"), (":path", "/")]).isSome                -- no :scheme
#guard (reqHead [(":method", "GET"), (":scheme", "https")]).isSome         -- no :path
#guard (reqHead [(":method", "GET"), (":scheme", "https"), (":path", "")]).isSome
#guard (reqHead (getReq ++ [(":method", "POST")])).isSome                      -- duplicate
#guard (reqHead (getReq ++ [(":protocol", "x")])).isSome                       -- unknown pseudo
#guard (reqHead ([("accept", "*/*")] ++ getReq)).isSome                        -- pseudo after regular
#guard (reqHead (getReq ++ [("Accept", "*/*")])).isSome                        -- uppercase name
#guard (reqHead (getReq ++ [("connection", "keep-alive")])).isSome             -- connection-specific
#guard (reqHead (getReq ++ [("transfer-encoding", "chunked")])).isSome
#guard (reqHead (getReq ++ [("te", "gzip")])).isSome
#guard reqHead (getReq ++ [("te", "trailers")]) == none
#guard (reqHead (getReq ++ [("content-length", "1"), ("content-length", "2")])).isSome
#guard (reqHead (getReq ++ [("content-length", "-1")])).isSome
-- CONNECT has only :method and :authority.
#guard reqHead [(":method", "CONNECT"), (":authority", "h:443")] == none
#guard (reqHead [(":method", "CONNECT"), (":authority", "h:443"), (":path", "/")]).isSome

#guard responseFields 200 [("Content-Type", "text/plain"), ("Connection", "close")]
  == [(":status", "200"), ("content-type", "text/plain")]
#guard headerListSize [("ab", "cde")] == 37

/-! ### An in-memory transport and a scripted client -/

/-- A one-way byte pipe: `write` from one side, `read` (with a deadline) from
    the other. -/
private structure Pipe where
  state : Std.Mutex (ByteArray × Bool × Option (IO.Promise Unit))

private def Pipe.new : IO Pipe := do return ⟨← Std.Mutex.new (ByteArray.empty, false, none)⟩

private def Pipe.write (p : Pipe) (bytes : ByteArray) : IO Unit := do
  let waiter ← p.state.atomically do
    let (buf, closed, w) ← get
    set (buf ++ bytes, closed, (none : Option (IO.Promise Unit)))
    return w
  if let some w := waiter then w.resolve ()

private def Pipe.close (p : Pipe) : IO Unit := do
  let waiter ← p.state.atomically do
    let (buf, _, w) ← get
    set (buf, true, (none : Option (IO.Promise Unit)))
    return w
  if let some w := waiter then w.resolve ()

/-- Everything written so far (or wait up to `ms` for something): `some`
    bytes, `some` empty once closed and drained, `none` on timeout. -/
private def Pipe.read (p : Pipe) (ms : Nat) : IO (Option ByteArray) := do
  repeat
    let w ← IO.Promise.new (α := Unit)
    let got ← p.state.atomically do
      let (buf, closed, _) ← get
      if buf.size > 0 then
        set (ByteArray.empty, closed, (none : Option (IO.Promise Unit)))
        return some buf
      if closed then return some ByteArray.empty
      set (buf, closed, some w)
      return none
    if let some b := got then return some b
    unless ← waitFor w ms do return none
  return none

/-- The client's end of a connection under test. -/
private structure Client where
  toServer : Pipe
  fromServer : Pipe
  buffer : IO.Ref ByteArray

private def Client.send (c : Client) (f : Frame) : IO Unit := c.toServer.write (encodeFrame f)

private def Client.sendRaw (c : Client) (bytes : ByteArray) : IO Unit := c.toServer.write bytes

/-- The next frame from the server; `none` when it closes or nothing comes. -/
private def Client.frame (c : Client) (ms : Nat := 3000) : IO (Option Frame) := do
  repeat
    let buf ← c.buffer.get
    if buf.size ≥ 9 then
      if let some h := decodeFrameHeader buf then
        let n := 9 + h.payloadLength.toNat
        if buf.size ≥ n then
          c.buffer.set (buf.extract n buf.size)
          return some { header := h, payload := buf.extract 9 n }
    match ← c.fromServer.read ms with
    | none => return none
    | some chunk =>
      if chunk.isEmpty then return none
      c.buffer.modify (· ++ chunk)
  return none

/-- The next frame that is not SETTINGS or a connection WINDOW_UPDATE. -/
private def Client.next (c : Client) (ms : Nat := 3000) : IO (Option Frame) := do
  repeat
    match ← c.frame ms with
    | none => return none
    | some f =>
      let housekeeping := f.header.frameType == .settings ||
        (f.header.frameType == .windowUpdate && f.header.streamId.val == 0)
      unless housekeeping do return some f
  return none

private def sid (n : Nat) : StreamId := StreamId.fromWire n.toUInt32

private def headersOf (fields : List (String × String)) : ByteArray :=
  HPACK.encodeHeadersStatic fields

/-- Run `handler` behind the engine and `script` as the client. -/
private def withServer (handler : Handler) (script : Client → IO α)
    (config : ServerConfig := {}) : IO α := do
  let toServer ← Pipe.new
  let fromServer ← Pipe.new
  let transport : Transport := {
    recv := do
      match ← (toServer.read 2000 : IO _) with
      | some b => return some b
      | none => return none
    send := fromServer.write }
  -- On its own thread: `Green.run` would run the server on this one until
  -- its first suspension — and a transport read blocks rather than suspends.
  let token ← Std.CancellationToken.new
  let server ← IO.asTask (prio := .dedicated) (Green.block (serve transport handler config) token)
  let client : Client := { toServer, fromServer, buffer := ← IO.mkRef ByteArray.empty }
  try
    script client
  finally
    toServer.close
    let _ ← IO.wait server

/-- Open a connection: preface and an empty SETTINGS. -/
private def Client.open (c : Client) (settings : List (SettingsKeyId × UInt32) := []) : IO Unit := do
  c.sendRaw connectionPreface
  c.send (buildSettingsFrame settings)

private def fail (msg : String) : IO α := throw (IO.userError msg)

/-- Decode a HEADERS payload (no dynamic table, as the server encodes). -/
private def fieldsOf (f : Frame) : List (String × String) :=
  ((HPACK.decodeHeaders (HPACK.DynamicTable.empty 4096) f.payload).map (·.1)).getD []

/-- Read one response on `stream`: its fields and body, up to END_STREAM. -/
private def Client.response (c : Client) (stream : Nat) : IO (List (String × String) × ByteArray) := do
  let mut fields := []
  let mut body := ByteArray.empty
  repeat
    let some f ← c.next | fail s!"no complete response on stream {stream}"
    if f.header.streamId.val.toNat != stream then continue
    match f.header.frameType with
    | .headers => fields := fieldsOf f
    | .data => body := body ++ f.payload
    | .rstStream => fail s!"stream {stream} reset: {repr (decodeRstStream f.payload)}"
    | _ => pure ()
    if FrameFlags.test f.header.flags FrameFlags.endStream then return (fields, body)
  return (fields, body)

/-- The error code of the GOAWAY the server ends with. -/
private def Client.goaway (c : Client) : IO (Option ErrorCode) := do
  repeat
    let some f ← c.frame | return none
    if f.header.frameType == .goaway then
      return (decodeGoaway f.payload).map (·.2.1)
  return none

/-- The error code of the next RST_STREAM. -/
private def Client.reset (c : Client) : IO (Option (Nat × ErrorCode)) := do
  repeat
    let some f ← c.next | return none
    if f.header.frameType == .rstStream then
      return (decodeRstStream f.payload).map (f.header.streamId.val.toNat, ·)
  return none

/-- A handler answering `method path body` and the body length. -/
private def echo : Handler := fun req responder => do
  let mut body := ByteArray.empty
  for _ in [0:100000] do
    let piece ← (req.body : IO _)
    if piece.isEmpty then break
    body := body ++ piece
  let text := s!"{req.method} {req.path} {body.size}"
  (responder.respond 200 [("content-type", "text/plain")] false : IO _)
  (responder.write text.toUTF8 : IO _)
  (responder.finish : IO _)

/-! ### The opening exchange -/

-- The server speaks first with SETTINGS (MAX_CONCURRENT_STREAMS, window,
-- header list size, no push), raises the connection window, and ACKs ours.
#eval withServer echo fun c => do
  c.open
  let some s ← c.frame | fail "no SETTINGS"
  unless s.header.frameType == .settings && !FrameFlags.test s.header.flags FrameFlags.ack do
    fail "the first frame is not SETTINGS"
  let params := (decodeSettingsPayload s.payload).getD []
  unless params.any (fun (k, v) => k == .maxConcurrentStreams && v == 100) &&
      params.any (fun (k, v) => k == .enablePush && v == 0) do
    fail s!"SETTINGS {repr params}"
  let some w ← c.frame | fail "no WINDOW_UPDATE"
  unless w.header.frameType == .windowUpdate && w.header.streamId.val == 0 &&
      decodeWindowUpdate w.payload == some (1048576 - 65535).toUInt32 do
    fail "the connection window is not raised"
  let some ack ← c.frame | fail "no SETTINGS ACK"
  unless ack.header.frameType == .settings && FrameFlags.test ack.header.flags FrameFlags.ack do
    fail "our SETTINGS were not acknowledged"

/-! ### Requests, bodies, responses -/

#eval withServer echo fun c => do
  c.open
  c.send (buildHeadersFrame (sid 1) (headersOf getReq) (endStream := true))
  let (fields, body) ← c.response 1
  unless fields.lookup ":status" == some "200" && fields.lookup "content-type" == some "text/plain" &&
      body == "GET /x 0".toUTF8 do
    fail s!"GET: {fields} {String.fromUTF8! body}"
  -- A body over several DATA frames, then END_STREAM on an empty one.
  c.send (buildHeadersFrame (sid 3) (headersOf (getReq.set 0 (":method", "POST"))))
  c.send (buildDataFrame (sid 3) (ByteArray.mk (List.replicate 1000 65).toArray))
  c.send (buildDataFrame (sid 3) (ByteArray.mk (List.replicate 500 66).toArray))
  c.send (buildDataFrame (sid 3) ByteArray.empty (endStream := true))
  let (_, body) ← c.response 3
  unless body == "POST /x 1500".toUTF8 do fail s!"POST: {String.fromUTF8! body}"
  -- A header block split over HEADERS + CONTINUATION.
  let block := headersOf getReq
  c.send (buildHeadersFrame (sid 5) (block.extract 0 3) (endStream := true) (endHeaders := false))
  c.send (buildContinuationFrame (sid 5) (block.extract 3 block.size) (endHeaders := true))
  let (fields, _) ← c.response 5
  unless fields.lookup ":status" == some "200" do fail "CONTINUATION"

-- Padding is stripped (and still counted by flow control).
#eval withServer echo fun c => do
  c.open
  c.send (buildHeadersFrame (sid 1) (headersOf (getReq.set 0 (":method", "POST"))))
  let padded := encodePadding "hello".toUTF8 10
  c.send { header := { payloadLength := padded.size.toUInt32, frameType := .data,
                       flags := FrameFlags.endStream ||| FrameFlags.padded, streamId := sid 1 },
           payload := padded }
  let (_, body) ← c.response 1
  unless body == "POST /x 5".toUTF8 do fail s!"padded: {String.fromUTF8! body}"

/-! ### Multiplexing: streams are handled concurrently -/

-- Stream 1's handler waits until stream 3's has run; handled one at a time
-- (as through 1.8.0), stream 1 would never answer.
#eval show IO Unit from do
  let gate ← IO.Promise.new (α := Unit)
  let handler : Handler := fun req responder => do
    if req.path == "/first" then
      let _ ← (waitFor gate 3000 : IO _)
    (responder.respond 200 [] false : IO _)
    (responder.write req.path.toUTF8 : IO _)
    (responder.finish : IO _)
    if req.path != "/first" then (gate.resolve () : IO _)
  withServer handler fun c => do
    c.open
    c.send (buildHeadersFrame (sid 1) (headersOf (getReq.set 2 (":path", "/first"))) (endStream := true))
    c.send (buildHeadersFrame (sid 3) (headersOf (getReq.set 2 (":path", "/second"))) (endStream := true))
    let (_, second) ← c.response 3
    let (_, first) ← c.response 1
    unless first == "/first".toUTF8 && second == "/second".toUTF8 do fail "multiplexing"

/-! ### Flow control -/

-- Sending: the client allows 10 bytes per stream, so a 25-byte body comes in
-- frames of at most 10, and only as the client opens its window.
#eval show IO Unit from do
  let handler : Handler := fun _ responder => do
    (responder.respond 200 [] false : IO _)
    (responder.write (ByteArray.mk (List.replicate 25 120).toArray) : IO _)
    (responder.finish : IO _)
  withServer handler fun c => do
    c.open [(.initialWindowSize, 10)]
    c.send (buildHeadersFrame (sid 1) (headersOf getReq) (endStream := true))
    let mut sizes : List Nat := []
    let mut done := false
    for _ in [0:20] do
      if done then break
      let some f ← c.next 500 | break
      if f.header.frameType == .data then
        sizes := sizes ++ [f.payload.size]
        if FrameFlags.test f.header.flags FrameFlags.endStream then done := true
        else if f.payload.size > 0 then
          c.send (buildWindowUpdateFrame (sid 1) f.payload.size.toUInt32)
    unless done && sizes.all (· ≤ 10) && sizes.foldl (· + ·) 0 == 25 do
      fail s!"DATA sizes {sizes}"

-- Sending, stalled: with the stream window at 0, nothing is sent until a
-- WINDOW_UPDATE, then the rest follows.
#eval show IO Unit from do
  let handler : Handler := fun _ responder => do
    (responder.respond 200 [] false : IO _)
    (responder.write "abc".toUTF8 : IO _)
    (responder.finish : IO _)
  withServer handler fun c => do
    c.open [(.initialWindowSize, 0)]
    c.send (buildHeadersFrame (sid 1) (headersOf getReq) (endStream := true))
    let some h ← c.next | fail "no HEADERS"
    unless h.header.frameType == .headers do fail "expected HEADERS first"
    if let some f ← c.next 300 then fail s!"sent {repr f.header.frameType} with a zero window"
    c.send (buildWindowUpdateFrame (sid 1) 100)
    let (_, body) ← c.response 1
    unless body == "abc".toUTF8 do fail "after WINDOW_UPDATE"

-- A SETTINGS_INITIAL_WINDOW_SIZE change applies to open streams (§6.9.2).
#eval show IO Unit from do
  let handler : Handler := fun _ responder => do
    (responder.respond 200 [] false : IO _)
    (responder.write "xyz".toUTF8 : IO _)
    (responder.finish : IO _)
  withServer handler fun c => do
    c.open [(.initialWindowSize, 0)]
    c.send (buildHeadersFrame (sid 1) (headersOf getReq) (endStream := true))
    let _ ← c.next
    c.send (buildSettingsFrame [(.initialWindowSize, 1000)])
    let (_, body) ← c.response 1
    unless body == "xyz".toUTF8 do fail "window raised by SETTINGS"

-- Receiving: bytes the handler reads come back as WINDOW_UPDATE, so an
-- upload larger than the window completes (through 1.8.0 it stalled).
#eval show IO Unit from do
  let config : ServerConfig := { initialWindowSize := 1000, connectionWindowSize := 65535 }
  withServer echo (config := config) fun c => do
    c.open
    c.send (buildHeadersFrame (sid 1) (headersOf (getReq.set 0 (":method", "POST"))))
    -- 100 KB in 500-byte frames, respecting the windows the server grants.
    let mut streamWindow : Int := 1000
    let mut connWindow : Int := 65535
    let mut sent := 0
    let mut result : Option ByteArray := none
    for _ in [0:10000] do
      if sent == 100000 then break
      if streamWindow ≥ 500 && connWindow ≥ 500 then
        c.send (buildDataFrame (sid 1) (ByteArray.mk (List.replicate 500 97).toArray)
          (endStream := sent + 500 == 100000))
        sent := sent + 500
        streamWindow := streamWindow - 500
        connWindow := connWindow - 500
      else
        let some f ← c.frame 2000 | fail s!"stalled after {sent} bytes"
        if f.header.frameType == .windowUpdate then
          let inc := (decodeWindowUpdate f.payload).getD 0
          if f.header.streamId.val == 0 then connWindow := connWindow + inc.toNat
          else streamWindow := streamWindow + inc.toNat
    let (_, body) ← c.response 1
    result := some body
    unless result == some "POST /x 100000".toUTF8 do fail s!"upload: {result.map String.fromUTF8!}"

-- Receiving beyond the window a stream was given is a stream error.
#eval withServer echo (config := { initialWindowSize := 100 }) fun c => do
  c.open
  c.send (buildHeadersFrame (sid 1) (headersOf (getReq.set 0 (":method", "POST"))))
  c.send (buildDataFrame (sid 1) (ByteArray.mk (List.replicate 200 97).toArray))
  let r ← c.reset
  unless r == some (1, .flowControlError) do fail s!"got {repr r}"

/-! ### Stream errors: RST_STREAM, the connection goes on -/

#eval show IO Unit from do
  let gate ← IO.Promise.new (α := Unit)
  let handler : Handler := fun req responder => do
    if req.streamId == 5 then
      -- The request is complete but the response is deliberately still open.
      -- Naming the path /slow did not slow `echo`: it could finish before
      -- DATA was read, making stream 5 fully closed (correctly a GOAWAY).
      -- The client below has bounded reads; its finally always releases us.
      let _ ← (IO.wait gate.result? : IO _)
    echo req responder
  try
    withServer handler fun c => do
      c.open
      -- A malformed request (uppercase field name).
      c.send (buildHeadersFrame (sid 1) (headersOf (getReq ++ [("Accept", "*")])) (endStream := true))
      unless (← c.reset) == some (1, .protocolError) do fail "malformed request not reset"
      -- content-length that disagrees with the DATA sent.
      c.send (buildHeadersFrame (sid 3) (headersOf (getReq ++ [("content-length", "10")])))
      c.send (buildDataFrame (sid 3) "abc".toUTF8 (endStream := true))
      unless (← c.reset) == some (3, .protocolError) do fail "content-length mismatch not reset"
      -- DATA after END_STREAM (half-closed remote): STREAM_CLOSED.
      c.send (buildHeadersFrame (sid 5) (headersOf getReq) (endStream := true))
      c.send (buildDataFrame (sid 5) "x".toUTF8)
      unless (← c.reset) == some (5, .streamClosed) do
        fail "DATA on a half-closed stream not reset"
      gate.resolve ()
      -- The connection still works.
      c.send (buildHeadersFrame (sid 7) (headersOf getReq) (endStream := true))
      let (fields, _) ← c.response 7
      unless fields.lookup ":status" == some "200" do fail "connection unusable after stream errors"
  finally
    gate.resolve ()

-- Beyond SETTINGS_MAX_CONCURRENT_STREAMS: REFUSED_STREAM.
#eval show IO Unit from do
  let gate ← IO.Promise.new (α := Unit)
  let handler : Handler := fun _ responder => do
    let _ ← (waitFor gate 3000 : IO _)
    (responder.respond 204 [] true : IO _)
  withServer handler (config := { maxConcurrentStreams := 1 }) fun c => do
    c.open
    c.send (buildHeadersFrame (sid 1) (headersOf getReq) (endStream := true))
    c.send (buildHeadersFrame (sid 3) (headersOf getReq) (endStream := true))
    let r ← c.reset
    gate.resolve ()
    unless r == some (3, .refusedStream) do fail s!"got {repr r}"

-- A handler that throws before responding produces 500.
#eval show IO Unit from do
  let handler : Handler := fun _ _ => throw (IO.userError "boom")
  withServer handler fun c => do
    c.open
    c.send (buildHeadersFrame (sid 1) (headersOf getReq) (endStream := true))
    let (fields, _) ← c.response 1
    unless fields.lookup ":status" == some "500" do fail s!"got {fields}"

-- Request heads over SETTINGS_MAX_HEADER_LIST_SIZE get 431.
#eval withServer echo (config := { maxHeaderListSize := 200 }) fun c => do
  c.open
  c.send (buildHeadersFrame (sid 1) (headersOf (getReq ++ [("x-big", String.ofList (List.replicate 300 'v'))]))
    (endStream := true))
  let (fields, _) ← c.response 1
  unless fields.lookup ":status" == some "431" do fail s!"got {fields}"

/-! ### Connection errors: GOAWAY -/

private def expectGoaway (setup : Client → IO Unit) (code : ErrorCode) (config : ServerConfig := {}) :
    IO Unit :=
  withServer echo (config := config) fun c => do
    setup c
    let got ← c.goaway
    unless got == some code do fail s!"expected GOAWAY {repr code}, got {repr got}"

-- A bad preface.
#eval expectGoaway (fun c => c.sendRaw "GET / HTTP/1.1\r\nHost: x\r\n\r\n\r\n\r\n".toUTF8) .protocolError
-- A preface not followed by SETTINGS.
#eval expectGoaway (fun c => do
    c.sendRaw connectionPreface
    c.send (buildPingFrame (ByteArray.mk #[0,0,0,0,0,0,0,0]))) .protocolError
-- DATA on stream 0.
#eval expectGoaway (fun c => do c.open; c.send (buildDataFrame StreamId.zero "x".toUTF8)) .protocolError
-- DATA on an idle stream.
#eval expectGoaway (fun c => do c.open; c.send (buildDataFrame (sid 9) "x".toUTF8)) .protocolError
-- DATA on a normally closed stream is a connection error, unlike the
-- half-closed stream above. Receiving the complete response pins that state
-- without sleeps or assumptions about the handler's scheduling.
#eval expectGoaway (fun c => do
    c.open
    c.send (buildHeadersFrame (sid 1) (headersOf getReq) (endStream := true))
    let _ ← c.response 1
    c.send (buildDataFrame (sid 1) "x".toUTF8)) .streamClosed
-- HEADERS on an even (server) stream id.
#eval expectGoaway (fun c => do
    c.open; c.send (buildHeadersFrame (sid 2) (headersOf getReq) (endStream := true))) .protocolError
-- A stream id lower than one already used.
#eval expectGoaway (fun c => do
    c.open
    c.send (buildHeadersFrame (sid 5) (headersOf getReq) (endStream := true))
    c.send (buildHeadersFrame (sid 3) (headersOf getReq) (endStream := true))) .protocolError
-- A frame larger than SETTINGS_MAX_FRAME_SIZE.
#eval expectGoaway (fun c => do
    c.open
    c.send (buildHeadersFrame (sid 1) (headersOf getReq))
    c.send (buildDataFrame (sid 1) (ByteArray.mk (List.replicate 16385 0).toArray))) .frameSizeError
-- An interrupted header block.
#eval expectGoaway (fun c => do
    c.open
    c.send (buildHeadersFrame (sid 1) (headersOf getReq) (endHeaders := false))
    c.send (buildPingFrame (ByteArray.mk #[0,0,0,0,0,0,0,0]))) .protocolError
-- An undecodable header block.
#eval expectGoaway (fun c => do
    c.open; c.send (buildHeadersFrame (sid 1) (ByteArray.mk #[0xff, 0xff]) (endStream := true)))
  .compressionError
-- SETTINGS_INITIAL_WINDOW_SIZE above 2^31-1.
#eval expectGoaway (fun c => c.open [(.initialWindowSize, 0x80000000)]) .flowControlError
-- A connection window above 2^31-1.
#eval expectGoaway (fun c => do
    c.open; c.send (buildWindowUpdateFrame StreamId.zero 0x7fffffff)) .flowControlError
-- PUSH_PROMISE from a client.
#eval expectGoaway (fun c => do
    c.open
    c.send { header := { payloadLength := 4, frameType := .pushPromise, flags := FrameFlags.endHeaders,
                         streamId := sid 1 }, payload := ByteArray.mk #[0, 0, 0, 2] }) .protocolError
-- Rapid reset (CVE-2023-44487): past `maxClientResets`, ENHANCE_YOUR_CALM.
#eval expectGoaway (config := { maxClientResets := 3 }) (fun c => do
    c.open
    for i in [0:5] do
      c.send (buildHeadersFrame (sid (2 * i + 1)) (headersOf (getReq.set 2 (":path", "/slow")))
        (endStream := false))
      c.send (buildRstStreamFrame (sid (2 * i + 1)) .cancel)) .enhanceYourCalm

-- PING is answered with its payload, flagged ACK.
#eval withServer echo fun c => do
  c.open
  let payload := ByteArray.mk #[1, 2, 3, 4, 5, 6, 7, 8]
  c.send (buildPingFrame payload)
  let mut pong := false
  for _ in [0:5] do
    let some f ← c.next | break
    if f.header.frameType == .ping then
      pong := FrameFlags.test f.header.flags FrameFlags.ack && f.payload == payload
      break
  unless pong do fail "PING not answered"

-- An idle connection (the transport's timeout, no stream open) ends with
-- GOAWAY NO_ERROR.
#eval expectGoaway (fun c => c.open) .noError

/-! ### The historical entry point -/

-- A transport exception (not a protocol GOAWAY) still tears down the
-- engine before its caller can release TLS/socket resources: waiting body
-- readers wake, and the handler cannot write after serve returns.
#eval show IO Unit from do
  let toServer ← Pipe.new
  let fromServer ← Pipe.new
  let broken ← IO.mkRef false
  let started ← IO.Promise.new (α := Unit)
  let finished ← IO.Promise.new (α := Bool)
  let done ← IO.Promise.new (α := Unit)
  let transport : Transport := {
    recv := do
      let bytes ← (toServer.read 3000 : IO _)
      if ← (broken.get : IO _) then throw (IO.userError "injected transport failure")
      return bytes
    send := fromServer.write }
  let handler : Handler := fun req responder => do
    (started.resolve () : IO _)
    let bodyFailed ← (try let _ ← req.body; pure false catch _ => pure true : IO _)
    let writeFailed ← (try responder.respond 200 [] true; pure false catch _ => pure true : IO _)
    (finished.resolve (bodyFailed && writeFailed) : IO _)
    (done.resolve () : IO _)
  let server ← IO.asTask (prio := .dedicated)
    (Green.block (serve transport handler) (← Std.CancellationToken.new))
  let c : Client := { toServer, fromServer, buffer := ← IO.mkRef ByteArray.empty }
  try
    c.open
    c.send (buildHeadersFrame (sid 1) (headersOf getReq) (endStream := false))
    unless ← waitFor started 3000 do fail "handler did not start"
    broken.set true
    toServer.write "wake the transport".toUTF8
    match ← IO.wait server with
    | .ok () => fail "transport failure was not propagated"
    | .error _ => pure ()
    unless ← waitFor done 3000 do fail "transport failure left the body reader waiting"
    unless (← IO.wait finished.result?).getD false do fail "handler wrote after teardown"
  finally
    toServer.close
    fromServer.close

#eval show IO Unit from do
  let toServer ← Pipe.new
  let fromServer ← Pipe.new
  let recv : IO ByteArray := do return (← toServer.read 3000).getD ByteArray.empty
  let serverTask ← IO.asTask (runHTTP2Connection recv fromServer.write fun fields _ => do
    return ([(":status", "201"), ("x-path", (fields.lookup ":path").getD "")], "made".toUTF8))
  let c : Client := { toServer, fromServer, buffer := ← IO.mkRef ByteArray.empty }
  c.open
  c.send (buildHeadersFrame (sid 1) (headersOf getReq) (endStream := true))
  let (fields, body) ← c.response 1
  toServer.close
  let _ ← IO.wait serverTask
  unless fields.lookup ":status" == some "201" && fields.lookup "x-path" == some "/x" &&
      body == "made".toUTF8 do
    fail s!"runHTTP2Connection: {fields}"

end Tests.Network.HTTP2.Server
