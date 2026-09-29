/-
  Linen.Network.HTTP2.Server — the HTTP/2 server connection (RFC 9113)

  Serves one HTTP/2 connection over any byte transport: the connection
  preface and SETTINGS exchange, multiplexed streams each handled
  concurrently, request bodies delivered as the handler reads them, responses
  split to the peer's frame size and paced by flow control in both
  directions, and the protocol's error handling (stream errors reset one
  stream, connection errors end the connection with GOAWAY).

  ## Design

  - **One reader.** `serve` runs a loop on the connection's green thread
    that reads and validates every frame. It alone decodes HPACK, so the
    decoder's dynamic table needs no lock.
  - **One handler per stream.** A complete request head starts the
    handler on its own green thread, at once — the body follows through the
    stream's inbox (`Request.body`), so a handler can stream an upload and a
    slow stream does not hold up the others.
  - **One lock, one writer.** All shared state (streams, windows, waiters) is
    in one `Std.Mutex`, held only for bookkeeping; frames are written under a
    separate write lock, so a HEADERS block and its CONTINUATIONs are
    contiguous on the wire. The encoder uses no dynamic table
    (`encodeHeadersStatic`), so blocks from different streams need no order.
  - **Waiting.** A body read with nothing buffered, and a send with no
    window, wait on a promise — resolved when DATA or WINDOW_UPDATE arrives,
    by a reset, or by the connection closing — with a timer bounding the
    wait. `IO.wait` on a promise is compensated by Lean's task manager, so a
    waiting handler does not starve the pool.
  - **Flow control, receiving.** The peer may send what we advertised
    (`ServerConfig.initialWindowSize` per stream, `connectionWindowSize` for
    the connection); bytes the handler has consumed — or that were discarded
    — are returned with WINDOW_UPDATE once half a window has accumulated.
    Bytes a handler does not read are therefore back-pressure, not memory.
  - **Teardown.** When the reader stops, the connection is marked closed
    *under the write lock*: no frame is being written, and none will be. So
    the caller may free the transport (a TLS session) as soon as `serve`
    returns, even if handlers are still running — their next write fails.

  ## Validation

  Every requirement of RFC 9113 §4–§8 that a server must check is checked:
  frame sizes and lengths, stream-id rules (odd, increasing, not idle or
  closed), the CONTINUATION sequence, SETTINGS values, flow-control windows
  (both levels, including SETTINGS_INITIAL_WINDOW_SIZE changes), padding,
  self-dependent priorities, and malformed requests (§8.1.1: pseudo-header
  rules, uppercase or connection-specific fields, `te`, content-length
  against the DATA received). Errors are stream errors or connection errors
  as the RFC assigns them.

  Through 1.8.0 this module called the handler as soon as a request's
  headers arrived (bodies were never delivered), handled streams one at a
  time, sent a whole body as one DATA frame regardless of the peer's frame
  size and windows, never returned receive window (an upload stalled after
  64 KiB), and validated incoming frames against the *peer's* settings.

  ## No `partial`

  Every loop is a `while`/`repeat` over a genuine lifecycle condition.

  ## Haskell equivalent
  `Network.HTTP2.Server` (https://hackage.haskell.org/package/http2)
-/
import Linen.Network.HTTP2.Frame.Types
import Linen.Network.HTTP2.Frame.Encode
import Linen.Network.HTTP2.Frame.Decode
import Linen.Network.HTTP2.HPACK.Table
import Linen.Network.HTTP2.HPACK.Encode
import Linen.Network.HTTP2.HPACK.Decode
import Linen.Network.HTTP2.Types
import Linen.Control.Concurrent
import Linen.Control.Concurrent.Green
import Std.Sync.Mutex
import Std.Data.HashMap
import Std.Data.HashSet
import Std.Internal.UV.Timer

namespace Network.HTTP2

open Control.Concurrent.Green (Green)

-- ── Configuration and interfaces ──────────────────────────────────

/-- What this server advertises and enforces. -/
structure ServerConfig where
  /-- SETTINGS_MAX_CONCURRENT_STREAMS: streams beyond it are refused. -/
  maxConcurrentStreams : Nat := 100
  /-- SETTINGS_INITIAL_WINDOW_SIZE: what a peer may send on a stream
      before we return window. -/
  initialWindowSize : Nat := 262144
  /-- The connection-level receive window (raised from 65535 at start). -/
  connectionWindowSize : Nat := 1048576
  /-- SETTINGS_MAX_HEADER_LIST_SIZE: a larger request head gets `431`. -/
  maxHeaderListSize : Nat := 65536
  /-- How long a body read, or a send waiting for window, may wait. -/
  timeoutMillis : Nat := 30000
  /-- Client-initiated resets tolerated on one connection before it is
      ended with ENHANCE_YOUR_CALM (the "rapid reset" attack, CVE-2023-44487,
      opens and resets streams to make the server start handlers). -/
  maxClientResets : Nat := 10000

/-- A byte transport. `recv`: the next bytes (`some` empty at end of input,
    `none` when the transport's idle timeout passed); `send`: write all. -/
structure Transport where
  recv : Green (Option ByteArray)
  send : ByteArray → IO Unit

/-- The frame payload size we accept (SETTINGS_MAX_FRAME_SIZE, left at its
    default, which every peer must be able to handle). -/
def localMaxFrameSize : Nat := 16384

/-- Our HPACK decoder's table limit (SETTINGS_HEADER_TABLE_SIZE, default). -/
def localHeaderTableSize : Nat := 4096

/-- A request, as delivered to a stream's handler. -/
structure Request where
  streamId : Nat
  method : String
  /-- `:scheme` (absent for CONNECT). -/
  scheme : Option String
  /-- `:authority`, if sent. -/
  authority : Option String
  /-- `:path` (empty for CONNECT). -/
  path : String
  /-- The regular header fields, in order. -/
  headers : List (String × String)
  /-- `content-length`, if sent. -/
  contentLength : Option Nat
  /-- The next piece of the body; empty at its end. Throws if the stream is
      reset, the connection closes, or nothing arrives in time. -/
  body : IO ByteArray

/-- The completed HTTP/1.1 request that initiated an h2c upgrade, and the
    peer's HTTP2-Settings. It becomes half-closed (remote) stream 1; its
    already-received body is read outside HTTP/2 receive flow control. -/
structure UpgradeRequest where
  request : Request
  settings : List (SettingsKeyId × UInt32)

/-- How a handler answers. `respond` sends the status and headers (at most
    once; `endStream` for a response with no body), `write` a piece of body
    (split and paced by flow control), `finish` ends the body. -/
structure Responder where
  respond : (status : Nat) → List (String × String) → (endStream : Bool) → IO Unit
  write : ByteArray → IO Unit
  finish : IO Unit

/-- A stream's handler. When it returns, a response it left open is ended
    (a missing one becomes `500`); if it throws, a response not yet started
    becomes `500` and one already started is reset. -/
abbrev Handler := Request → Responder → Green Unit

-- ── Request validation (RFC 9113 §8.1.1, §8.2, §8.3) ──────────────

/-- Fields that are connection-specific in HTTP/1.1 and malformed in HTTP/2
    (§8.2.2). -/
def connectionSpecificFields : List String :=
  ["connection", "keep-alive", "proxy-connection", "transfer-encoding", "upgrade"]

/-- Check a decoded request head and split it into a `Request` (with a
    placeholder body), or say why it is malformed — a stream error of type
    PROTOCOL_ERROR. -/
def parseRequestHead (streamId : Nat) (fields : List (String × String)) :
    Except String Request := do
  let mut method : Option String := none
  let mut scheme : Option String := none
  let mut authority : Option String := none
  let mut path : Option String := none
  let mut regular : List (String × String) := []
  let mut seenRegular := false
  for (name, value) in fields do
    if name.toList.any Char.isUpper then throw s!"uppercase field name {name}"
    if name.startsWith ":" then
      if seenRegular then throw s!"pseudo-header {name} after a regular field"
      match name with
      | ":method" => if method.isSome then throw "duplicate :method" else method := some value
      | ":scheme" => if scheme.isSome then throw "duplicate :scheme" else scheme := some value
      | ":authority" =>
        if authority.isSome then throw "duplicate :authority" else authority := some value
      | ":path" => if path.isSome then throw "duplicate :path" else path := some value
      | _ => throw s!"unknown pseudo-header {name}"
    else
      seenRegular := true
      if connectionSpecificFields.contains name then throw s!"connection-specific field {name}"
      if name == "te" && value != "trailers" then throw "te other than trailers"
      regular := (name, value) :: regular
  let some m := method | throw "missing :method"
  if m == "CONNECT" then
    if scheme.isSome || path.isSome then throw "CONNECT with :scheme or :path"
    if authority.isNone then throw "CONNECT without :authority"
  else
    if scheme.isNone then throw "missing :scheme"
    match path with
    | none => throw "missing :path"
    | some "" => throw "empty :path"
    | some _ => pure ()
  let fieldsInOrder := regular.reverse
  let lengths := fieldsInOrder.filter (·.1 == "content-length") |>.map (·.2.trimAscii.toString)
  let contentLength ← match lengths with
    | [] => pure none
    | l :: rest =>
      match l.toNat? with
      | some n => if rest.all (· == l) && l.all Char.isDigit then pure (some n)
                  else throw "invalid content-length"
      | none => throw "invalid content-length"
  return { streamId, method := m, scheme, authority, path := path.getD "", headers := fieldsInOrder,
           contentLength, body := pure ByteArray.empty }

/-- The header block of a response: `:status` first, then the fields with
    their names lowercased (§8.2.1) and connection-specific ones dropped
    (§8.2.2). -/
def responseFields (status : Nat) (headers : List (String × String)) : List (String × String) :=
  (":status", toString status) ::
    (headers.filterMap fun (n, v) =>
      let n := n.toLower
      if connectionSpecificFields.contains n then none else some (n, v))

/-- The size of a header list as SETTINGS_MAX_HEADER_LIST_SIZE counts it
    (§6.5.2): octets of names and values, plus 32 per field. -/
def headerListSize (fields : List (String × String)) : Nat :=
  fields.foldl (fun acc (n, v) => acc + n.utf8ByteSize + v.utf8ByteSize + 32) 0

-- ── Connection state ──────────────────────────────────────────────

/-- One stream's state, shared by the reader and its handler. -/
structure StreamSlot where
  /-- Received body pieces not yet read, oldest first, from `inboxHead`. -/
  inbox : Array ByteArray := #[]
  inboxHead : Nat := 0
  /-- END_STREAM received (the request is complete). -/
  remoteDone : Bool := false
  /-- Body bytes received, for the content-length check. -/
  received : Nat := 0
  contentLength : Option Nat := none
  /-- What the peer may still send on this stream. -/
  recvWindow : Int
  /-- Bytes read (or discarded) and not yet returned by WINDOW_UPDATE. -/
  unacked : Nat := 0
  /-- What we may still send on this stream. -/
  sendWindow : Int
  /-- Woken when body arrives, the stream is reset, or the connection ends. -/
  bodyWaiter : Option (IO.Promise Unit) := none
  /-- Woken when window opens, the stream is reset, or the connection ends. -/
  sendWaiter : Option (IO.Promise Unit) := none
  /-- RST_STREAM sent or received. -/
  reset : Bool := false
  /-- HEADERS sent. -/
  headersSent : Bool := false
  /-- END_STREAM sent. -/
  localDone : Bool := false

/-- Connection-wide state. -/
structure ConnState where
  streams : Std.HashMap Nat StreamSlot := {}
  /-- The highest client stream id seen. -/
  lastClientStream : Nat := 0
  /-- Recently closed stream ids (`closedOrder` bounds them): frames on them
      are STREAM_CLOSED, where on a never-used lower id they are a protocol
      error. -/
  closed : Std.HashSet Nat := {}
  closedOrder : Array Nat := #[]
  /-- Closed streams we reset: frames still in flight on them are ignored
      (§5.4.2). -/
  resetByUs : Std.HashSet Nat := {}
  /-- Closed streams the peer reset: frames on them are a *stream* error
      (§5.1, "closed"), where on a stream closed by END_STREAM both ways they
      are a connection error. -/
  resetByPeer : Std.HashSet Nat := {}
  connSendWindow : Int := 65535
  connRecvWindow : Int := 65535
  connUnacked : Nat := 0
  /-- The peer's SETTINGS_INITIAL_WINDOW_SIZE and SETTINGS_MAX_FRAME_SIZE. -/
  peerInitialWindow : Nat := 65535
  peerMaxFrameSize : Nat := 16384
  /-- The transport is done: nothing more is written. -/
  isClosed : Bool := false
  /-- GOAWAY received: no new streams. -/
  goingAway : Bool := false
  clientResets : Nat := 0

/-- How many closed stream ids are remembered. -/
def closedMemory : Nat := 4096

namespace ConnState

/-- Forget a stream, remembering its id as closed — and how: reset by us (its
    in-flight frames are ignored), reset by the peer, or neither (closed by
    END_STREAM both ways). -/
def retire (st : ConnState) (id : Nat) (byUs : Bool) (byPeer : Bool := false) : ConnState := Id.run do
  let mut st := { st with streams := st.streams.erase id, closed := st.closed.insert id,
                          closedOrder := st.closedOrder.push id }
  if byUs then st := { st with resetByUs := st.resetByUs.insert id }
  if byPeer then st := { st with resetByPeer := st.resetByPeer.insert id }
  if st.closedOrder.size > closedMemory then
    let old := st.closedOrder[0]!
    st := { st with closedOrder := st.closedOrder.extract 1 st.closedOrder.size,
                    closed := st.closed.erase old, resetByUs := st.resetByUs.erase old,
                    resetByPeer := st.resetByPeer.erase old }
  return st

end ConnState

/-- A running connection. -/
structure Conn where
  config : ServerConfig
  transport : Transport
  state : Std.Mutex ConnState
  writeLock : Std.Mutex Unit

/-- An error for operations on a stream or connection that is gone. -/
private def goneError (what : String) : IO.Error := IO.userError s!"HTTP/2: {what}"

/-- Wait for `p` for at most `ms`; `false` on timeout. A libuv timer resolves
    the race; the promise and the timer are both dropped afterwards. -/
def waitFor (p : IO.Promise Unit) (ms : Nat) : IO Bool := do
  let done ← IO.Promise.new (α := Bool)
  let timer ← Std.Internal.UV.Timer.mk ms.toUInt64 false
  let fired ← timer.next
  let _ ← IO.mapTask (t := p.result?) fun _ => done.resolve true
  let _ ← IO.mapTask (t := fired.result?) fun
    | some () => done.resolve false
    | none => pure ()
  let r := (← IO.wait done.result?).getD false
  if r then try timer.stop catch _ => pure ()
  return r

namespace Conn

/-- Write bytes to the transport, unless the connection is closed. Holding
    the write lock keeps a frame sequence contiguous and orders writes with
    teardown. -/
def send (c : Conn) (bytes : ByteArray) : IO Unit :=
  c.writeLock.atomically do
    if (← c.state.atomically get).isClosed then throw (goneError "connection closed")
    c.transport.send bytes

/-- `send`, ignoring failure — for frames whose loss only matters if the
    connection is going anyway (acknowledgements, resets). -/
def sendQuietly (c : Conn) (bytes : ByteArray) : IO Unit :=
  try c.send bytes catch _ => pure ()

/-- Wake every waiter (used when the connection ends). -/
private def wakeAll (st : ConnState) : IO Unit := do
  for (_, s) in st.streams.toList do
    if let some p := s.bodyWaiter then p.resolve ()
    if let some p := s.sendWaiter then p.resolve ()

/-- Mark the connection closed: no frame is being written (the write lock is
    held) and none will be; every waiting handler is woken. -/
def close (c : Conn) : IO Unit := do
  let st ← c.writeLock.atomically do
    c.state.atomically (modifyGet fun st => (st, { st with isClosed := true }))
  wakeAll st

/-- Reset a stream from our side with `code`, waking its handler, and retire
    it once the handler is done with it. -/
def resetStream (c : Conn) (id : Nat) (code : ErrorCode) : IO Unit := do
  let slot? ← c.state.atomically do
    let st ← get
    match st.streams[id]? with
    | none => pure none
    | some s =>
      -- Unread body bytes go back to the connection window.
      let unread := s.inbox.foldl (init := 0) fun acc b => acc + b.size
      set { (st.retire id true) with connUnacked := st.connUnacked + unread }
      pure (some s)
  c.sendQuietly (encodeFrame (buildRstStreamFrame (StreamId.fromWire id.toUInt32) code))
  if let some s := slot? then
    if let some p := s.bodyWaiter then p.resolve ()
    if let some p := s.sendWaiter then p.resolve ()

/-- Send WINDOW_UPDATE frames for consumed bytes once half a window has
    accumulated, at the connection and (if its body is still coming) on the
    stream. -/
def returnWindow (c : Conn) (id : Nat) (bytes : Nat) : IO Unit := do
  let frames ← c.state.atomically do
    let st ← get
    let mut frames := ByteArray.empty
    let mut st := { st with connUnacked := st.connUnacked + bytes }
    if st.connUnacked ≥ c.config.connectionWindowSize / 2 then
      frames := frames ++ encodeFrame (buildWindowUpdateFrame StreamId.zero st.connUnacked.toUInt32)
      st := { st with connRecvWindow := st.connRecvWindow + st.connUnacked, connUnacked := 0 }
    if let some s := st.streams[id]? then
      let s := { s with unacked := s.unacked + bytes }
      if !s.remoteDone && !s.reset && s.unacked ≥ c.config.initialWindowSize / 2 then
        frames := frames ++ encodeFrame
          (buildWindowUpdateFrame (StreamId.fromWire id.toUInt32) s.unacked.toUInt32)
        st := { st with streams := (st.streams.insert id
          { s with recvWindow := s.recvWindow + s.unacked, unacked := 0 }) }
      else
        st := { st with streams := st.streams.insert id s }
    set st
    pure frames
  if frames.size > 0 then c.sendQuietly frames

/-- The next piece of a stream's body, for `Request.body`. -/
def readBody (c : Conn) (id : Nat) : IO ByteArray := do
  repeat
    let waiter ← IO.Promise.new (α := Unit)
    let outcome ← c.state.atomically do
      let st ← get
      if st.isClosed then return Except.error "connection closed"
      match st.streams[id]? with
      | none => return .error "stream closed"
      | some s =>
        if s.reset then return .error "stream reset"
        if s.inboxHead < s.inbox.size then
          let chunk := s.inbox[s.inboxHead]!
          let s := if s.inboxHead + 1 == s.inbox.size then { s with inbox := #[], inboxHead := 0 }
                   else { s with inboxHead := s.inboxHead + 1 }
          set { st with streams := st.streams.insert id s }
          return .ok (some chunk)
        if s.remoteDone then return .ok (some ByteArray.empty)
        set { st with streams := st.streams.insert id { s with bodyWaiter := some waiter } }
        return .ok none
    match outcome with
    | .error why => throw (goneError why)
    | .ok (some chunk) =>
      if chunk.size > 0 then c.returnWindow id chunk.size
      return chunk
    | .ok none =>
      unless ← waitFor waiter c.config.timeoutMillis do
        throw (goneError s!"no request body within {c.config.timeoutMillis}ms")
  return ByteArray.empty

/-- Send HEADERS (+ CONTINUATION) for a response. -/
def sendHeaders (c : Conn) (id : Nat) (status : Nat) (headers : List (String × String))
    (endStream : Bool) : IO Unit := do
  let ok ← c.state.atomically do
    let st ← get
    match st.streams[id]? with
    | some s =>
      if s.reset || s.headersSent then return false
      set { st with streams := (st.streams.insert id
        { s with headersSent := true, localDone := s.localDone || endStream }) }
      return true
    | none => return false
  unless ok do throw (goneError "response on a reset, closed or answered stream")
  let block := HPACK.encodeHeadersStatic (responseFields status headers)
  let sid := StreamId.fromWire id.toUInt32
  let maxFrame := (← c.state.atomically get).peerMaxFrameSize
  let frames := match splitHeaderBlock block maxFrame with
    | [] => encodeFrame (buildHeadersFrame sid ByteArray.empty endStream true)
    | [one] => encodeFrame (buildHeadersFrame sid one endStream true)
    | first :: rest =>
      let last := rest.getLast!
      let middle := rest.dropLast
      encodeFrame (buildHeadersFrame sid first endStream false) ++
        middle.foldl (fun acc b => acc ++ encodeFrame (buildContinuationFrame sid b false))
          ByteArray.empty ++
        encodeFrame (buildContinuationFrame sid last true)
  c.send frames

/-- Send body bytes as DATA frames, each no larger than the peer's frame size
    and both windows, waiting for window when there is none. -/
def sendData (c : Conn) (id : Nat) (data : ByteArray) (endStream : Bool) : IO Unit := do
  let sid := StreamId.fromWire id.toUInt32
  let mut offset := 0
  let mut sentEnd := false
  while !sentEnd do
    let waiter ← IO.Promise.new (α := Unit)
    let remaining := data.size - offset
    let grant ← c.state.atomically do
      let st ← get
      if st.isClosed then return Except.error "connection closed"
      match st.streams[id]? with
      | none => return .error "stream closed"
      | some s =>
        if s.reset then return .error "stream reset"
        if s.localDone then return .error "response already ended"
        if remaining == 0 then
          -- An empty frame carrying only END_STREAM needs no window.
          set { st with streams := st.streams.insert id { s with localDone := endStream } }
          return .ok (some 0)
        let window := min s.sendWindow st.connSendWindow
        if window ≤ 0 then
          set { st with streams := st.streams.insert id { s with sendWaiter := some waiter } }
          return .ok none
        let n := min remaining (min window.toNat st.peerMaxFrameSize)
        let last := endStream && n == remaining
        set { st with
          connSendWindow := st.connSendWindow - n
          streams := (st.streams.insert id
            { s with sendWindow := s.sendWindow - n, localDone := last }) }
        return .ok (some n)
    match grant with
    | .error why => throw (goneError why)
    | .ok none =>
      unless ← waitFor waiter c.config.timeoutMillis do
        throw (goneError s!"no flow-control window within {c.config.timeoutMillis}ms")
    | .ok (some n) =>
      let last := endStream && offset + n == data.size
      c.send (encodeFrame (buildDataFrame sid (data.extract offset (offset + n)) last))
      offset := offset + n
      if last || (!endStream && offset == data.size) then sentEnd := true

/-- The stream's handler is done: end a response it left open, stop a
    request body it did not wait for (RST_STREAM NO_ERROR, §8.1), and retire
    the stream once both directions are finished. -/
def finishStream (c : Conn) (id : Nat) : IO Unit := do
  let s? ← c.state.atomically do return (← get).streams[id]?
  let some s := s? | return
  if s.reset then return
  if !s.headersSent then
    try c.sendHeaders id 500 [("content-length", "0")] true catch _ => pure ()
  else if !s.localDone then
    try c.sendData id ByteArray.empty true catch _ => pure ()
  let s? ← c.state.atomically do return (← get).streams[id]?
  let some s := s? | return
  if s.remoteDone then
    c.state.atomically (modify (·.retire id false))
  else
    c.resetStream id .noError

/-- Start the handler for a stream whose request head is complete. -/
def startHandler (c : Conn) (handler : Handler) (req : Request)
    (receivedBody : Option (IO ByteArray) := none) : IO Unit := do
  let id := req.streamId
  let body := match receivedBody with
    | none => c.readBody id
    | some read => do
      let st ← c.state.atomically get
      if st.isClosed || !st.streams.contains id then throw (goneError "stream closed")
      read
  let req := { req with body }
  let responder : Responder := {
    respond := fun status headers endStream => c.sendHeaders id status headers endStream
    write := fun bytes => if bytes.isEmpty then pure () else c.sendData id bytes false
    finish := c.sendData id ByteArray.empty true }
  let _ ← Control.Concurrent.forkGreen do
    try
      handler req responder
    catch _ =>
      let s? ← (c.state.atomically do return (← get).streams[id]? : IO _)
      if let some s := s? then
        if s.headersSent then (c.resetStream id .internalError : IO _)
    (c.finishStream id : IO _)

end Conn

-- ── The reader ────────────────────────────────────────────────────

/-- Why the reader stops. -/
inductive Stop where
  /-- The peer closed, or went idle past the transport's timeout with no
      stream open. -/
  | quiet
  /-- A connection error: GOAWAY with this code. -/
  | error (code : ErrorCode) (message : String)

/-- The reader's own state: its input buffer, HPACK decoder, and a header
    block being assembled. -/
structure ReaderState where
  buffer : ByteArray := ByteArray.empty
  pos : Nat := 0
  decoder : HPACK.DynamicTable := HPACK.DynamicTable.empty localHeaderTableSize
  /-- (stream, END_STREAM on its HEADERS, fragments) while CONTINUATION is due. -/
  assembling : Option (Nat × Bool × ByteArray) := none

private abbrev ReaderM := StateT ReaderState (ExceptT Stop Green)

private def stop (s : Stop) : ReaderM α := throw s

private def protocolError (msg : String) : ReaderM α := stop (.error .protocolError msg)

/-- Read exactly `n` bytes from the transport. An idle timeout with no open
    stream ends the connection quietly; with streams open, it keeps waiting
    (their handlers have their own timeouts). -/
private def readBytes (c : Conn) (n : Nat) : ReaderM ByteArray := do
  repeat
    let rs ← get
    if rs.buffer.size - rs.pos ≥ n then
      set { rs with pos := rs.pos + n }
      return rs.buffer.extract rs.pos (rs.pos + n)
    match ← (c.transport.recv : Green _) with
    | some chunk =>
      if chunk.isEmpty then stop .quiet
      let rs ← get
      set { rs with buffer := rs.buffer.extract rs.pos rs.buffer.size ++ chunk, pos := 0 }
    | none =>
      let st ← (c.state.atomically get : IO _)
      if st.streams.isEmpty then stop .quiet
  return ByteArray.empty

/-- Validate peer settings (§6.5.2), shared by SETTINGS frames and the
    HTTP2-Settings upgrade header. Unknown settings are ignored. -/
def validatePeerSettings (params : List (SettingsKeyId × UInt32)) :
    Except (ErrorCode × String) Unit := do
  for (key, value) in params do
    match key with
    | .enablePush => if value > 1 then throw (.protocolError, "SETTINGS_ENABLE_PUSH not 0 or 1")
    | .initialWindowSize =>
      if value.toNat > maxWindowSize.toNat then
        throw (.flowControlError, "SETTINGS_INITIAL_WINDOW_SIZE above 2^31-1")
    | .maxFrameSize =>
      if value.toNat < minMaxFrameSize.toNat || value.toNat > maxMaxFrameSize.toNat then
        throw (.protocolError, "SETTINGS_MAX_FRAME_SIZE out of range")
    | _ => pure ()

/-- Apply peer settings, acknowledging a frame but not an upgrade header:
    the 101 response implicitly acknowledges HTTP2-Settings (§3.2.1). -/
private def applyPeerSettings (c : Conn) (params : List (SettingsKeyId × UInt32))
    (acknowledge : Bool := true) : ReaderM Unit := do
  match validatePeerSettings params with
  | .error (code, message) => stop (.error code message)
  | .ok () => pure ()
  let overflow ← (c.state.atomically do
    let mut st ← get
    let mut overflow := false
    for (key, value) in params do
      match key with
      | .initialWindowSize =>
        -- §6.9.2: the change applies to every open stream's send window.
        let delta : Int := (value.toNat : Int) - (st.peerInitialWindow : Int)
        let mut streams := st.streams
        for (id, s) in st.streams.toList do
          let w := s.sendWindow + delta
          if w > (maxWindowSize.toNat : Int) then overflow := true
          streams := streams.insert id { s with sendWindow := w }
        st := { st with streams, peerInitialWindow := value.toNat }
      | .maxFrameSize => st := { st with peerMaxFrameSize := value.toNat }
      | _ => pure ()
    set st
    -- Window may have opened.
    for (_, s) in st.streams.toList do
      if let some p := s.sendWaiter then p.resolve ()
    return overflow : IO _)
  if overflow then stop (.error .flowControlError "SETTINGS_INITIAL_WINDOW_SIZE overflows a window")
  if acknowledge then (c.sendQuietly (encodeFrame (buildSettingsFrame [] true)) : IO _)

/-- A stream error: reset the stream, keep the connection. -/
private def streamError (c : Conn) (id : Nat) (code : ErrorCode) : ReaderM Unit := do
  let known ← (c.state.atomically do return (← get).streams.contains id : IO _)
  if known then (c.resetStream id code : IO _)
  else
    (c.sendQuietly (encodeFrame (buildRstStreamFrame (StreamId.fromWire id.toUInt32) code)) : IO _)

/-- Where a stream id stands, for frames that name one. -/
private inductive StreamStatus where
  | active (slot : StreamSlot)
  /-- Closed by a reset from the peer: frames on it are a stream error. -/
  | resetByPeer
  /-- Closed by END_STREAM both ways (or skipped over): frames on it are a
      connection error of type STREAM_CLOSED (§5.1). -/
  | closedNormally
  /-- Closed by a reset from us: in-flight frames are ignored. -/
  | resetByUs
  | idle

private def streamStatus (c : Conn) (id : Nat) : ReaderM StreamStatus := do
  let st ← (c.state.atomically get : IO _)
  if let some s := st.streams[id]? then
    -- END_STREAM both ways closes the stream, even before its handler has
    -- retired the slot.
    if s.remoteDone && s.localDone && !s.reset then return .closedNormally
    return .active s
  if st.resetByUs.contains id then return .resetByUs
  if st.resetByPeer.contains id then return .resetByPeer
  if st.closed.contains id then return .closedNormally
  if id ≤ st.lastClientStream then return .closedNormally  -- skipped: implicitly closed
  return .idle

/-- A frame other than PRIORITY, WINDOW_UPDATE or RST_STREAM on a closed
    stream (§5.1, "closed"). -/
private def onClosedStream (c : Conn) (id : Nat) (status : StreamStatus) : ReaderM Unit :=
  match status with
  | .resetByPeer => streamError c id .streamClosed
  | _ => stop (.error .streamClosed s!"frame on closed stream {id}")

/-- A complete header block for `id`: a new request, or trailers. -/
private def headerBlock (c : Conn) (handler : Handler) (id : Nat) (endStream : Bool)
    (block : ByteArray) : ReaderM Unit := do
  let rs ← get
  let some (fields, decoder) := HPACK.decodeHeaders rs.decoder block localHeaderTableSize
    | stop (.error .compressionError "HPACK decoding failed")
  set { rs with decoder }
  match ← streamStatus c id with
  | .active s =>
    -- Trailers: they must end the stream and carry no pseudo-headers.
    if s.remoteDone then streamError c id .streamClosed
    else if !endStream || fields.any (·.1.startsWith ":") then streamError c id .protocolError
    else
      let bad ← (c.state.atomically do
        let st ← get
        match st.streams[id]? with
        | none => return false
        | some s =>
          let bad := s.contentLength.any (· != s.received)
          set { st with streams := st.streams.insert id { s with remoteDone := true } }
          if let some p := s.bodyWaiter then p.resolve ()
          return bad : IO _)
      if bad then streamError c id .protocolError
  | .resetByUs => pure ()
  | .resetByPeer => onClosedStream c id .resetByPeer
  | .closedNormally => onClosedStream c id .closedNormally
  | .idle =>
    let st ← (c.state.atomically get : IO _)
    if st.goingAway then return
    (c.state.atomically (modify fun st => { st with lastClientStream := id }) : IO _)
    if st.streams.size ≥ c.config.maxConcurrentStreams then
      (c.state.atomically (modify (·.retire id true)) : IO _)
      streamError c id .refusedStream
      return
    let slot : StreamSlot := { recvWindow := c.config.initialWindowSize,
                               sendWindow := st.peerInitialWindow, remoteDone := endStream }
    if headerListSize fields > c.config.maxHeaderListSize then
      (c.state.atomically (modify fun st => { st with streams := st.streams.insert id slot }) : IO _)
      (do c.sendHeaders id 431 [("content-length", "0")] true; c.finishStream id : IO _)
      return
    match parseRequestHead id fields with
    | .error _ =>
      (c.state.atomically (modify (·.retire id true)) : IO _)
      streamError c id .protocolError
    | .ok req =>
      if endStream && req.contentLength.any (· != 0) then
        (c.state.atomically (modify (·.retire id true)) : IO _)
        streamError c id .protocolError
      else
        (c.state.atomically (modify fun st => { st with streams := (st.streams.insert id
          { slot with contentLength := req.contentLength }) }) : IO _)
        (c.startHandler handler req : IO _)

/-- A DATA frame's payload for stream `id` (`flowLength` counts padding). -/
private def dataFrame (c : Conn) (id : Nat) (content : ByteArray) (flowLength : Nat)
    (endStream : Bool) : ReaderM Unit := do
  -- Connection-level flow control first: every DATA byte counts.
  let connOk ← (c.state.atomically do
    let st ← get
    if (flowLength : Int) > st.connRecvWindow then return false
    set { st with connRecvWindow := st.connRecvWindow - flowLength }
    return true : IO _)
  unless connOk do stop (.error .flowControlError "DATA beyond the connection window")
  match ← streamStatus c id with
  | .idle => protocolError "DATA on an idle stream"
  | .resetByUs => (c.returnWindow id flowLength : IO _)
  | status@.resetByPeer | status@.closedNormally =>
    (c.returnWindow id flowLength : IO _)
    onClosedStream c id status
  | .active s =>
    if s.remoteDone then
      (c.returnWindow id flowLength : IO _)
      streamError c id .streamClosed
      return
    if (flowLength : Int) > s.recvWindow then
      (c.returnWindow id flowLength : IO _)
      streamError c id .flowControlError
      return
    let received := s.received + content.size
    let tooLong := s.contentLength.any (received > ·)
    let tooShort := endStream && s.contentLength.any (· != received)
    if tooLong || tooShort then
      (c.returnWindow id flowLength : IO _)
      streamError c id .protocolError
      return
    -- Deliver, and take the handler's waiter, in one step.
    let waiter ← (c.state.atomically do
      let st ← get
      match st.streams[id]? with
      | none => return none
      | some s =>
        set { st with streams := st.streams.insert id { s with
          inbox := if content.isEmpty then s.inbox else s.inbox.push content
          received, remoteDone := endStream, recvWindow := s.recvWindow - flowLength,
          bodyWaiter := none } }
        return s.bodyWaiter : IO _)
    if let some p := waiter then (p.resolve () : IO _)
    -- Padding is flow-controlled but never read: return it now.
    if flowLength > content.size then (c.returnWindow id (flowLength - content.size) : IO _)
    -- A stream whose handler already finished, now complete: retire it.
    let st ← (c.state.atomically get : IO _)
    if let some s := st.streams[id]? then
      if s.remoteDone && s.localDone then
        (c.state.atomically (modify (·.retire id false)) : IO _)

/-- Process one frame. -/
private def frame (c : Conn) (handler : Handler) (h : FrameHeader) (payload : ByteArray) :
    ReaderM Unit := do
  let id := h.streamId.val.toNat
  let flag (f : FrameFlags) := FrameFlags.test h.flags f
  -- While a header block is open, only its CONTINUATION may follow (§6.10).
  if let some (sid, endStream, fragments) := (← get).assembling then
    unless h.frameType == .continuation && id == sid do
      protocolError "a header block was interrupted"
    let fragments := fragments ++ payload
    if fragments.size > 2 * c.config.maxHeaderListSize + 16384 then
      stop (.error .enhanceYourCalm "header block too large")
    if flag FrameFlags.endHeaders then
      modify fun rs => { rs with assembling := none }
      headerBlock c handler sid endStream fragments
    else
      modify fun rs => { rs with assembling := some (sid, endStream, fragments) }
    return
  match h.frameType with
  | .data =>
    if id == 0 then protocolError "DATA on stream 0"
    let content ← if flag FrameFlags.padded then
        match decodePadding payload with
        | some (content, _) => pure content
        | none => protocolError "DATA padding exceeds the payload"
      else pure payload
    dataFrame c id content payload.size (flag FrameFlags.endStream)
  | .headers =>
    if id == 0 then protocolError "HEADERS on stream 0"
    if id % 2 == 0 then protocolError "HEADERS on a server-initiated stream id"
    let mut block := payload
    if flag FrameFlags.padded then
      match decodePadding block with
      | some (content, _) => block := content
      | none => protocolError "HEADERS padding exceeds the payload"
    if flag FrameFlags.priority then
      match decodePriority block with
      | some (_, dep, _) =>
        if dep.val.toNat == id then
          -- §5.3.1: a stream cannot depend on itself; the block must still
          -- be decoded to keep HPACK in step.
          block := block.extract 5 block.size
          if flag FrameFlags.endHeaders then
            let rs ← get
            match HPACK.decodeHeaders rs.decoder block localHeaderTableSize with
            | some (_, decoder) => set { rs with decoder }
            | none => stop (.error .compressionError "HPACK decoding failed")
          (c.state.atomically (modify fun st =>
            if id > st.lastClientStream then { st with lastClientStream := id } else st) : IO _)
          streamError c id .protocolError
          return
        block := block.extract 5 block.size
      | none => protocolError "HEADERS priority field truncated"
    -- A new stream must have a higher id than any before (§5.1.1).
    if (← streamStatus c id) matches .closedNormally then
      let st ← (c.state.atomically get : IO _)
      unless st.closed.contains id do
        protocolError "HEADERS on a stream id lower than one already used"
    if flag FrameFlags.endHeaders then
      headerBlock c handler id (flag FrameFlags.endStream) block
    else
      modify fun rs => { rs with assembling := some (id, flag FrameFlags.endStream, block) }
  | .priority =>
    if id == 0 then protocolError "PRIORITY on stream 0"
    if payload.size != 5 then streamError c id .frameSizeError
    else if let some (_, dep, _) := decodePriority payload then
      if dep.val.toNat == id then streamError c id .protocolError
  | .rstStream =>
    if id == 0 then protocolError "RST_STREAM on stream 0"
    match ← streamStatus c id with
    | .idle => protocolError "RST_STREAM on an idle stream"
    | .active s =>
      let tooMany ← (c.state.atomically do
        let st ← get
        let unread := s.inbox.foldl (init := 0) fun acc b => acc + b.size
        let st := { (st.retire id false (byPeer := true)) with
          connUnacked := st.connUnacked + unread, clientResets := st.clientResets + 1 }
        set st
        return decide (st.clientResets > c.config.maxClientResets) : IO _)
      if let some p := s.bodyWaiter then (p.resolve () : IO _)
      if let some p := s.sendWaiter then (p.resolve () : IO _)
      -- Mark reset for a handler still holding the slot's id.
      if tooMany then stop (.error .enhanceYourCalm "too many stream resets")
    | _ => pure ()
  | .settings =>
    if id != 0 then protocolError "SETTINGS on a stream"
    unless flag FrameFlags.ack do
      match decodeSettingsPayload payload with
      | some params => applyPeerSettings c params
      | none => stop (.error .frameSizeError "SETTINGS length not a multiple of 6")
  | .pushPromise => protocolError "PUSH_PROMISE from a client"
  | .ping =>
    if id != 0 then protocolError "PING on a stream"
    unless flag FrameFlags.ack do
      (c.sendQuietly (encodeFrame (buildPingFrame payload true)) : IO _)
  | .goaway =>
    if id != 0 then protocolError "GOAWAY on a stream"
    (c.state.atomically (modify fun st => { st with goingAway := true }) : IO _)
  | .windowUpdate =>
    let some inc := decodeWindowUpdate payload | stop (.error .frameSizeError "WINDOW_UPDATE length")
    if id == 0 then
      if inc == 0 then protocolError "WINDOW_UPDATE of 0 on the connection"
      let ok ← (c.state.atomically do
        let st ← get
        let w := st.connSendWindow + inc.toNat
        if w > (maxWindowSize.toNat : Int) then return false
        set { st with connSendWindow := w }
        for (_, s) in st.streams.toList do
          if let some p := s.sendWaiter then p.resolve ()
        return true : IO _)
      unless ok do stop (.error .flowControlError "connection window above 2^31-1")
    else
      match ← streamStatus c id with
      | .idle => protocolError "WINDOW_UPDATE on an idle stream"
      | .active _ =>
        if inc == 0 then streamError c id .protocolError
        else
          -- Read-modify-write under the lock: a handler's send may be
          -- spending the same window concurrently.
          let outcome ← (c.state.atomically do
            let st ← get
            match st.streams[id]? with
            | none => return (true, none)
            | some s =>
              let w := s.sendWindow + inc.toNat
              if w > (maxWindowSize.toNat : Int) then return (false, none)
              set { st with streams := st.streams.insert id { s with sendWindow := w } }
              return (true, s.sendWaiter) : IO _)
          match outcome with
          | (false, _) => streamError c id .flowControlError
          | (true, waiter) => if let some p := waiter then (p.resolve () : IO _)
      | _ => pure ()  -- a stream that just closed: window no longer matters
  | .continuation => protocolError "CONTINUATION without a header block"
  | .unknown _ => pure ()  -- §4.1: ignored

/-- The reader loop. -/
private def readLoop (c : Conn) (handler : Handler) (upgrade : Option UpgradeRequest) : ReaderM Unit := do
  if let some initial := upgrade then
    applyPeerSettings c initial.settings false
    let st ← (c.state.atomically get : IO _)
    let req := { initial.request with streamId := 1 }
    let slot : StreamSlot := {
      recvWindow := c.config.initialWindowSize
      sendWindow := st.peerInitialWindow, remoteDone := true, contentLength := req.contentLength }
    (c.state.atomically (modify fun st => { st with
      lastClientStream := 1, streams := st.streams.insert 1 slot }) : IO _)
    (c.startHandler handler req (some req.body) : IO _)
  let preface ← readBytes c connectionPrefaceLength
  if preface != connectionPreface then protocolError "invalid connection preface"
  let mut first := true
  repeat
    let headerBytes ← readBytes c frameHeaderSize
    let some h := decodeFrameHeader headerBytes | protocolError "invalid frame header"
    let len := h.payloadLength.toNat
    if len > localMaxFrameSize then
      stop (.error .frameSizeError s!"frame of {len} bytes, above SETTINGS_MAX_FRAME_SIZE")
    let payload ← readBytes c len
    if first then
      -- §3.4: the client preface ends with a SETTINGS frame.
      unless h.frameType == .settings && !FrameFlags.test h.flags FrameFlags.ack do
        protocolError "the client preface did not end with SETTINGS"
      first := false
    -- Fixed lengths (§6): a connection error, except PRIORITY (stream).
    let fixedLengthBad : Bool := match h.frameType with
      | .ping => len != 8
      | .rstStream => len != 4
      | .windowUpdate => len != 4
      | .settings => FrameFlags.test h.flags FrameFlags.ack && len != 0
      | _ => false
    if fixedLengthBad then stop (.error .frameSizeError "wrong length for the frame type")
    frame c handler h payload

/-- Serve one HTTP/2 connection whose preface is next on `transport`: send
    our SETTINGS, run the reader until the peer closes, goes idle, or commits
    a connection error (answered with GOAWAY), then close. Handlers run
    concurrently; the transport is not touched after this returns. For h2c
    Upgrade, `upgrade` seeds half-closed stream 1 with the completed HTTP/1.1
    request and its peer settings; the caller sends 101 first. -/
def serve (transport : Transport) (handler : Handler) (config : ServerConfig := {})
    (upgrade : Option UpgradeRequest := none) :
    Green Unit := do
  let state ← (Std.Mutex.new ({ connRecvWindow := config.connectionWindowSize } : ConnState) : IO _)
  let writeLock ← (Std.Mutex.new () : IO _)
  let c : Conn := { config, transport, state, writeLock }
  let settings := buildSettingsFrame [
    (.maxConcurrentStreams, config.maxConcurrentStreams.toUInt32),
    (.initialWindowSize, config.initialWindowSize.toUInt32),
    (.maxHeaderListSize, config.maxHeaderListSize.toUInt32),
    (.enablePush, 0)]
  let raise := config.connectionWindowSize - 65535
  let opening := encodeFrame settings ++
    (if raise > 0 then encodeFrame (buildWindowUpdateFrame StreamId.zero raise.toUInt32)
     else ByteArray.empty)
  try
    try (c.send opening : IO _) catch _ => return
    let outcome ← (readLoop c handler upgrade).run {} |>.run
    let lastId := (← (c.state.atomically get : IO _)).lastClientStream
    match outcome with
    | .error (.error code message) =>
      (c.sendQuietly (encodeFrame
        (buildGoawayFrame (StreamId.fromWire lastId.toUInt32) code message.toUTF8)) : IO _)
    | .error .quiet =>
      (c.sendQuietly (encodeFrame
        (buildGoawayFrame (StreamId.fromWire lastId.toUInt32) .noError ByteArray.empty)) : IO _)
    | .ok _ => pure ()
  finally
    -- Transport exceptions and cancellation must wake handlers and prohibit
    -- writes too, before the caller releases the socket/TLS session.
    (c.close : IO _)

-- ── The historical entry point ────────────────────────────────────

/-- Serve a connection with a handler that answers each request's fields
    with response fields (including `:status`) and a body, read and written
    in full. Kept for callers of the earlier API; `serve` streams both ways.
    $$\text{runHTTP2Connection} : \text{IO ByteArray} \to (\text{ByteArray} \to \text{IO Unit}) \to \dots \to \text{IO}(\text{Except}(\text{ConnectionError}, \text{Unit}))$$ -/
def runHTTP2Connection
    (recv : IO ByteArray)
    (send : ByteArray → IO Unit)
    (onRequest : List (String × String) → StreamId → IO (List (String × String) × ByteArray)) :
    IO (Except ConnectionError Unit) := do
  let transport : Transport := { recv := do return some (← (recv : IO _)), send }
  let handler : Handler := fun req responder => do
    let mut more := true
    while more do more := !(← (req.body : IO _)).isEmpty
    let fields := [(":method", req.method), (":path", req.path)] ++
      (req.scheme.map (":scheme", ·)).toList ++ (req.authority.map (":authority", ·)).toList ++
      req.headers
    let (respFields, body) ← (onRequest fields (StreamId.fromWire req.streamId.toUInt32) : IO _)
    let status := ((respFields.lookup ":status").bind String.toNat?).getD 200
    let rest := respFields.filter (·.1 != ":status")
    (responder.respond status rest body.isEmpty : IO _)
    unless body.isEmpty do
      (responder.write body : IO _)
      (responder.finish : IO _)
  try
    Control.Concurrent.Green.Green.block (serve transport handler) (← Std.CancellationToken.new)
    return .ok ()
  catch e =>
    return .error { errorCode := .internalError, message := toString e }

end Network.HTTP2
