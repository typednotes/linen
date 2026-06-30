/-
  Linen.Network.HTTP2.Server — HTTP/2 server connection handler

  Implements the HTTP/2 server-side connection lifecycle: connection preface
  validation, settings exchange, and the main frame processing loop.

  ## Design

  `runHTTP2Connection` is the main entry point. It takes IO callbacks for
  reading/writing bytes and a request handler, then manages the full HTTP/2
  protocol state machine.

  The implementation is structured as a loop that reads frames and dispatches
  by frame type. Header blocks spanning HEADERS + CONTINUATION frames are
  tracked via `HeaderBlockState`.

  ## Guarantees

  - Connection preface is validated before processing frames
  - Settings ACK is sent in response to SETTINGS frames
  - PING is echoed with ACK flag
  - GOAWAY is sent on protocol errors with appropriate error codes
  - Header block assembly tracks CONTINUATION state correctly
  - Frame size is validated against current settings

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
import Linen.Network.HTTP2.Stream
import Linen.Network.HTTP2.FlowControl

namespace Network.HTTP2

/-- Mutable state for an HTTP/2 connection. -/
structure ConnectionState where
  /-- Local settings (what we advertise). -/
  localSettings : Settings
  /-- Remote peer's settings. -/
  peerSettings : Settings
  /-- Whether we have received the peer's initial SETTINGS. -/
  peerSettingsReceived : Bool
  /-- Stream table. -/
  streams : StreamTable
  /-- Connection-level flow control. -/
  flowControl : ConnectionFlowControl
  /-- HPACK decoder dynamic table. -/
  decoderTable : HPACK.DynamicTable
  /-- HPACK encoder dynamic table. -/
  encoderTable : HPACK.DynamicTable
  /-- Header block assembly state. -/
  headerBlockState : HeaderBlockState
  /-- Whether a GOAWAY has been sent or received. -/
  goawayReceived : Bool
  /-- Last stream ID we will process (set when sending GOAWAY). -/
  lastGoodStreamId : StreamId
  deriving Inhabited

namespace ConnectionState

/-- Create initial connection state with default settings. -/
def initial : ConnectionState :=
  { localSettings := Settings.default
    peerSettings := Settings.default
    peerSettingsReceived := false
    streams := StreamTable.empty
    flowControl := ConnectionFlowControl.default
    decoderTable := HPACK.DynamicTable.empty 4096
    encoderTable := HPACK.DynamicTable.empty 4096
    headerBlockState := .idle
    goawayReceived := false
    lastGoodStreamId := StreamId.zero }

end ConnectionState

/-- Send a GOAWAY frame with the given error code and close the connection.
    $$\text{sendGoaway} : (\text{ByteArray} \to \text{IO Unit}) \to \text{StreamId} \to \text{ErrorCode} \to \text{String} \to \text{IO Unit}$$ -/
def sendGoaway (send : ByteArray → IO Unit) (lastStreamId : StreamId)
    (errorCode : ErrorCode) (msg : String := "") : IO Unit := do
  let debugData := msg.toUTF8
  let frame := buildGoawayFrame lastStreamId errorCode debugData
  send (encodeFrame frame)

/-- Send a RST_STREAM frame for a specific stream.
    $$\text{sendRstStream} : (\text{ByteArray} \to \text{IO Unit}) \to \text{StreamId} \to \text{ErrorCode} \to \text{IO Unit}$$ -/
def sendRstStream (send : ByteArray → IO Unit) (streamId : StreamId)
    (errorCode : ErrorCode) : IO Unit := do
  let frame := buildRstStreamFrame streamId errorCode
  send (encodeFrame frame)

/-- Process a received SETTINGS frame (non-ACK).
    Updates the peer settings and sends an ACK.

    $$\text{processSettings} : \text{ConnectionState} \to \text{ByteArray} \to (\text{ByteArray} \to \text{IO Unit}) \to
      \text{IO}(\text{Except}(\text{ConnectionError}, \text{ConnectionState}))$$ -/
def processSettings (state : ConnectionState) (payload : ByteArray)
    (send : ByteArray → IO Unit) : IO (Except ConnectionError ConnectionState) := do
  match decodeSettingsPayload payload with
  | none =>
    return .error { errorCode := .frameSizeError, message := "Invalid SETTINGS payload" }
  | some params =>
    -- Validate settings values
    let valid := params.all fun (k, v) =>
      match k with
      | .enablePush => v == 0 || v == 1
      | .initialWindowSize => v ≤ maxWindowSize
      | .maxFrameSize => v ≥ minMaxFrameSize && v ≤ maxMaxFrameSize
      | _ => true
    if !valid then
      return .error { errorCode := .protocolError, message := "Invalid SETTINGS value" }
    else
      let newPeerSettings := applySettings state.peerSettings params
      -- Send SETTINGS ACK
      let ackFrame := buildSettingsFrame [] true
      send (encodeFrame ackFrame)
      -- Update decoder table size if header table size changed
      let decoderTable := if newPeerSettings.headerTableSize != state.peerSettings.headerTableSize
        then state.decoderTable.resize newPeerSettings.headerTableSize
        else state.decoderTable
      return .ok { state with
        peerSettings := newPeerSettings
        peerSettingsReceived := true
        decoderTable := decoderTable }

/-- Process a received PING frame. Echo back with ACK flag.
    $$\text{processPing} : \text{ByteArray} \to (\text{ByteArray} \to \text{IO Unit}) \to \text{IO Unit}$$ -/
def processPing (payload : ByteArray) (send : ByteArray → IO Unit) : IO Unit := do
  let pong := buildPingFrame payload true
  send (encodeFrame pong)

/-- Process a received WINDOW_UPDATE frame.
    $$\text{processWindowUpdate} : \text{ConnectionState} \to \text{FrameHeader} \to \text{ByteArray} \to
      \text{Except}(\text{ConnectionError} \oplus \text{StreamError}, \text{ConnectionState})$$ -/
def processWindowUpdateFrame (state : ConnectionState) (header : FrameHeader) (payload : ByteArray) :
    Except (ConnectionError ⊕ StreamError) ConnectionState := do
  match decodeWindowUpdate payload with
  | none => .error (.inl { errorCode := .frameSizeError, message := "Invalid WINDOW_UPDATE" })
  | some increment =>
    if increment == 0 then
      if header.streamId.val == 0 then
        .error (.inl { errorCode := .protocolError, message := "WINDOW_UPDATE increment 0 on connection" })
      else
        .error (.inr { streamId := header.streamId, errorCode := .protocolError,
                        message := "WINDOW_UPDATE increment 0" })
    else if header.streamId.val == 0 then
      -- Connection-level
      match state.flowControl.processWindowUpdate increment with
      | .ok fc => .ok { state with flowControl := fc }
      | .error _ => .error (.inl { errorCode := .flowControlError,
                                    message := "Connection window overflow" })
    else
      -- Stream-level
      match state.streams.lookup header.streamId with
      | none => .ok state  -- Ignore for unknown/closed streams
      | some info =>
        match processStreamWindowUpdate info increment with
        | .ok info' => .ok { state with streams := state.streams.upsert info' }
        | .error _ => .error (.inr { streamId := header.streamId, errorCode := .flowControlError,
                                      message := "Stream window overflow" })

/-- Send a response: encode headers with HPACK, split into HEADERS + CONTINUATION
    frames if needed, then send DATA frame with the body.

    $$\text{sendResponse} : \text{ConnectionState} \to (\text{ByteArray} \to \text{IO Unit}) \to
      \text{StreamId} \to \text{List}(\text{HeaderField}) \to \text{ByteArray} \to \text{IO}(\text{ConnectionState})$$ -/
def sendResponse (state : ConnectionState) (send : ByteArray → IO Unit)
    (streamId : StreamId) (headers : List (String × String)) (body : ByteArray) :
    IO ConnectionState := do
  -- Encode headers with HPACK
  let (headerBlock, encoderTable) := HPACK.encodeHeaders state.encoderTable headers
  let maxPayload := state.peerSettings.maxFrameSize.toNat
  -- Split header block if needed
  let chunks := splitHeaderBlock headerBlock maxPayload
  match chunks with
  | [] =>
    -- Empty header block (shouldn't happen, but handle gracefully)
    let frame := buildHeadersFrame streamId ByteArray.empty (endStream := body.size == 0) (endHeaders := true)
    send (encodeFrame frame)
  | [single] =>
    -- Fits in one HEADERS frame
    let frame := buildHeadersFrame streamId single (endStream := body.size == 0) (endHeaders := true)
    send (encodeFrame frame)
  | first :: rest =>
    -- HEADERS + CONTINUATION frames
    let headersFrame := buildHeadersFrame streamId first (endStream := false) (endHeaders := false)
    send (encodeFrame headersFrame)
    let rec sendCont (remaining : List ByteArray) : IO Unit :=
      match remaining with
      | [] => pure ()
      | [last] =>
        let contFrame := buildContinuationFrame streamId last (endHeaders := true)
        send (encodeFrame contFrame)
      | chunk :: rest => do
        let contFrame := buildContinuationFrame streamId chunk (endHeaders := false)
        send (encodeFrame contFrame)
        sendCont rest
    sendCont rest
  -- Send DATA frame with body if non-empty
  if body.size > 0 then
    let dataFrame := buildDataFrame streamId body (endStream := true)
    send (encodeFrame dataFrame)
  return { state with encoderTable := encoderTable }

/-- Read exactly `n` bytes from the recv callback. Returns `none` on EOF/short read.
    $$\text{readExactly} : (\text{IO ByteArray}) \to \text{Nat} \to \text{IO}(\text{Option}(\text{ByteArray}))$$ -/
private def readExactly (recv : IO ByteArray) (n : Nat) : IO (Option ByteArray) := do
  let mut buf := ByteArray.empty
  let mut remaining := n
  -- Driven by EOF (`recv` returning empty) and by `remaining` strictly
  -- decreasing on every non-empty chunk — no fuel counter needed.
  while remaining > 0 do
    let chunk ← recv
    if chunk.size == 0 then
      return none  -- EOF
    let take := min chunk.size remaining
    for i in [:take] do
      buf := buf.push (chunk[i]!)
    remaining := remaining - take
  return some buf

/-- Run an HTTP/2 connection as a server.

    Takes IO callbacks for reading and writing bytes, plus a request handler
    that receives decoded headers and a stream ID and returns response headers
    and a body.

    $$\text{runHTTP2Connection} :
      \text{IO ByteArray} \to
      (\text{ByteArray} \to \text{IO Unit}) \to
      (\text{List}(\text{String} \times \text{String}) \to \text{StreamId} \to \text{IO}(\text{List}(\text{String} \times \text{String}) \times \text{ByteArray})) \to
      \text{IO}(\text{Except}(\text{ConnectionError}, \text{Unit}))$$ -/
def runHTTP2Connection
    (recv : IO ByteArray)
    (send : ByteArray → IO Unit)
    (onRequest : List (String × String) → StreamId → IO (List (String × String) × ByteArray)) :
    IO (Except ConnectionError Unit) := do
  -- Step 1: Read and validate connection preface
  let prefaceResult ← readExactly recv connectionPrefaceLength
  match prefaceResult with
  | none =>
    return .error { errorCode := .protocolError, message := "Failed to read connection preface" }
  | some prefaceBytes =>
    if prefaceBytes != connectionPreface then
      return .error { errorCode := .protocolError, message := "Invalid connection preface" }
    else pure ()

  -- Step 2: Send our initial SETTINGS
  let initialSettings := buildSettingsFrame [
    (.maxConcurrentStreams, 100),
    (.initialWindowSize, 65535),
    (.maxFrameSize, 16384)
  ]
  send (encodeFrame initialSettings)

  -- Step 3: Main frame processing loop, driven by `done` (set on EOF, GOAWAY,
  -- or a protocol error) — the genuine connection-lifecycle condition, like the
  -- socket event loop's `while ← running.get do`. No fuel counter.
  let mut state := ConnectionState.initial
  let mut done := false

  while !done do
    -- Read 9-byte frame header
    let headerResult ← readExactly recv frameHeaderSize
    match headerResult with
    | none =>
      done := true  -- Connection closed
    | some headerBytes =>
      match decodeFrameHeader headerBytes with
      | none =>
        sendGoaway send state.streams.lastClientStreamId .protocolError "Failed to decode frame header"
        done := true
      | some frameHeader =>
        -- Validate frame size
        match validateFrameSize frameHeader state.peerSettings with
        | some errCode =>
          sendGoaway send state.streams.lastClientStreamId errCode "Frame size error"
          done := true
        | none =>
          -- Read payload
          let payloadResult ← readExactly recv frameHeader.payloadLength.toNat
          match payloadResult with
          | none =>
            done := true  -- Connection closed mid-frame
          | some payload =>
            -- Check CONTINUATION state: if assembling, only CONTINUATION is allowed
            if state.headerBlockState.isAssembling then
              match frameHeader.frameType with
              | .continuation =>
                match state.headerBlockState.streamId? with
                | some expectedSid =>
                  if frameHeader.streamId != expectedSid then
                    sendGoaway send state.streams.lastClientStreamId .protocolError
                      "CONTINUATION on wrong stream"
                    done := true
                  else
                    -- Append fragment
                    match state.headerBlockState.appendFragment payload with
                    | none =>
                      sendGoaway send state.streams.lastClientStreamId .internalError
                        "Failed to append CONTINUATION fragment"
                      done := true
                    | some newHBS =>
                      if FrameFlags.test frameHeader.flags FrameFlags.endHeaders then
                        -- Header block complete
                        match newHBS.complete with
                        | none =>
                          sendGoaway send state.streams.lastClientStreamId .internalError
                            "Failed to complete header block"
                          done := true
                        | some (streamId, headerBlock) =>
                          -- Decode headers with HPACK
                          match HPACK.decodeHeaders state.decoderTable headerBlock with
                          | none =>
                            sendGoaway send state.streams.lastClientStreamId .compressionError
                              "HPACK decode error"
                            done := true
                          | some (headers, decoderTable) =>
                            state := { state with
                              decoderTable := decoderTable
                              headerBlockState := .idle }
                            -- Call request handler
                            let (respHeaders, respBody) ← onRequest headers streamId
                            state ← sendResponse state send streamId respHeaders respBody
                      else
                        state := { state with headerBlockState := newHBS }
                | none =>
                  sendGoaway send state.streams.lastClientStreamId .internalError
                    "CONTINUATION without stream"
                  done := true
              | _ =>
                -- Any non-CONTINUATION frame while assembling is a protocol error
                sendGoaway send state.streams.lastClientStreamId .protocolError
                  "Expected CONTINUATION frame"
                done := true
            else
              -- Normal frame dispatch
              match frameHeader.frameType with
              | .data =>
                -- DATA frame: deliver to stream, flow control
                if frameHeader.streamId.val == 0 then
                  sendGoaway send state.streams.lastClientStreamId .protocolError
                    "DATA on stream 0"
                  done := true
                else
                  let dataPayload := if FrameFlags.test frameHeader.flags FrameFlags.padded then
                    match decodePadding payload with
                    | some (content, _) => content
                    | none => payload
                  else payload
                  -- Update flow control
                  state := { state with
                    flowControl := state.flowControl.consumeRecv dataPayload.size }
                  -- If END_STREAM, close the stream
                  if FrameFlags.test frameHeader.flags FrameFlags.endStream then
                    state := { state with
                      streams := state.streams.updateState frameHeader.streamId .halfClosedRemote }

              | .headers =>
                -- HEADERS frame
                if frameHeader.streamId.val == 0 then
                  sendGoaway send state.streams.lastClientStreamId .protocolError
                    "HEADERS on stream 0"
                  done := true
                else
                  -- Open the stream
                  let initWindow := state.peerSettings.initialWindowSize.toNat
                  match state.streams.openClientStream frameHeader.streamId initWindow with
                  | none =>
                    -- Could be an already-open stream (trailers) or invalid
                    pure ()
                  | some newStreams =>
                    state := { state with streams := newStreams }

                  -- Handle padding
                  let mut headerPayload := payload
                  let mut _payloadOffset := 0
                  if FrameFlags.test frameHeader.flags FrameFlags.padded then
                    match decodePadding payload with
                    | some (content, _) => headerPayload := content
                    | none =>
                      sendGoaway send state.streams.lastClientStreamId .protocolError
                        "Invalid padding in HEADERS"
                      done := true
                      headerPayload := ByteArray.empty

                  -- Handle priority
                  if FrameFlags.test frameHeader.flags FrameFlags.priority then
                    match decodePriority headerPayload 0 with
                    | some (excl, dep, weight) =>
                      state := { state with
                        streams := state.streams.updatePriority frameHeader.streamId excl dep weight }
                      headerPayload := headerPayload.extract 5 headerPayload.size
                    | none =>
                      sendGoaway send state.streams.lastClientStreamId .protocolError
                        "Invalid priority in HEADERS"
                      done := true

                  if !done then
                    if FrameFlags.test frameHeader.flags FrameFlags.endHeaders then
                      -- Complete header block in this frame
                      match HPACK.decodeHeaders state.decoderTable headerPayload with
                      | none =>
                        sendGoaway send state.streams.lastClientStreamId .compressionError
                          "HPACK decode error"
                        done := true
                      | some (headers, decoderTable) =>
                        state := { state with decoderTable := decoderTable }
                        if FrameFlags.test frameHeader.flags FrameFlags.endStream then
                          state := { state with
                            streams := state.streams.updateState frameHeader.streamId .halfClosedRemote }
                        -- Call request handler
                        let (respHeaders, respBody) ← onRequest headers frameHeader.streamId
                        state ← sendResponse state send frameHeader.streamId respHeaders respBody
                    else
                      -- Start header block assembly
                      state := { state with
                        headerBlockState := .assembling frameHeader.streamId headerPayload }

              | .priority =>
                -- PRIORITY frame
                if frameHeader.streamId.val == 0 then
                  sendGoaway send state.streams.lastClientStreamId .protocolError
                    "PRIORITY on stream 0"
                  done := true
                else
                  match decodePriority payload 0 with
                  | some (excl, dep, weight) =>
                    state := { state with
                      streams := state.streams.updatePriority frameHeader.streamId excl dep weight }
                  | none =>
                    sendGoaway send state.streams.lastClientStreamId .protocolError
                      "Invalid PRIORITY frame"
                    done := true

              | .rstStream =>
                -- RST_STREAM frame
                if frameHeader.streamId.val == 0 then
                  sendGoaway send state.streams.lastClientStreamId .protocolError
                    "RST_STREAM on stream 0"
                  done := true
                else
                  match decodeRstStream payload with
                  | some _errorCode =>
                    state := { state with
                      streams := state.streams.updateState frameHeader.streamId .resetRemote }
                  | none =>
                    sendGoaway send state.streams.lastClientStreamId .frameSizeError
                      "Invalid RST_STREAM"
                    done := true

              | .settings =>
                -- SETTINGS frame
                if frameHeader.streamId.val != 0 then
                  sendGoaway send state.streams.lastClientStreamId .protocolError
                    "SETTINGS on non-zero stream"
                  done := true
                else if FrameFlags.test frameHeader.flags FrameFlags.ack then
                  -- SETTINGS ACK received, nothing to do
                  pure ()
                else
                  let result ← processSettings state payload send
                  match result with
                  | .ok newState => state := newState
                  | .error err =>
                    sendGoaway send state.streams.lastClientStreamId err.errorCode err.message
                    done := true

              | .pushPromise =>
                -- Server should not receive PUSH_PROMISE from client
                sendGoaway send state.streams.lastClientStreamId .protocolError
                  "Server received PUSH_PROMISE"
                done := true

              | .ping =>
                -- PING frame
                if frameHeader.streamId.val != 0 then
                  sendGoaway send state.streams.lastClientStreamId .protocolError
                    "PING on non-zero stream"
                  done := true
                else if FrameFlags.test frameHeader.flags FrameFlags.ack then
                  -- PING ACK received, nothing to do
                  pure ()
                else
                  processPing payload send

              | .goaway =>
                -- GOAWAY received
                match decodeGoaway payload with
                | some (lastStreamId, _errorCode, _debugData) =>
                  state := { state with
                    goawayReceived := true
                    lastGoodStreamId := lastStreamId }
                  done := true
                | none =>
                  done := true

              | .windowUpdate =>
                -- WINDOW_UPDATE frame
                match processWindowUpdateFrame state frameHeader payload with
                | .ok newState => state := newState
                | .error (.inl connErr) =>
                  sendGoaway send state.streams.lastClientStreamId connErr.errorCode connErr.message
                  done := true
                | .error (.inr streamErr) =>
                  sendRstStream send streamErr.streamId streamErr.errorCode

              | .continuation =>
                -- CONTINUATION without preceding HEADERS is a protocol error
                sendGoaway send state.streams.lastClientStreamId .protocolError
                  "Unexpected CONTINUATION frame"
                done := true

              | .unknown _ =>
                -- Unknown frame types are ignored per RFC 9113 Section 4.1
                pure ()

  return .ok ()

end Network.HTTP2
