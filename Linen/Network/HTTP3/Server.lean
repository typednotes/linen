/-
  Linen.Network.HTTP3.Server -- HTTP/3 server logic

  Provides the HTTP/3 protocol layer on top of QUIC connections.
  Handles HTTP/3 control streams, request/response framing, and QPACK.
  Ports Haskell's `Network.HTTP3.Server` from the `http3` package.

  ## Design

  The HTTP/3 server:
  1. Opens control streams and sends SETTINGS
  2. Accepts request streams from the client
  3. Decodes HEADERS frames using QPACK
  4. Dispatches to a request handler
  5. Encodes response HEADERS + DATA frames

  ## Guarantees

  - SETTINGS frame is sent on the control stream before any request processing
  - Each request stream gets a complete HEADERS + optional DATA response
-/

import Linen.Network.HTTP3.Frame
import Linen.Network.HTTP3.Error
import Linen.Network.HTTP3.QPACK.Encode
import Linen.Network.HTTP3.QPACK.Decode
import Linen.Network.QUIC.Connection
import Linen.Network.QUIC.Stream

namespace Network.HTTP3

/-- An HTTP/3 request with parsed headers and body reader.
    $$\text{H3Request} = \{ \text{method} : \text{String},\; \text{path} : \text{String},\; \ldots \}$$ -/
structure H3Request where
  /-- The HTTP method (from :method pseudo-header). -/
  method : String
  /-- The request path (from :path pseudo-header). -/
  path : String
  /-- The scheme (from :scheme pseudo-header). -/
  scheme : String
  /-- The authority (from :authority pseudo-header). -/
  authority : String
  /-- Regular (non-pseudo) headers. -/
  headers : List (String × String)
  /-- IO action to read the next chunk of the request body.
      Returns empty ByteArray when body is exhausted. -/
  readBody : IO ByteArray
  deriving Inhabited

/-- An HTTP/3 response.
    $$\text{H3Response} = \{ \text{status} : \mathbb{N},\; \text{headers} : \text{List}(\ldots),\; \text{body} : \text{ByteArray} \}$$ -/
structure H3Response where
  /-- The HTTP status code. -/
  status : Nat
  /-- Response headers. -/
  headers : List (String × String)
  /-- Response body. -/
  body : ByteArray

/-- HTTP/3 request handler type.
    $$\text{H3Handler} = \text{H3Request} \to \text{IO}(\text{H3Response})$$ -/
abbrev H3Handler := H3Request → IO H3Response

/-- Extract pseudo-headers from a decoded header list.
    Returns (method, path, scheme, authority, regularHeaders). -/
private def extractPseudoHeaders (headers : List QPACK.HeaderField) :
    Option (String × String × String × String × List (String × String)) :=
  let result := headers.foldl (init := ("", "", "", "", ([] : List (String × String))))
    fun (method, path, scheme, authority, regular) (name, value) =>
      if name == ":method" then (value, path, scheme, authority, regular)
      else if name == ":path" then (method, value, scheme, authority, regular)
      else if name == ":scheme" then (method, path, value, authority, regular)
      else if name == ":authority" then (method, path, scheme, value, regular)
      else (method, path, scheme, authority, regular ++ [(name, value)])
  let (method, path, scheme, authority, regular) := result
  if method.isEmpty || path.isEmpty then none
  else some (method, path, scheme, authority, regular)

/-- Encode an HTTP/3 response as HEADERS + DATA frames on a QUIC stream.
    $$\text{sendResponse} : \text{QUICStream} \to \text{H3Response} \to \text{IO}(\text{Unit})$$ -/
def sendResponse (stream : Network.QUIC.QUICStream) (resp : H3Response) : IO Unit := do
  -- Encode response headers via QPACK
  let responseHeaders : List QPACK.HeaderField :=
    [(":status", toString resp.status)] ++ resp.headers
  let headerBlock := QPACK.encodeHeaders responseHeaders
  -- Send HEADERS frame
  let headersFrame : Frame := { frameType := .headers, payload := headerBlock }
  stream.send headersFrame.encode false
  -- Send DATA frame with body (if non-empty)
  if resp.body.size > 0 then
    let dataFrame : Frame := { frameType := .data, payload := resp.body }
    stream.send dataFrame.encode true
  else
    -- Send empty finish
    stream.send ByteArray.empty true

/-- Handle a single HTTP/3 request stream.
    Reads frames from the stream, parses the request, invokes the handler,
    and sends the response.
    $$\text{handleRequestStream} : \text{QUICStream} \to \text{H3Handler} \to \text{IO}(\text{Unit})$$ -/
def handleRequestStream (stream : Network.QUIC.QUICStream) (handler : H3Handler) : IO Unit := do
  -- Read data from the stream
  let (data, _fin) ← stream.recv 65536
  -- Decode the first frame (should be HEADERS)
  match Frame.decode data with
  | none => throw (IO.userError "HTTP/3: failed to decode frame from request stream")
  | some (frame, consumed) =>
    if frame.frameType != .headers then
      throw (IO.userError "HTTP/3: expected HEADERS frame, got {frame.frameType}")
    -- Decode QPACK header block
    match QPACK.decodeHeaders frame.payload with
    | none => throw (IO.userError "HTTP/3: failed to decode QPACK headers")
    | some headers =>
      match extractPseudoHeaders headers with
      | none => throw (IO.userError "HTTP/3: missing required pseudo-headers")
      | some (method, path, scheme, authority, regularHeaders) =>
        -- Collect body from remaining DATA frames
        let bodyData := data.extract consumed data.size
        let bodyRef ← IO.mkRef bodyData
        let req : H3Request := {
          method, path, scheme, authority,
          headers := regularHeaders,
          readBody := do
            let body ← bodyRef.get
            bodyRef.set ByteArray.empty
            return body
        }
        let resp ← handler req
        sendResponse stream resp

/-- Handle an HTTP/3 connection. Opens control streams, sends SETTINGS,
    and processes request streams.
    $$\text{handleConnection} : \text{Connection} \to \text{H3Settings} \to \text{H3Handler} \to \text{IO}(\text{Unit})$$ -/
def handleConnection (conn : Network.QUIC.Connection) (settings : H3Settings)
    (_handler : H3Handler) : IO Unit := do
  -- Open a unidirectional control stream and send SETTINGS
  let controlStreamId ← conn.openStream false
  let settingsPayload := settings.encode
  let settingsFrame : Frame := { frameType := .settings, payload := settingsPayload }
  conn.sendStream controlStreamId settingsFrame.encode false
  -- Accept and handle request streams
  -- In a full implementation, this would loop accepting new streams from the client.
  -- For now, we demonstrate the structure.
  throw (IO.userError "HTTP/3: handleConnection requires QUIC stream accept (not yet implemented)")

end Network.HTTP3
