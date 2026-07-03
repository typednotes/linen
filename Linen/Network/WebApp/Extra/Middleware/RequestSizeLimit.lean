/-
  Linen.Network.WebApp.Extra.Middleware.RequestSizeLimit — limit request
  body size

  Rejects requests with bodies exceeding a specified size limit. Ports
  Hale's `Network.Wai.Middleware.RequestSizeLimit`.

  ## Guarantees
  - Bodies larger than the limit are never fully read into memory.
  - The wrapped request body reader tracks bytes consumed.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types

/-- Reject requests whose body exceeds `maxBytes`. For known-length bodies,
    checks `Content-Length` before reading. For chunked bodies, wraps the
    body reader with a counting reader.
    $$\text{requestSizeLimit} : \mathbb{N} \to \text{Middleware}$$
    Returns 413 Payload Too Large if the limit is exceeded. -/
def requestSizeLimit (maxBytes : Nat) : Middleware :=
  fun app req respond =>
    match req.requestBodyLength with
    | .knownLength n =>
      if n > maxBytes then
        AppM.respond respond (.responseBuilder status413 [] "Request body too large".toUTF8)
      else
        app req respond
    | .chunkedBody =>
      AppM.ioThen (IO.mkRef 0) fun consumed =>
      let wrappedBody : IO ByteArray := do
        let chunk ← req.requestBody
        if chunk.isEmpty then return chunk
        let total ← consumed.get
        let newTotal := total + chunk.size
        if newTotal > maxBytes then
          throw (IO.Error.userError "Request body too large")
        consumed.set newTotal
        return chunk
      app { req with requestBody := wrappedBody } respond

end Network.WebApp.Extra.Middleware
