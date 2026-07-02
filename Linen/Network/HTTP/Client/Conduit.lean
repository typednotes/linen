/-
  Linen.Network.HTTP.Client.Conduit — Conduit integration for HTTP client

  Bridges `Network.HTTP.Client` and `Data.Conduit`, providing streaming
  HTTP response bodies as conduit sources.

  ## Haskell equivalent
  `Network.HTTP.Client.Conduit` from the `http-conduit` package.

  ## Why `unsafe`
  `httpSource`/`httpSink` build on `ConduitT`, which is `unsafe` throughout
  this library (see `Linen.Data.Conduit.Internal.Conduit`) — there is no
  extra unboundedness introduced here beyond what that layer already
  carries.
-/

import Linen.Network.HTTP.Client.Types
import Linen.Network.HTTP.Client.Connection
import Linen.Network.HTTP.Client.Request
import Linen.Network.HTTP.Client.Response
import Linen.Data.Conduit.Internal.Conduit

namespace Network.HTTP.Client.Conduit

open Network.HTTP.Client
open Data.Conduit

/-- Execute an HTTP request and stream the response body as a conduit source.
    The connection is established, the request is sent, headers are read,
    and the body bytes are yielded in chunks.

    $$\text{httpSource} : \text{Request} \to \text{ConduitT}\ i\ \text{ByteArray}\ \text{IO}\ \text{Response}$$

    Returns the `Response` (with empty body field) after all body bytes
    have been yielded downstream. -/
unsafe def httpSource (req : Request) : ConduitT i ByteArray IO Response := do
  let conn ← liftConduit (connect req.host (if req.port == 0 then defaultPort req.isSecure else req.port) req.isSecure)
  liftConduit (sendRequest conn req)
  -- Read status line and headers
  let resp ← liftConduit (receiveResponse conn)
  -- Yield the body as a single chunk (receiveResponse already reads full body)
  if !resp.body.isEmpty then
    yield resp.body
  liftConduit conn.connClose
  pure { resp with body := ByteArray.empty }

/-- Execute an HTTP request with a callback that processes the response.
    The connection is automatically closed when the callback returns.

    $$\text{withResponse} : \text{Request} \to (\text{Response} \to \text{IO}\ \alpha) \to \text{IO}\ \alpha$$ -/
def withResponse (req : Request) (f : Response → IO α) : IO α := do
  let conn ← connect req.host (if req.port == 0 then defaultPort req.isSecure else req.port) req.isSecure
  try
    let resp ← performRequest conn req
    f resp
  finally
    conn.connClose

/-- Execute an HTTP request and collect the full response body as a ByteArray,
    using conduit for composition.

    $$\text{httpSink} : \text{Request} \to \text{ConduitT}\ i\ o\ \text{IO}\ (\text{Response} \times \text{ByteArray})$$ -/
unsafe def httpSink (req : Request) : ConduitT i o IO (Response × ByteArray) := do
  let conn ← liftConduit (connect req.host (if req.port == 0 then defaultPort req.isSecure else req.port) req.isSecure)
  let resp ← liftConduit (performRequest conn req)
  liftConduit conn.connClose
  pure (resp, resp.body)

end Network.HTTP.Client.Conduit
