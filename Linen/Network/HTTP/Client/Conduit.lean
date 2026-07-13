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

  ## `applyBearerAuth`/`applyBasicAuth`
  Upstream's `http-client` package defines these two request-mutating
  helpers (re-exported, among others, by `Network.HTTP.Client.Conduit`);
  `linen` had no prior port of either. Added here — the module `hoauth2`'s
  `Network.OAuth2.HttpClient`/`Network.OAuth2.TokenRequest` actually import
  them from — as minimal, direct header-prepending functions rather than
  pulling in a wider slice of `http-client`'s own request-building API
  (see `docs/imports/hoauth2/dependencies.md`).
-/

import Linen.Network.HTTP.Client.Types
import Linen.Network.HTTP.Client.Connection
import Linen.Network.HTTP.Client.Request
import Linen.Network.HTTP.Client.Response
import Linen.Data.Conduit.Internal.Conduit
import Linen.Data.Base64

namespace Network.HTTP.Client.Conduit

open Network.HTTP.Client
open Network.HTTP.Types
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

-- ── Authentication header helpers ──

/-- Prepend an `Authorization: Bearer <token>` header (`http-client`'s
    `applyBearerAuth`).

    $$\text{applyBearerAuth} : \text{String} \to \text{Request} \to \text{Request}$$ -/
def applyBearerAuth (token : String) (req : Request) : Request :=
  { req with headers := (hAuthorization, s!"Bearer {token}") :: req.headers }

/-- Prepend an `Authorization: Basic <base64(user:pass)>` header
    (`http-client`'s `applyBasicAuth`).

    $$\text{applyBasicAuth} : \text{String} \to \text{String} \to \text{Request} \to \text{Request}$$ -/
def applyBasicAuth (user pass : String) (req : Request) : Request :=
  { req with
      headers := (hAuthorization, s!"Basic {Data.Base64.encode s!"{user}:{pass}".toUTF8}") :: req.headers }

end Network.HTTP.Client.Conduit
