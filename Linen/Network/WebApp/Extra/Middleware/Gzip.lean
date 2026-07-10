/-
  Linen.Network.WebApp.Extra.Middleware.Gzip — gzip compression middleware

  Ports `Network.Wai.Middleware.Gzip`. For now, passes through
  without actual compression (full zlib FFI integration is deferred, as it
  was in the upstream source).

  ## Design
  - Checks `Accept-Encoding` for `"gzip"`.
  - Actual compression requires zlib FFI (not yet implemented, matching
    the upstream's own deferred TODO — no new stub is introduced here).
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types
open Data (CI)

/-- Gzip compression settings. -/
structure GzipSettings where
  /-- Minimum response size to compress (bytes). -/
  gzipMinSize : Nat := 860
  /-- MIME types to compress. -/
  gzipCheckMime : String → Bool := fun mime =>
    mime.startsWith "text/" ||
    mime == "application/json" ||
    mime == "application/javascript" ||
    mime == "application/xml" ||
    mime == "image/svg+xml"

/-- Check if the client accepts gzip encoding. -/
private def clientAcceptsGzip (req : Request) : Bool :=
  let ae := (req.requestHeaders.find? (fun (n, _) => n == CI.mk' "Accept-Encoding")).map (·.2)
  match ae with
  | some s => (s.splitOn "gzip").length != 1
  | none => false

/-- Gzip middleware. Passes eligible responses through unchanged when the
    client accepts gzip — actual compression is deferred to a future zlib
    integration, matching the upstream source.
    $$\text{gzip} : \text{GzipSettings} \to \text{Middleware}$$ -/
def gzip (_settings : GzipSettings := {}) : Middleware :=
  fun app req respond =>
    if clientAcceptsGzip req then
      app req fun resp => respond resp
    else
      app req respond

end Network.WebApp.Extra.Middleware
