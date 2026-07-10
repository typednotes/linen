/-
  Linen.Network.WebApp.Extra.Middleware.RealIp — extract the real client IP
  from proxy headers

  Updates `remoteHost` based on `X-Forwarded-For`/`X-Real-IP` headers. Ports
   `Network.Wai.Middleware.RealIp`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types
open Data (CI)

/-- Update the request's `remoteHost` from `X-Forwarded-For` or `X-Real-IP`
    headers. `X-Forwarded-For` takes precedence; uses the leftmost (client)
    IP.
    $$\text{realIp} : \text{Middleware}$$ -/
def realIp : Middleware :=
  fun app req respond =>
    let xff := req.requestHeaders.find? (fun (n, _) => n == xForwardedFor) |>.map (·.2)
    let xri := req.requestHeaders.find? (fun (n, _) => n == xRealIp) |>.map (·.2)
    let clientIp := xff.bind (fun s => s.splitOn "," |>.head? |>.map (String.trimAscii · |>.toString))
      |>.orElse (fun _ => xri)
    match clientIp with
    | some ip => app { req with remoteHost := ⟨ip, req.remoteHost.port⟩ } respond
    | none => app req respond
where
  xForwardedFor := CI.mk' "X-Forwarded-For"
  xRealIp := CI.mk' "X-Real-IP"

end Network.WebApp.Extra.Middleware
