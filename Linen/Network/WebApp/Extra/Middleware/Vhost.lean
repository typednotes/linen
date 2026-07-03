/-
  Linen.Network.WebApp.Extra.Middleware.Vhost — virtual host routing

  Routes requests to different applications based on the Host header. Ports
  Hale's `Network.Wai.Middleware.Vhost`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp

/-- Route requests based on the Host header. The first matching entry's
    application is used.
    $$\text{vhost} : \text{List (String} \times \text{Application)} \to \text{Middleware}$$ -/
def vhost (hosts : List (String × Application)) : Middleware :=
  fun app req respond =>
    let hostOpt := req.requestHeaderHost
    match hostOpt >>= fun h => hosts.find? (fun (pattern, _) => pattern == h) with
    | some (_, hostApp) => hostApp req respond
    | none => app req respond

end Network.WebApp.Extra.Middleware
