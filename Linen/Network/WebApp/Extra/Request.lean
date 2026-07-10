/-
  Linen.Network.WebApp.Extra.Request — request convenience queries

  Ports `Network.Wai.Request`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra

open Network.WebApp
open Network.HTTP.Types
open Data (CI)

/-- Whether a request "appears" secure, either because the connection itself
    is secure or because a trusted reverse proxy said so via
    `X-Forwarded-Proto: https` / `X-Forwarded-SSL: on`.
    $$\text{appearsSecure} : \text{Request} \to \text{Bool}$$ -/
def appearsSecure (req : Request) : Bool :=
  req.isSecure ||
  ((req.requestHeaders.find? (fun (n, _) => n == CI.mk' "X-Forwarded-Proto")).map (·.2) == some "https") ||
  ((req.requestHeaders.find? (fun (n, _) => n == CI.mk' "X-Forwarded-SSL")).map (·.2) == some "on")

end Network.WebApp.Extra
