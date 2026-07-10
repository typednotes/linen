/-
  Linen.Network.WebApp.Extra.Middleware.AcceptOverride — override Accept
  header from the query string

  Ports `Network.Wai.Middleware.AcceptOverride`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types

/-- Override the `Accept` header if an `_accept` query parameter is present.
    $$\text{acceptOverride} : \text{Middleware}$$ -/
def acceptOverride : Middleware :=
  fun app req respond =>
    let req' := match req.queryString.find? (fun (k, _) => k == "_accept") with
      | some (_, some v) =>
        { req with requestHeaders := (hAccept, v) :: req.requestHeaders.filter (·.1 != hAccept) }
      | _ => req
    app req' respond

end Network.WebApp.Extra.Middleware
