/-
  Linen.Network.WebApp.Extra.Middleware.MethodOverride — override HTTP
  method from the query string

  Ports Hale's `Network.Wai.Middleware.MethodOverride`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types

/-- Override the request method if an `_method` query parameter is present.
    $$\text{methodOverride} : \text{Middleware}$$ -/
def methodOverride : Middleware :=
  fun app req respond =>
    let req' := match req.queryString.find? (fun (k, _) => k == "_method") with
      | some (_, some v) => { req with requestMethod := parseMethod v }
      | _ => req
    app req' respond

end Network.WebApp.Extra.Middleware
