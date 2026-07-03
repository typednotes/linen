/-
  Linen.Network.WebApp.Extra.Middleware.CombineHeaders — merge duplicate
  response headers

  Combines response headers with the same name, joining values with commas.
  Ports Hale's `Network.Wai.Middleware.CombineHeaders`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types

/-- Combine duplicate response headers by joining their values with `", "`.
    $$\text{combineHeaders} : \text{Middleware}$$ -/
def combineHeaders : Middleware :=
  fun app req respond =>
    app req fun resp =>
      respond (resp.mapResponseHeaders combineHeaderList)
where
  combineHeaderList (hdrs : ResponseHeaders) : ResponseHeaders :=
    let groups := hdrs.foldl (init := ([] : List (HeaderName × List String))) fun acc (name, val) =>
      match acc.find? (fun (n, _) => n == name) with
      | some _ => acc.map fun (n, vs) => if n == name then (n, vs ++ [val]) else (n, vs)
      | none => acc ++ [(name, [val])]
    groups.map fun (name, vals) => (name, ", ".intercalate vals)

end Network.WebApp.Extra.Middleware
