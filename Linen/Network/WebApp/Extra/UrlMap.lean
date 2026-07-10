/-
  Linen.Network.WebApp.Extra.UrlMap — dispatch by path prefix

  Ports `Network.Wai.UrlMap`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra

open Network.WebApp

/-- Dispatch to the first `Application` whose route prefix matches the
    request path, stripping that prefix before delegating. Falls back to
    `fallback` if no route matches.
    $$\text{urlMap} : \text{List}(\text{String} \times \text{Application}) \to \text{Middleware}$$ -/
def urlMap (routes : List (String × Application)) : Middleware :=
  fun fallback req respond =>
    let path := req.rawPathInfo
    match routes.find? (fun (pathPrefix, _) => path.startsWith pathPrefix) with
    | some (pathPrefix, app) =>
      let newPath := (path.drop pathPrefix.length).toString
      let newPath := if newPath.isEmpty then "/" else newPath
      let segments := newPath.splitOn "/" |>.filter (!·.isEmpty)
      app { req with rawPathInfo := newPath, pathInfo := segments } respond
    | none => fallback req respond

end Network.WebApp.Extra
