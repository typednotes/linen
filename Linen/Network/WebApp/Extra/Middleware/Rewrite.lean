/-
  Linen.Network.WebApp.Extra.Middleware.Rewrite — URL rewriting

  Rewrites request paths based on custom rules. Ports
  `Network.Wai.Middleware.Rewrite`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types

/-- A rewrite rule takes path segments and headers, and returns the new
    path segments and possibly modified headers. -/
abbrev RewriteRule := List String → RequestHeaders → (List String × RequestHeaders)

/-- Rewrite request paths based on custom rules.
    $$\text{rewrite} : \text{RewriteRule} \to \text{Middleware}$$ -/
def rewrite (rule : RewriteRule) : Middleware :=
  fun app req respond =>
    let (newPath, newHeaders) := rule req.pathInfo req.requestHeaders
    let rawPath := "/" ++ "/".intercalate newPath
    let req' := { req with
      pathInfo := newPath
      rawPathInfo := rawPath
      requestHeaders := newHeaders
    }
    app req' respond

/-- Simple path prefix rewrite: strip a prefix and prepend a new one.
    $$\text{rewritePrefix} : \text{String} \to \text{String} \to \text{Middleware}$$ -/
def rewritePrefix (from_ to_ : String) : Middleware :=
  fun app req respond =>
    let path := req.rawPathInfo
    if path.startsWith from_ then
      let newPath := to_ ++ path.drop from_.length
      let segments := newPath.splitOn "/" |>.filter (!·.isEmpty)
      app { req with rawPathInfo := newPath, pathInfo := segments } respond
    else
      app req respond

end Network.WebApp.Extra.Middleware
