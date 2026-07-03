/-
  Linen.Network.WebApp.Extra.Middleware.Routed — path-based middleware
  routing

  Applies a middleware only to requests matching a path predicate. Ports
  Hale's `Network.Wai.Middleware.Routed`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp

/-- Apply `middle` only to requests matching `predicate`.
    $$\text{routed} : (\text{Request} \to \text{Bool}) \to \text{Middleware} \to \text{Middleware}$$ -/
def routed (predicate : Request → Bool) (middle : Middleware) : Middleware :=
  fun app req respond =>
    if predicate req then
      middle app req respond
    else
      app req respond

/-- Apply `middle` only to requests with the given path prefix.
    $$\text{routedPrefix} : \text{String} \to \text{Middleware} \to \text{Middleware}$$ -/
def routedPrefix (pathPrefix : String) (middle : Middleware) : Middleware :=
  routed (fun req => req.rawPathInfo.startsWith pathPrefix) middle

/-- Routing with an always-true predicate applies the middleware. -/
theorem routed_true (middle : Middleware) :
    routed (fun _ => true) middle = middle := rfl

/-- Routing with an always-false predicate is the identity. -/
theorem routed_false (middle : Middleware) :
    routed (fun _ => false) middle = (id : Middleware) := rfl

end Network.WebApp.Extra.Middleware
