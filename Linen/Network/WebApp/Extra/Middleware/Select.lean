/-
  Linen.Network.WebApp.Extra.Middleware.Select — conditionally apply a
  middleware

  Ports Hale's `Network.Wai.Middleware.Select`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp

/-- Conditionally apply a middleware based on a request predicate. If the
    predicate returns `some middleware`, apply it; otherwise pass through.
    $$\text{select} : (\text{Request} \to \text{Option Middleware}) \to \text{Middleware}$$ -/
def select (choose : Request → Option Middleware) : Middleware :=
  fun app req respond =>
    match choose req with
    | some mid => mid app req respond
    | none => app req respond

/-- `select` with an always-`none` chooser is the identity middleware.
    $$\text{select}(\lambda\, \_.\; \text{none}) = \text{id}$$ -/
theorem select_none : select (fun _ => (none : Option Middleware)) = (id : Middleware) := rfl

end Network.WebApp.Extra.Middleware
