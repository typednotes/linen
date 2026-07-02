/-
  Tests for `Linen.Network.HTTP.Client.Redirect`.

  Both `executeWithRedirects` and `execute` perform real network IO (they
  connect, send, and follow `Location` headers over the wire), so — like
  `Connection`'s establishers — they're pinned at the type level rather than
  exercised against a live server.
-/
import Linen.Network.HTTP.Client.Redirect

open Network.HTTP.Client

namespace Tests.Network.HTTP.Client.Redirect

/-! ### Redirect execution — signatures -/

example : Nat → Request → IO Response := executeWithRedirects
example (req : Request) (n : Nat) : IO Response := execute req n

/-! ### `execute`'s redirect budget defaults to 10 -/

example (req : Request) : execute req = executeWithRedirects 10 req := rfl

end Tests.Network.HTTP.Client.Redirect
