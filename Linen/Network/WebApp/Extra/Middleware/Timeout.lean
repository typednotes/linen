/-
  Linen.Network.WebApp.Extra.Middleware.Timeout — request timeout middleware

  Enforces a timeout on request processing. If the inner application takes
  longer than the specified duration, returns 503 Service Unavailable.
  Ports `Network.Wai.Middleware.Timeout`.
-/
import Linen.Network.WebApp
import Linen.Control.Concurrent.Green

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.WebApp.AppM (unsafeLift)
open Network.HTTP.Types
open Control.Concurrent.Green (Green)

/-- Enforce a timeout on request handling. If the inner application does
    not respond within `ms` milliseconds, returns 503 Service Unavailable.

    Uses `AppM.unsafeLift` because the timeout/app race requires runtime
    arbitration (an atomic flag ensures exactly-once response despite two
    potential responders). The indexed monad guarantee is upheld
    dynamically.
    $$\text{timeout} : \mathbb{N} \to \text{Middleware}$$ -/
def timeout (ms : Nat) : Middleware :=
  fun app req respond =>
    unsafeLift do
      -- Atomic flag: ensures exactly one call to `respond`.
      let respondedRef ← (IO.mkRef false : IO _)
      let respondOnce : Response → Green ResponseReceived := fun resp => do
        let alreadyResponded ← (respondedRef.swap true : IO _)
        if alreadyResponded then
          -- Second responder loses — return a dummy token.
          pure ResponseReceived.done
        else
          respond resp
      -- Run the app in a background task with the guarded callback.
      let resultRef ← (IO.mkRef (none : Option ResponseReceived) : IO _)
      let token ← (Std.CancellationToken.new : IO _)
      let _task ← (IO.asTask do
        let r ← Green.block (app req respondOnce).run token
        resultRef.set (some r) : IO _)
      -- Wait with timeout.
      (IO.sleep ms.toUInt32 : IO _)
      let result ← (resultRef.get : IO _)
      match result with
      | some r => return r
      | none =>
        -- Timed out — respond with 503 (respondOnce ensures at most one send).
        Green.block (respondOnce (.responseBuilder status503 []
          "Service Unavailable: request timed out".toUTF8)) token

end Network.WebApp.Extra.Middleware
