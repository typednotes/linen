/-
  Linen.Network.WebApp.Server.WithApplication — Test helpers

  Run a WAI Application on a free port for testing.
  The server is automatically shut down after the action completes.

  Ports `Network.Wai.Handler.Warp.WithApplication`.

  ## Guarantees
  - Server is always cleaned up (via try/finally)
  - Port defaults to one chosen by the OS (no conflicts; pass `port := 0`,
    the default), or a caller-specified fixed port (e.g. for an OAuth2
    redirect URI that must be pre-registered with an exact port). Either
    way the actual bound port is read back via `getSockName` and handed to
    `action` (binding with port 0 but reporting the literal `0` back would
    defeat the purpose of this helper — this is a correctness fix over the
    upstream source, using the `getSockName` primitive already available in
    Linen)
  - Action runs only after server is listening
-/
import Linen.Network.WebApp
import Linen.Network.Socket
import Linen.Network.Socket.EventDispatcher
import Linen.Control.Concurrent.Green
import Linen.Network.WebApp.Server.Settings
import Linen.Network.WebApp.Server.Run

namespace Network.WebApp.Server

open Network.WebApp
open Network.Socket
open Control.Concurrent.Green (Green)

/-- `withApplication` with custom Settings and, optionally, a fixed `port`
    to bind instead of letting the OS pick one (`port := 0`, the default). -/
def withApplicationSettings (settings : Settings) (mkApp : IO Application)
    (action : UInt16 → IO α) (port : UInt16 := 0) : IO α := do
  let app ← mkApp
  let sock ← Network.Socket.listenTCP "0.0.0.0" port 128
  let boundAddr ← Network.Socket.getSockName sock
  Network.Socket.setNonBlocking sock
  let disp ← EventDispatcher.create
  let token ← Std.CancellationToken.new
  let _serverTask ← IO.asTask (prio := .dedicated) do
    try
      Green.block (acceptLoopEL sock settings app disp) token
    catch _ => pure ()
    finally
      disp.shutdown
      let _ ← Network.Socket.close sock
  try
    IO.sleep 50
    action boundAddr.port
  finally
    token.cancel .cancel
    -- `token.cancel` alone only sets a flag `Green.await` never checks: the
    -- accept loop is typically parked in `disp.waitReadable` with no pending
    -- connection, and only an actual wake-up (which `disp.shutdown` now
    -- provides, see its docstring) lets `acceptLoopEL`'s cancellation check
    -- run and the loop actually exit. Without this, `_serverTask`'s dedicated
    -- thread — and the process itself — would never terminate.
    disp.shutdown

/-- Run an Application on a free port, or a fixed one if `port` is given. -/
def withApplication (mkApp : IO Application) (action : UInt16 → IO α) (port : UInt16 := 0) : IO α :=
  withApplicationSettings defaultSettings mkApp action port

end Network.WebApp.Server
