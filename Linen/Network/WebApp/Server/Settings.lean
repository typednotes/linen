/-
  Linen.Network.WebApp.Server.Settings — Server configuration

  Ports `Network.Wai.Handler.Warp.Settings`.

  ## Design

  `Settings` mirrors the source `Settings` record type. All fields have sensible
  defaults so `defaultSettings` (or `{}`) is a valid, production-ready configuration.

  ## Guarantees

  - `settingsPort` is `UInt16`, bounding the port to [0, 65535] by construction
  - `settingsTimeout > 0` by construction (proof field, erased at runtime)
  - `settingsBacklog > 0` by construction (proof field, erased at runtime)
-/

import Linen.Network.WebApp
import Linen.Network.HTTP.Types.Header
import Linen.Network.Socket.Types

namespace Network.WebApp.Server

open Network.HTTP.Types
open Network.Socket (SockAddr)

/-- Server settings.
    Proofs are embedded directly in the structure (erased at runtime, zero cost).
    $$\text{Settings} = \{ \text{port} : \text{UInt16},\; \text{host} : \text{String},\; \ldots \}$$ -/
structure Settings where
  /-- Port to listen on. Default: 3000. -/
  settingsPort : UInt16 := 3000
  /-- Host to bind to. Default: "0.0.0.0" (all interfaces). -/
  settingsHost : String := "0.0.0.0"
  /-- Called when an exception occurs during request handling.
      Receives the remote address if available. -/
  settingsOnException : Option SockAddr → IO Unit := fun _ => pure ()
  /-- Called just before the server starts its accept loop.
      Useful for logging "server started on port X". -/
  settingsBeforeMainLoop : IO Unit := pure ()
  /-- Server name for the `Server` response header. -/
  settingsServerName : String := "Linen/WebApp.Server"
  /-- Maximum number of bytes to flush from a request body on connection
      reuse. `none` means no flushing limit. -/
  settingsMaximumBodyFlush : Option Nat := some 8192
  /-- Timeout in seconds for each connection. Must be > 0. -/
  settingsTimeout : Nat := 30
  /-- Proof that timeout is positive (zero would immediately close connections).
      Erased at runtime. -/
  settingsTimeoutPos : settingsTimeout > 0 := by omega
  /-- Socket listen backlog. Must be > 0. -/
  settingsBacklog : Nat := 128
  /-- Proof that backlog is positive (zero would reject all connections).
      Erased at runtime. -/
  settingsBacklogPos : settingsBacklog > 0 := by omega
  /-- Graceful shutdown timeout in seconds. `none` means no graceful shutdown. -/
  settingsGracefulShutdownTimeout : Option Nat := none
  /-- Whether to auto-add the `Date` response header. -/
  settingsAddDateHeader : Bool := true
  /-- Whether to auto-add the `Server` response header. -/
  settingsAddServerHeader : Bool := true

/-- Default settings.
    $$\text{defaultSettings} = \text{Settings}\{\}$$ -/
def defaultSettings : Settings := {}

/-- The default settings have positive timeout and backlog (by construction). -/
theorem defaultSettings_valid : (defaultSettings).settingsTimeout > 0 ∧
    (defaultSettings).settingsBacklog > 0 :=
  ⟨defaultSettings.settingsTimeoutPos, defaultSettings.settingsBacklogPos⟩

end Network.WebApp.Server
