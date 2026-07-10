/-
  Linen.CDP.Domains.Inspector — the `Inspector` CDP domain

  Ports `CDP.Domains.Inspector` (see `docs/imports/cdp/dependencies.md`);
  naming conventions as in `CDP.Domains.CacheStorage`'s docstring.
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.Inspector

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

/-- The `Inspector.detached` event: the connection has been terminated. -/
structure Detached where
  /-- The reason the connection has been terminated. -/
  reason : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON Detached where
  parseJSON v := do .ok { reason := ← Value.getField v "reason" >>= FromJSON.parseJSON }

instance : Event Detached where
  eventName := "Inspector.detached"

/-- The `Inspector.targetCrashed` event. -/
structure TargetCrashed where
  deriving Repr, BEq, DecidableEq

instance : FromJSON TargetCrashed where parseJSON _ := .ok {}

instance : Event TargetCrashed where
  eventName := "Inspector.targetCrashed"

/-- The `Inspector.targetReloadedAfterCrash` event. -/
structure TargetReloadedAfterCrash where
  deriving Repr, BEq, DecidableEq

instance : FromJSON TargetReloadedAfterCrash where parseJSON _ := .ok {}

instance : Event TargetReloadedAfterCrash where
  eventName := "Inspector.targetReloadedAfterCrash"

/-- Parameters of the `Inspector.disable` command: disables Inspector domain
    notifications. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "Inspector.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Inspector.enable` command: enables Inspector domain
    notifications. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "Inspector.enable"
  decodeResponse _ := .ok ()

end CDP.Domains.Inspector
