/-
  Linen.CDP.Domains.Tethering — the `Tethering` CDP domain

  Ports `CDP.Domains.Tethering` (see `docs/imports/cdp/dependencies.md`);
  naming conventions as in `CDP.Domains.CacheStorage`'s docstring.

  The Tethering domain defines methods and events for browser port binding.
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.Tethering

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

/-- The `Tethering.accepted` event: informs that port was successfully bound
    and got a specified connection id. -/
structure Accepted where
  /-- Port number that was successfully bound. -/
  port : Int
  /-- Connection id to be used. -/
  connectionId : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON Accepted where
  parseJSON v := do
    .ok
      { port := ← Value.getField v "port" >>= FromJSON.parseJSON
        connectionId := ← Value.getField v "connectionId" >>= FromJSON.parseJSON }

instance : Event Accepted where
  eventName := "Tethering.accepted"

/-- Parameters of the `Tethering.bind` command: requests browser port
    binding. -/
structure PBind where
  /-- Port number to bind. -/
  port : Int
  deriving Repr, BEq, DecidableEq

instance : ToJSON PBind where
  toJSON p := Data.Json.object [("port", ToJSON.toJSON p.port)]

instance : Command PBind where
  Response := Unit
  commandName _ := "Tethering.bind"
  decodeResponse _ := .ok ()

/-- Parameters of the `Tethering.unbind` command: requests browser port
    unbinding. -/
structure PUnbind where
  /-- Port number to unbind. -/
  port : Int
  deriving Repr, BEq, DecidableEq

instance : ToJSON PUnbind where
  toJSON p := Data.Json.object [("port", ToJSON.toJSON p.port)]

instance : Command PUnbind where
  Response := Unit
  commandName _ := "Tethering.unbind"
  decodeResponse _ := .ok ()

end CDP.Domains.Tethering
