/-
  Linen.CDP.Domains.EventBreakpoints — the `EventBreakpoints` CDP domain

  Ports `CDP.Domains.EventBreakpoints` (see
  `docs/imports/cdp/dependencies.md`); naming conventions as in
  `CDP.Domains.CacheStorage`'s docstring.
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.EventBreakpoints

open Data.Json (ToJSON)
open CDP.Internal.Utils (Command)

/-- Parameters of the `EventBreakpoints.setInstrumentationBreakpoint` command:
    sets a breakpoint on a particular native event. -/
structure PSetInstrumentationBreakpoint where
  /-- Instrumentation name to stop on. -/
  eventName : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetInstrumentationBreakpoint where
  toJSON p := Data.Json.object [("eventName", ToJSON.toJSON p.eventName)]

instance : Command PSetInstrumentationBreakpoint where
  Response := Unit
  commandName _ := "EventBreakpoints.setInstrumentationBreakpoint"
  decodeResponse _ := .ok ()

/-- Parameters of the `EventBreakpoints.removeInstrumentationBreakpoint`
    command: removes a breakpoint on a particular native event. -/
structure PRemoveInstrumentationBreakpoint where
  /-- Instrumentation name to stop on. -/
  eventName : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRemoveInstrumentationBreakpoint where
  toJSON p := Data.Json.object [("eventName", ToJSON.toJSON p.eventName)]

instance : Command PRemoveInstrumentationBreakpoint where
  Response := Unit
  commandName _ := "EventBreakpoints.removeInstrumentationBreakpoint"
  decodeResponse _ := .ok ()

end CDP.Domains.EventBreakpoints
