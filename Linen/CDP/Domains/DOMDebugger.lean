/-
  Linen.CDP.Domains.DOMDebugger — the `DOMDebugger` CDP domain

  DOM debugging allows setting breakpoints on particular DOM operations and
  events: JavaScript execution stops on these operations as if there was a
  regular breakpoint set. Ports `CDP.Domains.DOMDebugger` (see
  `docs/imports/cdp/dependencies.md`); naming conventions as in
  `CDP.Domains.CacheStorage`'s docstring.

  References `DOM.NodeId`/`DOM.BackendNodeId` from
  `CDP.Domains.DOMPageNetworkEmulationSecurity` and `ScriptId`/
  `RemoteObjectId`/`RemoteObject` from `CDP.Domains.Runtime`. `EventListener`
  embeds `Option Runtime.RemoteObject`, which derives only `Repr, BEq` (no
  `DecidableEq`, per `CDP.Domains.Runtime`'s docstring), so `EventListener`
  itself derives only `Repr, BEq` as well. None of this module's own types
  are self- or mutually-recursive, so no termination proofs are needed here.
  Upstream declares no events for this domain — only commands.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.DOMPageNetworkEmulationSecurity
import Linen.CDP.Domains.Runtime

namespace CDP.Domains.DOMDebugger

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command)

-- ── Enumerations ──

/-- DOM breakpoint type. -/
inductive DOMBreakpointType where
  | subtreeModified | attributeModified | nodeRemoved
  deriving Repr, BEq, DecidableEq

instance : FromJSON DOMBreakpointType where
  parseJSON
    | .string "subtree-modified" => .ok .subtreeModified
    | .string "attribute-modified" => .ok .attributeModified
    | .string "node-removed" => .ok .nodeRemoved
    | v => .error s!"failed to parse DOMBreakpointType: {repr v}"

instance : ToJSON DOMBreakpointType where
  toJSON
    | .subtreeModified => .string "subtree-modified"
    | .attributeModified => .string "attribute-modified"
    | .nodeRemoved => .string "node-removed"

/-- CSP violation type. -/
inductive CSPViolationType where
  | trustedtypeSinkViolation | trustedtypePolicyViolation
  deriving Repr, BEq, DecidableEq

instance : FromJSON CSPViolationType where
  parseJSON
    | .string "trustedtype-sink-violation" => .ok .trustedtypeSinkViolation
    | .string "trustedtype-policy-violation" => .ok .trustedtypePolicyViolation
    | v => .error s!"failed to parse CSPViolationType: {repr v}"

instance : ToJSON CSPViolationType where
  toJSON
    | .trustedtypeSinkViolation => .string "trustedtype-sink-violation"
    | .trustedtypePolicyViolation => .string "trustedtype-policy-violation"

-- ── Event listeners ──

/-- Object event listener. -/
structure EventListener where
  /-- `EventListener`'s type. -/
  type : String
  /-- `EventListener`'s `useCapture`. -/
  useCapture : Bool
  /-- `EventListener`'s passive flag. -/
  passive : Bool
  /-- `EventListener`'s once flag. -/
  once : Bool
  /-- Script id of the handler code. -/
  scriptId : Runtime.ScriptId
  /-- Line number in the script (0-based). -/
  lineNumber : Int
  /-- Column number in the script (0-based). -/
  columnNumber : Int
  /-- Event handler function value. -/
  handler : Option Runtime.RemoteObject := none
  /-- Event original handler function value. -/
  originalHandler : Option Runtime.RemoteObject := none
  /-- Node the listener is added to (if any). -/
  backendNodeId : Option DOMPageNetworkEmulationSecurity.DOM.BackendNodeId := none
  deriving Repr, BEq

instance : FromJSON EventListener where
  parseJSON v := do
    .ok
      { type := ← Value.getField v "type" >>= FromJSON.parseJSON
        useCapture := ← Value.getField v "useCapture" >>= FromJSON.parseJSON
        passive := ← Value.getField v "passive" >>= FromJSON.parseJSON
        once := ← Value.getField v "once" >>= FromJSON.parseJSON
        scriptId := ← Value.getField v "scriptId" >>= FromJSON.parseJSON
        lineNumber := ← Value.getField v "lineNumber" >>= FromJSON.parseJSON
        columnNumber := ← Value.getField v "columnNumber" >>= FromJSON.parseJSON
        handler := ← (← Value.getFieldOpt v "handler").mapM FromJSON.parseJSON
        originalHandler := ← (← Value.getFieldOpt v "originalHandler").mapM FromJSON.parseJSON
        backendNodeId := ← (← Value.getFieldOpt v "backendNodeId").mapM FromJSON.parseJSON }

instance : ToJSON EventListener where
  toJSON p := Data.Json.object <|
    [ ("type", ToJSON.toJSON p.type), ("useCapture", ToJSON.toJSON p.useCapture)
    , ("passive", ToJSON.toJSON p.passive), ("once", ToJSON.toJSON p.once)
    , ("scriptId", ToJSON.toJSON p.scriptId), ("lineNumber", ToJSON.toJSON p.lineNumber)
    , ("columnNumber", ToJSON.toJSON p.columnNumber) ]
    ++ (p.handler.map fun v => ("handler", ToJSON.toJSON v)).toList
    ++ (p.originalHandler.map fun v => ("originalHandler", ToJSON.toJSON v)).toList
    ++ (p.backendNodeId.map fun v => ("backendNodeId", ToJSON.toJSON v)).toList

-- ── Commands ──

/-- Parameters of the `DOMDebugger.getEventListeners` command: returns event
    listeners of the given object. -/
structure PGetEventListeners where
  /-- Identifier of the object to return listeners for. -/
  objectId : Runtime.RemoteObjectId
  /-- The maximum depth at which Node children should be retrieved, defaults
      to 1. Use -1 for the entire subtree or provide an integer larger than
      0. -/
  depth : Option Int := none
  /-- Whether or not iframes and shadow roots should be traversed when
      returning the subtree (default is false). Reports listeners for all
      contexts if pierce is enabled. -/
  pierce : Option Bool := none
  deriving Repr, BEq

instance : ToJSON PGetEventListeners where
  toJSON p := Data.Json.object <|
    [("objectId", ToJSON.toJSON p.objectId)]
    ++ (p.depth.map fun v => ("depth", ToJSON.toJSON v)).toList
    ++ (p.pierce.map fun v => ("pierce", ToJSON.toJSON v)).toList

/-- Response of the `DOMDebugger.getEventListeners` command. -/
structure GetEventListeners where
  /-- Array of relevant listeners. -/
  listeners : List EventListener
  deriving Repr, BEq

instance : FromJSON GetEventListeners where
  parseJSON v := do .ok { listeners := ← Value.getField v "listeners" >>= FromJSON.parseJSON }

instance : Command PGetEventListeners where
  Response := GetEventListeners
  commandName _ := "DOMDebugger.getEventListeners"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `DOMDebugger.removeDOMBreakpoint` command: removes DOM
    breakpoint that was set using `setDOMBreakpoint`. -/
structure PRemoveDOMBreakpoint where
  /-- Identifier of the node to remove breakpoint from. -/
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  /-- Type of the breakpoint to remove. -/
  type : DOMBreakpointType
  deriving Repr, BEq

instance : ToJSON PRemoveDOMBreakpoint where
  toJSON p := Data.Json.object [("nodeId", ToJSON.toJSON p.nodeId), ("type", ToJSON.toJSON p.type)]

instance : Command PRemoveDOMBreakpoint where
  Response := Unit
  commandName _ := "DOMDebugger.removeDOMBreakpoint"
  decodeResponse _ := .ok ()

/-- Parameters of the `DOMDebugger.removeEventListenerBreakpoint` command:
    removes breakpoint on particular DOM event. -/
structure PRemoveEventListenerBreakpoint where
  /-- Event name. -/
  eventName : String
  /-- `EventTarget` interface name. -/
  targetName : Option String := none
  deriving Repr, BEq

instance : ToJSON PRemoveEventListenerBreakpoint where
  toJSON p := Data.Json.object <|
    [("eventName", ToJSON.toJSON p.eventName)]
    ++ (p.targetName.map fun v => ("targetName", ToJSON.toJSON v)).toList

instance : Command PRemoveEventListenerBreakpoint where
  Response := Unit
  commandName _ := "DOMDebugger.removeEventListenerBreakpoint"
  decodeResponse _ := .ok ()

/-- Parameters of the `DOMDebugger.removeInstrumentationBreakpoint` command:
    removes breakpoint on particular native event. -/
structure PRemoveInstrumentationBreakpoint where
  /-- Instrumentation name to stop on. -/
  eventName : String
  deriving Repr, BEq

instance : ToJSON PRemoveInstrumentationBreakpoint where
  toJSON p := Data.Json.object [("eventName", ToJSON.toJSON p.eventName)]

instance : Command PRemoveInstrumentationBreakpoint where
  Response := Unit
  commandName _ := "DOMDebugger.removeInstrumentationBreakpoint"
  decodeResponse _ := .ok ()

/-- Parameters of the `DOMDebugger.removeXHRBreakpoint` command: removes
    breakpoint from `XMLHttpRequest`. -/
structure PRemoveXHRBreakpoint where
  /-- Resource URL substring. -/
  url : String
  deriving Repr, BEq

instance : ToJSON PRemoveXHRBreakpoint where
  toJSON p := Data.Json.object [("url", ToJSON.toJSON p.url)]

instance : Command PRemoveXHRBreakpoint where
  Response := Unit
  commandName _ := "DOMDebugger.removeXHRBreakpoint"
  decodeResponse _ := .ok ()

/-- Parameters of the `DOMDebugger.setBreakOnCSPViolation` command: sets
    breakpoint on particular CSP violations. -/
structure PSetBreakOnCSPViolation where
  /-- CSP violations to stop upon. -/
  violationTypes : List CSPViolationType
  deriving Repr, BEq

instance : ToJSON PSetBreakOnCSPViolation where
  toJSON p := Data.Json.object [("violationTypes", ToJSON.toJSON p.violationTypes)]

instance : Command PSetBreakOnCSPViolation where
  Response := Unit
  commandName _ := "DOMDebugger.setBreakOnCSPViolation"
  decodeResponse _ := .ok ()

/-- Parameters of the `DOMDebugger.setDOMBreakpoint` command: sets breakpoint
    on particular operation with DOM. -/
structure PSetDOMBreakpoint where
  /-- Identifier of the node to set breakpoint on. -/
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  /-- Type of the operation to stop upon. -/
  type : DOMBreakpointType
  deriving Repr, BEq

instance : ToJSON PSetDOMBreakpoint where
  toJSON p := Data.Json.object [("nodeId", ToJSON.toJSON p.nodeId), ("type", ToJSON.toJSON p.type)]

instance : Command PSetDOMBreakpoint where
  Response := Unit
  commandName _ := "DOMDebugger.setDOMBreakpoint"
  decodeResponse _ := .ok ()

/-- Parameters of the `DOMDebugger.setEventListenerBreakpoint` command: sets
    breakpoint on particular DOM event. -/
structure PSetEventListenerBreakpoint where
  /-- DOM event name to stop on (any DOM event will do). -/
  eventName : String
  /-- `EventTarget` interface name to stop on. If equal to `"*"` or not
      provided, will stop on any `EventTarget`. -/
  targetName : Option String := none
  deriving Repr, BEq

instance : ToJSON PSetEventListenerBreakpoint where
  toJSON p := Data.Json.object <|
    [("eventName", ToJSON.toJSON p.eventName)]
    ++ (p.targetName.map fun v => ("targetName", ToJSON.toJSON v)).toList

instance : Command PSetEventListenerBreakpoint where
  Response := Unit
  commandName _ := "DOMDebugger.setEventListenerBreakpoint"
  decodeResponse _ := .ok ()

/-- Parameters of the `DOMDebugger.setInstrumentationBreakpoint` command:
    sets breakpoint on particular native event. -/
structure PSetInstrumentationBreakpoint where
  /-- Instrumentation name to stop on. -/
  eventName : String
  deriving Repr, BEq

instance : ToJSON PSetInstrumentationBreakpoint where
  toJSON p := Data.Json.object [("eventName", ToJSON.toJSON p.eventName)]

instance : Command PSetInstrumentationBreakpoint where
  Response := Unit
  commandName _ := "DOMDebugger.setInstrumentationBreakpoint"
  decodeResponse _ := .ok ()

/-- Parameters of the `DOMDebugger.setXHRBreakpoint` command: sets breakpoint
    on `XMLHttpRequest`. -/
structure PSetXHRBreakpoint where
  /-- Resource URL substring. All XHRs having this substring in the URL will
      get stopped upon. -/
  url : String
  deriving Repr, BEq

instance : ToJSON PSetXHRBreakpoint where
  toJSON p := Data.Json.object [("url", ToJSON.toJSON p.url)]

instance : Command PSetXHRBreakpoint where
  Response := Unit
  commandName _ := "DOMDebugger.setXHRBreakpoint"
  decodeResponse _ := .ok ()

end CDP.Domains.DOMDebugger
