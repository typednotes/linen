/-
  Tests for `Linen.CDP.Domains.DOMDebugger`.
-/
import Linen.CDP.Domains.DOMDebugger

open CDP.Domains.DOMDebugger
open CDP.Internal.Utils (Command)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.DOMDebugger

#guard decodeAs "\"subtree-modified\"" (α := DOMBreakpointType) = .ok .subtreeModified
#guard decodeAs "\"attribute-modified\"" (α := DOMBreakpointType) = .ok .attributeModified
#guard decodeAs "\"node-removed\"" (α := DOMBreakpointType) = .ok .nodeRemoved
#guard encode (ToJSON.toJSON DOMBreakpointType.subtreeModified) = "\"subtree-modified\""

#guard decodeAs "\"trustedtype-sink-violation\"" (α := CSPViolationType)
  = .ok .trustedtypeSinkViolation
#guard decodeAs "\"trustedtype-policy-violation\"" (α := CSPViolationType)
  = .ok .trustedtypePolicyViolation
#guard encode (ToJSON.toJSON CSPViolationType.trustedtypeSinkViolation)
  = "\"trustedtype-sink-violation\""

#guard
  match decodeAs
      "{\"type\": \"click\", \"useCapture\": true, \"passive\": false, \"once\": false, \"scriptId\": \"1\", \"lineNumber\": 0, \"columnNumber\": 0}"
      (α := EventListener) with
  | .ok v =>
    v == { type := "click", useCapture := true, passive := false, once := false
           scriptId := "1", lineNumber := 0, columnNumber := 0 }
  | .error _ => false
#guard
  encode
      (ToJSON.toJSON
        ({ type := "click", useCapture := true, passive := false, once := false
           scriptId := "1", lineNumber := 0, columnNumber := 0 } : EventListener))
  = "{\"type\":\"click\",\"useCapture\":true,\"passive\":false,\"once\":false,\"scriptId\":\"1\",\"lineNumber\":0,\"columnNumber\":0}"

#guard encode (ToJSON.toJSON ({ objectId := "obj1" } : PGetEventListeners)) = "{\"objectId\":\"obj1\"}"
#guard
  encode (ToJSON.toJSON ({ objectId := "obj1", depth := some 2 } : PGetEventListeners))
  = "{\"objectId\":\"obj1\",\"depth\":2}"
#guard Command.commandName ({ objectId := "obj1" } : PGetEventListeners) = "DOMDebugger.getEventListeners"
#guard
  match decodeAs "{\"listeners\": []}" (α := GetEventListeners) with
  | .ok v => v == { listeners := [] }
  | .error _ => false

#guard
  encode (ToJSON.toJSON ({ nodeId := 1, type := .subtreeModified } : PRemoveDOMBreakpoint))
  = "{\"nodeId\":1,\"type\":\"subtree-modified\"}"
#guard
  Command.commandName ({ nodeId := 1, type := .subtreeModified } : PRemoveDOMBreakpoint)
  = "DOMDebugger.removeDOMBreakpoint"

#guard
  encode (ToJSON.toJSON ({ eventName := "click" } : PRemoveEventListenerBreakpoint))
  = "{\"eventName\":\"click\"}"
#guard
  encode
      (ToJSON.toJSON ({ eventName := "click", targetName := some "*" } : PRemoveEventListenerBreakpoint))
  = "{\"eventName\":\"click\",\"targetName\":\"*\"}"
#guard
  Command.commandName ({ eventName := "click" } : PRemoveEventListenerBreakpoint)
  = "DOMDebugger.removeEventListenerBreakpoint"

#guard
  encode (ToJSON.toJSON ({ eventName := "setTimeout" } : PRemoveInstrumentationBreakpoint))
  = "{\"eventName\":\"setTimeout\"}"
#guard
  Command.commandName ({ eventName := "setTimeout" } : PRemoveInstrumentationBreakpoint)
  = "DOMDebugger.removeInstrumentationBreakpoint"

#guard encode (ToJSON.toJSON ({ url := "example.com" } : PRemoveXHRBreakpoint)) = "{\"url\":\"example.com\"}"
#guard
  Command.commandName ({ url := "example.com" } : PRemoveXHRBreakpoint) = "DOMDebugger.removeXHRBreakpoint"

#guard
  encode
      (ToJSON.toJSON
        ({ violationTypes := [.trustedtypeSinkViolation] } : PSetBreakOnCSPViolation))
  = "{\"violationTypes\":[\"trustedtype-sink-violation\"]}"
#guard
  Command.commandName ({ violationTypes := [] } : PSetBreakOnCSPViolation)
  = "DOMDebugger.setBreakOnCSPViolation"

#guard
  encode (ToJSON.toJSON ({ nodeId := 2, type := .attributeModified } : PSetDOMBreakpoint))
  = "{\"nodeId\":2,\"type\":\"attribute-modified\"}"
#guard
  Command.commandName ({ nodeId := 2, type := .attributeModified } : PSetDOMBreakpoint)
  = "DOMDebugger.setDOMBreakpoint"

#guard
  encode (ToJSON.toJSON ({ eventName := "click" } : PSetEventListenerBreakpoint))
  = "{\"eventName\":\"click\"}"
#guard
  encode
      (ToJSON.toJSON ({ eventName := "click", targetName := some "*" } : PSetEventListenerBreakpoint))
  = "{\"eventName\":\"click\",\"targetName\":\"*\"}"
#guard
  Command.commandName ({ eventName := "click" } : PSetEventListenerBreakpoint)
  = "DOMDebugger.setEventListenerBreakpoint"

#guard
  encode (ToJSON.toJSON ({ eventName := "setTimeout" } : PSetInstrumentationBreakpoint))
  = "{\"eventName\":\"setTimeout\"}"
#guard
  Command.commandName ({ eventName := "setTimeout" } : PSetInstrumentationBreakpoint)
  = "DOMDebugger.setInstrumentationBreakpoint"

#guard encode (ToJSON.toJSON ({ url := "example.com" } : PSetXHRBreakpoint)) = "{\"url\":\"example.com\"}"
#guard Command.commandName ({ url := "example.com" } : PSetXHRBreakpoint) = "DOMDebugger.setXHRBreakpoint"

end Tests.CDP.Domains.DOMDebugger
