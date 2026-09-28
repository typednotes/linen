/-
  Tests for `Linen.CDP.Domains.Runtime`.
-/
import Linen.CDP.Domains.Runtime

open CDP.Domains.Runtime
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Runtime

-- ── simple enums ──

#guard decodeAs "\"number\"" (α := ObjType) = .ok .number
#guard encode (ToJSON.toJSON ObjType.function) = "\"function\""
#guard decodeAs "\"typedarray\"" (α := Subtype) = .ok .typedarray
#guard decodeAs "\"accessor\"" (α := PropertyPreviewType) = .ok .accessor
#guard decodeAs "\"window\"" (α := WebDriverValueType) = .ok .window

/-! ### ObjectPreview / PropertyPreview / EntryPreview — mutually recursive;
    none of the three derives `DecidableEq` (only `BEq`, since the
    auto-deriving handler can't see through the hand-proven mutually
    recursive `FromJSON`/`ToJSON`), so equality checks here pattern-match
    and use `==`, mirroring `CDP.Domains.Media`'s `PlayerError` tests. -/

def leaf : ObjectPreview := { type := .number, overflow := false, properties := [] }

#guard match decodeAs "{\"type\": \"number\", \"overflow\": false, \"properties\": []}"
    (α := ObjectPreview) with
  | .ok v => v == leaf
  | .error _ => false

def withProp : ObjectPreview :=
  { type := .object, overflow := false
    properties := [{ name := "x", type := .number, value := some "1" }] }

#guard match decodeAs
    "{\"type\": \"object\", \"overflow\": false, \"properties\": [{\"name\": \"x\", \"type\": \"number\", \"value\": \"1\"}]}"
    (α := ObjectPreview) with
  | .ok v => v == withProp
  | .error _ => false

-- `PropertyPreview.valuePreview` recurses back into `ObjectPreview`.
def nestedPreview : ObjectPreview :=
  { type := .object, overflow := false
    properties :=
      [ { name := "inner", type := .object, valuePreview := some leaf } ] }

#guard match decodeAs (encode (ToJSON.toJSON nestedPreview)) (α := ObjectPreview) with
  | .ok v => v == nestedPreview
  | .error _ => false

-- `EntryPreview.key`/`value` and `ObjectPreview.entries` round-trip too.
def withEntry : ObjectPreview :=
  { type := .object, overflow := false, properties := []
    subtype := some .map
    entries := some [ { key := some leaf, value := leaf } ] }

#guard match decodeAs (encode (ToJSON.toJSON withEntry)) (α := ObjectPreview) with
  | .ok v => v == withEntry
  | .error _ => false

/-! ### StackTrace — self-referential via `parent`. -/

def frame : CallFrame :=
  { functionName := "f", scriptId := "1", url := "http://x", lineNumber := 0, columnNumber := 0 }

def parentTrace : StackTrace := { callFrames := [frame] }
def childTrace : StackTrace := { callFrames := [frame], parent := some parentTrace }

#guard match decodeAs (encode (ToJSON.toJSON childTrace)) (α := StackTrace) with
  | .ok v => v == childTrace
  | .error _ => false

/-! ### RemoteObject / events -/

#guard match decodeAs "{\"type\": \"string\"}" (α := RemoteObject) with
  | .ok v => v == ({ type := .string } : RemoteObject)
  | .error _ => false

#guard Event.eventName (α := ExecutionContextsCleared) = "Runtime.executionContextsCleared"
#guard decodeAs "{\"reason\": \"r\", \"exceptionId\": 1}" (α := ExceptionRevoked)
  = .ok { reason := "r", exceptionId := 1 }

/-! ### Commands -/

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PDisable) = "Runtime.disable"
#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PEnable) = "Runtime.enable"

#guard encode (ToJSON.toJSON ({ expression := "1+1" } : PEvaluate)) = "{\"expression\":\"1+1\"}"
#guard Command.commandName ({ expression := "1+1" } : PEvaluate) = "Runtime.evaluate"
#guard match decodeAs "{\"result\": {\"type\": \"number\"}}" (α := Evaluate) with
  | .ok v => v == ({ result := { type := .number } } : Evaluate)
  | .error _ => false

#guard encode (ToJSON.toJSON ({ objectId := "o1" } : PReleaseObject)) = "{\"objectId\":\"o1\"}"
#guard Command.commandName ({ objectId := "o1" } : PReleaseObject) = "Runtime.releaseObject"

#guard decodeAs "{\"id\": \"iso1\"}" (α := GetIsolateId) = .ok { id := "iso1" }
#guard Command.commandName ({} : PGetIsolateId) = "Runtime.getIsolateId"

#guard decodeAs "{\"usedSize\": 1.0, \"totalSize\": 2.0}" (α := GetHeapUsage)
  = .ok { usedSize := 1.0, totalSize := 2.0 }

#guard match decodeAs "{\"result\": [], \"exceptionDetails\": null}" (α := GetProperties) with
  | .ok v => v == ({ result := [], exceptionDetails := none } : GetProperties)
  | .error _ => false

#guard encode (ToJSON.toJSON ({ name := "b" } : PAddBinding)) = "{\"name\":\"b\"}"
#guard Command.commandName ({ name := "b" } : PAddBinding) = "Runtime.addBinding"

end Tests.CDP.Domains.Runtime
