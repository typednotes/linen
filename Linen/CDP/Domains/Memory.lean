/-
  Linen.CDP.Domains.Memory — the `Memory` CDP domain

  Ports `CDP.Domains.Memory` (see `docs/imports/cdp/dependencies.md`); naming
  conventions as in `CDP.Domains.CacheStorage`'s docstring.
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.Memory

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command)

/-- Memory pressure level. -/
inductive PressureLevel where
  | moderate | critical
  deriving Repr, BEq, DecidableEq

instance : FromJSON PressureLevel where
  parseJSON
    | .string "moderate" => .ok .moderate
    | .string "critical" => .ok .critical
    | v => .error s!"failed to parse PressureLevel: {repr v}"

instance : ToJSON PressureLevel where
  toJSON | .moderate => .string "moderate" | .critical => .string "critical"

/-- A heap profile sample. -/
structure SamplingProfileNode where
  /-- Size of the sampled allocation. -/
  size : Float
  /-- Total bytes attributed to this sample. -/
  total : Float
  /-- Execution stack at the point of allocation. -/
  stack : List String
  deriving Repr, BEq, DecidableEq

instance : FromJSON SamplingProfileNode where
  parseJSON v := do
    .ok
      { size := ← Value.getField v "size" >>= FromJSON.parseJSON
        total := ← Value.getField v "total" >>= FromJSON.parseJSON
        stack := ← Value.getField v "stack" >>= FromJSON.parseJSON }

instance : ToJSON SamplingProfileNode where
  toJSON p := Data.Json.object
    [("size", ToJSON.toJSON p.size), ("total", ToJSON.toJSON p.total), ("stack", ToJSON.toJSON p.stack)]

/-- Executable module information. -/
structure Module where
  /-- Name of the module. -/
  name : String
  /-- UUID of the module. -/
  uuid : String
  /-- Base address where the module is loaded into memory. Encoded as a
      decimal or hexadecimal (`0x`-prefixed) string. -/
  baseAddress : String
  /-- Size of the module in bytes. -/
  size : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON Module where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        uuid := ← Value.getField v "uuid" >>= FromJSON.parseJSON
        baseAddress := ← Value.getField v "baseAddress" >>= FromJSON.parseJSON
        size := ← Value.getField v "size" >>= FromJSON.parseJSON }

instance : ToJSON Module where
  toJSON p := Data.Json.object
    [ ("name", ToJSON.toJSON p.name), ("uuid", ToJSON.toJSON p.uuid)
    , ("baseAddress", ToJSON.toJSON p.baseAddress), ("size", ToJSON.toJSON p.size) ]

/-- An array of heap profile samples. -/
structure SamplingProfile where
  samples : List SamplingProfileNode
  modules : List Module
  deriving Repr, BEq, DecidableEq

instance : FromJSON SamplingProfile where
  parseJSON v := do
    .ok
      { samples := ← Value.getField v "samples" >>= FromJSON.parseJSON
        modules := ← Value.getField v "modules" >>= FromJSON.parseJSON }

instance : ToJSON SamplingProfile where
  toJSON p := Data.Json.object [("samples", ToJSON.toJSON p.samples), ("modules", ToJSON.toJSON p.modules)]

/-- Parameters of the `Memory.getDOMCounters` command. -/
structure PGetDOMCounters where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetDOMCounters where toJSON _ := .null

/-- Response of the `Memory.getDOMCounters` command. -/
structure GetDOMCounters where
  documents : Int
  nodes : Int
  jsEventListeners : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetDOMCounters where
  parseJSON v := do
    .ok
      { documents := ← Value.getField v "documents" >>= FromJSON.parseJSON
        nodes := ← Value.getField v "nodes" >>= FromJSON.parseJSON
        jsEventListeners := ← Value.getField v "jsEventListeners" >>= FromJSON.parseJSON }

instance : Command PGetDOMCounters where
  Response := GetDOMCounters
  commandName _ := "Memory.getDOMCounters"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Memory.prepareForLeakDetection` command. -/
structure PPrepareForLeakDetection where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PPrepareForLeakDetection where toJSON _ := .null

instance : Command PPrepareForLeakDetection where
  Response := Unit
  commandName _ := "Memory.prepareForLeakDetection"
  decodeResponse _ := .ok ()

/-- Parameters of the `Memory.forciblyPurgeJavaScriptMemory` command: simulates
    an OOM intervention by purging V8 memory. -/
structure PForciblyPurgeJavaScriptMemory where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PForciblyPurgeJavaScriptMemory where toJSON _ := .null

instance : Command PForciblyPurgeJavaScriptMemory where
  Response := Unit
  commandName _ := "Memory.forciblyPurgeJavaScriptMemory"
  decodeResponse _ := .ok ()

/-- Parameters of the `Memory.setPressureNotificationsSuppressed` command:
    enable/disable suppressing memory pressure notifications in all
    processes. -/
structure PSetPressureNotificationsSuppressed where
  /-- If `true`, memory pressure notifications will be suppressed. -/
  suppressed : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetPressureNotificationsSuppressed where
  toJSON p := Data.Json.object [("suppressed", ToJSON.toJSON p.suppressed)]

instance : Command PSetPressureNotificationsSuppressed where
  Response := Unit
  commandName _ := "Memory.setPressureNotificationsSuppressed"
  decodeResponse _ := .ok ()

/-- Parameters of the `Memory.simulatePressureNotification` command: simulates
    a memory pressure notification in all processes. -/
structure PSimulatePressureNotification where
  /-- Memory pressure level of the notification. -/
  level : PressureLevel
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSimulatePressureNotification where
  toJSON p := Data.Json.object [("level", ToJSON.toJSON p.level)]

instance : Command PSimulatePressureNotification where
  Response := Unit
  commandName _ := "Memory.simulatePressureNotification"
  decodeResponse _ := .ok ()

/-- Parameters of the `Memory.startSampling` command: starts collecting a
    native memory profile. -/
structure PStartSampling where
  /-- Average number of bytes between samples. -/
  samplingInterval : Option Int := none
  /-- Do not randomize intervals between samples. -/
  suppressRandomness : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStartSampling where
  toJSON p := Data.Json.object <|
    (p.samplingInterval.map fun v => ("samplingInterval", ToJSON.toJSON v)).toList
    ++ (p.suppressRandomness.map fun v => ("suppressRandomness", ToJSON.toJSON v)).toList

instance : Command PStartSampling where
  Response := Unit
  commandName _ := "Memory.startSampling"
  decodeResponse _ := .ok ()

/-- Parameters of the `Memory.stopSampling` command: stops collecting a native
    memory profile. -/
structure PStopSampling where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStopSampling where toJSON _ := .null

instance : Command PStopSampling where
  Response := Unit
  commandName _ := "Memory.stopSampling"
  decodeResponse _ := .ok ()

/-- Parameters of the `Memory.getAllTimeSamplingProfile` command: retrieves the
    native memory allocations profile collected since renderer process
    startup. -/
structure PGetAllTimeSamplingProfile where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetAllTimeSamplingProfile where toJSON _ := .null

/-- Response of the `Memory.getAllTimeSamplingProfile` command. -/
structure GetAllTimeSamplingProfile where
  profile : SamplingProfile
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetAllTimeSamplingProfile where
  parseJSON v := do .ok { profile := ← Value.getField v "profile" >>= FromJSON.parseJSON }

instance : Command PGetAllTimeSamplingProfile where
  Response := GetAllTimeSamplingProfile
  commandName _ := "Memory.getAllTimeSamplingProfile"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Memory.getBrowserSamplingProfile` command: retrieves the
    native memory allocations profile collected since browser process
    startup. -/
structure PGetBrowserSamplingProfile where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetBrowserSamplingProfile where toJSON _ := .null

/-- Response of the `Memory.getBrowserSamplingProfile` command. -/
structure GetBrowserSamplingProfile where
  profile : SamplingProfile
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetBrowserSamplingProfile where
  parseJSON v := do .ok { profile := ← Value.getField v "profile" >>= FromJSON.parseJSON }

instance : Command PGetBrowserSamplingProfile where
  Response := GetBrowserSamplingProfile
  commandName _ := "Memory.getBrowserSamplingProfile"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Memory.getSamplingProfile` command: retrieves the native
    memory allocations profile collected since the last `startSampling`
    call. -/
structure PGetSamplingProfile where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetSamplingProfile where toJSON _ := .null

/-- Response of the `Memory.getSamplingProfile` command. -/
structure GetSamplingProfile where
  profile : SamplingProfile
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetSamplingProfile where
  parseJSON v := do .ok { profile := ← Value.getField v "profile" >>= FromJSON.parseJSON }

instance : Command PGetSamplingProfile where
  Response := GetSamplingProfile
  commandName _ := "Memory.getSamplingProfile"
  decodeResponse := FromJSON.parseJSON

end CDP.Domains.Memory
