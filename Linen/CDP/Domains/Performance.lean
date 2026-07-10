/-
  Linen.CDP.Domains.Performance — the `Performance` CDP domain

  Ports `CDP.Domains.Performance` (see `docs/imports/cdp/dependencies.md`);
  naming conventions as in `CDP.Domains.CacheStorage`'s docstring.
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.Performance

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

/-- A run-time execution metric. -/
structure Metric where
  name : String
  value : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON Metric where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON }

instance : ToJSON Metric where
  toJSON p := Data.Json.object [("name", ToJSON.toJSON p.name), ("value", ToJSON.toJSON p.value)]

/-- The `Performance.metrics` event. -/
structure Metrics where
  /-- Current values of the metrics. -/
  metrics : List Metric
  /-- Timestamp title. -/
  title : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON Metrics where
  parseJSON v := do
    .ok
      { metrics := ← Value.getField v "metrics" >>= FromJSON.parseJSON
        title := ← Value.getField v "title" >>= FromJSON.parseJSON }

instance : Event Metrics where
  eventName := "Performance.metrics"

/-- Parameters of the `Performance.disable` command: disable collecting and
    reporting metrics. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "Performance.disable"
  decodeResponse _ := .ok ()

/-- Time domain to use for collecting and reporting duration metrics. -/
inductive PEnable.TimeDomain where
  | timeTicks | threadTicks
  deriving Repr, BEq, DecidableEq

instance : FromJSON PEnable.TimeDomain where
  parseJSON
    | .string "timeTicks" => .ok .timeTicks
    | .string "threadTicks" => .ok .threadTicks
    | v => .error s!"failed to parse PEnable.TimeDomain: {repr v}"

instance : ToJSON PEnable.TimeDomain where
  toJSON | .timeTicks => .string "timeTicks" | .threadTicks => .string "threadTicks"

/-- Parameters of the `Performance.enable` command: enable collecting and
    reporting metrics. -/
structure PEnable where
  /-- Time domain to use for collecting and reporting duration metrics. -/
  timeDomain : Option PEnable.TimeDomain := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where
  toJSON p := Data.Json.object ((p.timeDomain.map fun v => ("timeDomain", ToJSON.toJSON v)).toList)

instance : Command PEnable where
  Response := Unit
  commandName _ := "Performance.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Performance.getMetrics` command: retrieve current values
    of run-time metrics. -/
structure PGetMetrics where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetMetrics where toJSON _ := .null

/-- Response of the `Performance.getMetrics` command. -/
structure GetMetrics where
  /-- Current values for run-time metrics. -/
  metrics : List Metric
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetMetrics where
  parseJSON v := do .ok { metrics := ← Value.getField v "metrics" >>= FromJSON.parseJSON }

instance : Command PGetMetrics where
  Response := GetMetrics
  commandName _ := "Performance.getMetrics"
  decodeResponse := FromJSON.parseJSON

end CDP.Domains.Performance
