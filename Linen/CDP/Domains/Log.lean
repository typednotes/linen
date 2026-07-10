/-
  Linen.CDP.Domains.Log — the `Log` CDP domain

  Provides access to log entries. Ports `CDP.Domains.Log` (see
  `docs/imports/cdp/dependencies.md`); naming conventions as in
  `CDP.Domains.Memory`'s docstring.

  NOTE: the CDP domain is named `Log`. There is no `Log` namespace or
  identifier in Lean core, nor anywhere else already imported into this
  library (checked by grepping `Linen/` for `namespace Log`), so there is no
  actual name-resolution hazard — unlike `CDP.Domains.IO`, which does clash
  with core `IO` and documents the same non-issue in its header.

  None of this module's own types are self- or mutually-recursive, so no
  termination proofs are needed here.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.DOMPageNetworkEmulationSecurity
import Linen.CDP.Domains.Runtime

namespace CDP.Domains.Log

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

-- ── Types ──

/-- `LogEntry.source`: the origin of a log entry. -/
inductive LogEntrySource where
  | xml | javascript | network | storage | appcache | rendering | security
  | deprecation | worker | violation | intervention | recommendation | other
  deriving Repr, BEq, DecidableEq

instance : FromJSON LogEntrySource where
  parseJSON
    | .string "xml" => .ok .xml
    | .string "javascript" => .ok .javascript
    | .string "network" => .ok .network
    | .string "storage" => .ok .storage
    | .string "appcache" => .ok .appcache
    | .string "rendering" => .ok .rendering
    | .string "security" => .ok .security
    | .string "deprecation" => .ok .deprecation
    | .string "worker" => .ok .worker
    | .string "violation" => .ok .violation
    | .string "intervention" => .ok .intervention
    | .string "recommendation" => .ok .recommendation
    | .string "other" => .ok .other
    | v => .error s!"failed to parse LogEntrySource: {repr v}"

instance : ToJSON LogEntrySource where
  toJSON
    | .xml => .string "xml"
    | .javascript => .string "javascript"
    | .network => .string "network"
    | .storage => .string "storage"
    | .appcache => .string "appcache"
    | .rendering => .string "rendering"
    | .security => .string "security"
    | .deprecation => .string "deprecation"
    | .worker => .string "worker"
    | .violation => .string "violation"
    | .intervention => .string "intervention"
    | .recommendation => .string "recommendation"
    | .other => .string "other"

/-- `LogEntry.level`: the severity of a log entry. -/
inductive LogEntryLevel where
  | verbose | info | warning | error
  deriving Repr, BEq, DecidableEq

instance : FromJSON LogEntryLevel where
  parseJSON
    | .string "verbose" => .ok .verbose
    | .string "info" => .ok .info
    | .string "warning" => .ok .warning
    | .string "error" => .ok .error
    | v => .error s!"failed to parse LogEntryLevel: {repr v}"

instance : ToJSON LogEntryLevel where
  toJSON
    | .verbose => .string "verbose"
    | .info => .string "info"
    | .warning => .string "warning"
    | .error => .string "error"

/-- `LogEntry.category`. -/
inductive LogEntryCategory where
  | cors
  deriving Repr, BEq, DecidableEq

instance : FromJSON LogEntryCategory where
  parseJSON
    | .string "cors" => .ok .cors
    | v => .error s!"failed to parse LogEntryCategory: {repr v}"

instance : ToJSON LogEntryCategory where
  toJSON | .cors => .string "cors"

/-- A log entry. -/
structure LogEntry where
  /-- Log entry source. -/
  source : LogEntrySource
  /-- Log entry severity. -/
  level : LogEntryLevel
  /-- Logged text. -/
  text : String
  /-- The category of the log entry, if any. -/
  category : Option LogEntryCategory := none
  /-- Timestamp when this entry was added. -/
  timestamp : Runtime.Timestamp
  /-- URL of the resource if known. -/
  url : Option String := none
  /-- Line number in the resource. -/
  lineNumber : Option Int := none
  /-- JavaScript stack trace. -/
  stackTrace : Option Runtime.StackTrace := none
  /-- Identifier of the network request associated with this entry. -/
  networkRequestId : Option DOMPageNetworkEmulationSecurity.Network.RequestId := none
  /-- Identifier of the worker associated with this entry. -/
  workerId : Option String := none
  /-- Call arguments. -/
  args : Option (List Runtime.RemoteObject) := none
  deriving Repr, BEq

instance : FromJSON LogEntry where
  parseJSON v := do
    .ok
      { source := ← Value.getField v "source" >>= FromJSON.parseJSON
        level := ← Value.getField v "level" >>= FromJSON.parseJSON
        text := ← Value.getField v "text" >>= FromJSON.parseJSON
        category := ← (← Value.getFieldOpt v "category").mapM FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        url := ← (← Value.getFieldOpt v "url").mapM FromJSON.parseJSON
        lineNumber := ← (← Value.getFieldOpt v "lineNumber").mapM FromJSON.parseJSON
        stackTrace := ← (← Value.getFieldOpt v "stackTrace").mapM FromJSON.parseJSON
        networkRequestId := ← (← Value.getFieldOpt v "networkRequestId").mapM FromJSON.parseJSON
        workerId := ← (← Value.getFieldOpt v "workerId").mapM FromJSON.parseJSON
        args := ← (← Value.getFieldOpt v "args").mapM FromJSON.parseJSON }

instance : ToJSON LogEntry where
  toJSON p := Data.Json.object <|
       [("source", ToJSON.toJSON p.source)]
    ++ [("level", ToJSON.toJSON p.level)]
    ++ [("text", ToJSON.toJSON p.text)]
    ++ (p.category.map fun v => ("category", ToJSON.toJSON v)).toList
    ++ [("timestamp", ToJSON.toJSON p.timestamp)]
    ++ (p.url.map fun v => ("url", ToJSON.toJSON v)).toList
    ++ (p.lineNumber.map fun v => ("lineNumber", ToJSON.toJSON v)).toList
    ++ (p.stackTrace.map fun v => ("stackTrace", ToJSON.toJSON v)).toList
    ++ (p.networkRequestId.map fun v => ("networkRequestId", ToJSON.toJSON v)).toList
    ++ (p.workerId.map fun v => ("workerId", ToJSON.toJSON v)).toList
    ++ (p.args.map fun v => ("args", ToJSON.toJSON v)).toList

/-- `ViolationSetting.name`: the type of a violation. -/
inductive ViolationSettingName where
  | longTask | longLayout | blockedEvent | blockedParser | discouragedAPIUse
  | handler | recurringHandler
  deriving Repr, BEq, DecidableEq

instance : FromJSON ViolationSettingName where
  parseJSON
    | .string "longTask" => .ok .longTask
    | .string "longLayout" => .ok .longLayout
    | .string "blockedEvent" => .ok .blockedEvent
    | .string "blockedParser" => .ok .blockedParser
    | .string "discouragedAPIUse" => .ok .discouragedAPIUse
    | .string "handler" => .ok .handler
    | .string "recurringHandler" => .ok .recurringHandler
    | v => .error s!"failed to parse ViolationSettingName: {repr v}"

instance : ToJSON ViolationSettingName where
  toJSON
    | .longTask => .string "longTask"
    | .longLayout => .string "longLayout"
    | .blockedEvent => .string "blockedEvent"
    | .blockedParser => .string "blockedParser"
    | .discouragedAPIUse => .string "discouragedAPIUse"
    | .handler => .string "handler"
    | .recurringHandler => .string "recurringHandler"

/-- Violation configuration setting. -/
structure ViolationSetting where
  /-- Violation type. -/
  name : ViolationSettingName
  /-- Time threshold to trigger upon. -/
  threshold : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON ViolationSetting where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        threshold := ← Value.getField v "threshold" >>= FromJSON.parseJSON }

instance : ToJSON ViolationSetting where
  toJSON p := Data.Json.object
    [("name", ToJSON.toJSON p.name), ("threshold", ToJSON.toJSON p.threshold)]

-- ── Events ──

/-- Type of the `Log.entryAdded` event. -/
structure EntryAdded where
  /-- The entry. -/
  entry : LogEntry
  deriving Repr, BEq

instance : FromJSON EntryAdded where
  parseJSON v := do .ok { entry := ← Value.getField v "entry" >>= FromJSON.parseJSON }

instance : Event EntryAdded where
  eventName := "Log.entryAdded"

-- ── Commands ──

/-- Parameters of the `Log.clear` command: clears the log. -/
structure PClear where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PClear where toJSON _ := .null

instance : Command PClear where
  Response := Unit
  commandName _ := "Log.clear"
  decodeResponse _ := .ok ()

/-- Parameters of the `Log.disable` command: disables log domain, prevents
    further log entries from being reported to the client. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "Log.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Log.enable` command: enables log domain, sends the
    entries collected so far to the client by means of the `entryAdded`
    notification. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "Log.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Log.startViolationsReport` command: start violation
    reporting. -/
structure PStartViolationsReport where
  /-- Configuration for violations. -/
  config : List ViolationSetting
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStartViolationsReport where
  toJSON p := Data.Json.object [("config", ToJSON.toJSON p.config)]

instance : Command PStartViolationsReport where
  Response := Unit
  commandName _ := "Log.startViolationsReport"
  decodeResponse _ := .ok ()

/-- Parameters of the `Log.stopViolationsReport` command: stop violation
    reporting. -/
structure PStopViolationsReport where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStopViolationsReport where toJSON _ := .null

instance : Command PStopViolationsReport where
  Response := Unit
  commandName _ := "Log.stopViolationsReport"
  decodeResponse _ := .ok ()

end CDP.Domains.Log
