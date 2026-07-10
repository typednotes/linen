/-
  Linen.CDP.Domains.ServiceWorker — the `ServiceWorker` CDP domain

  Ports `CDP.Domains.ServiceWorker` (see `docs/imports/cdp/dependencies.md`);
  naming conventions as in `CDP.Domains.CacheStorage`'s docstring. This module
  references `Target.TargetID` from `CDP.Domains.BrowserTarget` (upstream's
  `BrowserTarget.TargetTargetID`). None of this module's own types are self-
  or mutually-recursive, so no termination proofs are needed here.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.BrowserTarget

namespace CDP.Domains.ServiceWorker

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)
open CDP.Domains

/-- `ServiceWorker.RegistrationID`. -/
abbrev RegistrationID := String

/-- ServiceWorker registration. -/
structure ServiceWorkerRegistration where
  registrationId : RegistrationID
  scopeURL : String
  isDeleted : Bool
  deriving Repr, BEq, DecidableEq

instance : FromJSON ServiceWorkerRegistration where
  parseJSON v := do
    .ok
      { registrationId := ← Value.getField v "registrationId" >>= FromJSON.parseJSON
        scopeURL := ← Value.getField v "scopeURL" >>= FromJSON.parseJSON
        isDeleted := ← Value.getField v "isDeleted" >>= FromJSON.parseJSON }

instance : ToJSON ServiceWorkerRegistration where
  toJSON r := Data.Json.object
    [ ("registrationId", ToJSON.toJSON r.registrationId), ("scopeURL", ToJSON.toJSON r.scopeURL)
    , ("isDeleted", ToJSON.toJSON r.isDeleted) ]

/-- Running status of a `ServiceWorkerVersion`. -/
inductive ServiceWorkerVersionRunningStatus where
  | stopped | starting | running | stopping
  deriving Repr, BEq, DecidableEq

instance : FromJSON ServiceWorkerVersionRunningStatus where
  parseJSON
    | .string "stopped" => .ok .stopped
    | .string "starting" => .ok .starting
    | .string "running" => .ok .running
    | .string "stopping" => .ok .stopping
    | v => .error s!"failed to parse ServiceWorkerVersionRunningStatus: {repr v}"

instance : ToJSON ServiceWorkerVersionRunningStatus where
  toJSON
    | .stopped => .string "stopped"
    | .starting => .string "starting"
    | .running => .string "running"
    | .stopping => .string "stopping"

/-- Lifecycle status of a `ServiceWorkerVersion`. -/
inductive ServiceWorkerVersionStatus where
  | new | installing | installed | activating | activated | redundant
  deriving Repr, BEq, DecidableEq

instance : FromJSON ServiceWorkerVersionStatus where
  parseJSON
    | .string "new" => .ok .new
    | .string "installing" => .ok .installing
    | .string "installed" => .ok .installed
    | .string "activating" => .ok .activating
    | .string "activated" => .ok .activated
    | .string "redundant" => .ok .redundant
    | v => .error s!"failed to parse ServiceWorkerVersionStatus: {repr v}"

instance : ToJSON ServiceWorkerVersionStatus where
  toJSON
    | .new => .string "new"
    | .installing => .string "installing"
    | .installed => .string "installed"
    | .activating => .string "activating"
    | .activated => .string "activated"
    | .redundant => .string "redundant"

/-- ServiceWorker version. -/
structure ServiceWorkerVersion where
  versionId : String
  registrationId : RegistrationID
  scriptURL : String
  runningStatus : ServiceWorkerVersionRunningStatus
  status : ServiceWorkerVersionStatus
  /-- The Last-Modified header value of the main script. -/
  scriptLastModified : Option Float := none
  /-- The time at which the response headers of the main script were received
      from the server. For cached script it is the last time the cache entry
      was validated. -/
  scriptResponseTime : Option Float := none
  controlledClients : Option (List BrowserTarget.Target.TargetID) := none
  targetId : Option BrowserTarget.Target.TargetID := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON ServiceWorkerVersion where
  parseJSON v := do
    .ok
      { versionId := ← Value.getField v "versionId" >>= FromJSON.parseJSON
        registrationId := ← Value.getField v "registrationId" >>= FromJSON.parseJSON
        scriptURL := ← Value.getField v "scriptURL" >>= FromJSON.parseJSON
        runningStatus := ← Value.getField v "runningStatus" >>= FromJSON.parseJSON
        status := ← Value.getField v "status" >>= FromJSON.parseJSON
        scriptLastModified := ← (← Value.getFieldOpt v "scriptLastModified").mapM FromJSON.parseJSON
        scriptResponseTime := ← (← Value.getFieldOpt v "scriptResponseTime").mapM FromJSON.parseJSON
        controlledClients := ← (← Value.getFieldOpt v "controlledClients").mapM FromJSON.parseJSON
        targetId := ← (← Value.getFieldOpt v "targetId").mapM FromJSON.parseJSON }

instance : ToJSON ServiceWorkerVersion where
  toJSON p := Data.Json.object <|
    [ ("versionId", ToJSON.toJSON p.versionId), ("registrationId", ToJSON.toJSON p.registrationId)
    , ("scriptURL", ToJSON.toJSON p.scriptURL), ("runningStatus", ToJSON.toJSON p.runningStatus)
    , ("status", ToJSON.toJSON p.status) ]
    ++ (p.scriptLastModified.map fun v => ("scriptLastModified", ToJSON.toJSON v)).toList
    ++ (p.scriptResponseTime.map fun v => ("scriptResponseTime", ToJSON.toJSON v)).toList
    ++ (p.controlledClients.map fun v => ("controlledClients", ToJSON.toJSON v)).toList
    ++ (p.targetId.map fun v => ("targetId", ToJSON.toJSON v)).toList

/-- ServiceWorker error message. -/
structure ServiceWorkerErrorMessage where
  errorMessage : String
  registrationId : RegistrationID
  versionId : String
  sourceURL : String
  lineNumber : Int
  columnNumber : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON ServiceWorkerErrorMessage where
  parseJSON v := do
    .ok
      { errorMessage := ← Value.getField v "errorMessage" >>= FromJSON.parseJSON
        registrationId := ← Value.getField v "registrationId" >>= FromJSON.parseJSON
        versionId := ← Value.getField v "versionId" >>= FromJSON.parseJSON
        sourceURL := ← Value.getField v "sourceURL" >>= FromJSON.parseJSON
        lineNumber := ← Value.getField v "lineNumber" >>= FromJSON.parseJSON
        columnNumber := ← Value.getField v "columnNumber" >>= FromJSON.parseJSON }

instance : ToJSON ServiceWorkerErrorMessage where
  toJSON m := Data.Json.object
    [ ("errorMessage", ToJSON.toJSON m.errorMessage), ("registrationId", ToJSON.toJSON m.registrationId)
    , ("versionId", ToJSON.toJSON m.versionId), ("sourceURL", ToJSON.toJSON m.sourceURL)
    , ("lineNumber", ToJSON.toJSON m.lineNumber), ("columnNumber", ToJSON.toJSON m.columnNumber) ]

-- ── Events ──

/-- The `ServiceWorker.workerErrorReported` event. -/
structure WorkerErrorReported where
  errorMessage : ServiceWorkerErrorMessage
  deriving Repr, BEq, DecidableEq

instance : FromJSON WorkerErrorReported where
  parseJSON v := do .ok { errorMessage := ← Value.getField v "errorMessage" >>= FromJSON.parseJSON }

instance : Event WorkerErrorReported where
  eventName := "ServiceWorker.workerErrorReported"

/-- The `ServiceWorker.workerRegistrationUpdated` event. -/
structure WorkerRegistrationUpdated where
  registrations : List ServiceWorkerRegistration
  deriving Repr, BEq, DecidableEq

instance : FromJSON WorkerRegistrationUpdated where
  parseJSON v := do .ok { registrations := ← Value.getField v "registrations" >>= FromJSON.parseJSON }

instance : Event WorkerRegistrationUpdated where
  eventName := "ServiceWorker.workerRegistrationUpdated"

/-- The `ServiceWorker.workerVersionUpdated` event. -/
structure WorkerVersionUpdated where
  versions : List ServiceWorkerVersion
  deriving Repr, BEq, DecidableEq

instance : FromJSON WorkerVersionUpdated where
  parseJSON v := do .ok { versions := ← Value.getField v "versions" >>= FromJSON.parseJSON }

instance : Event WorkerVersionUpdated where
  eventName := "ServiceWorker.workerVersionUpdated"

-- ── deliverPushMessage ──

/-- Parameters of the `ServiceWorker.deliverPushMessage` command. -/
structure PDeliverPushMessage where
  origin : String
  registrationId : RegistrationID
  data : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDeliverPushMessage where
  toJSON p := Data.Json.object
    [ ("origin", ToJSON.toJSON p.origin), ("registrationId", ToJSON.toJSON p.registrationId)
    , ("data", ToJSON.toJSON p.data) ]

instance : Command PDeliverPushMessage where
  Response := Unit
  commandName _ := "ServiceWorker.deliverPushMessage"
  decodeResponse _ := .ok ()

-- ── disable ──

/-- Parameters of the `ServiceWorker.disable` command. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where
  toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "ServiceWorker.disable"
  decodeResponse _ := .ok ()

-- ── dispatchSyncEvent ──

/-- Parameters of the `ServiceWorker.dispatchSyncEvent` command. -/
structure PDispatchSyncEvent where
  origin : String
  registrationId : RegistrationID
  tag : String
  lastChance : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDispatchSyncEvent where
  toJSON p := Data.Json.object
    [ ("origin", ToJSON.toJSON p.origin), ("registrationId", ToJSON.toJSON p.registrationId)
    , ("tag", ToJSON.toJSON p.tag), ("lastChance", ToJSON.toJSON p.lastChance) ]

instance : Command PDispatchSyncEvent where
  Response := Unit
  commandName _ := "ServiceWorker.dispatchSyncEvent"
  decodeResponse _ := .ok ()

-- ── dispatchPeriodicSyncEvent ──

/-- Parameters of the `ServiceWorker.dispatchPeriodicSyncEvent` command. -/
structure PDispatchPeriodicSyncEvent where
  origin : String
  registrationId : RegistrationID
  tag : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDispatchPeriodicSyncEvent where
  toJSON p := Data.Json.object
    [ ("origin", ToJSON.toJSON p.origin), ("registrationId", ToJSON.toJSON p.registrationId)
    , ("tag", ToJSON.toJSON p.tag) ]

instance : Command PDispatchPeriodicSyncEvent where
  Response := Unit
  commandName _ := "ServiceWorker.dispatchPeriodicSyncEvent"
  decodeResponse _ := .ok ()

-- ── enable ──

/-- Parameters of the `ServiceWorker.enable` command. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where
  toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "ServiceWorker.enable"
  decodeResponse _ := .ok ()

-- ── inspectWorker ──

/-- Parameters of the `ServiceWorker.inspectWorker` command. -/
structure PInspectWorker where
  versionId : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PInspectWorker where
  toJSON p := Data.Json.object [("versionId", ToJSON.toJSON p.versionId)]

instance : Command PInspectWorker where
  Response := Unit
  commandName _ := "ServiceWorker.inspectWorker"
  decodeResponse _ := .ok ()

-- ── setForceUpdateOnPageLoad ──

/-- Parameters of the `ServiceWorker.setForceUpdateOnPageLoad` command. -/
structure PSetForceUpdateOnPageLoad where
  forceUpdateOnPageLoad : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetForceUpdateOnPageLoad where
  toJSON p := Data.Json.object [("forceUpdateOnPageLoad", ToJSON.toJSON p.forceUpdateOnPageLoad)]

instance : Command PSetForceUpdateOnPageLoad where
  Response := Unit
  commandName _ := "ServiceWorker.setForceUpdateOnPageLoad"
  decodeResponse _ := .ok ()

-- ── skipWaiting ──

/-- Parameters of the `ServiceWorker.skipWaiting` command. -/
structure PSkipWaiting where
  scopeURL : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSkipWaiting where
  toJSON p := Data.Json.object [("scopeURL", ToJSON.toJSON p.scopeURL)]

instance : Command PSkipWaiting where
  Response := Unit
  commandName _ := "ServiceWorker.skipWaiting"
  decodeResponse _ := .ok ()

-- ── startWorker ──

/-- Parameters of the `ServiceWorker.startWorker` command. -/
structure PStartWorker where
  scopeURL : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStartWorker where
  toJSON p := Data.Json.object [("scopeURL", ToJSON.toJSON p.scopeURL)]

instance : Command PStartWorker where
  Response := Unit
  commandName _ := "ServiceWorker.startWorker"
  decodeResponse _ := .ok ()

-- ── stopAllWorkers ──

/-- Parameters of the `ServiceWorker.stopAllWorkers` command. -/
structure PStopAllWorkers where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStopAllWorkers where
  toJSON _ := .null

instance : Command PStopAllWorkers where
  Response := Unit
  commandName _ := "ServiceWorker.stopAllWorkers"
  decodeResponse _ := .ok ()

-- ── stopWorker ──

/-- Parameters of the `ServiceWorker.stopWorker` command. -/
structure PStopWorker where
  versionId : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStopWorker where
  toJSON p := Data.Json.object [("versionId", ToJSON.toJSON p.versionId)]

instance : Command PStopWorker where
  Response := Unit
  commandName _ := "ServiceWorker.stopWorker"
  decodeResponse _ := .ok ()

-- ── unregister ──

/-- Parameters of the `ServiceWorker.unregister` command. -/
structure PUnregister where
  scopeURL : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PUnregister where
  toJSON p := Data.Json.object [("scopeURL", ToJSON.toJSON p.scopeURL)]

instance : Command PUnregister where
  Response := Unit
  commandName _ := "ServiceWorker.unregister"
  decodeResponse _ := .ok ()

-- ── updateRegistration ──

/-- Parameters of the `ServiceWorker.updateRegistration` command. -/
structure PUpdateRegistration where
  scopeURL : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PUpdateRegistration where
  toJSON p := Data.Json.object [("scopeURL", ToJSON.toJSON p.scopeURL)]

instance : Command PUpdateRegistration where
  Response := Unit
  commandName _ := "ServiceWorker.updateRegistration"
  decodeResponse _ := .ok ()

end CDP.Domains.ServiceWorker
