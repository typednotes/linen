/-
  Tests for `Linen.CDP.Domains.ServiceWorker`.
-/
import Linen.CDP.Domains.ServiceWorker

open CDP.Domains.ServiceWorker
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.ServiceWorker

/-! ### ServiceWorkerRegistration FromJSON/ToJSON -/

#guard decodeAs
    "{\"registrationId\": \"r1\", \"scopeURL\": \"https://x/\", \"isDeleted\": false}"
    (α := ServiceWorkerRegistration)
  = .ok { registrationId := "r1", scopeURL := "https://x/", isDeleted := false }
#guard encode (ToJSON.toJSON ({ registrationId := "r1", scopeURL := "https://x/", isDeleted := true }
    : ServiceWorkerRegistration))
  = "{\"registrationId\":\"r1\",\"scopeURL\":\"https:\\/\\/x\\/\",\"isDeleted\":true}"

/-! ### ServiceWorkerVersionRunningStatus / ServiceWorkerVersionStatus round-trip -/

#guard decodeAs "\"running\"" (α := ServiceWorkerVersionRunningStatus) = .ok .running
#guard encode (ToJSON.toJSON ServiceWorkerVersionRunningStatus.stopping) = "\"stopping\""
#guard decodeAs "\"redundant\"" (α := ServiceWorkerVersionStatus) = .ok .redundant
#guard encode (ToJSON.toJSON ServiceWorkerVersionStatus.activating) = "\"activating\""

/-! ### ServiceWorkerVersion — optional fields, incl. `Target.TargetID` -/

#guard decodeAs
    ("{\"versionId\": \"v1\", \"registrationId\": \"r1\", \"scriptURL\": \"https://x/sw.js\", " ++
     "\"runningStatus\": \"running\", \"status\": \"activated\", " ++
     "\"controlledClients\": [\"t1\", \"t2\"], \"targetId\": \"t1\"}")
    (α := ServiceWorkerVersion)
  = .ok
      { versionId := "v1", registrationId := "r1", scriptURL := "https://x/sw.js"
        runningStatus := .running, status := .activated
        controlledClients := some ["t1", "t2"], targetId := some "t1" }

#guard encode (ToJSON.toJSON
    ({ versionId := "v1", registrationId := "r1", scriptURL := "https://x/sw.js"
       runningStatus := .stopped, status := .new } : ServiceWorkerVersion))
  = ("{\"versionId\":\"v1\",\"registrationId\":\"r1\",\"scriptURL\":\"https:\\/\\/x\\/sw.js\"," ++
     "\"runningStatus\":\"stopped\",\"status\":\"new\"}")

/-! ### ServiceWorkerErrorMessage -/

#guard decodeAs
    ("{\"errorMessage\": \"boom\", \"registrationId\": \"r1\", \"versionId\": \"v1\", " ++
     "\"sourceURL\": \"https://x/sw.js\", \"lineNumber\": 3, \"columnNumber\": 7}")
    (α := ServiceWorkerErrorMessage)
  = .ok
      { errorMessage := "boom", registrationId := "r1", versionId := "v1"
        sourceURL := "https://x/sw.js", lineNumber := 3, columnNumber := 7 }

/-! ### Events -/

#guard decodeAs
    ("{\"errorMessage\": {\"errorMessage\": \"boom\", \"registrationId\": \"r1\", " ++
     "\"versionId\": \"v1\", \"sourceURL\": \"https://x/sw.js\", \"lineNumber\": 1, " ++
     "\"columnNumber\": 1}}")
    (α := WorkerErrorReported)
  = .ok
      { errorMessage :=
          { errorMessage := "boom", registrationId := "r1", versionId := "v1"
            sourceURL := "https://x/sw.js", lineNumber := 1, columnNumber := 1 } }
#guard Event.eventName (α := WorkerErrorReported) = "ServiceWorker.workerErrorReported"

#guard decodeAs "{\"registrations\": []}" (α := WorkerRegistrationUpdated)
  = .ok { registrations := [] }
#guard Event.eventName (α := WorkerRegistrationUpdated) = "ServiceWorker.workerRegistrationUpdated"

#guard decodeAs "{\"versions\": []}" (α := WorkerVersionUpdated) = .ok { versions := [] }
#guard Event.eventName (α := WorkerVersionUpdated) = "ServiceWorker.workerVersionUpdated"

/-! ### Commands with parameters and a `()` response -/

#guard Command.commandName ({ origin := "https://x", registrationId := "r1", data := "d" }
    : PDeliverPushMessage) = "ServiceWorker.deliverPushMessage"
#guard encode (ToJSON.toJSON ({ origin := "https://x", registrationId := "r1", data := "d" }
    : PDeliverPushMessage)) = "{\"origin\":\"https:\\/\\/x\",\"registrationId\":\"r1\",\"data\":\"d\"}"
#guard match Command.decodeResponse (α := PDeliverPushMessage) (.object []) with
  | .ok () => true | _ => false

#guard Command.commandName
    ({ origin := "https://x", registrationId := "r1", tag := "t", lastChance := true }
      : PDispatchSyncEvent)
  = "ServiceWorker.dispatchSyncEvent"

#guard Command.commandName ({ origin := "https://x", registrationId := "r1", tag := "t" }
    : PDispatchPeriodicSyncEvent) = "ServiceWorker.dispatchPeriodicSyncEvent"

/-! ### Parameterless commands -/

#guard Command.commandName ({} : PDisable) = "ServiceWorker.disable"
#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PEnable) = "ServiceWorker.enable"
#guard Command.commandName ({} : PStopAllWorkers) = "ServiceWorker.stopAllWorkers"

/-! ### Remaining single-field commands -/

#guard Command.commandName ({ versionId := "v1" } : PInspectWorker) = "ServiceWorker.inspectWorker"
#guard Command.commandName ({ forceUpdateOnPageLoad := true } : PSetForceUpdateOnPageLoad)
  = "ServiceWorker.setForceUpdateOnPageLoad"
#guard Command.commandName ({ scopeURL := "https://x/" } : PSkipWaiting) = "ServiceWorker.skipWaiting"
#guard Command.commandName ({ scopeURL := "https://x/" } : PStartWorker) = "ServiceWorker.startWorker"
#guard Command.commandName ({ versionId := "v1" } : PStopWorker) = "ServiceWorker.stopWorker"
#guard Command.commandName ({ scopeURL := "https://x/" } : PUnregister) = "ServiceWorker.unregister"
#guard Command.commandName ({ scopeURL := "https://x/" } : PUpdateRegistration)
  = "ServiceWorker.updateRegistration"

end Tests.CDP.Domains.ServiceWorker
