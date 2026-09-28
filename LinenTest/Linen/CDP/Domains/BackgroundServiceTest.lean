/-
  Tests for `Linen.CDP.Domains.BackgroundService`.
-/
import Linen.CDP.Domains.BackgroundService

open CDP.Domains.BackgroundService
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.BackgroundService

/-! ### ServiceName -/

#guard decodeAs "\"backgroundFetch\"" (α := ServiceName) = .ok .backgroundFetch
#guard decodeAs "\"periodicBackgroundSync\"" (α := ServiceName) = .ok .periodicBackgroundSync
#guard encode (ToJSON.toJSON ServiceName.pushMessaging) = "\"pushMessaging\""
#guard match decodeAs "\"nonsense\"" (α := ServiceName) with | .ok _ => false | .error _ => true

/-! ### EventMetadata -/

#guard decodeAs "{\"key\": \"k\", \"value\": \"v\"}" (α := EventMetadata) = .ok { key := "k", value := "v" }
#guard encode (ToJSON.toJSON ({ key := "k", value := "v" } : EventMetadata)) = "{\"key\":\"k\",\"value\":\"v\"}"

/-! ### BackgroundServiceEvent -/

#guard decodeAs
    ("{\"timestamp\": 1.0, \"origin\": \"https://example.com\", " ++
     "\"serviceWorkerRegistrationId\": \"sw1\", \"service\": \"backgroundFetch\", " ++
     "\"eventName\": \"fetch-success\", \"instanceId\": \"i1\", \"eventMetadata\": []}")
    (α := BackgroundServiceEvent)
  = .ok
    { timestamp := 1.0, origin := "https://example.com", serviceWorkerRegistrationId := "sw1"
      service := .backgroundFetch, eventName := "fetch-success", instanceId := "i1", eventMetadata := [] }

/-! ### Events -/

#guard Event.eventName (α := RecordingStateChanged) = "BackgroundService.recordingStateChanged"
#guard Event.eventName (α := BackgroundServiceEventReceived)
  = "BackgroundService.backgroundServiceEventReceived"

#guard decodeAs "{\"isRecording\": true, \"service\": \"notifications\"}" (α := RecordingStateChanged)
  = .ok { isRecording := true, service := .notifications }

#guard decodeAs
    ("{\"backgroundServiceEvent\": {\"timestamp\": 0.0, \"origin\": \"o\", " ++
     "\"serviceWorkerRegistrationId\": \"sw\", \"service\": \"backgroundSync\", " ++
     "\"eventName\": \"e\", \"instanceId\": \"i\", \"eventMetadata\": []}}")
    (α := BackgroundServiceEventReceived)
  = .ok
    { backgroundServiceEvent :=
        { timestamp := 0.0, origin := "o", serviceWorkerRegistrationId := "sw", service := .backgroundSync
          eventName := "e", instanceId := "i", eventMetadata := [] } }

/-! ### Commands -/

#guard Command.commandName ({ service := .backgroundFetch } : PStartObserving)
  = "BackgroundService.startObserving"
#guard encode (ToJSON.toJSON ({ service := .backgroundFetch } : PStartObserving)) = "{\"service\":\"backgroundFetch\"}"

#guard Command.commandName ({ service := .pushMessaging } : PStopObserving) = "BackgroundService.stopObserving"

#guard Command.commandName ({ shouldRecord := true, service := .notifications } : PSetRecording)
  = "BackgroundService.setRecording"
#guard encode (ToJSON.toJSON ({ shouldRecord := true, service := .notifications } : PSetRecording))
  = "{\"shouldRecord\":true,\"service\":\"notifications\"}"

#guard Command.commandName ({ service := .paymentHandler } : PClearEvents) = "BackgroundService.clearEvents"

end Tests.CDP.Domains.BackgroundService
