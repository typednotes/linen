/-
  Linen.CDP.Domains.BackgroundService — the `BackgroundService` CDP domain

  Defines events for background web platform features. Ports
  `CDP.Domains.BackgroundService` (see `docs/imports/cdp/dependencies.md`);
  naming conventions as in `CDP.Domains.CacheStorage`'s docstring. Qualified
  references into the merged `DOM`/`Page`/`Network`/`Emulation`/`Security`
  domain follow `Linen.CDP.Domains.DOMPageNetworkEmulationSecurity`'s
  nested-namespace convention (`Network.TimeSinceEpoch`); the Service Worker
  registration id is `CDP.Domains.ServiceWorker.RegistrationID`. None of this
  module's own types are self- or mutually-recursive, so no termination
  proofs are needed here.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.DOMPageNetworkEmulationSecurity
import Linen.CDP.Domains.ServiceWorker

namespace CDP.Domains.BackgroundService

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)
open CDP.Domains

-- ── Types ──

/-- The Background Service that will be associated with the commands/events.
    Every Background Service operates independently, but they share the same
    API. -/
inductive ServiceName where
  | backgroundFetch | backgroundSync | pushMessaging | notifications | paymentHandler
  | periodicBackgroundSync
  deriving Repr, BEq, DecidableEq

instance : FromJSON ServiceName where
  parseJSON
    | .string "backgroundFetch" => .ok .backgroundFetch
    | .string "backgroundSync" => .ok .backgroundSync
    | .string "pushMessaging" => .ok .pushMessaging
    | .string "notifications" => .ok .notifications
    | .string "paymentHandler" => .ok .paymentHandler
    | .string "periodicBackgroundSync" => .ok .periodicBackgroundSync
    | v => .error s!"failed to parse ServiceName: {repr v}"

instance : ToJSON ServiceName where
  toJSON
    | .backgroundFetch => .string "backgroundFetch"
    | .backgroundSync => .string "backgroundSync"
    | .pushMessaging => .string "pushMessaging"
    | .notifications => .string "notifications"
    | .paymentHandler => .string "paymentHandler"
    | .periodicBackgroundSync => .string "periodicBackgroundSync"

/-- A key-value pair for additional event information to pass along. -/
structure EventMetadata where
  key : String
  value : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON EventMetadata where
  parseJSON v := do
    .ok
      { key := ← Value.getField v "key" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON }

instance : ToJSON EventMetadata where
  toJSON e := Data.Json.object [("key", ToJSON.toJSON e.key), ("value", ToJSON.toJSON e.value)]

/-- A `BackgroundService.BackgroundServiceEvent`. -/
structure BackgroundServiceEvent where
  /-- Timestamp of the event (in seconds). -/
  timestamp : DOMPageNetworkEmulationSecurity.Network.TimeSinceEpoch
  /-- The origin this event belongs to. -/
  origin : String
  /-- The Service Worker ID that initiated the event. -/
  serviceWorkerRegistrationId : ServiceWorker.RegistrationID
  /-- The Background Service this event belongs to. -/
  service : ServiceName
  /-- A description of the event. -/
  eventName : String
  /-- An identifier that groups related events together. -/
  instanceId : String
  /-- A list of event-specific information. -/
  eventMetadata : List EventMetadata
  deriving Repr, BEq, DecidableEq

instance : FromJSON BackgroundServiceEvent where
  parseJSON v := do
    .ok
      { timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        origin := ← Value.getField v "origin" >>= FromJSON.parseJSON
        serviceWorkerRegistrationId := ← Value.getField v "serviceWorkerRegistrationId" >>= FromJSON.parseJSON
        service := ← Value.getField v "service" >>= FromJSON.parseJSON
        eventName := ← Value.getField v "eventName" >>= FromJSON.parseJSON
        instanceId := ← Value.getField v "instanceId" >>= FromJSON.parseJSON
        eventMetadata := ← Value.getField v "eventMetadata" >>= FromJSON.parseJSON }

instance : ToJSON BackgroundServiceEvent where
  toJSON e := Data.Json.object
    [ ("timestamp", ToJSON.toJSON e.timestamp), ("origin", ToJSON.toJSON e.origin)
    , ("serviceWorkerRegistrationId", ToJSON.toJSON e.serviceWorkerRegistrationId)
    , ("service", ToJSON.toJSON e.service), ("eventName", ToJSON.toJSON e.eventName)
    , ("instanceId", ToJSON.toJSON e.instanceId), ("eventMetadata", ToJSON.toJSON e.eventMetadata) ]

-- ── Events ──

/-- The `BackgroundService.recordingStateChanged` event. -/
structure RecordingStateChanged where
  isRecording : Bool
  service : ServiceName
  deriving Repr, BEq, DecidableEq

instance : FromJSON RecordingStateChanged where
  parseJSON v := do
    .ok
      { isRecording := ← Value.getField v "isRecording" >>= FromJSON.parseJSON
        service := ← Value.getField v "service" >>= FromJSON.parseJSON }

instance : Event RecordingStateChanged where
  eventName := "BackgroundService.recordingStateChanged"

/-- The `BackgroundService.backgroundServiceEventReceived` event. -/
structure BackgroundServiceEventReceived where
  backgroundServiceEvent : BackgroundServiceEvent
  deriving Repr, BEq, DecidableEq

instance : FromJSON BackgroundServiceEventReceived where
  parseJSON v := do
    .ok { backgroundServiceEvent := ← Value.getField v "backgroundServiceEvent" >>= FromJSON.parseJSON }

instance : Event BackgroundServiceEventReceived where
  eventName := "BackgroundService.backgroundServiceEventReceived"

-- ── Commands ──

/-- Parameters of the `BackgroundService.startObserving` command: enables event
    updates for the service. -/
structure PStartObserving where
  service : ServiceName
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStartObserving where
  toJSON p := Data.Json.object [("service", ToJSON.toJSON p.service)]

instance : Command PStartObserving where
  Response := Unit
  commandName _ := "BackgroundService.startObserving"
  decodeResponse _ := .ok ()

/-- Parameters of the `BackgroundService.stopObserving` command: disables
    event updates for the service. -/
structure PStopObserving where
  service : ServiceName
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStopObserving where
  toJSON p := Data.Json.object [("service", ToJSON.toJSON p.service)]

instance : Command PStopObserving where
  Response := Unit
  commandName _ := "BackgroundService.stopObserving"
  decodeResponse _ := .ok ()

/-- Parameters of the `BackgroundService.setRecording` command: set the
    recording state for the service. -/
structure PSetRecording where
  shouldRecord : Bool
  service : ServiceName
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetRecording where
  toJSON p := Data.Json.object [("shouldRecord", ToJSON.toJSON p.shouldRecord), ("service", ToJSON.toJSON p.service)]

instance : Command PSetRecording where
  Response := Unit
  commandName _ := "BackgroundService.setRecording"
  decodeResponse _ := .ok ()

/-- Parameters of the `BackgroundService.clearEvents` command: clears all
    stored data for the service. -/
structure PClearEvents where
  service : ServiceName
  deriving Repr, BEq, DecidableEq

instance : ToJSON PClearEvents where
  toJSON p := Data.Json.object [("service", ToJSON.toJSON p.service)]

instance : Command PClearEvents where
  Response := Unit
  commandName _ := "BackgroundService.clearEvents"
  decodeResponse _ := .ok ()

end CDP.Domains.BackgroundService
