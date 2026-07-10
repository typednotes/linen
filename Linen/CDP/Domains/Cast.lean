/-
  Linen.CDP.Domains.Cast — the `Cast` CDP domain

  A domain for interacting with Cast, Presentation API, and Remote Playback API
  functionalities. Ports `CDP.Domains.Cast` (see
  `docs/imports/cdp/dependencies.md`); naming conventions as in
  `CDP.Domains.CacheStorage`'s docstring.
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.Cast

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

/-- A Cast sink (a Chromecast-compatible receiver device). -/
structure Sink where
  name : String
  id : String
  /-- Text describing the current session. Present only if there is an active
      session on the sink. -/
  session : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON Sink where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        id := ← Value.getField v "id" >>= FromJSON.parseJSON
        session := ← (← Value.getFieldOpt v "session").mapM FromJSON.parseJSON }

instance : ToJSON Sink where
  toJSON s := Data.Json.object <|
    [("name", ToJSON.toJSON s.name), ("id", ToJSON.toJSON s.id)]
    ++ (s.session.map fun v => ("session", ToJSON.toJSON v)).toList

/-- The `Cast.sinksUpdated` event. -/
structure SinksUpdated where
  sinks : List Sink
  deriving Repr, BEq, DecidableEq

instance : FromJSON SinksUpdated where
  parseJSON v := do .ok { sinks := ← Value.getField v "sinks" >>= FromJSON.parseJSON }

instance : Event SinksUpdated where
  eventName := "Cast.sinksUpdated"

/-- The `Cast.issueUpdated` event. -/
structure IssueUpdated where
  issueMessage : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON IssueUpdated where
  parseJSON v := do .ok { issueMessage := ← Value.getField v "issueMessage" >>= FromJSON.parseJSON }

instance : Event IssueUpdated where
  eventName := "Cast.issueUpdated"

/-- Parameters of the `Cast.enable` command: starts observing for sinks that can
    be used for tab mirroring, and if set, sinks compatible with
    `presentationUrl` as well. When sinks are found, a `sinksUpdated` event is
    fired. Also starts observing for issue messages: when an issue is added or
    removed, an `issueUpdated` event is fired. -/
structure PEnable where
  presentationUrl : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where
  toJSON p := Data.Json.object ((p.presentationUrl.map fun v => ("presentationUrl", ToJSON.toJSON v)).toList)

instance : Command PEnable where
  Response := Unit
  commandName _ := "Cast.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Cast.disable` command: stops observing for sinks and
    issues. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where
  toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "Cast.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Cast.setSinkToUse` command: sets a sink to be used when
    the web page requests the browser to choose a sink via Presentation API,
    Remote Playback API, or Cast SDK. -/
structure PSetSinkToUse where
  sinkName : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetSinkToUse where
  toJSON p := Data.Json.object [("sinkName", ToJSON.toJSON p.sinkName)]

instance : Command PSetSinkToUse where
  Response := Unit
  commandName _ := "Cast.setSinkToUse"
  decodeResponse _ := .ok ()

/-- Parameters of the `Cast.startDesktopMirroring` command: starts mirroring the
    desktop to the sink. -/
structure PStartDesktopMirroring where
  sinkName : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStartDesktopMirroring where
  toJSON p := Data.Json.object [("sinkName", ToJSON.toJSON p.sinkName)]

instance : Command PStartDesktopMirroring where
  Response := Unit
  commandName _ := "Cast.startDesktopMirroring"
  decodeResponse _ := .ok ()

/-- Parameters of the `Cast.startTabMirroring` command: starts mirroring the tab
    to the sink. -/
structure PStartTabMirroring where
  sinkName : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStartTabMirroring where
  toJSON p := Data.Json.object [("sinkName", ToJSON.toJSON p.sinkName)]

instance : Command PStartTabMirroring where
  Response := Unit
  commandName _ := "Cast.startTabMirroring"
  decodeResponse _ := .ok ()

/-- Parameters of the `Cast.stopCasting` command: stops the active Cast session
    on the sink. -/
structure PStopCasting where
  sinkName : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStopCasting where
  toJSON p := Data.Json.object [("sinkName", ToJSON.toJSON p.sinkName)]

instance : Command PStopCasting where
  Response := Unit
  commandName _ := "Cast.stopCasting"
  decodeResponse _ := .ok ()

end CDP.Domains.Cast
