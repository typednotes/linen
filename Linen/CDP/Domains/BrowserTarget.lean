/-
  Linen.CDP.Domains.BrowserTarget — the `Browser` and `Target` CDP domains.

  Ports `CDP.Domains.BrowserTarget` from cdp-hs, which bundles these two
  mutually-referential domains into a single module (`Browser` defines
  methods and events for managing the browser itself; `Target` supports
  discovering and attaching to additional targets, e.g. tabs). The two are
  kept in one Lean module (mirroring upstream) and separated into the nested
  namespaces `Browser` and `Target` under `CDP.Domains.BrowserTarget`,
  following the convention documented in
  `CDP.Domains.DOMPageNetworkEmulationSecurity`. Upstream's flat,
  domain-prefixed names (`browserWindowID`, `targetTargetID`, …) become
  `Browser.WindowID`, `Target.TargetID`, …; command-parameter records keep
  their `P` prefix (`Browser.PClose`, `Target.PCreateTarget`, …), matching the
  `CDP.Domains.Memory` convention.

  Because Lean elaborates top-to-bottom and forbids forward references, the
  declarations are emitted in a single topological order across both domains
  rather than domain-by-domain: `Target.TargetID` (needed by
  `Browser.PGetWindowForTarget`) and `Browser.BrowserContextID` (needed by
  `Target.TargetInfo` and several `Target` commands) are both declared up
  front, before either domain's larger types.

  This module references `Page.FrameId` from
  `CDP.Domains.DOMPageNetworkEmulationSecurity`. None of this module's own
  types are self- or mutually-recursive, so no termination proofs are needed
  here.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.DOMPageNetworkEmulationSecurity

namespace CDP.Domains.BrowserTarget

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

-- ── Identifiers shared across both domains ──

/-- `Browser.BrowserContextID`. -/
abbrev Browser.BrowserContextID := String

/-- `Browser.WindowID`. -/
abbrev Browser.WindowID := Int

/-- `Target.TargetID`. -/
abbrev Target.TargetID := String

/-- `Target.SessionID`: unique identifier of an attached debugging
    session. -/
abbrev Target.SessionID := String

-- ── Browser: leaf types ──

/-- `Browser.WindowState`: the state of the browser window. -/
inductive Browser.WindowState where
  | normal | minimized | maximized | fullscreen
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.WindowState where
  parseJSON
    | .string "normal" => .ok .normal
    | .string "minimized" => .ok .minimized
    | .string "maximized" => .ok .maximized
    | .string "fullscreen" => .ok .fullscreen
    | v => .error s!"failed to parse Browser.WindowState: {repr v}"

instance : ToJSON Browser.WindowState where
  toJSON
    | .normal => .string "normal" | .minimized => .string "minimized"
    | .maximized => .string "maximized" | .fullscreen => .string "fullscreen"

/-- `Browser.Bounds`: browser window bounds information. -/
structure Browser.Bounds where
  /-- The offset from the left edge of the screen to the window in
      pixels. -/
  left : Option Int := none
  /-- The offset from the top edge of the screen to the window in
      pixels. -/
  top : Option Int := none
  /-- The window width in pixels. -/
  width : Option Int := none
  /-- The window height in pixels. -/
  height : Option Int := none
  /-- The window state. Default to normal. -/
  windowState : Option Browser.WindowState := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.Bounds where
  parseJSON v := do
    .ok
      { left := ← (← Value.getFieldOpt v "left").mapM FromJSON.parseJSON
        top := ← (← Value.getFieldOpt v "top").mapM FromJSON.parseJSON
        width := ← (← Value.getFieldOpt v "width").mapM FromJSON.parseJSON
        height := ← (← Value.getFieldOpt v "height").mapM FromJSON.parseJSON
        windowState := ← (← Value.getFieldOpt v "windowState").mapM FromJSON.parseJSON }

instance : ToJSON Browser.Bounds where
  toJSON p := Data.Json.object <|
       (p.left.map fun v => ("left", ToJSON.toJSON v)).toList
    ++ (p.top.map fun v => ("top", ToJSON.toJSON v)).toList
    ++ (p.width.map fun v => ("width", ToJSON.toJSON v)).toList
    ++ (p.height.map fun v => ("height", ToJSON.toJSON v)).toList
    ++ (p.windowState.map fun v => ("windowState", ToJSON.toJSON v)).toList

/-- `Browser.PermissionType`. -/
inductive Browser.PermissionType where
  | accessibilityEvents | audioCapture | backgroundSync | backgroundFetch
  | clipboardReadWrite | clipboardSanitizedWrite | displayCapture
  | durableStorage | flash | geolocation | midi | midiSysex | nfc
  | notifications | paymentHandler | periodicBackgroundSync
  | protectedMediaIdentifier | sensors | videoCapture
  | videoCapturePanTiltZoom | idleDetection | wakeLockScreen | wakeLockSystem
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.PermissionType where
  parseJSON
    | .string "accessibilityEvents" => .ok .accessibilityEvents
    | .string "audioCapture" => .ok .audioCapture
    | .string "backgroundSync" => .ok .backgroundSync
    | .string "backgroundFetch" => .ok .backgroundFetch
    | .string "clipboardReadWrite" => .ok .clipboardReadWrite
    | .string "clipboardSanitizedWrite" => .ok .clipboardSanitizedWrite
    | .string "displayCapture" => .ok .displayCapture
    | .string "durableStorage" => .ok .durableStorage
    | .string "flash" => .ok .flash
    | .string "geolocation" => .ok .geolocation
    | .string "midi" => .ok .midi
    | .string "midiSysex" => .ok .midiSysex
    | .string "nfc" => .ok .nfc
    | .string "notifications" => .ok .notifications
    | .string "paymentHandler" => .ok .paymentHandler
    | .string "periodicBackgroundSync" => .ok .periodicBackgroundSync
    | .string "protectedMediaIdentifier" => .ok .protectedMediaIdentifier
    | .string "sensors" => .ok .sensors
    | .string "videoCapture" => .ok .videoCapture
    | .string "videoCapturePanTiltZoom" => .ok .videoCapturePanTiltZoom
    | .string "idleDetection" => .ok .idleDetection
    | .string "wakeLockScreen" => .ok .wakeLockScreen
    | .string "wakeLockSystem" => .ok .wakeLockSystem
    | v => .error s!"failed to parse Browser.PermissionType: {repr v}"

instance : ToJSON Browser.PermissionType where
  toJSON
    | .accessibilityEvents => .string "accessibilityEvents"
    | .audioCapture => .string "audioCapture"
    | .backgroundSync => .string "backgroundSync"
    | .backgroundFetch => .string "backgroundFetch"
    | .clipboardReadWrite => .string "clipboardReadWrite"
    | .clipboardSanitizedWrite => .string "clipboardSanitizedWrite"
    | .displayCapture => .string "displayCapture"
    | .durableStorage => .string "durableStorage"
    | .flash => .string "flash"
    | .geolocation => .string "geolocation"
    | .midi => .string "midi"
    | .midiSysex => .string "midiSysex"
    | .nfc => .string "nfc"
    | .notifications => .string "notifications"
    | .paymentHandler => .string "paymentHandler"
    | .periodicBackgroundSync => .string "periodicBackgroundSync"
    | .protectedMediaIdentifier => .string "protectedMediaIdentifier"
    | .sensors => .string "sensors"
    | .videoCapture => .string "videoCapture"
    | .videoCapturePanTiltZoom => .string "videoCapturePanTiltZoom"
    | .idleDetection => .string "idleDetection"
    | .wakeLockScreen => .string "wakeLockScreen"
    | .wakeLockSystem => .string "wakeLockSystem"

/-- `Browser.PermissionSetting`. -/
inductive Browser.PermissionSetting where
  | granted | denied | prompt
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.PermissionSetting where
  parseJSON
    | .string "granted" => .ok .granted
    | .string "denied" => .ok .denied
    | .string "prompt" => .ok .prompt
    | v => .error s!"failed to parse Browser.PermissionSetting: {repr v}"

instance : ToJSON Browser.PermissionSetting where
  toJSON | .granted => .string "granted" | .denied => .string "denied" | .prompt => .string "prompt"

/-- `Browser.PermissionDescriptor`: definition of `PermissionDescriptor`
    defined in the Permissions API,
    <https://w3c.github.io/permissions/#dictdef-permissiondescriptor>. -/
structure Browser.PermissionDescriptor where
  /-- Name of permission. See
      <https://cs.chromium.org/chromium/src/third_party/blink/renderer/modules/permissions/permission_descriptor.idl>
      for valid permission names. -/
  name : String
  /-- For "midi" permission, may also specify sysex control. -/
  sysex : Option Bool := none
  /-- For "push" permission, may specify `userVisibleOnly`. Note that
      `userVisibleOnly = true` is the only currently supported type. -/
  userVisibleOnly : Option Bool := none
  /-- For "clipboard" permission, may specify `allowWithoutSanitization`. -/
  allowWithoutSanitization : Option Bool := none
  /-- For "camera" permission, may specify `panTiltZoom`. -/
  panTiltZoom : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.PermissionDescriptor where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        sysex := ← (← Value.getFieldOpt v "sysex").mapM FromJSON.parseJSON
        userVisibleOnly := ← (← Value.getFieldOpt v "userVisibleOnly").mapM FromJSON.parseJSON
        allowWithoutSanitization :=
          ← (← Value.getFieldOpt v "allowWithoutSanitization").mapM FromJSON.parseJSON
        panTiltZoom := ← (← Value.getFieldOpt v "panTiltZoom").mapM FromJSON.parseJSON }

instance : ToJSON Browser.PermissionDescriptor where
  toJSON p := Data.Json.object <|
       [("name", ToJSON.toJSON p.name)]
    ++ (p.sysex.map fun v => ("sysex", ToJSON.toJSON v)).toList
    ++ (p.userVisibleOnly.map fun v => ("userVisibleOnly", ToJSON.toJSON v)).toList
    ++ (p.allowWithoutSanitization.map fun v => ("allowWithoutSanitization", ToJSON.toJSON v)).toList
    ++ (p.panTiltZoom.map fun v => ("panTiltZoom", ToJSON.toJSON v)).toList

/-- `Browser.BrowserCommandId`: browser command ids used by
    `Browser.executeBrowserCommand`. -/
inductive Browser.BrowserCommandId where
  | openTabSearch | closeTabSearch
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.BrowserCommandId where
  parseJSON
    | .string "openTabSearch" => .ok .openTabSearch
    | .string "closeTabSearch" => .ok .closeTabSearch
    | v => .error s!"failed to parse Browser.BrowserCommandId: {repr v}"

instance : ToJSON Browser.BrowserCommandId where
  toJSON | .openTabSearch => .string "openTabSearch" | .closeTabSearch => .string "closeTabSearch"

/-- `Browser.Bucket`: a Chrome histogram bucket. -/
structure Browser.Bucket where
  /-- Minimum value (inclusive). -/
  low : Int
  /-- Maximum value (exclusive). -/
  high : Int
  /-- Number of samples. -/
  count : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.Bucket where
  parseJSON v := do
    .ok
      { low := ← Value.getField v "low" >>= FromJSON.parseJSON
        high := ← Value.getField v "high" >>= FromJSON.parseJSON
        count := ← Value.getField v "count" >>= FromJSON.parseJSON }

instance : ToJSON Browser.Bucket where
  toJSON p := Data.Json.object
    [("low", ToJSON.toJSON p.low), ("high", ToJSON.toJSON p.high), ("count", ToJSON.toJSON p.count)]

/-- `Browser.Histogram`: a Chrome histogram. -/
structure Browser.Histogram where
  /-- Name. -/
  name : String
  /-- Sum of sample values. -/
  sum : Int
  /-- Total number of samples. -/
  count : Int
  /-- Buckets. -/
  buckets : List Browser.Bucket
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.Histogram where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        sum := ← Value.getField v "sum" >>= FromJSON.parseJSON
        count := ← Value.getField v "count" >>= FromJSON.parseJSON
        buckets := ← Value.getField v "buckets" >>= FromJSON.parseJSON }

instance : ToJSON Browser.Histogram where
  toJSON p := Data.Json.object
    [ ("name", ToJSON.toJSON p.name), ("sum", ToJSON.toJSON p.sum), ("count", ToJSON.toJSON p.count)
    , ("buckets", ToJSON.toJSON p.buckets) ]

-- ── Target: leaf types ──

/-- `Target.FilterEntry`: a filter used by target query/discovery/auto-attach
    operations. -/
structure Target.FilterEntry where
  /-- If set, causes exclusion of matching targets from the list. -/
  exclude : Option Bool := none
  /-- If not present, matches any type. -/
  type : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.FilterEntry where
  parseJSON v := do
    .ok
      { exclude := ← (← Value.getFieldOpt v "exclude").mapM FromJSON.parseJSON
        type := ← (← Value.getFieldOpt v "type").mapM FromJSON.parseJSON }

instance : ToJSON Target.FilterEntry where
  toJSON p := Data.Json.object <|
       (p.exclude.map fun v => ("exclude", ToJSON.toJSON v)).toList
    ++ (p.type.map fun v => ("type", ToJSON.toJSON v)).toList

/-- `Target.TargetFilter`: the entries in `TargetFilter` are matched
    sequentially against targets and the first entry that matches determines
    if the target is included or not, depending on the value of the
    `exclude` field in the entry. If filter is not specified, the one
    assumed is `[{type: "browser", exclude: true}, {type: "tab", exclude:
    true}, {}]` (i.e. include everything but `browser` and `tab`). -/
abbrev Target.TargetFilter := List Target.FilterEntry

/-- `Target.RemoteLocation`. -/
structure Target.RemoteLocation where
  host : String
  port : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.RemoteLocation where
  parseJSON v := do
    .ok
      { host := ← Value.getField v "host" >>= FromJSON.parseJSON
        port := ← Value.getField v "port" >>= FromJSON.parseJSON }

instance : ToJSON Target.RemoteLocation where
  toJSON p := Data.Json.object [("host", ToJSON.toJSON p.host), ("port", ToJSON.toJSON p.port)]

/-- `Target.TargetInfo`. -/
structure Target.TargetInfo where
  targetId : Target.TargetID
  type : String
  title : String
  url : String
  /-- Whether the target has an attached client. -/
  attached : Bool
  /-- Opener target id. -/
  openerId : Option Target.TargetID := none
  /-- Whether the target has access to the originating window. -/
  canAccessOpener : Bool
  /-- Frame id of originating window (is only set if target has an
      opener). -/
  openerFrameId : Option DOMPageNetworkEmulationSecurity.Page.FrameId := none
  browserContextId : Option Browser.BrowserContextID := none
  /-- Provides additional details for specific target types. For example,
      for the type of "page", this may be set to "portal" or
      "prerender". -/
  subtype : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.TargetInfo where
  parseJSON v := do
    .ok
      { targetId := ← Value.getField v "targetId" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON
        title := ← Value.getField v "title" >>= FromJSON.parseJSON
        url := ← Value.getField v "url" >>= FromJSON.parseJSON
        attached := ← Value.getField v "attached" >>= FromJSON.parseJSON
        openerId := ← (← Value.getFieldOpt v "openerId").mapM FromJSON.parseJSON
        canAccessOpener := ← Value.getField v "canAccessOpener" >>= FromJSON.parseJSON
        openerFrameId := ← (← Value.getFieldOpt v "openerFrameId").mapM FromJSON.parseJSON
        browserContextId := ← (← Value.getFieldOpt v "browserContextId").mapM FromJSON.parseJSON
        subtype := ← (← Value.getFieldOpt v "subtype").mapM FromJSON.parseJSON }

instance : ToJSON Target.TargetInfo where
  toJSON p := Data.Json.object <|
       [ ("targetId", ToJSON.toJSON p.targetId), ("type", ToJSON.toJSON p.type)
       , ("title", ToJSON.toJSON p.title), ("url", ToJSON.toJSON p.url)
       , ("attached", ToJSON.toJSON p.attached) ]
    ++ (p.openerId.map fun v => ("openerId", ToJSON.toJSON v)).toList
    ++ [("canAccessOpener", ToJSON.toJSON p.canAccessOpener)]
    ++ (p.openerFrameId.map fun v => ("openerFrameId", ToJSON.toJSON v)).toList
    ++ (p.browserContextId.map fun v => ("browserContextId", ToJSON.toJSON v)).toList
    ++ (p.subtype.map fun v => ("subtype", ToJSON.toJSON v)).toList

-- ── Browser: events ──

/-- The `Browser.downloadWillBegin` event. -/
structure Browser.DownloadWillBegin where
  /-- Id of the frame that caused the download to begin. -/
  frameId : DOMPageNetworkEmulationSecurity.Page.FrameId
  /-- Global unique identifier of the download. -/
  guid : String
  /-- URL of the resource being downloaded. -/
  url : String
  /-- Suggested file name of the resource (the actual name of the file saved
      on disk may differ). -/
  suggestedFilename : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.DownloadWillBegin where
  parseJSON v := do
    .ok
      { frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        guid := ← Value.getField v "guid" >>= FromJSON.parseJSON
        url := ← Value.getField v "url" >>= FromJSON.parseJSON
        suggestedFilename := ← Value.getField v "suggestedFilename" >>= FromJSON.parseJSON }

instance : Event Browser.DownloadWillBegin where
  eventName := "Browser.downloadWillBegin"

/-- `Browser.DownloadProgressState`, part of the `Browser.downloadProgress`
    event. -/
inductive Browser.DownloadProgressState where
  | inProgress | completed | canceled
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.DownloadProgressState where
  parseJSON
    | .string "inProgress" => .ok .inProgress
    | .string "completed" => .ok .completed
    | .string "canceled" => .ok .canceled
    | v => .error s!"failed to parse Browser.DownloadProgressState: {repr v}"

instance : ToJSON Browser.DownloadProgressState where
  toJSON
    | .inProgress => .string "inProgress" | .completed => .string "completed"
    | .canceled => .string "canceled"

/-- The `Browser.downloadProgress` event. -/
structure Browser.DownloadProgress where
  /-- Global unique identifier of the download. -/
  guid : String
  /-- Total expected bytes to download. -/
  totalBytes : Float
  /-- Total bytes received. -/
  receivedBytes : Float
  /-- Download status. -/
  state : Browser.DownloadProgressState
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.DownloadProgress where
  parseJSON v := do
    .ok
      { guid := ← Value.getField v "guid" >>= FromJSON.parseJSON
        totalBytes := ← Value.getField v "totalBytes" >>= FromJSON.parseJSON
        receivedBytes := ← Value.getField v "receivedBytes" >>= FromJSON.parseJSON
        state := ← Value.getField v "state" >>= FromJSON.parseJSON }

instance : Event Browser.DownloadProgress where
  eventName := "Browser.downloadProgress"

-- ── Browser: commands ──

/-- Parameters of the `Browser.setPermission` command: sets permission
    settings for a given origin. -/
structure Browser.PSetPermission where
  /-- Descriptor of permission to override. -/
  permission : Browser.PermissionDescriptor
  /-- Setting of the permission. -/
  setting : Browser.PermissionSetting
  /-- Origin the permission applies to, all origins if not specified. -/
  origin : Option String := none
  /-- Context to override. When omitted, default browser context is
      used. -/
  browserContextId : Option Browser.BrowserContextID := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PSetPermission where
  toJSON p := Data.Json.object <|
       [("permission", ToJSON.toJSON p.permission), ("setting", ToJSON.toJSON p.setting)]
    ++ (p.origin.map fun v => ("origin", ToJSON.toJSON v)).toList
    ++ (p.browserContextId.map fun v => ("browserContextId", ToJSON.toJSON v)).toList

instance : Command Browser.PSetPermission where
  Response := Unit
  commandName _ := "Browser.setPermission"
  decodeResponse _ := .ok ()

/-- Parameters of the `Browser.grantPermissions` command: grants specific
    permissions to the given origin and rejects all others. -/
structure Browser.PGrantPermissions where
  permissions : List Browser.PermissionType
  /-- Origin the permission applies to, all origins if not specified. -/
  origin : Option String := none
  /-- BrowserContext to override permissions. When omitted, default browser
      context is used. -/
  browserContextId : Option Browser.BrowserContextID := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PGrantPermissions where
  toJSON p := Data.Json.object <|
       [("permissions", ToJSON.toJSON p.permissions)]
    ++ (p.origin.map fun v => ("origin", ToJSON.toJSON v)).toList
    ++ (p.browserContextId.map fun v => ("browserContextId", ToJSON.toJSON v)).toList

instance : Command Browser.PGrantPermissions where
  Response := Unit
  commandName _ := "Browser.grantPermissions"
  decodeResponse _ := .ok ()

/-- Parameters of the `Browser.resetPermissions` command: resets all
    permission management for all origins. -/
structure Browser.PResetPermissions where
  /-- BrowserContext to reset permissions. When omitted, default browser
      context is used. -/
  browserContextId : Option Browser.BrowserContextID := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PResetPermissions where
  toJSON p := Data.Json.object <|
    (p.browserContextId.map fun v => ("browserContextId", ToJSON.toJSON v)).toList

instance : Command Browser.PResetPermissions where
  Response := Unit
  commandName _ := "Browser.resetPermissions"
  decodeResponse _ := .ok ()

/-- Which behavior to use for `Browser.setDownloadBehavior`. -/
inductive Browser.SetDownloadBehaviorBehavior where
  | deny | allow | allowAndName | default
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.SetDownloadBehaviorBehavior where
  parseJSON
    | .string "deny" => .ok .deny
    | .string "allow" => .ok .allow
    | .string "allowAndName" => .ok .allowAndName
    | .string "default" => .ok .default
    | v => .error s!"failed to parse Browser.SetDownloadBehaviorBehavior: {repr v}"

instance : ToJSON Browser.SetDownloadBehaviorBehavior where
  toJSON
    | .deny => .string "deny" | .allow => .string "allow"
    | .allowAndName => .string "allowAndName" | .default => .string "default"

/-- Parameters of the `Browser.setDownloadBehavior` command: sets the
    behavior when downloading a file. -/
structure Browser.PSetDownloadBehavior where
  /-- Whether to allow all or deny all download requests, or use default
      Chrome behavior if available (otherwise deny). `allowAndName` allows
      download and names files according to their download guids. -/
  behavior : Browser.SetDownloadBehaviorBehavior
  /-- BrowserContext to set download behavior. When omitted, default browser
      context is used. -/
  browserContextId : Option Browser.BrowserContextID := none
  /-- The default path to save downloaded files to. This is required if
      behavior is set to `allow` or `allowAndName`. -/
  downloadPath : Option String := none
  /-- Whether to emit download events (defaults to `false`). -/
  eventsEnabled : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PSetDownloadBehavior where
  toJSON p := Data.Json.object <|
       [("behavior", ToJSON.toJSON p.behavior)]
    ++ (p.browserContextId.map fun v => ("browserContextId", ToJSON.toJSON v)).toList
    ++ (p.downloadPath.map fun v => ("downloadPath", ToJSON.toJSON v)).toList
    ++ (p.eventsEnabled.map fun v => ("eventsEnabled", ToJSON.toJSON v)).toList

instance : Command Browser.PSetDownloadBehavior where
  Response := Unit
  commandName _ := "Browser.setDownloadBehavior"
  decodeResponse _ := .ok ()

/-- Parameters of the `Browser.cancelDownload` command: cancels a download
    if in progress. -/
structure Browser.PCancelDownload where
  /-- Global unique identifier of the download. -/
  guid : String
  /-- BrowserContext to perform the action in. When omitted, default browser
      context is used. -/
  browserContextId : Option Browser.BrowserContextID := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PCancelDownload where
  toJSON p := Data.Json.object <|
       [("guid", ToJSON.toJSON p.guid)]
    ++ (p.browserContextId.map fun v => ("browserContextId", ToJSON.toJSON v)).toList

instance : Command Browser.PCancelDownload where
  Response := Unit
  commandName _ := "Browser.cancelDownload"
  decodeResponse _ := .ok ()

/-- Parameters of the `Browser.close` command: closes the browser
    gracefully. -/
structure Browser.PClose where
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PClose where toJSON _ := .null

instance : Command Browser.PClose where
  Response := Unit
  commandName _ := "Browser.close"
  decodeResponse _ := .ok ()

/-- Parameters of the `Browser.crash` command: crashes the browser on the
    main thread. -/
structure Browser.PCrash where
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PCrash where toJSON _ := .null

instance : Command Browser.PCrash where
  Response := Unit
  commandName _ := "Browser.crash"
  decodeResponse _ := .ok ()

/-- Parameters of the `Browser.crashGpuProcess` command: crashes the GPU
    process. -/
structure Browser.PCrashGpuProcess where
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PCrashGpuProcess where toJSON _ := .null

instance : Command Browser.PCrashGpuProcess where
  Response := Unit
  commandName _ := "Browser.crashGpuProcess"
  decodeResponse _ := .ok ()

/-- Parameters of the `Browser.getVersion` command: returns version
    information. -/
structure Browser.PGetVersion where
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PGetVersion where toJSON _ := .null

/-- Response of the `Browser.getVersion` command. -/
structure Browser.GetVersion where
  /-- Protocol version. -/
  protocolVersion : String
  /-- Product name. -/
  product : String
  /-- Product revision. -/
  revision : String
  /-- User-Agent. -/
  userAgent : String
  /-- V8 version. -/
  jsVersion : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.GetVersion where
  parseJSON v := do
    .ok
      { protocolVersion := ← Value.getField v "protocolVersion" >>= FromJSON.parseJSON
        product := ← Value.getField v "product" >>= FromJSON.parseJSON
        revision := ← Value.getField v "revision" >>= FromJSON.parseJSON
        userAgent := ← Value.getField v "userAgent" >>= FromJSON.parseJSON
        jsVersion := ← Value.getField v "jsVersion" >>= FromJSON.parseJSON }

instance : Command Browser.PGetVersion where
  Response := Browser.GetVersion
  commandName _ := "Browser.getVersion"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Browser.getBrowserCommandLine` command: returns the
    command line switches for the browser process if, and only if,
    `--enable-automation` is on the command line. -/
structure Browser.PGetBrowserCommandLine where
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PGetBrowserCommandLine where toJSON _ := .null

/-- Response of the `Browser.getBrowserCommandLine` command. -/
structure Browser.GetBrowserCommandLine where
  /-- Commandline parameters. -/
  arguments : List String
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.GetBrowserCommandLine where
  parseJSON v := do .ok { arguments := ← Value.getField v "arguments" >>= FromJSON.parseJSON }

instance : Command Browser.PGetBrowserCommandLine where
  Response := Browser.GetBrowserCommandLine
  commandName _ := "Browser.getBrowserCommandLine"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Browser.getHistograms` command: gets Chrome
    histograms. -/
structure Browser.PGetHistograms where
  /-- Requested substring in name. Only histograms which have query as a
      substring in their name are extracted. An empty or absent query
      returns all histograms. -/
  query : Option String := none
  /-- If `true`, retrieve delta since last call. -/
  delta : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PGetHistograms where
  toJSON p := Data.Json.object <|
       (p.query.map fun v => ("query", ToJSON.toJSON v)).toList
    ++ (p.delta.map fun v => ("delta", ToJSON.toJSON v)).toList

/-- Response of the `Browser.getHistograms` command. -/
structure Browser.GetHistograms where
  /-- Histograms. -/
  histograms : List Browser.Histogram
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.GetHistograms where
  parseJSON v := do .ok { histograms := ← Value.getField v "histograms" >>= FromJSON.parseJSON }

instance : Command Browser.PGetHistograms where
  Response := Browser.GetHistograms
  commandName _ := "Browser.getHistograms"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Browser.getHistogram` command: gets a Chrome
    histogram by name. -/
structure Browser.PGetHistogram where
  /-- Requested histogram name. -/
  name : String
  /-- If `true`, retrieve delta since last call. -/
  delta : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PGetHistogram where
  toJSON p := Data.Json.object <|
       [("name", ToJSON.toJSON p.name)]
    ++ (p.delta.map fun v => ("delta", ToJSON.toJSON v)).toList

/-- Response of the `Browser.getHistogram` command. -/
structure Browser.GetHistogram where
  /-- Histogram. -/
  histogram : Browser.Histogram
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.GetHistogram where
  parseJSON v := do .ok { histogram := ← Value.getField v "histogram" >>= FromJSON.parseJSON }

instance : Command Browser.PGetHistogram where
  Response := Browser.GetHistogram
  commandName _ := "Browser.getHistogram"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Browser.getWindowBounds` command: gets the position
    and size of the browser window. -/
structure Browser.PGetWindowBounds where
  /-- Browser window id. -/
  windowId : Browser.WindowID
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PGetWindowBounds where
  toJSON p := Data.Json.object [("windowId", ToJSON.toJSON p.windowId)]

/-- Response of the `Browser.getWindowBounds` command. -/
structure Browser.GetWindowBounds where
  /-- Bounds information of the window. When window state is `minimized`,
      the restored window position and size are returned. -/
  bounds : Browser.Bounds
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.GetWindowBounds where
  parseJSON v := do .ok { bounds := ← Value.getField v "bounds" >>= FromJSON.parseJSON }

instance : Command Browser.PGetWindowBounds where
  Response := Browser.GetWindowBounds
  commandName _ := "Browser.getWindowBounds"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Browser.getWindowForTarget` command: gets the browser
    window that contains the devtools target. -/
structure Browser.PGetWindowForTarget where
  /-- Devtools agent host id. If called as a part of the session, associated
      `targetId` is used. -/
  targetId : Option Target.TargetID := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PGetWindowForTarget where
  toJSON p := Data.Json.object <|
    (p.targetId.map fun v => ("targetId", ToJSON.toJSON v)).toList

/-- Response of the `Browser.getWindowForTarget` command. -/
structure Browser.GetWindowForTarget where
  /-- Browser window id. -/
  windowId : Browser.WindowID
  /-- Bounds information of the window. When window state is `minimized`,
      the restored window position and size are returned. -/
  bounds : Browser.Bounds
  deriving Repr, BEq, DecidableEq

instance : FromJSON Browser.GetWindowForTarget where
  parseJSON v := do
    .ok
      { windowId := ← Value.getField v "windowId" >>= FromJSON.parseJSON
        bounds := ← Value.getField v "bounds" >>= FromJSON.parseJSON }

instance : Command Browser.PGetWindowForTarget where
  Response := Browser.GetWindowForTarget
  commandName _ := "Browser.getWindowForTarget"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Browser.setWindowBounds` command: sets the position
    and/or size of the browser window. -/
structure Browser.PSetWindowBounds where
  /-- Browser window id. -/
  windowId : Browser.WindowID
  /-- New window bounds. The `minimized`, `maximized` and `fullscreen`
      states cannot be combined with `left`, `top`, `width` or `height`.
      Leaves unspecified fields unchanged. -/
  bounds : Browser.Bounds
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PSetWindowBounds where
  toJSON p := Data.Json.object
    [("windowId", ToJSON.toJSON p.windowId), ("bounds", ToJSON.toJSON p.bounds)]

instance : Command Browser.PSetWindowBounds where
  Response := Unit
  commandName _ := "Browser.setWindowBounds"
  decodeResponse _ := .ok ()

/-- Parameters of the `Browser.setDockTile` command: sets dock tile details,
    platform-specific. -/
structure Browser.PSetDockTile where
  badgeLabel : Option String := none
  /-- Png encoded image. (Encoded as a base64 string when passed over
      JSON.) -/
  image : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PSetDockTile where
  toJSON p := Data.Json.object <|
       (p.badgeLabel.map fun v => ("badgeLabel", ToJSON.toJSON v)).toList
    ++ (p.image.map fun v => ("image", ToJSON.toJSON v)).toList

instance : Command Browser.PSetDockTile where
  Response := Unit
  commandName _ := "Browser.setDockTile"
  decodeResponse _ := .ok ()

/-- Parameters of the `Browser.executeBrowserCommand` command: invokes
    custom browser commands used by telemetry. -/
structure Browser.PExecuteBrowserCommand where
  commandId : Browser.BrowserCommandId
  deriving Repr, BEq, DecidableEq

instance : ToJSON Browser.PExecuteBrowserCommand where
  toJSON p := Data.Json.object [("commandId", ToJSON.toJSON p.commandId)]

instance : Command Browser.PExecuteBrowserCommand where
  Response := Unit
  commandName _ := "Browser.executeBrowserCommand"
  decodeResponse _ := .ok ()

-- ── Target: events ──

/-- The `Target.attachedToTarget` event. -/
structure Target.AttachedToTarget where
  /-- Identifier assigned to the session used to send/receive messages. -/
  sessionId : Target.SessionID
  targetInfo : Target.TargetInfo
  waitingForDebugger : Bool
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.AttachedToTarget where
  parseJSON v := do
    .ok
      { sessionId := ← Value.getField v "sessionId" >>= FromJSON.parseJSON
        targetInfo := ← Value.getField v "targetInfo" >>= FromJSON.parseJSON
        waitingForDebugger := ← Value.getField v "waitingForDebugger" >>= FromJSON.parseJSON }

instance : Event Target.AttachedToTarget where
  eventName := "Target.attachedToTarget"

/-- The `Target.detachedFromTarget` event. -/
structure Target.DetachedFromTarget where
  /-- Detached session identifier. -/
  sessionId : Target.SessionID
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.DetachedFromTarget where
  parseJSON v := do .ok { sessionId := ← Value.getField v "sessionId" >>= FromJSON.parseJSON }

instance : Event Target.DetachedFromTarget where
  eventName := "Target.detachedFromTarget"

/-- The `Target.receivedMessageFromTarget` event. -/
structure Target.ReceivedMessageFromTarget where
  /-- Identifier of a session which sends a message. -/
  sessionId : Target.SessionID
  message : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.ReceivedMessageFromTarget where
  parseJSON v := do
    .ok
      { sessionId := ← Value.getField v "sessionId" >>= FromJSON.parseJSON
        message := ← Value.getField v "message" >>= FromJSON.parseJSON }

instance : Event Target.ReceivedMessageFromTarget where
  eventName := "Target.receivedMessageFromTarget"

/-- The `Target.targetCreated` event. -/
structure Target.TargetCreated where
  targetInfo : Target.TargetInfo
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.TargetCreated where
  parseJSON v := do .ok { targetInfo := ← Value.getField v "targetInfo" >>= FromJSON.parseJSON }

instance : Event Target.TargetCreated where
  eventName := "Target.targetCreated"

/-- The `Target.targetDestroyed` event. -/
structure Target.TargetDestroyed where
  targetId : Target.TargetID
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.TargetDestroyed where
  parseJSON v := do .ok { targetId := ← Value.getField v "targetId" >>= FromJSON.parseJSON }

instance : Event Target.TargetDestroyed where
  eventName := "Target.targetDestroyed"

/-- The `Target.targetCrashed` event. -/
structure Target.TargetCrashed where
  targetId : Target.TargetID
  /-- Termination status type. -/
  status : String
  /-- Termination error code. -/
  errorCode : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.TargetCrashed where
  parseJSON v := do
    .ok
      { targetId := ← Value.getField v "targetId" >>= FromJSON.parseJSON
        status := ← Value.getField v "status" >>= FromJSON.parseJSON
        errorCode := ← Value.getField v "errorCode" >>= FromJSON.parseJSON }

instance : Event Target.TargetCrashed where
  eventName := "Target.targetCrashed"

/-- The `Target.targetInfoChanged` event. -/
structure Target.TargetInfoChanged where
  targetInfo : Target.TargetInfo
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.TargetInfoChanged where
  parseJSON v := do .ok { targetInfo := ← Value.getField v "targetInfo" >>= FromJSON.parseJSON }

instance : Event Target.TargetInfoChanged where
  eventName := "Target.targetInfoChanged"

-- ── Target: commands ──

/-- Parameters of the `Target.activateTarget` command: activates (focuses)
    the target. -/
structure Target.PActivateTarget where
  targetId : Target.TargetID
  deriving Repr, BEq, DecidableEq

instance : ToJSON Target.PActivateTarget where
  toJSON p := Data.Json.object [("targetId", ToJSON.toJSON p.targetId)]

instance : Command Target.PActivateTarget where
  Response := Unit
  commandName _ := "Target.activateTarget"
  decodeResponse _ := .ok ()

/-- Parameters of the `Target.attachToTarget` command: attaches to the
    target with given id. -/
structure Target.PAttachToTarget where
  targetId : Target.TargetID
  /-- Enables "flat" access to the session via specifying `sessionId`
      attribute in the commands. We plan to make this the default,
      deprecate non-flattened mode, and eventually retire it. See
      crbug.com/991325. -/
  flatten : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Target.PAttachToTarget where
  toJSON p := Data.Json.object <|
       [("targetId", ToJSON.toJSON p.targetId)]
    ++ (p.flatten.map fun v => ("flatten", ToJSON.toJSON v)).toList

/-- Response of the `Target.attachToTarget` command. -/
structure Target.AttachToTarget where
  /-- Id assigned to the session. -/
  sessionId : Target.SessionID
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.AttachToTarget where
  parseJSON v := do .ok { sessionId := ← Value.getField v "sessionId" >>= FromJSON.parseJSON }

instance : Command Target.PAttachToTarget where
  Response := Target.AttachToTarget
  commandName _ := "Target.attachToTarget"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Target.attachToBrowserTarget` command: attaches to
    the browser target, only uses flat `sessionId` mode. -/
structure Target.PAttachToBrowserTarget where
  deriving Repr, BEq, DecidableEq

instance : ToJSON Target.PAttachToBrowserTarget where toJSON _ := .null

/-- Response of the `Target.attachToBrowserTarget` command. -/
structure Target.AttachToBrowserTarget where
  /-- Id assigned to the session. -/
  sessionId : Target.SessionID
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.AttachToBrowserTarget where
  parseJSON v := do .ok { sessionId := ← Value.getField v "sessionId" >>= FromJSON.parseJSON }

instance : Command Target.PAttachToBrowserTarget where
  Response := Target.AttachToBrowserTarget
  commandName _ := "Target.attachToBrowserTarget"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Target.closeTarget` command: closes the target. If
    the target is a page that gets closed too. -/
structure Target.PCloseTarget where
  targetId : Target.TargetID
  deriving Repr, BEq, DecidableEq

instance : ToJSON Target.PCloseTarget where
  toJSON p := Data.Json.object [("targetId", ToJSON.toJSON p.targetId)]

instance : Command Target.PCloseTarget where
  Response := Unit
  commandName _ := "Target.closeTarget"
  decodeResponse _ := .ok ()

/-- Parameters of the `Target.exposeDevToolsProtocol` command: injects an
    object into the target's main frame that provides a communication
    channel with the browser target.

    The injected object will be available as `window[bindingName]`.

    The object has the following API:
    - `binding.send(json)` — a method to send messages over the remote
      debugging protocol
    - `binding.onmessage = json => handleMessage(json)` — a callback that
      will be called for the protocol notifications and command
      responses. -/
structure Target.PExposeDevToolsProtocol where
  targetId : Target.TargetID
  /-- Binding name, `cdp` if not specified. -/
  bindingName : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Target.PExposeDevToolsProtocol where
  toJSON p := Data.Json.object <|
       [("targetId", ToJSON.toJSON p.targetId)]
    ++ (p.bindingName.map fun v => ("bindingName", ToJSON.toJSON v)).toList

instance : Command Target.PExposeDevToolsProtocol where
  Response := Unit
  commandName _ := "Target.exposeDevToolsProtocol"
  decodeResponse _ := .ok ()

/-- Parameters of the `Target.createBrowserContext` command: creates a new
    empty `BrowserContext`. Similar to an incognito profile but you can have
    more than one. -/
structure Target.PCreateBrowserContext where
  /-- If specified, disposes this context when debugging session
      disconnects. -/
  disposeOnDetach : Option Bool := none
  /-- Proxy server, similar to the one passed to `--proxy-server`. -/
  proxyServer : Option String := none
  /-- Proxy bypass list, similar to the one passed to
      `--proxy-bypass-list`. -/
  proxyBypassList : Option String := none
  /-- An optional list of origins to grant unlimited cross-origin access to.
      Parts of the URL other than those constituting origin are
      ignored. -/
  originsWithUniversalNetworkAccess : Option (List String) := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Target.PCreateBrowserContext where
  toJSON p := Data.Json.object <|
       (p.disposeOnDetach.map fun v => ("disposeOnDetach", ToJSON.toJSON v)).toList
    ++ (p.proxyServer.map fun v => ("proxyServer", ToJSON.toJSON v)).toList
    ++ (p.proxyBypassList.map fun v => ("proxyBypassList", ToJSON.toJSON v)).toList
    ++ (p.originsWithUniversalNetworkAccess.map fun v =>
          ("originsWithUniversalNetworkAccess", ToJSON.toJSON v)).toList

/-- Response of the `Target.createBrowserContext` command. -/
structure Target.CreateBrowserContext where
  /-- The id of the context created. -/
  browserContextId : Browser.BrowserContextID
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.CreateBrowserContext where
  parseJSON v := do
    .ok { browserContextId := ← Value.getField v "browserContextId" >>= FromJSON.parseJSON }

instance : Command Target.PCreateBrowserContext where
  Response := Target.CreateBrowserContext
  commandName _ := "Target.createBrowserContext"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Target.getBrowserContexts` command: returns all
    browser contexts created with `Target.createBrowserContext`. -/
structure Target.PGetBrowserContexts where
  deriving Repr, BEq, DecidableEq

instance : ToJSON Target.PGetBrowserContexts where toJSON _ := .null

/-- Response of the `Target.getBrowserContexts` command. -/
structure Target.GetBrowserContexts where
  /-- An array of browser context ids. -/
  browserContextIds : List Browser.BrowserContextID
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.GetBrowserContexts where
  parseJSON v := do
    .ok { browserContextIds := ← Value.getField v "browserContextIds" >>= FromJSON.parseJSON }

instance : Command Target.PGetBrowserContexts where
  Response := Target.GetBrowserContexts
  commandName _ := "Target.getBrowserContexts"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Target.createTarget` command: creates a new page. -/
structure Target.PCreateTarget where
  /-- The initial URL the page will be navigated to. An empty string
      indicates `about:blank`. -/
  url : String
  /-- Frame width in DIP (headless chrome only). -/
  width : Option Int := none
  /-- Frame height in DIP (headless chrome only). -/
  height : Option Int := none
  /-- The browser context to create the page in. -/
  browserContextId : Option Browser.BrowserContextID := none
  /-- Whether `BeginFrame`s for this target will be controlled via DevTools
      (headless chrome only, not supported on MacOS yet, `false` by
      default). -/
  enableBeginFrameControl : Option Bool := none
  /-- Whether to create a new Window or Tab (chrome-only, `false` by
      default). -/
  newWindow : Option Bool := none
  /-- Whether to create the target in background or foreground (chrome-only,
      `false` by default). -/
  background : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Target.PCreateTarget where
  toJSON p := Data.Json.object <|
       [("url", ToJSON.toJSON p.url)]
    ++ (p.width.map fun v => ("width", ToJSON.toJSON v)).toList
    ++ (p.height.map fun v => ("height", ToJSON.toJSON v)).toList
    ++ (p.browserContextId.map fun v => ("browserContextId", ToJSON.toJSON v)).toList
    ++ (p.enableBeginFrameControl.map fun v => ("enableBeginFrameControl", ToJSON.toJSON v)).toList
    ++ (p.newWindow.map fun v => ("newWindow", ToJSON.toJSON v)).toList
    ++ (p.background.map fun v => ("background", ToJSON.toJSON v)).toList

/-- Response of the `Target.createTarget` command. -/
structure Target.CreateTarget where
  /-- The id of the page opened. -/
  targetId : Target.TargetID
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.CreateTarget where
  parseJSON v := do .ok { targetId := ← Value.getField v "targetId" >>= FromJSON.parseJSON }

instance : Command Target.PCreateTarget where
  Response := Target.CreateTarget
  commandName _ := "Target.createTarget"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Target.detachFromTarget` command: detaches the
    session with given id. -/
structure Target.PDetachFromTarget where
  /-- Session to detach. -/
  sessionId : Option Target.SessionID := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Target.PDetachFromTarget where
  toJSON p := Data.Json.object <|
    (p.sessionId.map fun v => ("sessionId", ToJSON.toJSON v)).toList

instance : Command Target.PDetachFromTarget where
  Response := Unit
  commandName _ := "Target.detachFromTarget"
  decodeResponse _ := .ok ()

/-- Parameters of the `Target.disposeBrowserContext` command: deletes a
    `BrowserContext`. All the belonging pages will be closed without
    calling their beforeunload hooks. -/
structure Target.PDisposeBrowserContext where
  browserContextId : Browser.BrowserContextID
  deriving Repr, BEq, DecidableEq

instance : ToJSON Target.PDisposeBrowserContext where
  toJSON p := Data.Json.object [("browserContextId", ToJSON.toJSON p.browserContextId)]

instance : Command Target.PDisposeBrowserContext where
  Response := Unit
  commandName _ := "Target.disposeBrowserContext"
  decodeResponse _ := .ok ()

/-- Parameters of the `Target.getTargetInfo` command: returns information
    about a target. -/
structure Target.PGetTargetInfo where
  targetId : Option Target.TargetID := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Target.PGetTargetInfo where
  toJSON p := Data.Json.object <|
    (p.targetId.map fun v => ("targetId", ToJSON.toJSON v)).toList

/-- Response of the `Target.getTargetInfo` command. -/
structure Target.GetTargetInfo where
  targetInfo : Target.TargetInfo
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.GetTargetInfo where
  parseJSON v := do .ok { targetInfo := ← Value.getField v "targetInfo" >>= FromJSON.parseJSON }

instance : Command Target.PGetTargetInfo where
  Response := Target.GetTargetInfo
  commandName _ := "Target.getTargetInfo"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Target.getTargets` command: retrieves a list of
    available targets. -/
structure Target.PGetTargets where
  /-- Only targets matching filter will be reported. If filter is not
      specified and target discovery is currently enabled, a filter used for
      target discovery is used for consistency. -/
  filter : Option Target.TargetFilter := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Target.PGetTargets where
  toJSON p := Data.Json.object <|
    (p.filter.map fun v => ("filter", ToJSON.toJSON v)).toList

/-- Response of the `Target.getTargets` command. -/
structure Target.GetTargets where
  /-- The list of targets. -/
  targetInfos : List Target.TargetInfo
  deriving Repr, BEq, DecidableEq

instance : FromJSON Target.GetTargets where
  parseJSON v := do .ok { targetInfos := ← Value.getField v "targetInfos" >>= FromJSON.parseJSON }

instance : Command Target.PGetTargets where
  Response := Target.GetTargets
  commandName _ := "Target.getTargets"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Target.setAutoAttach` command: controls whether to
    automatically attach to new targets which are considered to be related
    to this one. When turned on, attaches to all existing related targets
    as well. When turned off, automatically detaches from all currently
    attached targets. This also clears all targets added by
    `autoAttachRelated` from the list of targets to watch for creation of
    related targets. -/
structure Target.PSetAutoAttach where
  /-- Whether to auto-attach to related targets. -/
  autoAttach : Bool
  /-- Whether to pause new targets when attaching to them. Use
      `Runtime.runIfWaitingForDebugger` to run paused targets. -/
  waitForDebuggerOnStart : Bool
  /-- Enables "flat" access to the session via specifying `sessionId`
      attribute in the commands. We plan to make this the default,
      deprecate non-flattened mode, and eventually retire it. See
      crbug.com/991325. -/
  flatten : Option Bool := none
  /-- Only targets matching filter will be attached. -/
  filter : Option Target.TargetFilter := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Target.PSetAutoAttach where
  toJSON p := Data.Json.object <|
       [ ("autoAttach", ToJSON.toJSON p.autoAttach)
       , ("waitForDebuggerOnStart", ToJSON.toJSON p.waitForDebuggerOnStart) ]
    ++ (p.flatten.map fun v => ("flatten", ToJSON.toJSON v)).toList
    ++ (p.filter.map fun v => ("filter", ToJSON.toJSON v)).toList

instance : Command Target.PSetAutoAttach where
  Response := Unit
  commandName _ := "Target.setAutoAttach"
  decodeResponse _ := .ok ()

/-- Parameters of the `Target.autoAttachRelated` command: adds the specified
    target to the list of targets that will be monitored for any related
    target creation (such as child frames, child workers and new versions
    of service worker) and reported through `attachedToTarget`. The
    specified target is also auto-attached. This cancels the effect of any
    previous `setAutoAttach` and is also cancelled by subsequent
    `setAutoAttach`. Only available at the Browser target. -/
structure Target.PAutoAttachRelated where
  targetId : Target.TargetID
  /-- Whether to pause new targets when attaching to them. Use
      `Runtime.runIfWaitingForDebugger` to run paused targets. -/
  waitForDebuggerOnStart : Bool
  /-- Only targets matching filter will be attached. -/
  filter : Option Target.TargetFilter := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Target.PAutoAttachRelated where
  toJSON p := Data.Json.object <|
       [ ("targetId", ToJSON.toJSON p.targetId)
       , ("waitForDebuggerOnStart", ToJSON.toJSON p.waitForDebuggerOnStart) ]
    ++ (p.filter.map fun v => ("filter", ToJSON.toJSON v)).toList

instance : Command Target.PAutoAttachRelated where
  Response := Unit
  commandName _ := "Target.autoAttachRelated"
  decodeResponse _ := .ok ()

/-- Parameters of the `Target.setDiscoverTargets` command: controls whether
    to discover available targets and notify via
    `targetCreated`/`targetInfoChanged`/`targetDestroyed` events. -/
structure Target.PSetDiscoverTargets where
  /-- Whether to discover available targets. -/
  discover : Bool
  /-- Only targets matching filter will be attached. If `discover` is
      `false`, `filter` must be omitted or empty. -/
  filter : Option Target.TargetFilter := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON Target.PSetDiscoverTargets where
  toJSON p := Data.Json.object <|
       [("discover", ToJSON.toJSON p.discover)]
    ++ (p.filter.map fun v => ("filter", ToJSON.toJSON v)).toList

instance : Command Target.PSetDiscoverTargets where
  Response := Unit
  commandName _ := "Target.setDiscoverTargets"
  decodeResponse _ := .ok ()

/-- Parameters of the `Target.setRemoteLocations` command: enables target
    discovery for the specified locations, when `setDiscoverTargets` was set
    to `true`. -/
structure Target.PSetRemoteLocations where
  /-- List of remote locations. -/
  locations : List Target.RemoteLocation
  deriving Repr, BEq, DecidableEq

instance : ToJSON Target.PSetRemoteLocations where
  toJSON p := Data.Json.object [("locations", ToJSON.toJSON p.locations)]

instance : Command Target.PSetRemoteLocations where
  Response := Unit
  commandName _ := "Target.setRemoteLocations"
  decodeResponse _ := .ok ()

end CDP.Domains.BrowserTarget
