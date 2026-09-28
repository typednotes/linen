/-
  Tests for `Linen.CDP.Domains.BrowserTarget`.
-/
import Linen.CDP.Domains.BrowserTarget

open CDP.Domains.BrowserTarget
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.BrowserTarget

-- ── Browser leaf types ──

#guard decodeAs "\"maximized\"" (α := Browser.WindowState) = .ok .maximized
#guard encode (ToJSON.toJSON Browser.WindowState.fullscreen) = "\"fullscreen\""

#guard decodeAs "{\"left\": 0, \"top\": 1, \"width\": 800, \"height\": 600}" (α := Browser.Bounds)
  = .ok { left := some 0, top := some 1, width := some 800, height := some 600, windowState := none }
#guard encode (ToJSON.toJSON ({ windowState := some .minimized } : Browser.Bounds))
  = "{\"windowState\":\"minimized\"}"

#guard decodeAs "\"geolocation\"" (α := Browser.PermissionType) = .ok .geolocation
#guard encode (ToJSON.toJSON Browser.PermissionType.midiSysex) = "\"midiSysex\""

#guard decodeAs "\"granted\"" (α := Browser.PermissionSetting) = .ok .granted
#guard encode (ToJSON.toJSON Browser.PermissionSetting.denied) = "\"denied\""

#guard decodeAs "{\"name\": \"geolocation\"}" (α := Browser.PermissionDescriptor)
  = .ok { name := "geolocation" }
#guard encode (ToJSON.toJSON ({ name := "midi", sysex := some true } : Browser.PermissionDescriptor))
  = "{\"name\":\"midi\",\"sysex\":true}"

#guard decodeAs "\"openTabSearch\"" (α := Browser.BrowserCommandId) = .ok .openTabSearch
#guard encode (ToJSON.toJSON Browser.BrowserCommandId.closeTabSearch) = "\"closeTabSearch\""

#guard decodeAs "{\"low\": 0, \"high\": 10, \"count\": 3}" (α := Browser.Bucket)
  = .ok { low := 0, high := 10, count := 3 }
#guard encode (ToJSON.toJSON ({ low := 0, high := 10, count := 3 } : Browser.Bucket))
  = "{\"low\":0,\"high\":10,\"count\":3}"

#guard decodeAs "{\"name\": \"h\", \"sum\": 5, \"count\": 1, \"buckets\": []}" (α := Browser.Histogram)
  = .ok { name := "h", sum := 5, count := 1, buckets := [] }

-- ── Target leaf types ──

#guard decodeAs "{\"exclude\": true}" (α := Target.FilterEntry) = .ok { exclude := some true }
#guard encode (ToJSON.toJSON ({ type := some "page" } : Target.FilterEntry)) = "{\"type\":\"page\"}"

#guard decodeAs "{\"host\": \"localhost\", \"port\": 9222}" (α := Target.RemoteLocation)
  = .ok { host := "localhost", port := 9222 }
#guard encode (ToJSON.toJSON ({ host := "localhost", port := 9222 } : Target.RemoteLocation))
  = "{\"host\":\"localhost\",\"port\":9222}"

#guard decodeAs
    "{\"targetId\": \"t1\", \"type\": \"page\", \"title\": \"T\", \"url\": \"u\", \"attached\": true, \"canAccessOpener\": false}"
    (α := Target.TargetInfo)
  = .ok
      { targetId := "t1", type := "page", title := "T", url := "u", attached := true
        canAccessOpener := false }

-- ── Browser events ──

#guard decodeAs
    "{\"frameId\": \"f1\", \"guid\": \"g1\", \"url\": \"u\", \"suggestedFilename\": \"file\"}"
    (α := Browser.DownloadWillBegin)
  = .ok { frameId := "f1", guid := "g1", url := "u", suggestedFilename := "file" }
#guard Event.eventName (α := Browser.DownloadWillBegin) = "Browser.downloadWillBegin"

#guard decodeAs "\"completed\"" (α := Browser.DownloadProgressState) = .ok .completed
#guard decodeAs
    "{\"guid\": \"g1\", \"totalBytes\": 100.0, \"receivedBytes\": 50.0, \"state\": \"inProgress\"}"
    (α := Browser.DownloadProgress)
  = .ok { guid := "g1", totalBytes := 100.0, receivedBytes := 50.0, state := .inProgress }
#guard Event.eventName (α := Browser.DownloadProgress) = "Browser.downloadProgress"

-- ── Browser commands ──

#guard Command.commandName
    ({ permission := { name := "geolocation" }, setting := .granted } : Browser.PSetPermission)
  = "Browser.setPermission"
#guard encode (ToJSON.toJSON
    ({ permission := { name := "geolocation" }, setting := .granted } : Browser.PSetPermission))
  = "{\"permission\":{\"name\":\"geolocation\"},\"setting\":\"granted\"}"

#guard Command.commandName
    ({ permissions := [.geolocation] } : Browser.PGrantPermissions) = "Browser.grantPermissions"
#guard Command.commandName ({} : Browser.PResetPermissions) = "Browser.resetPermissions"

#guard Command.commandName ({ behavior := .deny } : Browser.PSetDownloadBehavior)
  = "Browser.setDownloadBehavior"
#guard decodeAs "\"allowAndName\"" (α := Browser.SetDownloadBehaviorBehavior) = .ok .allowAndName

#guard Command.commandName ({ guid := "g1" } : Browser.PCancelDownload) = "Browser.cancelDownload"
#guard encode (ToJSON.toJSON ({} : Browser.PClose)) = "null"
#guard Command.commandName ({} : Browser.PClose) = "Browser.close"
#guard Command.commandName ({} : Browser.PCrash) = "Browser.crash"
#guard Command.commandName ({} : Browser.PCrashGpuProcess) = "Browser.crashGpuProcess"

#guard Command.commandName ({} : Browser.PGetVersion) = "Browser.getVersion"
#guard decodeAs
    "{\"protocolVersion\": \"1.3\", \"product\": \"Chrome\", \"revision\": \"r\", \"userAgent\": \"ua\", \"jsVersion\": \"v8\"}"
    (α := Browser.GetVersion)
  = .ok { protocolVersion := "1.3", product := "Chrome", revision := "r", userAgent := "ua", jsVersion := "v8" }

#guard Command.commandName ({} : Browser.PGetBrowserCommandLine) = "Browser.getBrowserCommandLine"
#guard decodeAs "{\"arguments\": [\"--headless\"]}" (α := Browser.GetBrowserCommandLine)
  = .ok { arguments := ["--headless"] }

#guard Command.commandName ({} : Browser.PGetHistograms) = "Browser.getHistograms"
#guard decodeAs "{\"histograms\": []}" (α := Browser.GetHistograms) = .ok { histograms := [] }

#guard Command.commandName ({ name := "h" } : Browser.PGetHistogram) = "Browser.getHistogram"
#guard decodeAs "{\"histogram\": {\"name\": \"h\", \"sum\": 1, \"count\": 1, \"buckets\": []}}"
    (α := Browser.GetHistogram)
  = .ok { histogram := { name := "h", sum := 1, count := 1, buckets := [] } }

#guard Command.commandName ({ windowId := 1 } : Browser.PGetWindowBounds) = "Browser.getWindowBounds"
#guard decodeAs "{\"bounds\": {}}" (α := Browser.GetWindowBounds)
  = .ok { bounds := { left := none, top := none, width := none, height := none, windowState := none } }

#guard Command.commandName ({} : Browser.PGetWindowForTarget) = "Browser.getWindowForTarget"
#guard decodeAs "{\"windowId\": 1, \"bounds\": {}}" (α := Browser.GetWindowForTarget)
  = .ok
      { windowId := 1
        bounds := { left := none, top := none, width := none, height := none, windowState := none } }

#guard Command.commandName
    ({ windowId := 1, bounds := {} } : Browser.PSetWindowBounds) = "Browser.setWindowBounds"
#guard Command.commandName ({} : Browser.PSetDockTile) = "Browser.setDockTile"
#guard Command.commandName
    ({ commandId := .openTabSearch } : Browser.PExecuteBrowserCommand) = "Browser.executeBrowserCommand"

-- ── Target events ──

#guard decodeAs
    "{\"sessionId\": \"s1\", \"targetInfo\": {\"targetId\": \"t1\", \"type\": \"page\", \"title\": \"T\", \"url\": \"u\", \"attached\": true, \"canAccessOpener\": false}, \"waitingForDebugger\": false}"
    (α := Target.AttachedToTarget)
  = .ok
      { sessionId := "s1"
        targetInfo :=
          { targetId := "t1", type := "page", title := "T", url := "u", attached := true
            canAccessOpener := false }
        waitingForDebugger := false }
#guard Event.eventName (α := Target.AttachedToTarget) = "Target.attachedToTarget"

#guard decodeAs "{\"sessionId\": \"s1\"}" (α := Target.DetachedFromTarget) = .ok { sessionId := "s1" }
#guard Event.eventName (α := Target.DetachedFromTarget) = "Target.detachedFromTarget"

#guard decodeAs "{\"sessionId\": \"s1\", \"message\": \"m\"}" (α := Target.ReceivedMessageFromTarget)
  = .ok { sessionId := "s1", message := "m" }
#guard Event.eventName (α := Target.ReceivedMessageFromTarget) = "Target.receivedMessageFromTarget"

#guard Event.eventName (α := Target.TargetCreated) = "Target.targetCreated"
#guard decodeAs "{\"targetId\": \"t1\"}" (α := Target.TargetDestroyed) = .ok { targetId := "t1" }
#guard Event.eventName (α := Target.TargetDestroyed) = "Target.targetDestroyed"

#guard decodeAs "{\"targetId\": \"t1\", \"status\": \"crashed\", \"errorCode\": 1}" (α := Target.TargetCrashed)
  = .ok { targetId := "t1", status := "crashed", errorCode := 1 }
#guard Event.eventName (α := Target.TargetCrashed) = "Target.targetCrashed"
#guard Event.eventName (α := Target.TargetInfoChanged) = "Target.targetInfoChanged"

-- ── Target commands ──

#guard Command.commandName ({ targetId := "t1" } : Target.PActivateTarget) = "Target.activateTarget"

#guard encode (ToJSON.toJSON ({ targetId := "t1" } : Target.PAttachToTarget)) = "{\"targetId\":\"t1\"}"
#guard Command.commandName ({ targetId := "t1" } : Target.PAttachToTarget) = "Target.attachToTarget"
#guard decodeAs "{\"sessionId\": \"s1\"}" (α := Target.AttachToTarget) = .ok { sessionId := "s1" }

#guard encode (ToJSON.toJSON ({} : Target.PAttachToBrowserTarget)) = "null"
#guard Command.commandName ({} : Target.PAttachToBrowserTarget) = "Target.attachToBrowserTarget"
#guard decodeAs "{\"sessionId\": \"s1\"}" (α := Target.AttachToBrowserTarget) = .ok { sessionId := "s1" }

#guard Command.commandName ({ targetId := "t1" } : Target.PCloseTarget) = "Target.closeTarget"
#guard Command.commandName ({ targetId := "t1" } : Target.PExposeDevToolsProtocol)
  = "Target.exposeDevToolsProtocol"

#guard Command.commandName ({} : Target.PCreateBrowserContext) = "Target.createBrowserContext"
#guard decodeAs "{\"browserContextId\": \"b1\"}" (α := Target.CreateBrowserContext)
  = .ok { browserContextId := "b1" }

#guard Command.commandName ({} : Target.PGetBrowserContexts) = "Target.getBrowserContexts"
#guard decodeAs "{\"browserContextIds\": [\"b1\"]}" (α := Target.GetBrowserContexts)
  = .ok { browserContextIds := ["b1"] }

#guard encode (ToJSON.toJSON ({ url := "about:blank" } : Target.PCreateTarget))
  = "{\"url\":\"about:blank\"}"
#guard Command.commandName ({ url := "about:blank" } : Target.PCreateTarget) = "Target.createTarget"
#guard decodeAs "{\"targetId\": \"t1\"}" (α := Target.CreateTarget) = .ok { targetId := "t1" }

#guard Command.commandName ({} : Target.PDetachFromTarget) = "Target.detachFromTarget"
#guard Command.commandName
    ({ browserContextId := "b1" } : Target.PDisposeBrowserContext) = "Target.disposeBrowserContext"

#guard Command.commandName ({} : Target.PGetTargetInfo) = "Target.getTargetInfo"
#guard decodeAs
    "{\"targetInfo\": {\"targetId\": \"t1\", \"type\": \"page\", \"title\": \"T\", \"url\": \"u\", \"attached\": true, \"canAccessOpener\": false}}"
    (α := Target.GetTargetInfo)
  = .ok
      { targetInfo :=
          { targetId := "t1", type := "page", title := "T", url := "u", attached := true
            canAccessOpener := false } }

#guard Command.commandName ({} : Target.PGetTargets) = "Target.getTargets"
#guard decodeAs "{\"targetInfos\": []}" (α := Target.GetTargets) = .ok { targetInfos := [] }

#guard Command.commandName
    ({ autoAttach := true, waitForDebuggerOnStart := false } : Target.PSetAutoAttach)
  = "Target.setAutoAttach"
#guard Command.commandName
    ({ targetId := "t1", waitForDebuggerOnStart := false } : Target.PAutoAttachRelated)
  = "Target.autoAttachRelated"
#guard Command.commandName
    ({ discover := true } : Target.PSetDiscoverTargets) = "Target.setDiscoverTargets"
#guard Command.commandName
    ({ locations := [{ host := "h", port := 1 }] } : Target.PSetRemoteLocations)
  = "Target.setRemoteLocations"

end Tests.CDP.Domains.BrowserTarget
