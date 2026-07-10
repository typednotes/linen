/-
  Linen.CDP.Domains — aggregator re-exporting every `CDP.Domains.*` module

  Ports `CDP.Domains` (see `docs/imports/cdp/dependencies.md`): upstream's
  package root simply re-exports each per-domain module so a client only
  needs one import to reach every command/event/type across the protocol.
-/
import Linen.CDP.Domains.CacheStorage
import Linen.CDP.Domains.Cast
import Linen.CDP.Domains.DOMStorage
import Linen.CDP.Domains.Database
import Linen.CDP.Domains.DeviceOrientation
import Linen.CDP.Domains.EventBreakpoints
import Linen.CDP.Domains.HeadlessExperimental
import Linen.CDP.Domains.Input
import Linen.CDP.Domains.Inspector
import Linen.CDP.Domains.Media
import Linen.CDP.Domains.Memory
import Linen.CDP.Domains.Performance
import Linen.CDP.Domains.Runtime
import Linen.CDP.Domains.Debugger
import Linen.CDP.Domains.HeapProfiler
import Linen.CDP.Domains.IO
import Linen.CDP.Domains.IndexedDB
import Linen.CDP.Domains.SystemInfo
import Linen.CDP.Domains.Tethering
import Linen.CDP.Domains.WebAudio
import Linen.CDP.Domains.WebAuthn
import Linen.CDP.Domains.Tracing
import Linen.CDP.Domains.Profiler
import Linen.CDP.Domains.DOMPageNetworkEmulationSecurity
import Linen.CDP.Domains.Log
import Linen.CDP.Domains.DOMDebugger
import Linen.CDP.Domains.PerformanceTimeline
import Linen.CDP.Domains.Animation
import Linen.CDP.Domains.LayerTree
import Linen.CDP.Domains.Audits
import Linen.CDP.Domains.CSS
import Linen.CDP.Domains.BrowserTarget
import Linen.CDP.Domains.Overlay
import Linen.CDP.Domains.Accessibility
import Linen.CDP.Domains.DOMSnapshot
import Linen.CDP.Domains.Fetch
import Linen.CDP.Domains.ServiceWorker
import Linen.CDP.Domains.Storage
import Linen.CDP.Domains.BackgroundService
