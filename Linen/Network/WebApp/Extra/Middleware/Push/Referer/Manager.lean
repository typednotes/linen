/-
  Linen.Network.WebApp.Extra.Middleware.Push.Referer.Manager — Push prediction
  manager

  Ports `Network.Wai.Middleware.Push.Referer.Manager`. Manages the
  referer-based push prediction table. Tracks which resources are commonly
  requested after a page load, and suggests them for push.
-/
import Linen.Network.WebApp.Extra.Middleware.Push.Referer.Types
import Linen.Network.WebApp.Extra.Middleware.Push.Referer.LRU

namespace Network.WebApp.Extra.Middleware.Push.Referer

/-- The push prediction manager. Thread-safe via `IO.Ref`. -/
structure PushManager where
  /-- Settings. -/
  settings : PushSettings
  /-- The push table: maps page paths to push resources. -/
  table : IO.Ref (LRU String (List PushPath))

/-- Create a new push manager. -/
def PushManager.new (settings : PushSettings := {}) : IO PushManager := do
  let ref ← IO.mkRef (LRU.empty settings.maxEntries)
  return ⟨settings, ref⟩

/-- Record a resource request with its referer, updating the push table.
    When a resource is requested with a Referer header pointing to a page,
    we learn that the page needs that resource. -/
def PushManager.record (mgr : PushManager) (referer resource : String) : IO Unit := do
  mgr.table.modify fun cache =>
    let (existing, cache') := cache.lookup referer
    let resources := match existing with
      | some rs =>
        if rs.contains resource then rs
        else if rs.length >= mgr.settings.maxPushesPerPage then rs
        else rs ++ [resource]
      | none => [resource]
    cache'.insert referer resources

/-- Get the list of resources to push for a given page path. -/
def PushManager.getPushes (mgr : PushManager) (pagePath : String) : IO (List PushPath) := do
  let cache ← mgr.table.get
  let (result, _) := cache.lookup pagePath
  return result.getD []

end Network.WebApp.Extra.Middleware.Push.Referer
