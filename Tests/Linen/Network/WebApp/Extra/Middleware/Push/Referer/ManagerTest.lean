import Linen.Network.WebApp.Extra.Middleware.Push.Referer.Manager

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.Push.Referer.Manager`

    Coverage: an unseen page has no pushes; `record` learns associations;
    repeated resources are deduped; `maxPushesPerPage` caps the learned list. -/

open Network.WebApp.Extra.Middleware.Push.Referer

namespace Tests.Network.WebApp.Extra.Middleware.Push.Referer.Manager

#eval show IO Unit from do
  let mgr ← PushManager.new
  let pushes ← mgr.getPushes "/unseen"
  unless pushes.isEmpty do throw (IO.userError "expected no pushes for an unseen page")

#eval show IO Unit from do
  let mgr ← PushManager.new
  mgr.record "/" "/app.css"
  mgr.record "/" "/app.js"
  let pushes ← mgr.getPushes "/"
  unless pushes == ["/app.css", "/app.js"] do
    throw (IO.userError s!"expected learned resources in insertion order, got {pushes}")

#eval show IO Unit from do
  let mgr ← PushManager.new
  mgr.record "/" "/app.css"
  mgr.record "/" "/app.css"
  let pushes ← mgr.getPushes "/"
  unless pushes == ["/app.css"] do
    throw (IO.userError s!"expected duplicate resource to be deduped, got {pushes}")

#eval show IO Unit from do
  let mgr ← PushManager.new { maxPushesPerPage := 2 }
  mgr.record "/" "/a.css"
  mgr.record "/" "/b.css"
  mgr.record "/" "/c.css"
  let pushes ← mgr.getPushes "/"
  unless pushes == ["/a.css", "/b.css"] do
    throw (IO.userError s!"expected list capped at maxPushesPerPage, got {pushes}")

end Tests.Network.WebApp.Extra.Middleware.Push.Referer.Manager
