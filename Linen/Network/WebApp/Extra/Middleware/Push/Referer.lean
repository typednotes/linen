/-
  Linen.Network.WebApp.Extra.Middleware.Push.Referer — HTTP/2 server push via
  referer prediction

  Ports `Network.Wai.Middleware.Push.Referer`. Watches Referer headers
  to learn which resources are commonly requested together with a page, then
  suggests them for HTTP/2 server push.

  ## Design

  When a request arrives with a Referer header pointing to a page on the
  same origin, the middleware records the association between the page and
  the requested resource. On subsequent page requests, the learned resources
  are pushed proactively.

  Note: HTTP/2 server push is being deprecated in browsers, but this
  middleware is still useful for preload hints and as a reference
  implementation.

  ## Guarantees
  - Thread-safe push table via `IO.Ref`
  - LRU eviction prevents unbounded memory growth
  - Only same-origin referers are tracked
-/
import Linen.Network.WebApp
import Linen.Network.HTTP.Types.Header
import Linen.Network.WebApp.Extra.Middleware.Push.Referer.Types
import Linen.Network.WebApp.Extra.Middleware.Push.Referer.LRU
import Linen.Network.WebApp.Extra.Middleware.Push.Referer.Manager
import Linen.Network.WebApp.Extra.Middleware.Push.Referer.ParseURL

namespace Network.WebApp.Extra.Middleware.Push

open Network.WebApp
open Network.HTTP.Types
open Referer

/-- Create the push-by-referer middleware.
    The middleware learns from Referer headers and adds Link preload headers
    for predicted resources.
    $$\text{pushOnReferer} : \text{PushSettings} \to \text{IO Middleware}$$ -/
def pushOnReferer (settings : PushSettings := {}) : IO Middleware := do
  let mgr ← PushManager.new settings
  return fun app req respond =>
    AppM.ioThen (do
      let path := req.rawPathInfo
      -- Record this resource if it has a Referer from the same host
      if let some referer := req.requestHeaderReferer then
        let refPath := extractPath referer
        if isStaticResource path && refPath != path then
          mgr.record refPath path
      -- Look up pushable resources for this page
      mgr.getPushes path) fun pushes =>
    if pushes.isEmpty then
      app req respond
    else
      -- Add Link preload headers for predicted resources
      app req fun resp => do
        let linkHeaders := pushes.map fun p =>
          (Data.CI.mk' "Link", s!"<{p}>; rel=preload")
        respond (resp.mapResponseHeaders (· ++ linkHeaders))

end Network.WebApp.Extra.Middleware.Push
