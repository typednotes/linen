/-
  Linen.Network.WebApp.Extra.Middleware.Push.Referer.Types — HTTP/2 push types

  Ports `Network.Wai.Middleware.Push.Referer.Types`. Types for the
  referer-based HTTP/2 server push prediction system.
-/
namespace Network.WebApp.Extra.Middleware.Push.Referer

/-- A URL path used as a push resource. -/
abbrev PushPath := String

/-- A mapping from a page path to its associated push resources. -/
structure PushEntry where
  /-- The page that triggers pushes. -/
  pagePath : String
  /-- Resources to push when this page is requested. -/
  pushPaths : List PushPath
deriving BEq, Repr

/-- Configuration for the push referer middleware. -/
structure PushSettings where
  /-- Maximum number of entries in the push table. -/
  maxEntries : Nat := 1000
  /-- Maximum number of push resources per page. -/
  maxPushesPerPage : Nat := 20
  /-- Whether to enable push. -/
  enabled : Bool := true

end Network.WebApp.Extra.Middleware.Push.Referer
