import Linen.Network.WebApp.Extra.Middleware.Push.Referer.Types

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.Push.Referer.Types`

    Coverage: `PushSettings` defaults and `PushEntry`'s derived `BEq`. -/

open Network.WebApp.Extra.Middleware.Push.Referer

namespace Tests.Network.WebApp.Extra.Middleware.Push.Referer.Types

#guard ({} : PushSettings).maxEntries == 1000
#guard ({} : PushSettings).maxPushesPerPage == 20
#guard ({} : PushSettings).enabled == true

#guard ({ maxEntries := 5, maxPushesPerPage := 2, enabled := false } : PushSettings).maxEntries == 5

#guard ({ pagePath := "/", pushPaths := ["/a.css"] } : PushEntry)
  == ({ pagePath := "/", pushPaths := ["/a.css"] } : PushEntry)
#guard ({ pagePath := "/", pushPaths := ["/a.css"] } : PushEntry)
  != ({ pagePath := "/", pushPaths := ["/b.css"] } : PushEntry)

end Tests.Network.WebApp.Extra.Middleware.Push.Referer.Types
