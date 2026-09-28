import Linen.Network.WebApp.Extra.Middleware.Push.Referer.ParseURL

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.Push.Referer.ParseURL`

    Coverage: `extractPath` strips scheme/host and query string for
    `https://`/`http://`/bare-path referers; `isStaticResource` recognizes
    common static-asset extensions. -/

open Network.WebApp.Extra.Middleware.Push.Referer

namespace Tests.Network.WebApp.Extra.Middleware.Push.Referer.ParseURL

#guard extractPath "https://example.com/page?q=1" == "/page"
#guard extractPath "http://example.com/a/b" == "/a/b"
#guard extractPath "/page?q=1" == "/page"
#guard extractPath "/plain" == "/plain"
#guard extractPath "https://example.com" == "/"

#guard isStaticResource "/app.css" == true
#guard isStaticResource "/app.js" == true
#guard isStaticResource "/logo.png" == true
#guard isStaticResource "/page" == false
#guard isStaticResource "/api/users" == false

end Tests.Network.WebApp.Extra.Middleware.Push.Referer.ParseURL
