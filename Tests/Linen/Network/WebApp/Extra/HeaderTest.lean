import Linen.Network.WebApp.Extra.Header

/-! ### Tests for `Linen.Network.WebApp.Extra.Header`

    Coverage: `contentLength`'s numeric parse (present/absent/non-numeric),
    `hasContentType`'s prefix match. -/

open Network.WebApp Network.WebApp.Extra Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Header

#guard contentLength { defaultRequest with requestHeaders := [(hContentLength, "42")] } == some 42
#guard contentLength { defaultRequest with requestHeaders := [(hContentLength, "nope")] } == none
#guard contentLength defaultRequest == none

#guard hasContentType "text/" { defaultRequest with requestHeaders := [(hContentType, "text/plain")] }
#guard !hasContentType "application/json" { defaultRequest with requestHeaders := [(hContentType, "text/plain")] }
#guard !hasContentType "text/" defaultRequest

end Tests.Network.WebApp.Extra.Header
