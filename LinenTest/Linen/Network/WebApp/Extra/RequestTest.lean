import Linen.Network.WebApp.Extra.Request

/-! ### Tests for `Linen.Network.WebApp.Extra.Request`

    Coverage: `appearsSecure`'s three ways to be secure (direct TLS,
    `X-Forwarded-Proto`, `X-Forwarded-SSL`) and the plain-HTTP negative. -/

open Network.WebApp Network.WebApp.Extra Network.HTTP.Types
open Data (CI)

namespace Tests.Network.WebApp.Extra.Request

#guard appearsSecure { defaultRequest with isSecure := true }
#guard appearsSecure { defaultRequest with requestHeaders := [(CI.mk' "X-Forwarded-Proto", "https")] }
#guard appearsSecure { defaultRequest with requestHeaders := [(CI.mk' "X-Forwarded-SSL", "on")] }
#guard !appearsSecure defaultRequest
#guard !appearsSecure { defaultRequest with requestHeaders := [(CI.mk' "X-Forwarded-Proto", "http")] }

end Tests.Network.WebApp.Extra.Request
