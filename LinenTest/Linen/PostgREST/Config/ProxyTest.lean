/-
  Tests for `Linen.PostgREST.Config.Proxy`.
-/
import Linen.PostgREST.Config.Proxy

open PostgREST.Config

namespace Tests.PostgREST.Config.Proxy

/-! ### `UriScheme` -/

#guard toString UriScheme.http == "http"
#guard toString UriScheme.https == "https"
#guard UriScheme.http.defaultPort == 80
#guard UriScheme.https.defaultPort == 443

/-! ### `ProxyUri.toUri` -/

#guard toString ({ puScheme := .https, puHost := "api.example.com", puPort := 443, puBasePath := "/" } : ProxyUri) == "https://api.example.com/"
#guard toString ({ puScheme := .https, puHost := "api.example.com", puPort := 8443, puBasePath := "/v1" } : ProxyUri) == "https://api.example.com:8443/v1"
#guard toString ({ puScheme := .http, puHost := "localhost", puPort := 80, puBasePath := "/" } : ProxyUri) == "http://localhost/"
#guard toString ({ puScheme := .http, puHost := "localhost", puPort := 3000, puBasePath := "/" } : ProxyUri) == "http://localhost:3000/"

/-! ### `ProxyUri.parse` -/

#guard ProxyUri.parse "https://api.example.com:8443/v1" ==
  some { puScheme := .https, puHost := "api.example.com", puPort := 8443, puBasePath := "/v1" }
#guard ProxyUri.parse "http://example.com" ==
  some { puScheme := .http, puHost := "example.com", puPort := 80, puBasePath := "/" }
#guard ProxyUri.parse "https://example.com" ==
  some { puScheme := .https, puHost := "example.com", puPort := 443, puBasePath := "/" }
#guard ProxyUri.parse "http://example.com:8080" ==
  some { puScheme := .http, puHost := "example.com", puPort := 8080, puBasePath := "/" }
#guard ProxyUri.parse "http://example.com/api/v1" ==
  some { puScheme := .http, puHost := "example.com", puPort := 80, puBasePath := "/api/v1" }
#guard ProxyUri.parse "http://example.com:9999/api/v1" ==
  some { puScheme := .http, puHost := "example.com", puPort := 9999, puBasePath := "/api/v1" }
#guard ProxyUri.parse "ftp://example.com" == none
#guard ProxyUri.parse "example.com" == none
#guard ProxyUri.parse "http://" == none
#guard ProxyUri.parse "http://example.com:notaport" ==
  some { puScheme := .http, puHost := "example.com", puPort := 80, puBasePath := "/" }

/-! ### `openApiServerUrl` -/

#guard openApiServerUrl (some { puScheme := .https, puHost := "api.example.com", puPort := 443, puBasePath := "/" }) "127.0.0.1" 3000 == "https://api.example.com/"
#guard openApiServerUrl none "127.0.0.1" 3000 == "http://127.0.0.1:3000/"
#guard openApiServerUrl none "!4" 3000 == "http://0.0.0.0:3000/"
#guard openApiServerUrl none "!6" 3000 == "http://[::]:3000/"

end Tests.PostgREST.Config.Proxy
