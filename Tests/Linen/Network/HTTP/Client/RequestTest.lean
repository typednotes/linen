/-
  Tests for `Linen.Network.HTTP.Client.Request`.

  `serializeRequest` is pure, so the exact HTTP/1.1 wire bytes are checked with
  `#guard`; `sendRequest` (IO over a `Connection`) is pinned at the type level.
-/
import Linen.Network.HTTP.Client.Request

open Network.HTTP.Client Network.HTTP.Types Data

namespace Tests.Network.HTTP.Client.Request

/-! ### serializeRequest — auto Host / Content-Length / Connection -/

-- GET on the default port: bare Host, Connection: close, no Content-Length.
#guard serializeRequest { method := .standard .GET, host := "example.com", port := 80 }
        == "GET / HTTP/1.1\r\nConnection: close\r\nHost: example.com\r\n\r\n".toUTF8

-- POST with a body over a non-standard TLS port: Content-Length added, host:port.
#guard serializeRequest
          { method := .standard .POST, host := "api.test", port := 8443, path := "/v1",
            queryString := "?a=1", isSecure := true, body := some "hello".toUTF8 }
        == "POST /v1?a=1 HTTP/1.1\r\nConnection: close\r\nContent-Length: 5\r\nHost: api.test:8443\r\n\r\nhello".toUTF8

-- The standard HTTPS port (443) is omitted from the Host header.
#guard serializeRequest { method := .standard .GET, host := "secure.test", port := 443, isSecure := true }
        == "GET / HTTP/1.1\r\nConnection: close\r\nHost: secure.test\r\n\r\n".toUTF8

-- A user-supplied Host header is respected (not duplicated/overwritten).
#guard serializeRequest
          { method := .standard .GET, host := "x", port := 80, headers := [(hHost, "custom.example")] }
        == "GET / HTTP/1.1\r\nConnection: close\r\nHost: custom.example\r\n\r\n".toUTF8

-- A custom header is rendered alongside the auto-generated ones.
#guard serializeRequest
          { method := .standard .GET, host := "h", port := 80, headers := [(hUserAgent, "linen/0.1")] }
        == "GET / HTTP/1.1\r\nConnection: close\r\nHost: h\r\nUser-Agent: linen/0.1\r\n\r\n".toUTF8

/-! ### sendRequest — signature (writes over a connection) -/

example : Connection → Request → IO Unit := sendRequest

end Tests.Network.HTTP.Client.Request
