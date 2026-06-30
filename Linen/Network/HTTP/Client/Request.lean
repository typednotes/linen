/-
  Linen.Network.HTTP.Client.Request — HTTP/1.1 request serialization

  Serializes an `HttpClientRequest` to wire format for sending over a `Connection`.

  ## Wire format
  ```
  METHOD /path?query HTTP/1.1\r\n
  Host: hostname\r\n
  Content-Length: NNN\r\n    (if body present)
  Header1: value1\r\n
  ...\r\n
  \r\n
  [body bytes]
  ```

  ## Guarantees
  - Host header is always present (auto-added from Request.host)
  - Content-Length is always present when body is non-empty
  - Content-Length matches actual body size
-/

import Linen.Network.HTTP.Client.Types

namespace Network.HTTP.Client

open Network.HTTP.Types

/-- CRLF line ending. -/
private def crlf : String := "\r\n"

/-- Render the request line: "METHOD /path?query HTTP/1.1\r\n". -/
private def renderRequestLine (req : Request) : String :=
  let method := toString req.method
  let pathAndQuery := req.path ++ req.queryString
  s!"{method} {pathAndQuery} {req.httpVersion}{crlf}"

/-- Render a single header: "Name: value\r\n". -/
private def renderHeader (h : Header) : String :=
  let (name, value) := h
  s!"{name}: {value}{crlf}"

/-- Check if a header name is present in a header list. -/
private def hasHeader (headers : RequestHeaders) (name : HeaderName) : Bool :=
  headers.any (fun (n, _) => n == name)

/-- Build the Host header value from the request.
    Includes port if it's non-standard (not 80 for HTTP, not 443 for HTTPS). -/
private def hostHeaderValue (req : Request) : String :=
  let defaultPort := if req.isSecure then 443 else 80
  if req.port == defaultPort then req.host
  else s!"{req.host}:{req.port}"

/-- Serialize an HTTP/1.1 request to a ByteArray.

    Automatically adds:
    - `Host` header (from `Request.host` and `Request.port`)
    - `Content-Length` header when body is present

    $$\text{serializeRequest} : \text{Request} \to \text{ByteArray}$$ -/
def serializeRequest (req : Request) : ByteArray := Id.run do
  let requestLine := renderRequestLine req
  -- Build headers: start with user headers, then add auto-generated ones
  let headers := req.headers
  -- Add Host header if not already present
  let headers := if hasHeader headers hHost then headers
    else (hHost, hostHeaderValue req) :: headers
  -- Add Content-Length if body is present and header not already set
  let headers := match req.body with
    | some body =>
      if hasHeader headers hContentLength then headers
      else (hContentLength, toString body.size) :: headers
    | none => headers
  -- Add Connection: close (no keep-alive for now)
  let headers := if hasHeader headers hConnection then headers
    else (hConnection, "close") :: headers
  -- Serialize everything
  let mut result := requestLine
  for h in headers do
    result := result ++ renderHeader h
  result := result ++ crlf  -- blank line after headers
  let headerBytes := result.toUTF8
  -- Append body if present
  match req.body with
  | some body => return headerBytes ++ body
  | none => return headerBytes

/-- Send a serialized request over a connection.
    $$\text{sendRequest} : \text{Connection} \to \text{Request} \to \text{IO Unit}$$ -/
def sendRequest (conn : Connection) (req : Request) : IO Unit := do
  let bytes := serializeRequest req
  conn.connWrite bytes

end Network.HTTP.Client
