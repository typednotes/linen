/-
  Linen.Network.HTTP.Client.Types — HTTP client core types

  Defines the transport `Connection` abstraction, wire-level `Request`,
  and parsed `Response` for HTTP/1.1 client operations.

  ## Design
  - `Connection` is a record of read/write/close callbacks, abstracting
    over plain TCP and TLS transport. This avoids a sum type and lets
    the same response-parsing code work over both transports.
  - `Request` carries all fields needed to serialize an HTTP/1.1 request.
  - `Response` carries the parsed status, headers, and body.
-/

import Linen.Network.HTTP.Types.Method
import Linen.Network.HTTP.Types.Status
import Linen.Network.HTTP.Types.Header

namespace Network.HTTP.Client

open Network.HTTP.Types

/-- HTTP version as a major.minor pair. -/
structure HttpVersion where
  major : Nat := 1
  minor : Nat := 1
deriving BEq, Repr

instance : ToString HttpVersion where
  toString v := s!"HTTP/{v.major}.{v.minor}"

/-- The standard HTTP/1.1 version. -/
def http11 : HttpVersion := { major := 1, minor := 1 }

/-- Abstract transport connection (plain TCP or TLS).

    Both constructors carry read/write/close callbacks that work
    uniformly regardless of the underlying transport.

    $$\text{Connection} = \{\text{read} : \mathbb{N} \to \text{IO ByteArray},\;
      \text{write} : \text{ByteArray} \to \text{IO Unit},\;
      \text{close} : \text{IO Unit},\;
      \text{isSecure} : \text{Bool}\}$$ -/
structure Connection where
  /-- Read up to `n` bytes. Returns empty `ByteArray` on EOF. -/
  connRead : Nat → IO ByteArray
  /-- Write all bytes (loops internally until fully written). -/
  connWrite : ByteArray → IO Unit
  /-- Close the underlying connection. -/
  connClose : IO Unit
  /-- Whether this connection uses TLS. -/
  connIsSecure : Bool

/-- HTTP client request — the wire-level representation.

    All fields needed to serialize a complete HTTP/1.1 request.
    `host` is used for both the Host header and connection establishment.

    $$\text{Request} = \{ \text{method} : \text{Method},\;
      \text{host} : \text{String},\; \text{port} : \text{UInt16},\;
      \text{path} : \text{String},\; \ldots \}$$ -/
structure Request where
  /-- HTTP method (GET, POST, etc.). -/
  method : Method
  /-- Target hostname. -/
  host : String
  /-- Target port. -/
  port : UInt16
  /-- Request path (e.g., "/api/v1/users"). -/
  path : String := "/"
  /-- Query string including leading '?' (e.g., "?page=1&limit=10"). Empty if none. -/
  queryString : String := ""
  /-- Request headers (excluding auto-generated Host and Content-Length). -/
  headers : RequestHeaders := []
  /-- Request body. `none` means no body. -/
  body : Option ByteArray := none
  /-- Whether to use TLS (HTTPS). -/
  isSecure : Bool := false
  /-- HTTP version to use. -/
  httpVersion : HttpVersion := http11

/-- HTTP client response — the parsed representation.

    $$\text{Response} = \{ \text{statusCode} : \text{Status},\;
      \text{headers} : \text{ResponseHeaders},\;
      \text{body} : \text{ByteArray} \}$$ -/
structure Response where
  /-- Response status code and reason phrase. -/
  statusCode : Status
  /-- Response headers. -/
  headers : ResponseHeaders
  /-- Response body (fully read). -/
  body : ByteArray
  /-- HTTP version from the response. -/
  httpVersion : HttpVersion := http11

namespace Response

/-- Look up a header value by name (case-insensitive). -/
def findHeader (resp : Response) (name : HeaderName) : Option String :=
  resp.headers.find? (fun (n, _) => n == name) |>.map Prod.snd

/-- Get the Content-Length header value as a Nat, if present. -/
def contentLength (resp : Response) : Option Nat :=
  resp.findHeader hContentLength >>= String.toNat?

/-- Check if the response status code indicates success (2xx). -/
def isSuccess (resp : Response) : Bool :=
  let code := resp.statusCode.statusCode
  200 ≤ code && code ≤ 299

end Response

end Network.HTTP.Client
