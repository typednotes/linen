/-
  Linen.Network.HTTP.Simple — Simple high-level HTTP client

  Provides one-shot HTTP request functions for common use cases.
  No conduit dependency — just plain IO.

  ## Haskell equivalent
  `Network.HTTP.Simple` from the `http-conduit` package.
-/

import Linen.Network.HTTP.Client.Types
import Linen.Network.HTTP.Client.Connection
import Linen.Network.HTTP.Client.Response

namespace Network.HTTP.Simple

open Network.HTTP.Client
open Network.HTTP.Types

/-- Parse a URL string into a Request.
    Supports `http://` and `https://` schemes.
    Returns `none` on malformed URLs.
    $$\text{parseUrl} : \text{String} \to \text{Option Request}$$ -/
def parseUrl (url : String) : Option Request :=
  -- Determine scheme
  let schemeResult :=
    if url.startsWith "https://" then some (true, (url.drop 8).toString)
    else if url.startsWith "http://" then some (false, (url.drop 7).toString)
    else none
  match schemeResult with
  | none => none
  | some (isSecure, afterScheme) =>
    -- Split host[:port] from path at first '/'
    let (hostPort, pathAndQuery) :=
      match afterScheme.splitOn "/" with
      | [] => ("", "/")
      | hp :: rest =>
        let path := if rest.isEmpty then "/" else "/" ++ "/".intercalate rest
        (hp, path)
    if hostPort.isEmpty then none
    else
      -- Split host from port
      let (host, port) :=
        match hostPort.splitOn ":" with
        | [h] => (h, if isSecure then (443 : UInt16) else 80)
        | [h, p] => (h, match p.toNat? with
            | some n => n.toUInt16
            | none => if isSecure then 443 else 80)
        | _ => (hostPort, if isSecure then 443 else 80)
      -- Split path from query string
      let (path, queryString) :=
        match pathAndQuery.splitOn "?" with
        | [p] => (p, "")
        | [p, q] => (p, "?" ++ q)
        | _ => (pathAndQuery, "")
      some { method := Method.standard .GET
           , host
           , port
           , path
           , queryString
           , isSecure
           , headers := [] }

/-- Parse a URL, throwing an IO error on failure. -/
def parseUrl! (url : String) : IO Request := do
  match parseUrl url with
  | some req => return req
  | none => throw (IO.Error.userError s!"Failed to parse URL: {url}")

/-- Perform a simple GET request and return the response body as a ByteArray.
    $$\text{simpleHttp} : \text{String} \to \text{IO ByteArray}$$ -/
def simpleHttp (url : String) : IO ByteArray := do
  let req ← parseUrl! url
  let conn ← connect req.host req.port req.isSecure
  try
    let resp ← performRequest conn req
    return resp.body
  finally
    conn.connClose

/-- Perform an HTTP request and return the full response with a ByteArray body.
    $$\text{httpBS} : \text{Request} \to \text{IO Response}$$ -/
def httpBS (req : Request) : IO Response := do
  let port := if req.port == 0 then defaultPort req.isSecure else req.port
  let conn ← connect req.host port req.isSecure
  try
    performRequest conn req
  finally
    conn.connClose

/-- Perform an HTTP request and return the response body as a ByteArray.
    Alias for `httpBS` that returns just the body.
    $$\text{httpLbs} : \text{Request} \to \text{IO ByteArray}$$ -/
def httpLbs (req : Request) : IO ByteArray := do
  let resp ← httpBS req
  return resp.body

end Network.HTTP.Simple
