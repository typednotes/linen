/-
  Linen.Network.HTTP.Client.Redirect — HTTP redirect following

  Follows 3xx redirects according to RFC 9110:
  - 301/302/303: convert to GET, drop body
  - 307/308: preserve method and body

  ## Termination
  `maxRedirects` is a genuine part of the API (a caller-specified redirect
  budget, matching curl/wget), not a fuel counter standing in for a real
  termination argument — it decreases structurally on each hop
  (`n + 1 → n`), which Lean's checker accepts directly.
-/

import Linen.Network.HTTP.Client.Types
import Linen.Network.HTTP.Client.Connection
import Linen.Network.HTTP.Client.Request
import Linen.Network.HTTP.Client.Response

namespace Network.HTTP.Client

open Network.HTTP.Types

/-- Check if a status code is a redirect (3xx with Location header). -/
private def isRedirect (status : Status) : Bool :=
  let c := status.statusCode
  c == 301 || c == 302 || c == 303 || c == 307 || c == 308

/-- For 301/302/303 redirects, the method should change to GET and body should be dropped. -/
private def shouldChangeToGet (statusCode : Nat) : Bool :=
  statusCode == 301 || statusCode == 302 || statusCode == 303

/-- Find the first index of a character in a string. -/
private def findChar (s : String) (c : Char) : Option Nat :=
  go 0 s.toList
where
  go (i : Nat) : List Char → Option Nat
  | [] => none
  | x :: xs => if x == c then some i else go (i + 1) xs

/-- Parse a Location header URL into request fields.
    Handles absolute URLs ("https://example.com/path") and relative paths ("/newpath"). -/
private def parseLocation (location : String) (origReq : Request) : IO Request := do
  if location.startsWith "http://" || location.startsWith "https://" then
    let isSecure := location.startsWith "https://"
    let afterScheme := (location.drop (if isSecure then 8 else 7)).toString
    -- Split host:port from path
    let (hostPort, pathAndQuery) := match findChar afterScheme '/' with
      | some idx => ((afterScheme.take idx).toString, (afterScheme.drop idx).toString)
      | none => (afterScheme, "/")
    -- Split host from port
    let (host, port) := match findChar hostPort ':' with
      | some idx =>
        let h := (hostPort.take idx).toString
        let p := (hostPort.drop (idx + 1)).toString
        match p.toNat? with
        | some n => (h, n.toUInt16)
        | none => (h, defaultPort isSecure)
      | none => (hostPort, defaultPort isSecure)
    -- Split path from query string
    let (path, queryString) := match findChar pathAndQuery '?' with
      | some idx => ((pathAndQuery.take idx).toString, (pathAndQuery.drop idx).toString)
      | none => (pathAndQuery, "")
    return { origReq with host, port, path, queryString, isSecure }
  else
    -- Relative URL — keep same host/port/scheme
    let (path, queryString) := match findChar location '?' with
      | some idx => ((location.take idx).toString, (location.drop idx).toString)
      | none => (location, "")
    return { origReq with path, queryString }

/-- Execute an HTTP request, following redirects up to `maxRedirects` times.

    $$\text{executeWithRedirects} : \mathbb{N} \to \text{Request} \to \text{IO Response}$$ -/
def executeWithRedirects (maxRedirects : Nat) (req : Request) : IO Response := do
  go maxRedirects req
where
  go : Nat → Request → IO Response
  | 0, req => do
    let conn ← connect req.host req.port req.isSecure
    try
      performRequest conn req
    finally
      conn.connClose
  | n + 1, req => do
    let conn ← connect req.host req.port req.isSecure
    let resp ← try
      performRequest conn req
    finally
      conn.connClose
    if isRedirect resp.statusCode then
      match resp.findHeader hLocation with
      | some location =>
        let mut newReq ← parseLocation location req
        if shouldChangeToGet resp.statusCode.statusCode then
          newReq := { newReq with method := .standard .GET, body := none }
        go n newReq
      | none =>
        return resp
    else
      return resp

/-- Execute a full HTTP request with default redirect following (up to 10).
    $$\text{execute} : \text{Request} \to \text{Nat} \to \text{IO Response}$$ -/
def execute (req : Request) (maxRedirects : Nat := 10) : IO Response :=
  executeWithRedirects maxRedirects req

end Network.HTTP.Client
