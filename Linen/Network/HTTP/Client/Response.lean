/-
  Linen.Network.HTTP.Client.Response — HTTP/1.1 response parsing

  Parses HTTP/1.1 responses from a `Connection`:
  - Status line: "HTTP/1.1 200 OK\r\n"
  - Headers: "Name: value\r\n" until blank line "\r\n"
  - Body: Content-Length, chunked Transfer-Encoding, or read-until-close

  ## Totality
  The original `http-client` source used `partial def` for the network
  read-loops (`readLine`, `parseHeaders`, `readExactly`, `readUntilClose`,
  `readChunkedBody`). Since these loops are driven by runtime conditions (EOF, a
  blank line, a byte count) with no structural measure, they are written here as
  condition-driven `while` loops — the same idiom used by the socket event loop
  and the HTTP/2 server — so no `partial`/fuel is needed.
-/

import Linen.Network.HTTP.Client.Types
import Linen.Network.HTTP.Client.Request

namespace Network.HTTP.Client

open Network.HTTP.Types
open Data

-- ── ByteArray-level line reading ──

/-- The CRLF bytes: 0x0D 0x0A. -/
private def crlfBytes : ByteArray :=
  ByteArray.empty.push 0x0D |>.push 0x0A

/-- Find the index of CRLF (\r\n) in a ByteArray at or after `start`. -/
private def findCRLF (buf : ByteArray) (start : Nat := 0) : Option Nat :=
  ((List.range buf.size).drop start).find?
    (fun i => i + 1 < buf.size && buf.get! i == 0x0D && buf.get! (i + 1) == 0x0A)

/-- Read bytes from a connection until a full line (ending with \r\n) is buffered.
    Returns the line bytes (without CRLF) and the leftover bytes after it. -/
private def readLine (conn : Connection) (initial : ByteArray := ByteArray.empty)
    : IO (ByteArray × ByteArray) := do
  let mut buf := initial
  let mut out : ByteArray × ByteArray := (ByteArray.empty, ByteArray.empty)
  let mut done := false
  while !done do
    match findCRLF buf with
    | some idx =>
      out := (buf.extract 0 idx, buf.extract (idx + 2) buf.size)
      done := true
    | none =>
      let chunk ← conn.connRead 4096
      if chunk.isEmpty then
        out := (buf, ByteArray.empty)  -- EOF before CRLF
        done := true
      else
        buf := buf ++ chunk
  return out

/-- Convert a ByteArray line to a String. -/
private def lineToString (line : ByteArray) : String :=
  String.fromUTF8! line

/-- Parse a status line: "HTTP/x.y code reason".
    Returns (HttpVersion, Status) or throws on malformed input. -/
def parseStatusLine (line : String) : IO (HttpVersion × Status) := do
  let parts := line.splitOn " "
  match parts with
  | versionStr :: codeStr :: rest =>
    let version ← if versionStr.startsWith "HTTP/" then
      let verStr := (versionStr.drop 5).toString
      let verParts := verStr.splitOn "."
      match verParts with
      | [maj, min] =>
        match (maj.toNat?, min.toNat?) with
        | (some major, some minor) => pure { major, minor : HttpVersion }
        | _ => throw (IO.Error.userError s!"Invalid HTTP version: {versionStr}")
      | _ => throw (IO.Error.userError s!"Invalid HTTP version: {versionStr}")
    else throw (IO.Error.userError s!"Expected HTTP version, got: {versionStr}")
    let code ← match codeStr.toNat? with
      | some n => pure n
      | none => throw (IO.Error.userError s!"Invalid status code: {codeStr}")
    let reason := " ".intercalate rest
    if h : 100 ≤ code ∧ code ≤ 999 then
      let status : Status := ⟨code, reason, h⟩
      return (version, status)
    else
      throw (IO.Error.userError s!"Status code out of range: {code}")
  | _ => throw (IO.Error.userError s!"Malformed status line: {line}")

/-- Find the first index of a character in a string (by char position). -/
private def findCharIdx (s : String) (c : Char) : Option Nat :=
  s.toList.findIdx? (· == c)

/-- Parse headers from the connection until a blank line. -/
private def parseHeaders (conn : Connection) (buf0 : ByteArray)
    (acc0 : ResponseHeaders := []) : IO (ResponseHeaders × ByteArray) := do
  let mut buf := buf0
  let mut acc := acc0
  let mut rest := ByteArray.empty
  let mut done := false
  while !done do
    let (lineBytes, r) ← readLine conn buf
    if lineBytes.isEmpty then
      rest := r
      done := true
    else
      let line := lineToString lineBytes
      -- Parse "Name: value" or "Name:value"; skip malformed lines.
      match findCharIdx line ':' with
      | some colonIdx =>
        let name := (String.take line colonIdx).toString
        let rawValue := (String.drop line (colonIdx + 1)).toString
        let value := (String.trimAsciiStart rawValue).toString
        acc := (CI.mk' name, value) :: acc
      | none => pure ()
      buf := r
  return (acc.reverse, rest)

/-- Read exactly `n` bytes from a connection (or fewer, on EOF). -/
private def readExactly (conn : Connection) (n : Nat) (initial : ByteArray := ByteArray.empty)
    : IO ByteArray := do
  let mut buf := initial
  let mut eof := false
  while buf.size < n && !eof do
    let chunk ← conn.connRead (n - buf.size)
    if chunk.isEmpty then eof := true
    else buf := buf ++ chunk
  return if buf.size ≥ n then buf.extract 0 n else buf

/-- Read all remaining bytes until EOF. -/
private def readUntilClose (conn : Connection) (initial : ByteArray := ByteArray.empty)
    : IO ByteArray := do
  let mut buf := initial
  let mut eof := false
  while !eof do
    let chunk ← conn.connRead 8192
    if chunk.isEmpty then eof := true
    else buf := buf ++ chunk
  return buf

/-- Read until at least `k` bytes are buffered (or EOF), returning the full
    buffer (any excess beyond `k` is retained — unlike `readExactly`, which
    truncates). -/
private def fillTo (conn : Connection) (k : Nat) (initial : ByteArray) : IO ByteArray := do
  let mut buf := initial
  let mut eof := false
  while buf.size < k && !eof do
    let chunk ← conn.connRead (k - buf.size)
    if chunk.isEmpty then eof := true
    else buf := buf ++ chunk
  return buf

/-- Parse a hexadecimal string to Nat. -/
private def hexToNat (s : String) : Option Nat := Id.run do
  let mut result := 0
  for c in s.toLower.toList do
    if '0' ≤ c && c ≤ '9' then
      result := result * 16 + (c.toNat - '0'.toNat)
    else if 'a' ≤ c && c ≤ 'f' then
      result := result * 16 + (c.toNat - 'a'.toNat + 10)
    else
      return none
  return some result

/-- Parse a chunked transfer-encoding body. -/
private def readChunkedBody (conn : Connection) (buf0 : ByteArray)
    (acc0 : ByteArray := ByteArray.empty) : IO ByteArray := do
  let mut buf := buf0
  let mut acc := acc0
  let mut done := false
  while !done do
    let (sizeLineBytes, rest) ← readLine conn buf
    let sizeLine := lineToString sizeLineBytes
    let sizeStr := (String.trimAscii ((sizeLine.splitOn ";").head!)).toString
    match hexToNat sizeStr with
    | none => throw (IO.Error.userError s!"Invalid chunk size: {sizeStr}")
    | some size =>
      if size == 0 then
        let _ ← readLine conn rest  -- consume the trailing CRLF
        done := true
      else
        -- Read the chunk data + trailing CRLF, KEEPING any already-buffered
        -- bytes that belong to the following chunk(s).
        let filled ← fillTo conn (size + 2) rest
        acc := acc ++ filled.extract 0 size
        buf := filled.extract (size + 2) filled.size
  return acc

/-- Look up a header value (case-insensitive). -/
private def findHeader' (headers : ResponseHeaders) (name : HeaderName) : Option String :=
  headers.find? (fun (n, _) => n == name) |>.map Prod.snd

/-- Receive and parse a complete HTTP/1.1 response from a connection.

    Handles three body-reading strategies:
    1. **Content-Length**: read exactly that many bytes
    2. **Transfer-Encoding: chunked**: decode chunked encoding
    3. **Neither**: read until connection close

    $$\text{receiveResponse} : \text{Connection} \to \text{IO Response}$$ -/
def receiveResponse (conn : Connection) : IO Response := do
  let (statusLineBytes, buf) ← readLine conn
  let statusLineStr := lineToString statusLineBytes
  let (version, status) ← parseStatusLine statusLineStr
  let (headers, bodyBuf) ← parseHeaders conn buf
  let body ← match findHeader' headers hContentLength with
    | some lenStr =>
      match lenStr.toNat? with
      | some len => readExactly conn len bodyBuf
      | none => throw (IO.Error.userError s!"Invalid Content-Length: {lenStr}")
    | none =>
      let te := findHeader' headers hTransferEncoding
      if te == some "chunked" then
        readChunkedBody conn bodyBuf
      else
        readUntilClose conn bodyBuf
  return { statusCode := status, headers, body, httpVersion := version }

/-- Perform an HTTP request on a connection: send request, receive response.
    $$\text{performRequest} : \text{Connection} \to \text{Request} \to \text{IO Response}$$ -/
def performRequest (conn : Connection) (req : Request) : IO Response := do
  sendRequest conn req
  receiveResponse conn

end Network.HTTP.Client
