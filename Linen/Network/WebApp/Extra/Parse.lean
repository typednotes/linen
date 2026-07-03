/-
  Linen.Network.WebApp.Extra.Parse — request body parsing

  Parses URL-encoded form data from request bodies. Ports Hale's
  `Network.Wai.Parse` (whose multipart branch and percent-decoding were
  already `TODO` stubs there — ported as-is, no new stubs introduced here).
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Parse

open Network.WebApp
open Network.HTTP.Types

/-- A parsed form parameter (name, value). -/
abbrev Param := String × String

/-- A parsed file upload. -/
structure FileInfo where
  fileName : String
  fileContentType : String
  fileContent : ByteArray

/-- Backend for handling file uploads. -/
inductive BackEnd where
  /-- Store in memory as `ByteArray`. -/
  | lbs
  /-- Write to temp files under `dir`. -/
  | tempFile (dir : String)

/-- Percent-decode a `+`-for-space encoded token (full `%XX` decoding is not
    yet implemented, matching Hale's source). -/
private def urlDecode (s : String) : String :=
  s.map fun c => if c == '+' then ' ' else c

/-- Parse a URL-encoded form body (`application/x-www-form-urlencoded`).
    $$\text{parseUrlEncoded} : \text{String} \to \text{List Param}$$ -/
def parseUrlEncoded (body : String) : List Param :=
  let pairs := body.splitOn "&"
  pairs.filterMap fun pair =>
    match pair.splitOn "=" with
    | [k, v] => some (urlDecode k, urlDecode v)
    | [k] => some (urlDecode k, "")
    | _ => none

/-- Parse request body parameters. For URL-encoded bodies, parses directly;
    multipart bodies are not yet parsed (returns no params/files), matching
    Hale's source. Returns `(params, files)`.
    $$\text{parseRequestBody} : \text{Request} \to \text{IO}(\text{List Param} \times \text{List}(\text{String} \times \text{FileInfo}))$$ -/
def parseRequestBody (req : Request) : IO (List Param × List (String × FileInfo)) := do
  let body ← Network.WebApp.getRequestBodyChunk req
  let bodyStr := String.fromUTF8! body
  let ct := req.requestHeaders.find? (fun (n, _) => n == hContentType) |>.map (·.2)
  match ct with
  | some ct' =>
    if ct'.startsWith "application/x-www-form-urlencoded" then
      return (parseUrlEncoded bodyStr, [])
    else
      return ([], [])
  | none => return ([], [])

end Network.WebApp.Extra.Parse
