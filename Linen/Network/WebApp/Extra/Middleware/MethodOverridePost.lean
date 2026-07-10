/-
  Linen.Network.WebApp.Extra.Middleware.MethodOverridePost — override method
  from a POST body

  For POST requests, reads the `_method` parameter from the URL-encoded
  body. Ports `Network.Wai.Middleware.MethodOverridePost`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types

/-- Override the request method for POST requests if an `_method` parameter
    is found in the URL-encoded request body.

    Note: this consumes the request body's first chunk to find the
    parameter, then replays that chunk to the wrapped application so the
    full body is still readable.
    $$\text{methodOverridePost} : \text{Middleware}$$ -/
def methodOverridePost : Middleware :=
  fun app req respond =>
    if req.requestMethod == .standard .POST then
      AppM.ioThen (do
        let chunk ← req.requestBody
        let bodyStr := String.fromUTF8! chunk
        let params := parseSimpleQuery bodyStr
        let returned ← IO.mkRef false
        let newBody : IO ByteArray := do
          let done ← returned.get
          if done then req.requestBody
          else
            returned.set true
            return chunk
        match params.find? (fun (k, _) => k == "_method") with
        | some (_, v) =>
          pure { req with requestMethod := parseMethod v, requestBody := newBody }
        | none =>
          pure { req with requestBody := newBody })
        fun req' => app req' respond
    else
      app req respond
where
  parseSimpleQuery (s : String) : List (String × String) :=
    let pairs := s.splitOn "&"
    pairs.filterMap fun pair =>
      match pair.splitOn "=" with
      | [k, v] => some (k, v)
      | _ => none

end Network.WebApp.Extra.Middleware
