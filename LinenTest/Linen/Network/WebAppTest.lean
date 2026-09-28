import Linen.Network.WebApp

/-! ### Tests for `Linen.Network.WebApp`

    Coverage: response constructors, request accessors/modifiers,
    `strictRequestBody`'s chunk-draining loop, and the middleware
    combinators' algebraic laws (`#guard`/`rfl`, matching the `theorem`s
    proved in the source module). -/

open Network.WebApp Network.HTTP.Types

namespace Tests.Network.WebApp

-- ── Response constructors ──

#guard (responseLBS status200 [(hContentType, "text/plain")] "hi").status.statusCode == 200
#guard (responseLBS status200 [] "hi").headers.isEmpty
#guard (responseFile' status200 [] "/tmp/x").status.statusCode == 200
#guard (responseStream' status200 [] (fun _ _ => pure ())).status.statusCode == 200

-- ── Request accessors/modifiers ──

#guard requestHeader hContentType { defaultRequest with requestHeaders := [(hContentType, "text/plain")] } == some "text/plain"
#guard requestHeader hContentType defaultRequest == none
#guard (mapRequestHeaders (fun h => (hServer, "linen") :: h) defaultRequest).requestHeaders.length == 1
#guard defaultRequest.requestMethod == .standard .GET
#guard defaultRequest.rawPathInfo == ""

-- `strictRequestBody`/`consumeRequestBodyStrict`: drains a fixed sequence of
-- chunks (via an `IO.Ref`-backed body) down to the empty terminator.
#eval show IO Unit from do
  let chunks ← IO.mkRef ([ "ab".toUTF8, "cd".toUTF8, ByteArray.empty ] : List ByteArray)
  let body : IO ByteArray := do
    match ← chunks.get with
    | c :: rest => chunks.set rest; pure c
    | [] => pure ByteArray.empty
  let req := setRequestBodyChunks body defaultRequest
  let all ← strictRequestBody req
  unless String.fromUTF8! all == "abcd" do
    throw (IO.userError s!"strictRequestBody: expected \"abcd\", got {String.fromUTF8! all}")

#eval show IO Unit from do
  let req := setRequestBodyChunks (pure ByteArray.empty) defaultRequest
  let all ← consumeRequestBodyStrict req
  unless all.isEmpty do throw (IO.userError "consumeRequestBodyStrict: expected empty body")

-- ── Middleware combinators ──

-- `addHeader` prepends onto the response's header list.
#guard (addHeader hServer "linen" (responseLBS status200 [] "")).headers == [(hServer, "linen")]

-- `modifyRequest`/`modifyResponse`/`ifRequest`: proved as `theorem`s in the
-- source module (`rfl`); spot-checked here again as executable witnesses.
example : modifyRequest id = (idMiddleware : Middleware) := modifyRequest_id
example : modifyResponse id = (idMiddleware : Middleware) := modifyResponse_id
example (m : Middleware) : ifRequest (fun _ => false) m = (idMiddleware : Middleware) := ifRequest_false m
example (m : Middleware) : composeMiddleware idMiddleware m = m := idMiddleware_comp_left m
example (m : Middleware) : composeMiddleware m idMiddleware = m := idMiddleware_comp_right m

end Tests.Network.WebApp
