/-
  Linen.Network.HTTP.Client.Contrib — response-handling helper

  Port of `hoauth2`'s `Network.HTTP.Client.Contrib` (see
  `docs/imports/hoauth2/dependencies.md`), which turns a non-2xx HTTP
  response into an error value instead of a status code the caller has to
  inspect separately.

  ## Substitutions
  Upstream's `Response BSL.ByteString` (the `http-conduit`
  library-parameterized response) is `linen`'s own
  `Network.HTTP.Client.Response`, whose `body` field is already a plain
  `ByteArray` (no lazy-bytestring analogue is needed). `Data.Aeson`'s
  `FromJSON` is `Linen.Data.Json.Types.FromJSON`, and `eitherDecode` is
  `Linen.Data.Json.Decode.decodeAs`.
-/

import Linen.Network.HTTP.Client.Types
import Linen.Data.Json.Decode

namespace Network.HTTP.Client.Contrib

open Network.HTTP.Client
open Data.Json (FromJSON)
open Data.Json.Decode (decodeAs)

/-- Extract the body out of a `Response`: `Right` on a 2xx status, `Left` the
    body (or, if the body is empty, a rendering of the whole response) on
    failure.

    $$\text{handleResponse} : \text{Response} \to \text{Except}\ \text{ByteArray}\ \text{ByteArray}$$ -/
def handleResponse (resp : Response) : Except ByteArray ByteArray :=
  if resp.isSuccess then
    .ok resp.body
  else if resp.body.isEmpty then
    .error (s!"{resp.statusCode.statusCode} {resp.statusCode.statusMessage}".toUTF8)
  else
    .error resp.body

/-- Like `handleResponse`, but also JSON-decodes a successful body into `α`.

    $$\text{handleResponseJSON} : \text{Response} \to \text{Except}\ \text{ByteArray}\ \alpha$$ -/
def handleResponseJSON {α : Type} [FromJSON α] (resp : Response) : Except ByteArray α :=
  match handleResponse resp with
  | .error e => .error e
  | .ok body =>
    match decodeAs (String.fromUTF8! body) with
    | .ok a => .ok a
    | .error msg => .error msg.toUTF8

end Network.HTTP.Client.Contrib
