/-
  Linen.Network.WebApp.Extra.Header — request header convenience queries

  Ports Hale's `Network.Wai.Header`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra

open Network.WebApp
open Network.HTTP.Types

/-- The parsed `Content-Length` request header, if present and numeric.
    $$\text{contentLength} : \text{Request} \to \text{Option}\ \mathbb{N}$$ -/
def contentLength (req : Request) : Option Nat :=
  match req.requestHeaders.find? (fun (n, _) => n == hContentLength) with
  | some (_, v) => v.toNat?
  | none => none

/-- Whether the request's `Content-Type` header starts with `ct`. -/
def hasContentType (ct : String) (req : Request) : Bool :=
  match req.requestHeaders.find? (fun (n, _) => n == hContentType) with
  | some (_, v) => v.startsWith ct
  | none => false

end Network.WebApp.Extra
