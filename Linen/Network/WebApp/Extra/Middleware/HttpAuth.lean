/-
  Linen.Network.WebApp.Extra.Middleware.HttpAuth — HTTP Basic Authentication

  Ports Hale's `Network.Wai.Middleware.HttpAuth`.
-/
import Linen.Network.WebApp
import Linen.Data.Base64

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types

/-- Check function type: given a username and password, return whether
    access is granted. -/
abbrev CheckCreds := String → String → IO Bool

/-- HTTP Basic Authentication middleware. Checks the `Authorization` header
    against the provided credential checker. Returns 401 Unauthorized if
    credentials are missing or invalid.
    $$\text{basicAuth} : \text{CheckCreds} \to \text{String} \to \text{Middleware}$$
    The `realm` parameter is displayed in the browser's auth dialog. -/
def basicAuth (check : CheckCreds) (realm : String := "Restricted") : Middleware :=
  fun app req respond =>
    let authHeader := req.requestHeaders.find? (fun (n, _) => n == hAuthorization)
    match authHeader with
    | some (_, value) =>
      if value.startsWith "Basic " then
        let encoded := (value.drop 6).toString
        match Data.Base64.decode encoded with
        | some decoded =>
          let credStr := String.fromUTF8! decoded
          match credStr.splitOn ":" with
          | user :: rest =>
            let pass := ":".intercalate rest
            AppM.ioThen (check user pass) fun ok =>
            if ok then app req respond
            else AppM.respond respond (unauthorized realm)
          | _ => AppM.respond respond (unauthorized realm)
        | none => AppM.respond respond (unauthorized realm)
      else AppM.respond respond (unauthorized realm)
    | none => AppM.respond respond (unauthorized realm)
where
  unauthorized (realm : String) : Response :=
    .responseBuilder status401
      [(hWWWAuthenticate, s!"Basic realm=\"{realm}\"")] "Unauthorized".toUTF8

end Network.WebApp.Extra.Middleware
